#!/bin/sh
set -eu

DOCKER_SOCKET="${DOCKER_SOCKET:-/var/run/docker.sock}"
WORKSPACE_NETWORK="${WORKSPACE_NETWORK:-workspace_user}"
INFRASTRUCTURE_LABEL="${INFRASTRUCTURE_LABEL:-hackschule.workspace.peer_firewall.infrastructure}"
# The case server of »Ein Krimi im Terminal« (museum.bitstein) and the Git
# server of the groups (git.workspace) carry this label: students reach their
# SSH port, and they only answer. They are no infrastructure: from there, a
# student must not reach anyone else's Workspace.
CASE_SERVER_LABEL="${CASE_SERVER_LABEL:-hackschule.workspace.peer_firewall.case_server}"
# The speech service of the Telefonspiel (phone_speech): the emulator in a
# Workspace asks it for speech and music on port 8000, and it only answers.
PHONE_LABEL="${PHONE_LABEL:-hackschule.workspace.peer_firewall.phone}"
PHONE_TCP_PORT="${PHONE_TCP_PORT:-8000}"
PEER_TCP_PORTS="${PEER_TCP_PORTS:-1234}"
SYNC_INTERVAL="${SYNC_INTERVAL:-2}"
NFT_TABLE="hackschule_workspace"
# The real line of the Telefonspiel (phone_asterisk publishes SIP on 5060):
# only the provider's servers may reach it. Bots on the internet try
# thousands of extensions and passwords a day; none is ever accepted, but
# they need not reach Asterisk at all. The addresses are those of the
# trunk's host name (PHONE_SIP_SERVER in credentials.rb), resolved again
# every SIP_RESOLVE_INTERVAL seconds. The name has several addresses and DNS
# gives one at a time, so the set only grows (read back from the table at
# start); and only what someone else starts is dropped: Asterisk's own
# registration and what the provider sends back over it pass, whichever
# address it uses. Unset (no trunk): no rule.
SIP_TRUNK_HOST="${SIP_TRUNK_HOST:-}"
SIP_PORT="${SIP_PORT:-5060}"
SIP_RESOLVE_INTERVAL="${SIP_RESOLVE_INTERVAL:-300}"
SIP_TABLE="hackschule_sip"

log() {
    printf '[peer-firewall] %s\n' "$*"
}

docker_unversioned_api() {
    curl \
        --silent \
        --show-error \
        --fail \
        --max-time 3 \
        --unix-socket "$DOCKER_SOCKET" \
        "http://localhost$1"
}

wait_for_docker() {
    while :; do
        if version_json="$(docker_unversioned_api /version 2>/dev/null)"; then
            DOCKER_API_VERSION="$(printf '%s' "$version_json" | jq -r '.ApiVersion // empty')"
            if [ -n "$DOCKER_API_VERSION" ]; then
                export DOCKER_API_VERSION
                log "Docker API v${DOCKER_API_VERSION} is available"
                return 0
            fi
        fi
        log 'waiting for Docker API'
        sleep 1
    done
}

docker_api() {
    curl \
        --silent \
        --show-error \
        --fail \
        --max-time 3 \
        --unix-socket "$DOCKER_SOCKET" \
        "http://localhost/v${DOCKER_API_VERSION}$1"
}

render_ruleset() {
    cat <<EOF_RULES
table bridge ${NFT_TABLE} {
    set user_subnets {
        type ipv4_addr
        flags interval
        elements = { ${subnet_elements} }
    }

    set infrastructure {
        type ipv4_addr
EOF_RULES
    if [ -n "$infrastructure_elements" ]; then
        printf '        elements = { %s }\n' "$infrastructure_elements"
    fi
    cat <<EOF_RULES
    }

    set case_servers {
        type ipv4_addr
EOF_RULES
    if [ -n "$case_server_elements" ]; then
        printf '        elements = { %s }\n' "$case_server_elements"
    fi
    cat <<EOF_RULES
    }

    set phone_services {
        type ipv4_addr
EOF_RULES
    if [ -n "$phone_elements" ]; then
        printf '        elements = { %s }\n' "$phone_elements"
    fi
    cat <<EOF_RULES
    }

    chain peer_filter {
        type filter hook forward priority filter - 1; policy accept;

        # Infrastructure services on workspace_user must keep unrestricted
        # access to student containers (nginx/code-server proxying, MySQL,
        # Neo4j and backend health/lifecycle checks).
        ip saddr @infrastructure ip daddr @user_subnets accept
        ip saddr @user_subnets ip daddr @infrastructure accept

        # Student-to-student ICMP remains useful for the networking tutorial.
        ip saddr @user_subnets ip daddr @user_subnets ip protocol icmp accept

        # Direct peer connections are deliberately limited to the TCP port
        # used by the existing netcat tutorial.
        ip saddr @user_subnets ip daddr @user_subnets tcp dport { ${PEER_TCP_PORTS} } accept
        ip saddr @user_subnets ip daddr @user_subnets tcp sport { ${PEER_TCP_PORTS} } ct state established accept

        # SSH to the case server, and its answers on those connections.
        ip saddr @user_subnets ip daddr @case_servers tcp dport 22 accept
        ip saddr @case_servers ip daddr @user_subnets tcp sport 22 ct state established accept

        # The Telefonspiel's speech service, and its answers.
        ip saddr @user_subnets ip daddr @phone_services tcp dport ${PHONE_TCP_PORT} accept
        ip saddr @phone_services ip daddr @user_subnets tcp sport ${PHONE_TCP_PORT} ct state established accept

        # Everything else directly between student-side endpoints is private.
        # bridge/forward cannot use an ICMP reject verdict on the nftables
        # version shipped with Debian Bookworm, so silently drop it here.
        ip saddr @user_subnets ip daddr @user_subnets drop
    }
}
EOF_RULES
}

install_ruleset() {
    # Remove the obsolete L3/inet table from the first version of this helper.
    # Same-bridge Docker traffic can stay entirely in the bridge forwarding path.
    nft delete table inet "$NFT_TABLE" >/dev/null 2>&1 || true

    if nft list table bridge "$NFT_TABLE" >/dev/null 2>&1; then
        {
            printf 'delete table bridge %s\n' "$NFT_TABLE"
            render_ruleset
        } | nft -f -
    else
        render_ruleset | nft -f -
    fi
}

discover_network_state() {
    network_json="$(docker_api "/networks/${WORKSPACE_NETWORK}")" || return 1

    ipv6_enabled="$(printf '%s' "$network_json" | jq -r '.EnableIPv6 // false')"
    if [ "$ipv6_enabled" = 'true' ]; then
        log "ERROR: ${WORKSPACE_NETWORK} has IPv6 enabled; refusing to run with an IPv4-only peer policy"
        return 1
    fi

    subnet_elements="$(
        printf '%s' "$network_json" \
            | jq -r '[.IPAM.Config[]?.Subnet // empty | select(contains(":") | not)] | join(", ")'
    )"

    if [ -z "$subnet_elements" ]; then
        log "ERROR: ${WORKSPACE_NETWORK} has no IPv4 subnet"
        return 1
    fi

    infrastructure_elements="$(ips_with_label "$INFRASTRUCTURE_LABEL")" || return 1
    case_server_elements="$(ips_with_label "$CASE_SERVER_LABEL")" || return 1
    phone_elements="$(ips_with_label "$PHONE_LABEL")" || return 1
}

# The addresses on the workspace network of all running containers with the
# label set to true, separated by commas.
ips_with_label() {
    elements=''
    container_ids="$(
        docker_api '/containers/json' \
            | jq -r --arg label_name "$1" \
                '.[] | select(.Labels[$label_name] == "true") | .Id'
    )" || return 1

    for container_id in $container_ids; do
        container_json="$(docker_api "/containers/${container_id}/json")" || return 1
        ip="$(
            printf '%s' "$container_json" \
                | jq -r --arg network "$WORKSPACE_NETWORK" \
                    '.NetworkSettings.Networks[$network].IPAddress // empty'
        )"
        [ -n "$ip" ] || continue

        if [ -n "$elements" ]; then
            elements="${elements}, ${ip}"
        else
            elements="$ip"
        fi
    done
    printf '%s' "$elements"
}

apply_network_state() {
    # nft executes a batch atomically, so there is no moment where the subnet
    # is known but the infrastructure exception set is half-updated.
    {
        printf 'flush set bridge %s user_subnets\n' "$NFT_TABLE"
        printf 'add element bridge %s user_subnets { %s }\n' "$NFT_TABLE" "$subnet_elements"
        printf 'flush set bridge %s infrastructure\n' "$NFT_TABLE"
        if [ -n "$infrastructure_elements" ]; then
            printf 'add element bridge %s infrastructure { %s }\n' "$NFT_TABLE" "$infrastructure_elements"
        fi
        printf 'flush set bridge %s case_servers\n' "$NFT_TABLE"
        if [ -n "$case_server_elements" ]; then
            printf 'add element bridge %s case_servers { %s }\n' "$NFT_TABLE" "$case_server_elements"
        fi
        printf 'flush set bridge %s phone_services\n' "$NFT_TABLE"
        if [ -n "$phone_elements" ]; then
            printf 'add element bridge %s phone_services { %s }\n' "$NFT_TABLE" "$phone_elements"
        fi
    } | nft -f -
}

# The interface of the default route: where the internet comes in
external_interface() {
    awk '$2 == "00000000" { print $1; exit }' /proc/net/route
}

# The trunk's addresses, IPv4 and IPv6, comma-separated (no v4-mapped
# ones): what the name resolves to now, a few times (DNS answers with one
# address at a time), and what the table allows already
sip_known() {
    nft list set inet "$SIP_TABLE" "$1" 2>/dev/null | sed -n 's/.*elements = { *\(.*\) *}.*/\1/p' | tr ',' '\n' | tr -d ' '
}

sip_trunk_v4() {
    {
        for _ in 1 2 3 4; do getent ahostsv4 "$SIP_TRUNK_HOST" 2>/dev/null | awk '{ print $1 }'; done
        sip_known trunk4
    } | grep -E '^[0-9.]+$' | sort -u | paste -sd, -
}

sip_trunk_v6() {
    {
        for _ in 1 2 3 4; do getent ahostsv6 "$SIP_TRUNK_HOST" 2>/dev/null | awk '$1 ~ /:/ && $1 !~ /^::ffff:/ { print $1 }'; done
        sip_known trunk6
    } | grep ':' | sort -u | paste -sd, -
}

render_sip_ruleset() {
    # forward, before Docker's own filter (priority 0): a packet dropped
    # here never reaches the container; the trunk's own replies and calls
    # come from its addresses and pass
    printf 'table inet %s {\n' "$SIP_TABLE"
    printf '    set trunk4 {\n        type ipv4_addr\n'
    [ -n "$1" ] && printf '        elements = { %s }\n' "$1"
    printf '    }\n    set trunk6 {\n        type ipv6_addr\n'
    [ -n "$2" ] && printf '        elements = { %s }\n' "$2"
    printf '    }\n'
    printf '    chain forward {\n        type filter hook forward priority -5; policy accept;\n'
    printf '        iifname "%s" meta nfproto ipv4 meta l4proto udp ct direction original ct original proto-dst %s ip saddr != @trunk4 counter drop\n' "$3" "$SIP_PORT"
    printf '        iifname "%s" meta nfproto ipv6 meta l4proto udp ct direction original ct original proto-dst %s ip6 saddr != @trunk6 counter drop\n' "$3" "$SIP_PORT"
    printf '    }\n}\n'
}

sip_last=''

apply_sip_guard() {
    [ -n "$SIP_TRUNK_HOST" ] || return 0
    v4="$(sip_trunk_v4)"
    v6="$(sip_trunk_v6)"
    iface="$(external_interface)"
    if [ -z "${v4}${v6}" ] || [ -z "$iface" ]; then
        log "SIP guard: cannot resolve ${SIP_TRUNK_HOST} or find the external interface; keeping the last rule"
        return 0
    fi
    state="${v4}|${v6}|${iface}"
    if [ "$state" = "$sip_last" ] && nft list table inet "$SIP_TABLE" >/dev/null 2>&1; then
        return 0
    fi
    # replaced in one batch: there is no moment without the rule
    if {
        printf 'table inet %s\n' "$SIP_TABLE"
        printf 'delete table inet %s\n' "$SIP_TABLE"
        render_sip_ruleset "$v4" "$v6" "$iface"
    } | nft -f -; then
        sip_last="$state"
        log "SIP guard: UDP ${SIP_PORT} on ${iface} only from ${SIP_TRUNK_HOST} (${v4:-no IPv4}; ${v6:-no IPv6})"
    else
        log 'SIP guard: could not install the rule'
    fi
}

# Do not tear the table down when this helper exits. Keeping the last known
# ruleset is safer during image updates or crashes; the next instance replaces
# it atomically once Docker is available again.

wait_for_docker

while ! discover_network_state; do
    log "waiting for Docker network ${WORKSPACE_NETWORK}"
    sleep 1
done

# The first installed table already contains the subnet and infrastructure
# sets, so there is no fail-open initialization window.
install_ruleset
last_state="${subnet_elements}|${infrastructure_elements}|${case_server_elements}|${phone_elements}"
log "protecting ${WORKSPACE_NETWORK}; peer TCP ports: ${PEER_TCP_PORTS}; peer UDP disabled"
log "active subnets: ${subnet_elements}; infrastructure: ${infrastructure_elements:-none}; case servers: ${case_server_elements:-none}; phone: ${phone_elements:-none}"
apply_sip_guard
sip_checked="$(date +%s)"

while :; do
    # the SIP guard: again after the interval, or at once if its table went
    if [ -n "$SIP_TRUNK_HOST" ]; then
        now="$(date +%s)"
        if [ $((now - sip_checked)) -ge "$SIP_RESOLVE_INTERVAL" ] || { [ -n "$sip_last" ] && ! nft list table inet "$SIP_TABLE" >/dev/null 2>&1; }; then
            apply_sip_guard
            sip_checked="$now"
        fi
    fi

    if discover_network_state; then
        current_state="${subnet_elements}|${infrastructure_elements}|${case_server_elements}|${phone_elements}"

        if ! nft list table bridge "$NFT_TABLE" >/dev/null 2>&1; then
            log 'firewall table disappeared; restoring it'
            install_ruleset
            last_state="$current_state"
        elif [ "$current_state" != "$last_state" ]; then
            apply_network_state
            log "active subnets: ${subnet_elements}; infrastructure: ${infrastructure_elements:-none}; case servers: ${case_server_elements:-none}; phone: ${phone_elements:-none}"
            last_state="$current_state"
        fi
    else
        log 'could not refresh Docker network state; keeping the last firewall state'
    fi

    sleep "$SYNC_INTERVAL"
done
