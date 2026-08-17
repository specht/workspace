#!/bin/sh
set -eu

DOCKER_SOCKET="${DOCKER_SOCKET:-/var/run/docker.sock}"
WORKSPACE_NETWORK="${WORKSPACE_NETWORK:-workspace_user}"
INFRASTRUCTURE_LABEL="${INFRASTRUCTURE_LABEL:-hackschule.workspace.peer_firewall.infrastructure}"
PEER_TCP_PORTS="${PEER_TCP_PORTS:-1234}"
SYNC_INTERVAL="${SYNC_INTERVAL:-2}"
NFT_TABLE="hackschule_workspace"

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

    infrastructure_elements=''
    container_ids="$(
        docker_api '/containers/json' \
            | jq -r --arg label_name "$INFRASTRUCTURE_LABEL" \
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

        if [ -n "$infrastructure_elements" ]; then
            infrastructure_elements="${infrastructure_elements}, ${ip}"
        else
            infrastructure_elements="$ip"
        fi
    done

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
    } | nft -f -
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
last_state="${subnet_elements}|${infrastructure_elements}"
log "protecting ${WORKSPACE_NETWORK}; peer TCP ports: ${PEER_TCP_PORTS}; peer UDP disabled"
log "active subnets: ${subnet_elements}; infrastructure: ${infrastructure_elements:-none}"

while :; do
    if discover_network_state; then
        current_state="${subnet_elements}|${infrastructure_elements}"

        if ! nft list table bridge "$NFT_TABLE" >/dev/null 2>&1; then
            log 'firewall table disappeared; restoring it'
            install_ruleset
            last_state="$current_state"
        elif [ "$current_state" != "$last_state" ]; then
            apply_network_state
            log "active subnets: ${subnet_elements}; infrastructure: ${infrastructure_elements:-none}"
            last_state="$current_state"
        fi
    else
        log 'could not refresh Docker network state; keeping the last firewall state'
    fi

    sleep "$SYNC_INTERVAL"
done
