#!/usr/bin/env bash
# Copies the student image built here (build-image.sh) to another server and
# loads it there, for a server whose disk has no room to build it itself
# (docker/code/BUILDING.md, »Space warning«). Only the finished image travels;
# the toolchain images and BuildKit's cache stay here.
#
#   ./upload-image.sh HOST [REMOTE_DIR]              # through a registry
#   ./upload-image.sh --archive HOST [REMOTE_DIR]    # the whole image as a file
#
# HOST is an SSH host (an entry in ~/.ssh/config or user@host). REMOTE_DIR is a
# directory on the server the SSH user may write to (default: the home
# directory there); best on a disk other than Docker's.
#
# Through a registry (the default): a registry container on the server, which
# listens only on 127.0.0.1 there and keeps its layers in REMOTE_DIR/registry,
# reached from here through SSH (socat here, docker exec there; no port
# forwarding needed). docker push sends only the layers
# it does not have yet, so after the first run an update sends only what
# changed; docker pull on the server unpacks layer by layer. At the end the
# registry stops and its garbage collection removes the layers no image needs
# any more; REMOTE_DIR/registry stays for the next run.
#
# As an archive (--archive): docker save, compressed with zstd into a file
# here, sent with rsync (which continues after a dropped connection), docker
# load there. Every update sends the whole image, and loading needs about
# twice the image on the server for a moment.
#
# Six steps, each printed with the commands it runs:
#   1. here: what to send (the archive: compressed, named after the image's
#      ID, so that a second run skips it);
#   2. the server: the tools, room in REMOTE_DIR, which image is there now;
#   3. sending (the registry: started, the tunnel, docker push; the archive:
#      rsync), again after a dropped connection, up to UPLOAD_ATTEMPTS times;
#   4. room for the image in Docker's directory there; if there is none
#      beside the old image, the old one goes first, but only if no container
#      uses it (otherwise the script stops and says so);
#   5. the image into Docker there (docker pull, or docker load), checked:
#      the layers of the one built here, started once (code-server
#      --version), tagged as candidate too;
#   6. cleaning up: the old image if no container uses it any more (running
#      Workspaces keep it until they restart; the script says how to remove
#      it later), the registry stopped and collected (the archive: the file,
#      unless KEEP_REMOTE_FILE=1), and the note of the run.
#
# Run it again as often as needed: after an interruption it continues with
# the step that was not finished (a note on the server, REMOTE_DIR/
# hs_code_server-ID.replaces, keeps the image being replaced until the end),
# and when everything is done it changes nothing. Not two runs at the same
# time. Nothing else on the server is removed: no other images, no cache.
#
# Settings by environment: IMAGE (default hs_code_server:latest), WORK_DIR
# for the archive here (default ~/.cache/hackschule-images, outside the
# repository), UPLOAD_ATTEMPTS (default 50), ZSTD_LEVEL (default 6),
# REGISTRY_PORT on the server and TUNNEL_PORT here (both default 5099),
# KEEP_REMOTE_FILE, CHECK_ONLY=1 to stop after step 2 (the server is looked
# at, nothing is sent or changed there), NO_COLOR=1 for plain text.
set -euo pipefail
# The whole script is one block: bash reads it completely before running
# it, so that editing the file during a run cannot break that run.
{
cd "$(dirname "$0")"

method=registry
if [ "${1:-}" = '--archive' ]; then
    method=archive
    shift
fi
host="${1:-}"
remote_dir="${2:-.}"
if [ -z "$host" ] || [ "$host" = '-h' ] || [ "$host" = '--help' ]; then
    sed -n '2,/^set -euo/p' "$0" | sed '$d' | sed 's/^# \{0,1\}//'
    exit 2
fi
image="${IMAGE:-hs_code_server:latest}"
repository="${image%%:*}"
work_dir="${WORK_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/hackschule-images}"
attempts="${UPLOAD_ATTEMPTS:-50}"
level="${ZSTD_LEVEL:-6}"
registry_port="${REGISTRY_PORT:-5099}"
tunnel_port="${TUNNEL_PORT:-5099}"
registry_name='hackschule-upload-registry'

# --- Output: colours on a terminal, plain text elsewhere ---------------------
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
    bold=$'\e[1m' dim=$'\e[2m' cyan=$'\e[36m' green=$'\e[32m' yellow=$'\e[33m' red=$'\e[31m' reset=$'\e[0m'
else
    bold='' dim='' cyan='' green='' yellow='' red='' reset=''
fi
duration() { local s=$1; if [ "$s" -ge 60 ]; then printf '%d min %02d s' $((s / 60)) $((s % 60)); else printf '%d s' "$s"; fi; }
current_step=0
step_started=$SECONDS
finish_step() { [ "$current_step" -gt 0 ] && printf '%s      took %s%s\n' "$dim" "$(duration $((SECONDS - step_started)))" "$reset"; return 0; }
step() {
    finish_step
    current_step=$1
    step_started=$SECONDS
    printf '\n%s%s[%s/6]%s%s %s%s\n' "$bold" "$cyan" "$1" "$reset" "$bold" "$2" "$reset"
}
info() { printf '      %s\n' "$*"; }
ok() { printf '      %s%s%s\n' "$green" "$*" "$reset"; }
warn() { printf '      %s%s%s\n' "$yellow" "$*" "$reset"; }
fail() { printf '\n%s%sStopped:%s %s%s%s\n' "$bold" "$red" "$reset" "$red" "$*" "$reset" >&2; exit 1; }
shown() { printf '%s      $ %s%s\n' "$dim" "$*" "$reset" >&2; }
gib() { awk -v b="$1" 'BEGIN { printf "%.1f GiB", b / 1073741824 }'; }

# Every command on the server is shown before it runs, on stderr, so that
# what a command prints can be read back.
remote() {
    shown "ssh $host $1"
    # shellcheck disable=SC2029  # expanded here on purpose
    ssh "$host" "$1"
}
# Containers on the server that use an image (running or stopped)
users_of() { remote "docker ps -a --filter ancestor=$1 --format '{{.Names}} ({{.Status}})'"; }

needed_here='docker ssh'
[ "$method" = archive ] && needed_here='docker zstd rsync ssh'
[ "$method" = registry ] && needed_here='docker ssh curl socat setsid'
for tool in $needed_here; do
    command -v "$tool" >/dev/null || fail "Missing here: $tool"
done

id="$(docker image inspect --format '{{.Id}}' "$image" 2>/dev/null)" ||
    fail "No image $image here; build it with ./build-image.sh."
short="${id#sha256:}"
short="${short:0:12}"
name="$(echo "$repository" | tr '/' '-')-$short"
unpacked_bytes="$(docker image inspect --format '{{.Size}}' "$image")"
largest_layer="$(docker history -H=false --format '{{.Size}}' "$image" | sort -n | tail -1)"
# What identifies the image on both sides: its layers. The ID would not do,
# because Docker's classic store and the containerd image store compute it
# from different documents.
fingerprint='docker image inspect --format "{{json .RootFS.Layers}}"'
layers="$(eval "$fingerprint '$image'" | sha256sum | cut -c1-16)"

printf '%s%s%s %s(%s, %s unpacked) to %s, %s%s\n' "$bold" "$image" "$reset" "$dim" "$short" "$(gib "$unpacked_bytes")" "$host" \
    "$([ "$method" = registry ] && echo 'through a registry' || echo 'as an archive')" "$reset"

# --- 1. Here -----------------------------------------------------------------
file="$work_dir/$name.tar.zst"
if [ "$method" = archive ]; then
    step 1 'Compressing the image'
    mkdir -p "$work_dir"
    if [ -s "$file" ] && zstd -q -t "$file" 2>/dev/null; then
        ok "Already compressed: $file"
    else
        shown "docker save $image | zstd -T0 -$level -o $file.part"
        info 'docker save gathers the layers first: for a minute or two nothing moves'
        if command -v pv >/dev/null; then
            docker save "$image" | pv -s "$unpacked_bytes" -N packing -pterab | zstd -T0 -"$level" -q -o "$file.part"
        else
            docker save "$image" | zstd -T0 -"$level" -q -o "$file.part"
        fi
        mv "$file.part" "$file"
    fi
    file_bytes="$(stat -c %s "$file")"
    ok "$(gib "$file_bytes") to send ($file)"
else
    step 1 'The image here'
    info "$(docker image inspect --format '{{len .RootFS.Layers}}' "$image") layers, the largest $(gib "$largest_layer"); the registry gets only those it does not have"
fi

# --- 2. The server -----------------------------------------------------------
step 2 "Checking $host"
if [ "$method" = archive ]; then
    remote 'for tool in rsync zstd docker; do command -v $tool >/dev/null || { echo "Missing on the server: $tool" >&2; exit 1; }; done'
else
    remote 'command -v docker >/dev/null || { echo "Missing on the server: docker" >&2; exit 1; }'
fi
remote_pv="$(remote 'command -v pv >/dev/null && echo yes || true')"
if [ -n "${CHECK_ONLY:-}" ]; then
    remote "test -d '$remote_dir'" || fail "No directory $remote_dir on $host."
else
    remote "mkdir -p '$remote_dir'"
fi
remote "test -w '$remote_dir'" ||
    fail "$remote_dir on $host is not writable for the SSH user; take a directory of your own there, for example: ssh $host 'sudo mkdir -p $remote_dir/image-upload && sudo chown \$(id -un) $remote_dir/image-upload', then $remote_dir/image-upload as REMOTE_DIR."
remote_abs="$(remote "cd '$remote_dir' && pwd")"
have="$(remote "df -Pk '$remote_abs' | awk 'NR == 2 { print \$4 * 1024 }'")"
if [ "$method" = archive ]; then
    partial="$(remote "stat -c %s '$remote_abs/$name.tar.zst' 2>/dev/null || echo 0")"
    [ "$have" -ge $((file_bytes - partial)) ] ||
        fail "Not enough room in $remote_abs on $host: $(gib "$have") free, $(gib $((file_bytes - partial))) needed."
    [ "$partial" -eq 0 ] || info "Already there from an earlier run: $(gib "$partial")"
else
    storage="$remote_abs/registry"
    kept="$(remote "du -sb '$storage' 2>/dev/null | cut -f1 || echo 0")"
    # gzip in the registry: about half the unpacked image the first time
    if [ "${kept:-0}" -eq 0 ] && [ "$have" -lt $((unpacked_bytes / 2)) ]; then
        fail "Not enough room for the registry in $remote_abs on $host: $(gib "$have") free, about $(gib $((unpacked_bytes / 2))) needed the first time."
    fi
    [ "${kept:-0}" -eq 0 ] || info "The registry has kept $(gib "$kept") from earlier runs; only what is new goes over the line"
fi
info "Room in $remote_abs: $(gib "$have") free"

# A note beside the upload: the ID of the image being replaced, written
# before the new one goes into Docker and removed at the very end. While it
# exists, a run is not finished, and a new run picks up where that one
# stopped.
marker="$remote_abs/$name.replaces"
current="$(remote "docker image inspect --format '{{.Id}}' '$image' 2>/dev/null || true")"
current_layers=''
[ -z "$current" ] || current_layers="$(remote "$fingerprint '$image' | sha256sum | cut -c1-16")"
pending="$(remote "test -e '$marker' && echo yes || true")"
loaded_already=''
if [ -n "$current" ] && [ "$current_layers" = "$layers" ]; then
    if [ -z "$pending" ]; then
        finish_step
        printf '\n%s%sNothing to do:%s %s already has this image, and no earlier run is unfinished.\n' "$bold" "$green" "$reset" "$host"
        exit 0
    fi
    loaded_already=1
    old="$(remote "cat '$marker'")"
    warn "$host has this image already; finishing the run that loaded it"
else
    old="$current"
    [ -z "$pending" ] || old="$(remote "cat '$marker'")"
    if [ -z "$current" ]; then
        info "$host has no $image yet"
    else
        info "$host has $image ${current#sha256:}; it gets replaced"
    fi
fi
if [ -n "${CHECK_ONLY:-}" ]; then
    finish_step
    printf '\n%s%sCHECK_ONLY:%s stopping here; nothing was sent or changed on %s.\n' "$bold" "$yellow" "$reset" "$host"
    exit 0
fi

# --- The registry and the tunnel ---------------------------------------------
tunnel_pid=''
# socat runs in a process group of its own, so that the connections it
# forked (each an ssh) end with it
close_tunnel() { [ -z "$tunnel_pid" ] || kill -- "-$tunnel_pid" 2>/dev/null || true; tunnel_pid=''; }
trap close_tunnel EXIT
open_tunnel() {
    close_tunnel
    # Not ssh -L: servers that forbid port forwarding (AllowTcpForwarding no,
    # no-port-forwarding in authorized_keys) refuse it. Instead, socat here
    # takes each connection of docker push and runs a command on the server
    # for it, nc inside the registry's container (busybox in registry:2),
    # which only needs docker there. One SSH connection each, not a shared
    # one: docker keeps idle connections open, more than sshd's 10 sessions
    # per connection.
    local relay="ssh $host docker exec -i $registry_name nc 127.0.0.1 5000"
    shown "socat TCP-LISTEN:$tunnel_port,bind=127.0.0.1,fork,reuseaddr SYSTEM:'$relay' &"
    # quiet: it says »exiting on signal 15« for every connection at the end
    setsid socat "TCP-LISTEN:$tunnel_port,bind=127.0.0.1,fork,reuseaddr" "SYSTEM:$relay" 2>/dev/null &
    tunnel_pid=$!
    for _ in $(seq 1 30); do
        curl -s -o /dev/null "http://127.0.0.1:$tunnel_port/v2/" && return 0
        kill -0 "$tunnel_pid" 2>/dev/null || fail "The tunnel to $host did not open; is port $tunnel_port here taken? TUNNEL_PORT=… takes another."
        sleep 1
    done
    fail "The registry on $host does not answer through the tunnel; try it by hand: ssh $host docker exec $registry_name wget -qO- http://127.0.0.1:5000/v2/"
}
start_registry() {
    # :Z labels the directory for the container where SELinux is on
    remote "docker start $registry_name >/dev/null 2>&1 || docker run -d --name $registry_name -p 127.0.0.1:$registry_port:5000 -v '$storage:/var/lib/registry:Z' registry:2 >/dev/null"
}
stop_registry() {
    remote "docker rm -f $registry_name >/dev/null 2>&1 || true"
}
upload_tag="127.0.0.1:$tunnel_port/$repository:upload"
pull_tag="127.0.0.1:$registry_port/$repository:upload"

removed_before=''
if [ -z "$loaded_already" ]; then
    # --- 3. Sending, again after a dropped connection ------------------------
    if [ "$method" = archive ]; then
        step 3 "Sending to $host:$remote_abs"
        # --append-verify continues a file; a missing one counts as an error
        # (exit code 23) although rsync sends it whole, so it starts empty
        remote "touch '$remote_abs/$name.tar.zst'"
        attempt=1
        until { shown "rsync --partial --append-verify --info=progress2 $file $host:$remote_abs/"
                rsync --partial --append-verify --info=progress2 "$file" "$host:$remote_abs/"; }; do
            [ "$attempt" -lt "$attempts" ] ||
                fail "Gave up after $attempts attempts; run the script again to continue where it stopped."
            attempt=$((attempt + 1))
            warn "Connection lost; attempt $attempt of $attempts in 30 seconds"
            sleep 30
        done
        ok 'Sent'
        remote "zstd -q -t '$remote_abs/$name.tar.zst'"
        ok 'The file is complete'
    else
        step 3 "Pushing to a registry on $host"
        start_registry
        open_tunnel
        shown "docker tag $image $upload_tag"
        docker tag "$image" "$upload_tag"
        attempt=1
        until { shown "docker push $upload_tag"; docker push "$upload_tag"; }; do
            [ "$attempt" -lt "$attempts" ] ||
                fail "Gave up after $attempts attempts; run the script again to continue where it stopped."
            attempt=$((attempt + 1))
            warn "Push interrupted; attempt $attempt of $attempts in 30 seconds"
            sleep 30
            open_tunnel
        done
        shown "docker image rm $upload_tag"
        docker image rm "$upload_tag" >/dev/null
        close_tunnel
        ok 'Pushed; the registry has every layer of the image'
    fi

    # --- 4. Room for the image -----------------------------------------------
    step 4 "The room on $host"
    remote "echo '$old' > '$marker'"
    # Docker's directory, and containerd's where Docker keeps images there
    # (the containerd image store): the less free of the two counts.
    # docker load unpacks the layers through temporary files, so while it
    # runs it needs about twice the image (measured on the production
    # server, 28 October 2026: 31 GB for an image of 16.6 GiB); docker pull
    # unpacks layer by layer and needs the image and its largest layer.
    root="$(remote "docker info --format '{{.DockerRootDir}}'")"
    have="$(remote "for d in '$root' /var/lib/containerd; do [ -d \$d ] && df -Pk \$d | awk 'NR == 2 { print \$4 * 1024 }'; done | sort -n | head -1")"
    if [ "$method" = archive ]; then
        needed=$((unpacked_bytes * 2))
    else
        needed=$((unpacked_bytes + largest_layer))
    fi
    info "Room for images ($root): $(gib "$have") free, $(gib "$needed") needed for the new image"
    if [ "$have" -lt "$needed" ]; then
        [ -n "$current" ] || fail "Not enough room for the image on $host, and no old image to make room with."
        users="$(users_of "$current")"
        if [ -n "$users" ]; then
            printf '%s\n' "$users" >&2
            fail 'No room beside the old image, and the containers above still use it. Stop them (their Workspaces) or make room otherwise, then run the script again.'
        fi
        warn 'No room beside the old image; removing it first (no container uses it)'
        # without its last tag the image goes too
        remote "docker image rm $repository:candidate 2>/dev/null || true"
        remote "docker image rm '$image'"
        removed_before=1
    else
        ok 'Room beside the old image; it stays until the new one is checked'
    fi

    # --- 5. Into Docker ------------------------------------------------------
    step 5 "The image into Docker on $host"
    if [ "$method" = archive ]; then
        if [ -n "$remote_pv" ]; then
            # pv draws on the terminal, so ssh gets one (-t)
            shown "ssh -t $host pv '$remote_abs/$name.tar.zst' | zstd -dc | docker load"
            ssh -t "$host" "pv -s $file_bytes -N loading -pterab '$remote_abs/$name.tar.zst' | zstd -dc | docker load"
        else
            info "No pv on $host: no progress bar while loading (about as long as compressing took)"
            remote "zstd -dc '$remote_abs/$name.tar.zst' | docker load"
        fi
    else
        start_registry
        # docker pull draws its progress on the terminal, so ssh gets one (-t)
        shown "ssh -t $host docker pull $pull_tag"
        ssh -t "$host" "docker pull $pull_tag"
        remote "docker tag $pull_tag '$image' && docker image rm $pull_tag >/dev/null"
    fi
else
    step 5 "The image on $host"
fi
loaded="$(remote "$fingerprint '$image' | sha256sum | cut -c1-16")"
[ "$loaded" = "$layers" ] ||
    fail "$image on $host has other layers than the one built here."
ok 'Its layers are those of the image built here'
remote "docker run --rm --entrypoint /app/code-server/bin/code-server '$image' --version"
ok 'It starts'
remote "docker tag '$image' $repository:candidate"

# --- 6. Cleaning up ----------------------------------------------------------
step 6 "Cleaning up on $host"
if [ -n "$old" ] && [ "$old" != "$(remote "docker image inspect --format '{{.Id}}' '$image'")" ] && [ -z "$removed_before" ] &&
   [ -n "$(remote "docker image inspect --format '{{.Id}}' $old 2>/dev/null || true")" ]; then
    users="$(users_of "$old")"
    if [ -n "$users" ]; then
        warn "The old image ${old#sha256:} stays: these containers still use it"
        printf '%s\n' "$users" | sed 's/^/        /'
        warn "Once they have restarted with the new image: ssh $host docker image rm $old"
    else
        remote "docker image rm $old"
        ok 'Old image removed'
    fi
fi
if [ "$method" = archive ]; then
    [ -n "${KEEP_REMOTE_FILE:-}" ] || remote "rm -f '$remote_abs/$name.tar.zst'"
else
    stop_registry
    # the tag now names only the new image: what the old one alone needed goes
    shown "ssh $host docker run --rm -v $storage:/var/lib/registry:Z registry:2 garbage-collect --delete-untagged /etc/docker/registry/config.yml"
    ssh "$host" "docker run --rm -v '$storage:/var/lib/registry:Z' registry:2 garbage-collect --delete-untagged /etc/docker/registry/config.yml" | tail -1
    kept="$(remote "du -sb '$storage' | cut -f1")"
    ok "The registry keeps $(gib "$kept") in $storage for the next run"
fi
remote "rm -f '$marker'"
finish_step
printf '\n%s%sDone%s in %s: %s (%s) is on %s; new Workspaces start with it.\n' \
    "$bold" "$green" "$reset" "$(duration "$SECONDS")" "$image" "$short" "$host"
exit 0
}
