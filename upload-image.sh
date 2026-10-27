#!/usr/bin/env bash
# Copies the student image built here (build-image.sh) to another server and
# loads it there, for a server whose disk has no room to build it itself
# (docker/code/BUILDING.md, »Space warning«). Only the finished image travels;
# the toolchain images and BuildKit's cache stay here.
#
#   ./upload-image.sh HOST [REMOTE_DIR]
#
# HOST is an SSH host (an entry in ~/.ssh/config or user@host). REMOTE_DIR is
# where the compressed file waits on the server until it is loaded (default:
# the home directory there). Best a disk other than Docker's: then the old
# image can stay until the new one is loaded and checked.
#
# Six steps, each printed with the commands it runs:
#   1. compress the image into a file here, named after the image's ID, so
#      that a second run with the same image skips this step;
#   2. check the server: rsync, zstd and docker there, room for the file in
#      REMOTE_DIR, and which image hs_code_server:latest is there now;
#   3. send the file with rsync; if the connection drops, it tries again
#      (up to UPLOAD_ATTEMPTS times) and continues where it stopped;
#   4. test the file on the server; if Docker has no room there for the new
#      image beside the old one, remove the old one first, but only if no
#      container uses it (otherwise stop and say so);
#   5. load the new image, check that it has the layers of the one built
#      here, start it once (code-server --version), and tag it as candidate;
#   6. remove the old image if no container uses it any more (running
#      Workspaces keep it until they restart; the script says how to remove
#      it later), the file on the server (KEEP_REMOTE_FILE=1 keeps it) and
#      the note of the run.
#
# Run it again as often as needed: after an interruption it continues with
# the step that was not finished (a note on the server, NAME.replaces, keeps
# the image being replaced until the end), and when everything is done it
# changes nothing. Not two runs at the same time. Nothing else on the server
# is removed: no other images, no cache.
#
# Settings by environment: IMAGE (default hs_code_server:latest), WORK_DIR
# for the file here (default ~/.cache/hackschule-images, outside the
# repository), UPLOAD_ATTEMPTS (default 50), ZSTD_LEVEL (default 6),
# KEEP_REMOTE_FILE, CHECK_ONLY=1 to stop after step 2 (the server is looked
# at, nothing is sent or changed there), NO_COLOR=1 for plain text. With pv
# installed (here, on the server) compressing and loading show a progress
# bar.
set -euo pipefail
# The whole script is one block: bash reads it completely before running
# it, so that editing the file during a run cannot break that run.
{
cd "$(dirname "$0")"

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

for tool in docker zstd rsync ssh; do
    command -v "$tool" >/dev/null || fail "Missing here: $tool"
done
have_pv=''
command -v pv >/dev/null && have_pv=1
mkdir -p "$work_dir"

id="$(docker image inspect --format '{{.Id}}' "$image" 2>/dev/null)" ||
    fail "No image $image here; build it with ./build-image.sh."
short="${id#sha256:}"
short="${short:0:12}"
name="$(echo "$repository" | tr '/' '-')-$short.tar.zst"
file="$work_dir/$name"
unpacked_bytes="$(docker image inspect --format '{{.Size}}' "$image")"
# What identifies the image on both sides: its layers. The ID would not do,
# because Docker's classic store and the containerd image store compute it
# from different documents.
fingerprint='docker image inspect --format "{{json .RootFS.Layers}}"'
layers="$(eval "$fingerprint '$image'" | sha256sum | cut -c1-16)"

printf '%s%s%s %s(%s, %s unpacked) to %s%s\n' "$bold" "$image" "$reset" "$dim" "$short" "$(gib "$unpacked_bytes")" "$host" "$reset"

# --- 1. The file here --------------------------------------------------------
step 1 'Compressing the image'
if [ -s "$file" ] && zstd -q -t "$file" 2>/dev/null; then
    ok "Already compressed: $file"
else
    shown "docker save $image | zstd -T0 -$level -o $file.part"
    info 'docker save gathers the layers first: for a minute or two nothing moves'
    if [ -n "$have_pv" ]; then
        docker save "$image" | pv -s "$unpacked_bytes" -N packing -pterab | zstd -T0 -"$level" -q -o "$file.part"
    else
        docker save "$image" | zstd -T0 -"$level" -q -o "$file.part"
    fi
    mv "$file.part" "$file"
fi
file_bytes="$(stat -c %s "$file")"
ok "$(gib "$file_bytes") to send ($file)"

# --- 2. The server -----------------------------------------------------------
step 2 "Checking $host"
remote 'for tool in rsync zstd docker; do command -v $tool >/dev/null || { echo "Missing on the server: $tool" >&2; exit 1; }; done'
remote_pv="$(remote 'command -v pv >/dev/null && echo yes || true')"
if [ -n "${CHECK_ONLY:-}" ]; then
    remote "test -d '$remote_dir'" || fail "No directory $remote_dir on $host."
else
    remote "mkdir -p '$remote_dir'"
fi
remote "test -w '$remote_dir'" ||
    fail "$remote_dir on $host is not writable for the SSH user; take a directory of your own there, for example: ssh $host 'sudo mkdir -p $remote_dir/image-upload && sudo chown \$(id -un) $remote_dir/image-upload', then $remote_dir/image-upload as REMOTE_DIR."
have="$(remote "df -Pk '$remote_dir' | awk 'NR == 2 { print \$4 * 1024 }'")"
partial="$(remote "stat -c %s '$remote_dir/$name' 2>/dev/null || echo 0")"
[ "$have" -ge $((file_bytes - partial)) ] ||
    fail "Not enough room in $remote_dir on $host: $(gib "$have") free, $(gib $((file_bytes - partial))) needed."
info "Room in $remote_dir: $(gib "$have") free"
[ "$partial" -eq 0 ] || info "Already there from an earlier run: $(gib "$partial")"

# A note beside the upload: the ID of the image being replaced, written
# before loading and removed at the very end. While it exists, a run is not
# finished, and a new run picks up where that one stopped.
marker="$remote_dir/$name.replaces"
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

removed_before=''
if [ -z "$loaded_already" ]; then
    # --- 3. Sending, again after a dropped connection ------------------------
    step 3 "Sending to $host:$remote_dir"
    # --append-verify continues a file; a missing one counts as an error
    # (exit code 23) although rsync sends it whole, so it starts empty
    remote "touch '$remote_dir/$name'"
    attempt=1
    until { shown "rsync --partial --append-verify --info=progress2 $file $host:$remote_dir/"
            rsync --partial --append-verify --info=progress2 "$file" "$host:$remote_dir/"; }; do
        [ "$attempt" -lt "$attempts" ] ||
            fail "Gave up after $attempts attempts; run the script again to continue where it stopped."
        attempt=$((attempt + 1))
        warn "Connection lost; attempt $attempt of $attempts in 30 seconds"
        sleep 30
    done
    ok 'Sent'

    # --- 4. Testing the file, room for the image -----------------------------
    step 4 "Testing the file and the room on $host"
    remote "zstd -q -t '$remote_dir/$name'"
    ok 'The file is complete'
    remote "echo '$old' > '$marker'"
    # Docker's directory, and containerd's where Docker keeps images there
    # (the containerd image store): the less free of the two counts
    root="$(remote "docker info --format '{{.DockerRootDir}}'")"
    have="$(remote "for d in '$root' /var/lib/containerd; do [ -d \$d ] && df -Pk \$d | awk 'NR == 2 { print \$4 * 1024 }'; done | sort -n | head -1")"
    info "Room for images ($root, /var/lib/containerd): $(gib "$have") free, $(gib "$unpacked_bytes") needed"
    if [ "$have" -lt "$unpacked_bytes" ]; then
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
fi

# --- 5. Loading and checking the new image -----------------------------------
step 5 "Loading and checking the image on $host"
if [ -z "$loaded_already" ]; then
    if [ -n "$remote_pv" ]; then
        # pv draws on the terminal, so ssh gets one (-t)
        shown "ssh -t $host pv '$remote_dir/$name' | zstd -dc | docker load"
        ssh -t "$host" "pv -s $file_bytes -N loading -pterab '$remote_dir/$name' | zstd -dc | docker load"
    else
        info "No pv on $host: no progress bar while loading (about as long as compressing took)"
        remote "zstd -dc '$remote_dir/$name' | docker load"
    fi
fi
loaded="$(remote "$fingerprint '$image' | sha256sum | cut -c1-16")"
[ "$loaded" = "$layers" ] ||
    fail "$image on $host has other layers than the one built here; the file stays in $remote_dir."
ok 'Its layers are those of the image built here'
remote "docker run --rm --entrypoint /app/code-server/bin/code-server '$image' --version"
ok 'It starts'
remote "docker tag '$image' $repository:candidate"

# --- 6. Removing the old image, the file and the note ------------------------
step 6 "Cleaning up on $host"
if [ -n "$old" ] && [ -z "$removed_before" ] && [ -n "$(remote "docker image inspect --format '{{.Id}}' $old 2>/dev/null || true")" ]; then
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
if [ -z "${KEEP_REMOTE_FILE:-}" ]; then
    remote "rm -f '$remote_dir/$name'"
fi
remote "rm -f '$marker'"
finish_step
printf '\n%s%sDone%s in %s: %s (%s) is on %s; new Workspaces start with it.\n' \
    "$bold" "$green" "$reset" "$(duration "$SECONDS")" "$image" "$short" "$host"
exit 0
}
