#!/usr/bin/env bash
# Copies the student image built here (build-image.sh) to another server and
# loads it there, for a server whose disk has no room to build it itself
# (docker/code/BUILDING.md, »Space warning«). Only the finished image travels;
# the toolchain images and BuildKit's cache stay here.
#
#   ./upload-image.sh HOST [REMOTE_DIR]
#
# HOST is an SSH host (an entry in ~/.ssh/config or user@host). REMOTE_DIR is
# where the compressed file lands on the server (default: the home directory
# there); put it on a disk with room, not necessarily the one Docker uses.
#
# Steps, each one printed with the commands it runs, each safe to run again:
#   1. compress the image into a file here, named after the image's ID, so
#      that a second run with the same image skips this step;
#   2. check the server: rsync, zstd and docker there, room for the file in
#      REMOTE_DIR, and which image hs_code_server:latest is there now;
#   3. send the file with rsync; if the connection drops, it tries again
#      (up to UPLOAD_ATTEMPTS times) and continues where it stopped;
#   4. test the file on the server; if Docker's directory there has no room
#      for the new image beside the old one, remove the old one first, but
#      only if no container uses it (otherwise stop and say so);
#   5. load the new image, check that it has the layers of the one built
#      here, start it once (code-server --version), and tag it as candidate;
#   6. remove the old image if no container uses it any more (running
#      Workspaces keep it until they restart; then run the script again or
#      remove it by hand), and the file on the server (KEEP_REMOTE_FILE=1
#      keeps it).
#
# Nothing else on the server is removed: no other images, no cache.
#
# Settings by environment: IMAGE (default hs_code_server:latest), WORK_DIR
# for the file here (default ~/.cache/hackschule-images, outside the
# repository), UPLOAD_ATTEMPTS (default 50), ZSTD_LEVEL (default 6),
# KEEP_REMOTE_FILE, and CHECK_ONLY=1 to stop after step 2: the server is
# looked at, nothing is sent or changed there.
set -euo pipefail
cd "$(dirname "$0")"

host="${1:-}"
remote_dir="${2:-.}"
if [ -z "$host" ] || [ "$host" = '-h' ] || [ "$host" = '--help' ]; then
    sed -n '2,/^set -euo/p' "$0" | sed '$d' | sed 's/^# \{0,1\}//'
    exit 2
fi
image="${IMAGE:-hs_code_server:latest}"
work_dir="${WORK_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/hackschule-images}"
mkdir -p "$work_dir"
attempts="${UPLOAD_ATTEMPTS:-50}"
level="${ZSTD_LEVEL:-6}"

for tool in docker zstd rsync ssh; do
    command -v "$tool" >/dev/null || { echo "Missing here: $tool" >&2; exit 1; }
done

id="$(docker image inspect --format '{{.Id}}' "$image" 2>/dev/null)" ||
    { echo "No image $image here; build it with ./build-image.sh." >&2; exit 1; }
short="${id#sha256:}"
short="${short:0:12}"
name="$(echo "${image%%:*}" | tr '/' '-')-$short.tar.zst"
file="$work_dir/$name"
unpacked_bytes="$(docker image inspect --format '{{.Size}}' "$image")"
# What identifies the image on both sides: its layers. The ID would not do,
# because Docker's classic store and the containerd image store compute it
# from different documents.
fingerprint='docker image inspect --format "{{json .RootFS.Layers}}"'
layers="$(eval "$fingerprint '$image'" | sha256sum | cut -c1-16)"
gib() { awk -v b="$1" 'BEGIN { printf "%.1f GiB", b / 1073741824 }'; }

echo "=== $image ($short), $(gib "$unpacked_bytes") unpacked ==="

# 1. The file here
if [ -s "$file" ] && zstd -q -t "$file" 2>/dev/null; then
    echo "=== 1. Already compressed: $file ($(gib "$(stat -c %s "$file")")) ==="
else
    echo "=== 1. Compressing into $file ==="
    echo "+ docker save $image | zstd -T0 -$level -q -o $file.part" >&2
    docker save "$image" | zstd -T0 -"$level" -q -o "$file.part"
    mv "$file.part" "$file"
fi
file_bytes="$(stat -c %s "$file")"
echo "=== $(gib "$file_bytes") to send ==="

# Every command on the server is printed before it runs, on stderr, so
# that what a command prints can be read back.
remote() {
    echo "+ ssh $host $1" >&2
    # shellcheck disable=SC2029  # expanded here on purpose
    ssh "$host" "$1"
}
# Containers on the server that use an image (running or stopped)
users_of() {
    remote "docker ps -a --filter ancestor=$1 --format '{{.Names}} ({{.Status}})'"
}

# 2. The server
echo "=== 2. Checking $host ==="
remote 'for tool in rsync zstd docker; do command -v $tool >/dev/null || { echo "Missing: $tool" >&2; exit 1; }; done'
if [ -n "${CHECK_ONLY:-}" ]; then
    remote "test -d '$remote_dir'" || { echo "No directory $remote_dir on $host." >&2; exit 1; }
else
    remote "mkdir -p '$remote_dir'"
fi
have="$(remote "df -Pk '$remote_dir' | awk 'NR == 2 { print \$4 * 1024 }'")"
partial="$(remote "stat -c %s '$remote_dir/$name' 2>/dev/null || echo 0")"
if [ "$have" -lt $((file_bytes - partial)) ]; then
    echo "Not enough room in $remote_dir on $host: $(gib "$have") free, $(gib $((file_bytes - partial))) needed." >&2
    exit 1
fi
old="$(remote "docker image inspect --format '{{.Id}}' '$image' 2>/dev/null || true")"
old_layers=''
[ -z "$old" ] || old_layers="$(remote "$fingerprint '$image' | sha256sum | cut -c1-16")"
if [ -z "$old" ]; then
    echo "=== $host has no $image yet ==="
elif [ "$old_layers" = "$layers" ]; then
    echo "=== $host already has this image ($short); nothing to do ==="
    exit 0
else
    echo "=== $host has $image ${old#sha256:} now; it gets replaced ==="
fi
if [ -n "${CHECK_ONLY:-}" ]; then
    echo "=== CHECK_ONLY: stopping here; nothing was sent or changed on $host ==="
    exit 0
fi

# 3. Sending, again after a dropped connection
echo "=== 3. Sending to $host:$remote_dir ==="
attempt=1
until { echo "+ rsync --partial --append-verify --progress $file $host:$remote_dir/"
        rsync --partial --append-verify --progress "$file" "$host:$remote_dir/"; }; do
    if [ "$attempt" -ge "$attempts" ]; then
        echo "Gave up after $attempts attempts; run the script again to continue where it stopped." >&2
        exit 1
    fi
    attempt=$((attempt + 1))
    echo "=== Connection lost; attempt $attempt of $attempts in 30 seconds ==="
    sleep 30
done

# 4. Testing the file, room for the image
echo "=== 4. Testing the file on $host ==="
remote "zstd -q -t '$remote_dir/$name'"
# Docker's directory, and containerd's where Docker keeps images there
# (the containerd image store): the less free of the two counts
root="$(remote "docker info --format '{{.DockerRootDir}}'")"
have="$(remote "for d in '$root' /var/lib/containerd; do [ -d \$d ] && df -Pk \$d | awk 'NR == 2 { print \$4 * 1024 }'; done | sort -n | head -1")"
echo "=== Room for images on $host ($root, /var/lib/containerd): $(gib "$have") free, $(gib "$unpacked_bytes") needed ==="
removed_before=''
if [ "$have" -lt "$unpacked_bytes" ]; then
    if [ -z "$old" ]; then
        echo "Not enough room in $root on $host, and no old image to make room with." >&2
        exit 1
    fi
    users="$(users_of "$old")"
    if [ -n "$users" ]; then
        echo "No room beside the old image, and containers still use it:" >&2
        echo "$users" >&2
        echo 'Stop them (their Workspaces) or make room otherwise, then run the script again.' >&2
        exit 1
    fi
    echo "=== No room beside the old image; removing it first (no container uses it) ==="
    # without its last tag the image goes too
    remote "docker image rm $(echo "${image%%:*}"):candidate 2>/dev/null || true"
    remote "docker image rm '$image'"
    removed_before=1
fi

# 5. Loading and checking the new image
echo "=== 5. Loading into Docker on $host ==="
remote "zstd -dc '$remote_dir/$name' | docker load"
loaded="$(remote "$fingerprint '$image' | sha256sum | cut -c1-16")"
if [ "$loaded" != "$layers" ]; then
    echo "$image on $host has other layers than the one built here; the file stays in $remote_dir." >&2
    exit 1
fi
echo "=== $image on $host has the layers of the image built here ==="
remote "docker run --rm --entrypoint /app/code-server/bin/code-server '$image' --version"
remote "docker tag '$image' $(echo "${image%%:*}"):candidate"

# 6. Removing the old image and the file
echo "=== 6. Cleaning up on $host ==="
if [ -n "$old" ] && [ -z "$removed_before" ]; then
    users="$(users_of "$old")"
    if [ -n "$users" ]; then
        echo "=== The old image ${old#sha256:} stays: these containers still use it ==="
        echo "$users"
        echo "=== Once they have restarted with the new image: ssh $host docker image rm $old ==="
    else
        remote "docker image rm $old"
    fi
fi
if [ -z "${KEEP_REMOTE_FILE:-}" ]; then
    remote "rm -f '$remote_dir/$name'"
fi
echo "=== Done: $image ($short) is on $host; new Workspaces start with it ==="
