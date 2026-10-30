#!/usr/bin/env bash
set -euo pipefail

# A guard between stages, not a quota: individual Docker RUN commands can
# consume multiple GB. Keep df -h / open while building and abort if needed.
# a single step of the image (the .NET SDK, Flutter with Android) takes
# several GiB, and layers are written on top: 20 GiB before each stage
min_gib="${MIN_ROOT_FREE_GIB:-20}"
case "$min_gib" in
    ''|*[!0-9]*) echo 'MIN_ROOT_FREE_GIB must be a nonnegative integer' >&2; exit 2 ;;
esac
# The filesystem Docker writes its layers to, which need not be / (a
# second disk under /mnt/docker, say); / if Docker does not say.
target="$(docker info --format '{{.DockerRootDir}}' 2>/dev/null || true)"
[ -d "$target" ] || target=/
available_kib="$(df -Pk "$target" | awk 'NR == 2 {print $4}')"
required_kib="$((min_gib * 1024 * 1024))"

if (( available_kib < required_kib )); then
    echo "Refusing to start another build stage: $target has $(df -h "$target" | tail -1)" >&2
    echo "Minimum before/after each stage: ${min_gib} GiB. Do not fill the live Docker filesystem." >&2
    exit 1
fi

printf 'Space check (%s): %s GiB free (minimum: %s GiB)\n' "$target" \
    "$(LC_ALL=C awk -v k="$available_kib" 'BEGIN { printf "%.1f", k / 1048576 }')" "$min_gib"
