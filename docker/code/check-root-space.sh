#!/usr/bin/env bash
set -euo pipefail

# A guard between stages, not a quota: individual Docker RUN commands can
# consume multiple GB. Keep df -h / open while building and abort if needed.
min_gib="${MIN_ROOT_FREE_GIB:-1}"
case "$min_gib" in
    ''|*[!0-9]*) echo 'MIN_ROOT_FREE_GIB must be a nonnegative integer' >&2; exit 2 ;;
esac
available_kib="$(df -Pk / | awk 'NR == 2 {print $4}')"
required_kib="$((min_gib * 1024 * 1024))"

if (( available_kib < required_kib )); then
    echo "Refusing to start another build stage: root has $(df -h / | tail -1)" >&2
    echo "Minimum before/after each stage: ${min_gib} GiB. Do not fill the live Docker filesystem." >&2
    exit 1
fi

printf 'Root space check: %s GiB free (minimum: %s GiB)\n' \
    "$(LC_ALL=C awk -v k="$available_kib" 'BEGIN { printf "%.1f", k / 1048576 }')" "$min_gib"
