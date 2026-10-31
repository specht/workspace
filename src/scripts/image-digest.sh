#!/bin/sh
# The digest of an image tag, for pinning a base image (FROM name:tag@digest):
# the digest of the manifest list, the same for every platform. When a
# Dockerfile moves to a new tag, its digest changes with it.
#
#   src/scripts/image-digest.sh nginx:1.30.4
#   src/scripts/image-digest.sh linuxserver/code-server:4.140.0
set -eu
[ $# -ge 1 ] || { echo "usage: $0 IMAGE:TAG …" >&2; exit 1; }
for image in "$@"; do
    digest=$(docker buildx imagetools inspect "$image" --format '{{.Manifest.Digest}}')
    [ -n "$digest" ] || { echo "$image: no digest found" >&2; exit 1; }
    echo "$image@$digest"
done
