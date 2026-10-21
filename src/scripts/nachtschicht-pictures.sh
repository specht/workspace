#!/bin/sh
# The pictures of the Nachtschicht at the end of Backrooms Level 3
# (nachtschicht-pictures.mjs), from the folder export.sh builds in the
# starter, with its baked light:
#
#   src/scripts/nachtschicht-pictures.sh ../nachtschicht-site
#
# Runs the browser in the generator's image (graphics card, Playwright),
# writes WebP next to Level 3 and .pictures.json, so that
# recreate-screenshots.rb --status lists them and calls them stale when one
# of the two scripts changes.
set -e
site=$(cd "$1" && pwd)
repo=$(cd "$(dirname "$0")/../.." && pwd)
page=$repo/src/content/level-3-4c9e6a2d
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
docker run --rm --gpus all -e NVIDIA_DRIVER_CAPABILITIES=all --shm-size 1g \
    -v "$repo/src/scripts:/scripts:ro" -v "$site:/site:ro" -v "$work:/out" -w /app \
    workspace_tutorial_screenshots sh -c "cp /scripts/nachtschicht-pictures.mjs /app/ && node /app/nachtschicht-pictures.mjs /site /out" 2>&1 \
    | grep -v -e '^$' -e 'Failed to load resource'
for png in "$work"/*.png; do
    cwebp -quiet -q 86 "$png" -o "$page/$(basename "$png" .png).webp"
done
cd "$repo"
ruby -rjson -rdigest -e '
    inputs = ["src/scripts/nachtschicht-pictures.mjs", "src/scripts/nachtschicht-pictures.sh"]
    digest = Digest::SHA256.new
    inputs.sort.each { |file| digest << "#{file}\n" << File.binread(file) << "\n" }
    pictures = Dir["#{ARGV[0]}/*.png"].map { |png| File.basename(png, ".png") + ".webp" }.sort
    File.write("#{ARGV[1]}/.pictures.json", JSON.pretty_generate(
        "script" => "src/scripts/nachtschicht-pictures.sh",
        "command" => "src/scripts/nachtschicht-pictures.sh ../nachtschicht-site   # the folder export.sh builds in the starter",
        "inputs" => inputs, "hash" => digest.hexdigest, "pictures" => pictures) + "\n")
' "$work" "$page"
echo "$(ls "$work"/*.png | wc -l) pictures in $page"
