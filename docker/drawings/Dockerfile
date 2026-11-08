# The tools that make the drawings of the pages, the same on every machine:
# Python with numpy and Pillow for the scripts in bilder/ (diagramme.py,
# kugeln.py), LaTeX and dvisvgm for their labels (src/scripts/diagram_labels.py),
# fonttools with brotli to embed the fonts as WOFF2 (src/scripts/svg_fonts.py),
# rsvg-convert and cwebp for the rare WebP (render-svg.rb --webp), and the
# site's fonts (from the repository), so that a WebP shows the same letters
# as the page.
#
# Not a service: src/scripts/drawings builds it and runs one command in it,
#     src/scripts/drawings python3 src/content/reference/vektoren/bilder/diagramme.py
#     src/scripts/drawings ruby src/scripts/render-svg.rb src/content/dns/dns-tree.svg
FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3

RUN apt-get update && apt-get install -y --no-install-recommends \
        python3 python3-numpy python3-pil python3-fonttools python3-brotli \
        ruby \
        texlive-latex-base texlive-latex-recommended preview-latex-style dvisvgm \
        librsvg2-bin webp \
        fontconfig fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# the fonts of the site (0xProto is in no Debian package), from the
# repository mounted at /repo
COPY fonts.conf /etc/fonts/conf.d/90-site-fonts.conf

# a home for whichever user runs it (src/scripts/drawings: the caller's), with
# the cache of the typeset labels mounted into it
RUN mkdir -p /home/drawings/.cache && chmod -R 777 /home/drawings
ENV HOME=/home/drawings DRAWINGS_CONTAINER=1 LANG=C.UTF-8
