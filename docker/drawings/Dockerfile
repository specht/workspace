# The tools that make the drawings of the pages, the same on every machine:
# Python with numpy and Pillow for the scripts in bilder/ (diagramme.py,
# kugeln.py; gcc for render_large.py), LaTeX and dvisvgm for their labels (src/scripts/diagram_labels.py),
# fonttools with brotli to embed the fonts as WOFF2 (src/scripts/svg_fonts.py),
# rsvg-convert and cwebp for the rare WebP (render-svg.rb --webp), and the
# site's fonts (from the repository), so that a WebP shows the same letters
# as the page.
#
# Not a service: src/scripts/drawings builds it and runs one command in it,
#     src/scripts/drawings python3 src/content/reference/vektoren/bilder/diagramme.py
#     src/scripts/drawings ruby src/scripts/render-svg.rb src/content/dns/dns-tree.svg
FROM debian:trixie-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2

RUN apt-get update && apt-get install -y --no-install-recommends \
        python3 python3-numpy python3-pil python3-fonttools python3-brotli \
        ruby \
        texlive-latex-base texlive-latex-recommended preview-latex-style dvisvgm \
        gcc libc6-dev \
        librsvg2-bin webp \
        fontconfig fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# the fonts of the site (0xProto is in no Debian package), from the
# repository mounted at /repo
COPY fonts.conf /etc/fonts/conf.d/90-site-fonts.conf

# fonts the drawings show as samples (»Schrift mit Absicht«, the names of
# fonts, the person boxes of Morison and Carter): the Microsoft core fonts
# (Times New Roman, Arial, Verdana, Georgia, Comic Sans, Impact, Courier
# New) through Debian's installer, which fetches them from Microsoft's
# original packages; Open Sans and Roboto Slab from Debian; Space Grotesk,
# Didact Gothic (OFL) and PxPlus IBM VGA 8x16 (the screen of an old PC, from VileR's Ultimate Oldschool PC Font Pack, CC BY-SA 4.0), licences beside them, from fonts/
RUN sed -i 's/^Components: main$/Components: main contrib/' /etc/apt/sources.list.d/debian.sources \
    && echo 'ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true' | debconf-set-selections \
    && apt-get update && apt-get install -y --no-install-recommends \
        ttf-mscorefonts-installer wget ca-certificates cabextract \
        fonts-open-sans fonts-roboto-slab \
    && rm -rf /var/lib/apt/lists/*
COPY fonts/ /usr/local/share/fonts/samples/
RUN fc-cache -f >/dev/null

# a home for whichever user runs it (src/scripts/drawings: the caller's), with
# the cache of the typeset labels mounted into it
RUN mkdir -p /home/drawings/.cache && chmod -R 777 /home/drawings
ENV HOME=/home/drawings DRAWINGS_CONTAINER=1 LANG=C.UTF-8
