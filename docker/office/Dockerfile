# The office service (docs/plan-textverarbeitung.md): reads the documents
# students upload to a document task and draws their pages. LibreOffice
# without a window (Writer and Calc), in German: a spreadsheet shows
# »#WERT!« and »WAHR« as the programs of the class do (libreoffice-l10n-de;
# server.py starts it with LANG=de_DE.UTF-8). Python with UNO to read the document,
# poppler to turn the PDF into pictures and to find where each paragraph
# landed, and fonts so that a page breaks its lines as in Word:
# Microsoft's core fonts themselves (Arial, Times New Roman, Courier New,
# Verdana, Georgia …; the Debian installer fetches them from their original
# packages while the image is built, under Microsoft's licence for them),
# and for the Office fonts that are not free to download the ones made with
# their widths: Carlito for Calibri, Caladea for Cambria. Aptos (Word's
# default since 2024) is missing: Microsoft wants a licence of its own for
# using it on a server. webp and Pillow are for
# the scripts that draw the pictures of the activities (src/scripts/office).
#
# The code is mounted at /app (src/office). No internet: the service sits
# on a network of its own with the backend only (config.rb).
FROM debian:trixie-slim

RUN sed -i 's/^Components: main$/Components: main contrib/' /etc/apt/sources.list.d/debian.sources \
    && echo 'ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true' | debconf-set-selections
RUN apt-get update && apt-get install -y --no-install-recommends \
        libreoffice-writer-nogui libreoffice-calc-nogui python3-uno python3-docx \
        poppler-utils webp python3-pil \
        ca-certificates wget cabextract ttf-mscorefonts-installer \
        fontconfig fonts-crosextra-carlito fonts-crosextra-caladea \
        fonts-liberation2 fonts-dejavu-core \
        hyphen-de hunspell-de-de libreoffice-l10n-de \
    && rm -rf /var/lib/apt/lists/*
RUN useradd -m -u 1600 office
USER office
ENV HOME=/home/office
CMD ["python3", "-u", "/app/server.py"]
