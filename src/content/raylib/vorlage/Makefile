# Übersetzt PROGRAM.c (ohne Angabe: main.c) in eine Webseite PROGRAM.html,
# die du mit »Go Live« in der Vorschau öffnest. raylib ist im Workspace schon
# installiert. Die Shader licht.vs und licht.fs und die Textur texturen.png
# werden in die Seite eingebettet.
#
# Die Webseite um das Programm herum kommt von PixelRAM: mit Konsole für
# printf, Vollbild und CRT-Filter. make lädt sie beim ersten Mal herunter.
PROGRAM := main

SEITE_VERSION := v0.1.22
SEITE_URL := https://raw.githubusercontent.com/specht/pixelram/$(SEITE_VERSION)/shell.html
SEITE := .build/seite-$(SEITE_VERSION).html

CFLAGS := -O2 -std=c11 -Wall -Wextra
# Bilder für Texturen: texturen.png, wenn es sie gibt
BILDER := $(wildcard texturen.png)

WEB_FLAGS := -DPLATFORM_WEB -sUSE_GLFW=3 -sASYNCIFY -sSINGLE_FILE=1 \
	--embed-file licht.vs --embed-file licht.fs \
	$(addprefix --embed-file ,$(BILDER)) \
	--shell-file $(SEITE)

.PHONY: all clean

all: $(PROGRAM).html

$(SEITE):
	@mkdir -p $(dir $@)
	@echo "Lade die Webseite von PixelRAM $(SEITE_VERSION) ..."
	@curl -fsSL "$(SEITE_URL)" | sed 's|<title>PixelRAM</title>|<title>raylib</title>|' > $@.tmp
	@mv $@.tmp $@

$(PROGRAM).html: $(PROGRAM).c licht.vs licht.fs $(BILDER) $(SEITE)
	emcc $(CFLAGS) $(WEB_FLAGS) $(PROGRAM).c -lraylib -o $@

clean:
	rm -f $(PROGRAM).html
