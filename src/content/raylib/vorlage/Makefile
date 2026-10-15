# Übersetzt PROGRAM.c (ohne Angabe: main.c) in eine Webseite PROGRAM.html,
# die du mit »Go Live« in der Vorschau öffnest. raylib ist im Workspace schon
# installiert. Die Shader light.vs und light.fs und die Textur textures.png
# werden in die Seite eingebettet.
#
# Die Webseite um das Programm herum kommt von PixelRAM: mit Konsole für
# printf, Vollbild und CRT-Filter. make lädt sie beim ersten Mal herunter.
PROGRAM := main

PAGE_VERSION := v0.1.22
PAGE_URL := https://raw.githubusercontent.com/specht/pixelram/$(PAGE_VERSION)/shell.html
PAGE := .build/page-$(PAGE_VERSION).html

CFLAGS := -O2 -std=c11 -Wall -Wextra
# Bilder für Texturen: textures.png, wenn es sie gibt
IMAGES := $(wildcard textures.png)

WEB_FLAGS := -DPLATFORM_WEB -sUSE_GLFW=3 -sASYNCIFY -sSINGLE_FILE=1 \
	--embed-file light.vs --embed-file light.fs \
	$(addprefix --embed-file ,$(IMAGES)) \
	--shell-file $(PAGE)

.PHONY: all clean

all: $(PROGRAM).html

$(PAGE):
	@mkdir -p $(dir $@)
	@echo "Lade die Webseite von PixelRAM $(PAGE_VERSION) ..."
	@curl -fsSL "$(PAGE_URL)" | sed 's|<title>PixelRAM</title>|<title>raylib</title>|' > $@.tmp
	@mv $@.tmp $@

$(PROGRAM).html: $(PROGRAM).c light.vs light.fs $(IMAGES) $(PAGE)
	emcc $(CFLAGS) $(WEB_FLAGS) $(PROGRAM).c -lraylib -o $@

clean:
	rm -f $(PROGRAM).html
