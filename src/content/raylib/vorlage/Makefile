# Übersetzt PROGRAM.c (ohne Angabe: main.c) in eine Webseite PROGRAM.html,
# die du mit »Go Live« in der Vorschau öffnest. raylib ist im Workspace schon
# installiert. Die Shader light.vs und light.fs und die Textur textures.png
# werden in die Seite eingebettet.
#
# Die Webseite um das Programm herum kommt von PixelRAM: mit Konsole für
# printf, Vollbild und CRT-Filter. make lädt beim ersten Mal die neueste
# Version herunter und nimmt danach immer diese Kopie in .build, auch ohne
# Internet. make pixelram-update holt die neueste Version wieder.
PROGRAM := main

PIXELRAM_GIT := https://github.com/specht/pixelram.git
PAGE_VERSION := $(shell ls .build/page-v*.html 2>/dev/null | sed 's|.*/page-||; s|\.html$$||' | sort -V | tail -n 1)
ifeq ($(PAGE_VERSION),)
PAGE_VERSION := $(shell git ls-remote --tags --refs --sort=-v:refname $(PIXELRAM_GIT) 'v*' 2>/dev/null | head -n 1 | sed 's|.*refs/tags/||')
endif
PAGE_URL := https://raw.githubusercontent.com/specht/pixelram/$(PAGE_VERSION)/shell.html
PAGE := .build/page-$(PAGE_VERSION).html

CFLAGS := -O2 -std=c11 -Wall -Wextra
# Bilder für Texturen: textures.png, wenn es sie gibt
IMAGES := $(wildcard textures.png)

WEB_FLAGS := -DPLATFORM_WEB -sUSE_GLFW=3 -sASYNCIFY -sSINGLE_FILE=1 \
	--embed-file light.vs --embed-file light.fs \
	$(addprefix --embed-file ,$(IMAGES)) \
	--shell-file $(PAGE)

.PHONY: all clean pixelram-update

all: $(PROGRAM).html

$(PAGE):
	@if [ -z "$(PAGE_VERSION)" ]; then \
		echo "Keine Version von PixelRAM gefunden: Ist GitHub erreichbar? (git ls-remote $(PIXELRAM_GIT))"; \
		exit 1; \
	fi
	@mkdir -p $(dir $@)
	@echo "Lade die Webseite von PixelRAM $(PAGE_VERSION) ..."
	@curl -fsSL "$(PAGE_URL)" -o $@.download || { rm -f $@.download; echo "Die Webseite von PixelRAM ließ sich nicht laden: $(PAGE_URL)"; exit 1; }
	@sed 's|<title>PixelRAM</title>|<title>raylib</title>|' $@.download > $@.tmp && rm -f $@.download
	@mv $@.tmp $@

$(PROGRAM).html: $(PROGRAM).c light.vs light.fs $(IMAGES) $(PAGE)
	emcc $(CFLAGS) $(WEB_FLAGS) $(PROGRAM).c -lraylib -o $@

clean:
	rm -f $(PROGRAM).html

# Die heruntergeladene Webseite vergessen und mit der neuesten bauen.
pixelram-update:
	rm -f .build/page-*.html
	$(MAKE) PROGRAM=$(PROGRAM)
