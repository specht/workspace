# raylib-Vorlage

Vorlage für die Hackschule-Aktivität »Eine Klötzchenwelt in 3D«: 3D-Grafik und
ein kleines Spiel in C mit [raylib](https://www.raylib.com/), übersetzt zu
WebAssembly.

## Übersetzen

Im Hackschule Workspace ist raylib schon installiert:

```sh
make
```

Das erzeugt `main.html`; öffne die Datei mit »Go Live« in der Vorschau. Jedes
andere Programm im Ordner geht genauso: `make PROGRAM=test` macht aus
`test.c` die Datei `test.html`.

## Dateien

- `main.c`: das Programm
- `licht.vs`, `licht.fs`: die Shader für das Licht (Vertex- und Fragment-Shader),
  die in die Seite eingebettet werden
- `Makefile`: übersetzt das Programm; die Webseite drumherum (mit Konsole,
  Vollbild und CRT-Filter) lädt es beim ersten Mal von PixelRAM
