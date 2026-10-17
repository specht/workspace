# Die Überwachungskameras des Museums

Bilder der drei Kameras aus der Tatnacht für »Ein Krimi im Terminal«. Hier
liegen die Ausgangsbilder, so wie ChatGPT sie geliefert hat (als JPEG,
Qualität 92), und was man braucht, um daraus die Aufnahmen der Kameras zu
machen oder ein Bild zu verbessern.

- `cam1/`, `cam2/`, `cam3/`: die Ausgangsbilder. `empty.jpg` ist der
  leere Raum, aus dem die anderen Bilder der Kamera bearbeitet wurden;
  `cam2/open-nobody.jpg` ist nur Ausgangsbild für die Bilder mit offener Tür.
- `frames.txt`: welches Bild zu welcher Uhrzeit auf welcher Kamera.
- `prompts.md`: wie die Bilder entstanden sind, und was sich dabei bewährt hat.
- `build.py`: macht daraus die Aufnahmen.
- `font/`: die VGA-Schrift der Einblendung, »Px437 IBM VGA 9x16« aus dem
  Ultimate Oldschool PC Font Pack von VileR (https://int10h.org/oldschool-pc-fonts/),
  CC BY-SA 4.0 (`font/LICENSE.txt`).

## Aufnahmen bauen

```bash
python3 -m venv /tmp/cctv
/tmp/cctv/bin/pip install opencv-python-headless pillow numpy
/tmp/cctv/bin/python src/content/terminal-mystery/beispiele/cctv/build.py /tmp/cctv-frames
```

Jede Aufnahme heißt nach Kamera und Zeit, etwa `cam2_20260314_231202.jpg`:
704×576 Pixel wie ein alter Rekorder, Kamera 1 schwarzweiß (im Dunkeln),
die anderen in blassen Farben, weich, verrauscht, mit Vignette und starker
JPEG-Kompression. Oben links stehen Datum und Uhrzeit, unten links die
Kamera, in der VGA-Schrift mit verdoppelten Pixeln. Dasselbe Bild kommt bei
jedem Lauf gleich heraus.

`build.py` verwendet jedes Bild ganz. Es schneidet nichts aus, verschiebt
nichts und malt nichts hinein: Montagen fallen auf. Ist ein Bild falsch
(Gehrichtung, verschobene Gegenstände, eine Tür, die falsch aufgeht), wird
es mit ChatGPT neu erzeugt (`prompts.md`) und hier ersetzt.

## Ein Bild verbessern

1. In ChatGPT das Ausgangsbild der Kamera hochladen (meist `empty.jpg`) und
   mit dem Prompt aus `prompts.md` bearbeiten lassen.
2. Prüfen: Blick und Gegenstände unverändert, Gehrichtung passt zur Folge,
   Türen gehen richtig auf (nach außen, Bänder rechts).
3. Das Bild hier unter demselben Namen ablegen, bei einem neuen Bild eine
   Zeile in `frames.txt` ergänzen, und `build.py` laufen lassen.
