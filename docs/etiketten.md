# Etiketten am Tag der offenen Tür

Besucher gestalten auf `/etiketten` einen Aufkleber, und der
ZPL-Etikettendrucker am Stand druckt ihn sofort. Das geht auf dem eigenen
Handy über einen QR-Code, auf den Tablets und Rechnern am Stand, ohne Konto.

Der Editor soll es leicht machen, dass etwas gut aussieht:

- Stift und Radierer in drei Stärken; Füllen mit acht Mustern (Schwarz,
  Weiß, Raster, Punkte, Streifen, Linien, Mauer, Verlauf); Linie, Rechteck und
  Kreis, als Umriss oder mit Muster gefüllt.
- »Spiegeln«: links und rechts, oder in alle vier Ecken; jeder Strich, jede
  Form, jede Füllung und jedes Bild kommt gespiegelt mit.
- »Pixel«: alles auf großen Pixeln von 8 × 8 Punkten, mit Raster.
- Bilder (Bootstrap Icons), Text in vier Schriften und ein Foto schweben
  über dem Aufkleber, bis man »Fertig« drückt: verschieben, Größe, schwarz
  oder weiß. Ein Foto wird auf dem Gerät mit Atkinson-Dithering zu Punkten.
- Sechs Rahmen (Rand, Doppelt, Ticket, Punkte, Namensschild »HALLO ich
  bin«, Sprechblase) und »Umkehren« für Weiß auf Schwarz.
- Auf breiten Bildschirmen stehen die Werkzeuge neben dem Aufkleber, auf
  Handys darunter; der Aufkleber bleibt beim Scrollen oben.

## Am Tag selbst

1. Unter »Administration«, »Etikettendrucker« (`/etiketten-station`) die
   Etiketten für 2, 4, 8 oder 12 Stunden einschalten. Jedes Einschalten macht
   einen neuen QR-Code und einen neuen Schlüssel für den Laptop; der alte
   druckt nicht mehr.
2. Auf dem Laptop, an dessen USB der Drucker hängt, `label-station.rb` von
   der Seite laden und mit dem Befehl starten, der dort steht. Es braucht nur
   Ruby, ohne Gems. Unter Linux schreibt es nach `/dev/usb/lp0`; wird das
   verweigert, braucht der Benutzer die Gruppe `lp` (oder `sudo`). Ein Drucker
   in CUPS geht mit `--lp NAME`, zum Ausprobieren ohne Drucker schreibt
   `--dry-run ORDNER` jedes Etikett in eine Datei.
3. Den QR-Code ausdrucken oder auf einem Bildschirm zeigen; die Rechner am
   Stand öffnen dieselbe Adresse (»Seite öffnen«).

Die Seite zeigt, ob der Laptop gerade fragt, und alle Etiketten der letzten
zwei Tage mit »nochmal« und »löschen«.

## Wie es gebaut ist

- `src/ruby/include/label_printer.rb`: Größe, ZPL, Schalter, Wartezeit;
  getestet in `test/label_printer_test.rb`.
- `src/ruby/include/label_printer_api.rb`: Schalter, Warteschlange
  (`LabelJob`), Drucken für Besucher, `next` und `done` für den Laptop.
- `src/static/etiketten.html`, `include/labels.js`: die Seite zum Malen.
  Nach jedem Strich wird jeder Punkt schwarz oder weiß, so sieht man, was
  gedruckt wird.
- `src/static/etiketten-station.html`, `include/label_station.js`: die Seite
  der Administration.
- `src/scripts/label-station.rb`: das Programm auf dem Laptop. Der Server des
  Workspace erreicht den Drucker in der Schule nicht; der Laptop fragt alle
  zwei Sekunden nach dem nächsten Etikett.

Das Etikett hat die Maße des Druckers der alten Hackschule: 57 × 31,5 mm bei
8 Punkten je Millimeter, mit einem Millimeter Rand ringsum 472 × 268 Punkte,
und dieselbe Verschiebung `^LT0^LS-189^LH0,0`. Sitzt der Druck auf einem
anderen Drucker schief, ersetzt `--home` diese Verschiebung.

Gegen zu viele Etiketten: Drucken geht nur mit dem Schlüssel aus dem
QR-Code, solange eingeschaltet ist; jeder Browser wartet 20 Sekunden bis zum
nächsten Etikett; höchstens 15 warten gleichzeitig. Gespeichert wird nur das
Bild mit Uhrzeit, ohne Namen, zwei Tage lang (`privacy.rb`). Ein Aufkleber
mit Foto (die Seite sagt es mit `photo: true`) wird gelöscht, sobald der
Laptop ihn gedruckt meldet, ungedruckt nach zwei Stunden; das Foto selbst
verlässt das Gerät nicht.

## Offen

- Rätsel wie in der alten Hackschule: eine kleine Logikaufgabe, und wer sie
  löst, bekommt ein Pixelbild gedruckt. Die Pixelbilder der alten Hackschule
  stammen von Reddit ohne Lizenz; es bräuchte eigene Bilder oder die
  Erlaubnis der Zeichner.
