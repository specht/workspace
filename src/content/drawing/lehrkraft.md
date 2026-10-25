Ein Einstieg in Grafikprogrammierung ohne Vorkenntnisse: Lernende zeichnen in Ruby Pixel für Pixel ein Bild, erst eine Wiese mit Regenbogen nach Anleitung, dann ein eigenes, und sehen jeden Befehl sofort auf der Zeichenfläche. Ruby wird nur so weit gebraucht, wie es die Befehle verlangen; Schleifen kommen erst beim Regenbogen und beim Himmel vor.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten, der erste Pixel, die Wiese mit Regenbogen und das gespeicherte Bild.</li>
<li><b>2. Doppelstunde:</b> das eigene Bild mit den Ideen am Ende der Seite, dann Abgabe.</li>
</ul></td></tr>
<tr><th>Einrichten</th><td>Die Erweiterung »Pixelflow Canvas« (<code>gymnasiumsteglitz.pixelflow-canvas</code>) fragt beim ersten Mal, ob man dem Herausgeber vertraut. Das Rubygem landet in <code>/workspace/.gem</code>; beides bleibt nach einem Neustart erhalten.</td></tr>
<tr><th>Abgabe</th><td>»Dein eigenes Bild« kopiert <code>bild.rb</code> und <code>bild.png</code> aus <code>/workspace</code>. In der Klassenübersicht öffnet ein Klick auf die Abgabe den Code; <code>bild.png</code> steht links in der Dateiliste.</td></tr>
</table>

#### Was schiefgeht

- Fast alle Fragen in der ersten Stunde sind dieselbe Meldung: `Connection refused … port 19223`. Die Zeichenfläche ist dann nicht offen, oft weil der Tab geschlossen wurde. Das Programm verbindet sich mit der Zeichenfläche und kann ohne sie nicht zeichnen.
- Die zweite häufige Stelle ist das gespeicherte Bild: `save_as_png` schreibt in den Ordner, in dem `ruby` aufgerufen wurde. Wer im Terminal mit `cd` woanders steht, findet `rainbow.png` dort statt in `/workspace`.

#### Die Aufgaben

»Das erste Programm« und »Das Bild als Datei« sehen nur nach, ob `rainbow.rb` als Text und `rainbow.png` als echtes PNG in `/workspace` liegen. Die übrigen fragen nach dem, was man auf der Zeichenfläche beobachten kann. »Ein Eimer Farbe im Himmel« und »Ohne Maske« sind Vorhersagen: Für die Klassenübersicht zählt die erste Antwort, danach sollen die Lernenden es ausprobieren.

#### Weiter

Wer schneller fertig ist, findet in der Dokumentation des Gems Animationen (`ensure_max_fps`) und Mausereignisse (`fetch_events`). Für Echtzeitgrafik und Spiele führt [Echtzeitgrafik mit C](/pixelram) weiter.
