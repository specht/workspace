Lernende bauen in TIC-80 ihr erstes eigenes Spiel, mit allem, was dazugehört: Code in Lua, Figuren, Töne. Nach einem gemeinsamen Spiel (eine Figur fängt fallende Sterne) entsteht ein eigenes. TIC-80 ist dafür ideal, weil seine Grenzen (240 × 136 Pixel, 16 Farben) Entscheidungen erzwingen und weil alles in einem Fenster liegt. Lua wird unterwegs erklärt; wer Variablen, `if` und Schleifen aus einer anderen Sprache kennt, kommt schnell voran.

<table class='table'>
<tr><th>Ablauf</th><td>Erste Doppelstunde: TIC-80 kennenlernen, das Beispielprogramm, Speichern im Workspace, der Bildschirm, die Figur. Zweite: ein fallender Stern, Fangen, eine eigene Figur. Dritte: viele Sterne, Leben, Ton, dann das eigene Spiel und die Abgabe.</td></tr>
<tr><th>Speichern</th><td>TIC-80 speichert in <code>/workspace/TIC-80</code>. Als <code>stars.lua</code> ist das Spiel eine Textdatei: Code oben, Sprites und Töne als Kommentarblöcke darunter. Ändert sich eine Datei außerhalb von TIC-80, während dort an derselben gearbeitet wird, bleibt sie, und TIC-80 legt seinen Stand daneben (<code>stars (TIC-80).lua</code>).</td></tr>
<tr><th>Abgabe</th><td>»Dein Spiel« kopiert eine <code>.lua</code>-Datei aus <code>TIC-80</code>. In der Klassenübersicht ist ihr Code lesbar; spielen lässt sie sich, indem man sie in den eigenen Ordner <code>TIC-80</code> legt und in TIC-80 mit <code>load</code> lädt.</td></tr>
</table>

**Was schiefgeht.** Speichern ohne Endung (`save stars`) legt eine binäre `stars.tic` an, die die Aufgaben nicht lesen können; die Karte sagt dann, dass `stars.lua` fehlt. Lua-Fehler erscheinen beim Start (<kbd>Strg</kbd><kbd>R</kbd>) in der Konsole, mit Zeilennummer. Umlaute zeigt TIC-80 nicht an. Und wer im Spiel <kbd>Esc</kbd> drückt, landet in der Konsole, nicht im Editor; ein zweites <kbd>Esc</kbd> führt dorthin.

**Die Aufgaben.** »Im Workspace gespeichert«, »Ein Stern fällt«, »Viele Sterne«, »Ein Ton beim Fangen« und »Ein Himmel« lesen `TIC-80/stars.lua` und suchen typische Bausteine (`function TIC`, `math.random`, `for`, `sfx(`, `pix(`); die Karte nennt die Zeile, in der sie etwas gefunden hat. »Deine Figur« sieht nach, ob Sprite 1 anders aussieht als die mitgelieferte Figur. »Ohne cls« ist eine Vorhersage.

**Weiter.** Für alle, die schneller sind, sind die Ideen am Ende der Seite gedacht; der Rekord mit `pmem` und eine Karte im Map-Editor sind die ergiebigsten. Gute Spiele von anderen gibt es auf [tic80.com](https://tic80.com/play): Dort lässt sich jedes Spiel auch im Code-Editor ansehen.
