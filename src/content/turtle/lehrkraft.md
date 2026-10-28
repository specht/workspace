Ein Einstieg ins Programmieren mit Bildern: Lernende steuern eine Schildkröte, die eine Linie hinter sich herzieht, mit einfachen Befehlen ohne Objekte (`forward 100`, `left 90`). Über Wiederholungen, eigene Funktionen mit Parametern, Stift und Farbe und eine Variable, die wächst, kommen sie zu Spiralen, Sternen und einem eigenen Bild; als Extras rekursive Bäume und Kurven: ein Baum, ein schöner Baum in Braun und Grün, das Sierpiński-Dreieck, die Koch-Kurve und die Hilbert-Kurve. Ruby, Python oder JavaScript. Die Schildkröte geht auf Logo zurück (Seymour Papert, Wally Feurzeig, Cynthia Solomon, 1967).

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine. Neben <a href='/pip-1'>Pip: Hinab in die Stollen</a> ein zweiter Einstieg; wer Pip kennt, kennt Schleifen schon.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein; JavaScript wählen die Lernenden auf der Seite selbst. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td>Zwei Doppelstunden, die erste bis einschließlich »Eigene Befehle«.<ul>
<li>Die Schildkröte (1 Vorhersage, 3 Aufgaben): eine Treppe, ein Quadrat, die Vorhersage, um wie viel sich die Schildkröte an der Ecke eines Dreiecks dreht, das Dreieck.</li>
<li>Wiederholen (2 Aufgaben): das Quadrat mit einer Schleife in wenigen Zeilen, ein Sechseck.</li>
<li>Eigene Befehle (3 Aufgaben): <code>square(size)</code>, Vielecke mit beliebig vielen Ecken, eine Blume aus Quadraten.</li>
<li>Stift und Farbe (2 Aufgaben): eine gestrichelte Linie, drei bunte Quadrate.</li>
<li>Spiralen (1 Vorhersage, 1 Aufgabe): eine eckige Spirale mit einer Variablen, die Vorhersage, was 91 statt 90 Grad tun.</li>
<li>Sterne (1 Aufgabe) und die Geschichte der Schildkröte (Logo, »Mindstorms«).</li>
<li>Kreise und Federn (2 Aufgaben): <code>circle(radius)</code> als Vieleck mit 36 Ecken über den Umfang 2 · π · r, eine Feder aus 30 Kreisen.</li>
<li>Eigene Bilder (3 Aufgaben ohne Vorlage, verteilt über die Seite): ein Muster in höchstens sechs Zeilen, eine Blume, ein Bild mit mindestens zwei Farben; dazu ein eigener Baum bei den Extras.</li>
<li>Bäume und Kurven, Extras (10): Sterne mit beliebig vielen Zacken, ein rekursiver Baum, ein schöner, ein schiefer und ein hängender Baum, ein eigener Baum, ein Farnblatt, das Sierpiński-Dreieck, die Koch-Kurve, die Hilbert-Kurve. Wie eine rekursive Figur aufgebaut ist, zeigt vor jeder ein Bild, das man Stufe für Stufe durchklickt; die Teile aus der Stufe davor sind farbig.</li>
</ul></td></tr>
<tr><th>Technik</th><td>Das Programm läuft im Browser, nicht im Workspace: Ruby und Python als WebAssembly, beim ersten Lauf einmal geladen (Python etwa 6 MB, Ruby etwa 5 MB, danach aus dem Cache des Browsers). Die Schildkröte zeichnet, während das Programm läuft; die Geschwindigkeit lässt sich auch mitten im Lauf ändern, »Stopp« beendet auch eine Endlosschleife.</td></tr>
<tr><th>Prüfung</th><td>Endet ein Programm von selbst, gehen seine Befehle an den Server. Er zeichnet sie nach und vergleicht die Linien mit dem grauen Bild der Aufgabe, gleich in welcher Reihenfolge, in welcher Richtung oder in wie vielen Stücken sie gezeichnet sind. Was fehlt, zeigt die Karte gestrichelt, was zu viel ist, rot. Bei Aufgaben mit einer eigenen Funktion ruft die Prüfung sie mehrmals mit anderen Werten auf, jedes Mal auf einem frischen Blatt. Die beiden Aufgaben »Viermal dasselbe« und »Ein Sechseck« verlangen höchstens drei oder vier Zeilen, also eine Schleife. Beim eigenen Bild zählen nur mindestens 20 Linien und zwei Farben.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Aufgabe, für jede Sprache getrennt, bis die Person ihn löscht, auch ein angehaltenes Programm; sichtbar nur für die Person selbst. Lehrkräfte sehen im Klassenboard nur, ob eine Aufgabe gelöst ist. Ein eingereichtes Bild erscheint als Code in den Einreichungen, ohne Namen, bis du sie einschaltest.</td></tr>
</table>

#### Was schiefgeht

- An der Ecke eines Dreiecks dreht sich die Schildkröte um 120 Grad, nicht um 60: Sie dreht sich um so viel, wie sich ihre Richtung ändert. Die Vorhersage greift genau das auf; die Seite erklärt danach, dass alle Drehungen einer umrundeten Figur zusammen 360 Grad ergeben, und darauf bauen Sechseck, Vieleck und Stern.
- Ruby rechnet `360 / 7` als ganze Zahl, 51, und das Siebeneck schließt sich nicht; die Prüfung ruft `polygon` deshalb auch mit sieben Ecken auf, und der dritte Tipp nennt `360.0`. In Python tut das nur `//`.
- Bei einer Funktion zählt nur, was ihr Aufruf zeichnet: Wer im Programm `square(150)` richtig zeichnet, aber in der Funktion `150` statt `size` schreibt, fällt bei `square(40)` auf.
- Gespiegelt ist falsch: Ein Stern mit `left 144` statt `right 144` liegt auf der anderen Seite der Linie; das graue Bild zeigt, wohin er gehört.
- Läuft ein Programm endlos, prüft der Workspace nichts; »Stopp« hält es an und behält den Code.

#### Für die Besprechung

- Eine Schildkröte, die man selbst spielt: Lass ein Quadrat oder einen Stern im Raum ablaufen und dabei die Drehungen zählen, wie es Papert in »Mindstorms« beschreibt.
- Die Spirale mit 91 Grad zeigt, wie eine kleine Änderung, oft wiederholt, das ganze Bild verändert.
- Wer bei den Bäumen angekommen ist, kann sie mit Zufall wachsen lassen (der Kasten »Ausprobieren« dort); geprüft wird das nicht.
- Die eingereichten Bilder eignen sich für einen Rundgang am Ende der zweiten Doppelstunde.
