Rekursion zum Zuschauen: Lernende zeichnen mit der Schildkröte aus <a href='/turtle-1'>Turtle: Bilder aus Befehlen</a> Figuren, die aus kleineren Kopien ihrer selbst bestehen. Ein Baum, ein schöner Baum in Braun und Grün, ein schiefer und ein hängender Baum, ein Farnblatt, das Sierpiński-Dreieck, die Koch-Kurve, als Extra die Hilbert-Kurve und zum Schluss ein eigener Baum. Ruby, Python oder JavaScript.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/turtle-1'>Turtle: Bilder aus Befehlen</a> (die Befehle, eigene Funktionen, Stift und Farbe) und <a href='/kata-recursion'>Katas: Rekursion</a> (eine Funktion, die sich selbst aufruft, mit einem Fall, der aufhört).</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein; JavaScript wählen die Lernenden auf der Seite selbst. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Ein Baum (4 Aufgaben): ein rekursiver Baum, ein schöner in Braun und Grün, ein schiefer und einer, der bis zum Boden hängt.</li>
<li>Ein Farnblatt (1 Aufgabe).</li>
<li>Das Sierpiński-Dreieck und die Koch-Kurve (2 Aufgaben), mit der Personenbox zu Wacław Sierpiński.</li>
<li>Als Extra: die Hilbert-Kurve.</li>
<li>Zum Schluss ein eigener Baum ohne Vorlage.</li>
</ul>Vor jeder Figur steht ein Bild, das man Stufe für Stufe durchklickt: Jede Stufe zeigt die Teile aus der Stufe davor in Farbe und, blass dahinter, die Stufe davor selbst. Statt eines Rezepts im Aufgabentext steht so vor Augen, wie sich eine Stufe aus der davor zusammensetzt.</td></tr>
<tr><th>Prüfung</th><td>Wie in <a href='/turtle-1'>Turtle: Bilder aus Befehlen</a>: Der Server zeichnet die Befehle nach und vergleicht die Linien mit dem grauen Bild, gleich in welcher Reihenfolge sie gezeichnet sind. Bei Aufgaben mit einer eigenen Funktion ruft die Prüfung sie mit mehreren Tiefen auf. Beim eigenen Baum zählt nichts: Jeder Baum mit wenigstens einer Linie ist gelöst.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Aufgabe, für jede Sprache getrennt, bis die Person ihn löscht; sichtbar nur für die Person selbst. Lehrkräfte sehen im Klassenboard nur, ob eine Aufgabe gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die Schildkröte kehrt nicht an den Anfang zurück: Wer am Ende einer Stufe das `back` vergisst oder sich nicht zurückdreht, setzt den zweiten Ast an die Spitze des ersten. Das Bild der Stufen zeigt, wo jeder Teilbaum beginnt.
- Ohne Fall, der aufhört, ruft sich die Funktion immer weiter auf, bis das Programm mit einer Fehlermeldung abbricht; geprüft wird dann nichts.
- Große Tiefen zeichnen Zehntausende Linien: »Schnell« oder »Sofort« lohnen sich, auch mitten im Lauf.

#### Für die Besprechung

- Wer bei den Bäumen angekommen ist, kann sie mit Zufall wachsen lassen (der Kasten »Ausprobieren« dort); geprüft wird das nicht.
- Wie viele Linien zeichnet ein Baum der Tiefe 5, 6, 10? Jede Stufe verdoppelt die Äste, und dieselbe Frage stellt sich bei jeder Rekursion mit zwei Aufrufen.
