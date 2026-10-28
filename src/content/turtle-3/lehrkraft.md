Der dritte Teil der Turtle-Grafik, nach <a href='/turtle-2'>Turtle: Bäume und Kurven</a>: Farben aus Zahlen (der Farbkreis und das Mischen zweier Farben), `push` und `pop` zum Merken und Zurückkehren, der Zufall und Pflanzen aus Wörtern (Lindenmayer-Systeme). Die Aufgaben mit einer Vorlage üben je eine dieser Ideen; die Aufgaben mit Zufall und die eigenen Bilder prüft niemand, jedes Bild ist dort richtig. Ruby, Python oder JavaScript.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/turtle-2'>Turtle: Bäume und Kurven</a> (ein rekursiver Baum). Für die Pflanzen aus Wörtern eine Schleife über die Buchstaben eines Strings und das Ersetzen in einem String (<a href='/kata-texts'>Katas: Texte</a>).</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein; JavaScript wählen die Lernenden auf der Seite selbst. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td>Zwei Doppelstunden, die erste bis einschließlich »Zufall«.<ul>
<li>Farben aus Zahlen (3 Aufgaben): eine Sonne aus 36 Farben, eine Spirale durch den Farbkreis, ein Fächer, dessen Farbe von Gold nach Rot übergeht.</li>
<li>Merken und zurückkehren (1 Vorhersage, 1 Aufgabe): wo die Schildkröte nach <code>pop</code> steht, der Baum aus dem zweiten Teil ohne Rückweg.</li>
<li>Zufall (3 Aufgaben ohne Vorlage): ein Spaziergang, ein Baum im Wind, eine Wiese.</li>
<li>Pflanzen aus Wörtern (2 Aufgaben, 1 ohne Vorlage): ein Wort Buchstabe für Buchstabe zeichnen, ein Wort nach einer Regel wachsen lassen, eine eigene Pflanze. Vor der zweiten zeigt ein Bild Stufe für Stufe, wie jedes <code>F</code> der Regel zur Pflanze der Stufe davor wird.</li>
<li>Dein Bild (1 Aufgabe ohne Vorlage).</li>
</ul></td></tr>
<tr><th>Neue Befehle</th><td><code>color 200</code> (eine Farbe aus dem Farbkreis: 0 Rot, 120 Grün, 240 Blau), <code>color "gold", "crimson", 0.25</code> (gemischt), <code>push</code> und <code>pop</code> (merken sich Ort, Richtung, Stift, Farbe und Breite und kehren dorthin zurück, wie <code>save()</code> und <code>restore()</code> am Canvas im Browser), <code>random(1, 6)</code> (eine ganze Zufallszahl, mit Kommazahlen eine Kommazahl). In Python und JavaScript mit Klammern.</td></tr>
<tr><th>Prüfung</th><td>Wie in <a href='/turtle-1'>Turtle: Bilder aus Befehlen</a>: Der Server zeichnet die Befehle nach und vergleicht sie mit dem Bild der Aufgabe, bei den Farbaufgaben auch die Farben. Die Aufgaben mit Zufall und die eigenen Bilder sind gelöst, sobald die Schildkröte etwas zeichnet.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Aufgabe, für jede Sprache getrennt, bis die Person ihn löscht; sichtbar nur für die Person selbst. Lehrkräfte sehen im Klassenboard nur, ob eine Aufgabe gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- In Ruby ist `i / 18` eine ganze Zahl, fast immer 0: Der Fächer bleibt golden. Der zweite Tipp nennt `i / 18.0`.
- Ein `pop` ohne `push` davor bricht das Programm mit einer Meldung ab; ein vergessenes `pop` lässt den nächsten Ast an der Spitze des letzten wachsen.
- Beim Wachsen nacheinander zwei Regeln mit `gsub` oder `replace` anwenden: Die zweite ersetzt auch, was die erste gerade eingesetzt hat. Mit zwei Regeln baut `grow` das neue Wort Buchstabe für Buchstabe (die Lösung der eigenen Pflanze zeigt es).

#### Für die Besprechung

- Wie lang wird das Wort? Aus einem `F` werden 11 Zeichen, dann 61, dann 311: Jede Stufe macht es etwa fünfmal so lang.
- Die eingereichten Bilder vom Ende eignen sich für einen Rundgang.
