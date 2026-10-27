Grammatiken zum Lesen und Programmieren, in Ruby oder Python: Lernende leiten Sätze aus einer Grammatik ab, lesen und übersetzen Syntaxdiagramme und EBNF, schreiben aus einer Grammatik für Ausdrücke einen Parser im rekursiven Abstieg, der Wörter annimmt oder ablehnt, und ordnen die Grammatiken in die Chomsky-Hierarchie ein.

<table class='table'>
<tr><th>Voraussetzungen</th><td><ul>
<li>»Kellerautomaten«: Klammern prüfen, der Stack.</li>
<li>Funktionen mit Rückgabewert und Schleifen in Ruby oder Python; dass eine Funktion sich selbst aufrufen kann, erklärt die Seite an einem Beispiel.</li>
</ul></td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Eine Aufgabe, die Lernende in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Regeln, Terminale und Nichtterminale, Ableitung und Syntaxbaum am Satz aus drei Wörtern, Syntax und Semantik, BNF und EBNF, Syntaxdiagramme lesen und in EBNF übersetzen.</li>
<li><b>2. Doppelstunde:</b> der Parser für Klammerwörter zum Abtippen, die Vorhersage zu den offenen Aufrufen, <code>digit</code> und <code>number</code> selbst, dann <code>expression</code>, <code>term</code> und <code>factor</code>; die mehrdeutige Grammatik und die Chomsky-Hierarchie.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/grammatiken</code>: <code>brackets</code>, <code>number</code> und <code>expression</code>, mit <code>.rb</code> in Ruby und <code>.py</code> in Python. <code>brackets</code> tippen die Lernenden von der Seite ab, <code>number</code> beginnt mit einem Anfang von der Seite, <code>expression</code> ist eine Kopie von <code>number</code>.</td></tr>
<tr><th>Prüfung</th><td>Zwei Aufgaben laden die Datei der Lernenden in ihrem Workspace und rufen ihre Funktionen mit eigenen Wörtern auf, als Gast, der in <code>/workspace</code> nichts verändern kann. Die Rückmeldung nennt den ersten Aufruf, der nicht passt. Der Workspace muss dafür laufen.</td></tr>
</table>

#### Die Aufgaben

- **»Wie viele Sätze?«** (12): Wer 7 eingibt, hat addiert statt multipliziert; die Rückmeldung sagt das.
- **»Ein Diagramm lesen«**: `3,` passt nicht, weil nach dem Komma eine Ziffer kommen muss. `007` passt, und darüber lässt sich gut sprechen: Das Diagramm legt die Form fest, nicht, was üblich ist.
- **»Vom Diagramm zur Regel«**: Die drei falschen Regeln erlauben `<>` nicht, erlauben `<1;2;>` oder erlauben `<;5>`; jede Rückmeldung nennt das Wort.
- **»Wie tief geht es?«** ist eine Vorhersage (4). Das Bild mit dem Stack der Aufrufe steht erst nach der Antwort auf der Seite.
- **»Zahlen nach ihrer Regel«** gehört zum Kasten »Jetzt bist du dran«. Die Prüfung ruft `digit` und `number` auch mit einem Index mitten im Wort auf (`number('x42', 1)`); wer immer bei 0 anfängt, scheitert dort.
- **»Ausdrücke prüfen«** ruft `accepts`, `factor` und `term` auf. Wer in `accepts` nur prüft, ob `expression` etwas gelesen hat, nimmt `1+2)` an; die Rückmeldung verweist auf das ganze Wort.
- **»Wie viele Bäume?«** ist eine Vorhersage (2); die beiden Bäume stehen danach auf der Seite.
- **»Wo endet regulär?«**: Nur `factor` enthält sich über `expression` selbst mittendrin. Die Rückmeldungen zu den Regeln mit `{ … }` erklären, dass eine Schleife einem endlichen Automaten reicht.

#### Was schiefgeht

- **`NoMethodError` für `nil` (Ruby) oder `TypeError` mit `NoneType` (Python):** Eine Funktion gibt `nil` oder `None` zurück, und die nächste rechnet damit weiter, etwa `peek(word, i)` mit `i = nil`. Nach jedem Aufruf, der `nil` geben kann, muss die Funktion selbst aufhören.
- **`SystemStackError` (Ruby) oder `RecursionError` (Python):** Eine Funktion ruft sich am Anfang selbst auf, ohne ein Zeichen gelesen zu haben, wie im Kasten »Ausprobieren«. Darum steht die Wiederholung in EBNF als Schleife `{ … }`.
- **Leerzeichen:** Die Grammatik kennt keine; `accepts('2 + 3')` ist `false`. Ein Tokenizer, der sie überspringt, ist Thema von »Ein Rechner für Ausdrücke«.

#### Begriffe

- Die Seite benutzt EBNF in der Schreibweise von Wirth (`=`, `|`, `{ }`, `[ ]`, `( )`, Terminale in Anführungszeichen), dieselbe wie »Ein Rechner für Ausdrücke«. BNF mit spitzen Klammern und `::=` wird nur genannt.
- Ableitungen werden als Kette und als Syntaxbaum gezeigt, jeweils mit dem Ersetzen des am weitesten links stehenden Nichtterminals.
- Die Chomsky-Hierarchie steht als Tabelle mit den passenden Automaten; Typ 1 und 0 werden nur genannt. Für Typ 3 erklärt die Seite die rechtslineare Form und dass EBNF-Schleifen ohne Rekursion mittendrin regulär bleiben.

#### Weiter

»Ein Rechner für Ausdrücke« schließt an: dieselbe Grammatik mit `-` und `/`, ein Tokenizer davor, und die Funktionen bauen den Syntaxbaum und rechnen ihn aus.
