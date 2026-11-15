Lernende bauen einen Rechner, der Ausdrücke wie `2 + 3 * (4 - 1)` versteht, in Ruby oder Python: Sie zerlegen den Text in Tokens (mit einem Automaten wie in <a href='/automata'>»Endliche Automaten«</a>), bauen daraus mit rekursivem Abstieg einen Baum, in dem Punkt vor Strich und Klammern stehen, rechnen den Baum aus, melden Fehler verständlich und erweitern den Rechner um Potenzen (rechtsassoziativ) und Variablen. Ein Baum zum Durchklicken auf der Seite zeigt, in welcher Reihenfolge gerechnet wird.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>einen Ausdruck mit Punkt vor Strich, Klammern und Potenzen von Hand als Baum zeichnen und auswerten,</li>
<li>erklären, warum ein endlicher Automat Klammern nicht zählen kann und ein rekursiver Parser es kann,</li>
<li>eine Grammatik in Funktionen übersetzen, die sich gegenseitig aufrufen (rekursiver Abstieg),</li>
<li>begründen, warum <code>2 ^ 3 ^ 2</code> von rechts gerechnet wird, und das im eigenen Parser umsetzen.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Punkt vor Strich, Einrichten, Zerlegen, warum ein Automat keine Klammern zählen kann.</li>
<li><b>2. Doppelstunde:</b> der Baum, Ausrechnen, Fehler, Potenzen und Variablen.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>»Endliche Automaten« und Grundlagen in Ruby oder Python: Funktionen, Arrays oder Listen, Rekursion zumindest gesehen.</td></tr>
<tr><th>Dateien</th><td><code>/workspace/calculator/calculator.rb</code> oder <code>calculator.py</code>. Gerüst und Musterlösung beider Sprachen liegen unter <code>stufen/</code> und <code>loesung/</code>; die Prüfungen rufen <code>tokenize</code>, <code>parse</code> und <code>evaluate</code> auf.</td></tr>
</table>

#### Tipps in Stufen

»Zerlegen«, »Ein Baum aus Tokens«, »Ausrechnen«, »Fehler«, »Hoch« und »Variablen« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Eigene Projekte

Wie man ein Projekt mit Meilensteinen und Kriterien begleitet: [Eigene Projekte begleiten](/lehrmaterial-projekte).

Nach der Seite haben die Lernenden eine Grammatik, einen Parser und einen Baum, den sie ausrechnen. Die Ideen erweitern die Sprache; jede beginnt mit der neuen Regel in der Grammatik, und bewertet wird an Beispielen, die die Lernenden vorher aufschreiben, mit dem Ergebnis, das sie erwarten.

- **Funktionen** (eine Doppelstunde): `sqrt(16)` und `max(3, 7)`. Die Grammatik bekommt eine Regel für einen Aufruf mit einer Liste von Ausdrücken; zu klären ist, was `max()` ohne Argumente tun soll. *Meilensteine:* die neue Regel in der Grammatik und Beispiele mit dem erwarteten Ergebnis; `sqrt(16)` wird ausgerechnet; Aufrufe mit mehreren Argumenten und falsche Aufrufe im Vorführen. *Kriterien:* Alle vorher aufgeschriebenen Beispiele ergeben das erwartete Ergebnis, ein falscher Aufruf gibt eine verständliche Fehlermeldung, und die Lernenden erklären die neue Regel am Baum eines Aufrufs.
- **Rechnen mit Einheiten** (ein Projekt): `3 km + 200 m` ergibt `3.2 km`, `5 m * 2 s` eine neue Einheit, `3 km + 2 s` einen Fehler. Jeder Wert im Baum trägt dann eine Zahl und eine Einheit; die Fehlermeldungen aus [»Fehler«](#fehler) bekommen neue Fälle. *Meilensteine:* die Grammatik mit Einheiten und eine Liste von Beispielen samt Ergebnis; Addieren gleicher Einheiten; Umrechnen, neue Einheiten und Fehler im Vorführen. *Kriterien:* Die Beispiele stimmen, auch die, die einen Fehler ergeben sollen, und die Lernenden erklären, wie ein Wert im Baum Zahl und Einheit zusammen weitergibt.
- **Eine kleine Programmiersprache** (ein Projekt, für Starke): Vergleiche, `if` und eine Schleife; aus `evaluate` wird ein Interpreter, der mit den [Variablen](#variablen) schon ein Gedächtnis hat. Zum Vorführen läuft ein kleines Programm, das jemand anderes geschrieben hat. *Meilensteine:* die Grammatik für Vergleiche, `if` und Schleife mit einem Beispielprogramm; Vergleiche und `if` laufen; die Schleife läuft mit dem Programm von jemand anderem. *Kriterien:* Das fremde Programm läuft und gibt aus, was seine Autorin oder sein Autor erwartet, und die Lernenden erklären, wie `evaluate` eine Schleife ausführt.
