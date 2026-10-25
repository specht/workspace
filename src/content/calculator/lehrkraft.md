Lernende bauen einen Rechner, der Ausdrücke wie `2 + 3 * (4 - 1)` versteht, in Ruby oder Python: Sie zerlegen den Text in Tokens (mit einem Automaten wie in <a href='/automata'>»Endliche Automaten«</a>), bauen daraus mit rekursivem Abstieg einen Baum, in dem Punkt vor Strich und Klammern stehen, rechnen den Baum aus, melden Fehler verständlich und erweitern den Rechner um Potenzen (rechtsassoziativ) und Variablen. Ein Baum zum Durchklicken auf der Seite zeigt, in welcher Reihenfolge gerechnet wird.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Punkt vor Strich, Einrichten, Zerlegen, warum ein Automat keine Klammern zählen kann.</li>
<li><b>2. Doppelstunde:</b> der Baum, Ausrechnen, Fehler, Potenzen und Variablen.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>»Endliche Automaten« und Grundlagen in Ruby oder Python: Funktionen, Arrays oder Listen, Rekursion zumindest gesehen.</td></tr>
<tr><th>Dateien</th><td><code>/workspace/calculator/calculator.rb</code> oder <code>calculator.py</code>. Gerüst und Musterlösung beider Sprachen liegen unter <code>stufen/</code> und <code>loesung/</code>; die Prüfungen rufen <code>tokenize</code>, <code>parse</code> und <code>evaluate</code> auf.</td></tr>
</table>

#### Tipps in Stufen

»Zerlegen«, »Ein Baum aus Tokens«, »Ausrechnen«, »Fehler«, »Hoch« und »Variablen« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.
