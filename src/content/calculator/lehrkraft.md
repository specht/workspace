Lernende bauen einen Rechner, der Ausdrücke wie `2 + 3 * (4 - 1)` versteht, in Ruby oder Python: Sie zerlegen den Text in Tokens (mit einem Automaten wie in <a href='/automata'>»Endliche Automaten«</a>), bauen daraus mit rekursivem Abstieg einen Baum, in dem Punkt vor Strich und Klammern stehen, rechnen den Baum aus, melden Fehler verständlich und erweitern den Rechner um Potenzen (rechtsassoziativ) und Variablen. Ein Baum zum Durchklicken auf der Seite zeigt, in welcher Reihenfolge gerechnet wird.

<table class='table'>
<tr><th>Ablauf</th><td>Erste Doppelstunde: Punkt vor Strich, Einrichten, Zerlegen, warum ein Automat keine Klammern zählen kann. Zweite: der Baum, Ausrechnen, Fehler, Potenzen und Variablen.</td></tr>
<tr><th>Voraussetzungen</th><td>»Endliche Automaten« und Grundlagen in Ruby oder Python: Funktionen, Arrays oder Listen, Rekursion zumindest gesehen.</td></tr>
<tr><th>Dateien</th><td><code>/workspace/calculator/calculator.rb</code> oder <code>calculator.py</code>. Gerüst und Musterlösung beider Sprachen liegen unter <code>stufen/</code> und <code>loesung/</code>; die Prüfungen rufen <code>tokenize</code>, <code>parse</code> und <code>evaluate</code> auf.</td></tr>
</table>
