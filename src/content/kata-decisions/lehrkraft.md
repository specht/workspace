Lernende schreiben Funktionen mit Entscheidungen direkt in die Karten der Seite, in Ruby, Python oder JavaScript: `if`, `elsif`/`elif` und `else`, Vergleiche, `and` und `or`, und Funktionen, die `true` oder `false` zurückgeben. Am Ende stehen FizzBuzz und die Frage, was für ein Dreieck drei Seiten ergeben.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen mit Parametern und Rückgabe, etwa aus den <a href='/kata-functions'>Katas: Erste Funktionen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): gerade, die größere Zahl ohne <code>max</code>, der Betrag ohne <code>abs</code>, volljährig.</li>
<li>Übung (4): Schaltjahre, Noten aus Punkten, Eintrittspreise, die größte von dreien; überall zählen die Grenzen.</li>
<li>Knobelei (3): FizzBuzz, Dreiecke, Pythagoras in jeder Reihenfolge.</li>
<li>Extra (1): Schere, Stein, Papier.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Jede Kata ruft die Funktion mit festen Werten auf, besonders auf den Grenzen; die größere Zahl, die größte von dreien und FizzBuzz zusätzlich mit Werten, die für jede Person anders sind. Wo eine eingebaute Funktion die Aufgabe lösen würde (<code>max</code>, <code>abs</code>, <code>even?</code>), ist sie in der Kata abgeschaltet, und die Rückmeldung sagt es.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- `>` statt `>=` auf einer Grenze: volljährig mit 18, eine 1 ab 92 Punkten, Eintritt ab 6 Jahren. Die Rückmeldungen nennen jeweils die Grenze.
- Die Reihenfolge der Zweige: Wer bei den Noten mit `>= 30` anfängt, gibt jedem eine 5; wer bei FizzBuzz `% 3` vor `% 15` prüft, sagt nie `FizzBuzz`.
- Die größte von dreien mit einem `if`, das `c` vergisst, sobald `a > b` ist.
- Beim Dreieck nur `a + b` gegen `c` prüfen; bei `1, 2, 3` liegen alle Ecken auf einer Linie.

#### Die Aufgaben

- Einstieg: `even`, `max_of_two`, `absolute`, `adult`.
- Übung: `leap_year`, `grade`, `ticket_price`, `max_of_three`.
- Knobelei: `fizzbuzz`, `triangle`, `pythagorean`.
- Extra: `rock_paper_scissors`, mit einer Tabelle statt neun Fällen.
