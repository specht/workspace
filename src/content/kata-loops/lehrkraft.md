Lernende schreiben Funktionen mit Schleifen direkt in die Karten der Seite, in Ruby oder Python: zählen, summieren, Listen füllen, herunterzählen, und Schleifen, die laufen, bis etwas gefunden ist (Primzahlen, der größte gemeinsame Teiler, die Collatz-Folge).

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen und Entscheidungen, etwa aus den <a href='/kata-functions'>Katas: Erste Funktionen</a> und den <a href='/kata-decisions'>Katas: Entscheidungen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): Summe bis <code>n</code>, eine Zahlenreihe, Quadratzahlen, Countdown; der Startcode zeigt bei den ersten beiden, wo die Schleife hingehört.</li>
<li>Übung (4): Fakultät, Quersumme ohne Text, Fibonacci, Teiler zählen.</li>
<li>Knobelei (3): Primzahlen, Euklids größter gemeinsamer Teiler, die Collatz-Folge.</li>
<li>Extras (2): vollkommene Zahlen, alle Primzahlen bis eine Million in sechs Sekunden (das Sieb des Eratosthenes).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, dazu bei den meisten Katas Werte, die für jede Person anders sind. Wo eine eingebaute Funktion die Aufgabe lösen würde (<code>sum</code>, <code>reduce</code>, <code>math.factorial</code>, <code>gcd</code>), ist sie abgeschaltet. Beim letzten Extra hat der Code sechs Sekunden; eine Prüfung jeder Zahl bis zur Wurzel ist knapp zu langsam, das Sieb braucht etwa eine.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt, sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die Grenzen der Schleife: `1...n` in Ruby und `range(1, n)` in Python hören vor `n` auf. Die Rückmeldungen zu `sum_to` und `count_divisors` nennen es.
- Ein Produkt, das bei 0 anfängt (`factorial`), bleibt 0.
- `prime(1)` ergibt `true`, wenn die Schleife bei 1 nichts zu prüfen hat.
- Eine `while`-Schleife, deren Bedingung nie falsch wird: Nach zehn Sekunden bricht die Prüfung ab und fragt nach einer Endlosschleife.

#### Die Aufgaben

- Einstieg: `sum_to`, `numbers`, `squares`, `countdown`.
- Übung: `factorial`, `digit_sum`, `fibonacci`, `count_divisors`.
- Knobelei: `prime`, `gcd`, `collatz_steps`.
- Extras: `perfect_numbers`, `primes_up_to` mit Zeitgrenze.
