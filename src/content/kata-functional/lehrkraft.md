Lernende geben Funktionen weiter wie Zahlen, direkt in den Karten der Seite, in Ruby, Python oder JavaScript: Lambdas, eine Funktion, die eine Funktion baut, dann `map`, `filter` und `reduce` einzeln und als Kette. Zum Schluss schreiben sie eine Schleife als Kette um und lösen eine Aufgabe, bei der die Schleife klarer bleibt.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Listen und Dictionaries, etwa aus den <a href='/kata-lists'>Katas: Listen</a> und den <a href='/kata-dicts'>Katas: Dictionaries</a>. Die <a href='/kata-recursion'>Katas: Rekursion</a> helfen beim Startwert von <code>reduce</code> und beim Extra mit McCarthys <code>maplist</code>, sind aber nicht nötig.</td></tr>
<tr><th>Ablauf</th><td>Zwei Doppelstunden, die erste bis einschließlich <code>filter</code>.<ul>
<li>Funktionen als Werte (1 Vorhersage, 2 Katas): <code>twice</code> mit einem Lambda, eine Funktion <code>n</code>-mal anwenden, eine Funktion, die eine Funktion zum Multiplizieren baut (Closure). Dazu Alonzo Church und das λ.</li>
<li><code>map</code> (1 Vorhersage, 2 Katas): Preise mit Mehrwertsteuer (in Cent), Wortlängen. Die Vorhersage zeigt, dass <code>map</code> nichts weglässt.</li>
<li><code>filter</code> (3 Katas): gerade Zahlen, Wörter mit einem Buchstaben (das Lambda benutzt einen Parameter von draußen), die Namen derer, die eine Klausur bestanden haben (filter, dann map).</li>
<li><code>reduce</code> (1 Vorhersage, 3 Katas): was ohne Startwert bei der leeren Liste geschieht (Ruby: <code>nil</code>, Python und JavaScript: ein Fehler), das Produkt (Startwert 1), das längste Wort, Zahlungen je Person in ein Dictionary.</li>
<li>Ketten (3 Katas): was eine Art Ware im Warenkorb kostet, eine Schleife als Kette, und wann ein Konto zum ersten Mal im Minus ist, wo die Schleife mit <code>return</code> klarer ist; danach zeigt die Seite dieselbe Kata mit <code>reduce</code>.</li>
<li>Extras (2): zwei Funktionen zu einer zusammensetzen (f ∘ g), <code>map</code> selbst schreiben, mit einer Schleife oder rekursiv.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, auch die leere Liste. Wo <code>map</code> oder <code>filter</code> die Arbeit tun soll und bei der umgeschriebenen Schleife, sind in Ruby <code>each</code>, <code>for</code>, <code>times</code> und <code>loop</code> abgeschaltet, in JavaScript <code>forEach</code>, mit einer eigenen Rückmeldung. Nicht abschalten lassen sich <code>while</code> in Ruby, <code>for</code> und <code>while</code> in Python und JavaScript: Dort bittet die Aufgabe nur darum, und in Python nimmt die Prüfung eine List Comprehension an, die in Python übliche Schreibweise für <code>map</code> und <code>filter</code>. Bei den Katas mit <code>reduce</code> sind Schleifen in Ruby nicht abgeschaltet, weil Rubys <code>reduce</code> selbst <code>each</code> aufruft. Was die Aufgabe fertig lösen würde, ist abgeschaltet (<code>max_by</code>, <code>max</code>, <code>sorted</code>, <code>math.prod</code>, <code>group_by</code>, <code>map</code> im Extra); <code>f &lt;&lt; g</code> in Ruby nicht, die Aufgabe bittet darum, es selbst zu schreiben.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- In Python gibt <code>map</code> ein <code>map</code>-Objekt zurück, keine Liste: Die Rückmeldung zeigt <code>&lt;map object at …&gt;</code>, und es fehlt <code>list(…)</code>.
- <code>map</code> statt <code>filter</code>: Aus den geraden Zahlen wird eine Liste aus <code>true</code> und <code>false</code>. Die Vorhersage vor den Katas zu <code>map</code> bereitet darauf vor, die Rückmeldung verweist auf sie.
- Beim Produkt fängt <code>reduce</code> bei 0 an, und alles bleibt 0; ohne Startwert gibt die leere Liste in Ruby <code>nil</code>, in Python und JavaScript einen Fehler.
- Beim Eintragen in ein Dictionary gibt der Block die eingetragene Zahl zurück statt des Dictionarys (in Ruby die Zuweisung als letzte Zeile, in Python eine Funktion ohne <code>return</code>, in JavaScript eine Arrow Function ohne geschweifte Klammern); beim nächsten Element bricht es ab.
- <code>make_counter</code> legt <code>count = 0</code> im Lambda an statt davor und gibt immer 1 zurück, oder alle Zähler teilen sich eine globale Variable; in Python fehlt <code>nonlocal</code>. <code>apply_n</code> ruft <code>f</code> auf und vergisst das Ergebnis.
- In JavaScript ist <code>-3 % 2</code> gleich <code>-1</code>: Wer nach <code>=== 1</code> fragt, verliert die negativen ungeraden Zahlen.
- Beim Konto zählt genau 0 schon als Minus, oder die Schleife hört nicht auf und findet das letzte Mal statt des ersten.

#### Die Aufgaben

- Funktionen als Werte: die Vorhersage »Zweimal quadrieren«, `apply_n`, `make_counter`, ein Zähler, der sich zwischen den Aufrufen etwas merkt.
- `map`: die Vorhersage »Was macht map daraus?«, `with_tax`, `lengths`.
- `filter`: `evens`, `with_letter`, `passed`.
- `reduce`: die Vorhersage »Die leere Liste ohne Startwert«, `product`, `longest`, `totals`.
- Ketten: `category_total`, `odd_square_sum`, `first_overdraft`.
- Extras: `compose`, `my_map`.
