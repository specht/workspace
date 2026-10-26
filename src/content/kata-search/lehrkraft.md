Lernende schreiben Funktionen, die in Listen suchen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript: erst Element für Element, dann in sortierten Listen durch Halbieren (binäre Suche, Zahlenraten, die Wurzel), bis hunderttausend Suchen in hunderttausend Zahlen in Sekunden gelingen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Listen, etwa aus den <a href='/kata-lists'>Katas: Listen</a>; für »Nur anders sortiert?« die Strichliste aus den <a href='/kata-dicts'>Katas: Dictionaries</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): lineare Suche (der Index oder nichts), der Index der kleinsten Zahl, von hinten gesucht, die erste Zahl über einer Grenze.</li>
<li>Übung (4): binäre Suche, Zahlenraten durch Halbieren (die Liste der Tipps), ab welchem Index eine sortierte Liste eine Grenze erreicht, die ganzzahlige Wurzel bis eine Billion.</li>
<li>Knobelei (3): in eine sortierte Liste einsetzen, ob eine Liste nur anders sortiert ist (ohne zu sortieren), wo eine gedrehte sortierte Liste wieder anfängt.</li>
<li>Extras (2): wie viele Schritte die binäre Suche braucht, hunderttausend Suchen in fünf Sekunden.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, bei der Wurzel und beim letzten Extra mit einer Zeitgrenze: Durchprobieren oder lineare Suche sind dort zu langsam, und die Rückmeldung sagt, warum. Was die Suche fertig mitbringt, ist abgeschaltet (<code>index</code>, <code>bsearch</code>, <code>bisect</code>, <code>min</code>, <code>sort</code>, <code>Math.sqrt</code>).</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die lineare Suche sucht weiter und merkt sich das letzte statt des ersten Vorkommens.
- Beim Index der kleinsten Zahl gewinnt mit <code>&lt;=</code> die letzte von gleich kleinen.
- Von hinten gezählt, kommt der Index 0 nicht mehr dran.
- Die binäre Suche hört mit <code>low &lt; high</code> ein Feld zu früh auf und findet in <code>[4]</code> die 4 nicht.
- Beim Zahlenraten bleibt die Grenze auf dem falschen Tipp stehen (<code>low = guess</code> statt <code>guess + 1</code>), und es rät für manche Zahlen ewig.
- Beim Einsetzen läuft die Suche über das Ende der Liste hinaus.
- »Nur anders sortiert?« prüft, ob jede Zahl vorkommt, aber nicht, wie oft.

#### Die Aufgaben

- Einstieg: `linear_search`, `min_index`, `last_index`, `first_greater`.
- Übung: `binary_search`, `guesses`, `first_at_least`, `int_sqrt`.
- Knobelei: `insert_sorted`, `is_permutation`, `rotation_point`.
- Extras: `search_steps`, `count_found`.
