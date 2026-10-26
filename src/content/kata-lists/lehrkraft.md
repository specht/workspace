Lernende schreiben Funktionen, die Listen bekommen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript: Elemente der Reihe nach durchgehen, mit dem Index arbeiten, eine neue Liste bauen, Doppelte entfernen und zwei sortierte Listen mischen, den Kern von Merge Sort.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Schleifen und Texte, etwa aus den <a href='/kata-loops'>Katas: Wiederholen</a> und den <a href='/kata-texts'>Katas: Texte</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): Summe, die größte Zahl, gerade Zahlen zählen, ist ein Element dabei.</li>
<li>Übung (4): eine Liste rückwärts, erstes und letztes tauschen, einen Platz nach links rücken, aufsteigend sortiert?</li>
<li>Knobelei (3): ohne Doppelte, die fehlende Zahl von 1 bis n, zwei sortierte Listen mischen.</li>
<li>Extras (2): zwei Zahlen, die zusammen eine Zahl ergeben (ihre Indizes), eine Liste in Stücke teilen.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, auch die leere Liste und Listen mit einem Element. Wo eine eingebaute Funktion die Aufgabe lösen würde (<code>sum</code>, <code>max</code>, <code>include?</code>, <code>reverse</code>, <code>uniq</code>, <code>sort</code>), ist sie abgeschaltet; bei »Ohne Doppelte« ist das Nachsehen in der neuen Liste wieder erlaubt.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- In Ruby ist das Letzte in der Funktion ihr Ergebnis: Endet sie mit der Schleife, kommt die Liste zurück statt der Summe. In Python und JavaScript fehlt das <code>return</code>. Die Rückmeldung nennt beides.
- Das Maximum fängt bei 0 an und stimmt bei lauter negativen Zahlen nicht.
- <code>contains</code> gibt schon beim ersten Element <code>false</code> zurück, statt nach der Schleife.
- Beim Rückwärtsgehen fehlt der Index 0, oder es fängt bei <code>length</code> statt <code>length - 1</code> an.
- Beim Tauschen ist das erste Element schon überschrieben, wenn es nach hinten soll.
- Beim Mischen bleibt der Rest der längeren Liste liegen.

#### Die Aufgaben

- Einstieg: `list_sum`, `list_max`, `count_even`, `contains`.
- Übung: `reverse_list`, `swap_ends`, `rotate_left`, `ascending`.
- Knobelei: `remove_duplicates`, `missing_number`, `merge_sorted`.
- Extras: `two_sum`, `chunks`.
