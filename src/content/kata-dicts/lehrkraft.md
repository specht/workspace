Lernende schreiben Funktionen mit Schlüsseln und Werten (Hash in Ruby, Dictionary in Python, Objekt in JavaScript) direkt in die Karten der Seite: nachschlagen, Strichlisten führen, gruppieren, umkehren und mit einem Dictionary schneller werden als mit einer Liste.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Listen, etwa aus den <a href='/kata-lists'>Katas: Listen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): im Telefonbuch nachschlagen, eine Strichliste, was ein Einkauf kostet (in Cent), wer so viele Punkte hat.</li>
<li>Übung (4): wie oft jeder Buchstabe, das häufigste Wort, Wörter nach Länge gruppieren, zwei Strichlisten zusammenrechnen.</li>
<li>Knobelei (3): umgekehrt nachschlagen (jede Farbe mit ihren Obstsorten), der Durchschnitt je Fach, die erste Zahl, die zum zweiten Mal kommt, in einer Liste von 100 000 Zahlen.</li>
<li>Extras (2): die Fehlschläge eines kleinen LRU-Caches zählen, Anagramme sammeln.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte. Wo eine eingebaute Funktion die Aufgabe lösen würde (<code>tally</code>, <code>group_by</code>, <code>invert</code>, <code>Object.groupBy</code>), ist sie abgeschaltet; in Python bittet die Aufgabe nur darum, ohne <code>Counter</code> zu arbeiten. Bei der ersten Wiederholung hat der Code zehn Sekunden: Wer bei jeder Zahl die Liste durchsucht, ist zu langsam, und die Rückmeldung rät zu einem Dictionary.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Ein Schlüssel, den es nicht gibt: <code>nil</code>, <code>None</code> oder <code>undefined</code> statt eines Werts, den die Aufgabe nennt.
- Beim Zählen fängt jedes weitere Vorkommen wieder bei 1 an.
- Beim Gruppieren ersetzt das zweite Wort die Gruppe, statt angehängt zu werden; beim Umkehren bleibt so nur die letzte Obstsorte.
- <code>merge</code> oder <code>{**a, **b}</code> überschreibt, statt zusammenzuzählen.
- In Ruby ist <code>3 / 2</code> gleich <code>1</code>, in Python <code>3 // 2</code>: Der Durchschnitt braucht Kommazahlen.
- In JavaScript sind die Schlüssel eines Objekts Texte; die Prüfung vergleicht so, dass das nicht stört.

#### Die Aufgaben

- Einstieg: `phone_number`, `count_items`, `total_price`, `keys_with_value`.
- Übung: `letter_count`, `most_frequent`, `group_by_length`, `merge_counts`.
- Knobelei: `invert`, `averages`, `first_repeat`.
- Extras: `lru_misses`, `anagram_groups`.
