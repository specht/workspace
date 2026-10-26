Lernende schreiben kleine Funktionen direkt in die Karten der Seite, in Ruby, Python oder JavaScript: Werte bekommen, rechnen, ein Ergebnis zurückgeben. Ohne Terminal und Dateien; ein Knopf, **Ausführen**, zeigt, was der Code ausgibt, und ruft die Funktion mit festen und zufälligen Werten auf.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine Programmiererfahrung nötig; Variablen und Rechenzeichen werden gebraucht, aber nicht eigens erklärt.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): eine Zeile Arbeit, das Beispiel in der Aufgabe zeigt fast den Weg.</li>
<li>Übung (4): Reihenfolge der Rechnung, ganze Zahlen und Kommazahlen, Text mit führender Null.</li>
<li>Knobelei (2): Uhrzeit mit <code>%</code>, die letzte Ziffer auch negativer Zahlen.</li>
<li>Extras (2): zählen nicht mit, für Schnelle.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Jede Kata ruft die Funktion mit festen Werten auf, einige zusätzlich mit Werten, die für jede Person anders sind; eine Tabelle der Beispiele besteht also nicht. Der Code läuft in einem eigenen Container ohne Netz, nicht im Workspace.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Ruby rechnet `7 / 2` ganzzahlig: `fahrenheit` und `average` kommen dann auf ganze Zahlen. Die Rückmeldung sagt es; `1.8` oder `3.0` hilft.
- `puts` oder `print` statt `return`: Die Funktion schreibt das Ergebnis hin, gibt aber nichts zurück.
- `a + b + c / 3` ohne Klammern teilt nur `c`.
- `-57 % 10` ist in Ruby und Python 3, nicht 7; die Rückmeldung zu `last_digit` erklärt es.

#### Die Aufgaben

- Einstieg: `add`, `double`, `greet`, `rectangle_area`.
- Übung: `fahrenheit`, `clock` (Sekunden als `2:05`), `average`, `price` mit Rabatt.
- Knobelei: `clock_after` (nach Mitternacht wieder bei 0), `last_digit` (ohne Text).
- Extras: `reverse_digits` für höchstens drei Ziffern ohne Schleife, `duration` als `2:03:05`.
