Lernende suchen mit `grep -E` in einem Logfile nach Fehlern und nach einem Programm, das die Website nach Lücken absucht, holen in Ruby oder Python mit Gruppen den Status aus jeder Zeile und prüfen Postleitzahlen, Daten und Benutzernamen. Am Ende verbinden sie Ausdrücke mit endlichen Automaten (Thompson-Konstruktion, Satz von Kleene) und sehen, warum kein Ausdruck Klammern prüfen kann.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/automata'>Endliche Automaten</a> (DEA, NEA, Fehlerzustand); im Terminal einen Befehl ausführen. Grundlagen in Ruby oder Python: Funktionen, Listen, Schleifen.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen. Die Ausdrücke selbst sind in beiden Sprachen gleich, nur die Anker heißen anders (<code>\A</code> und <code>\z</code> in Ruby, <code>re.fullmatch</code> in Python).</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Das Logfile mit grep: <code>grep -c 404</code> als Vorhersage, Zeichenklassen und Wiederholungen, der Punkt, die Anker, <code>-o</code> mit <code>sort | uniq -c</code>; dann <code>log.rb</code> oder <code>log.py</code> mit <code>=~</code> oder <code>re.search</code>, Gruppen und <code>status</code>.</li>
<li><b>2. Doppelstunde:</b> Eingaben prüfen (Postleitzahl, Datum, Benutzername, E-Mail-Adressen), Backtracking, dann vom Ausdruck zum Automaten, der Satz von Kleene und die Klammern.</li>
<li><b>Für Schnelle:</b> der Kasten zum Zählen der Status mit grep, <code>scan</code> oder <code>findall</code> mit Gruppen, die Zeit beim Backtracking messen, <a href='/automatron'>Automatron</a>.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/regex</code>: <code>access.log</code> (mit <code>wget</code> von der Website), <code>log</code> und <code>validate</code>, mit <code>.rb</code> in Ruby und <code>.py</code> in Python. Das Logfile ist ausgedacht, 247 Zeilen eines Tages; die Adressen stammen aus Bereichen für Beispiele (RFC 5737) und dem Netz <code>10.12.0.0/16</code> der Schule.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Drei Aufgaben führen den grep-Befehl aus der Karte in <code>/workspace/regex</code> aus, als Gast, der nichts verändern kann, und zählen die Zeilen der Ausgabe.</li>
<li>Drei laden <code>log</code> oder <code>validate</code> der Lernenden und rufen ihre Funktionen auf; die Rückmeldung nennt den ersten Text, bei dem die Funktion sich irrt. Der Workspace muss dafür laufen.</li>
<li>Die übrigen sind Vorhersagen, Fragen und Zahlen.</li>
</ul></td></tr>
</table>

#### Die Aufgaben

- **Wie viele 404?** (Vorhersage) `grep -c 404` zählt 33 Zeilen, 18 davon mit dem Status 404; in den anderen steht 404 in der Zahl der Bytes (`4048`, `14042`). Danach zählen die Lernenden mit `'" 404 '`.
- **Fehler des Servers** verlangt die drei Zeilen mit 500 und 502. Wer `5[0-9]{2}` ohne das Anführungszeichen schreibt, findet auch `537.36` im Namen des Browsers; die Rückmeldung sagt das.
- **Nur PHP**: Sieben Zeilen mit `\.php`, alle vom Scanner `198.51.100.66`. Mit `.php` oder `php` kommt `/ag/informatik/php-kurs.html` dazu, und die Rückmeldung nennt den Punkt.
- **Aus dem Schulnetz**: 148 mit `^10\.`; 180 ohne `^` (der Browser heißt oft `Windows NT 10.0`) und 247 mit `10.` ohne Backslash haben je eine eigene Rückmeldung.
- **Wer sucht da?** braucht zwei grep mit einer Pipe und `sort | uniq -c`: `198.51.100.66` mit zwölf Anfragen.
- **Der Status einer Zeile** (Kasten »Jetzt bist du dran«): Wer `\d{3}` ohne das, was davor und dahinter steht, schreibt, bekommt die Ziffern der Zeit oder der Adresse; wer nur Leerzeichen herumschreibt, die Zahl `404` aus `4048`.
- **Fünf Ziffern irgendwo** (Vorhersage, mehrere richtig): Ohne Anker passt `\d{5}` auch auf `D-10115 Berlin` und `101150`. Danach erklärt die Seite `\A`, `\z` und `re.fullmatch` und warum `^` und `$` in Ruby für Prüfungen nicht reichen (`"10115\n<script>"`).
- **Ein Datum** (Kasten »Jetzt bist du dran«): Tag 01 bis 31, Monat 01 bis 12, in Alternativen. Die Seite zeigt danach, dass `31.02.2026` durchkommt: Ein Ausdruck prüft die Form, nicht den Kalender; das ist eine gute Stelle, Syntax und Semantik zu besprechen.
- **Ein Benutzername**: `\A[a-z][a-z0-9_]{2,15}\z`. Die Rückmeldungen erkennen `\w` (große Buchstaben, in Python auch Umlaute), `^…$`, eine falsche Länge und einen Anfang mit Ziffer oder `_`.
- **E-Mail-Adressen** (Vorhersage): grob prüfen und eine Mail zum Bestätigen schicken; die Seite verlinkt RFC 5322 und den Ausdruck von über 6000 Zeichen, zeigt ihn aber nicht.
- **Fünf Ziffern als Automat**: sechs Zustände, q<sub>0</sub> bis q<sub>5</sub>.
- **Welcher Ausdruck?**: `0|1(0|1)*` zum gezeichneten Automaten; jede falsche Antwort nennt ein Wort, an dem Ausdruck und Automat sich unterscheiden.
- **Klammern mit einem Ausdruck** (Vorhersage): Keiner kann es. Danach eine Überlegung ohne Beweis; den Beweis mit dem Schubfachprinzip führen die <a href='/kellerautomat'>Kellerautomaten</a>.

#### Was schiefgeht

- **`wget` meldet `404 Not Found`:** Solange die Aktivität nur in der Entwicklung im Katalog steht, liefert die Website das Logfile nur dort aus.
- **grep findet nichts, obwohl das Muster stimmt:** Das Muster steht nicht in einfachen Anführungszeichen, und das Terminal hat das `"` oder die Leerzeichen selbst verbraucht.
- **grep findet mit `\d` nichts oder Falsches:** grep kennt `\d` nicht und sucht stattdessen ein `d`; dort heißt es `[0-9]`.
- **Python: `re.error` oder ein Backslash verschwindet:** Das Muster ist kein Raw-String; es fehlt das `r` vor dem Anführungszeichen.
- **Ruby: `undefined method '[]' for nil`:** `line.match` hat nichts gefunden; `status` muss dann `nil` zurückgeben, bevor es `m[1]` liest.
- **Das Messen der Zeit dauert zu lange:** Beim Backtracking verdoppelt jedes a die Zeit; <kbd>Strg</kbd><kbd>C</kbd> bricht ab.

#### Tipps in Stufen

Die beiden Kästen »Jetzt bist du dran« (Status und Datum) haben drei Tipps, der Benutzername als Aufgabe ebenso: ein Beispiel von Hand, der Weg in Worten, ein Gerüst mit Lücken. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.

#### Weiter

<a href='/kellerautomat'>Kellerautomaten</a> (Klammern mit einem Stack, das Schubfachprinzip) und <a href='/grammatiken'>Grammatiken</a> (Regeln, die sich selbst benutzen). Die Thompson-Konstruktion, die Potenzmengenkonstruktion und die Minimierung stehen ausführlich im Handbuch unter <a href='/reference/automaten'>Endliche Automaten</a>; Automatron zeigt sie für jeden Ausdruck.
