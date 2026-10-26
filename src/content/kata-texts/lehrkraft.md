Lernende schreiben Funktionen, die mit Texten arbeiten, direkt in die Karten der Seite, in Ruby, Python oder JavaScript: Zeichen der Reihe nach durchgehen, zählen, einen neuen Text bauen, mit den Nummern der Zeichen rechnen und zum Schluss wie Caesar und Vigenère verschlüsseln.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Schleifen und Entscheidungen, etwa aus den <a href='/kata-loops'>Katas: Wiederholen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): einen Text umdrehen, Vokale zählen, Anfangsbuchstaben, Großbuchstaben aus den Nummern der Zeichen (ohne <code>upcase</code>).</li>
<li>Übung (4): Palindrom, Wörter zählen, das längste Wort, eine Adresse aus einem Titel (<code>'Hello World!'</code> wird <code>'hello-world'</code>).</li>
<li>Knobelei (3): die Caesar-Verschlüsselung, Anagramme, Namen schreiben (<code>'ADA LOVELACE'</code> wird <code>'Ada Lovelace'</code>).</li>
<li>Extras (2): die Vigenère-Verschlüsselung, Regeln für ein Passwort.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, bei Caesar dazu Verschiebungen, die für jede Person anders sind. Wo eine eingebaute Funktion die Aufgabe lösen würde (<code>reverse</code>, <code>upcase</code>, <code>capitalize</code>), ist sie abgeschaltet; beim Palindrom ist <code>reverse</code> wieder erlaubt.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Beim Umdrehen kommt jeder Buchstabe hinten statt vorn an, und heraus kommt derselbe Text. Die Rückmeldung fragt, wohin der neue Buchstabe muss.
- Große Vokale werden nicht gezählt, wenn der Text nicht vorher klein gemacht wird.
- Mehrere Leerzeichen hintereinander: In JavaScript liefert <code>split(' ')</code> leere Wörter; in Ruby und Python zerlegt <code>split</code> ohne Argument richtig. Wer die Leerzeichen zählt, kommt beim leeren Text auf ein Wort.
- Großbuchstaben durch »minus 32« auch für Satzzeichen und Leerzeichen.
- Caesar ohne den Sprung von <code>z</code> zurück zu <code>a</code>; in JavaScript ist <code>-1 % 26</code> gleich <code>-1</code>, die Rückmeldung nennt es.
- Anagramme mit einer Menge der Buchstaben: <code>'aab'</code> und <code>'abb'</code> wären dann gleich.

#### Die Aufgaben

- Einstieg: `reverse_text`, `count_vowels`, `initials`, `shout`.
- Übung: `palindrome`, `word_count`, `longest_word`, `slug`.
- Knobelei: `caesar`, `anagram`, `format_name`.
- Extras: `vigenere`, `strong_password`.
