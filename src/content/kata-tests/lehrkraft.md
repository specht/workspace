Lernende schreiben Tests statt Funktionen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript: Zeilen wie <code>assert_equal(erwartet, ergebnis)</code>, die mit einer richtigen Fassung bestehen und jede von mehreren fehlerhaften entlarven, so dass sie lernen, an den Rändern und bei den Ausnahmen zu suchen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die Funktionen, die getestet werden, kennen die Lernenden aus den Katas: gerade Zahlen und Schaltjahre aus den <a href='/kata-decisions'>Entscheidungen</a>, die Adresse und Caesar aus den <a href='/kata-texts'>Texten</a>, das Mischen aus den <a href='/kata-lists'>Listen</a>, die binäre Suche aus dem <a href='/kata-search'>Suchen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (3 Katas): Tests für gerade Zahlen, die größere von zwei Zahlen, den Betrag.</li>
<li>Übung (3): Tests für Schaltjahre, für die Adresse aus einem Titel, für FizzBuzz.</li>
<li>Knobelei (2): Tests für die binäre Suche (die Fehler stecken an den Rändern) und fürs Mischen zweier sortierter Listen.</li>
<li>Extras (2): einen gezeigten Fehler mit einem Test finden, Tests für Caesar.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Die Tests laufen gegen eine richtige Fassung, bei der alle bestehen müssen, und gegen drei oder vier fehlerhafte, von denen jede an mindestens einem Test scheitern muss (Prüfart <code>tests</code>). Kommt eine durch, sagt die Rückmeldung, was diese Fassung anders macht; scheitert ein Test an der richtigen, nennt sie die Zeile und den Wert. Wer die Funktion selbst hineinschreibt, bekommt den Hinweis, nur Tests zu schreiben.</td></tr>
<tr><th>Gespeichert</th><td>Die Tests jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Nur das Erwartbare prüfen: eine gerade Zahl, ein gewöhnliches Jahr. Die fehlerhaften Fassungen irren sich bei 0, bei negativen Zahlen, bei 1900 und 2000, an den Rändern einer Liste.
- Den erwarteten Wert falsch ausrechnen: Dann scheitert der Test an der richtigen Fassung, und die Rückmeldung zeigt, was sie wirklich ergibt.
- In Python ist <code>True</code> nicht dasselbe wie <code>1</code>; die Rückmeldung nennt den Typ.
- Die Funktion selbst hineinschreiben, statt sie zu testen.

#### Die Aufgaben

- Einstieg: Tests für `even`, `max_of_two`, `absolute`.
- Übung: Tests für `leap_year`, `slug`, `fizzbuzz`.
- Knobelei: Tests für `binary_search`, `merge_sorted`.
- Extras: einen Fehler in `max_of_three` finden, Tests für `caesar`.
