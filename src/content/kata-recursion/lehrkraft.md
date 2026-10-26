Lernende schreiben Funktionen, die sich selbst aufrufen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript: mit einem Fall, in dem sie sofort fertig sind, und einem Aufruf mit etwas Kleinerem; von der Fakultät bis zu den Türmen von Hanoi und allen Reihenfolgen eines Textes.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen und Listen, etwa aus den <a href='/kata-lists'>Katas: Listen</a>; die Fakultät und Fibonacci aus den <a href='/kata-loops'>Katas: Wiederholen</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): Fakultät, die Summe einer Liste, ein Countdown als Liste, die Länge eines Textes ohne <code>length</code>, alle rekursiv.</li>
<li>Übung (4): Potenz, Fibonacci, Palindrom, wie oft ein Zeichen vorkommt.</li>
<li>Knobelei (3): die Türme von Hanoi (die Züge als Liste), Listen in Listen flach machen, alle Reihenfolgen eines Textes.</li>
<li>Extras (2): Fibonacci mit einem Gedächtnis in drei Sekunden, ob einige Zahlen zusammen eine Summe ergeben.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte. Stimmen sie, sieht die Prüfung im Code nach, ob die Funktion sich selbst aufruft und, wo die Aufgabe es verlangt, ohne Schleife auskommt (<code>recursive</code>, <code>loops</code>; in Ruby mit dem Parser, in Python und JavaScript am Text); eine Lösung mit Schleife bekommt eine eigene Rückmeldung. Beim Flachmachen, bei den Reihenfolgen und beim Gedächtnis ist eine Schleife neben der Rekursion erlaubt.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Der Fall, der aufhört, fehlt oder kommt nie: <code>power(5, 0)</code> mit einem Abbruch erst bei 1 ruft sich ohne Ende auf, bis der Speicher voll ist.
- Fibonacci fängt bei 1 statt bei 0 an, und die ganze Reihe verschiebt sich.
- Beim Countdown steht die Zahl hinter dem Rest, und es zählt hinauf.
- Das Palindrom vergleicht nur das erste und das letzte Zeichen und fragt für die Mitte nicht weiter.
- Bei Hanoi kommen die oberen Scheiben zuerst auf das Ziel statt auf den Hilfsstab.
- Flachmachen nur eine Ebene tief; bei den Reihenfolgen gibt der leere Text keine statt einer zurück.
- Die rekursive Fibonacci-Funktion ist bei 35 schon spürbar langsam: Das Ausprobieren unter ihr zählt, warum, und das Extra mit dem Gedächtnis zeigt, wie es schneller geht.

#### Die Aufgaben

- Einstieg: `factorial`, `total`, `countdown`, `text_length`.
- Übung: `power`, `fibonacci`, `palindrome`, `count_char`.
- Knobelei: `hanoi`, `flatten`, `permutations`.
- Extras: `fast_fibonacci`, `subset_sum`.
