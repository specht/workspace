Lernende schreiben Funktionen, die Zahlen ins Zweier- und Sechzehnersystem und in jedes andere schreiben und zurücklesen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript, nur mit Teilen und Rest; dazu negative Zahlen im Zweierkomplement eines Bytes und römische Zahlen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Schleifen, Strings und Dictionaries, etwa aus den <a href='/kata-loops'>Katas: Schleifen</a> und den <a href='/kata-dicts'>Katas: Dictionaries</a>.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde.<ul>
<li>Einstieg (4 Katas): eine Zahl in Bits, Bits zurück in eine Zahl, ein einzelnes Bit, wie viele Bits eine Zahl braucht.</li>
<li>Übung (4): Einsen zählen, ins Sechzehnersystem und zurück, ist es eine Zweierpotenz.</li>
<li>Knobelei (3): negative Zahlen in einem Byte (Zweierkomplement), jedes Zahlensystem von 2 bis 16, von einem System ins andere.</li>
<li>Extras (2): römische Zahlen schreiben und lesen.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, bei den meisten dazu Zahlen, die für jede Person anders sind. Was die Umwandlung fertig mitbringt, ist abgeschaltet (<code>to_s(2)</code>, <code>bin</code>, <code>format</code>, <code>toString(2)</code>, <code>to_i(2)</code>, <code>int</code>, <code>parseInt</code>); in Ruby bittet »Ein einzelnes Bit« nur darum, ohne <code>n[i]</code> zu arbeiten.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die Bits kommen verkehrt herum heraus, wenn jedes neue hinten statt vorn angehängt wird; die Rückmeldung zeigt es an der 13.
- Beim Zurücklesen werden nur die Einsen gezählt, statt bei jeder Stelle zu verdoppeln.
- Die 0: Sie ist <code>'0'</code> und braucht ein Bit; eine Schleife, die bei 0 nicht läuft, gibt einen leeren Text oder 0 zurück.
- »Gerade« reicht nicht für eine Zweierpotenz: 12 ist gerade, aber keine Zweierpotenz, und 1 ist eine Zweierpotenz, aber ungerade.
- Im Byte fehlen die Nullen vorn: 5 ist <code>'00000101'</code>.
- Römische Zahlen ohne die Paare wie <code>IV</code> und <code>CM</code>; beim Lesen wird ein kleineres Zeichen vor einem größeren dazugezählt statt abgezogen.

#### Die Aufgaben

- Einstieg: `to_binary`, `from_binary`, `bit`, `binary_length`.
- Übung: `count_ones`, `to_hex`, `from_hex`, `power_of_two`.
- Knobelei: `twos_complement`, `to_base`, `convert`.
- Extras: `roman`, `from_roman`.
