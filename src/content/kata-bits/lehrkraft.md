Lernende schreiben Funktionen, die Zahlen ins Zweier- und Sechzehnersystem und in jedes andere schreiben und zurücklesen, direkt in die Karten der Seite, in Ruby, Python oder JavaScript, nur mit Teilen und Rest; dazu negative Zahlen im Zweierkomplement eines Bytes, schriftliches Addieren von Bits mit Übertrag und Überlauf, und römische Zahlen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Schleifen, Strings und Dictionaries, etwa aus den <a href='/kata-loops'>Katas: Schleifen</a> und den <a href='/kata-dicts'>Katas: Dictionaries</a>.</td></tr>
<tr><th>Ablauf</th><td>Zwei Doppelstunden, die erste bis zur Knobelei.<ul>
<li>Einstieg (4 Katas): eine Zahl in Bits, Bits zurück in eine Zahl, ein einzelnes Bit, wie viele Bits eine Zahl braucht.</li>
<li>Übung (4): Einsen zählen, ins Sechzehnersystem und zurück, ist es eine Zweierpotenz.</li>
<li>Knobelei (3): negative Zahlen in einem Byte (Zweierkomplement), jedes Zahlensystem von 2 bis 16, von einem System ins andere.</li>
<li>Addieren (4 Katas, 2 Vorhersagen): schriftlich von Hand mit Übertrag, eine Stelle (was ein Volladdierer tut), zwei Bitfolgen, ein Byte mit Überlauf (200 + 100 ergibt 44), negieren durch Umdrehen und 1 Addieren.</li>
<li>Extras (2): römische Zahlen schreiben und lesen.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Feste Werte, bei den meisten dazu Zahlen, die für jede Person anders sind. Was die Umwandlung fertig mitbringt, ist abgeschaltet (<code>to_s(2)</code>, <code>bin</code>, <code>format</code>, <code>toString(2)</code>, <code>to_i(2)</code>, <code>int</code>, <code>parseInt</code>); in Ruby bittet »Ein einzelnes Bit« nur darum, ohne <code>n[i]</code> zu arbeiten. Beim Addieren sind dieselben Umwandlungen abgeschaltet; wer die Bits mit eigenen Schleifen erst in eine Zahl und zurück rechnet, besteht trotzdem, die Aufgabe bittet aber darum, Stelle für Stelle zu rechnen.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die Bits kommen verkehrt herum heraus, wenn jedes neue hinten statt vorn angehängt wird; die Rückmeldung zeigt es an der 13.
- Beim Zurücklesen werden nur die Einsen gezählt, statt bei jeder Stelle zu verdoppeln.
- Die 0: Sie ist <code>'0'</code> und braucht ein Bit; eine Schleife, die bei 0 nicht läuft, gibt einen leeren Text oder 0 zurück.
- »Gerade« reicht nicht für eine Zweierpotenz: 12 ist gerade, aber keine Zweierpotenz, und 1 ist eine Zweierpotenz, aber ungerade.
- Im Byte fehlen die Nullen vorn: 5 ist <code>'00000101'</code>.
- Beim Addieren bleibt der letzte Übertrag liegen: <code>add_binary('1', '1')</code> ergibt <code>'0'</code> statt <code>'10'</code>. Oder der Übertrag fehlt ganz, und jede Stelle wird für sich addiert.
- Im Byte bleibt der Übertrag vorn im Ergebnis, das dann neun Bits hat; beim Negieren werden die Bits nur umgedreht, und es fehlt die 1 (aus 5 wird −6).
- Römische Zahlen ohne die Paare wie <code>IV</code> und <code>CM</code>; beim Lesen wird ein kleineres Zeichen vor einem größeren dazugezählt statt abgezogen.

#### Die Aufgaben

- Einstieg: `to_binary`, `from_binary`, `bit`, `binary_length`.
- Übung: `count_ones`, `to_hex`, `from_hex`, `power_of_two`.
- Knobelei: `twos_complement`, `to_base`, `convert`.
- Addieren: die Vorhersagen »Von Hand addieren« und »200 + 100 in einem Byte«, dann `add_digits`, `add_binary`, `add_byte`, `negate`.
- Extras: `roman`, `from_roman`.
