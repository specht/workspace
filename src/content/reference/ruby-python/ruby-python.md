<div class='meta'>
kind: reference
search: Ruby, Python, puts, print, Array, Liste, Hash, dict, Set, String, each, for, map, Listen-Abstraktion, list comprehension, def, lambda, Block, nil, None, Bytes, pack, File, open, rescue, except, Fehlermeldung, NoMethodError, AttributeError, TypeError
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Ruby und Python nebeneinander

<p class='abstract'>
Viele Aktivitäten lassen dir die Wahl zwischen Ruby und Python, etwa <a href='/rle'>Lauflängen</a>, <a href='/huffman'>Huffman</a>, <a href='/lzw'>LZW</a>, <a href='/steganography'>Steganographie</a>, <a href='/totp'>Einmalcodes</a>, <a href='/automata'>Automaten</a>, der <a href='/calculator'>Taschenrechner</a>, der <a href='/routenplaner'>Routenplaner</a> und der <a href='/webserver'>Webserver</a>. Hier steht dasselbe in beiden Sprachen nebeneinander: zum Nachschlagen in der eigenen, und um zu sehen, wie es in der anderen aussieht. Wie man ein Programm überhaupt schreibt und startet, zeigen die Aktivitäten <a href='/ruby'>Ruby</a> und <a href='/python'>Python</a>.
</p>

Die beiden Sprachen sind fast gleich alt, beide startet man ohne Compiler direkt aus dem Quelltext, und beide wollen, dass sich Programme gut lesen. Sie unterscheiden sich vor allem in der Schreibweise: Ruby schließt Blöcke mit `end` und ruft Methoden an fast allem auf (`text.length`), Python rückt Blöcke ein und hat für vieles eingebaute Funktionen (`len(text)`).

_person(van-rossum)

_person(matsumoto)

## Ein Programm

<!-- part: rp-programm -->
<!-- short: ruby prog.rb und python3 prog.py; puts/p und print; end und Einrückung; ARGV[0] und sys.argv[1]; require_relative und import. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>starten</th><td><code>ruby prog.rb</code></td><td><code>python3 prog.py</code></td></tr>
<tr><th>ausprobieren, Zeile für Zeile</th><td><code>irb</code>, beenden mit <code>exit</code></td><td><code>python3</code>, beenden mit <code>exit()</code></td></tr>
<tr><th>ausgeben</th><td><code>puts x</code>; <code>p x</code> zeigt <code>x</code>, wie man es schreiben würde, mit Anführungszeichen</td><td><code>print(x)</code>; <code>print(repr(x))</code> mit Anführungszeichen</td></tr>
<tr><th>ohne Zeilenende ausgeben</th><td><code>print x</code></td><td><code>print(x, end='')</code></td></tr>
<tr><th>einlesen</th><td><code>gets.chomp</code></td><td><code>input()</code></td></tr>
<tr><th>Kommentar</th><td colspan='2'><code># bis zum Zeilenende</code>, in beiden gleich</td></tr>
<tr><th>Argumente beim Start</th><td><code>ARGV[0]</code> ist das erste</td><td><code>import sys</code>, dann <code>sys.argv[1]</code>; <code>sys.argv[0]</code> ist der Name des Programms</td></tr>
<tr><th>eine andere Datei laden</th><td><code>require_relative 'graph'</code></td><td><code>import graph</code> oder <code>from graph import build_graph</code></td></tr>
<tr><th>nur, wenn die Datei selbst gestartet wird</th><td><code>if __FILE__ == $0</code> … <code>end</code></td><td><code>if __name__ == '__main__':</code></td></tr>
<tr><th>mit einer Meldung abbrechen</th><td><code>abort('Datei fehlt')</code></td><td><code>sys.exit('Datei fehlt')</code></td></tr>
</tbody>
</table>
</div>

Ein Block endet in Ruby mit `end`; die Einrückung ist nur für Menschen. In Python ist die Einrückung der Block: Nach `if`, `for`, `def` und `while` steht ein Doppelpunkt, und alles, was weiter eingerückt ist, gehört dazu.

```ruby
if count > 3
  puts 'viele'
else
  puts 'wenige'
end
```

```python
if count > 3:
    print('viele')
else:
    print('wenige')
```

Ein `elsif` heißt in Python `elif`, `&&` `||` `!` heißen `and` `or` `not` (Ruby kennt beide Schreibweisen), `unless x` heißt `if not x`.
<!-- /part -->

## Zahlen, Wahrheit und nichts

<!-- part: rp-werte -->
<!-- short: 7 / 2 ist in Ruby 3, in Python 3.5; nil und None; in Ruby ist 0 wahr; 'x'.to_i ist 0, int('x') ein Fehler. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>nichts</th><td><code>nil</code></td><td><code>None</code></td></tr>
<tr><th>wahr, falsch</th><td><code>true</code>, <code>false</code></td><td><code>True</code>, <code>False</code></td></tr>
<tr><th><code>7 / 2</code></th><td><strong>3</strong>: zwei ganze Zahlen ergeben eine ganze, abgerundet</td><td><strong>3.5</strong>: <code>/</code> gibt immer eine Kommazahl</td></tr>
<tr><th>ganzzahlig teilen</th><td><code>7 / 2</code> → 3</td><td><code>7 // 2</code> → 3</td></tr>
<tr><th>mit Komma teilen</th><td><code>7.0 / 2</code>, <code>7.fdiv(2)</code> → 3.5</td><td><code>7 / 2</code> → 3.5</td></tr>
<tr><th>Rest, Potenz</th><td colspan='2'><code>7 % 3</code> → 1, <code>2 ** 10</code> → 1024, in beiden gleich; auch <code>-7 % 3</code> ist in beiden 2 und <code>-7 / 2</code> bzw. <code>-7 // 2</code> −4</td></tr>
<tr><th>Text zu Zahl</th><td><code>'42'.to_i</code>, <code>'3.5'.to_f</code>; <code>'x'.to_i</code> ist <strong>0</strong>, ohne Fehler</td><td><code>int('42')</code>, <code>float('3.5')</code>; <code>int('x')</code> ist ein <code>ValueError</code></td></tr>
<tr><th>Zahl zu Text</th><td><code>42.to_s</code></td><td><code>str(42)</code></td></tr>
<tr><th>Werte in Text einsetzen</th><td><code>"#{name} hat #{count}"</code>, nur in doppelten Anführungszeichen</td><td><code>f'{name} hat {count}'</code></td></tr>
<tr><th>auf zwei Stellen</th><td><code>format('%.2f', x)</code></td><td><code>f'{x:.2f}'</code></td></tr>
</tbody>
</table>
</div>

**Was als falsch gilt**, ist verschieden. In Ruby sind nur `nil` und `false` falsch, alles andere ist wahr, auch `0`, `''` und `[]`. In Python sind auch `0`, `0.0`, `''`, `[]`, `{}` und `None` falsch. `if list:` heißt in Python also »wenn die Liste nicht leer ist«; in Ruby schreibt man dafür `unless list.empty?` oder `if list.any?`.
<!-- /part -->

## Text

<!-- part: rp-text -->
<!-- short: length und len, s[1..] und s[1:], start_with? und startswith, split, strip, join, each_char und for, ord und chr. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>Länge</th><td><code>s.length</code></td><td><code>len(s)</code></td></tr>
<tr><th>ein Zeichen, das letzte</th><td colspan='2'><code>s[0]</code>, <code>s[-1]</code>, in beiden gleich</td></tr>
<tr><th>ein Stück</th><td><code>s[1, 3]</code>: ab 1, drei Zeichen; <code>s[1..]</code>: ab 1 bis zum Ende</td><td><code>s[1:4]</code>: ab 1 bis vor 4; <code>s[1:]</code>: ab 1 bis zum Ende</td></tr>
<tr><th>enthält, beginnt, endet</th><td><code>s.include?('ll')</code>, <code>s.start_with?('Ha')</code>, <code>s.end_with?('o')</code></td><td><code>'ll' in s</code>, <code>s.startswith('Ha')</code>, <code>s.endswith('o')</code></td></tr>
<tr><th>groß, klein</th><td><code>s.upcase</code>, <code>s.downcase</code></td><td><code>s.upper()</code>, <code>s.lower()</code></td></tr>
<tr><th>Leerraum und Zeilenende weg</th><td><code>s.strip</code>; nur das Zeilenende: <code>s.chomp</code></td><td><code>s.strip()</code>; nur das Zeilenende: <code>s.rstrip('\n')</code></td></tr>
<tr><th>teilen, zusammenfügen</th><td><code>'a,b'.split(',')</code>, <code>list.join(',')</code></td><td><code>'a,b'.split(',')</code>, <code>','.join(list)</code></td></tr>
<tr><th>Zeichen für Zeichen</th><td><code>s.each_char { |c| … }</code>, als Array <code>s.chars</code></td><td><code>for c in s:</code>, als Liste <code>list(s)</code></td></tr>
<tr><th>wiederholen</th><td colspan='2'><code>'ab' * 3</code> → <code>'ababab'</code>, in beiden gleich</td></tr>
<tr><th>vorn auffüllen</th><td colspan='2'><code>s.rjust(8, '0')</code>, in beiden gleich</td></tr>
<tr><th>Nummer eines Zeichens und zurück</th><td><code>'A'.ord</code> → 65, <code>65.chr</code></td><td><code>ord('A')</code> → 65, <code>chr(65)</code></td></tr>
<tr><th>mit einem regulären Ausdruck</th><td><code>s.scan(/(\d+)(\D)/)</code>, <code>s =~ /\d/</code></td><td><code>import re</code>, dann <code>re.findall(r'(\d+)(\D)', s)</code>, <code>re.search(r'\d', s)</code></td></tr>
</tbody>
</table>
</div>

In Ruby lässt sich ein String verändern (`s << 'x'` hängt an), in Python nicht: Jede Methode gibt einen neuen String zurück, und `s.upper()` allein, ohne `s = …`, ändert nichts.
<!-- /part -->

## Listen

<!-- part: rp-listen -->
<!-- short: Array und Liste: << und append, pop, shift und pop(0), each und for, map und [… for …], select und [… if …], sort_by und sorted(key=…). -->
In Ruby heißt eine Liste **Array**, in Python **Liste** (`list`). Geschrieben wird sie in beiden gleich: `[3, 1, 2]`.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>Länge, enthält</th><td><code>a.length</code>, <code>a.include?(2)</code></td><td><code>len(a)</code>, <code>2 in a</code></td></tr>
<tr><th>hinten anhängen, wegnehmen</th><td><code>a &lt;&lt; 4</code> oder <code>a.push(4)</code>, <code>a.pop</code></td><td><code>a.append(4)</code>, <code>a.pop()</code></td></tr>
<tr><th>vorn wegnehmen</th><td><code>a.shift</code></td><td><code>a.pop(0)</code></td></tr>
<tr><th>das erste, das letzte, ab dem zweiten</th><td><code>a.first</code>, <code>a.last</code>, <code>a[1..]</code></td><td><code>a[0]</code>, <code>a[-1]</code>, <code>a[1:]</code></td></tr>
<tr><th>der Reihe nach</th><td><code>a.each { |x| … }</code></td><td><code>for x in a:</code></td></tr>
<tr><th>mit Index</th><td><code>a.each_with_index { |x, i| … }</code></td><td><code>for i, x in enumerate(a):</code></td></tr>
<tr><th>von 0 bis 9</th><td><code>10.times { |i| … }</code>, <code>(0...10).each</code></td><td><code>for i in range(10):</code></td></tr>
<tr><th>jedes umrechnen</th><td><code>a.map { |x| x * 2 }</code></td><td><code>[x * 2 for x in a]</code></td></tr>
<tr><th>nur manche</th><td><code>a.select { |x| x &gt; 1 }</code></td><td><code>[x for x in a if x &gt; 1]</code></td></tr>
<tr><th>Summe, Minimum, Maximum</th><td><code>a.sum</code>, <code>a.min</code>, <code>a.max</code></td><td><code>sum(a)</code>, <code>min(a)</code>, <code>max(a)</code></td></tr>
<tr><th>das mit dem kleinsten Wert von …</th><td><code>a.min_by { |p| p[:cost] }</code></td><td><code>min(a, key=lambda p: p['cost'])</code></td></tr>
<tr><th>sortiert</th><td><code>a.sort</code>, <code>a.sort_by { |x| -x }</code>; <code>a.sort!</code> sortiert <code>a</code> selbst</td><td><code>sorted(a)</code>, <code>sorted(a, key=lambda x: -x)</code>; <code>a.sort()</code> sortiert <code>a</code> selbst</td></tr>
<tr><th>paarweise: <code>[1, 2, 3, 4]</code> → (1, 2), (3, 4)</th><td><code>a.each_slice(2)</code></td><td><code>zip(a[0::2], a[1::2])</code></td></tr>
<tr><th>Nachbarn: (1, 2), (2, 3), (3, 4)</th><td><code>a.each_cons(2)</code></td><td><code>zip(a, a[1:])</code></td></tr>
<tr><th>auspacken</th><td colspan='2'><code>first, *rest = a</code>, in beiden gleich</td></tr>
</tbody>
</table>
</div>

In Ruby gibt fast jede dieser Methoden einen neuen Wert zurück; ein `!` am Ende (`sort!`, `map!`) heißt, dass sie das Array selbst ändert. Für Paare und feste Gruppen, die sich nicht ändern, nimmt man in Python ein **Tupel**, `(3, 'B')`, in Ruby einfach ein Array, `[3, 'B']`.
<!-- /part -->

## Hashes, Dictionaries und Mengen

<!-- part: rp-hashes -->
<!-- short: Hash und dict: h[k], fetch und get mit Vorgabe, Hash.new(0) und get(k, 0) + 1, each { |k, v| } und items(); Set und set. -->
Was zu einem Schlüssel gehört, steht in Ruby in einem **Hash**, in Python in einem **Dictionary** (`dict`).

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>anlegen</th><td><code>{ 'car' =&gt; 50, 'bike' =&gt; 15 }</code></td><td><code>{'car': 50, 'bike': 15}</code></td></tr>
<tr><th>nachschlagen</th><td><code>h['car']</code>; fehlt der Schlüssel, <code>nil</code></td><td><code>h['car']</code>; fehlt der Schlüssel, ein <code>KeyError</code></td></tr>
<tr><th>mit einer Vorgabe, wenn er fehlt</th><td><code>h.fetch('bus', 0)</code></td><td><code>h.get('bus', 0)</code></td></tr>
<tr><th>gibt es den Schlüssel?</th><td><code>h.key?('car')</code></td><td><code>'car' in h</code></td></tr>
<tr><th>eintragen, entfernen</th><td><code>h['bus'] = 30</code>, <code>h.delete('bus')</code></td><td><code>h['bus'] = 30</code>, <code>h.pop('bus')</code></td></tr>
<tr><th>alle durchgehen</th><td><code>h.each { |key, value| … }</code></td><td><code>for key, value in h.items():</code></td></tr>
<tr><th>nur Schlüssel, nur Werte</th><td><code>h.keys</code>, <code>h.values</code></td><td><code>list(h.keys())</code>, <code>list(h.values())</code></td></tr>
<tr><th>zählen</th><td><code>counts = Hash.new(0)</code>, dann <code>counts[x] += 1</code></td><td><code>counts[x] = counts.get(x, 0) + 1</code>, oder <code>collections.Counter</code></td></tr>
<tr><th>zu jedem Schlüssel eine Liste</th><td><code>Hash.new { |hash, key| hash[key] = [] }</code></td><td><code>edges.setdefault(id, []).append(…)</code></td></tr>
<tr><th>zwei Werte als Schlüssel</th><td><code>h[['q0', 'a']]</code>, ein Array</td><td><code>h[('q0', 'a')]</code>, ein Tupel; eine Liste geht nicht</td></tr>
</tbody>
</table>
</div>

In Ruby gibt es neben Strings **Symbole** als Schlüssel: `{ car: 50 }` ist dasselbe wie `{ :car => 50 }` und wird mit `h[:car]` gelesen, nicht mit `h['car']`. Python hat keine Symbole; `{'car': 50}` sieht ähnlich aus, der Schlüssel ist aber ein String.

Eine **Menge** enthält jeden Wert höchstens einmal: in Ruby `Set['q0', 'q1']`, in Python `{'q0', 'q1'}` (eine leere Menge ist `set()`, denn `{}` ist ein leeres Dictionary). `a | b` vereinigt, `a & b` schneidet, `x in s` bzw. `s.include?(x)` fragt. Als Schlüssel eines Dictionary braucht Python eine Menge, die sich nicht ändern kann: `frozenset(s)`.
<!-- /part -->

## Funktionen

<!-- part: rp-funktionen -->
<!-- short: def … end und def …:; Ruby gibt den letzten Wert zurück, Python ohne return None; Blöcke und lambda. -->
```ruby
def distance(a, b, scale = 1)
  Math.sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2) * scale
end
```

```python
import math

def distance(a, b, scale=1):
    return math.sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2) * scale
```

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>Rückgabe</th><td>der Wert der letzten Zeile, auch ohne <code>return</code></td><td>nur mit <code>return</code>; sonst <code>None</code></td></tr>
<tr><th>zwei Werte zurückgeben</th><td><code>return a, b</code> gibt ein Array, <code>x, y = f</code> packt aus</td><td><code>return a, b</code> gibt ein Tupel, <code>x, y = f()</code> packt aus</td></tr>
<tr><th>aufrufen ohne Argumente</th><td><code>f</code> oder <code>f()</code></td><td>immer <code>f()</code>; <code>f</code> allein ist die Funktion selbst</td></tr>
<tr><th>eine kleine Funktion als Wert</th><td><code>square = -&gt;(x) { x * x }</code>, aufrufen mit <code>square.(4)</code></td><td><code>square = lambda x: x * x</code>, aufrufen mit <code>square(4)</code></td></tr>
<tr><th>ein Stück Code mitgeben</th><td>ein Block: <code>a.map { |x| x * 2 }</code>, über mehrere Zeilen <code>do |x|</code> … <code>end</code></td><td>eine Funktion oder ein <code>lambda</code>: <code>sorted(a, key=lambda x: -x)</code></td></tr>
<tr><th>eine Variable von außen ändern</th><td>nur globale mit <code>$</code>: <code>$visits += 1</code></td><td><code>global visits</code> in der Funktion, dann <code>visits += 1</code></td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Dateien und Bytes

<!-- part: rp-dateien -->
<!-- short: File.read und open().read(), binär mit File.binread und 'rb'; bytes und b'…'; pack('C*') und bytes([…]). -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>eine Textdatei lesen</th><td><code>File.read('a.txt')</code></td><td><code>open('a.txt', encoding='utf-8').read()</code></td></tr>
<tr><th>Zeile für Zeile, ohne Zeilenende</th><td><code>File.readlines('a.txt', chomp: true)</code></td><td><code>for line in open('a.txt', encoding='utf-8'):</code>, dann <code>line.rstrip('\n')</code></td></tr>
<tr><th>schreiben</th><td><code>File.write('a.txt', text)</code></td><td><code>open('a.txt', 'w', encoding='utf-8').write(text)</code></td></tr>
<tr><th>gibt es die Datei?</th><td><code>File.exist?('a.txt')</code></td><td><code>from pathlib import Path</code>, dann <code>Path('a.txt').exists()</code></td></tr>
<tr><th>eine Datei als Bytes lesen</th><td><code>data = File.binread('bild.gif')</code>, ein String</td><td><code>data = open('bild.gif', 'rb').read()</code>, vom Typ <code>bytes</code></td></tr>
<tr><th>Bytes als Zahlen</th><td><code>data.bytes</code>, <code>data.each_byte { |b| … }</code></td><td><code>list(data)</code>, <code>for b in data:</code></td></tr>
<tr><th>Zahlen zu Bytes</th><td><code>[65, 66].pack('C*')</code> → <code>'AB'</code></td><td><code>bytes([65, 66])</code> → <code>b'AB'</code></td></tr>
<tr><th>Text zu Bytes und zurück</th><td><code>text.b</code>; zurück <code>data.force_encoding('UTF-8')</code></td><td><code>text.encode()</code>; zurück <code>data.decode()</code></td></tr>
</tbody>
</table>
</div>

Python trennt Text (`str`, `'AB'`) und Bytes (`bytes`, `b'AB'`) streng: `'A' + b'B'` ist ein Fehler, und ein Byte aus `data[0]` ist eine Zahl. In Ruby ist beides ein String; `File.binread` und `.b` sorgen nur dafür, dass er Byte für Byte gelesen wird. Wie man Zahlen aus mehreren Bytes liest und mit Bits rechnet, steht unter [Bits und Bytes](/reference/bits-und-bytes#endian).
<!-- /part -->

## Fehler

<!-- part: rp-fehler -->
<!-- short: begin/rescue und try/except; NoMethodError für nil und AttributeError für None; TypeError beim Mischen von Zahl und Text. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th>einen Fehler melden</th><td><code>raise ArgumentError, 'leer'</code></td><td><code>raise ValueError('leer')</code></td></tr>
<tr><th>einen Fehler abfangen</th><td><code>begin</code> … <code>rescue ZeroDivisionError =&gt; e</code> … <code>ensure</code> … <code>end</code></td><td><code>try:</code> … <code>except ZeroDivisionError as e:</code> … <code>finally:</code></td></tr>
</tbody>
</table>
</div>

Ruby nennt in der Meldung zuerst Datei und Zeile, Python zeigt einen **Traceback**, bei dem die wichtigste Zeile ganz unten steht. Häufige Meldungen:

- **Etwas ist leer**: Ruby `undefined method 'upcase' for nil (NoMethodError)`, Python `AttributeError: 'NoneType' object has no attribute 'upper'`. Ein Schlüssel, den es nicht gibt, eine Methode, die nichts zurückgibt, oder in Python ein vergessenes `return`.
- **Ein Name, den es nicht gibt**: Ruby `undefined local variable or method 'x' for main (NameError)`, Python `NameError: name 'x' is not defined`. Vertippt, oder die Variable steht in einer anderen Funktion.
- **Zahl plus Text**: Ruby `String can't be coerced into Integer (TypeError)`, Python `TypeError: unsupported operand type(s) for +: 'int' and 'str'`. Erst umwandeln, mit `to_i` oder `int(…)`.
- **Text plus Zahl**: Ruby `no implicit conversion of Integer into String (TypeError)`, Python `TypeError: can only concatenate str (not "int") to str`. Einsetzen mit `"#{x}"` oder `f'{x}'`.
- **Durch 0 geteilt**: Ruby `divided by 0 (ZeroDivisionError)`, Python `ZeroDivisionError: division by zero`. Oft durch die Länge einer leeren Liste.
- **Ein Block ist nicht zu Ende**: Ruby `syntax error, unexpected end-of-input`: ein `end` fehlt. Python `IndentationError: expected an indented block after 'if' statement …`: Nach dem Doppelpunkt ist nichts eingerückt.
<!-- /part -->

<!-- part: rp-fallen -->
<!-- short: Ruby rundet 7 / 2 ab, 0 ist in Ruby wahr, Python braucht return, und [[]] * 3 sind dreimal dieselbe Liste. -->
### Fallen beim Wechseln

- **Teilen**: Wer von Python kommt, bekommt in Ruby für `7 / 2` eine 3. Eine Kommazahl braucht `7.0 / 2` oder `to_f`.
- **Wahrheit**: In Ruby ist `0` wahr. `if count` fragt dort nur, ob `count` nicht `nil` ist.
- **Rückgabe**: In Python gibt eine Funktion ohne `return` `None` zurück, auch wenn die letzte Zeile ein Ergebnis ausrechnet.
- **Umwandeln**: `'zwölf'.to_i` ist in Ruby einfach 0. Python meldet bei `int('zwölf')` einen Fehler.
- **Dieselbe Liste mehrmals**: `[[]] * 3` in Python und `Array.new(3, [])` in Ruby sind drei Verweise auf ein und dieselbe Liste; was man an die erste anhängt, steht in allen. Drei eigene Listen geben `[[] for _ in range(3)]` und `Array.new(3) { [] }`.
<!-- /part -->

## Zum Nachschlagen

- [Ruby-Dokumentation](https://docs.ruby-lang.org/en/): alle Klassen und Methoden, `String`, `Array`, `Hash` und so weiter, auf Englisch.
- [Python-Dokumentation](https://docs.python.org/3/library/): die eingebauten Typen und Funktionen und alle Module, auf Englisch; das [Tutorial](https://docs.python.org/3/tutorial/) gibt es auch [auf Deutsch](https://docs.python.org/de/3/tutorial/).
- [To Ruby From Python](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/to-ruby-from-python/): was in Ruby anders ist, für alle, die Python können, auf Englisch.
