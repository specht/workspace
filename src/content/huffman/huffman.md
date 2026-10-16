<div class='meta'>
image: title.webp
kind: labor
language: Ruby / Python
languages: [ruby, python]
duration: 2 Doppelstunden
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

<style>
    .hf-stepper {
        margin: 1em 0 1.5em;
    }
    .hf-stepper input {
        font-family: '0xProto Nerd Font Mono', monospace;
        margin-bottom: 0.6em;
    }
    .hf-controls {
        display: flex;
        gap: 0.3em;
        align-items: center;
        flex-wrap: wrap;
        margin-bottom: 0.6em;
    }
    .hf-controls .hf-count {
        margin-left: 0.6em;
        color: var(--bs-secondary-color);
    }
    .hf-say {
        min-height: 3em;
    }
    .hf-forest {
        overflow-x: auto;
        margin-bottom: 0.6em;
    }
    .hf-forest svg {
        display: block;
    }
    .hf-forest line {
        stroke: var(--bs-secondary-color);
        stroke-width: 1.5;
    }
    .hf-forest .node circle, .hf-forest .leaf rect {
        fill: var(--bs-body-bg);
        stroke: var(--bs-body-color);
        stroke-width: 1.5;
    }
    .hf-forest .pick circle, .hf-forest .pick rect {
        stroke: #bc2326;
        stroke-width: 3;
    }
    .hf-forest text {
        fill: var(--bs-body-color);
        font-family: '0xProto Nerd Font Mono', monospace;
        font-size: 13px;
        text-anchor: middle;
        dominant-baseline: central;
    }
    .hf-forest text.bit {
        fill: #238acc;
        font-weight: bold;
    }
    .hf-forest text.weight {
        font-size: 11px;
        fill: var(--bs-secondary-color);
    }
    .hf-table td, .hf-table th {
        font-family: '0xProto Nerd Font Mono', monospace;
    }
    .hf-table th {
        font-family: inherit;
    }
    .hf-table {
        max-width: 36em;
    }
</style>

# Komprimieren mit Huffman

<p class='abstract'>
Im Morsecode ist E ein einziger kurzer Ton und Q ein langes Lang-lang-kurz-lang, weil E so viel häufiger vorkommt. Huffman-Codes machen dasselbe mit Bytes, aber so, dass man keine Pausen braucht, um zu wissen, wo ein Code aufhört. Du baust den Baum, aus dem die Codes kommen, in Ruby oder Python, codierst und decodierst damit und findest heraus, wie JPEG seine Huffman-Tabellen mit ein paar Bytes speichert.
</p>

## Kurze Codes für häufige Zeichen

Jedes Byte hat 8 Bits, ob es ein »e« ist, in einem deutschen Text etwa jeder sechste Buchstabe, oder ein »q«, das kaum vorkommt. Das ist bequem, aber verschwenderisch. Samuel Morse und Alfred Vail haben das schon in den 1830er Jahren besser gemacht: Im Morsecode ist E ein einziger kurzer Ton (·), T ein langer (−), und seltene Buchstaben wie Q (−−·−) sind lang. Im Mittel wird eine Nachricht so kürzer.

Der Morsecode braucht dafür Pausen zwischen den Buchstaben: Ohne sie könnte ·· ein I sein oder zweimal E. Ein Computer soll ohne Pausen auskommen. Das geht, wenn kein Code der Anfang eines anderen ist. Dann heißt die Codetabelle **präfixfrei**: Wer Bit für Bit liest, weiß in dem Moment, in dem ein Code vollständig ist, dass er zu Ende ist.

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table hf-table'>
<thead><tr><th>Zeichen</th><th>Code</th></tr></thead>
<tbody>
<tr><td>A</td><td>0</td></tr>
<tr><td>B</td><td>10</td></tr>
<tr><td>R</td><td>110</td></tr>
<tr><td>C</td><td>1110</td></tr>
<tr><td>D</td><td>1111</td></tr>
</tbody>
</table>
</div>

Mit dieser Tabelle ist `0101100` eindeutig: `0` ist A, dann `10` ist B, `110` ist R, `0` ist A. Kein Code beginnt mit einem anderen, also gibt es keine zweite Möglichkeit.

_task(praefix)

## Der Baum

Wie findet man die besten Codes, also die, mit denen ein Text so wenige Bits wie möglich braucht? Diese Frage stellte Robert Fano 1951 seinen Studierenden am MIT als Hausarbeit, statt einer Prüfung. Er selbst kannte nur ein Verfahren, das gute, aber nicht immer die besten Codes findet. David Huffman fand nach Monaten eins, das immer die besten findet, und es ist erstaunlich einfach:

1. Zähle, wie oft jedes Byte vorkommt. Jedes Byte ist ein Baum aus einem einzigen Blatt, und seine Häufigkeit ist das Gewicht des Baums.
2. Nimm die beiden leichtesten Bäume und häng sie unter einen neuen Knoten. Sein Gewicht ist die Summe ihrer Gewichte.
3. Wiederhole Schritt 2, bis nur noch ein Baum übrig ist.

Den Code eines Bytes liest du auf dem Weg von oben zu seinem Blatt ab: links eine 0, rechts eine 1. Seltene Bytes werden früh zusammengefasst und landen tief unten, mit langen Codes; häufige kommen spät dazu und bleiben oben, mit kurzen. Und weil jedes Byte an einem Blatt hängt und nie auf dem Weg zu einem anderen liegt, ist die Tabelle von selbst präfixfrei.

Probier es aus, mit dem Text unten oder einem eigenen. Rot umrandet sind die beiden Bäume, die als nächste zusammenkommen.

<div class='hf-stepper' id='hf-stepper' data-stepper>
<input class='form-control' type='text' id='hf-input' value='RHABARBERBARBARA' aria-label='Text für den Huffman-Baum' spellcheck='false'>
<div class='hf-controls'>
<button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
<span class='hf-count'></span>
</div>
<div class='hf-say' aria-live='polite'></div>
<div class='hf-forest' role='img' aria-label='Die Bäume dieses Schritts'></div>
<div class='hf-codes'></div>
</div>

Haben zwei Bäume dasselbe Gewicht, ist es egal, welchen man nimmt. Die Codes sehen dann anders aus, manche sind sogar anders lang, aber der Text braucht insgesamt genau gleich viele Bits.

_task(von-hand)

## Einrichten

Lege im Terminal (<kbd>Strg</kbd><kbd>J</kbd>) einen Ordner an und öffne ihn mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>:

```bash
mkdir /workspace/huffman
```

<!-- language: ruby -->
Lege darin die Datei `huffman.rb` an:

```ruby
# Huffman-Codes: häufige Bytes bekommen kurze Codes, seltene lange.
#
# Ein Baum ist entweder ein Blatt, das Byte selbst (eine Zahl), oder ein
# Knoten, ein Array [links, rechts]. Links steht für 0, rechts für 1.

# Wie oft jedes Byte vorkommt: {65 => 5, 66 => 2, ...}
def frequencies(data)
end

# Immer die beiden leichtesten Bäume zusammenfassen, bis einer übrig ist.
def build_tree(counts)
end

# Der Code jedes Bytes als String aus 0 und 1: {65 => '0', 66 => '10', ...}
def code_table(tree)
end

# Der ganze Text als String aus 0 und 1
def encode(data, table)
end

# Aus den Bits und dem Baum wieder die Bytes
def decode(bits, tree)
end

if __FILE__ == $0
    p frequencies('ABRACADABRA')
end
```

<!-- tutorial-step
# Das Gerüst von der Seite läuft, und noch löst es keine Aufgabe (ruby)
write-file: huffman/huffman.rb <- previous-code
check-task-fails: zaehlen
check-task-fails: baum
check-task-fails: decodieren
check-task-fails: kanonisch
-->
<!-- /language -->
<!-- language: python -->
Lege darin die Datei `huffman.py` an:

```python
# Huffman-Codes: häufige Bytes bekommen kurze Codes, seltene lange.
#
# Ein Baum ist entweder ein Blatt, das Byte selbst (eine Zahl), oder ein
# Knoten, ein Tupel (links, rechts). Links steht für 0, rechts für 1.


def frequencies(data):
    """Wie oft jedes Byte vorkommt: {65: 5, 66: 2, ...}"""


def build_tree(counts):
    """Immer die beiden leichtesten Bäume zusammenfassen, bis einer übrig ist."""


def code_table(tree):
    """Der Code jedes Bytes als String aus 0 und 1: {65: '0', 66: '10', ...}"""


def encode(data, table):
    """Der ganze Text als String aus 0 und 1"""


def decode(bits, tree):
    """Aus den Bits und dem Baum wieder die Bytes"""


if __name__ == '__main__':
    print(frequencies(b'ABRACADABRA'))
```

<!-- tutorial-step
# Das Gerüst von der Seite läuft, und noch löst es keine Aufgabe (python)
write-file: huffman/huffman.py <- previous-code
check-task-fails: zaehlen python
check-task-fails: baum python
check-task-fails: decodieren python
check-task-fails: kanonisch python
-->

`b'ABRACADABRA'` sind Bytes, keine Zeichen; alle Methoden bekommen und liefern Bytes.
<!-- /language -->

Die Bits schreiben wir der Einfachheit halber als String aus den Zeichen `0` und `1`. Echte Programme packen je 8 davon in ein Byte, wie GIF und JPEG es tun; gezählt wird hier trotzdem in Bits.

## Zählen

<!-- language: ruby -->
Schreib `frequencies`. Es gibt einen Hash zurück, der für jedes Byte des Texts sagt, wie oft es vorkommt.
<!-- /language -->
<!-- language: python -->
Schreib `frequencies`. Es gibt ein `dict` zurück, das für jedes Byte des Texts sagt, wie oft es vorkommt.
<!-- /language -->

<!-- language: ruby -->
- `data.each_byte { |byte| … }` geht die Bytes durch, als Zahlen.
- `Hash.new(0)` ist ein Hash, der für jeden Schlüssel, den es noch nicht gibt, 0 liefert. Damit kannst du gleich `counts[byte] += 1` schreiben.
<!-- /language -->
<!-- language: python -->
- `for byte in data:` geht die Bytes durch, als Zahlen.
- `counts.get(byte, 0)` liefert 0, wenn es das Byte noch nicht gibt. Damit kannst du gleich `counts[byte] = counts.get(byte, 0) + 1` schreiben.
<!-- /language -->

_task(zaehlen)

## Den Baum bauen

<!-- language: ruby -->
Schreib `build_tree` nach den drei Schritten von oben. Ein Blatt ist das Byte selbst, eine Zahl; ein Knoten ist ein Array aus seinen beiden Kindern. Für »ABRACADABRA« könnte der Baum so aussehen: `[65, [[67, 68], [66, 82]]]`.
<!-- /language -->
<!-- language: python -->
Schreib `build_tree` nach den drei Schritten von oben. Ein Blatt ist das Byte selbst, eine Zahl; ein Knoten ist ein Tupel aus seinen beiden Kindern. Für »ABRACADABRA« könnte der Baum so aussehen: `(65, ((67, 68), (66, 82)))`.
<!-- /language -->

<!-- language: ruby -->
- Beim Bauen brauchst du zu jedem Baum sein Gewicht. Halte deshalb eine Liste von Paaren `[weight, tree]`, am Anfang eines für jedes Byte.
- `sort_by! { |weight, _| weight }` sortiert die Liste nach dem Gewicht, `shift` nimmt das erste Element heraus. Zweimal `shift` nach dem Sortieren gibt dir die beiden leichtesten.
- Am Ende ist ein einziges Paar übrig, und sein Baum ist das Ergebnis.
<!-- /language -->
<!-- language: python -->
- Beim Bauen brauchst du zu jedem Baum sein Gewicht. Halte deshalb eine Liste von Paaren `(weight, tree)`, am Anfang eines für jedes Byte.
- `trees.sort(key=lambda pair: pair[0])` sortiert die Liste nach dem Gewicht, `trees.pop(0)` nimmt das erste Element heraus. Zweimal `pop(0)` nach dem Sortieren gibt dir die beiden leichtesten.
- Am Ende ist ein einziges Paar übrig, und sein Baum ist das Ergebnis.
<!-- /language -->

<!-- language: ruby -->
Dann `code_table`: Es geht den Baum von oben durch und merkt sich den Weg. Am einfachsten ist das mit einer Methode, die sich selbst aufruft, für das linke Kind mit einer `0` mehr im Weg, für das rechte mit einer `1`. Kommt sie an ein Blatt, also an eine Zahl (`tree.is_a?(Integer)`), ist der Weg bis dorthin der Code dieses Bytes. Eine Methode darf dafür weitere Parameter mit Standardwerten haben, etwa `def code_table(tree, prefix = '', table = {})`.
<!-- /language -->
<!-- language: python -->
Dann `code_table`: Es geht den Baum von oben durch und merkt sich den Weg. Am einfachsten ist das mit einer Funktion, die sich selbst aufruft, für das linke Kind mit einer `0` mehr im Weg, für das rechte mit einer `1`. Kommt sie an ein Blatt, also an eine Zahl (`isinstance(tree, int)`), ist der Weg bis dorthin der Code dieses Bytes. Eine Funktion darf dafür weitere Parameter mit Standardwerten haben, etwa `def code_table(tree, prefix='')`; `table.update(…)` nimmt die Einträge eines anderen `dict` dazu.
<!-- /language -->

Ein Sonderfall: Besteht der Text aus einem einzigen Byte, immer wieder, ist der Baum nur ein Blatt, und der Weg dorthin ist leer. Gib diesem Byte dann den Code `0`.

_task(baum)

## Codieren und decodieren

<!-- language: ruby -->
`encode` ist kurz: Für jedes Byte der Code aus der Tabelle, alle hintereinander. `join` hängt eine Liste von Strings aneinander.
<!-- /language -->
<!-- language: python -->
`encode` ist kurz: Für jedes Byte der Code aus der Tabelle, alle hintereinander. `''.join(…)` hängt Strings aneinander.
<!-- /language -->

<!-- language: ruby -->
`decode` liest die Bits einzeln und geht dabei im Baum nach unten, bei `0` nach links (`node[0]`), bei `1` nach rechts (`node[1]`). Kommt es an ein Blatt, hat es ein Byte gefunden, merkt es sich und beginnt wieder oben. Wie bei LZW macht `pack('C*')` am Schluss aus den Zahlen einen String. Denk auch hier an den Baum, der nur ein Blatt ist.
<!-- /language -->
<!-- language: python -->
`decode` liest die Bits einzeln und geht dabei im Baum nach unten, bei `0` nach links (`node[0]`), bei `1` nach rechts (`node[1]`). Kommt es an ein Blatt, hat es ein Byte gefunden, merkt es sich und beginnt wieder oben. Wie bei LZW macht `bytes(…)` am Schluss aus den Zahlen Bytes. Denk auch hier an den Baum, der nur ein Blatt ist.
<!-- /language -->

_task(decodieren)

## Wie gut ist das?

Lade die Spezifikation von GIF in den Ordner, einen Text von 1990 mit 83.003 Bytes, und ein Foto als JPEG:

```bash_copy
wget ${web_root}/lzw/spec-gif89a.txt
wget ${web_root}/jpeg/44-baseline.jpg
```

<!-- language: ruby -->
`File.binread('spec-gif89a.txt')` liest sie Byte für Byte. Lass dein Programm ausgeben, wie viele Bits `encode` daraus macht, und probier gleich, ob `decode` die Datei wieder genau herausgibt.
<!-- /language -->
<!-- language: python -->
`open('spec-gif89a.txt', 'rb').read()` liest sie als Bytes; das `b` in `'rb'` ist wichtig. Lass dein Programm ausgeben, wie viele Bits `encode` daraus macht, und probier gleich, ob `decode` die Datei wieder genau herausgibt.
<!-- /language -->

_task(spec)

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead>
<tr><th><code>spec-gif89a.txt</code></th><th>Bytes</th></tr>
</thead>
<tbody>
<tr><td>Original</td><td>83.003</td></tr>
<tr><td>Huffman</td><td>dein Ergebnis</td></tr>
<tr><td><a href='/lzw'>LZW</a>, mit wachsender Breite der Codes</td><td>28.813</td></tr>
<tr><td><code>gzip -9</code></td><td>20.271</td></tr>
</tbody>
</table>
</div>

Huffman sieht jedes Byte einzeln an: Es weiß, dass »e« häufig ist, aber nicht, dass nach »sc« fast immer ein »h« kommt. LZW findet genau solche Folgen und schlägt Huffman bei diesem Text deutlich. Am besten ist, beides zu verbinden: `gzip` sucht erst Wiederholungen, mit LZ77, einem Verwandten von LZW, und codiert das Ergebnis dann mit Huffman. JPEG macht es ähnlich: Erst werden die Zahlen eines Bildes so umgerechnet, dass viele Nullen entstehen, dann codiert Huffman, was übrig bleibt.

_task(foto)

## Die Tabelle mitschicken

Wer decodieren will, braucht die Codes. Anders als bei LZW lassen sie sich nicht beim Lesen erraten; die Tabelle muss mit in die Datei. Den ganzen Baum zu speichern, wäre aufwendig. Es geht mit viel weniger: Es genügt, für jedes Byte die **Länge** seines Codes zu speichern, wenn beide Seiten die Codes nach derselben festen Regel daraus berechnen. Solche Codes heißen **kanonisch**:

1. Sortiere die Bytes nach der Länge ihres Codes, bei gleicher Länge nach dem Byte.
2. Das erste Byte bekommt lauter Nullen, so viele, wie sein Code lang ist.
3. Jedes weitere bekommt den Code des vorigen plus 1. Ist sein Code länger als der vorige, kommen hinten Nullen dazu, bis die Länge stimmt.

Aus den Längen A: 2, B: 2, C: 2, D: 3, E: 3 werden so die Codes A = `00`, B = `01`, C = `10`, D = `110` (`10` + 1 = `11`, mit einer Null dahinter), E = `111`. Die Längen sind dieselben wie vorher, also braucht der Text genauso viele Bits, nur die Codes selbst haben sich geändert.

Genau so speichert JPEG seine Huffman-Tabellen im [DHT-Segment](/jpeg): 16 Zahlen, wie viele Codes es mit 1, 2, … 16 Bits gibt, dann die Symbole in ihrer Reihenfolge. Daraus berechnet der Decoder die Codes. Ein Code darf bei JPEG höchstens 16 Bits haben; ein Encoder, dessen Baum tiefer wird, muss ihn so umbauen, dass kein Code länger ist.

<!-- language: ruby -->
Schreib `canonical(lengths)`: Es bekommt einen Hash mit der Länge jedes Bytes und gibt einen Hash mit den kanonischen Codes zurück, als Strings aus 0 und 1. `code.to_s(2)` schreibt eine Zahl als Binärzahl, `rjust(length, '0')` füllt vorn mit Nullen auf, und `<<` schiebt eine Zahl nach links, hängt also hinten Nullen an: `0b10 << 1` ist `0b100`.
<!-- /language -->
<!-- language: python -->
Schreib `canonical(lengths)`: Es bekommt ein `dict` mit der Länge jedes Bytes und gibt ein `dict` mit den kanonischen Codes zurück, als Strings aus 0 und 1. `format(code, 'b')` schreibt eine Zahl als Binärzahl, `rjust(length, '0')` füllt vorn mit Nullen auf, und `<<` schiebt eine Zahl nach links, hängt also hinten Nullen an: `0b10 << 1` ist `0b100`. `sorted(lengths.items(), key=lambda item: (item[1], item[0]))` sortiert nach Länge und dann nach Byte.
<!-- /language -->

_task(kanonisch)

## Weiter geht's

- **JPEG**: In [Eine JPEG-Datei decodieren](/jpeg) liest du die Huffman-Tabellen aus einem DHT-Segment und decodierst damit Bit für Bit die Koeffizienten eines Fotos.
<!-- language: ruby -->
- **Bits packen**: Schreib `encode` so um, dass es echte Bytes liefert, mit `[bits].pack('B*')`, und `decode` passend dazu. Wie erkennt `decode`, wo die Bits zu Ende sind, wenn das letzte Byte nicht voll ist?
<!-- /language -->
<!-- language: python -->
- **Bits packen**: Schreib `encode` so um, dass es echte Bytes liefert, mit `int(bits[i:i + 8].ljust(8, '0'), 2)` für je 8 Bits, und `decode` passend dazu. Wie erkennt `decode`, wo die Bits zu Ende sind, wenn das letzte Byte nicht voll ist?
<!-- /language -->
- **Bruchteile von Bits**: Ein Byte, das 90 % eines Texts ausmacht, bekommt bei Huffman mindestens ein ganzes Bit, obwohl ihm nach der Rechnung von Claude Shannon gut ein Siebtel Bit genügen würde. Die arithmetische Codierung kommt dem näher; such heraus, wie sie funktioniert.

<script>
document.addEventListener('DOMContentLoaded', function() {
    // The Huffman stepper: counts the bytes of the text in the input, merges
    // the two lightest trees step by step and draws the forest as SVG.
    // Ties go to the tree that was made first (leaves by byte value), so the
    // same text always gives the same tree.
    const root = document.getElementById('hf-stepper');
    if (!root) return;
    const input = document.getElementById('hf-input');
    const label = (b) => b === 32 ? '␣' : (b > 32 && b < 127 ? String.fromCharCode(b) : b.toString(16).padStart(2, '0'));
    const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

    function build(data) {
        const counts = new Map();
        data.forEach((b) => counts.set(b, (counts.get(b) || 0) + 1));
        let made = 0;
        let forest = [...counts.keys()].sort((a, b) => a - b).map((b) => ({ weight: counts.get(b), byte: b, made: made++ }));
        const order = (list) => list.slice().sort((a, b) => a.weight - b.weight || a.made - b.made);
        const steps = [{ forest: order(forest), pick: [] }];
        while (forest.length > 1) {
            const sorted = order(forest);
            const [left, right] = sorted;
            steps[steps.length - 1].pick = [left, right];
            const node = { weight: left.weight + right.weight, left: left, right: right, made: made++ };
            forest = sorted.slice(2).concat([node]);
            steps.push({ forest: order(forest), pick: [], made: node });
        }
        return { counts, steps };
    }

    function leaves(t) { return t.left ? leaves(t.left) + leaves(t.right) : 1; }
    function depth(t) { return t.left ? 1 + Math.max(depth(t.left), depth(t.right)) : 0; }

    const DX = 46, DY = 54, TOP = 22;
    function draw(t, x0, picks, out) {
        // In-order positions: leaves left to right, a node above the middle of its children.
        let next = 0;
        const place = (n, d) => {
            if (!n.left) { n.x = x0 + (next++ + 0.5) * DX; n.y = TOP + d * DY; return; }
            place(n.left, d + 1); place(n.right, d + 1);
            n.x = (n.left.x + n.right.x) / 2; n.y = TOP + d * DY;
        };
        place(t, 0);
        const walk = (n) => {
            if (!n.left) return;
            [[n.left, '0'], [n.right, '1']].forEach(([c, bit]) => {
                out.push(`<line x1='${n.x}' y1='${n.y}' x2='${c.x}' y2='${c.y}'/>`);
                out.push(`<text class='bit' x='${(n.x + c.x) / 2 + (bit === '0' ? -8 : 8)}' y='${(n.y + c.y) / 2 - 4}'>${bit}</text>`);
                walk(c);
            });
        };
        walk(t);
        const nodes = (n) => {
            const pick = picks.includes(t) && n === t ? ' pick' : '';
            if (!n.left) {
                out.push(`<g class='leaf${pick}'><rect x='${n.x - 15}' y='${n.y - 14}' width='30' height='28' rx='5'/><text x='${n.x}' y='${n.y}'>${esc(label(n.byte))}</text><text class='weight' x='${n.x}' y='${n.y + 23}'>${n.weight}</text></g>`);
            } else {
                out.push(`<g class='node${pick}'><circle cx='${n.x}' cy='${n.y}' r='14'/><text x='${n.x}' y='${n.y}'>${n.weight}</text></g>`);
                nodes(n.left); nodes(n.right);
            }
        };
        nodes(t);
        return leaves(t) * DX;
    }

    function codes(t, prefix, table) {
        if (!t.left) { table.set(t.byte, prefix || '0'); return table; }
        codes(t.left, prefix + '0', table);
        codes(t.right, prefix + '1', table);
        return table;
    }

    let built = { steps: [] }, current = 0;
    function render() {
        const step = built.steps[current];
        const count = built.steps.length;
        root.querySelector('.hf-count').textContent = count ? `Schritt ${current + 1} von ${count}` : '';
        const say = root.querySelector('.hf-say');
        const forest = root.querySelector('.hf-forest');
        const table = root.querySelector('.hf-codes');
        if (!step) { say.textContent = 'Tipp oben einen Text ein.'; forest.innerHTML = ''; table.innerHTML = ''; return; }
        const name = (t) => t.left ? `den Baum mit Gewicht ${t.weight}` : `»${esc(label(t.byte))}« (${t.weight})`;
        if (step.pick.length) {
            say.innerHTML = current === 0
                ? `Jedes Byte ist ein Blatt, mit seiner Häufigkeit als Gewicht. Die beiden leichtesten sind ${name(step.pick[0])} und ${name(step.pick[1])}.`
                : `Ein neuer Knoten mit Gewicht ${step.made.weight}. Als Nächstes kommen ${name(step.pick[0])} und ${name(step.pick[1])} zusammen.`;
        } else {
            say.innerHTML = count === 1
                ? 'Nur ein einziges Byte: Der Baum ist ein Blatt, und sein Code ist 0.'
                : `Fertig: ein einziger Baum. Die Codes stehen auf den Wegen von oben zu den Blättern.`;
        }
        const out = [];
        let x = 0;
        step.forest.forEach((t) => { x += draw(t, x, step.pick, out) + 12; });
        const height = TOP + Math.max(...step.forest.map(depth)) * DY + 34;
        forest.innerHTML = `<svg width='${x}' height='${height}' viewBox='0 0 ${x} ${height}'>${out.join('')}</svg>`;
        if (!step.pick.length) {
            const tree = step.forest[0];
            const table_ = codes(tree, '', new Map());
            const rows = [...table_.entries()].sort((a, b) => built.counts.get(b[0]) - built.counts.get(a[0]) || a[0] - b[0]);
            let total = 0;
            const body = rows.map(([b, c]) => {
                const n = built.counts.get(b);
                total += n * c.length;
                return `<tr><td>${esc(label(b))}</td><td>${n}</td><td>${c}</td><td>${n * c.length}</td></tr>`;
            }).join('');
            const bytes = [...built.counts.values()].reduce((a, b) => a + b, 0);
            table.innerHTML = `<table class='table table-sm hf-table'><thead><tr><th>Byte</th><th>Anzahl</th><th>Code</th><th>Bits</th></tr></thead><tbody>${body}</tbody></table>` +
                `<p>Zusammen ${total} Bits statt ${bytes * 8}, etwa ${Math.round(100 * total / (bytes * 8))} %.</p>`;
        } else {
            table.innerHTML = '';
        }
        root.querySelector("[data-go='first']").disabled = current <= 0;
        root.querySelector("[data-go='prev']").disabled = current <= 0;
        root.querySelector("[data-go='next']").disabled = current >= count - 1;
        root.querySelector("[data-go='last']").disabled = current >= count - 1;
    }
    const go = (k) => { current = Math.max(0, Math.min(built.steps.length - 1, k)); render(); };
    root.querySelector("[data-go='first']").addEventListener('click', () => go(0));
    root.querySelector("[data-go='prev']").addEventListener('click', () => go(current - 1));
    root.querySelector("[data-go='next']").addEventListener('click', () => go(current + 1));
    root.querySelector("[data-go='last']").addEventListener('click', () => go(built.steps.length - 1));
    function refresh() {
        const data = Array.from(new TextEncoder().encode(input.value.slice(0, 200)));
        built = data.length ? build(data) : { steps: [], counts: new Map() };
        current = 0;
        render();
    }
    input.addEventListener('input', refresh);
    refresh();
});
</script>

<!-- tutorial-step
# Die Musterlösungen aus loesung/ lösen jede Programmieraufgabe, in beiden Sprachen
write-file: huffman/huffman.rb <- file:loesung/huffman.rb
write-file: huffman/huffman.py <- file:loesung/huffman.py
check-task: zaehlen
check-task: zaehlen python
check-task: baum
check-task: baum python
check-task: decodieren
check-task: decodieren python
check-task: kanonisch
check-task: kanonisch python
-->
