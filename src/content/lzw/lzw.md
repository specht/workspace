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
    .lzw-stepper {
        margin: 1em 0 1.5em;
    }
    .lzw-input {
        display: flex;
        gap: 0.5em;
        flex-wrap: wrap;
        margin-bottom: 0.6em;
    }
    .lzw-input input {
        flex: 1 1 16em;
        min-width: 0;
        font-family: '0xProto Nerd Font Mono', monospace;
    }
    .lzw-controls {
        display: flex;
        gap: 0.3em;
        align-items: center;
        flex-wrap: wrap;
        margin-bottom: 0.6em;
    }
    .lzw-controls .lzw-count {
        margin-left: 0.6em;
        color: var(--bs-secondary-color);
    }
    .lzw-text {
        font-family: '0xProto Nerd Font Mono', monospace;
        font-size: 1.05em;
        line-height: 2;
        word-break: break-all;
        margin-bottom: 0.6em;
    }
    .lzw-codes span.done {
        opacity: 0.55;
    }
    .lzw-codes span.now {
        padding: 0 0.15em;
        color: var(--bs-body-color);
        font-weight: 600;
        background-color: color-mix(in srgb, #ffe617 75%, transparent);
        border-radius: 0.2em;
    }
    .lzw-text span.done {
        opacity: 0.45;
    }
    .lzw-text span.now {
        background-color: color-mix(in srgb, #ffe617 75%, transparent);
        border-radius: 0.2em;
    }
    .lzw-text span.next {
        outline: 2px solid #bc2326;
        outline-offset: -1px;
        border-radius: 0.2em;
    }
    .lzw-steps {
        max-height: 24em;
        overflow-y: auto;
    }
    .lzw-steps table {
        margin: 0;
    }
    .lzw-steps td, .lzw-steps th {
        font-family: '0xProto Nerd Font Mono', monospace;
        white-space: nowrap;
    }
    .lzw-steps th {
        font-family: inherit;
    }
    .lzw-steps tr.current td {
        background-color: color-mix(in srgb, #ffe617 45%, transparent);
    }
    .lzw-steps .special {
        color: #bc2326;
        font-weight: bold;
    }
    .lzw-summary {
        margin-top: 0.6em;
        color: var(--bs-secondary-color);
    }
    html[data-bs-theme="dark"] .lzw-text span.now,
    html[data-bs-theme="dark"] .lzw-codes span.now {
        background-color: color-mix(in srgb, #7a6a00 75%, transparent);
    }
    html[data-bs-theme="dark"] .lzw-steps tr.current td {
        background-color: color-mix(in srgb, #7a6a00 60%, transparent);
    }
    html[data-bs-theme="dark"] .lzw-steps .special {
        color: #f384ae;
    }
</style>

# Komprimieren mit LZW

<p class='abstract'>
Rhabarberbarbara, Barbarabar, Barbarbarbier: Wo sich etwas wiederholt, lässt es sich kürzer sagen. LZW merkt sich beim Lesen jede Folge von Bytes, die schon einmal vorkam, und schreibt beim nächsten Mal nur noch eine Nummer. Das Wörterbuch dafür wird nie mitgeschickt: Wer entpackt, baut es beim Lesen selbst noch einmal. Du schreibst beides selbst, in Ruby oder Python, den Kompressor und den Entpacker, und probierst sie an Texten, Fotos und der Spezifikation von GIF aus.
</p>

## Wiederholungen

Ein Text wiederholt sich ständig: Buchstabenfolgen wie »ch«, »sch«, »ein« und »der« kommen auf jeder Seite dutzendfach vor, ganze Wörter auch. Ein Programm, das komprimiert, sucht solche Wiederholungen und ersetzt sie durch etwas Kürzeres. Beim Entpacken muss genau das Original zurückkommen, Byte für Byte; das heißt **verlustfrei**.

Das Verfahren auf dieser Seite haben Abraham Lempel und Jacob Ziv 1978 erfunden und Terry Welch 1984 so vereinfacht, wie es seitdem benutzt wird, daher der Name **LZW**. Es steckt in GIF-Bildern, in TIFF, in alten PDF-Dateien und im Unix-Programm `compress`. Die Idee ist ein **Wörterbuch**, in dem jede Folge von Bytes, die schon vorkam, eine Nummer hat, den **Code**:

- Am Anfang stehen darin die 256 möglichen Bytes, jedes einzeln. Das Byte 65 (»A«) hat den Code 65.
- Jedes Mal, wenn das Programm einen Code ausgibt, kommt ein neuer Eintrag dazu, mit dem nächsten freien Code: 256, 257, 258, …
- Ein Code steht also für eine immer längere Folge, und wo sich etwas wiederholt, ersetzt ein einziger Code mehrere Bytes.

## Komprimieren

Der Kompressor liest den Text Byte für Byte und merkt sich das **Präfix**, das er gerade aufbaut: den Anfang des restlichen Texts, der schon im Wörterbuch steht. Für jedes Byte gilt:

1. Steht das Präfix mit dem neuen Byte dahinter schon im Wörterbuch, wird das Präfix um dieses Byte länger.
2. Sonst gibt er den Code des Präfixes aus, trägt das Präfix mit dem neuen Byte als neuen Eintrag ein und beginnt ein neues Präfix, das nur aus dem neuen Byte besteht.
3. Ist der Text zu Ende, gibt er den Code des letzten Präfixes aus.

Probier es aus. Jeder Klick liest ein Byte; das gelbe ist das Präfix, das rot umrandete das nächste Byte. Du kannst auch einen eigenen Text eintippen.

<div class='lzw-stepper' id='lzw-encode' data-stepper>
<div class='lzw-input'>
<input class='form-control' type='text' id='lzw-encode-input' value='RHABARBERBARBARABARBARBARENBARTBARBIER' aria-label='Text zum Komprimieren' spellcheck='false'>
</div>
<div class='lzw-controls'>
<button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
<span class='lzw-count'></span>
</div>
<div class='lzw-text' aria-live='polite'></div>
<div class='lzw-steps'></div>
<div class='lzw-summary'></div>
</div>

Bei der Rhabarberbarbara stehen nach dem ersten »BAR« für »BA«, »AR« und »RB« eigene Einträge im Wörterbuch, und schon das zweite »BARBARA« braucht weniger Codes als Buchstaben. Ganz am Anfang kann LZW nichts sparen: Das Wörterbuch kennt noch nichts außer den einzelnen Bytes.

_task(von-hand)

## Einrichten

Lege im Terminal (<kbd>Strg</kbd><kbd>J</kbd>) einen Ordner an und öffne ihn mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>:

```bash
mkdir /workspace/lzw
```

<!-- language: ruby -->
Lege darin die Datei `lzw.rb` an:

```ruby
# LZW: komprimieren und entpacken

# Macht aus den Bytes eines Strings eine Liste von Codes.
def compress(data)
    codes = []
    codes
end

# Macht aus einer Liste von Codes wieder die Bytes, als String.
def decompress(codes)
    ''
end

p compress('ABABABA')
```

<!-- tutorial-step
# Das Gerüst von der Seite läuft, und noch löst es keine Aufgabe (ruby)
write-file: lzw/lzw.rb <- previous-code
check-task-fails: kompressor
check-task-fails: entpacker
-->

Ein String besteht aus **Bytes**, Zahlen von 0 bis 255: `'ABABABA'` sind die Bytes 65, 66, 65, … Was unten außerhalb der Methoden steht, ist dein Programm: Es läuft, wenn du die Datei startest, mit `ruby lzw.rb`. Die Prüfungen der Aufgaben laden nur deine Methoden und rufen sie direkt auf; dein Programm läuft dabei nicht. Im Moment gibt das Programm eine leere Liste aus; das änderst du jetzt.
<!-- /language -->
<!-- language: python -->
Lege darin die Datei `lzw.py` an:

```python
# LZW: komprimieren und entpacken


def compress(data):
    """Macht aus Bytes eine Liste von Codes."""
    codes = []
    return codes


def decompress(codes):
    """Macht aus einer Liste von Codes wieder die Bytes."""
    return b''


print(compress(b'ABABABA'))
```

<!-- tutorial-step
# Das Gerüst von der Seite läuft, und noch löst es keine Aufgabe (python)
write-file: lzw/lzw.py <- previous-code
check-task-fails: kompressor python
check-task-fails: entpacker python
-->

`b'ABABABA'` ist kein Text, sondern eine Folge von **Bytes**, Zahlen von 0 bis 255. Was unten außerhalb der Funktionen steht, ist dein Programm: Es läuft, wenn du die Datei startest, mit `python3 lzw.py`. Die Prüfungen der Aufgaben laden nur deine Funktionen und rufen sie direkt auf; dein Programm läuft dabei nicht. Im Moment gibt das Programm eine leere Liste aus; das änderst du jetzt.
<!-- /language -->

## Der Kompressor

<!-- language: ruby -->
Schreib `compress` nach den drei Regeln oben. Was du aus Ruby dafür brauchst:

- `data.each_byte { |byte| … }` geht die Bytes eines Strings durch und liefert Zahlen: für `'AB'` erst 65, dann 66.
- Ein Präfix ist am einfachsten ein Array von Zahlen, etwa `[65, 66]` für »AB«. `current + [byte]` ist ein neues Array mit einer Zahl mehr.
- Das Wörterbuch ist ein Hash mit solchen Arrays als Schlüsseln und Codes als Werten. Am Anfang enthält es für jede Zahl `i` von 0 bis 255 den Eintrag `[i]` mit dem Code `i`. Der nächste freie Code ist dann immer `table.size`, und `table.key?(prefix)` sagt, ob es ein Präfix `prefix` schon gibt.
- Das Präfix am Anfang ist leer, `[]`. Achte am Ende darauf, dass ein leerer Text keinen Code ergibt.
<!-- /language -->
<!-- language: python -->
Schreib `compress` nach den drei Regeln oben. Was du aus Python dafür brauchst:

- Eine `for`-Schleife über Bytes liefert Zahlen: `for byte in b'AB'` gibt erst 65, dann 66.
- Aus einer Zahl wird mit `bytes([byte])` wieder ein Byte, und `+` hängt Bytes aneinander: `b'AB' + bytes([67])` ist `b'ABC'`.
- Das Wörterbuch ist ein `dict` mit Bytes als Schlüsseln und Codes als Werten. Am Anfang enthält es für jede Zahl `i` von 0 bis 255 den Eintrag `bytes([i])` mit dem Code `i`. Der nächste freie Code ist dann immer `len(table)`.
- Das Präfix am Anfang ist leer, `b''`. Achte am Ende darauf, dass ein leerer Text keinen Code ergibt.
<!-- /language -->

_task(kompressor)

## Entpacken

Der Entpacker bekommt nur die Codes. Die Codes unter 256 versteht er sofort, sie sind einzelne Bytes. Für alle anderen baut er das Wörterbuch selbst nach, so wie der Kompressor es gebaut hat, nur einen Schritt später:

1. Der erste Code ist immer ein einzelnes Byte. Schreib es hin; es ist der **vorige Eintrag** für den nächsten Schritt.
2. Für jeden weiteren Code: Schlag den Eintrag nach und schreib ihn hin.
3. Trag dann den vorigen Eintrag mit dem **ersten Byte** des jetzigen dahinter als neuen Eintrag ein. Genau diesen Eintrag hat der Kompressor angelegt, als er den vorigen Code ausgegeben hat, denn das nächste Byte war das erste Byte des jetzigen Eintrags.

Das Wörterbuch des Entpackers ist also immer einen Eintrag hinter dem des Kompressors. Meistens macht das nichts, aber nicht immer:

_task(sonderfall)

Der Kompressor kann einen Eintrag benutzen, den er gerade erst angelegt hat. Das passiert, wenn sich ein Präfix sofort wiederholt und dabei mit seinem eigenen ersten Byte weitergeht, wie bei `ABABABA`. Der Entpacker kennt diesen Eintrag noch nicht, aber er kann ihn ausrechnen: Es ist der vorige Eintrag mit dessen eigenem ersten Byte dahinter. Das ist die einzige Ausnahme von Regel 2.

Hier entpackt die Zeitlupe die Codes des Texts, der oben im Feld steht; die Ausnahme ist rot markiert. Tipp oben `ABABABA` oder `AAAAAAAA` ein und sieh hier zu.

<div class='lzw-stepper' id='lzw-decode' data-stepper>
<div class='lzw-controls'>
<button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
<button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
<span class='lzw-count'></span>
</div>
<div class='lzw-text' aria-live='polite'></div>
<div class='lzw-steps'></div>
</div>

<!-- language: ruby -->
Schreib jetzt `decompress`. Diesmal ist das Wörterbuch andersherum: ein Hash mit den Codes als Schlüsseln und den Einträgen, Arrays von Zahlen, als Werten. Sammle alle Zahlen in einem Array; `entry[0]` ist das erste Byte eines Eintrags. Am Schluss macht `pack('C*')` aus dem Array von Zahlen wieder einen String: `[65, 66].pack('C*')` ist `'AB'`.
<!-- /language -->
<!-- language: python -->
Schreib jetzt `decompress`. Diesmal ist das Wörterbuch andersherum: ein `dict` mit den Codes als Schlüsseln und den Bytes als Werten. `entry[:1]` ist das erste Byte eines Eintrags, als Bytes (`entry[0]` wäre eine Zahl). Am Schluss setzt `b''.join(parts)` eine Liste von Bytes wieder zu einer einzigen Folge zusammen.
<!-- /language -->

_task(entpacker)

## Wie gut ist das?

<!-- language: ruby -->
Bisher zählen wir Codes. Gespeichert werden muss aber jeder Code mit so vielen Bits, wie er braucht, und die Codes werden immer größer. Beim i-ten Code (ab 0 gezählt) hat das Wörterbuch 256 + i Einträge, der größte mögliche Code ist also 255 + i. So viele Bits braucht der Code: In Ruby sagt das `(255 + i).bit_length`. Schreib eine Funktion `size_in_bytes(codes)`, die alle Bits zusammenzählt und in Bytes umrechnet, aufgerundet.
<!-- /language -->
<!-- language: python -->
Bisher zählen wir Codes. Gespeichert werden muss aber jeder Code mit so vielen Bits, wie er braucht, und die Codes werden immer größer. Beim i-ten Code (ab 0 gezählt) hat das Wörterbuch 256 + i Einträge, der größte mögliche Code ist also 255 + i. So viele Bits braucht der Code: In Python sagt das `(255 + i).bit_length()`. Schreib eine Funktion `size_in_bytes(codes)`, die alle Bits zusammenzählt und in Bytes umrechnet, aufgerundet.
<!-- /language -->

Lade dann zwei Dateien zum Ausprobieren in den Ordner: die Spezifikation von GIF, ein Text von 1990 mit 83.003 Bytes, und ein Foto als JPEG.

```bash_copy
wget ${web_root}/lzw/spec-gif89a.txt
wget ${web_root}/jpeg/44-baseline.jpg
```

<!-- language: ruby -->
Eine Datei liest Ruby mit `File.binread('spec-gif89a.txt')` Byte für Byte, so wie sie ist. `ARGV[0]` ist der Name, den du beim Start mitgibst: `ruby lzw.rb spec-gif89a.txt`. Lass dein Programm für eine Datei ausgeben, wie groß sie ist, wie viele Codes `compress` daraus macht und wie viele Bytes die brauchen. Probier dabei auch gleich, ob `decompress` wieder genau die Datei ergibt.
<!-- /language -->
<!-- language: python -->
Eine Datei liest Python mit `open('spec-gif89a.txt', 'rb').read()` als Bytes; das `b` in `'rb'` ist wichtig, sonst bekommst du Text statt Bytes. `sys.argv[1]` ist der Name, den du beim Start mitgibst (`python3 lzw.py spec-gif89a.txt`), nach `import sys`. Lass dein Programm für eine Datei ausgeben, wie groß sie ist, wie viele Codes `compress` daraus macht und wie viele Bytes die brauchen. Probier dabei auch gleich, ob `decompress` wieder genau die Datei ergibt.
<!-- /language -->

_task(spec)

Echte Programme komprimieren noch besser. `gzip` benutzt LZ77, die erste Idee von Lempel und Ziv von 1977, und codiert das Ergebnis danach noch mit [Huffman](/huffman):

```bash
gzip -9 -k spec-gif89a.txt
ls -l spec-gif89a.txt*
```

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead>
<tr><th><code>spec-gif89a.txt</code></th><th>Bytes</th></tr>
</thead>
<tbody>
<tr><td>Original</td><td>83.003</td></tr>
<tr><td>LZW, mit wachsender Breite der Codes</td><td>dein Ergebnis</td></tr>
<tr><td><code>gzip -9</code></td><td>20.271</td></tr>
</tbody>
</table>
</div>

Und das Foto?

_task(foto)

_task(warum)

## Ein volles Wörterbuch

Unser Wörterbuch wächst ohne Grenze: Bei der Spezifikation hat es am Ende über 17.000 Einträge, bei einer großen Datei wären es Millionen. Echte Programme setzen eine Grenze. GIF erlaubt höchstens 4096 Einträge, also Codes mit höchstens 12 Bits; `compress` geht bis 16 Bits. Ist das Wörterbuch voll, kommen keine Einträge mehr dazu, oder es beginnt von vorn: Der Kompressor schickt dafür einen eigenen Code, den Clear Code, und beide Seiten fangen wieder mit den 256 Bytes an.

Bau das in deine beiden Funktionen ein: Reserviere den Code 256 als Clear Code, die neuen Einträge beginnen dann bei 257. Hat das Wörterbuch 4096 Einträge, gibt `compress` den Clear Code aus, und beide Seiten beginnen neu. Vergleich mit `size_in_bytes`, wie viel größer die Spezifikation dadurch wird, und überleg, warum ein frisches Wörterbuch bei einem langen Text sogar helfen kann.

## Weiter geht's

- **GIF**: In einem GIF-Bild sind die »Bytes« die Nummern der Farben, und die Codes stehen Bit für Bit in der Datei. Wie das genau aussieht, steht in [Eine GIF-Datei decodieren](/gif), mit einem Decoder in C.
- **Das Patent**: Auf LZW hatte die Firma Unisys ein Patent, und ab 1994 verlangte sie Lizenzgebühren von Programmen, die GIF-Dateien schrieben. Als Antwort entstand 1995 das Format PNG, das ohne LZW auskommt. Das Patent lief 2003 in den USA und 2004 in Europa aus.
- **Andere Texte**: Komprimiere Texte in anderen Sprachen, eine Datei aus lauter gleichen Bytes, eine mit Zufallszahlen (`head -c 100000 /dev/urandom > zufall.bin`). Wann lohnt sich LZW, wann nicht?

<script>
document.addEventListener('DOMContentLoaded', function() {
    // Two steppers: one compresses the text in the input, one decompresses
    // the codes that come out. Both work on the UTF-8 bytes of the text.
    const encodeRoot = document.getElementById('lzw-encode');
    const decodeRoot = document.getElementById('lzw-decode');
    if (!encodeRoot || !decodeRoot) return;
    const input = document.getElementById('lzw-encode-input');

    const show = (bytes) => {
        let out = '';
        for (const b of bytes) {
            if (b >= 32 && b < 127) out += String.fromCharCode(b);
            else out += '\\x' + b.toString(16).padStart(2, '0');
        }
        return out.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    };
    const key = (bytes) => bytes.join(',');

    function compressSteps(data) {
        const table = new Map();
        for (let i = 0; i < 256; i++) table.set(String(i), i);
        let size = 256;
        const steps = [];
        const codes = [];
        let current = [];
        let start = 0;
        data.forEach((byte, i) => {
            const longer = current.concat([byte]);
            if (current.length === 0) {
                current = [byte];
                start = i;
                return;
            }
            if (table.has(key(longer))) {
                steps.push({ at: i, start: start, piece: current, byte: byte, known: true });
                current = longer;
            } else {
                const code = table.get(key(current));
                codes.push(code);
                table.set(key(longer), size);
                steps.push({ at: i, start: start, piece: current, byte: byte, known: false, code: code, entry: longer, index: size });
                size++;
                current = [byte];
                start = i;
            }
        });
        if (current.length) {
            const code = table.get(key(current));
            codes.push(code);
            steps.push({ at: data.length, start: start, piece: current, end: true, code: code });
        }
        return { steps, codes };
    }

    function decompressSteps(codes) {
        const table = new Map();
        for (let i = 0; i < 256; i++) table.set(i, [i]);
        const steps = [];
        let previous = null;
        let length = 0;
        codes.forEach((code) => {
            let entry, special = false;
            if (table.has(code)) entry = table.get(code);
            else { entry = previous.concat([previous[0]]); special = true; }
            const step = { code: code, entry: entry, special: special, from: length };
            if (previous !== null) {
                const added = previous.concat([entry[0]]);
                step.index = table.size;
                step.added = added;
                table.set(table.size, added);
            }
            length += entry.length;
            step.to = length;
            steps.push(step);
            previous = entry;
        });
        return steps;
    }

    function stepper(root, render) {
        let current = 0, count = 0;
        const go = (k) => { current = Math.max(0, Math.min(count - 1, k)); count = render(current) || count; update(); };
        const update = () => {
            root.querySelector("[data-go='first']").disabled = current <= 0;
            root.querySelector("[data-go='prev']").disabled = current <= 0;
            root.querySelector("[data-go='next']").disabled = current >= count - 1;
            root.querySelector("[data-go='last']").disabled = current >= count - 1;
        };
        root.querySelector("[data-go='first']").addEventListener('click', () => go(0));
        root.querySelector("[data-go='prev']").addEventListener('click', () => go(current - 1));
        root.querySelector("[data-go='next']").addEventListener('click', () => go(current + 1));
        root.querySelector("[data-go='last']").addEventListener('click', () => go(count - 1));
        return {
            reset: (n) => { count = n; current = 0; render(0); update(); },
            scroll: () => {
                const box = root.querySelector('.lzw-steps');
                const row = box.querySelector('tr.current');
                if (row) box.scrollTop = row.offsetTop - box.clientHeight + row.offsetHeight + 8;
            },
        };
    }

    let data = [], encoded = { steps: [], codes: [] }, decoded = [];

    const enc = stepper(encodeRoot, (k) => {
        const step = encoded.steps[k];
        encodeRoot.querySelector('.lzw-count').textContent = encoded.steps.length ? `Schritt ${k + 1} von ${encoded.steps.length}` : '';
        encodeRoot.querySelector('.lzw-text').innerHTML = data.map((b, i) => {
            let cls = '';
            if (step && i < step.start) cls = 'done';
            if (step && i >= step.start && i < step.start + step.piece.length) cls = 'now';
            if (step && !step.end && i === step.at) cls = 'next';
            return `<span class='${cls}'>${show([b])}</span>`;
        }).join('');
        const rows = encoded.steps.slice(0, k + 1).map((s, i) => {
            const cls = i === k ? 'current' : '';
            if (s.end) return `<tr class='${cls}'><td>${show(s.piece)}</td><td>Ende</td><td></td><td><strong>${s.code}</strong></td><td></td></tr>`;
            const both = show(s.piece) + show([s.byte]);
            return `<tr class='${cls}'><td>${show(s.piece)}</td><td>${show([s.byte])}</td><td>${s.known ? both + ' ist bekannt' : both + ' ist neu'}</td><td>${s.known ? '' : '<strong>' + s.code + '</strong>'}</td><td>${s.known ? '' : s.index + ' = ' + show(s.entry)}</td></tr>`;
        });
        encodeRoot.querySelector('.lzw-steps').innerHTML = rows.length ?
            `<table class='table table-sm'><thead><tr><th>Präfix</th><th>Byte</th><th>Im Wörterbuch?</th><th>Ausgabe</th><th>Neuer Eintrag</th></tr></thead><tbody>${rows.join('')}</tbody></table>` : '';
        enc.scroll();
        return encoded.steps.length;
    });

    const dec = stepper(decodeRoot, (k) => {
        const step = decoded[k];
        const all = [];
        decoded.forEach((s) => all.push(...s.entry));
        // The codes, the one being read now marked, the ones before it faint
        decodeRoot.querySelector('.lzw-count').innerHTML = decoded.length ?
            `Code ${k + 1} von ${decoded.length}: <span class='lzw-codes'>` +
            encoded.codes.map((c, i) => `<span class='${i < k ? 'done' : i === k ? 'now' : ''}'>${c}</span>`).join(' ') +
            '</span>' : '';
        decodeRoot.querySelector('.lzw-text').innerHTML = all.map((b, i) => {
            let cls = '';
            if (step && i >= step.to) return `<span style='visibility: hidden;'>${show([b])}</span>`;
            if (step && i < step.from) cls = 'done';
            if (step && i >= step.from && i < step.to) cls = 'now';
            return `<span class='${cls}'>${show([b])}</span>`;
        }).join('');
        const rows = decoded.slice(0, k + 1).map((s, i) => {
            const cls = i === k ? 'current' : '';
            const what = s.special ? `<span class='special'>noch nicht da: voriger + sein erstes Byte</span>` : (s.code < 256 ? 'ein Byte' : 'bekannt');
            return `<tr class='${cls}'><td><strong>${s.code}</strong></td><td>${what}</td><td>${show(s.entry)}</td><td>${s.added ? s.index + ' = ' + show(s.added) : ''}</td></tr>`;
        });
        decodeRoot.querySelector('.lzw-steps').innerHTML = rows.length ?
            `<table class='table table-sm'><thead><tr><th>Code</th><th>Im Wörterbuch?</th><th>Ausgabe</th><th>Neuer Eintrag</th></tr></thead><tbody>${rows.join('')}</tbody></table>` : '';
        dec.scroll();
        return decoded.length;
    });

    function refresh() {
        data = Array.from(new TextEncoder().encode(input.value.slice(0, 400)));
        encoded = compressSteps(data);
        decoded = decompressSteps(encoded.codes);
        const bits = encoded.codes.reduce((sum, _, i) => sum + (255 + i).toString(2).length, 0);
        encodeRoot.querySelector('.lzw-summary').textContent = data.length ?
            `${data.length} Bytes werden zu ${encoded.codes.length} Codes: ${encoded.codes.join(' ')}. Mit so vielen Bits pro Code, wie nötig sind, wären das ${Math.ceil(bits / 8)} Bytes.` : '';
        enc.reset(encoded.steps.length);
        dec.reset(decoded.length);
    }
    input.addEventListener('input', refresh);
    refresh();
});
</script>

<!-- tutorial-step
# Die Musterlösungen aus loesung/ lösen jede Programmieraufgabe, in beiden Sprachen
write-file: lzw/lzw.rb <- file:loesung/lzw.rb
write-file: lzw/lzw.py <- file:loesung/lzw.py
check-task: kompressor
check-task: kompressor python
check-task: entpacker
check-task: entpacker python
-->
