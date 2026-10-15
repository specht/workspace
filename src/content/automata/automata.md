<div class='meta'>
image: teaser.webp
kind: projekt
language: Python
duration: 2 Doppelstunden
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Endliche Automaten in Python

<p class='abstract'>
Ein Drehkreuz, ein Zahlenschloss und ein Automat, der durch 3 teilt, ohne zu rechnen: Du baust endliche Automaten in Python und findest heraus, warum ein Schloss bei 1 2 1 2 1 3 nicht aufgeht, obwohl der Code 1213 ist. Dann schreibst du einen Automaten, der raten darf, verwandelst ihn mit einem kleinen Programm in einen, der nicht raten muss, und siehst zu, wie der explodiert.
</p>

Ein Drehkreuz am Schwimmbad, ein Getränkeautomat, eine Ampel: Sie alle sind in jedem Moment in einem von wenigen **Zuständen** (zu, offen; grün, gelb, rot). Jede Eingabe, eine Münze, ein Knopfdruck, ein Takt, lässt sie nach einer festen Regel in einen anderen Zustand wechseln. Mehr gibt es nicht: keine Variablen, kein Gedächtnis außer dem Zustand. So ein Gerät heißt in der Informatik **endlicher Automat**, und mit genau dieser Idee sucht dein Editor nach Wörtern, prüfen Programme, ob eine E-Mail-Adresse richtig aussieht, und zerlegt ein Compiler deinen Code in Wörter.

In dieser Aktivität programmierst du Automaten in Python, und zwar so einfach wie möglich: Ein Automat ist eine Tabelle, ein Python-`dict`, und eine Schleife, die die Eingabe Zeichen für Zeichen liest.

## Einrichten

Lege im Terminal (<kbd>Strg</kbd><kbd>J</kbd>) einen Ordner an und öffne ihn mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>:

```bash
mkdir /workspace/automata
```

Alle Dateien dieser Aktivität kommen in diesen Ordner. Ein Programm startest du im Terminal mit `python3` und dem Namen der Datei.

## Ein Drehkreuz

Das Drehkreuz am Eingang zum Schwimmbad ist erst zu. Wirfst du eine Münze ein, ist es offen; gehst du durch (»drücken«), ist es wieder zu. Drückst du, ohne zu zahlen, bleibt es zu, und eine zweite Münze in ein offenes Drehkreuz ist verschenkt. Als Bild, mit einem Kreis für jeden Zustand und einem Pfeil für jeden Übergang:

<svg class="automaton" role="img" aria-label="Zwei Kreise: zu und offen. Ein Pfeil ohne Anfang zeigt auf zu, dort beginnt das Drehkreuz. münze führt von zu nach offen, drücken von offen zurück nach zu. drücken in zu und münze in offen führen jeweils im Bogen zum selben Zustand zurück." style="max-width: 22em; width: 100%; margin: 1em auto;"
 viewBox="0.00 0.00 250.72 98.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 94)">
<g id="node2" class="node state">
<ellipse fill="white" stroke="black" cx="66.72" cy="-29" rx="29" ry="29"/>
<text text-anchor="middle" x="66.72" y="-25.5" font-size="15.00">zu</text>
</g>
<g id="edge1" class="edge">
<path fill="none" stroke="black" d="M0.77,-29C1.68,-29 15.69,-29 30.46,-29"/>
<polygon fill="black" stroke="black" points="30.72,-31.45 37.72,-29 30.72,-26.55 30.72,-31.45"/>
</g>
<g id="edge4" class="edge">
<path fill="none" stroke="black" d="M58.11,-56.99C57.83,-67.42 60.7,-76 66.72,-76 71.23,-76 73.98,-71.17 74.95,-64.36"/>
<polygon fill="black" stroke="black" points="77.41,-64.1 75.33,-56.99 72.52,-63.85 77.41,-64.1"/>
<text text-anchor="middle" x="66.72" y="-79.6" font-size="13.00">drücken</text>
</g>
<g id="node3" class="node state">
<ellipse fill="white" stroke="black" cx="213.72" cy="-29" rx="29" ry="29"/>
<text text-anchor="middle" x="213.72" y="-25.5" font-size="15.00">offen</text>
</g>
<g id="edge2" class="edge">
<path fill="none" stroke="black" d="M96.02,-29C119.28,-29 152.34,-29 177.34,-29"/>
<polygon fill="black" stroke="black" points="177.49,-31.45 184.49,-29 177.49,-26.55 177.49,-31.45"/>
<text text-anchor="middle" x="140.22" y="-32.6" font-size="13.00">münze</text>
</g>
<g id="edge3" class="edge">
<path fill="none" stroke="black" d="M186.95,-16.98C180.48,-14.51 173.46,-12.27 166.72,-11 143.57,-6.63 136.87,-6.63 113.72,-11 109.3,-11.83 104.75,-13.08 100.32,-14.54"/>
<polygon fill="black" stroke="black" points="99.26,-12.32 93.49,-16.98 100.91,-16.94 99.26,-12.32"/>
<text text-anchor="middle" x="140.22" y="-14.6" font-size="13.00">drücken</text>
</g>
<g id="edge5" class="edge">
<path fill="none" stroke="black" d="M200.38,-55.1C199.29,-66.36 203.74,-76 213.72,-76 221.52,-76 225.94,-70.12 226.98,-62.17"/>
<polygon fill="black" stroke="black" points="229.43,-62.13 227.06,-55.1 224.53,-62.07 229.43,-62.13"/>
<text text-anchor="middle" x="213.72" y="-79.6" font-size="13.00">münze</text>
</g>
</g>
</svg>

Lege `turnstile.py` an:

```python
# Ein Drehkreuz wie am Eingang zum Schwimmbad. Für jeden Zustand und jede
# Eingabe steht hier, in welchen Zustand das Drehkreuz wechselt.
TRANSITIONS = {
    ('zu', 'münze'): 'offen',
    ('zu', 'drücken'): 'zu',
    ('offen', 'münze'): 'offen',
    ('offen', 'drücken'): 'zu',
}


def run(inputs):
    state = 'zu'
    for symbol in inputs:
        new_state = TRANSITIONS[(state, symbol)]
        print(f'{state:>5}  {symbol:<8} → {new_state}')
        state = new_state
    return state


run(['drücken', 'münze', 'drücken', 'drücken'])
```

`TRANSITIONS` ist die Tabelle des Automaten: Zu jedem Paar aus Zustand und Eingabe steht dort der neue Zustand, so wie zu jedem Pfeil im Bild. `TRANSITIONS[('zu', 'münze')]` ist `'offen'`. Die Funktion `run` beginnt in `zu`, liest die Eingaben der Reihe nach, schlägt für jede den neuen Zustand nach und gibt ihn aus. Starte das Programm:

```bash
python3 turnstile.py
```

<!-- tutorial-screenshot
# Ordner automata, turnstile.py offen, darunter die Ausgabe
viewport: 1029x720
write-file: automata/turnstile.py <- file:stufen/turnstile.py
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: automata
press: Enter
wait-for-input-value: /workspace/automata/
press: Enter
wait-for-text: turnstile.py
open-file: turnstile.py
hide-left-sidebar
terminal-open
terminal-run: clear
terminal-run: python3 turnstile.py
terminal-wait-for-prompt
move-mouse: selector:.terminal
-->
<img class='full' src='drehkreuz.webp' alt='Oben im Editor turnstile.py mit der Tabelle TRANSITIONS und der Funktion run, unten im Terminal vier Zeilen Ausgabe: zu, drücken, zu; zu, münze, offen; offen, drücken, zu; zu, drücken, zu.'>

Jede Zeile ist ein Pfeil im Bild: der alte Zustand, die Eingabe und der neue Zustand. Probier eigene Folgen aus, indem du die letzte Zeile änderst. Was passiert, wenn du `run(['tritt'])` schreibst?

_task(drehkreuz)

## Ein Automat, der rechnen kann

Das Drehkreuz macht etwas. Viele Automaten beantworten stattdessen eine Frage über ein Wort: Gehört es dazu oder nicht? Sie haben dafür einen **Startzustand** und einen oder mehrere **akzeptierende Zustände**, im Bild mit doppeltem Rand. Der Automat liest das Wort; endet er in einem akzeptierenden Zustand, nimmt er das Wort an.

Hier ist ein Automat, der Binärzahlen liest und annimmt, wenn sie durch 3 teilbar sind:

<svg class="automaton" role="img" aria-label="Drei Kreise für die Reste 0, 1 und 2; 0 ist der Start und hat einen doppelten Rand, weil dort Zahlen enden, die durch 3 teilbar sind. Von 0 führt 0 zurück zu 0 und 1 zu 1. Von 1 führt 1 zu 0 und 0 zu 2. Von 2 führt 0 zu 1 und 1 zurück zu 2." style="max-width: 24em; width: 100%; margin: 1em auto;"
 viewBox="0.00 0.00 251.72 84.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 80)">
<g id="node2" class="node accept">
<ellipse fill="white" stroke="black" cx="59.72" cy="-22" rx="18" ry="18"/>
<ellipse fill="none" stroke="black" cx="59.72" cy="-22" rx="22" ry="22"/>
<text text-anchor="middle" x="59.72" y="-18.5" font-size="15.00">0</text>
</g>
<g id="edge1" class="edge">
<path fill="none" stroke="black" d="M0.89,-22C2.56,-22 16.6,-22 30.43,-22"/>
<polygon fill="black" stroke="black" points="30.58,-24.45 37.58,-22 30.58,-19.55 30.58,-24.45"/>
</g>
<g id="edge2" class="edge">
<path fill="none" stroke="black" d="M52.4,-42.99C51.5,-53.09 53.94,-62 59.72,-62 64.06,-62 66.51,-56.99 67.09,-50.22"/>
<polygon fill="black" stroke="black" points="69.54,-49.97 67.04,-42.99 64.64,-50.01 69.54,-49.97"/>
<text text-anchor="middle" x="59.72" y="-65.6" font-size="13.00">0</text>
</g>
<g id="node3" class="node state">
<ellipse fill="white" stroke="black" cx="144.72" cy="-22" rx="18" ry="18"/>
<text text-anchor="middle" x="144.72" y="-18.5" font-size="15.00">1</text>
</g>
<g id="edge3" class="edge">
<path fill="none" stroke="black" d="M82.09,-22C93.55,-22 107.71,-22 119.56,-22"/>
<polygon fill="black" stroke="black" points="119.6,-24.45 126.6,-22 119.6,-19.55 119.6,-24.45"/>
<text text-anchor="middle" x="104.22" y="-25.6" font-size="13.00">1</text>
</g>
<g id="edge4" class="edge">
<path fill="none" stroke="black" d="M129,-12.46C122.97,-9.08 115.76,-5.7 108.72,-4 100.91,-2.11 92.63,-3.79 85.19,-6.77"/>
<polygon fill="black" stroke="black" points="83.96,-4.64 78.63,-9.79 86.01,-9.09 83.96,-4.64"/>
<text text-anchor="middle" x="104.22" y="-7.6" font-size="13.00">1</text>
</g>
<g id="node4" class="node state">
<ellipse fill="white" stroke="black" cx="225.72" cy="-22" rx="18" ry="18"/>
<text text-anchor="middle" x="225.72" y="-18.5" font-size="15.00">2</text>
</g>
<g id="edge5" class="edge">
<path fill="none" stroke="black" d="M162.86,-22C173.84,-22 188.24,-22 200.39,-22"/>
<polygon fill="black" stroke="black" points="200.61,-24.45 207.61,-22 200.61,-19.55 200.61,-24.45"/>
<text text-anchor="middle" x="185.22" y="-25.6" font-size="13.00">0</text>
</g>
<g id="edge6" class="edge">
<path fill="none" stroke="black" d="M210.81,-11.38C202.33,-6.16 191.15,-1.48 180.72,-4 175.99,-5.14 171.18,-7.05 166.71,-9.19"/>
<polygon fill="black" stroke="black" points="165.52,-7.05 160.44,-12.46 167.78,-11.4 165.52,-7.05"/>
<text text-anchor="middle" x="185.22" y="-7.6" font-size="13.00">0</text>
</g>
<g id="edge7" class="edge">
<path fill="none" stroke="black" d="M218.69,-38.66C217.13,-48.62 219.47,-58 225.72,-58 230.41,-58 232.9,-52.73 233.19,-45.89"/>
<polygon fill="black" stroke="black" points="235.62,-45.5 232.75,-38.66 230.73,-45.8 235.62,-45.5"/>
<text text-anchor="middle" x="225.72" y="-61.6" font-size="13.00">1</text>
</g>
</g>
</svg>

Wie kann er das, ohne zu rechnen? Sein Zustand ist der **Rest**, der bisher beim Teilen durch 3 bleibt. Liest er von links eine weitere Ziffer, verdoppelt sich die Zahl, und die Ziffer kommt dazu: Aus `11` (3) wird mit einer `0` die `110` (6), mit einer `1` die `111` (7). Mit dem Rest passiert dasselbe: Aus Rest `r` wird mit der Ziffer `d` der Rest von `2 · r + d`. Mehr als den Rest muss sich der Automat nicht merken. Für `110`:

<table class='table' style='width: auto;'>
<tr><th>gelesen</th><td><code>1</code></td><td><code>11</code></td><td><code>110</code></td></tr>
<tr><th>Wert</th><td>1</td><td>3</td><td>6</td></tr>
<tr><th>Zustand (Rest)</th><td>1</td><td>0</td><td>0</td></tr>
</table>

Er endet in 0, also ist 6 durch 3 teilbar. Lege `divisible.py` an:

```python
# Ist eine Binärzahl durch 3 teilbar? Der Automat liest sie Ziffer für Ziffer
# von links. Sein Zustand ist der Rest, der bis dahin beim Teilen durch 3 bleibt.
START = 0
ACCEPTING = {0}
TRANSITIONS = {
    (0, '0'): 0, (0, '1'): 1,
    (1, '0'): 2, (1, '1'): 0,
    (2, '0'): 1, (2, '1'): 2,
}


def accepts(word):
    state = START
    for symbol in word:
        state = TRANSITIONS[(state, symbol)]
    return state in ACCEPTING


for word in ['11', '110', '111', '1001', '10000']:
    print(word, int(word, 2), accepts(word))
```

`START` und `ACCEPTING` sind neu: Die Funktion `accepts` beginnt in `START` und sagt am Ende, ob der letzte Zustand in der Menge `ACCEPTING` liegt. Zum Vergleich gibt das Programm neben jedem Wort seinen Wert aus; `int(word, 2)` liest eine Binärzahl.

```bash
python3 divisible.py
```

_task(rest)

Der Automat hat nur drei Zustände und liest jede Ziffer genau einmal. Darum ist ihm egal, wie lang die Zahl ist. `bin(3 ** 1000)` schreibt 3 hoch 1000 als Binärzahl, mit `0b` davor, das `[2:]` abschneidet. Hänge diese Zeile an und probier es aus:

```python
print(accepts(bin(3 ** 1000)[2:]))
```

Das sind 1585 Ziffern, und `accepts` merkt sich nie mehr als eine Zahl von 0 bis 2.

_task(teilbar-5)

### Was ein Automat nicht kann

Die Stärke des Automaten ist auch seine Grenze: Er weiß nur, in welchem Zustand er ist, und davon hat er endlich viele. Was er sich merken müsste, ohne dass es eine feste Obergrenze gibt, kann er nicht. Darum heißen diese Automaten **endlich**.

_task(grenzen)

## Das Zahlenschloss

Am Fahrradkeller hängt ein Zahlenschloss mit Tastenfeld. Es hat keine Enter-Taste: Es geht auf, sobald die letzten vier getippten Ziffern der Code sind, 1213. Als Automat merkt es sich, wie viel vom Code schon stimmt; darum heißen seine Zustände so, wie das, was vom Code schon getippt ist:

```python
# Ein Zahlenschloss mit dem Code 1213. Es öffnet, sobald die letzten vier
# getippten Ziffern der Code sind. Der Zustand merkt sich, wie viel vom Code
# schon stimmt.
START = 'start'
ACCEPTING = {'1213'}
TRANSITIONS = {
    ('start', '1'): '1',
    ('1', '2'): '12',
    ('12', '1'): '121',
    ('121', '3'): '1213',
}


def opens(digits):
    state = START
    for digit in digits:
        # Fehlt ein Übergang, geht es zurück zum Start.
        state = TRANSITIONS.get((state, digit), START)
    return state in ACCEPTING


for digits in ['1213', '991213', '12', '121213']:
    print(digits, opens(digits))
```

Neu ist `TRANSITIONS.get((state, digit), START)`: `get` sucht den Übergang in der Tabelle, und gibt es ihn nicht, nimmt es `START`. So muss nicht jede der zehn Ziffern in jedem Zustand einzeln in der Tabelle stehen; alles, was nicht zum Code passt, führt zurück zum Anfang. Starte das Programm:

<!-- tutorial-screenshot
# lock.py: 121213 öffnet nicht
write-file: automata/lock.py <- file:stufen/lock.py
terminal-open
terminal-maximize
terminal-run: clear
terminal-run: python3 lock.py
terminal-wait-for-prompt
crop-terminal-lines: auto
-->
<img class='full' src='schloss.webp' alt='python3 lock.py gibt aus: 1213 True, 991213 True, 12 False und 121213 False.'>

Die ersten drei Zeilen stimmen. Aber die letzte: Lena hat sich vertippt, 1 2 1 2, und dann richtig weitergetippt, 1 3. Die letzten vier Ziffern sind 1213, und trotzdem bleibt das Schloss zu. Geh die Ziffern mit dem Automaten durch: Nach `121` kommt eine `2`. Das ist nicht die erwartete `3`, also geht es zurück nach `start`. Dabei hat Lena mit `12` gerade wieder den Anfang des Codes getippt. Der Automat hat es vergessen.

_task(schloss)

### Automatron zeichnet nach

Für jede Ziffer in jedem Zustand die richtige Antwort zu finden, ist Denkarbeit. Ein Werkzeug kann sie dir abnehmen: [Automatron](/automatron) baut aus einem **regulären Ausdruck** einen Automaten. `(0|1|2|3|4|5|6|7|8|9)*1213` heißt: beliebig viele Ziffern (`|` ist »oder«, `*` »beliebig oft, auch keinmal«), danach `1213`. Gib den Ausdruck in Automatron ein. Unter »Minimierung« zeigt es den kleinsten Automaten, der dasselbe tut:

_tool(automatron)

<!-- tutorial-screenshot
# Automatron: der minimierte DEA für das Schloss
open-site: /automatron
tab: preview
viewport: 1029x700
click: selector:#ti_input
type: (0|1|2|3|4|5|6|7|8|9)*1213
press: Enter
wait-for-text: kleinster DEA
sleep: 1
crop-element: #output_area svg:nth-of-type(3)
-->
<img class='full' src='automatron-schloss.webp' alt='Automatron zeichnet fünf Zustände s0 bis s4 in einer Reihe, s4 grün mit doppeltem Rand. Mit 1 geht es von s0 nach s1, mit 2 nach s2, mit 1 nach s3, mit 3 nach s4. Zurück führen: 1 von s1 nach s1, 2 von s3 nach s2, 1 von s3 nach s1, 1 von s4 nach s1, alle übrigen Ziffern nach s0.'>

`s0` ist `start`, `s1` ist `1`, `s2` ist `12`, `s3` ist `121` und `s4` ist `1213`. Vergleiche die Pfeile mit deiner Tabelle: Automatron zeichnet auch die Pfeile zurück nach `s0`, die in `lock.py` das `get` erledigt. Darüber steht, wie Automatron zu diesem Automaten gekommen ist. Einen Teil davon baust du gleich selbst.

## Ein Automat, der rät

Beim Schloss steckte die Arbeit in den Pfeilen zurück. Es geht auch anders, wenn der Automat raten darf: In `start` wartet er. Kommt eine 1, *könnte* hier der Code anfangen, oder auch nicht. Der Automat nimmt einfach beides: Er bleibt in `start` und geht zugleich nach `1`. Ab da folgt er dem Code; passt eine Ziffer nicht, endet dieser Weg einfach.

<svg class="automaton" role="img" aria-label="Fünf Kreise in einer Reihe: start, 1, 12, 121 und 1213, der letzte mit doppeltem Rand. start führt mit jeder Ziffer 0 bis 9 zu sich selbst zurück und mit 1 außerdem nach 1. Danach geht es mit 2 nach 12, mit 1 nach 121 und mit 3 nach 1213." style="max-width: 36em; width: 100%; margin: 1em auto;"
 viewBox="0.00 0.00 483.72 94.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 90)">
<g id="node2" class="node state">
<ellipse fill="white" stroke="black" cx="62.72" cy="-29" rx="25" ry="25"/>
<text text-anchor="middle" x="62.72" y="-25.5" font-size="15.00">start</text>
</g>
<g id="edge1" class="edge">
<path fill="none" stroke="black" d="M0.76,-29C1.66,-29 15.93,-29 30.44,-29"/>
<polygon fill="black" stroke="black" points="30.52,-31.45 37.52,-29 30.52,-26.55 30.52,-31.45"/>
</g>
<g id="edge2" class="edge">
<path fill="none" stroke="black" d="M54.94,-52.88C54.3,-63.18 56.9,-72 62.72,-72 67.09,-72 69.64,-67.04 70.37,-60.21"/>
<polygon fill="black" stroke="black" points="72.83,-59.92 70.5,-52.88 67.93,-59.83 72.83,-59.92"/>
<text text-anchor="middle" x="62.72" y="-75.6" font-size="13.00">0–9</text>
</g>
<g id="node4" class="node state">
<ellipse fill="white" stroke="black" cx="157.72" cy="-29" rx="25" ry="25"/>
<text text-anchor="middle" x="157.72" y="-25.5" font-size="15.00">1</text>
</g>
<g id="edge3" class="edge">
<path fill="none" stroke="black" d="M88.15,-29C99.47,-29 113.07,-29 125.15,-29"/>
<polygon fill="black" stroke="black" points="125.45,-31.45 132.45,-29 125.45,-26.55 125.45,-31.45"/>
<text text-anchor="middle" x="110.22" y="-32.6" font-size="13.00">1</text>
</g>
<g id="node3" class="node accept">
<ellipse fill="white" stroke="black" cx="446.72" cy="-29" rx="25" ry="25"/>
<ellipse fill="none" stroke="black" cx="446.72" cy="-29" rx="29" ry="29"/>
<text text-anchor="middle" x="446.72" y="-25.5" font-size="15.00">1213</text>
</g>
<g id="node5" class="node state">
<ellipse fill="white" stroke="black" cx="252.72" cy="-29" rx="25" ry="25"/>
<text text-anchor="middle" x="252.72" y="-25.5" font-size="15.00">12</text>
</g>
<g id="edge4" class="edge">
<path fill="none" stroke="black" d="M183.15,-29C194.47,-29 208.07,-29 220.15,-29"/>
<polygon fill="black" stroke="black" points="220.45,-31.45 227.45,-29 220.45,-26.55 220.45,-31.45"/>
<text text-anchor="middle" x="205.22" y="-32.6" font-size="13.00">2</text>
</g>
<g id="node6" class="node state">
<ellipse fill="white" stroke="black" cx="347.72" cy="-29" rx="25" ry="25"/>
<text text-anchor="middle" x="347.72" y="-25.5" font-size="15.00">121</text>
</g>
<g id="edge5" class="edge">
<path fill="none" stroke="black" d="M278.15,-29C289.47,-29 303.07,-29 315.15,-29"/>
<polygon fill="black" stroke="black" points="315.45,-31.45 322.45,-29 315.45,-26.55 315.45,-31.45"/>
<text text-anchor="middle" x="300.22" y="-32.6" font-size="13.00">1</text>
</g>
<g id="edge6" class="edge">
<path fill="none" stroke="black" d="M373.2,-29C384.46,-29 398.03,-29 410.37,-29"/>
<polygon fill="black" stroke="black" points="410.47,-31.45 417.47,-29 410.47,-26.55 410.47,-31.45"/>
<text text-anchor="middle" x="395.22" y="-32.6" font-size="13.00">3</text>
</g>
</g>
</svg>

So ein Automat heißt **nichtdeterministischer endlicher Automat**, kurz **NEA**. Die Automaten von vorhin, bei denen jeder Schritt feststeht, heißen **deterministisch**, kurz **DEA**. Ein NEA nimmt ein Wort an, wenn wenigstens einer seiner Wege in einem akzeptierenden Zustand endet.

Python kann nicht raten, aber es kann alle Wege zugleich verfolgen: Statt eines Zustands merkt sich `opens` eine **Menge** von Zuständen, nämlich alle, in denen der NEA gerade sein könnte. Die Tabelle führt darum zu Mengen. Lege `lock_nfa.py` an:

```python_copy
# Das Zahlenschloss als nichtdeterministischer endlicher Automat (NEA).
# Ein Übergang führt hier zu einer Menge von Zuständen: Der NEA darf raten.
DIGITS = '0123456789'
START = 'start'
ACCEPTING = {'1213'}
TRANSITIONS = {}

# In start wartet der NEA: Jede Ziffer führt wieder nach start …
for digit in DIGITS:
    TRANSITIONS[('start', digit)] = {'start'}
# … aber bei einer 1 darf er auch raten, dass hier der Code beginnt.
TRANSITIONS[('start', '1')] = {'start', '1'}
TRANSITIONS[('1', '2')] = {'12'}
TRANSITIONS[('12', '1')] = {'121'}
TRANSITIONS[('121', '3')] = {'1213'}


def opens(digits):
    states = {START}
    for digit in digits:
        new_states = set()
        for state in states:
            new_states |= TRANSITIONS.get((state, digit), set())
        states = new_states
        print(digit, sorted(states))
    return len(states & ACCEPTING) > 0


if __name__ == '__main__':
    print(opens('121213'))
```

Ein paar Zeichen sind neu. `{'start', '1'}` ist eine Menge; `a |= b` fügt alle Elemente von `b` zur Menge `a` hinzu, und `states & ACCEPTING` sind die Zustände, die in beiden Mengen liegen. Der Teil unter `if __name__ == '__main__':` läuft nur, wenn du die Datei selbst startest, nicht, wenn ein anderes Programm sie lädt; das brauchst du gleich.

_task(nea-zustaende)

```bash
python3 lock_nfa.py
```

Nach jeder Ziffer steht die Menge der Zustände, in denen der NEA sein könnte. `121213` öffnet, ohne dass du über einen einzigen Pfeil zurück nachdenken musstest.

## Vom NEA zum DEA

Ein NEA ist leicht zu schreiben. Ein DEA ist leichter auszuführen: ein Nachschlagen pro Zeichen, kein Hantieren mit Mengen, und genau so bauen ihn Programme in Hardware oder in schnelle Suchfunktionen ein. Zum Glück muss man sich nicht entscheiden. Die Mengen, die `opens` ausgibt, sind selbst die Zustände eines DEA. Ein Programm kann sie alle aufzählen, ausgehend von `{start}`: Für jede Menge und jede Ziffer rechnet es aus, welche Menge danach kommt, und macht weiter, bis keine neue mehr auftaucht. Das heißt **Potenzmengenkonstruktion**. Lege `convert.py` an:

```python_copy
# Die Potenzmengenkonstruktion: Sie baut aus einem NEA einen DEA. Jeder
# Zustand des DEA ist eine Menge von Zuständen des NEA, nämlich die, in denen
# der NEA gerade sein könnte.
from lock_nfa import DIGITS, START, ACCEPTING, TRANSITIONS


# Alle Zustände, in die der NEA aus states mit symbol kommt.
def step(states, symbol):
    new_states = set()
    for state in states:
        new_states |= TRANSITIONS.get((state, symbol), set())
    return frozenset(new_states)


def to_dfa():
    start = frozenset({START})
    found = {start}
    todo = [start]
    transitions = {}
    while todo:
        states = todo.pop()
        for symbol in DIGITS:
            target = step(states, symbol)
            transitions[(states, symbol)] = target
            if target not in found:
                found.add(target)
                todo.append(target)
    return found, transitions


found, transitions = to_dfa()
print(len(found), 'Zustände')
for (states, symbol), target in transitions.items():
    if target != {START}:
        print(sorted(states), symbol, '→', sorted(target))
```

`frozenset` ist eine Menge, die sich nicht mehr ändern lässt. Nur solche Mengen dürfen in Python Schlüssel eines `dict` sein oder in einer anderen Menge liegen. `todo` ist die Liste der Mengen, deren Übergänge noch nicht ausgerechnet sind. Am Ende gibt das Programm aus, wie viele Zustände der DEA hat, und alle Übergänge, die nicht zurück nach `{start}` führen:

<!-- tutorial-screenshot
# convert.py: der DEA für das Schloss
write-file: automata/lock_nfa.py <- file:stufen/lock_nfa.py
write-file: automata/convert.py <- file:stufen/convert.py
terminal-open
terminal-maximize
terminal-run: clear
terminal-run: python3 convert.py
terminal-wait-for-prompt
crop-terminal-lines: auto
-->
<img class='full' src='umwandeln.webp' alt='python3 convert.py gibt aus: 5 Zustände, dann acht Übergänge, etwa von der Menge 1, 121, start mit 2 zur Menge 12, start und mit 3 zur Menge 1213, start.'>

Fünf Zustände, wie bei Automatron. Und sieh dir die Übergänge an: `['1', '121', 'start'] 2 → ['12', 'start']` ist genau der Pfeil, den du in `lock.py` von Hand gesucht hast, von `121` mit einer 2 nach `12`. Das Programm hat ihn gefunden, ohne nachzudenken.

### Ein Generalschlüssel

Jetzt zeigt sich, wie viel leichter ein NEA zu ändern ist.

_task(generalschluessel)

Starte danach `convert.py` noch einmal. Es baut den DEA aus deinem neuen NEA, ohne dass du daran etwas ändern musst. Den DEA mit Generalschlüssel von Hand zu bauen, mit allen Pfeilen zurück, wäre eine knifflige Aufgabe; Automatron kommt mit `(0|1|2|3|4|5|6|7|8|9)*(1213|4711)` auf dasselbe Ergebnis.

_task(dea-generalschluessel)

### Wenn der DEA explodiert

Beim Schloss hat der DEA so viele Zustände wie der NEA. Das ist nicht immer so. Ein NEA, der prüft, ob die N-te Ziffer von hinten eine 1 ist, rät, wo diese 1 steht, und zählt dann bis zum Ende ab. Er braucht N + 1 Zustände. Lege `last.py` an:

```python
# Ist die N-te Ziffer von hinten eine 1? Der NEA rät, wo diese 1 steht, und
# zählt danach die Ziffern bis zum Ende ab.
N = 3
DIGITS = '01'
START = 0
ACCEPTING = {N}
TRANSITIONS = {
    (0, '0'): {0},
    (0, '1'): {0, 1},
}
for i in range(1, N):
    TRANSITIONS[(i, '0')] = {i + 1}
    TRANSITIONS[(i, '1')] = {i + 1}
```

Ändere in `convert.py` die Zeile `from lock_nfa import …` zu `from last import …` und starte es. Ändere dann `N` in `last.py` auf 5 und starte `convert.py` noch einmal.

_task(explosion)

Der DEA kann nicht raten, wo die 1 steht. Er muss sich darum alle letzten N Ziffern merken, und jede Möglichkeit ist ein eigener Zustand. Bei N = 30 wären das über eine Milliarde. Automatron zeigt unter den Beispielen den DEA für »drittletztes Symbol ist 1«; zähl seine Zustände.

## Zusammenfassung

- Ein **endlicher Automat** hat endlich viele Zustände und eine Tabelle, die für jeden Zustand und jedes Zeichen den nächsten nennt. In Python ist das ein `dict` mit Paaren als Schlüsseln.
- Ein Automat, der Wörter prüft, hat einen **Startzustand** und **akzeptierende Zustände**. Er nimmt ein Wort an, wenn er am Ende in einem akzeptierenden Zustand ist.
- Ein **DEA** weiß bei jedem Zeichen genau, wohin. Ein **NEA** darf mehrere Wege zugleich gehen; er nimmt ein Wort an, wenn einer davon akzeptiert. Verfolgt wird er mit einer Menge von Zuständen.
- Die **Potenzmengenkonstruktion** macht aus jedem NEA einen DEA, der dasselbe tut. Er kann aber exponentiell mehr Zustände haben.
- Was keine feste Obergrenze hat, etwa gleich viele a wie b, kann kein endlicher Automat prüfen.

_task(nea-oder-dea)

## Wie geht es weiter?

- **Reguläre Ausdrücke in Python.** `import re` und `re.fullmatch('[0-9]*1213', '121213')` prüfen dasselbe wie dein Schloss. Hinter `re` steckt die Idee dieser Seite: Ausdruck, Automat, Zeichen für Zeichen lesen.
- **Automatron ganz.** Automatron macht mehr als die Potenzmengenkonstruktion: Erst baut es aus dem Ausdruck einen NEA mit ε-Übergängen (Thompson-Konstruktion), am Ende verkleinert es den DEA (Minimierung). Probier die Beispiele aus und lies die Erklärungen zwischen den Bildern.
- **Mehr als endlich.** Klammern prüfen, ob jede `(` ein `)` hat, kann ein endlicher Automat nicht. Ein **Kellerautomat** kann es, mit einem Stapel als Gedächtnis. So prüfen Compiler deinen Code.

<!-- tutorial-screenshot
# Teaser: Automatron, der DEA für »die viertletzte Ziffer ist eine 1«
open-site: /automatron
tab: preview
viewport: 1029x700
click: selector:#ti_input
type: (0|1)*1(0|1)(0|1)(0|1)
press: Enter
wait-for-text: kleinster DEA
sleep: 1
crop-element: #output_area svg:nth-of-type(3)
-->
<img class='teaser' hidden src='teaser.webp' alt='Ein Gewirr aus 16 Zuständen s0 bis s15 und Pfeilen mit 0 und 1, gezeichnet von Automatron: der kleinste DEA, der prüft, ob die viertletzte Ziffer einer Binärzahl eine 1 ist. Acht Zustände haben einen doppelten Rand.'>
