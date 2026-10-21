<div class='meta'>
image: teaser.webp
image_alt: "Ein Mann isst an der Strandpromenade Pommes, Möwen fliegen um ihn herum"
kind: projekt
language: Ruby / Python
languages: [ruby, python]
duration: 3 Doppelstunden
requires: [crypto]
reference: [farbraeume]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Geheime Botschaften in Bildern

<p class='abstract'>
Verschlüsselung macht eine Nachricht unlesbar, aber jeder sieht, dass es sie gibt. Steganographie versteckt die Nachricht selbst: in einem Foto, das aussieht wie jedes andere. Du liest ein Bild als Liste von Zahlen, versteckst Text in den untersten Bits, findest heraus, wie man so etwas entdeckt, und baust zum Schluss ein Verfahren, das mit Farbklassen und einer Zahl mit wechselnder Basis genau so viel versteckt, wie du willst, nicht nur ganze Bits.
</p>

## Verstecken statt verschlüsseln

In [Kryptographie im Terminal](/crypto) hast du Nachrichten verschlüsselt. Wer die verschlüsselte Datei abfängt, kann sie nicht lesen, aber er weiß, dass da etwas Geheimes ist. Manchmal ist schon das zu viel.

**Steganographie** (griechisch für »verdecktes Schreiben«) versteckt, *dass* es eine Nachricht gibt. Früher war das unsichtbare Tinte oder ein winziger Mikropunkt auf einem Brief. Heute ist es oft ein Bild: Ein Foto hat Hunderttausende Pixel, und an jedem kann man ein wenig ändern, ohne dass es jemand sieht. Die Kunst ist, genau so wenig zu ändern, dass niemand Verdacht schöpft, weder ein Mensch noch ein Programm, das danach sucht.

Beides lässt sich kombinieren: erst verschlüsseln, dann verstecken. Dann muss jemand die Nachricht erst finden und kann sie danach noch immer nicht lesen.

## Einrichten

Lege einen Ordner an und öffne ihn mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>:

```bash
mkdir /workspace/stego
```

Hol dir die Bilder dieser Seite: zwei Fotos und ein Fundstück, in dem schon etwas versteckt ist.

```bash_copy
wget ${web_root}/steganography/fundstueck.png
wget ${web_root}/steganography/strandhaeuser.png
wget ${web_root}/steganography/moewen.png
```

<p class='expl'>Die Fotos der Strandhäuser und der Möwen mit freundlicher Genehmigung von Ian Kobylanski · <a href='https://koby.photography/' target='_blank'>https://koby.photography/</a></p>

PNG-Dateien sind komprimiert, aber ohne Verlust: Jedes Pixel kommt genau so wieder heraus, wie es hineingesteckt wurde. Das ist wichtig, denn eine versteckte Botschaft lebt in den kleinsten Unterschieden. Ein JPEG würde sie beim Speichern zerstören. Das Lesen und Schreiben übernimmt eine Bibliothek:

<!-- language: ruby -->
```bash
gem install chunky_png
```

**ChunkyPNG** liest und schreibt PNG-Dateien in Ruby. `ChunkyPNG::Image.from_file('fundstueck.png')` liest ein Bild, `image.save('neu.png')` schreibt es.
<!-- /language -->
<!-- language: python -->
```bash
pip install pillow
```

**Pillow** liest und schreibt Bilder in Python. `Image.open('fundstueck.png')` liest ein Bild, `image.save('neu.png')` schreibt es.
<!-- /language -->

## Ein Bild ist eine Liste von Zahlen

Jedes Pixel hat drei Kanäle, Rot, Grün und Blau, jeder eine Zahl von 0 bis 255. Ein Bild mit 480 × 320 Pixeln ist also nichts anderes als 480 · 320 · 3 = 460.800 Zahlen. Das Gerüst unten holt sie in eine einzige Liste, Zeile für Zeile, und macht aus so einer Liste wieder ein Bild.

<!-- language: ruby -->
Lege die Datei `stego.rb` an:

```ruby_copy
require 'chunky_png'

# Rot, Grün und Blau jedes Pixels, Zeile für Zeile, in einem Array:
# [r0, g0, b0, r1, g1, b1, ...]
def channels(image)
    image.pixels.flat_map { |c| [ChunkyPNG::Color.r(c), ChunkyPNG::Color.g(c), ChunkyPNG::Color.b(c)] }
end

# Das Bild zu einem solchen Array
def from_channels(width, height, values)
    ChunkyPNG::Image.new(width, height, values.each_slice(3).map { |r, g, b| ChunkyPNG::Color.rgb(r, g, b) })
end

# Ein Bild aus einem einzigen Bit jedes Kanals: 255, wo es gesetzt ist, sonst 0
def bit_plane(image, bit = 0)
end

image = ChunkyPNG::Image.from_file('fundstueck.png')
p [image.width, image.height]
p channels(image)[0, 6]
```

Starte es mit `ruby stego.rb`.

<!-- tutorial-screenshot
# Ordner stego mit dem Gerüst und den Bildern, ChunkyPNG installiert
write-file: stego/stego.rb <- file:stufen/start.rb
write-file: stego/fundstueck.png <- file:beispiele/fundstueck.png
write-file: stego/strandhaeuser.png <- file:beispiele/strandhaeuser.png
write-file: stego/moewen.png <- file:beispiele/moewen.png
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: stego
press: Enter
wait-for-input-value: /workspace/stego/
press: Enter
wait-for-text: stego.rb
open-file: stego.rb
terminal-open
terminal-run: gem install chunky_png
terminal-wait-for-prompt: 120
terminal-run: clear
terminal-run: ruby stego.rb
terminal-wait-for-prompt
terminal-expect-no-text: Error
move-mouse: selector:.terminal
sleep: 1
-->
<img class='full' src='start.webp' alt='Der Ordner stego mit den drei Bildern und stego.rb; im Terminal gibt ruby stego.rb die Größe 480 und 320 aus und die ersten sechs Zahlen des Bildes.'>
<!-- /language -->
<!-- language: python -->
Lege die Datei `stego.py` an:

```python_copy
from PIL import Image


# Rot, Grün und Blau jedes Pixels, Zeile für Zeile, in einer Liste:
# [r0, g0, b0, r1, g1, b1, ...]
def channels(image):
    return list(image.convert('RGB').tobytes())


# Das Bild zu einer solchen Liste
def from_channels(width, height, values):
    return Image.frombytes('RGB', (width, height), bytes(values))


# Ein Bild aus einem einzigen Bit jedes Kanals: 255, wo es gesetzt ist, sonst 0
def bit_plane(image, bit=0):
    pass


image = Image.open('fundstueck.png')
print(image.size)
print(channels(image)[:6])
```

Starte es mit `python3 stego.py`.

<!-- tutorial-screenshot
# Das Gerüst in Python, Pillow installiert
write-file: stego/stego.py <- file:stufen/start.py
close-tab: stego.rb
terminal-open
terminal-run: rm stego.rb
terminal-run: clear
terminal-run: pip install pillow
terminal-wait-for-prompt: 120
terminal-run: clear
terminal-run: python3 stego.py
terminal-wait-for-prompt
terminal-expect-no-text: Error
open-file: stego.py
move-mouse: selector:.terminal
sleep: 1
-->
<img class='full' src='start-python.webp' alt='Der Ordner stego mit den drei Bildern und stego.py; im Terminal gibt python3 stego.py die Größe (480, 320) aus und die ersten sechs Zahlen des Bildes.'>
<!-- /language -->

Die ersten sechs Zahlen sind die ersten beiden Pixel oben links, ein helles Himmelblau: `68, 168, 224` und noch einmal dasselbe.

### Das unterste Bit

Jede dieser Zahlen ist im Computer acht Bits, eine Zahl im Zweiersystem. 208 ist `11010000`: 128 + 64 + 16. Das Bit ganz rechts, das **unterste Bit**, zählt nur 1. Ändert man es, wird aus 208 die 209, ein Blau, das kein Mensch von 208 unterscheiden kann. Das oberste Bit dagegen zählt 128: Aus 208 würde 80, ein ganz anderes Blau.

<!-- language: ruby -->
In Ruby liefert `v[0]` das unterste Bit einer Zahl `v`, `v[1]` das nächste und so weiter: `208[0]` ist 0, `209[0]` ist 1.
<!-- /language -->
<!-- language: python -->
In Python schiebt `v >> bit` die Zahl um `bit` Stellen nach rechts, und `& 1` behält nur das unterste Bit: `(208 >> 0) & 1` ist 0, `(209 >> 0) & 1` ist 1, `(208 >> 4) & 1` ist 1.
<!-- /language -->

Schreib `bit_plane(image, bit)`: Es macht aus jedem Kanal 255, wenn sein Bit Nummer `bit` gesetzt ist, und sonst 0, und gibt das als neues Bild zurück, mit `from_channels`. Man nennt so ein Bild eine **Bitebene**. Die Bitebene 7 zeigt grob das Bild selbst; je tiefer das Bit, desto mehr sieht es nach Rauschen aus.

_task(bitebene)

Sieh dir die Bitebenen 0 und 1 des Fundstücks an. Speichere sie dafür als Bilder und öffne sie in Visual Studio Code:

<!-- language: ruby -->
```ruby
image = ChunkyPNG::Image.from_file('fundstueck.png')
bit_plane(image, 0).save('ebene-0.png')
bit_plane(image, 1).save('ebene-1.png')
```
<!-- /language -->
<!-- language: python -->
```python
image = Image.open('fundstueck.png')
bit_plane(image, 0).save('ebene-0.png')
bit_plane(image, 1).save('ebene-1.png')
```
<!-- /language -->

<!-- tutorial-step
# bit_plane aus der Musterlösung: Die Bitebene 1 des Fundstücks zeigt den Hinweis
write-file: stego/stego.py <- file:loesung/stego.py
write-file: stego/stego.rb <- file:loesung/stego.rb
check-task: bitebene
check-task: bitebene python
terminal-open
terminal-run: clear
terminal-run: PYTHONDONTWRITEBYTECODE=1 python3 -c "from stego import *; bit_plane(Image.open('fundstueck.png'), 1).save('ebene-1.png')"
terminal-wait-for-prompt
terminal-expect-no-text: Error
-->

<!-- tutorial-screenshot
# Die Bitebene 1 des Fundstücks, in VS Code geöffnet (über die Schnellsuche:
# open-file wartet auf einen Texteditor, ein Bild öffnet die Bildvorschau)
press: Control+P
type: ebene-1.png
sleep: 1
press: Enter
hide-bottom-panel
sleep: 3
-->
<img class='full' src='ebene-1.webp' alt='Die Datei ebene-1.png in Visual Studio Code: auf Schwarz in weißer Schrift »Hier nicht. Schau ins unterste Bit: reveal(bild)«.'>

In Bitebene 1 steht ein Hinweis, mitten in einem Foto, das ganz normal aussieht. Bitebene 0 sieht dagegen nach Rauschen aus, wie bei jedem Foto, nur dass in ihr diesmal etwas steckt.

<!-- language: ruby -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Speichere alle acht Bitebenen eines Fotos ohne Botschaft: <code>image = ChunkyPNG::Image.from_file('strandhaeuser.png')</code> und <code>(0..7).each { |bit| bit_plane(image, bit).save("haus-#{bit}.png") }</code>. Ab welcher Ebene erkennst du die Häuser nicht mehr? Wo hält sich ein Muster am längsten, im glatten Himmel oder an den Türen?
</div>
<!-- /language -->
<!-- language: python -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Speichere alle acht Bitebenen eines Fotos ohne Botschaft: <code>image = Image.open('strandhaeuser.png')</code> und <code>for bit in range(8): bit_plane(image, bit).save(f'haus-{bit}.png')</code>. Ab welcher Ebene erkennst du die Häuser nicht mehr? Wo hält sich ein Muster am längsten, im glatten Himmel oder an den Türen?
</div>
<!-- /language -->

Hier geht es mit einem anderen Foto weiter, und mit mehr als einem Bit: Zieh die beiden Griffe, und nur die Bits zwischen ihnen bleiben, alle anderen werden 0. Liegen beide auf demselben Bit, ist das eine Bitebene; »verstärken« streckt, was übrig bleibt, auf 0 bis 255, sonst wären die unteren Bits fast schwarz. Wie viele Bits von oben brauchst du, bis der Himmel glatt aussieht? Oben kannst du dasselbe Foto mit einer Botschaft wählen, als Schrift im untersten Bit: Sieht das Foto anders aus? Und die Bitebene 0?

<div class='bitplane-widget' data-x='300' data-y='30' data-credit='Foto: Uwe Jelting, <a href="https://commons.wikimedia.org/wiki/File:Strandwagen_Auf_Borkum_(183887415).jpeg">Strandwagen auf Borkum</a>, CC0'>
<img data-noconvert src='strandwagen.png' data-label='ohne Botschaft' alt='Ein roter Strandwagen auf hellem Sand unter blauem Himmel, mit den gewählten Bits jedes Kanals.'>
<img data-noconvert src='strandwagen-bild.png' data-label='mit einer Botschaft im untersten Bit' alt='Derselbe Strandwagen, im untersten Bit jedes Kanals eine Schrift, mit den gewählten Bits jedes Kanals.'>
</div>
<script src='/include/bitplane_widget.js'></script>

## Eine Botschaft im untersten Bit

Das Verfahren heißt **Least Significant Bit**, kurz LSB: Jedes unterste Bit jedes Kanals wird durch ein Bit der Botschaft ersetzt. Damit ein Programm die Botschaft wieder herausholen kann, müssen beide Seiten dieselben Regeln benutzen. Hier sind sie:

1. Die Botschaft ist Text, als Bytes in UTF-8. Vor diese Bytes kommt ihre **Länge** als Zahl mit 32 Bit, also 4 Bytes. So weiß, wer liest, wie viele Bytes kommen.
2. Jedes Byte wird zu 8 Bits, das höchste Bit zuerst: Aus »A« (65) wird `01000001`.
3. Die Bits kommen der Reihe nach in die untersten Bits der Kanäle: das erste in Rot des ersten Pixels, das zweite in Grün, das dritte in Blau, dann das zweite Pixel und so weiter, Zeile für Zeile, genau in der Reihenfolge von `channels`.

Schreib `hide(image, message)`: Es gibt ein neues Bild zurück, in dem die Botschaft steckt, und meldet einen Fehler, wenn sie nicht hineinpasst. Und schreib `reveal(image)`: Es liest zuerst 32 Bits und daraus die Länge, dann so viele Bytes, und macht wieder Text daraus.

<!-- language: ruby -->
- `message.b.bytes` sind die Bytes eines Textes, `[länge].pack('N').bytes` die vier Bytes einer Zahl mit 32 Bit.
- `(v & ~1) | bit` setzt das unterste Bit von `v` auf `bit` und lässt die übrigen, wie sie sind.
- Am Ende macht `bytes.pack('C*').force_encoding('UTF-8')` aus einem Array von Bytes wieder Text, auch mit Umlauten.
<!-- /language -->
<!-- language: python -->
- `message.encode()` sind die Bytes eines Textes, `len(data).to_bytes(4, 'big')` die vier Bytes einer Zahl mit 32 Bit.
- `(v & ~1) | bit` setzt das unterste Bit von `v` auf `bit` und lässt die übrigen, wie sie sind.
- Am Ende macht `bytes(liste).decode()` aus einer Liste von Bytes wieder Text, auch mit Umlauten.
<!-- /language -->

_task(verstecken)

Jetzt kannst du lesen, was im Fundstück steckt:

<!-- language: ruby -->
```ruby
puts reveal(ChunkyPNG::Image.from_file('fundstueck.png'))
```
<!-- /language -->
<!-- language: python -->
```python
print(reveal(Image.open('fundstueck.png')))
```
<!-- /language -->

<!-- tutorial-step
# Das Fundstück verrät sein Codewort, in beiden Sprachen
check-task: verstecken
check-task: verstecken python
terminal-open
terminal-run: clear
terminal-run: PYTHONDONTWRITEBYTECODE=1 python3 -c "from stego import *; print(reveal(Image.open('fundstueck.png')))"
# Die Zeile bricht im Terminal um, mitten im Satz
wait-for-text: STRANDKORB.
terminal-run: ruby -r ./stego -e "puts reveal(ChunkyPNG::Image.from_file('fundstueck.png'))"
terminal-wait-for-prompt
terminal-expect-no-text: Error
-->

_task(codewort)

Versteck eine eigene Botschaft in `strandhaeuser.png`, speichere das Ergebnis als `geheim.png` und öffne beide nebeneinander. Findest du einen Unterschied?

<div class='hint try'>
<strong>Ausprobieren:</strong> Schick <code>geheim.png</code> einmal durch JPEG und zurück, mit ImageMagick: <code>convert geheim.png -quality 95 geheim.jpg</code> und <code>convert geheim.jpg zurueck.png</code>. Was liefert <code>reveal</code> für <code>zurueck.png</code>? Probier es auch mit <code>-quality 100</code>. Warum hilft selbst die beste Qualität nicht?
</div>

## Wie viel passt hinein?

Ein Bild mit 480 × 320 Pixeln hat 460.800 Kanäle und damit 460.800 unterste Bits: 57.600 Bytes, davon 4 für die Länge. Das ist eine ganze Menge Text, etwa 30 Seiten eines Buchs.

Die Spezifikation des GIF-Formats ist größer, 83.003 Bytes. Hol sie dir und versuch, sie in `strandhaeuser.png` zu verstecken:

```bash
wget ${web_root}/lzw/spec-gif89a.txt
```

<!-- language: ruby -->
`File.binread('spec-gif89a.txt')` liest die Datei als Bytes. `hide` sollte sich jetzt mit einer Fehlermeldung weigern.
<!-- /language -->
<!-- language: python -->
`open('spec-gif89a.txt', 'rb').read()` liest die Datei als Bytes. Damit `hide` auch Bytes nimmt, nicht nur Text, mach aus `message.encode()` ein `message.encode() if isinstance(message, str) else message`. `hide` sollte sich jetzt mit einer Fehlermeldung weigern.
<!-- /language -->

_task(kapazitaet)

Mehr passt hinein, wenn man mehr als ein Bit pro Kanal ersetzt. Gib `hide` und `reveal` einen weiteren Parameter `depth`, wie viele der untersten Bits jeder Kanal trägt, zum Beispiel `hide(image, message, 2)`. Mit 2 Bits passt die Spezifikation hinein. Nur: Mit jedem Bit mehr wird der Unterschied größer. Ein Kanal kann sich mit 1 Bit um höchstens 1 ändern, mit 2 Bits um 3, mit 4 Bits um 15.

_task(sichtbar)

So sieht der Himmel aus `moewen.png` aus, wenn alle untersten Bits voller Daten sind:

<div class='gallery columns-4'>
<figure><img src='depth-1.webp' alt='Ein glatter blauer Himmel mit einer Möwe, wie im Original.'><figcaption>1 Bit pro Kanal: wie das Original</figcaption></figure>
<figure><img src='depth-2.webp' alt='Derselbe Himmel, noch immer glatt.'><figcaption>2 Bits: noch nichts zu sehen</figcaption></figure>
<figure><img src='depth-4.webp' alt='Derselbe Himmel, körnig, mit waagerechten Streifen im Verlauf.'><figcaption>4 Bits: körnig und gestreift</figcaption></figure>
<figure><img src='depth-6.webp' alt='Derselbe Himmel, grob gesprenkelt in Blau, Grün und Violett.'><figcaption>6 Bits: grob gesprenkelt</figcaption></figure>
</div>

Mit 1 und 2 Bits sieht man nichts. Mit 4 Bits sieht man Rauschen und Streifen: Im glatten Verlauf des Himmels behält jeder Kanal nur noch seine oberen 4 Bits, also 16 Stufen statt 256, und der Verlauf zerfällt in **Bänder**. Gerade glatte Flächen verraten zu viele versteckte Daten; in Laub, Wellen oder Stoff fällt dasselbe Rauschen kaum auf.

## Entdecken

Wer nach versteckten Botschaften sucht, hat das Bild ohne Botschaft nicht zum Vergleich. Trotzdem gibt es Spuren.

### Was die Bitebene zeigt

Hier sind die Strandhäuser und daneben ihre Bitebene 0: so, wie sie sind, mit der GIF-Spezifikation als Text im untersten Bit (so viel davon hineinpasst), und mit derselben Spezifikation, aber vorher **komprimiert**:

<div class='gallery columns-4'>
<figure><img src='plane-photo.webp' alt='Eine Ecke der Strandhäuser: Himmel, zwei blasse Giebel, unten eine rote und eine hellblaue Tür.'><figcaption>Das Foto</figcaption></figure>
<figure><img src='plane-clean.webp' alt='Die Bitebene 0 des Fotos: Rauschen, oben im Himmel mit großen Flecken.'><figcaption>Bitebene 0, ohne Botschaft</figcaption></figure>
<figure><img src='plane-text.webp' alt='Die Bitebene 0 mit der Spezifikation als Text: regelmäßige senkrechte Streifen.'><figcaption>mit der Spezifikation als Text: Streifen</figcaption></figure>
<figure><img src='plane-packed.webp' alt='Die Bitebene 0 mit der komprimierten Spezifikation: gleichmäßiges Rauschen.'><figcaption>komprimiert: gleichmäßiges Rauschen</figcaption></figure>
</div>

Text verrät sich: In UTF-8 ist bei den meisten Zeichen das oberste Bit jedes Bytes 0, und Buchstaben, Leerzeichen und Zeilenenden wiederholen sich. Jedes achte Bit ist fast immer gleich, und das ergibt die Streifen. Komprimierte Daten dagegen sehen aus wie Zufall: Ein guter Kompressor nimmt jede Wiederholung heraus, und übrig bleiben Bits, die so oft 0 wie 1 sind. Die Bitebene sieht dann aus wie bei einem gewöhnlichen Foto. Verschlüsselte Daten sehen genauso aus.

Komprimiere eine Botschaft vor dem Verstecken:

<!-- language: ruby -->
```ruby
require 'zlib'
packed = Zlib::Deflate.deflate(File.binread('spec-gif89a.txt'), 9)
```

Aus 83.003 Bytes werden gut 20.000. Zurück geht es mit `Zlib::Inflate.inflate`. Damit `reveal` dafür Bytes liefert statt Text, gib ihm `raw: true`.
<!-- /language -->
<!-- language: python -->
```python
import zlib
packed = zlib.compress(open('spec-gif89a.txt', 'rb').read(), 9)
```

Aus 83.003 Bytes werden gut 20.000. Zurück geht es mit `zlib.decompress`. Damit `reveal` dafür Bytes liefert statt Text, gib ihm `raw=True`.
<!-- /language -->

### Der Chi-Quadrat-Test

Ist die Botschaft damit unauffindbar? Nein, und gerade weil ihre Bits so gleichmäßig sind. Andreas Westfeld und Andreas Pfitzmann haben 1999 gezeigt, wie man das merkt.

Ersetzt man das unterste Bit, wird aus einer 208 entweder 208 oder 209, aus einer 209 ebenso. Die Werte 208 und 209 bilden ein **Paar**, und dasselbe gilt für 0 und 1, 2 und 3, bis 254 und 255. Sind die versteckten Bits zufällig, landet jeder Wert eines Paares mit gleicher Wahrscheinlichkeit auf dem einen oder dem anderen. Nach dem Verstecken kommen deshalb beide Werte eines Paares **gleich oft** vor. In einem gewöhnlichen Foto ist das nicht so: Dort ist 208 vielleicht 300-mal da und 209 nur 250-mal.

Der **Chi-Quadrat-Test** misst, wie weit die Paare von »gleich oft« entfernt sind. Zuerst zählst du in den Zeilen, die du untersuchst, wie oft jeder Wert von 0 bis 255 vorkommt, in allen drei Kanälen zusammen. Für jedes Paar heißen seine beiden Anzahlen \\( a \\) und \\( b \\): Beim Paar 208 und 209 ist \\( a \\), wie oft 208 vorkommt, und \\( b \\), wie oft 209 vorkommt.

Wären beide Werte gleich oft da, bekäme jeder die Hälfte von zusammen \\( a + b \\). Diese **erwartete Anzahl** heißt \\( e \\):

$$e = \frac{a+b}{2}$$

Mit 300-mal 208 und 250-mal 209 ist \\( e = \frac{300+250}{2} = 275 \\). So oft käme jeder der beiden Werte vor, wenn sie gleich häufig wären. Tatsächlich liegt 208 um 25 darüber und 209 um 25 darunter. Der Test quadriert diese Abweichungen und teilt sie durch \\( e \\), denn 25 zu viel ist bei 275 viel, bei 27.500 kaum der Rede wert:

$$\frac{(a-e)^2}{e} + \frac{(b-e)^2}{e} = \frac{(300-275)^2}{275} + \frac{(250-275)^2}{275} = \frac{625 + 625}{275} \approx 4{,}5$$

Das rechnest du für jedes Paar, von 0 und 1 bis 254 und 255, und addierst alles:

$$\chi^2 = \sum_{\text{Paare}} \left( \frac{(a-e)^2}{e} + \frac{(b-e)^2}{e} \right)$$

Zum Schluss teilst du durch die Zahl der Paare. Paare mit \\( e < 5 \\) lässt du weg, sie sind zu selten, um etwas zu sagen. Auch zufällige Bits treffen \\( e \\) nicht genau: Wer 550-mal eine Münze wirft, bekommt selten genau 275-mal Kopf. Im Mittel ergibt der Zufall etwa 1 pro Paar. Kommt also etwa 1 heraus, sind die Paare genau so gleich, wie es zufällige unterste Bits machen: verdächtig. Ein Foto ohne Botschaft liegt weit darüber.

Schreib `chi_square(image, top, bottom)`: Es rechnet den Test für die Zeilen `top` bis `bottom` des Bildes. Mit Pillow schneidet `image.crop((0, top, image.width, bottom))` diese Zeilen aus, mit ChunkyPNG `image.crop(0, top, image.width, bottom - top)`.

_task(chi)

Rechne den Test für die Strandhäuser ohne Botschaft und mit der komprimierten Spezifikation, jeweils für die Zeilen 0 bis 40, 40 bis 80, 80 bis 120 und 120 bis 160. Die Botschaft füllt nur die obersten gut 110 Zeilen.

_task(komprimiert)

Der Test findet die Botschaft und sogar, wo sie aufhört: Dort, wo sie steckt, liegt das Ergebnis nahe bei 1, darunter ist es so groß wie ohne Botschaft. Am deutlichsten ist es im Himmel, wo das Foto ohne Botschaft weit über 100 ergibt. Rechne zum Vergleich auch die Zeilen 200 bis 240, die Türen mit ihrer feinen Maserung: Dort ist der Test schon ohne Botschaft klein, denn das Rauschen des Fotos ist dort selbst fast so gleichmäßig wie eine Botschaft. Eine Botschaft in solchen Flächen fällt viel weniger auf. Was hilft noch?

- **Weniger verstecken** und die Bits über das ganze Bild verteilen, an Stellen, die ein Schlüssel auswählt. Dann ist jede Zeile nur ein wenig gleicher als sonst, und der Test wird unsicher.
- **±1 statt Ersetzen**: Passt das unterste Bit nicht, wird der Wert zufällig um 1 erhöht oder erniedrigt, statt das Bit zu setzen. Aus 208 kann dann auch 207 werden, und die Paare gleichen sich nicht mehr an. Andere Tests finden auch das, aber schwerer.
- **Gar nicht mehr Bits ersetzen**, sondern ein anderes Verfahren, das sich nach dem richtet, was das Auge sieht. Darum geht es im Rest dieser Seite.

## Farbklassen

Das LSB-Verfahren lässt sich auch anders beschreiben. Alle Farben, die sich nur im untersten Bit jedes Kanals unterscheiden, bilden eine **Farbklasse**: zum Beispiel die 8 Farben mit Rot 208 oder 209, Grün 148 oder 149 und Blau 36 oder 37. Innerhalb einer Klasse sieht keine Farbe anders aus als die anderen. Ein Pixel darf also jede Farbe seiner Klasse annehmen, und *welche* es annimmt, trägt die Botschaft: 8 Möglichkeiten sind 3 Bits.

Im RGB-Würfel mit 256 × 256 × 256 Farben sind diese Klassen kleine Würfel mit der Kantenlänge 2. Warum nicht 3? Dann hat eine Klasse 3 · 3 · 3 = 27 Farben, und jedes Pixel kann eine von 27 Möglichkeiten tragen. Das sind \\( \log\_2 27 \approx 4{,}75 \\) Bits, keine ganze Zahl. Ersetzen kann man da nichts mehr. Man braucht eine andere Idee.

### Eine Zahl mit wechselnder Basis

Du kennst diese Idee von der Uhr. 3725 Sekunden sind 1 Stunde, 2 Minuten und 5 Sekunden: 3725 durch 60 ist 62, Rest **5**; 62 durch 60 ist 1, Rest **2**; übrig bleibt **1**. Die Sekunden und Minuten haben die Basis 60, die Stunden 24. Jede Stelle hat ihre eigene Basis.

Genauso wird die Botschaft eine einzige große Zahl \\( P \\), und jedes Pixel eine Stelle. Seine Basis ist die Größe \\( s \\) seiner Klasse:

1. Die Ziffer für das nächste Pixel ist \\( P \bmod s \\), der Rest beim Teilen. Das Pixel bekommt die Farbe mit dieser Nummer in seiner Klasse.
2. Weiter geht es mit \\( P \,//\, s \\), dem Ergebnis der Division ohne Rest.
3. Das wiederholt sich, bis \\( P \\) null ist.

Zum Lesen geht man rückwärts, vom letzten benutzten Pixel bis zum ersten: \\( P \\) beginnt bei 0, und für jedes Pixel wird \\( P = P \cdot s + \text{Nummer seiner Farbe} \\). Das ist dasselbe wie »1 Stunde ist 60 Minuten, plus 2 Minuten, mal 60, plus 5 Sekunden«. Dafür muss der Leser wissen, wie viele Pixel benutzt wurden; diese Zahl steht vorher in den untersten Bits der ersten 11 Pixel, 33 Bits.

_task(stellen)

Ruby und Python rechnen mit beliebig großen ganzen Zahlen, ohne Bibliothek: Eine Botschaft mit 20.000 Bytes ist eine Zahl mit 160.000 Bits, und `%` und `//` (in Ruby `divmod`) funktionieren genauso wie mit kleinen Zahlen. Je größer die Zahl, desto länger dauert jede Division; für ein paar Sätze merkst du davon nichts.

<!-- language: ruby -->
Schreib eine Klasse `BoxClasses` mit `BoxClasses.new(width)` für Würfel mit `width` Werten pro Kanal:

- `cell(color)` gibt zu einer Farbe `0xRRGGBB` ihre Klasse, etwa `[r / width, g / width, b / width]`.
- `members(cell)` gibt alle Farben dieser Klasse als Array, aufsteigend sortiert. Am Rand des Würfels ist eine Klasse kleiner: 256 ist nicht durch 3 teilbar.

Dann `embed(image, message, classes)` und `extract(image, classes)` nach den Regeln oben, mit den Farben jedes Pixels als `0xRRGGBB` (`image.pixels.map { |c| c >> 8 }`). Die Botschaft wird zur Zahl mit `("\x01".b + message.b).unpack1('H*').to_i(16)`; die 1 davor sorgt dafür, dass Nullen am Anfang nicht verloren gehen. Und `capacity(image, classes)` zählt zusammen, wie viele Bits passen: für jedes Pixel ab dem zwölften `Math.log2` der Größe seiner Klasse.
<!-- /language -->
<!-- language: python -->
Schreib eine Klasse `BoxClasses` mit `BoxClasses(width)` für Würfel mit `width` Werten pro Kanal:

- `cell(color)` gibt zu einer Farbe `0xRRGGBB` ihre Klasse, etwa `(r // width, g // width, b // width)`.
- `members(cell)` gibt alle Farben dieser Klasse als Liste, aufsteigend sortiert. Am Rand des Würfels ist eine Klasse kleiner: 256 ist nicht durch 3 teilbar.

Dann `embed(image, message, classes)` und `extract(image, classes)` nach den Regeln oben, mit den Farben jedes Pixels als `0xRRGGBB`. Die Botschaft wird zur Zahl mit `int.from_bytes(b'\x01' + message.encode(), 'big')`; die 1 davor sorgt dafür, dass Nullen am Anfang nicht verloren gehen. Zurück geht es mit `number.to_bytes((number.bit_length() + 7) // 8, 'big')[1:]`. Und `capacity(image, classes)` zählt zusammen, wie viele Bits passen: für jedes Pixel ab dem zwölften `math.log2` der Größe seiner Klasse.
<!-- /language -->

Mit `BoxClasses(2)` ist das wieder das LSB-Verfahren, 3 Bits pro Pixel. Mit 3 sind es 4,75 Bits, mit 5 schon fast 7. So lässt sich die Menge, die man versteckt, fein einstellen, nicht nur in Schritten von ganzen Bits.

_task(klassen)

## Am Test vorbei

Mit Würfeln der Breite 2 ist das Verfahren wieder das LSB-Verfahren, und der Chi-Quadrat-Test findet es wie vorher. Mit der Breite 3 ändert sich etwas, das man nicht sofort sieht: Jetzt kann ein Kanal nicht mehr nur das unterste Bit wechseln, sondern einen von drei Werten annehmen. Was macht der Test daraus?

_task(vorbei)

<!-- language: ruby -->
```ruby
image = ChunkyPNG::Image.from_file('strandhaeuser.png')
hidden = embed(image, File.read('spec-gif89a.txt')[0, 20000], BoxClasses.new(3))
puts chi_square(hidden, 0, 40)
```
<!-- /language -->
<!-- language: python -->
```python
image = Image.open('strandhaeuser.png')
hidden = embed(image, open('spec-gif89a.txt').read()[:20000], BoxClasses(3))
print(chi_square(hidden, 0, 40))
```
<!-- /language -->

Die Botschaft füllt die obersten gut 70 Zeilen, und trotzdem schlägt der Test nicht an. Er vergleicht die Paare 2k und 2k+1, weil das LSB-Verfahren genau diese beiden Werte gleich häufig macht. Würfel der Breite 3 machen aber Gruppen von drei Werten gleich häufig: 0, 1 und 2, dann 3, 4 und 5. Die Paare des Tests liegen quer dazu, 2 und 3 etwa gehören zu verschiedenen Gruppen. So geht es mit jeder ungeraden Breite. Bei der Breite 4 dagegen liegt jedes Paar in einer Gruppe, und der Test findet die Botschaft wieder.

<div class='scroll-x'>
<table class='table value-groups'>
<tr><th>Werte</th><td>0</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td><td>10</td><td>11</td></tr>
<tr><th>Paare des Tests</th><td colspan='2' class='group-even'>0–1</td><td colspan='2' class='group-odd'>2–3</td><td colspan='2' class='group-even'>4–5</td><td colspan='2' class='group-odd'>6–7</td><td colspan='2' class='group-even'>8–9</td><td colspan='2' class='group-odd'>10–11</td></tr>
<tr><th>Würfel der Breite 3</th><td colspan='3' class='group-even'>0–2</td><td colspan='3' class='group-odd'>3–5</td><td colspan='3' class='group-even'>6–8</td><td colspan='3' class='group-odd'>9–11</td></tr>
<tr><th>Würfel der Breite 4</th><td colspan='4' class='group-even'>0–3</td><td colspan='4' class='group-odd'>4–7</td><td colspan='4' class='group-even'>8–11</td></tr>
</table>
</div>

<p class='expl'>Jede Farbe einer Gruppe kommt nach dem Verstecken gleich oft vor. Bei der Breite 4 liegen in jeder Gruppe zwei ganze Paare, also werden auch die Paare gleich. Bei der Breite 3 schneiden die Gruppen durch die Paare hindurch: 2 und 3, 8 und 9 liegen in verschiedenen Gruppen.</p>

Der Test muss also wissen, wonach er sucht. Er lässt sich leicht verallgemeinern: Statt der Paare nimmt er Gruppen von `width` Werten, die Werte \\( k \cdot w \\) bis \\( k \cdot w + w - 1 \\). Für jede Gruppe ist \\( e \\) der Durchschnitt ihrer Zählungen, und sie trägt \\( \sum (c - e)^2 / e \\) bei, über ihre \\( w \\) Zählungen \\( c \\). Geteilt durch \\( w - 1 \\) ergibt eine Gruppe, die nur der Zufall gleichmacht, wieder etwa 1. Mit \\( w = 2 \\) ist das genau der Test von vorhin.

_task(gruppen)

Für den versteckten Text mit der Breite 3 ergibt der Test mit `width` 3 jetzt etwa 1. Wer etwas verstecken will, darf sich nicht darauf verlassen, dass der andere sein Verfahren nicht kennt; ein paar Breiten auszuprobieren dauert nur Sekunden. In der Kryptographie heißt das **Prinzip von Kerckhoffs**: Ein Verfahren muss sicher bleiben, auch wenn der Gegner alles darüber weiß außer dem Schlüssel.

### Das zweite Fundstück

Hol dir noch ein Bild:

```bash
wget ${web_root}/steganography/fundstueck-2.png
```

Darin steckt eine Botschaft, versteckt mit Würfeln, aber welche Breite sie haben, steht nirgends. Probier mit deinem Test die Breiten 2 bis 8 für die Zeilen 0 bis 40 aus:

<!-- language: ruby -->
```ruby
image = ChunkyPNG::Image.from_file('fundstueck-2.png')
(2..8).each { |width| puts "#{width}: #{chi_square(image, 0, 40, width).round(1)}" }
```

Mit der richtigen Breite liest `extract` die Botschaft. Sie ist lang; gib nur den Anfang aus, etwa mit `[0, 200]`.
<!-- /language -->
<!-- language: python -->
```python
image = Image.open('fundstueck-2.png')
for width in range(2, 9):
    print(width, round(chi_square(image, 0, 40, width), 1))
```

Mit der richtigen Breite liest `extract` die Botschaft. Sie ist lang; gib nur den Anfang aus, etwa mit `[:200]`.
<!-- /language -->

_task(fundstueck-2)

_task(abgabe)

## Wie geht es weiter?

- **Mit Schlüssel:** Wähle die Pixel nicht der Reihe nach, sondern in einer Reihenfolge, die ein Zufallsgenerator mit einem geheimen Startwert festlegt. Wer den Schlüssel nicht kennt, weiß nicht einmal, welche Pixel er untersuchen soll.
- **Ohne feste Breite:** Der Test mit Gruppen findet Würfel jeder Breite, weil alle Pixel dieselben Klassen haben. Was, wenn ein Schlüssel für jedes Pixel eine andere Einteilung wählt?
- **Andere Farbräume:** Ein Würfel in RGB ist nicht die beste Farbklasse, denn das Auge sieht RGB nicht gleichmäßig: Unterschiede in der Helligkeit sieht es viel genauer als im Farbton. Im Handbuch unter [Farbräume](/reference/farbraeume) stehen Farbräume wie CIELab und OKLab, in denen gleiche Abstände ungefähr gleich verschieden aussehen. Schreib eine Klasseneinteilung mit Quadern in einem davon, mit mehr Stufen für die Helligkeit als für die Farbe; `embed` und `extract` brauchen von ihr nur `cell` und `members`.
- **Töne:** Eine WAV-Datei ist eine Liste von Zahlen wie ein Bild, nur in der Zeit. Das unterste Bit eines Tons hört man genauso wenig.
- **JPEG:** In einem JPEG gibt es keine Pixel, die man ändern könnte, nur die Koeffizienten der DCT aus [Ein JPEG-Decoder](/jpeg). Verfahren wie F5 verstecken ihre Bits dort.
