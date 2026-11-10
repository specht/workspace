<div class='meta'>
image: teaser.webp
image_alt: "Eine Welle, in gleichen Abständen gemessen; jede Messung ist ein Punkt auf einer von sieben Stufen, und unter jedem Punkt stehen die drei Bits, die gespeichert werden."
kind: labor
language: Python
duration: 5 Doppelstunden
curriculum: "Oberstufe: Algorithmen und Datenstrukturen (Fourier-Transformation, Huffman-Codierung), Vertiefung Technische Informatik (Abtastung, Quantisierung, Audiokompression)"
topics: [kompression, codierung, zahlensysteme]
requires: [huffman]
reference: [bits-und-bytes]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

<script src='/include/sampling_widget.js'></script>

# Audio komprimieren

<p class='abstract'>
Eine Minute Musik in CD-Qualität braucht 10 MB, als MP3 nur ein Zehntel davon, und man hört kaum einen Unterschied. Du schreibst WAV-Dateien Byte für Byte, zerlegst Klänge mit der Fourier-Transformation in ihre Frequenzen und baust in Python Schritt für Schritt einen Coder, der weglässt, was das Ohr nicht hört. Unterwegs liest du echte MP3-Dateien und hörst, was Opus und die Codecs von heute daraus gemacht haben.
</p>

## Ton als Zahlen

Was du hörst, ist Luft, die schwingt: Der Druck steigt und fällt, hunderte bis tausende Male in der Sekunde. Ein Mikrofon macht daraus eine Spannung, die genauso steigt und fällt. Ein Computer kann mit einer Spannung nichts anfangen, nur mit Zahlen. Darum misst ein Wandler sie in festen Abständen und schreibt jede Messung als ganze Zahl auf. Zwei Entscheidungen bestimmen, wie gut das klingt und wie viel Platz es braucht: **wie oft** gemessen wird und **wie genau**.

### Einrichten

Lege im Terminal einen Ordner an und hol die beiden Klänge dieser Seite:

```bash_copy
mkdir -p ~/audio && cd ~/audio
wget ${web_root}/audio/stimme.wav
wget ${web_root}/audio/musik.wav
```

<!-- tutorial-step
terminal-open
terminal-run: mkdir -p ~/audio && cd ~/audio
-->

Öffne sie im Explorer: VS Code spielt WAV-Dateien selbst ab. `stimme.wav` ist ein Satz, gesprochen von Thorsten, einer freien Stimme für Sprachausgabe; sie wurde mit 22.050 Messungen in der Sekunde aufgenommen. `musik.wav` sind acht Sekunden einer Band, die es nicht gibt: Schlagzeug, Bass, Akkorde und eine Melodie, ausgerechnet aus Sinusschwingungen und Rauschen, mit 44.100 Messungen in der Sekunde.

### Wie oft und wie genau

Wie oft gemessen wird, heißt **Abtastrate** und wird in Hertz angegeben: 44.100 Hz sind 44.100 Messungen in der Sekunde. Je seltener gemessen wird, desto mehr von den schnellen Schwingungen geht verloren. Eine Abtastrate hält Frequenzen bis knapp unter ihrer Hälfte fest; was schneller schwingt, muss vorher herausgefiltert werden. Das Ohr eines jungen Menschen hört bis etwa 20.000 Hz. Darum misst eine CD 44.100-mal in der Sekunde, gut doppelt so oft.

_person(shannon)

Wie genau gemessen wird, bestimmen die **Bits je Messung**. Mit 16 Bit hat jede Messung eine von 65.536 Stufen, von −32.768 bis 32.767. Jede Messung wird auf die nächste Stufe gerundet, das heißt **quantisiert**. Jedes Bit weniger halbiert die Zahl der Stufen und verdoppelt den Fehler beim Runden. Diesen Fehler hörst du als Rauschen.

Probier beides aus. Spiel einen Klang ab und schalte um, während er läuft:

<div class='sampling-widget'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell bei der Musik die Abtastrate von 44.100 schrittweise auf 4.000 Hz. Achte auf das Zischen der Hi-Hat: Bei welcher Rate ist es weg? Wie klingt die Musik bei 8.000 Hz, der Rate des Telefons? Geh dann zurück auf 44.100 Hz und nimm Bits weg. Ab wie vielen Bits hörst du ein Rauschen, und wo hörst du es zuerst, in lauten oder in leisen Stellen? Im Bild sitzt bei wenigen Bits jede Messung auf einer der Linien. Bei der Stimme ändert sich zwischen 44.100 und 22.050 Hz nichts: warum?
</div>

Die Messungen in gleichen Abständen als Zahlen zu senden, heißt **Pulscodemodulation**, kurz PCM. Alec Reeves, ein Ingenieur bei ITT in Paris, schlug sie 1937 für Telefongespräche vor und meldete sie 1938 zum Patent an. Bauen ließ sie sich erst Jahrzehnte später: 1961 schickte das Telefonnetz der USA die ersten Gespräche als PCM, 8.000 Messungen mit je 8 Bit in der Sekunde. Die **Compact Disc** von Philips und Sony kam am 1. Oktober 1982 in Japan in die Läden, mit zwei Kanälen für Stereo und je 44.100 Messungen mit 16 Bit in der Sekunde. Die krumme Zahl kommt vom Videorekorder: Studios speicherten Musik digital auf Videobändern, drei Messungen in jeder Zeile des Fernsehbilds. Beim europäischen Fernsehen sind das 294 Zeilen mal 50 Halbbilder mal 3 Messungen, genau 44.100 in der Sekunde.

_task(cd-lied)

### Eine WAV-Datei Byte für Byte

Eine WAV-Datei speichert genau das: wie oft gemessen wurde, wie genau, und dann die Messungen, eine Zahl nach der anderen. Das Format haben IBM und Microsoft 1991 festgelegt; die [Beschreibung des Formats an der McGill University](https://www.mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html) fasst die Spezifikation zusammen und verlinkt sie. Eine WAV-Datei ist eine RIFF-Datei: eine Folge von **Chunks**, jeder mit einem Namen aus vier Zeichen und seiner Länge. Zahlen stehen im **Little Endian** in der Datei, das niedrige Byte zuerst, wie bei [GIF](/gif).

Hier ist eine ganze WAV-Datei, 60 Bytes groß: acht Messungen eines Tons mit 1.000 Hz, 8.000-mal in der Sekunde gemessen, also eine Millisekunde lang.

<pre class='spec hexdump'>
00000000  <span class='m0'>52 49 46 46 34 00 00 00  57 41 56 45</span> <span class='m1'>66 6d 74 20</span>  |RIFF4...WAVEfmt |
00000010  <span class='m1'>10 00 00 00 01 00 01 00  40 1f 00 00 80 3e 00 00</span>  |........@....&gt;..|
00000020  <span class='m1'>02 00 10 00</span> <span class='m2'>64 61 74 61  10 00 00 00</span> <span class='m3'>00 00 41 2d</span>  |....data......A-|
00000030  <span class='m3'>00 40 41 2d 00 00 bf d2  00 c0 bf d2</span>              |.@A-........|
</pre>

Die Farben zeigen die drei Chunks: <span class='m0'>RIFF</span>, <span class='m1'>fmt</span> und <span class='m2'>data</span>, und danach die <span class='m3'>Messungen</span>. Die ersten 44 Bytes heißen zusammen der **Header**; sie sind in jeder WAV-Datei dieser Art gleich aufgebaut, nur die Zahlen darin ändern sich.

#### Der RIFF-Chunk

<pre class='spec'>
      7 6 5 4 3 2 1 0        Field Name                    Type
     +---------------+
   0 |               |       Chunk ID ("RIFF")             4 Bytes
     +-   . . . .   -+
   3 |               |
     +---------------+
   4 |               |       Chunk Size                    Unsigned, 4 Bytes
     +-   . . . .   -+
   7 |               |
     +---------------+
   8 |               |       Format ("WAVE")               4 Bytes
     +-   . . . .   -+
  11 |               |
     +---------------+
</pre>

<pre class='spec hexdump'>
00000000  <span class='m0'>52 49 46 46</span> <span class='m1'>34 00 00 00</span>  <span class='m2'>57 41 56 45</span> 66 6d 74 20  |RIFF4...WAVEfmt |
</pre>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table bytes'>
<tr>
<td><span class='m0'><code>52 49 46 46</code></span></td>
<td>Die Zeichen <code>RIFF</code></td>
</tr>
<tr>
<td><span class='m1'><code>34 00 00 00</code></span></td>
<td>Chunk Size: 0x34 = 52
<p class='expl'>
So viele Bytes kommen nach diesem Feld noch, bis die Datei zu Ende ist: 60 − 8. Im Header gezählt sind das 36 Bytes und dazu die Bytes der Messungen.
</p>
</td>
</tr>
<tr>
<td><span class='m2'><code>57 41 56 45</code></span></td>
<td>Die Zeichen <code>WAVE</code>: In dieser RIFF-Datei steckt Ton.</td>
</tr>
</table>
</div>

#### Der fmt-Chunk

<pre class='spec'>
      7 6 5 4 3 2 1 0        Field Name                    Type
     +---------------+
  12 |               |       Chunk ID ("fmt ")             4 Bytes
     +-   . . . .   -+
  15 |               |
     +---------------+
  16 |               |       Chunk Size (16)               Unsigned, 4 Bytes
     +-   . . . .   -+
  19 |               |
     +---------------+
  20 |               |       Audio Format (1 = PCM)        Unsigned, 2 Bytes
     +-             -+
  21 |               |
     +---------------+
  22 |               |       Number of Channels            Unsigned, 2 Bytes
     +-             -+
  23 |               |
     +---------------+
  24 |               |       Sample Rate                   Unsigned, 4 Bytes
     +-   . . . .   -+
  27 |               |
     +---------------+
  28 |               |       Byte Rate                     Unsigned, 4 Bytes
     +-   . . . .   -+
  31 |               |
     +---------------+
  32 |               |       Block Align                   Unsigned, 2 Bytes
     +-             -+
  33 |               |
     +---------------+
  34 |               |       Bits per Sample               Unsigned, 2 Bytes
     +-             -+
  35 |               |
     +---------------+
</pre>

<pre class='spec hexdump'>
00000000  52 49 46 46 34 00 00 00  57 41 56 45 <span class='m0'>66 6d 74 20</span>  |RIFF4...WAVEfmt |
00000010  <span class='m1'>10 00 00 00</span> <span class='m2'>01 00</span> <span class='m3'>01 00</span>  <span class='m4'>40 1f 00 00</span> <span class='m5'>80 3e 00 00</span>  |........@....&gt;..|
00000020  <span class='m6'>02 00</span> <span class='m7'>10 00</span> 64 61 74 61  10 00 00 00 00 00 41 2d  |....data......A-|
</pre>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table bytes'>
<tr>
<td><span class='m0'><code>66 6d 74 20</code></span></td>
<td>Die Zeichen <code>fmt </code>, mit einem Leerzeichen am Ende: Jeder Name hat vier Zeichen.</td>
</tr>
<tr>
<td><span class='m1'><code>10 00 00 00</code></span></td>
<td>Chunk Size: 16 Bytes folgen in diesem Chunk.</td>
</tr>
<tr>
<td><span class='m2'><code>01 00</code></span></td>
<td>Audio Format: 1 heißt PCM, die Messungen stehen so in der Datei, wie sie gemessen wurden.
<p class='expl'>
Andere Zahlen stehen für Verfahren, die die Messungen umrechnen; eines davon, μ-law, kommt im nächsten Kapitel.
</p>
</td>
</tr>
<tr>
<td><span class='m3'><code>01 00</code></span></td>
<td>Number of Channels: 1, Mono. Bei Stereo stünde hier 2, und die Messungen für links und rechts kämen abwechselnd.</td>
</tr>
<tr>
<td><span class='m4'><code>40 1f 00 00</code></span></td>
<td>Sample Rate: 0x1f40 = 8.000 Messungen in der Sekunde</td>
</tr>
<tr>
<td><span class='m5'><code>80 3e 00 00</code></span></td>
<td>Byte Rate: 0x3e80 = 16.000 Bytes in der Sekunde
<p class='expl'>
Abtastrate mal Kanäle mal Bytes je Messung: 8.000 · 1 · 2. Ein Programm, das die Datei abspielt, sieht daran, wie schnell es lesen muss.
</p>
</td>
</tr>
<tr>
<td><span class='m6'><code>02 00</code></span></td>
<td>Block Align: 2 Bytes für alle Kanäle eines Zeitpunkts</td>
</tr>
<tr>
<td><span class='m7'><code>10 00</code></span></td>
<td>Bits per Sample: 16</td>
</tr>
</table>
</div>

#### Der data-Chunk

<pre class='spec'>
      7 6 5 4 3 2 1 0        Field Name                    Type
     +---------------+
  36 |               |       Chunk ID ("data")             4 Bytes
     +-   . . . .   -+
  39 |               |
     +---------------+
  40 |               |       Chunk Size                    Unsigned, 4 Bytes
     +-   . . . .   -+
  43 |               |
     +---------------+
  44 |               |       Samples                       Chunk Size Bytes
     +-   . . . .   -+
     |               |
     +---------------+
</pre>

<pre class='spec hexdump'>
00000020  02 00 10 00 <span class='m0'>64 61 74 61</span>  <span class='m1'>10 00 00 00</span> <span class='m2'>00 00</span> <span class='m3'>41 2d</span>  |....data......A-|
00000030  <span class='m4'>00 40</span> <span class='m3'>41 2d</span> <span class='m2'>00 00</span> <span class='m5'>bf d2</span>  <span class='m6'>00 c0</span> <span class='m5'>bf d2</span>              |.@A-........|
</pre>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table bytes'>
<tr>
<td><span class='m0'><code>64 61 74 61</code></span></td>
<td>Die Zeichen <code>data</code></td>
</tr>
<tr>
<td><span class='m1'><code>10 00 00 00</code></span></td>
<td>Chunk Size: 16 Bytes, acht Messungen zu je 2 Bytes</td>
</tr>
<tr>
<td><span class='m2'><code>00 00</code></span></td>
<td>0: Die Schwingung beginnt bei null.</td>
</tr>
<tr>
<td><span class='m3'><code>41 2d</code></span></td>
<td>0x2d41 = 11.585</td>
</tr>
<tr>
<td><span class='m4'><code>00 40</code></span></td>
<td>0x4000 = 16.384, der höchste Punkt: halb so laut, wie es geht.</td>
</tr>
<tr>
<td><span class='m5'><code>bf d2</code></span></td>
<td>0xd2bf = −11.585
<p class='expl'>
Eine negative Zahl steht im Zweierkomplement in der Datei: 0xd2bf ist 53.951, und 53.951 − 65.536 = −11.585. Jede Zahl ab 0x8000 ist negativ.
</p>
</td>
</tr>
<tr>
<td><span class='m6'><code>00 c0</code></span></td>
<td>0xc000 = −16.384, der tiefste Punkt</td>
</tr>
</table>
</div>

Eine Schwingung mit 1.000 Hz dauert eine Millisekunde, genau so lange wie diese acht Messungen: Die Datei ist eine einzige Schwingung, gemessen an acht Punkten.

### Den Header schreiben

Bytes wie diese baut in Python das Modul [`struct`](https://docs.python.org/3/library/struct.html). `struct.pack` bekommt einen Text, der sagt, wie die Bytes aussehen sollen, und die Werte dazu:

<!-- page-code: own -->
```python
struct.pack('<4sI4s', b'RIFF', 52, b'WAVE')
```

`<` heißt Little Endian, `4s` sind vier Bytes, so wie sie sind, `I` ist eine Zahl ohne Vorzeichen mit 4 Bytes und `H` eine mit 2 Bytes. Heraus kommen die ersten 12 Bytes der Datei oben.

Lege im Ordner `audio` die Datei `wav.py` an:

<!-- page-code: own -->
```python
import struct


def wav_header(n, rate):
    size = 2 * n
    riff = struct.pack('<4sI4s', b'RIFF', 36 + size, b'WAVE')
    # hier: fmt und data
    return riff + fmt + data
```

`wav_header` bekommt die Zahl der Messungen `n` und die Abtastrate `rate` und gibt die 44 Bytes des Headers zurück, für eine Datei in Mono mit 16 Bit. `size` ist die Zahl der Bytes der Messungen, zwei für jede.

<div class='hint own' data-check='wav-header'>
<strong>Jetzt bist du dran:</strong> Schreib die beiden Zeilen für <code>fmt</code> und <code>data</code>, jede mit einem <code>struct.pack</code>. Die Felder, ihre Längen und ihre Werte stehen in den Tabellen oben.
</div>

<!-- hint -->
Der fmt-Chunk hat acht Felder. Schreib sie dir der Reihe nach auf, jedes mit seiner Länge: Welche haben 4 Bytes, welche 2? Welche Werte hängen von `rate` ab, welche sind immer gleich?
<!-- /hint -->
<!-- hint -->
Für den fmt-Chunk passt `'<4sIHHIIHH'`: der Name, dann die Chunk Size mit 4 Bytes, zwei Felder mit 2 Bytes, zwei mit 4 und zwei mit 2. Die Byte Rate ist `2 * rate`. Der data-Chunk braucht nur `'<4sI'`.
<!-- /hint -->

<!-- solution -->
```python
def wav_header(n, rate):
    size = 2 * n
    riff = struct.pack('<4sI4s', b'RIFF', 36 + size, b'WAVE')
    fmt = struct.pack('<4sIHHIIHH', b'fmt ', 16, 1, 1, rate, 2 * rate, 2, 16)
    data = struct.pack('<4sI', b'data', size)
    return riff + fmt + data
```
<!-- /solution -->

_task(wav-header)

Nach dem Header kommen die Messungen. Im Programm sind sie Kommazahlen von −1 bis 1, damit es beim Rechnen egal ist, wie viele Bits die Datei hat; erst beim Schreiben werden sie zu ganzen Zahlen mit 16 Bit. Schreib zwei Funktionen dazu, eine zum Schreiben und eine zum Lesen, und oben die Zeilen für `wave` und `numpy`:

```python
~ import struct
import wave

import numpy as np
```

```python
def write_wav(path, samples, rate):
    numbers = np.clip(np.round(samples * 32767), -32768, 32767).astype('<i2')
    with open(path, 'wb') as f:
        f.write(wav_header(len(numbers), rate))
        f.write(numbers.tobytes())


def read_wav(path):
    with wave.open(path, 'rb') as f:
        rate = f.getframerate()
        frames = f.readframes(f.getnframes())
    return np.frombuffer(frames, dtype='<i2') / 32767, rate
```

`samples` ist ein numpy-Array, eine Liste von Zahlen, mit der man auf einmal rechnet: `samples * 32767` multipliziert jede Zahl darin. `np.round` rundet jede, `np.clip` begrenzt jede auf −32.768 bis 32.767, und `astype('<i2')` macht daraus ganze Zahlen mit 2 Bytes im Little Endian. `tobytes` gibt ihre Bytes, eine Messung nach der anderen.

Zum Lesen nimmt `read_wav` das Modul [`wave`](https://docs.python.org/3/library/wave.html) aus Python: Es liest den Header und gibt die Abtastrate und die Bytes der Messungen. Ist der Header kaputt, bricht es mit einem Fehler ab. `np.frombuffer` macht aus den Bytes wieder Zahlen, und `/ 32767` bringt sie zurück auf −1 bis 1.

### Ein Ton aus Zahlen

Ein reiner Ton ist eine Sinusschwingung. Mit `f` Schwingungen in der Sekunde und halb so laut, wie es geht, ist die Messung zur Zeit `t` (in Sekunden)

$$x(t) = 0{,}5 \cdot \sin(2\pi f t)$$

Die `n`-te Messung wird zur Zeit \\(t = n / \text{rate}\\) gemacht. Lege die Datei `tones.py` an:

<!-- page-code: own -->
```python
import numpy as np

from wav import write_wav


def tone(freq, seconds, rate):
    # hier: die Messungen eines Tons


if __name__ == '__main__':
    write_wav('kurz.wav', tone(1000, 0.001, 8000), 8000)
    write_wav('a.wav', tone(440, 2, 44100), 44100)
```

<div class='hint own' data-check='ton'>
<strong>Jetzt bist du dran:</strong> Schreib <code>tone(freq, seconds, rate)</code>. Die Funktion gibt ein numpy-Array mit den Messungen eines Tons mit <code>freq</code> Hz zurück, <code>seconds</code> Sekunden lang, bei <code>rate</code> Messungen in der Sekunde. <code>np.arange(k)</code> gibt die Zahlen 0, 1, …, k − 1 als Array, und <code>np.sin</code> rechnet den Sinus jeder Zahl eines Arrays.
</div>

<!-- hint -->
Wie viele Messungen sind es? Bei `rate` Messungen in der Sekunde und `seconds` Sekunden: `round(seconds * rate)`. Für `tone(1000, 0.001, 8000)` sind es 8.
<!-- /hint -->
<!-- hint -->
Erst die Zeiten, dann die Formel, beides für das ganze Array auf einmal:

```python
t = np.arange(…) / rate
return 0.5 * np.sin(…)
```
<!-- /hint -->

<!-- solution -->
```python
def tone(freq, seconds, rate):
    t = np.arange(round(seconds * rate)) / rate
    return 0.5 * np.sin(2 * np.pi * freq * t)
```
<!-- /solution -->

_task(ton)

Lass das Programm laufen und sieh dir die kleine Datei an:

```bash
python3 tones.py
hexdump -C kurz.wav
```

<!-- tutorial-step
write-file: audio/wav.py <- file:stufen/wav.py
write-file: audio/tones.py <- file:stufen/tones-1.py
check-task: wav-header
check-task: ton
terminal-run: cd /workspace/audio && python3 tones.py && hexdump -C kurz.wav
terminal-wait-for-prompt: 30
wait-for-text: 00 c0 bf d2
-->

Dort stehen dieselben 60 Bytes wie oben. Öffne dann `a.wav` im Explorer und spiel sie ab: zwei Sekunden lang der Kammerton a mit 440 Hz, nach dem ein Orchester seine Instrumente stimmt.

<div class='hint try'>
<strong>Ausprobieren:</strong> Schreib in <code>tones.py</code> eine dritte Datei mit <code>tone(440, 2, 8000)</code> und Abtastrate 8.000. Klingt sie anders als <code>a.wav</code>? Und wie groß sind die beiden Dateien (<code>ls -l</code>)? Versuch es dann mit <code>tone(5000, 2, 8000)</code>: Welchen Ton hörst du, und warum keinen mit 5.000 Hz? Nimm die Zeilen danach wieder heraus.
</div>

### Drei Töne auf einmal

Zwei Klänge, die zugleich erklingen, sind in Zahlen eine Summe: Der Druck der Luft ist an jeder Stelle die Summe dessen, was beide Klänge aus ihr machen. Ein Akkord aus drei Tönen, A, Cis und E, ist darum die Summe dreier Töne. Schreib unten in `tones.py` dazu:

```python
~     write_wav('a.wav', tone(440, 2, 44100), 44100)
    chord = tone(440, 2, 44100) + tone(554.37, 2, 44100) + tone(659.26, 2, 44100)
    write_wav('akkord.wav', chord, 44100)
```

<!-- tutorial-step
write-file: audio/tones.py <- file:stufen/tones-2.py
terminal-run: python3 tones.py
terminal-wait-for-prompt: 30
-->

_task(drei-toene)

<!-- after: drei-toene -->
Jeder der drei Töne schwingt bis 0,5, zusammen kommen sie an manchen Stellen bis fast 1,5. In der Datei gibt es aber keine Zahl über 32.767: `np.clip` schneidet alles darüber ab, und die Welle hat dort flache Stellen statt runder Spitzen. Eine solche Ecke ist ein Gemisch vieler hoher Frequenzen, die in keinem der drei Töne vorkamen, und die hörst du als Kratzen. Das heißt **Clipping**, und es passiert genauso, wenn ein Mikrofon zu laut ausgesteuert ist. Ohne `np.clip` wäre es schlimmer: Eine Zahl über 32.767 würde in 16 Bit zu einer negativen, und es knallte.

Damit es sauber klingt, muss die Summe unter 1 bleiben, etwa so: `chord = (…) / 3`. Mach das und hör noch einmal hin.
<!-- /after -->

<!-- stage: stufen/tones-3.py as tones.py -->

## Kleine Tricks

Eine Minute in CD-Qualität braucht 10 MB. Bevor es darum geht, was das Ohr nicht hört, bringen drei kleine Tricks schon einiges: Stufen, die sich nach der Lautstärke richten, eine Vorhersage der nächsten Messung und kurze Codes für kleine Zahlen. Die beiden letzten verlieren nichts: Was herauskommt, ist Bit für Bit dasselbe wie vorher.

### Leise Stellen, grobe Stufen

Das Telefonnetz schickt Gespräche seit 1961 als 8.000 Messungen in der Sekunde mit je 8 Bit, 64.000 Bit in der Sekunde. 8 Bit sind 255 Stufen von −127 bis 127, mit gleichen Abständen von 1/127 zwischen −1 und 1. Eine laute Stimme nutzt sie alle. Eine leise, die nur bis 0,06 geht, nutzt nur die Stufen von −8 bis 8. Der Fehler beim Runden ist bei beiden gleich groß, bei der leisen Stimme aber fast so laut wie die Stimme selbst.

Im Widget ist die Stimme so leise; damit du etwas hörst, wird sie nach dem Runden wieder lauter gemacht:

<div class='sampling-widget' data-mulaw data-bits='8' data-clip='stimme' data-level='0.06'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Spiel die leise Stimme mit gleichmäßigen Stufen ab und hör auf das Rauschen am Ende jedes Worts, wenn die Stimme leiser wird. Schalt dann auf 16 Bit und zurück auf 8. Schalt danach auf μ-law: Wo ist das Rauschen geblieben? Stell zum Schluss 4 Bit ein und zähl im Bild die Linien der Stufen, auf denen die leise Stimme sitzt, einmal gleichmäßig und einmal nach μ-law.
</div>

Gleichmäßige Stufen sind für leise Stellen zu grob und für laute unnötig fein: Ein Fehler von 1/127 fällt bei einer Messung von 0,9 nicht auf, bei einer von 0,02 schon. Besser sind Stufen, die mit der Messung wachsen, eng bei null und weit bei den lauten Messungen. Dann ist der Fehler immer ungefähr derselbe Bruchteil der Messung, bei leisen Stellen genauso wie bei lauten.

<img class='drawing' style='width: 35em' src='mulaw-stufen.svg' alt='Die Stufen von 4 Bit auf der positiven Hälfte von 0 bis 1, zweimal übereinander. Oben gleichmäßig: acht Striche in gleichen Abständen, bei 0, einem Siebtel, zwei Siebteln und so weiter bis 1. Unten nach μ-law: dieselben acht Striche, aber dicht gedrängt bei 0 und nach rechts immer weiter auseinander, der vorletzte erst bei knapp 0,5. Ein blasser Streifen von 0 bis 0,06 markiert eine leise Stimme: Oben liegt darin nur der Strich bei 0, unten liegen vier.'>

So arbeitet **μ-law** (gesprochen »mü-law«): Erst wird jede Messung `x` mit einem Logarithmus gestaucht, dann gleichmäßig gerundet, und beim Abspielen wird das Stauchen wieder rückgängig gemacht. Gestaucht wird mit

$$y = \operatorname{sgn}(x) \cdot \frac{\ln(1 + \mu \lvert x \rvert)}{\ln(1 + \mu)}, \quad \mu = 255$$

`sgn(x)` ist das Vorzeichen, 1 oder −1: Der Logarithmus rechnet mit dem Betrag \\(\lvert x \rvert\\), und das Vorzeichen kommt danach wieder dazu. Aus 0 wird 0, aus 1 wird 1, aber schon 0,06 wird zu 0,5. Die halbe Zahl der Stufen gehört so den Messungen unter 0,06.

Festgelegt hat das die internationale Fernmeldeunion 1972 in der Norm [G.711](https://www.itu.int/rec/T-REC-G.711): μ-law in Nordamerika und Japan, das ähnliche A-law in Europa und fast überall sonst. Um die Null liegen die Stufen von 8 Bit nach μ-law enger als die von 13 Bit mit gleichmäßigen Stufen. G.711 ist bis heute im Einsatz, auch bei Anrufen über das Internet.

Lege die Datei `mulaw.py` an. `mulaw` staucht wie in der Formel, `np.log` ist der natürliche Logarithmus und `np.sign` das Vorzeichen:

<!-- page-code: own -->
```python
import numpy as np

from wav import read_wav, write_wav

MU = 255


def mulaw(x):
    return np.sign(x) * np.log(1 + MU * np.abs(x)) / np.log(1 + MU)


def unmulaw(y):
    # hier: das Stauchen rückgängig machen
```

<div class='hint own' data-check='unmulaw'>
<strong>Jetzt bist du dran:</strong> Schreib <code>unmulaw(y)</code>: Die Funktion macht <code>mulaw</code> rückgängig, also gilt <code>unmulaw(mulaw(x)) == x</code>, bis auf die letzten Stellen hinter dem Komma. Wie bei <code>mulaw</code> soll sie mit einem ganzen numpy-Array auf einmal rechnen.
</div>

<!-- hint -->
Rechne mit dem Betrag: Aus \\(\lvert y \rvert = \ln(1 + \mu \lvert x \rvert) / \ln(1 + \mu)\\) wird, mit \\(\ln(1 + \mu)\\) multipliziert, \\(\lvert y \rvert \cdot \ln(1 + \mu) = \ln(1 + \mu \lvert x \rvert)\\). Was ist \\(e\\) hoch beide Seiten?
<!-- /hint -->
<!-- hint -->
\\(e^{\lvert y \rvert \cdot \ln(1 + \mu)}\\) ist \\((1 + \mu)^{\lvert y \rvert}\\), also gilt \\((1 + \mu)^{\lvert y \rvert} = 1 + \mu \lvert x \rvert\\). Löse nach \\(\lvert x \rvert\\) auf und multipliziere zum Schluss mit `np.sign(y)`. `**` rechnet in numpy mit jeder Zahl eines Arrays.
<!-- /hint -->

<!-- solution -->
```python
def unmulaw(y):
    return np.sign(y) * ((1 + MU) ** np.abs(y) - 1) / MU
```
<!-- /solution -->

_task(unmulaw)

Jetzt fehlt das Runden, und ein Maß dafür, wie gut eine Kopie zum Original passt. Schreib unter `unmulaw`:

```python
def quantise(x, bits):
    top = 2 ** (bits - 1) - 1
    return np.round(x * top) / top


def snr(original, copy):
    noise = original - copy
    return 10 * np.log10(np.sum(original ** 2) / np.sum(noise ** 2))
```

`quantise` rundet jede Messung auf eine von `2 * top + 1` gleichmäßigen Stufen, bei 8 Bit auf ein Vielfaches von 1/127. `snr` vergleicht die Energie des Originals, die Summe der Quadrate aller Messungen, mit der Energie des Fehlers. Das Verhältnis heißt **Signal-Rausch-Abstand** und wird in **Dezibel** angegeben, 10-mal dem Logarithmus zur Basis 10: 10 dB mehr sind 10-mal so viel Energie, 20 dB 100-mal so viel. Ist eine Welle doppelt so hoch, hat sie 4-mal so viel Energie, das sind 6 dB mehr. Je größer die Zahl, desto weniger hört man den Fehler.

Unten in die Datei kommt das Programm: Es macht die Stimme leiser, rundet sie auf beide Arten mit 8 Bit und gibt aus, wie weit der Fehler jeweils unter der Stimme liegt. Die leiseste Fassung schreibt es in zwei Dateien, wieder laut gemacht:

```python
if __name__ == '__main__':
    voice, rate = read_wav('stimme.wav')
    for level in [1, 0.25, 0.06]:
        quiet = voice * level
        even = quantise(quiet, 8)
        law = unmulaw(quantise(mulaw(quiet), 8))
        print(f'{level:4}: gleichmäßig {snr(quiet, even):4.1f} dB, μ-law {snr(quiet, law):4.1f} dB')
    write_wav('leise-gleichmaessig.wav', even / level, rate)
    write_wav('leise-mulaw.wav', law / level, rate)
```

_task(viermal-leiser)

<!-- after: viermal-leiser -->
Lass das Programm laufen:

```bash
python3 mulaw.py
```

<!-- tutorial-step
write-file: audio/mulaw.py <- file:stufen/mulaw.py
check-task: unmulaw
terminal-run: cd /workspace/audio && (test -f stimme.wav || curl -sO http://workspace_nginx_1/audio/stimme.wav) && (test -f musik.wav || curl -sO http://workspace_nginx_1/audio/musik.wav) && python3 mulaw.py
terminal-wait-for-prompt: 30
wait-for-text: 0.06: gleichmäßig 12.1 dB
-->

Bei voller Lautstärke liegen beide Fehler weit unter der Stimme, gleichmäßig 35 dB, μ-law knapp 38. Viermal leiser verliert die gleichmäßige Rundung 12 dB, wie vorhergesagt: Ihr Fehler bleibt gleich groß, die Stimme hat ein Sechzehntel der Energie. Noch einmal etwa viermal leiser sind es nur 12 dB, der Fehler ist fast so laut wie die Stimme. μ-law verliert dabei kaum etwas, weil seine Stufen mit der Stimme kleiner werden. Hör dir die beiden Dateien `leise-gleichmaessig.wav` und `leise-mulaw.wav` an.
<!-- /after -->

### Die nächste Messung raten

Bei 44.100 Messungen in der Sekunde ändert sich ein Klang von einer Messung zur nächsten nur wenig. Hier sind acht Messungen aus der Stimme, ab dem Index 56.019, als ganze Zahlen mit 16 Bit:

<div class='table-responsive'>
<table class='table'>
<tr><th>Messung</th><td>6.445</td><td>6.866</td><td>7.298</td><td>7.730</td><td>8.163</td><td>8.600</td><td>9.040</td><td>9.481</td></tr>
<tr><th>minus die vorige</th><td></td><td>421</td><td>432</td><td>432</td><td>433</td><td>437</td><td>440</td><td>441</td></tr>
<tr><th>minus die Gerade</th><td></td><td></td><td>11</td><td>0</td><td>1</td><td>4</td><td>3</td><td>1</td></tr>
</table>
</div>

Die Messungen sind Zahlen um 8.000, ihre Unterschiede zur vorigen Messung nur um 430. Noch besser rät, wer die Gerade durch die beiden letzten Messungen verlängert: Steigt der Klang von 6.445 auf 6.866, um 421, dann ist die beste Schätzung für die nächste Messung 6.866 + 421 = 7.287. Gemessen sind 7.298, der Rest ist 11. Allgemein ist die Schätzung für die Messung mit dem Index `n`

$$x\_{n-1} + (x\_{n-1} - x\_{n-2}) = 2 x\_{n-1} - x\_{n-2}$$

An dieser Stelle steigt die Welle fast gerade an, darum klappt es so gut; an anderen Stellen ist der Rest größer. Gespeichert werden statt der Messungen die Reste. Wer sie liest, rechnet dieselbe Schätzung aus und zählt den Rest dazu: Er bekommt die Messung genau zurück.

Die Idee ist alt. C. Chapin Cutler von den Bell Labs meldete 1950 ein [Patent](https://patents.google.com/patent/US2605361A/en) an, Unterschiede statt Messungen zu senden, die **Differenz-Pulscodemodulation** (DPCM). Mit Unterschieden, die sich nach der Lautstärke richten, kam die ITU 1984 in der Norm G.721 auf 4 Bit je Messung, halb so viel wie G.711. Dort werden die Unterschiede gerundet, und etwas geht verloren. Hier bleibt jeder Rest, wie er ist.

Lege `predict.py` an. Zum Rechnen braucht es die Messungen als ganze Zahlen, wie in der Datei; `numbers` macht sie aus dem, was `read_wav` gibt. Mit `int64`, 8 Bytes je Zahl, passt auch ein Rest, der größer ist als 16 Bit:

<!-- page-code: own -->
```python
import numpy as np

from wav import read_wav


def numbers(path):
    samples, rate = read_wav(path)
    return np.round(samples * 32767).astype(np.int64)


def residual(x):
    # hier: die Reste
```

Mit numpy rechnet man am besten mit Ausschnitten statt mit einer Schleife. `x[2:]` sind alle Messungen ab dem Index 2, `x[1:-1]` alle vom Index 1 bis ohne die letzte, `x[:-2]` alle ohne die letzten beiden. Die drei sind gleich lang, und an derselben Stelle steht in ihnen eine Messung, ihre Vorgängerin und deren Vorgängerin.

<div class='hint own' data-check='residual'>
<strong>Jetzt bist du dran:</strong> Schreib <code>residual(x)</code>: Die Funktion bekommt ein numpy-Array mit ganzen Zahlen und gibt ein gleich langes Array zurück. Die ersten beiden Zahlen bleiben, wie sie sind, denn für sie gibt es keine Gerade. Ab dem Index 2 steht dort die Messung minus die Schätzung <code>2 * x[n - 1] - x[n - 2]</code>. <code>x</code> selbst bleibt dabei unverändert.
</div>

<!-- hint -->
Fang mit einer Kopie an, `r = x.copy()`; dann bleiben die ersten beiden Zahlen von selbst, und `x` ändert sich nicht. Einem Ausschnitt kann man ein ganzes Array zuweisen: `r[2:] = …`.
<!-- /hint -->
<!-- hint -->
Die Messungen sind `x[2:]`, ihre Vorgängerinnen `x[1:-1]` und deren Vorgängerinnen `x[:-2]`:

```python
r = x.copy()
r[2:] = x[2:] - (2 * … - …)
return r
```
<!-- /hint -->

<!-- solution -->
```python
def residual(x):
    r = x.copy()
    r[2:] = x[2:] - (2 * x[1:-1] - x[:-2])
    return r
```
<!-- /solution -->

_task(residual)

Zurück geht es nur der Reihe nach, denn für jede Messung braucht man die beiden davor, und die stehen erst fest, wenn sie selbst zurückgerechnet sind. Darum braucht `restore` eine Schleife. Schreib sie unter `residual`, dazu das Programm:

```python
def restore(r):
    x = r.copy()
    for n in range(2, len(x)):
        x[n] = r[n] + 2 * x[n - 1] - x[n - 2]
    return x


if __name__ == '__main__':
    for path in ['stimme.wav', 'musik.wav']:
        x = numbers(path)
        r = residual(x)
        print(f'{path}: Messungen im Mittel {np.abs(x).mean():.0f}, Reste {np.abs(r).mean():.0f}')
        print('  zurück wie vorher:', np.array_equal(restore(r), x))
```

`np.abs(x).mean()` ist der Mittelwert der Beträge, wie groß eine Zahl im Mittel ist. `np.array_equal` sagt, ob zwei Arrays in jeder Zahl übereinstimmen.

```bash
python3 predict.py
```

<!-- tutorial-step
write-file: audio/predict.py <- file:stufen/predict.py
check-task: residual
terminal-run: python3 predict.py
terminal-wait-for-prompt: 30
wait-for-text: Reste 397
-->

Bei der Stimme sind die Reste im Mittel ein Zwanzigstel so groß wie die Messungen, bei der Musik ein Zehntel, und `restore` bringt jede Messung genau zurück.

<div class='hint try'>
<strong>Ausprobieren:</strong> Rate statt mit der Geraden mit der vorigen Messung allein: <code>r[2:] = x[2:] - x[1:-1]</code>, und in <code>restore</code> entsprechend <code>x[n] = r[n] + x[n - 1]</code>. Bei welcher der beiden Dateien hilft die Gerade mehr? Die Hi-Hat der Musik ist Rauschen: Lässt sich Rauschen vorhersagen? Stell beides danach wieder zurück.
</div>

### Kleine Zahlen, kurze Codes

Die Reste sind klein, aber in der Datei hätte jeder immer noch 16 Bit, und gespart wäre nichts. Kleine Zahlen brauchen kurze Codes, große dürfen längere haben, wie bei [Huffman](/huffman). Ein Huffman-Code bräuchte eine Tabelle für jede Datei. Für Zahlen, bei denen kleine häufig und große selten sind, geht es ohne: Solomon Golomb fand 1966 Codes dafür, und Robert Rice nahm am Jet Propulsion Laboratory der NASA davon die einfachsten, für Daten von Raumsonden. Ein **Rice-Code** hat einen Parameter `k` und schreibt eine Zahl `u ≥ 0` in zwei Teilen:

1. `u // 2**k` als so viele Nullen, gefolgt von einer 1,
2. dann `u % 2**k` als Binärzahl mit genau `k` Stellen.

Mit `k = 2` sieht das so aus:

<div class='table-responsive'>
<table class='table'>
<tr><th><code>u</code></th><td>0</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td></tr>
<tr><th>Code</th><td><code>1 00</code></td><td><code>1 01</code></td><td><code>1 10</code></td><td><code>1 11</code></td><td><code>01 00</code></td><td><code>01 01</code></td><td><code>01 10</code></td><td><code>01 11</code></td><td><code>001 00</code></td><td><code>001 01</code></td></tr>
</table>
</div>

Zahlen bis 3 brauchen 3 Bit, bis 7 vier, und jede weitere Vierergruppe ein Bit mehr. Wer liest, zählt die Nullen bis zur 1 und liest dann `k` Bits: Ein Code endet von selbst, ohne Trennzeichen. Ein größeres `k` passt für größere Zahlen.

Die Reste sind aber auch negativ. Darum werden sie vorher im Zickzack umgeordnet, 0, −1, 1, −2, 2, … werden zu 0, 1, 2, 3, 4, …: Eine Zahl `r ≥ 0` wird zu `2 * r`, eine negative zu `-2 * r - 1`. Lege `rice.py` an:

<!-- page-code: own -->
```python
import numpy as np

from predict import numbers, residual


def zigzag(r):
    return np.where(r >= 0, 2 * r, -2 * r - 1)


def rice(u, k):
    # hier: der Code als Text aus 0 und 1
```

`np.where(bedingung, a, b)` nimmt für jede Zahl `a`, wo die Bedingung gilt, sonst `b`.

<div class='hint own' data-check='rice'>
<strong>Jetzt bist du dran:</strong> Schreib <code>rice(u, k)</code> für eine ganze Zahl <code>u ≥ 0</code> und <code>k ≥ 1</code>. Die Funktion gibt den Rice-Code als Text aus Nullen und Einsen zurück, ohne Leerzeichen: <code>rice(5, 2)</code> ergibt <code>'0101'</code>.
</div>

<!-- hint -->
Rechne erst die beiden Teile aus: `q = u // 2**k` und `rest = u % 2**k`. Aus `q` werden `'0' * q` und eine `'1'`.
<!-- /hint -->
<!-- hint -->
`format(rest, 'b')` schreibt eine Zahl binär, aber ohne führende Nullen. Mit `format(rest, '02b')` hat sie mindestens 2 Stellen, mit `format(rest, f'0{k}b')` mindestens `k`.
<!-- /hint -->

<!-- solution -->
```python
def rice(u, k):
    return '0' * (u // 2 ** k) + '1' + format(u % 2 ** k, f'0{k}b')
```
<!-- /solution -->

_task(rice)

Für die ganze Datei braucht es keine Texte, nur ihre Länge: `u // 2**k` Nullen, eine 1 und `k` Bits. `u >> k` ist dasselbe wie `u // 2**k` und rechnet mit dem ganzen Array. Welches `k` am besten ist, probiert `best_bits` einfach aus:

```python
def rice_bits(u, k):
    return int(np.sum((u >> k) + 1 + k))


def best_bits(u):
    return min(rice_bits(u, k) for k in range(16))


if __name__ == '__main__':
    for path in ['stimme.wav', 'musik.wav']:
        x = numbers(path)
        u = zigzag(residual(x))
        print(f'{path}: {best_bits(u) / (16 * len(x)):.0%} mit einem k')
```

```bash
python3 rice.py
```

<!-- tutorial-step
write-file: audio/rice.py <- file:stufen/rice-1.py
check-task: rice
terminal-run: python3 rice.py
terminal-wait-for-prompt: 30
wait-for-text: musik.wav: 70% mit einem k
-->

Die Stimme braucht 58 % ihrer 16 Bit je Messung, die Musik 70 %, und nichts ist verloren. Ein `k` für die ganze Datei passt aber nicht überall: In einer Pause sind die Reste winzig, in einem lauten Wort groß. Teil die Reste darum in Blöcke von 4.096 Messungen und such für jeden Block sein eigenes `k`. Das kostet 4 Bit je Block, um `k` zu speichern.

_task(je-block)

<!-- after: je-block -->
Schreib unten in die Schleife:

```python
~         print(f'{path}: {best_bits(u) / (16 * len(x)):.0%} mit einem k')
        blocks = sum(best_bits(u[s:s + 4096]) + 4 for s in range(0, len(u), 4096))
        print(f'{path}: {blocks / (16 * len(x)):.0%} mit einem k je Block')
```

<!-- tutorial-step
write-file: audio/rice.py <- file:stufen/rice-2.py
terminal-run: python3 rice.py
terminal-wait-for-prompt: 30
wait-for-text: musik.wav: 67% mit einem k je Block
-->

Die Stimme schrumpft von 58 auf 46 %, die Musik nur von 70 auf 67 %. Die Stimme hat Pausen und leise Silben, in denen ein kleines `k` reicht; ein Viertel ihrer Messungen liegt nahe bei null. Die Musik ist überall ungefähr gleich laut, und ein `k` passt fast überall.
<!-- /after -->

<!-- stage: stufen/rice-2.py as rice.py -->

So arbeitet **FLAC**, das freie Format für Musik ohne Verlust. Tony Robinson an der Universität Cambridge beschrieb 1994 sein Programm Shorten, das mit Vorhersage und Rice-Codes arbeitet. Josh Coalson begann im Jahr 2000 mit FLAC, Version 1.0 erschien 2001, und seit Dezember 2024 beschreibt es der [RFC 9639](https://www.rfc-editor.org/rfc/rfc9639.html). Die Vorhersage mit der Geraden steht dort genau so, als »fixed predictor« der Ordnung 2. FLAC kann mehr: Es sucht für jeden Block die beste Vorhersage, auch eine aus bis zu 32 vorigen Messungen mit eigenen Faktoren, und teilt jeden Block in Stücke mit eigenem `k`. Lass ffmpeg die beiden Dateien als FLAC schreiben:

```bash
ffmpeg -loglevel error -i stimme.wav stimme.flac
ffmpeg -loglevel error -i musik.wav musik.flac
ls -l *.wav *.flac
```

<!-- tutorial-step
terminal-run: ffmpeg -y -loglevel error -i stimme.wav stimme.flac && ffmpeg -y -loglevel error -i musik.wav musik.flac && ls -l stimme.* musik.*
terminal-wait-for-prompt: 30
wait-for-text: musik.flac
-->

Die Stimme braucht als FLAC etwa ein Drittel, die Musik etwa 60 %. Viel mehr schafft kein Verfahren ohne Verlust: Rauschen lässt sich nicht vorhersagen, und jedes Bit davon muss mit in die Datei. Wer eine Minute Musik auf ein Zehntel bringen will, muss etwas weglassen: das, was das Ohr nicht hört. Dazu muss man wissen, aus welchen Frequenzen ein Klang besteht.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>SHORTEN: Simple lossless and near-lossless waveform compression</strong>, Tony Robinson, Cambridge University Engineering Department, 1994. Die Vorhersage aus den vorigen Messungen und kurze Codes für die Reste, die Idee hinter FLAC, auf wenigen Seiten: <a href='http://mi.eng.cam.ac.uk/reports/svr-ftp/auto-pdf/robinson_tr156.pdf'>PDF</a>, die <a href='https://mi.eng.cam.ac.uk/reports/abstracts/robinson_tr156.html'>Zusammenfassung</a> in Cambridge.</p>
</div>
