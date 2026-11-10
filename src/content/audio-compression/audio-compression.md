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
