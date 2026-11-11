<div class='meta'>
image: teaser.webp
image_alt: "Ein kurzer Klang als Welle und dahinter seine Echos, jedes etwas leiser als das vorige."
kind: labor
language: Python
duration: 8 Doppelstunden
curriculum: "Oberstufe: Algorithmen und Datenstrukturen (Fourier-Transformation, Faltung), Vertiefung Technische Informatik (digitale Signalverarbeitung)"
topics: [codierung, listen]
requires: [audio-compression]
reference: [bits-und-bytes]
</div>

<script src='/include/effect_widget.js'></script>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Audioeffekte

<p class='abstract'>
Ein Equalizer, ein Echo, Hall, ein Chorus, ein Compressor, eine Stimme in einer anderen Tonhöhe und ein Roboter, der singt: Du baust die Effekte eines Tonstudios in Python mit numpy und hörst bei jedem im Widget, was seine Regler tun.
</p>

## Einrichten

Leg einen Ordner an und hol die Klänge aus »Audio komprimieren«: die Musik und die Stimme.

```bash_copy
mkdir -p /workspace/effects && cd /workspace/effects
wget ${web_root}/audio/musik.wav ${web_root}/audio/stimme.wav
```

Zum Lesen und Schreiben der WAV-Dateien nimmst du `wav.py`, wie du es in »Audio komprimieren« geschrieben hast. Lege es an:

```python
import struct
import wave

import numpy as np


def wav_header(n, rate):
    size = 2 * n
    riff = struct.pack('<4sI4s', b'RIFF', 36 + size, b'WAVE')
    fmt = struct.pack('<4sIHHIIHH', b'fmt ', 16, 1, 1, rate, 2 * rate, 2, 16)
    data = struct.pack('<4sI', b'data', size)
    return riff + fmt + data


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

`read_wav` gibt die Samples als Kommazahlen von −1 bis 1 und die Abtastrate, `write_wav` schreibt sie wieder als Datei mit 16 Bit.

<!-- tutorial-step
write-file: effects/wav.py <- file:stufen/wav.py
terminal-run: mkdir -p /workspace/effects && cd /workspace/effects && (test -f musik.wav || curl -sO http://workspace_nginx_1/audio/musik.wav) && (test -f stimme.wav || curl -sO http://workspace_nginx_1/audio/stimme.wav) && ls
terminal-wait-for-prompt: 30
wait-for-text: stimme.wav
-->

## Equalizer

In »Audio komprimieren« hast du Klänge in ihre Frequenzen zerlegt. Wer die Frequenzen kennt, kann sie auch verändern. Ein **Equalizer**, kurz EQ, macht einen Bereich von Frequenzen lauter oder leiser: der Bassregler am Lautsprecher, die Klangregler im Musikprogramm, im Tonstudio Dutzende davon. Ein Regler des EQ hat drei Einstellungen: um welche Frequenz es geht, wie breit der Bereich ist und wie viele Dezibel dazukommen oder wegfallen. Die Breite zählt man in Oktaven, denn das Ohr hört Frequenzen auf einer logarithmischen Skala: Von 100 bis 200 Hz ist es so weit wie von 1.000 bis 2.000 Hz.

<div class='effect-widget' data-effect='eq'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Am Anfang ist alles weit weg von der Mitte 12 dB leiser, <strong>Übrige</strong>: Was hörst du von der Musik noch? Stell <strong>Übrige</strong> auf 0 dB und dann 60 Hz, eine Oktave und +12 dB ein: Welches Instrument wird lauter? Dann −24 dB bei 10 kHz mit zwei Oktaven: Was verschwindet? Schalt mit <strong>Effekt an</strong> hin und her und achte darauf, was sich ändert. Schieb bei +12 dB die Frequenz langsam von unten nach oben und hör, wann die Melodie lauter wird. Zum Schluss ein Bandpass, der nur einen Bereich durchlässt: Stell <strong>Übrige</strong>, was weit weg von der Mitte geschieht, auf −24 dB, die Verstärkung auf 0 dB, die Mitte auf 1 kHz und die Breite auf eine Oktave. Wie klingt die Musik jetzt, und woran erinnert sie dich?
</div>

Ein EQ rechnet in Dezibel, aber was er mit dem Klang tut, ist eine Multiplikation: Jede Frequenz wird mit einem Faktor multipliziert.

_task(eq-factor)

<!-- after: eq-factor -->
Aus \\(G\\) Dezibel wird der Faktor \\(10^{G / 20}\\) für die Höhe der Schwingung, 20, nicht 10 wie bei der Energie, denn die Energie ist das Quadrat der Höhe. Für den Regler im Widget ist die Verstärkung eine Glocke über den Oktaven: In der Mitte wirkt sie ganz, `width` Oktaven daneben noch zur Hälfte, weiter weg immer weniger. Ist `d` der Abstand von der Mitte in Oktaven, gibt der Regler

$$G(d) = G \cdot 2^{-(d / \text{width})^2}$$

Dezibel dazu. Lege `eq.py` an:

<!-- page-code: own -->
```python
import numpy as np

from wav import read_wav, write_wav


def bell(freqs, centre, width, gain):
    # hier: für jede Frequenz der Faktor, mit dem das EQ ihre Schwingung multipliziert


def equalize(x, rate, centre, width, gain):
    X = np.fft.rfft(x)
    freqs = np.fft.rfftfreq(len(x), 1 / rate)
    return np.fft.irfft(X * bell(freqs, centre, width, gain), len(x))


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('musik.wav')
    save('musik-bass.wav', equalize(x, rate, 60, 1, 12), rate)
    save('musik-ohne-hihat.wav', equalize(x, rate, 10000, 2, -30), rate)
```

`equalize` macht mit der FFT von numpy, was du in »Audio komprimieren« selbst gebaut hast: `np.fft.rfft` zerlegt die ganzen acht Sekunden auf einmal in ihre Frequenzen, nur die erste Hälfte, bis zur halben Abtastrate, und `np.fft.rfftfreq` gibt die Frequenz zu jeder Zahl. Jede Zahl wird mit dem Faktor ihrer Frequenz multipliziert, und `np.fft.irfft` macht daraus wieder Samples. `save` macht leiser, was lauter als 1 geworden ist, statt es abzuschneiden.

<div class='hint own' data-check='eq-bell'>
<strong>Jetzt bist du dran:</strong> Schreib <code>bell(freqs, centre, width, gain)</code>: <code>freqs</code> ist ein numpy-Array von Frequenzen in Hz, <code>centre</code> die Mitte in Hz, <code>width</code> die Breite in Oktaven und <code>gain</code> die Verstärkung in dB. Gib für jede Frequenz den Faktor für die Schwingung zurück.
</div>

<!-- hint -->
Drei Schritte: der Abstand in Oktaven, daraus die Dezibel nach der Formel oben, daraus der Faktor. Wie viele Oktaven `f` von `centre` entfernt ist, sagt `np.log2(f / centre)`: Bei der doppelten Frequenz ist es 1, bei der halben −1.
<!-- /hint -->
<!-- hint -->
Die erste Frequenz der FFT ist 0 Hz, und `np.log2(0)` ist minus unendlich. `np.maximum(freqs, 1)` macht aus 0 erst 1:

```python
octaves = np.log2(np.maximum(freqs, 1) / centre)
db = gain * 2 ** (-(octaves / width) ** 2)
```
<!-- /hint -->

<!-- solution -->
```python
def bell(freqs, centre, width, gain):
    octaves = np.log2(np.maximum(freqs, 1) / centre)
    db = gain * 2 ** (-(octaves / width) ** 2)
    return 10 ** (db / 20)
```
<!-- /solution -->

_task(eq-bell)

```bash
python3 eq.py
```

<!-- tutorial-step
write-file: effects/eq.py <- file:stufen/eq.py
check-task: eq-bell
terminal-run: cd /workspace/effects && python3 eq.py
terminal-wait-for-prompt: 60
wait-for-text: musik-ohne-hihat.wav
-->

```text
musik-bass.wav: lauteste Stelle 2.27
musik-ohne-hihat.wav: lauteste Stelle 0.80
```

Hör dir beide Dateien an und vergleich sie mit `musik.wav`. Beim Bass wäre die lauteste Stelle mehr als doppelt so hoch, wie eine WAV-Datei fassen kann: Die Kick-Drum und der Bass bekommen bis zum Vierfachen. `save` macht deshalb das Ganze leiser, und neben dem lauten Bass klingt der Rest etwas leiser als vorher. Ohne die Hi-Hat fehlt das Zischen oben; die anderen Instrumente bleiben, wie sie waren, denn sie haben bei 10.000 Hz kaum etwas.
<!-- /after -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Mach die Stimme, <code>stimme.wav</code>, verständlicher oder unverständlicher: Gib zwischen 2.000 und 4.000 Hz einige dB dazu oder nimm dort viel weg. Woran hörst du den Unterschied zuerst? Bau dann wie im Widget einen Bandpass: Gib <code>bell</code> einen fünften Parameter <code>rest=0</code>, die Dezibel weit weg von der Mitte, und rechne <code>db = rest + (gain - rest) * 2 ** (-(octaves / width) ** 2)</code>. Mit <code>rest=-24</code> und <code>gain=0</code> bleibt nur der Bereich um <code>centre</code>.
</div>

## Echo

Ruft man in den Bergen, kommt der Ruf von einer Felswand zurück, leiser und etwas später. Ein **Echo** im Tonstudio macht dasselbe: Es zählt zum Klang eine Kopie dazu, die um eine feste Zeit verzögert ist. Kommt auch jedes Echo selbst wieder als Echo zurück, immer leiser, heißt das **Rückkopplung**. Mit der Verzögerung \\(D\\) in Samples und der Rückkopplung \\(g\\) ist

$$y\_n = x\_n + g \cdot y\_{n - D}$$

Jedes Sample bekommt das, was \\(D\\) Samples vorher herauskam, \\(g\\)-mal so hoch dazu. Die **Mischung** sagt, wie viel von den Echos zum Klang kommt.

<div class='effect-widget' data-effect='echo'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Hör dir die Stimme mit 0,3 Sekunden Verzögerung an und dreh die Rückkopplung langsam hoch: Wie viele Echos hörst du, bevor sie verschwinden? Mach die Verzögerung dann immer kürzer, bis unter 0,1 Sekunden: Wann hörst du keine einzelnen Echos mehr, sondern einen Raum? Probier dasselbe mit dem Schlagzeug.
</div>

_task(echo-height)

Lege `echo.py` an:

<!-- page-code: own -->
```python
import numpy as np

from wav import read_wav, write_wav


def echo(x, rate, delay, feedback, mix):
    # hier: der Klang mit seinen Echos


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    # zwei Sekunden Stille dahinter, damit die Echos ausklingen
    x = np.concatenate([x, np.zeros(2 * rate)])
    save('stimme-echo.wav', echo(x, rate, 0.3, 0.5, 0.6), rate)
    save('stimme-halle.wav', echo(x, rate, 0.05, 0.7, 0.5), rate)
```

<div class='hint own' data-check='echo'>
<strong>Jetzt bist du dran:</strong> Schreib <code>echo(x, rate, delay, feedback, mix)</code>: <code>delay</code> ist die Verzögerung in Sekunden. Rechne <code>y</code> nach der Formel oben, Sample für Sample, und gib <code>x + mix * (y - x)</code> zurück. <code>x</code> selbst soll sich dabei nicht ändern.
</div>

<!-- hint -->
Die Verzögerung in Samples ist `D = round(delay * rate)`. Fang mit einer Kopie an, `y = x.copy()`; dann ist `y` schon \\(x\_n\\), und es fehlt nur noch das Echo.
<!-- /hint -->
<!-- hint -->
Eine Schleife ab dem ersten Sample, das ein Echo bekommt:

```python
for n in range(D, len(y)):
    y[n] += …
```

Nimm dabei `y[n - D]`, nicht `x[n - D]`: Erst so kommt auch jedes Echo als Echo zurück.
<!-- /hint -->

<!-- solution -->
```python
def echo(x, rate, delay, feedback, mix):
    D = round(delay * rate)
    y = x.copy()
    for n in range(D, len(y)):
        y[n] += feedback * y[n - D]
    return x + mix * (y - x)
```
<!-- /solution -->

_task(echo)

```bash
python3 echo.py
```

<!-- tutorial-step
write-file: effects/echo.py <- file:stufen/echo.py
check-task: echo
terminal-run: cd /workspace/effects && python3 echo.py
terminal-wait-for-prompt: 60
wait-for-text: stimme-halle.wav
-->

Hör dir `stimme-echo.wav` und `stimme-halle.wav` an. Mit 0,3 Sekunden hörst du jedes Echo für sich, mit 0,05 Sekunden verschwimmen sie zu einem Klang wie in einem gekachelten Bad. Zwei Klänge hört das Ohr erst als zwei, wenn zwischen ihnen mehr als einige Zehntelsekunden liegen; was schneller folgt, hört es als einen.

Lange bevor Rechner Klänge bearbeiteten, machte man Echos mit Tonband: Ein Tonkopf nahm den Klang auf, ein zweiter ein Stück weiter spielte ihn wieder ab. Die Verzögerung ergab sich aus dem Abstand der beiden Köpfe und der Geschwindigkeit des Bands, die Rückkopplung daraus, wie viel von der Wiedergabe wieder aufgenommen wurde. Ein solches Bandecho hörst du in »Let It Be« von den Beatles: Phil Spector mischte das Lied 1970 für das Album neu und gab Ringo Starrs Hi-Hat in der zweiten Strophe so viel Echo, dass jeder Schlag mehrmals zurückkommt. Starr gefiel das nicht, und 2003 erschien auf »Let It Be... Naked« eine Fassung ohne Spectors Zutaten, mit einer trockenen Hi-Hat.

<div class='hint try'>
<strong>Ausprobieren:</strong> Hör dir die zweite Strophe in beiden Fassungen an, auf YouTube: <a href='https://www.youtube.com/watch?v=QDYfEBY9NM4'>die Fassung des Albums</a> und <a href='https://www.youtube.com/watch?v=lcA-qlMP11s'>die Fassung von »Naked«</a>. Achte auf die Hi-Hat, sobald das Schlagzeug einsetzt: Wie oft hörst du jeden Schlag? Schätze, wie lang die Verzögerung ist, und stell sie im Widget oben ein.
</div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Setz die Rückkopplung auf 1 oder höher. Was passiert mit der lautesten Stelle, und warum?
</div>

## Chorus

Singen zwei Menschen dieselbe Melodie, klingt es nie wie einer, der lauter singt: Der eine ist immer ein klein wenig früher oder später, und das ändert sich ständig. Ein **Chorus** macht das mit einem einzigen Klang nach. Er zählt eine Kopie dazu, die etwa 15 Millisekunden hinterherläuft, und diese Verzögerung wandert langsam hin und her: um die **Tiefe**, mit dem **Tempo** einer Sinusschwingung. Zu kurz für ein Echo, das man hört; was man hört, ist ein Klang, der breiter wird.

<div class='effect-widget' data-effect='chorus'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Hör dir die Melodie mit einer Tiefe von 0 an: Wie klingt sie, nur mit einer Kopie 15 ms dahinter? Dreh dann die Tiefe langsam auf und hör, wann die Melodie zu schweben beginnt. Mit einem hohen Tempo und viel Tiefe singt sie falsch: Woran hörst du das?
</div>

Die Verzögerung ist fast nie eine ganze Zahl von Samples: Bei 44.100 Samples in der Sekunde sind 15,3 Millisekunden 674,73 Samples. Die Kopie muss also zwischen zwei Samples gelesen werden, so wie H.264 einen Block um einen Viertelpixel verschiebt: Zwischen zwei Samples liegt eine Gerade, und auf ihr liest man ab. An der Stelle 2,25 sind das drei Viertel von Sample 2 und ein Viertel von Sample 3.

Lege `chorus.py` an:

<!-- page-code: own -->
```python
import numpy as np

from wav import read_wav, write_wav


def read(x, pos):
    # hier: die Werte von x an den Stellen pos, zwischen zwei Samples gemischt


def chorus(x, rate, depth, speed, mix):
    n = np.arange(len(x))
    delay = (0.015 + depth * (1 + np.sin(2 * np.pi * speed * n / rate)) / 2) * rate
    pos = n - delay
    wet = np.where(pos >= 0, read(x, np.maximum(pos, 0)), 0)
    return x + mix * wet


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-chorus.wav', chorus(x, rate, 0.004, 1, 0.7), rate)
```

`chorus` rechnet für jedes Sample, wie weit die Kopie gerade hinterherläuft, in Samples, und liest sie an der Stelle `n - delay`. Vor dem ersten Sample gibt es noch keine Kopie: Dort nimmt `np.where` 0.

<div class='hint own' data-check='chorus-read'>
<strong>Jetzt bist du dran:</strong> Schreib <code>read(x, pos)</code>: <code>pos</code> ist ein Array von Stellen mit Nachkommastellen, alle zwischen 0 und <code>len(x) - 1</code>. Gib für jede Stelle den Wert zwischen den beiden Samples daneben zurück, auf der Geraden zwischen ihnen.
</div>

<!-- hint -->
Für jede Stelle das Sample davor, `i = np.floor(pos).astype(int)`, und wie weit die Stelle dahinter liegt, `f = pos - i`, eine Zahl von 0 bis unter 1. Der Wert ist dann `(1 - f) * x[i] + f * x[i + 1]`.
<!-- /hint -->
<!-- hint -->
Steht eine Stelle genau auf dem letzten Sample, gibt es kein `x[i + 1]`. Mit `np.clip(…, 0, len(x) - 2)` wird `i` höchstens das vorletzte, und `f` ist dann 1:

```python
i = np.clip(np.floor(pos).astype(int), 0, len(x) - 2)
f = pos - i
```
<!-- /hint -->

<!-- solution -->
```python
def read(x, pos):
    i = np.clip(np.floor(pos).astype(int), 0, len(x) - 2)
    f = pos - i
    return (1 - f) * x[i] + f * x[i + 1]
```
<!-- /solution -->

_task(chorus-read)

```bash
python3 chorus.py
```

<!-- tutorial-step
write-file: effects/chorus.py <- file:stufen/chorus.py
check-task: chorus-read
terminal-run: cd /workspace/effects && python3 chorus.py
terminal-wait-for-prompt: 60
wait-for-text: stimme-chorus.wav
-->

Hör dir `stimme-chorus.wav` an: eine Stimme, die klingt wie zwei, die nicht ganz zusammen sprechen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere in <code>chorus</code> die 15 Millisekunden, <code>0.015</code>, in eine Millisekunde, <code>0.001</code>, und gib mehr Mischung. Jetzt rauscht die Stimme wie ein startendes Flugzeug: Das ist ein **Flanger**. Er heißt so, weil man ihn früher mit zwei Tonbändern machte und eines am Rand der Spule, englisch *flange*, mit dem Finger bremste.
</div>

## Hall

In einem Raum kommt ein Klang nicht nur einmal zurück, sondern von jeder Wand, von der Decke und vom Boden, und jedes Echo wird wieder zurückgeworfen. Nach kurzer Zeit sind es so viele, dass man keines mehr einzeln hört, nur noch ein Nachklingen, das leiser wird: **Hall**. Wie lange er dauert, misst man als **Nachhallzeit**: die Zeit, bis er um 60 dB leiser ist, ein Tausendstel der Höhe. Ein Wohnzimmer hat etwa eine halbe Sekunde, eine große Kirche mehrere.

<div class='effect-widget' data-effect='reverb'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Hör dir das Schlagzeug in einem kleinen Raum an, Raumgröße ½ und 0,5 Sekunden Nachhall, dann in einer Kirche, 2 und 4 Sekunden. Achte auf die Snare zwischen zwei Schlägen: Wann hörst du noch den vorigen Schlag nachklingen? Mach den Nachhall dann sehr lang und die Raumgröße klein: Was klingt jetzt metallisch?
</div>

Manfred Schroeder von den Bell Labs fand 1962 einen Hall, den ein Rechner schnell genug ausrechnen kann. Er nimmt vier Echos mit Rückkopplung nebeneinander, mit Verzögerungen von 29,7 bis 43,7 Millisekunden, die zueinander nicht passen, so dass ihre Echos sich nie wiederholen. Jedes ist dein `echo` mit der Mischung 1, mit einer Rückkopplung, die es in der Nachhallzeit um 60 dB leiser macht. Danach kommen zwei **Allpässe**: Sie lassen jede Frequenz gleich laut durch, verschmieren aber jedes Echo zu vielen kleinen, so dass der Hall dicht wird. Ein Allpass rechnet

$$y\_n = -g \cdot x\_n + x\_{n - D} + g \cdot y\_{n - D}$$

Lege `reverb.py` an. Es nimmt `echo` aus `echo.py`:

<!-- page-code: own -->
```python
import numpy as np

from echo import echo
from wav import read_wav, write_wav

COMBS = [0.0297, 0.0371, 0.0411, 0.0437]


def allpass(x, rate, delay, g):
    # hier: der Allpass, Sample für Sample


def reverb(x, rate, size, time, mix):
    wet = np.zeros_like(x)
    for delay in COMBS:
        D = round(delay * size * rate)
        g = 10 ** (-3 * D / rate / time)
        wet += echo(x, rate, delay * size, g, 1) / len(COMBS)
    for delay in (0.005, 0.0017):
        wet = allpass(wet, rate, delay, 0.7)
    return x + mix * wet


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    x = np.concatenate([x, np.zeros(3 * rate)])
    save('stimme-raum.wav', reverb(x, rate, 1, 1.5, 0.4), rate)
    save('stimme-kirche.wav', reverb(x, rate, 2, 4, 0.6), rate)
```

<div class='hint own' data-check='allpass'>
<strong>Jetzt bist du dran:</strong> Schreib <code>allpass(x, rate, delay, g)</code> nach der Formel oben: <code>delay</code> in Sekunden, für die Samples vor <code>D</code> nur <code>-g * x[n]</code>.
</div>

<!-- hint -->
Wie bei `echo`: `D = round(delay * rate)` und eine Schleife. Fang mit `y = np.zeros_like(x)` an, einem Array aus Nullen so lang wie `x`, und rechne `y[n]` für jedes `n` aus.
<!-- /hint -->
<!-- hint -->
```python
for n in range(len(x)):
    y[n] = -g * x[n]
    if n >= D:
        y[n] += …
```
<!-- /hint -->

<!-- solution -->
```python
def allpass(x, rate, delay, g):
    D = round(delay * rate)
    y = np.zeros_like(x)
    for n in range(len(x)):
        y[n] = -g * x[n]
        if n >= D:
            y[n] += x[n - D] + g * y[n - D]
    return y
```
<!-- /solution -->

_task(allpass)

```bash
python3 reverb.py
```

<!-- tutorial-step
write-file: effects/reverb.py <- file:stufen/reverb.py
check-task: allpass
terminal-run: cd /workspace/effects && python3 reverb.py
terminal-wait-for-prompt: 120
wait-for-text: stimme-kirche.wav
-->

Hör dir `stimme-raum.wav` und `stimme-kirche.wav` an. In der Kirche verschwimmen die Wörter: Der Hall eines Worts ist noch laut, wenn das nächste beginnt. Darum sind Hörsäle und Klassenräume gedämpft, und eine Kirche ist für Orgel und Chor gebaut, nicht für Vorträge.

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass die beiden Allpässe weg: Wie klingt der Hall nur aus den vier Echos? Und was passiert mit nur einem Echo statt vier?
</div>

## Compressor

Im Radio klingt ein Lied gleichmäßig laut, auch wo jemand leise singt, und ein Podcast ist in der Bahn gut zu verstehen, ohne dass ein Lachen in den Ohren schmerzt. Dahinter steckt ein **Compressor**, wie man ihn auch im deutschen Tonstudio nennt: Er macht laute Stellen leiser und lässt leise, wie sie sind. Was über dem **Threshold** liegt, der Schwelle, staucht er nach der **Ratio**: Bei 4 : 1 kommt von 12 dB über dem Threshold nur ein Viertel heraus, 3 dB. Danach ist alles höchstens so laut wie vorher, und man kann es als Ganzes wieder lauter machen, mit dem **Make-up Gain**. Jetzt sind die leisen Stellen lauter als vorher, die lauten aber nicht.

Wie laut der Klang gerade ist, sagt ein einzelnes Sample nicht, denn eine Schwingung geht ständig durch null. Der Compressor folgt deshalb dem Betrag der Samples mit Verzögerung, dem **Pegel**:

$$p\_n = c \cdot p\_{n-1} + (1 - c) \cdot |x\_n|$$

\\(c\\) liegt knapp unter 1: Je näher an 1, desto langsamer folgt der Pegel. Wird der Klang lauter, nimmt der Compressor ein \\(c\\) für die Zeit **Attack**, wird er leiser, eins für die Zeit **Release**.

<div class='effect-widget' data-effect='compressor'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Hör dir das Schlagzeug bei 20 : 1 an und zieh den Threshold langsam von 0 dB herunter bis −40 dB. Achte auf den Anschlag jeder Snare, den ersten Moment des Schlags: Wann wird er stumpf? Stell dann die Attack auf 30 ms: Was kommt zurück, und warum? Schau dabei rechts im Bild, wie viele dB der Compressor wegnimmt, und auf die Welle darunter.
</div>

_task(compressor-level)

Lege `compressor.py` an:

<!-- page-code: own -->
```python
import math

import numpy as np

from wav import read_wav, write_wav


def gain_db(level, threshold, ratio):
    # hier: wie viele dB der Compressor bei diesem Pegel dazugibt (0 oder weniger)


def compress(x, rate, threshold, ratio, attack, release, makeup):
    a = math.exp(-1 / (attack * rate))
    r = math.exp(-1 / (release * rate))
    level = 0
    y = np.zeros_like(x)
    for n, v in enumerate(np.abs(x).tolist()):
        c = a if v > level else r
        level = c * level + (1 - c) * v
        db = 20 * math.log10(max(level, 1e-9))
        y[n] = x[n] * 10 ** ((gain_db(db, threshold, ratio) + makeup) / 20)
    return y


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-komprimiert.wav', compress(x, rate, -30, 4, 0.005, 0.1, 15), rate)
```

`compress` geht einmal über alle Samples, denn jeder Pegel braucht den vorigen. `math.exp` macht aus der Zeit `attack` in Sekunden das \\(c\\): Nach dieser Zeit ist der Pegel knapp zwei Drittel des Wegs zum neuen Betrag gegangen. Der Pegel in Dezibel geht an `gain_db`, und das Sample wird mit dem Faktor multipliziert, der zu `gain_db` und dem Make-up Gain `makeup` gehört.

<div class='hint own' data-check='gain-db'>
<strong>Jetzt bist du dran:</strong> Schreib <code>gain_db(level, threshold, ratio)</code>: Liegt der Pegel <code>level</code> in dB höchstens beim Threshold <code>threshold</code>, gibt der Compressor 0 dB dazu. Liegt er darüber, gib zurück, um wie viele dB der Compressor ihn leiser macht, als negative Zahl.
</div>

<!-- hint -->
Liegt der Pegel 12 dB über dem Threshold und die Ratio ist 4, bleiben 12 / 4 = 3 dB über dem Threshold. Der Compressor nimmt also 9 dB weg: `gain_db` gibt −9 zurück.
<!-- /hint -->
<!-- hint -->
Von dem, was über dem Threshold liegt, `level - threshold`, bleibt der Teil `1 / ratio`; weggenommen wird der Teil `1 - 1 / ratio`.
<!-- /hint -->

<!-- solution -->
```python
def gain_db(level, threshold, ratio):
    if level <= threshold:
        return 0
    return (threshold - level) * (1 - 1 / ratio)
```
<!-- /solution -->

_task(gain-db)

```bash
python3 compressor.py
```

<!-- tutorial-step
write-file: effects/compressor.py <- file:stufen/compressor.py
check-task: gain-db
terminal-run: cd /workspace/effects && python3 compressor.py
terminal-wait-for-prompt: 120
wait-for-text: stimme-komprimiert.wav
-->

Hör dir `stimme-komprimiert.wav` an und dann noch einmal `stimme.wav`: In der komprimierten Fassung sind die leisen Silben etwa 5 dB lauter geworden, die lauten etwa 1 dB leiser. Lagen zwischen ihnen vorher fast 20 dB, sind es jetzt etwa 13. Ein Compressor macht aber alles Leise lauter, auch ein Rauschen oder ein Atmen: Er kann nicht wissen, welche leisen Klänge man hören will.

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell im Widget die Musik ein, die Attack auf 1 ms, die Release auf 20 ms und den Threshold tief. Jetzt klingt der Bass verzerrt: Der Pegel folgt so schnell, dass der Compressor die einzelnen Schwingungen des Basses leiser und lauter macht und ihre Form verändert. Wie lang muss die Release sein, bis die Verzerrung verschwindet?
</div>

