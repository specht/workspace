<div class='meta'>
kind: reference
search: numpy, Array, ndarray, Slice, Broadcasting, dtype, uint8, FFT
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# numpy: Rechnen mit ganzen Arrays

<p class='abstract'>
numpy rechnet in Python mit ganzen Reihen von Zahlen auf einmal: mit den Samples eines Tons, den Pixeln eines Bilds, den Bildern eines Videos. Statt einer Schleife über eine Million Zahlen steht eine Zeile, und sie läuft oft zehn- bis zwanzigmal so schnell. Im Workspace ist numpy installiert. Hier steht, was <a href='/audio-compression'>Audio komprimieren</a>, <a href='/audio-effects'>Audioeffekte</a>, <a href='/video-compression'>Video komprimieren</a>, <a href='/fraktale-kompression'>Fraktale Bildkompression</a> und <a href='/gaussian-splatting'>Gaussian Splatting</a> davon brauchen.
</p>

```python
import numpy as np
```

## Arrays

<!-- part: anlegen -->
### Ein Array anlegen

Ein **Array** ist eine Reihe von Zahlen, alle vom selben Typ. Anders als eine Liste kann es mehrere Achsen haben: ein Ton eine, ein Graustufenbild zwei (Zeile, Spalte), ein Farbbild drei (Zeile, Spalte, Farbe).

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Ausdruck</th><th>Ergebnis</th></tr>
</thead>
<tbody>
<tr><td><code>np.array([3, 1, 4])</code></td><td><code>[3 1 4]</code>, aus einer Liste</td></tr>
<tr><td><code>np.zeros(4)</code></td><td><code>[0. 0. 0. 0.]</code>, lauter Nullen</td></tr>
<tr><td><code>np.full(3, 7)</code></td><td><code>[7 7 7]</code>, lauter Siebenen</td></tr>
<tr><td><code>np.arange(5)</code></td><td><code>[0 1 2 3 4]</code>, wie <code>range</code></td></tr>
<tr><td><code>np.arange(0, 1, 0.25)</code></td><td><code>[0. 0.25 0.5 0.75]</code>, auch mit Kommazahlen</td></tr>
<tr><td><code>np.linspace(0, 1, 5)</code></td><td><code>[0. 0.25 0.5 0.75 1.]</code>, 5 Zahlen in gleichen Abständen, das Ende mitgezählt</td></tr>
<tr><td><code>np.zeros((400, 960, 3))</code></td><td>ein schwarzes Bild: 400 Zeilen, 960 Spalten, 3 Farben</td></tr>
<tr><td><code>np.zeros_like(x)</code></td><td>Nullen in der Form und dem Typ von <code>x</code></td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: form-typ -->
### Form und Typ

```python
bild = np.zeros((400, 960, 3), dtype=np.uint8)
print(bild.shape, bild.ndim, bild.dtype, bild.size)
```

```text
(400, 960, 3) 3 uint8 1152000
```

`shape` ist die Länge jeder Achse, `ndim` die Zahl der Achsen, `size` die Zahl aller Werte. `dtype` ist der Typ jedes Werts: `float64` für Kommazahlen, `int64` für ganze Zahlen, `uint8` für Bytes von 0 bis 255, wie in Bildern, `int16` für Samples mit 16 Bit, wie in WAV-Dateien. `x.astype(np.uint8)` macht ein Array eines anderen Typs daraus.

Ein `uint8` hat nur 256 Werte. Was darüber hinausgeht, beginnt wieder bei 0:

```python
a = np.array([200, 100], dtype=np.uint8)
print(a[0] + a[1])
print(a.astype(int).sum())
```

```text
44
300
```

Darum rechnet man mit Pixeln in `int` oder `float` und macht erst am Ende wieder Bytes daraus, gerundet und auf 0 bis 255 begrenzt:

```python
print(np.clip(np.round(np.array([-20.4, 99.6, 300.0])), 0, 255).astype(np.uint8))
```

```text
[  0 100 255]
```
<!-- /part -->

## Auf Teile zugreifen

<!-- part: index -->
### Index und Slice

Wie bei einer Liste zählt der Index ab 0, ein negativer von hinten; `a:b` ist ein **Slice**, von `a` bis vor `b`, `::2` jeder zweite Wert.

```python
x = np.array([10, 20, 30, 40, 50])
print(x[0], x[-1], x[1:3], x[::2], x[::-1])
```

```text
10 50 [20 30] [10 30 50] [50 40 30 20 10]
```

Mit mehreren Achsen steht für jede Achse ein Index, durch Kommas getrennt; `:` heißt die ganze Achse.

```python
m = np.arange(12).reshape(3, 4)
print(m[1, 2], m[1], m[:, 0], m[0:2, 1:3])
```

```text
6 [4 5 6 7] [0 4 8] [[1 2]
 [5 6]]
```

`m[1, 2]` ist Zeile 1, Spalte 2; `m[:, 0]` die erste Spalte. In einem Bild ist das `bild[y, x]`: zuerst die Zeile, dann die Spalte. `bild[:, :, 0]` ist der Rotanteil aller Pixel, kurz `bild[..., 0]`. Wer einem Slice etwas zuweist, ändert das Array: `bild[:, :, 0] = 255` macht alles rot.
<!-- /part -->

<!-- part: bedingungen -->
### Mit Bedingungen

```python
x = np.array([-2, 5, -1, 8])
print(x > 0, x[x > 0], np.where(x > 0, x, 0), np.clip(x, 0, 6), np.maximum(x, 0))
```

```text
[False  True False  True] [5 8] [0 5 0 8] [0 5 0 6] [0 5 0 8]
```

`x > 0` vergleicht jeden Wert und gibt ein Array aus `True` und `False`; `x[x > 0]` behält nur die Werte, bei denen es `True` ist. `np.where(bedingung, a, b)` nimmt für jeden Wert `a`, wo die Bedingung gilt, sonst `b`. `np.clip(x, 0, 6)` begrenzt auf 0 bis 6, `np.maximum(x, 0)` nimmt für jeden Wert den größeren von beiden.
<!-- /part -->

## Rechnen

<!-- part: rechnen -->
### Mit ganzen Arrays

Jede Rechnung gilt für jeden Wert, ohne Schleife; zwei Arrays derselben Form werden Wert für Wert verrechnet.

```python
x = np.array([1.0, 2.0, 3.0])
print(x * 2, x + x, x ** 2, np.sqrt(x))
t = np.arange(4) / 4
print(np.sin(2 * np.pi * t).round(3))
```

```text
[2. 4. 6.] [2. 4. 6.] [1. 4. 9.] [1.         1.41421356 1.73205081]
[ 0.  1.  0. -1.]
```

`np.sin`, `np.cos`, `np.exp`, `np.log2`, `np.log10`, `np.abs` und `np.sqrt` rechnen ebenso für jeden Wert. Die zweite Zeile ist eine Sinusschwingung mit 4 Samples je Schwingung: So entsteht ein Ton aus Zahlen.

Passen die Formen nicht genau, dehnt numpy die kleinere aus, wo sie eine Achse weniger hat oder eine Achse der Länge 1. Das heißt **Broadcasting**:

```python
m = np.arange(6).reshape(2, 3)
print(m * np.array([1, 10, 100]))
print(m - m.mean())
```

```text
[[  0  10 200]
 [  3  40 500]]
[[-2.5 -1.5 -0.5]
 [ 0.5  1.5  2.5]]
```

Die Zeile `[1, 10, 100]` wird mit jeder Zeile von `m` multipliziert, und eine einzelne Zahl wie der Mittelwert mit allem.
<!-- /part -->

<!-- part: zusammenfassen -->
### Zusammenfassen

```python
m = np.array([[1, 5, 2], [7, 0, 3]])
print(m.sum(), m.sum(axis=0), m.sum(axis=1), m.max(), m.argmax(), m.mean(axis=1))
```

```text
18 [8 5 5] [ 8 10] 7 3 [2.66666667 3.33333333]
```

Ohne `axis` fasst eine Funktion alles zusammen; mit `axis=0` jede Spalte (über die Zeilen hinweg), mit `axis=1` jede Zeile. `argmax` gibt den Index des größten Werts, gezählt, als wäre das Array eine einzige Reihe. Ebenso: `sum`, `mean`, `max`, `min`, `argmin`, `np.count_nonzero`.
<!-- /part -->

<!-- part: zusammensetzen -->
### Zusammensetzen und umformen

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Ausdruck</th><th>Ergebnis</th></tr>
</thead>
<tbody>
<tr><td><code>np.concatenate([a, b])</code></td><td><code>[1 2 3 4]</code> für <code>a = [1, 2]</code> und <code>b = [3, 4]</code>: hintereinander</td></tr>
<tr><td><code>np.stack([a, b])</code></td><td><code>[[1 2] [3 4]]</code>: übereinander, mit einer neuen Achse</td></tr>
<tr><td><code>np.repeat(a, 3)</code></td><td><code>[1 1 1 2 2 2]</code>: jeden Wert dreimal</td></tr>
<tr><td><code>np.tile(a, 3)</code></td><td><code>[1 2 1 2 1 2]</code>: das Ganze dreimal</td></tr>
<tr><td><code>np.arange(6).reshape(2, 3)</code></td><td><code>[[0 1 2] [3 4 5]]</code>: dieselben Werte in einer anderen Form</td></tr>
<tr><td><code>x.reshape(-1, 2)</code></td><td>so viele Zeilen wie nötig, je 2 Werte; <code>-1</code> rechnet numpy aus</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Bytes, Bilder und Töne

<!-- part: bytes -->
### Aus Bytes und zurück

```python
data = bytes([0, 1, 255, 0, 128, 64])
print(np.frombuffer(data, dtype=np.uint8).reshape(2, 3))
samples = np.frombuffer(bytes([0, 1, 0, 128]), dtype='<i2')
print(samples, samples / 32768)
print(np.array([1, 256], dtype='<i2').tobytes())
```

```text
[[  0   1 255]
 [  0 128  64]]
[   256 -32768] [ 0.0078125 -1.       ]
b'\x01\x00\x00\x01'
```

`np.frombuffer` liest Bytes als Zahlen eines Typs, ohne sie zu kopieren; `'<i2'` heißt ganze Zahlen mit 2 Bytes, Little Endian, wie die Samples einer WAV-Datei (Handbuch, [Bits und Bytes](/reference/bits-und-bytes)). `tobytes()` geht den Weg zurück. Ein Array aus `frombuffer` lässt sich nicht ändern; `.copy()` macht ein eigenes daraus.
<!-- /part -->

<!-- part: bilder -->
### Bilder mit Pillow

```python
from PIL import Image

bild = np.asarray(Image.open('foto.png'))
print(bild.shape, bild.dtype)
Image.fromarray(bild[::2, ::2]).save('klein.png')
```

`np.asarray(Image.open(…))` macht aus einem Bild ein Array mit der Form (Zeilen, Spalten, Farben) und dem Typ `uint8`; `Image.fromarray` macht aus einem solchen Array wieder ein Bild. `bild[::2, ::2]` nimmt jede zweite Zeile und Spalte: halb so breit und halb so hoch. Wie man die Bilder eines Videos als Array bekommt, steht bei [ffmpeg](/reference/ffmpeg).
<!-- /part -->

<!-- part: fft -->
### Frequenzen mit der FFT

```python
rate = 8000
t = np.arange(rate) / rate
ton = np.sin(2 * np.pi * 1000 * t) + 0.5 * np.sin(2 * np.pi * 2500 * t)
spektrum = np.abs(np.fft.rfft(ton))
freq = np.fft.rfftfreq(len(ton), 1 / rate)
print(freq[np.argmax(spektrum)])
zurueck = np.fft.irfft(np.fft.rfft(ton), len(ton))
print(np.allclose(zurueck, ton))
```

```text
1000.0
True
```

`np.fft.rfft` zerlegt Samples in ihre Frequenzen: Aus 8.000 Samples werden 4.001 komplexe Zahlen, eine für jede Frequenz von 0 Hz bis zur halben Abtastrate, und ihr Betrag sagt, wie stark sie ist. `np.fft.rfftfreq` gibt die Frequenz zu jeder dieser Zahlen, hier 0, 1, 2 … Hz. `np.fft.irfft` macht aus den Frequenzen wieder Samples; `np.allclose` vergleicht zwei Arrays bis auf winzige Rundungsfehler.
<!-- /part -->

## Zufall und Dateien

<!-- part: zufall -->
### Zufall

```python
rng = np.random.default_rng(42)
print(rng.integers(1, 7, size=5), rng.uniform(-1, 1, 3))
```

`default_rng(42)` ist ein Zufallsgenerator mit dem Startwert 42: Mit demselben Startwert kommen bei jedem Lauf dieselben Zahlen, praktisch zum Vergleichen. `integers(1, 7, size=5)` würfelt fünfmal, von 1 bis vor 7; `uniform(-1, 1, 3)` gibt 3 Kommazahlen zwischen −1 und 1, etwa für Rauschen.
<!-- /part -->

<!-- part: speichern -->
### Speichern und laden

```python
np.save('feld.npy', x)
x = np.load('feld.npy')
np.savez('alles.npz', bloecke=bloecke, werte=werte)
daten = np.load('alles.npz')
print(daten['bloecke'])
```

`np.save` schreibt ein Array mit Form und Typ in eine Datei, `np.load` liest es wieder; `np.savez` schreibt mehrere unter Namen in eine Datei.
<!-- /part -->

## Zum Nachschlagen

- [NumPy: the absolute basics for beginners](https://numpy.org/doc/stable/user/absolute_beginners.html):
  eine Einführung mit Bildern, auf Englisch.
- [Broadcasting](https://numpy.org/doc/stable/user/basics.broadcasting.html):
  wann numpy Formen ausdehnt und wann nicht.
- [Indexing on ndarrays](https://numpy.org/doc/stable/user/basics.indexing.html):
  alle Arten, auf Teile eines Arrays zuzugreifen.
- [Discrete Fourier Transform (numpy.fft)](https://numpy.org/doc/stable/reference/routines.fft.html):
  die Funktionen der FFT.
- [NumPy Reference](https://numpy.org/doc/stable/reference/index.html):
  jede Funktion mit ihren Parametern.
