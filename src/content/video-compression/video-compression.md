<div class='meta'>
image: teaser.webp
image_alt: "Ein Bild aus dem Film Spring: ein Mädchen mit Wanderstab und ein kleiner Hund auf einem Pfad zwischen Felsen im Abendlicht, dahinter ein Berg. Darüber liegen Quadrate, die Blöcke eines Codecs: große im ruhigen Himmel und am unscharfen Berg, kleine an Felsen, Gras und den beiden Figuren."
kind: labor
language: Python
duration: 9 Doppelstunden
curriculum: "Oberstufe: Algorithmen und Datenstrukturen (Diskrete Kosinustransformation, Bewegungsschätzung), Vertiefung Technische Informatik (Videokompression)"
topics: [kompression, codierung]
requires: [jpeg, audio-compression]
reference: [bits-und-bytes]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Video komprimieren

<p class='abstract'>
Eine Sekunde Video in Full HD braucht unkomprimiert über 150 MB, ein Stream nur ein paar Hundert Kilobyte. Du liest Videos Bild für Bild in Python, sparst an der Farbe, sagst Blöcke aus ihren Nachbarn und aus dem vorigen Bild voraus, schätzt Bewegung und vergleichst, was H.264, AV1 und die Codecs von heute daraus machen.
</p>

## Bilder pro Sekunde

Ein Video ist eine Folge von Bildern, die schnell genug nacheinander kommen, dass das Auge eine Bewegung sieht. Jedes Bild hat Pixel, jedes Pixel drei Zahlen für Rot, Grün und Blau. Das ist viel: Ein Bild in Full HD hat 1.920 × 1.080 Pixel mit je 3 Bytes, über 6 MB, und davon kommen 25 in jeder Sekunde.

_task(full-hd)

### Das Pferd in Bewegung

Berührt ein galoppierendes Pferd immer mit einem Huf den Boden? Das wollte Leland Stanford wissen, früher Gouverneur von Kalifornien und Besitzer von Rennpferden. Der Fotograf Eadweard Muybridge stellte dafür im Juni 1878 auf Stanfords Rennbahn in Palo Alto zwölf Kameras nebeneinander auf. Unter der Bahn lagen Drähte, und das Pferd riss im Vorbeilaufen einen nach dem anderen ab; jeder Draht löste eine Kamera aus. Auf den Bildern sieht man: Für einen Moment schwebt das Pferd, alle vier Hufe in der Luft, und zwar mit den Beinen unter dem Körper, nicht weit nach vorn und hinten gestreckt, wie Maler es bis dahin gemalt hatten.

<figure><img class='full fit-width' src='/video/muybridge.jpg' alt='Die Tafel »The Horse in Motion« von 1878: zwölf Fotos in drei Reihen zu vier, ein Reiter auf einem galoppierenden Pferd vor einer Wand mit Linien und Zahlen. Auf den Bildern 2 und 3 hat das Pferd die Beine unter dem Körper und keinen Huf am Boden. Das zwölfte Bild zeigt das Pferd im Stehen. Darunter der Titel und die Angaben: die Stute Sallie Gardner, Leland Stanford, Palo Alto, 19. Juni 1878.'><figcaption>»The Horse in Motion«, Eadweard Muybridge, 1878 (gemeinfrei, <a href='https://commons.wikimedia.org/wiki/File:The_Horse_in_Motion_high_res.jpg'>Wikimedia Commons</a>, verkleinert)</figcaption></figure>

Unter der Tafel steht, dass die Fotos etwa eine fünfundzwanzigstel Sekunde auseinander liegen: 25 Bilder pro Sekunde, so viele wie später beim europäischen Fernsehen. Die ersten elf Fotos zeigen einen Galoppsprung. Ausgeschnitten und nacheinander gezeigt, werden sie zu einem Film. Muybridge tat das ab 1880 mit einer Scheibe, auf die er die Bilder malte und die er drehte. Das machst du gleich selbst, mit Python.

### Einrichten

Lege einen Ordner an und hol dir die Tafel mit Muybridges Fotos:

```bash_copy
mkdir -p ~/video && cd ~/video
wget ${web_root}/video/muybridge.jpg
```

<!-- tutorial-step
terminal-open
terminal-run: mkdir -p /workspace/video && cd /workspace/video && (test -f muybridge.jpg || curl -sO http://workspace_nginx_1/video/muybridge.jpg) && ls
terminal-wait-for-prompt: 60
wait-for-text: muybridge.jpg
-->

### Bilder in Python

Python liest kein Video, aber ffmpeg kann es: Es packt die Bilder aus und schreibt die Pixel, ein Byte nach dem anderen, in eine **Pipe**, aus der Python liest. Umgekehrt nimmt ffmpeg Pixel aus einer Pipe und macht daraus ein Video. Lege `video.py` an:

```python
import subprocess

import numpy as np

WIDTH = 960
HEIGHT = 400


def read_frames(path):
    command = ['ffmpeg', '-v', 'error', '-i', path, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-']
    data = subprocess.run(command, capture_output=True, check=True).stdout
    return np.frombuffer(data, dtype=np.uint8).reshape(-1, HEIGHT, WIDTH, 3)


def write_video(path, frames, fps=24):
    count, height, width, _ = frames.shape
    command = ['ffmpeg', '-v', 'error', '-y', '-f', 'rawvideo', '-pix_fmt', 'rgb24',
               '-s', f'{width}x{height}', '-r', str(fps), '-i', '-',
               '-c:v', 'libx264', '-crf', '18', '-pix_fmt', 'yuv420p', path]
    subprocess.run(command, input=frames.astype(np.uint8).tobytes(), check=True)
```

`read_frames` startet ffmpeg mit `-f rawvideo -pix_fmt rgb24`: keine Datei, nur Pixel, drei Bytes je Pixel, und `-` heißt, in die Pipe statt in eine Datei. `subprocess.run` wartet, bis ffmpeg fertig ist, und gibt in `stdout` alle Bytes. `reshape(-1, HEIGHT, WIDTH, 3)` macht daraus ein Array mit vier Achsen: Bild, Zeile, Spalte, Farbe. `-1` heißt, numpy soll die Zahl der Bilder selbst ausrechnen. `WIDTH` und `HEIGHT` passen zu den Videos, die nach dem Pferd kommen: Sie haben 960 × 400 Pixel. `frames[10, 200, 480]` ist dann das Pixel in der Mitte des elften Bildes, drei Zahlen von 0 bis 255.

`write_video` geht den Weg zurück: ffmpeg bekommt die Größe der Bilder (`-s`) und die Bilder pro Sekunde (`-r`), liest die Pixel aus der Pipe (`-i -`) und schreibt mit H.264 eine MP4-Datei. Was `-crf 18` und `yuv420p` heißen, kommt in den nächsten Kapiteln.

### Ein Film aus elf Fotos

Lege `horse.py` an:

```python
import numpy as np
from PIL import Image

from video import write_video

plate = np.asarray(Image.open('muybridge.jpg').convert('L'))
frames = []
for k in range(11):
    row, column = divmod(k, 4)
    y = 40 + 218 * row
    x = 36 + 335 * column
    frames.append(plate[y:y + 200, x:x + 316])
frames = np.stack(frames)
rgb = np.stack([frames, frames, frames], axis=-1)
write_video('horse.mp4', np.tile(rgb, (10, 1, 1, 1)), fps=25)
```

`Image.open(…).convert('L')` liest die Tafel als Graustufen, ein Byte je Pixel, und `np.asarray` macht ein Array mit Zeilen und Spalten daraus. Die Fotos liegen in einem Raster: `divmod(k, 4)` gibt für Foto `k` die Reihe und die Spalte, und alle 218 Zeilen und 335 Spalten beginnt ein neues Foto. `plate[y:y + 200, x:x + 316]` schneidet eines aus, 200 Zeilen hoch und 316 Spalten breit. `np.stack` legt die elf Fotos zu einem Array mit drei Achsen übereinander, und noch einmal `np.stack` macht aus dem Grau drei gleiche Farben, Rot, Grün und Blau. `np.tile` wiederholt den Galoppsprung zehnmal.

```bash
python3 horse.py
```

<!-- tutorial-step
write-file: video/video.py <- file:stufen/video.py
write-file: video/horse.py <- file:stufen/horse.py
terminal-run: cd /workspace/video && python3 horse.py && ls -l horse.mp4
terminal-wait-for-prompt: 60
wait-for-text: horse.mp4
-->

Öffne `horse.mp4`: Das Pferd galoppiert, so schnell wie 1878 in Palo Alto.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere <code>fps</code> auf 12 und auf 50. Bei welcher Zahl sieht der Galopp für dich echt aus? Und was passiert, wenn du mit <code>range(12)</code> auch das zwölfte Foto mitnimmst?
</div>

### Vier Ausschnitte aus Spring

Muybridges Pferd hat elf Bilder in Grau. Für den Rest der Seite gibt es Videos von heute, Ausschnitte aus **Spring**, einem Film der Blender Foundation von 2019, Regie Andy Goralczyk: © Blender Foundation | [cloud.blender.org/spring](https://studio.blender.org/films/spring/), unter der Lizenz [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.de). Es sind vier Stücke von je vier Sekunden, verkleinert auf 960 × 400 Pixel, 24 Bilder in der Sekunde. Gemacht sind sie aus der Fassung, die Blender auf YouTube zeigt und die auf Wikimedia Commons liegt: Der Film ist also schon einmal komprimiert, mit 2.048 Pixeln Breite. Verkleinert auf weniger als die Hälfte fällt davon kaum noch etwas auf. Hol sie dir:

```bash_copy
wget ${web_root}/video/glance.mp4 ${web_root}/video/pan.mp4 ${web_root}/video/slope.mp4 ${web_root}/video/tilt.mp4
```

<!-- tutorial-step
terminal-run: cd /workspace/video && for c in glance pan slope tilt; do test -f $c.mp4 || curl -sO http://workspace_nginx_1/video/$c.mp4; done; ls *.mp4
terminal-wait-for-prompt: 60
wait-for-text: pan.mp4
-->

<script src='/include/video_in_view.js'></script>
<div class='gallery columns-2'>
<figure><video src='/video/glance.mp4' poster='/video/glance.webp' data-play-in-view muted loop playsinline preload='none' role='img' aria-label='Eine Nahaufnahme des Mädchens mit dunklen Haaren, großen grünen Augen und einem Wanderstab; sie schaut besorgt zur Seite, nur ihre Augen bewegen sich ein wenig.'></video><figcaption><code>glance.mp4</code>: Das Mädchen schaut zur Seite; fast nur ihre Augen bewegen sich.</figcaption></figure>
<figure><video src='/video/pan.mp4' poster='/video/pan.webp' data-play-in-view muted loop playsinline preload='none' role='img' aria-label='Die Kamera gleitet langsam an einem verschneiten Bergkamm entlang, vor ziehenden Wolken.'></video><figcaption><code>pan.mp4</code>: Die Kamera gleitet langsam an einem Bergkamm entlang.</figcaption></figure>
<figure><video src='/video/slope.mp4' poster='/video/slope.webp' data-play-in-view muted loop playsinline preload='none' role='img' aria-label='Ein Hang im Abendlicht mit Felsen und Gras; das Mädchen mit Wanderstab und ihr kleiner Hund gehen einen Pfad entlang.'></video><figcaption><code>slope.mp4</code>: Ein Hang im Abendlicht, das Mädchen und ihr Hund gehen vorbei.</figcaption></figure>
<figure><video src='/video/tilt.mp4' poster='/video/tilt.webp' data-play-in-view muted loop playsinline preload='none' role='img' aria-label='Die Kamera fährt schnell eine Felswand mit gelben Pflanzen hinauf.'></video><figcaption><code>tilt.mp4</code>: Die Kamera fährt schnell eine Felswand hinauf.</figcaption></figure>
</div>

Öffne die Dateien auch im Explorer: VS Code spielt sie ab.

### Weniger Bilder

Wie viele Bilder pro Sekunde braucht es? Beim Ton bestimmte die Abtastrate, welche Frequenzen ankommen; beim Video bestimmt die Zahl der Bilder, wie flüssig eine Bewegung aussieht. Stummfilme liefen in vielen Kinos mit 22 bis 26 Bildern pro Sekunde, je nach Kino und Vorführer. Mit dem Tonfilm ging das nicht mehr, denn der Ton musste in derselben Geschwindigkeit laufen: Zwischen 1927 und 1930 einigte man sich auf 24 Bilder pro Sekunde, und dabei ist es im Kino bis heute geblieben.

Lege `frames.py` an. `held` soll zeigen, wie ein Video mit weniger Bildern aussieht, aber gleich lang bleibt: Es nimmt nur jedes `step`-te Bild und zeigt jedes davon `step`-mal.

<!-- page-code: own -->
```python
import numpy as np
from PIL import Image

from video import read_frames, write_video


def held(frames, step):
    # hier: jedes step-te Bild, jedes step-mal


if __name__ == '__main__':
    frames = read_frames('pan.mp4')
    print(frames.shape, frames.nbytes, 'Bytes')
    write_video('pan-6.mp4', held(frames, 4))
```

<div class='hint own' data-check='held'>
<strong>Jetzt bist du dran:</strong> Schreib <code>held(frames, step)</code>: ein Array mit genauso vielen Bildern wie <code>frames</code>, in dem die Bilder 0, <code>step</code>, <code>2 * step</code>, … je <code>step</code>-mal hintereinander stehen. Bei 24 Bildern pro Sekunde und <code>step = 4</code> sieht der Schwenk aus wie mit 6 Bildern pro Sekunde.
</div>

<!-- hint -->
`frames[::step]` sind die Bilder 0, `step`, `2 * step`, …, wie bei den Samples in »Audio komprimieren«. `np.repeat(a, n, axis=0)` wiederholt jedes Element entlang der ersten Achse `n`-mal.
<!-- /hint -->
<!-- hint -->
Ist die Zahl der Bilder kein Vielfaches von `step`, ist das Ergebnis von `np.repeat` etwas zu lang: Schneid es mit `[:len(frames)]` ab.

```python
return np.repeat(frames[::step], step, axis=0)[:len(frames)]
```
<!-- /hint -->

<!-- solution -->
```python
def held(frames, step):
    return np.repeat(frames[::step], step, axis=0)[:len(frames)]
```
<!-- /solution -->

_task(held)

```bash
python3 frames.py
```

<!-- tutorial-step
write-file: video/video.py <- file:stufen/video.py
write-file: video/frames.py <- file:stufen/frames-1.py
check-task: held
terminal-run: python3 frames.py
terminal-wait-for-prompt: 60
wait-for-text: 110592000 Bytes
-->

96 Bilder, je 400 Zeilen mit 960 Pixeln und 3 Farben: 110.592.000 Bytes für vier Sekunden, als MP4 sind es 1,3 MB. Öffne `pan-6.mp4` und `pan.mp4` nacheinander: Mit 6 Bildern pro Sekunde ruckelt der Schwenk, die Wolken springen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Versuch <code>step</code> 2 und 3, also 12 und 8 Bilder pro Sekunde. Ab wann sieht der Schwenk für dich flüssig aus? Mach dasselbe mit <code>glance.mp4</code>: Warum fällt es dort viel weniger auf?
</div>

### Halbbilder

Das Fernsehen hatte ein anderes Problem. Ein Bild nach dem anderen zu übertragen, hätte zu viel Bandbreite gebraucht, und mit nur 25 Bildern in der Sekunde flackerte die Röhre. Darum sendete es in **Halbbildern**: erst alle geraden Zeilen, dann alle ungeraden, 50-mal in der Sekunde, im Takt des Stromnetzes mit 50 Hz. Die beiden Halbbilder eines Bildes sind aber zu verschiedenen Zeiten aufgenommen, eine fünfzigstel Sekunde auseinander. Das heißt auf Englisch **Interlacing**.

<div class='hint own' data-check='interlace'>
<strong>Jetzt bist du dran:</strong> Schreib <code>interlace(frames)</code>: Bild <code>k</code> des Ergebnisses hat die geraden Zeilen (0, 2, 4, …) von Bild <code>k</code> und die ungeraden (1, 3, 5, …) von Bild <code>k + 1</code>. Das Ergebnis hat ein Bild weniger als <code>frames</code>, und <code>frames</code> selbst bleibt unverändert.
</div>

<!-- hint -->
Fang mit einer Kopie aller Bilder außer dem letzten an, `out = frames[:-1].copy()`: Darin stehen die geraden Zeilen schon richtig.
<!-- /hint -->
<!-- hint -->
Die ungeraden Zeilen aller Bilder sind `out[:, 1::2]`; sie kommen aus den ungeraden Zeilen der Bilder 1 bis zum letzten, `frames[1:, 1::2]`.
<!-- /hint -->

<!-- solution -->
```python
def interlace(frames):
    out = frames[:-1].copy()
    out[:, 1::2] = frames[1:, 1::2]
    return out
```
<!-- /solution -->

_task(interlace)

Unten ins Programm kommt, was die Halbbilder der schnellen Fahrt zeigt. Von einem davon speichert es einen Ausschnitt, die oberen 200 Zeilen und die Spalten 300 bis 699, doppelt so groß und mit scharfen Pixeln (`Image.NEAREST`):

```python
~     write_video('pan-6.mp4', held(frames, 4))
    fast = read_frames('tilt.mp4')
    combed = interlace(fast)
    Image.fromarray(combed[48, :200, 300:700]).resize((800, 400), Image.NEAREST).save('combing.png')
    write_video('tilt-interlaced.mp4', combed)
```

_task(combing)

<!-- after: combing -->
<!-- tutorial-step
write-file: video/frames.py <- file:stufen/frames-2.py
check-task: interlace
terminal-run: python3 frames.py && ls -l tilt*.mp4
terminal-wait-for-prompt: 60
wait-for-text: tilt-interlaced.mp4
-->

Öffne `combing.png`: Wo sich etwas bewegt, sind die Kanten in feine Streifen zerfranst, jede zweite Zeile ein Stück verschoben. Das sieht aus wie die Zinken eines Kamms und heißt darum **Combing** (englisch *comb*, der Kamm). Wo das Bild still steht, fällt nichts auf.

<!-- tutorial-screenshot
image-file: video/combing.png 1
-->
<img class='full fit-width' src='combing.webp' alt='Ein vergrößerter Ausschnitt aus einem Halbbild der Fahrt die Felswand hinauf: Die gelben Pflanzen an der Wand sind in waagerechte Streifen zerfranst, jede zweite Zeile ein Stück nach oben verschoben.'>

Und noch etwas zeigt `ls -l`: `tilt-interlaced.mp4` ist größer als `tilt.mp4`, obwohl es kein Bild mehr hat. Die feinen Streifen sind für einen Codec schwer zu speichern. Streams im Internet senden ganze Bilder; Halbbilder gibt es noch im Fernsehen, etwa bei Sendern mit 1080i, dem i für interlaced.
<!-- /after -->

<!-- stage: stufen/frames-2.py as frames.py -->

## Farbe sparen

Das Auge sieht Unterschiede in der Helligkeit viel feiner als Unterschiede in der Farbe. Darauf baut jeder Codec für Video: Er trennt zuerst die Helligkeit von der Farbe und speichert die Farbe gröber.

Die Idee ist älter als jeder Codec. Als das Farbfernsehen kam, sollten die alten Schwarz-Weiß-Fernseher weiter ein Bild zeigen. Darum blieb das Signal für die Helligkeit, wie es war, und die Farbe kam als zwei zusätzliche Signale dazu, mit viel weniger Bandbreite. In den USA sendete man so ab 1954 nach der Norm NTSC. In der Bundesrepublik begann das Farbfernsehen am 25. August 1967 mit PAL, das Walter Bruch bei Telefunken in Hannover entwickelt hatte.

### Helligkeit und Farbe trennen

Statt Rot, Grün und Blau speichert ein Codec drei andere Zahlen: **Y** ist die Helligkeit, auf Englisch **Luma**. **Cb** sagt, wie viel blauer das Pixel ist als ein Grau derselben Helligkeit, und **Cr**, wie viel röter; zusammen heißen die beiden **Chroma**. Die Formeln sind dieselben wie bei JPEG:

$$Y = 0{,}299\,R + 0{,}587\,G + 0{,}114\,B$$

$$Cb = 128 - 0{,}168736\,R - 0{,}331264\,G + 0{,}5\,B$$

$$Cr = 128 + 0{,}5\,R - 0{,}418688\,G - 0{,}081312\,B$$

Grün zählt für die Helligkeit am meisten, weil das Auge für Grün am empfindlichsten ist, Blau am wenigsten. Bei einem Grau sind R, G und B gleich: Dann ist Y genau dieser Wert, und Cb und Cr sind 128, denn die Zahlen vor R, G und B ergeben in beiden Zeilen zusammen 0. Fernsehen in HD rechnet mit etwas anderen Gewichten (die Norm BT.709) und lässt Y nur von 16 bis 235 laufen; die Idee ist dieselbe. Wie die drei Teile zusammen ein Foto ergeben, zeigt <a href='/colorspace' target='_blank' rel='noopener'>Trennung von Helligkeit und Farbe</a>.

Für dieses Kapitel gibt es ein einzelnes Bild als PNG, von derselben Stelle des Films wie `slope.mp4`. Warum kein Bild aus `slope.mp4`, steht am Ende des Kapitels.

```bash_copy
wget ${web_root}/video/slope.png
```

Lege `chroma.py` an:

<!-- page-code: own -->
```python
import numpy as np
from PIL import Image


def to_ycbcr(rgb):
    # hier: Y, Cb und Cr aus R, G und B


def save_grey(path, plane):
    Image.fromarray(plane.round().clip(0, 255).astype(np.uint8)).save(path)


if __name__ == '__main__':
    frame = np.asarray(Image.open('slope.png').convert('RGB'))
    ycbcr = to_ycbcr(frame)
    save_grey('y.png', ycbcr[..., 0])
    save_grey('cb.png', ycbcr[..., 1])
    save_grey('cr.png', ycbcr[..., 2])
```

`save_grey` speichert eine Ebene als Graubild: runden, auf 0 bis 255 begrenzen, in Bytes umwandeln.

<div class='hint own' data-check='ycbcr'>
<strong>Jetzt bist du dran:</strong> Schreib <code>to_ycbcr(rgb)</code>. Es bekommt ein Array, dessen letzte Achse R, G und B sind, für ein Pixel, ein Bild oder ein ganzes Video, und gibt ein Array derselben Form zurück, dessen letzte Achse Y, Cb und Cr sind.
</div>

<!-- hint -->
`rgb[..., 0]` sind alle Werte für Rot, egal wie viele Achsen davor stehen: Die drei Punkte heißen »alle Achsen davor«. Mit `r`, `g` und `b` rechnest du die Formeln oben für alle Pixel auf einmal.
<!-- /hint -->
<!-- hint -->
`np.stack([y, cb, cr], axis=-1)` legt die drei wieder zusammen, als neue letzte Achse.

```python
r, g, b = rgb[..., 0], rgb[..., 1], rgb[..., 2]
y = 0.299 * r + 0.587 * g + 0.114 * b
```
<!-- /hint -->

<!-- solution -->
```python
def to_ycbcr(rgb):
    r, g, b = rgb[..., 0], rgb[..., 1], rgb[..., 2]
    y = 0.299 * r + 0.587 * g + 0.114 * b
    cb = 128 - 0.168736 * r - 0.331264 * g + 0.5 * b
    cr = 128 + 0.5 * r - 0.418688 * g - 0.081312 * b
    return np.stack([y, cb, cr], axis=-1)
```
<!-- /solution -->

_task(ycbcr)

Das Mädchen in `slope.png` trägt eine rote Jacke. Bevor du das Programm startest:

_task(jacket)

```bash
python3 chroma.py
```

<!-- tutorial-step
terminal-run: cd /workspace/video && (test -f slope.png || curl -sO http://workspace_nginx_1/video/slope.png) && ls -l slope.png
terminal-wait-for-prompt: 30
wait-for-text: slope.png
-->

<!-- after: jacket -->
<!-- tutorial-step
write-file: video/chroma.py <- file:stufen/chroma-1.py
check-task: ycbcr
terminal-run: python3 chroma.py && ls *.png
terminal-wait-for-prompt: 60
wait-for-text: y.png
-->

Öffne `y.png`, `cb.png` und `cr.png`:

<div class='gallery'>
<figure>
<!-- tutorial-screenshot
image-file: video/y.png 1
-->
<img class='inspect' src='plane-y.webp' alt='Das Bild vom Hang in Graustufen: das Mädchen, der Hund, die Felsen und das Gras mit allen Einzelheiten, wie ein Schwarz-Weiß-Foto.'>
<figcaption>Y: die Helligkeit</figcaption></figure>
<figure>
<!-- tutorial-screenshot
image-file: video/cb.png 1
-->
<img class='inspect' src='plane-cb.webp' alt='Dasselbe Bild fast überall in einem mittleren Grau, mit weichen Umrissen. Das Gras in der Sonne und der Weg sind dunkler; vom Mädchen sieht man den Umriss, die Jacke etwas dunkler als das Grau.'>
<figcaption>Cb: blauer oder weniger blau als Grau</figcaption></figure>
<figure>
<!-- tutorial-screenshot
image-file: video/cr.png 1
-->
<img class='inspect' src='plane-cr.webp' alt='Wieder fast überall mittleres Grau. Hell heraus sticht die Jacke des Mädchens, etwas heller sind die sonnigen Stellen; die Felsen im Schatten sind etwas dunkler.'>
<figcaption>Cr: röter oder weniger rot als Grau</figcaption></figure>
</div>

`y.png` sieht aus wie ein Schwarz-Weiß-Foto, mit allen Einzelheiten. `cb.png` und `cr.png` sind fast überall ein mittleres Grau nahe 128, denn die meisten Farben in der Natur sind nicht sehr bunt. Die Jacke ist in Cr deutlich heller, im Mittel 162, weil sie viel röter ist als ein Grau derselben Helligkeit, und in Cb etwas dunkler, im Mittel 105, weil sie wenig Blau hat. Am dunkelsten in Cb ist das Gras in der Abendsonne: Gelb hat kaum Blau.
<!-- /after -->

### Chroma Subsampling

Wenn das Auge die Farbe nicht so scharf sieht, genügt sie gröber: für jedes Quadrat aus 2 × 2 Pixeln ein Cb und ein Cr, der Mittelwert aus vier. Y bleibt für jedes Pixel. Das heißt **Chroma Subsampling**, und so, wie es fast jedes Video macht, **4:2:0**. Die Zahlen kommen aus der Fernsehtechnik: In einem Streifen aus 4 Pixeln Breite und 2 Zeilen hat die erste Zeile 2 Werte für die Farbe und die zweite 0 neue. Bei 4:4:4 bleibt alles, bei 4:2:2 wird die Farbe nur halb so breit.

<img class='drawing d-none d-lg-block' style='width: 60em' src='subsampling-wide.svg' alt='Drei Streifen aus 4 × 2 Pixeln. In jedem Pixel ein kleines Quadrat für Y. Bei 4:4:4 hat jedes Pixel einen eigenen Punkt für Cb und Cr, 4 neue Farbwerte in jeder Zeile, 3 Bytes je Pixel. Bei 4:2:2 teilen sich je zwei Pixel nebeneinander einen Punkt, 2 neue Farbwerte in jeder Zeile, 2 Bytes je Pixel. Bei 4:2:0 teilen sich je vier Pixel in einem Quadrat einen Punkt in ihrer Mitte: 2 neue Farbwerte in der ersten Zeile, 0 in der zweiten, 1,5 Bytes je Pixel.'>
<img class='drawing d-lg-none' style='width: 27.5em' src='subsampling.svg' alt='Drei Streifen untereinander aus 4 × 2 Pixeln. In jedem Pixel ein kleines Quadrat für Y. Bei 4:4:4 hat jedes Pixel einen eigenen Punkt für Cb und Cr, 4 neue Farbwerte in jeder Zeile, 3 Bytes je Pixel. Bei 4:2:2 teilen sich je zwei Pixel nebeneinander einen Punkt, 2 neue Farbwerte in jeder Zeile, 2 Bytes je Pixel. Bei 4:2:0 teilen sich je vier Pixel in einem Quadrat einen Punkt in ihrer Mitte: 2 neue Farbwerte in der ersten Zeile, 0 in der zweiten, 1,5 Bytes je Pixel.'>

_task(bytes-420)

Erweitere `chroma.py`. `subsample(plane, n)` soll eine Ebene gröber machen, `upsample` macht sie wieder so groß wie vorher, indem es jeden Wert `n`-mal nach unten und `n`-mal nach rechts wiederholt. `from_ycbcr` rechnet zurück nach R, G und B, mit den Formeln aus <a href='/jpeg' target='_blank' rel='noopener'>der JPEG-Aktivität</a>, und `save_detail` speichert einen Ausschnitt um das Mädchen, dreimal so groß:

<!-- page-code: own -->
```python
def from_ycbcr(ycbcr):
    y, cb, cr = ycbcr[..., 0], ycbcr[..., 1] - 128, ycbcr[..., 2] - 128
    r = y + 1.402 * cr
    g = y - 0.344136 * cb - 0.714136 * cr
    b = y + 1.772 * cb
    return np.stack([r, g, b], axis=-1).round().clip(0, 255).astype(np.uint8)


def subsample(plane, n=2):
    # hier: der Mittelwert jedes Quadrats aus n × n Werten


def upsample(plane, n=2):
    return np.repeat(np.repeat(plane, n, axis=0), n, axis=1)


def save_detail(path, rgb):
    Image.fromarray(rgb[130:310, 460:700]).resize((720, 540), Image.NEAREST).save(path)
```

Und unten im Hauptprogramm, nach den drei Graubildern:

```python
    small_chroma = ycbcr.copy()
    small_chroma[..., 1] = upsample(subsample(ycbcr[..., 1]))
    small_chroma[..., 2] = upsample(subsample(ycbcr[..., 2]))
    small_luma = ycbcr.copy()
    small_luma[..., 0] = upsample(subsample(ycbcr[..., 0]))
    save_detail('detail.png', frame)
    save_detail('detail-chroma.png', from_ycbcr(small_chroma))
    save_detail('detail-luma.png', from_ycbcr(small_luma))
```

`small_chroma` hat die Farbe gröber, `small_luma` zum Vergleich die Helligkeit.

<div class='hint own' data-check='subsample'>
<strong>Jetzt bist du dran:</strong> Schreib <code>subsample(plane, n)</code>: Es bekommt eine Ebene mit zwei Achsen, deren Höhe und Breite Vielfache von <code>n</code> sind, und gibt eine Ebene zurück, die <code>n</code>-mal kleiner ist, in jeder Richtung. Jeder Wert ist der Mittelwert eines Quadrats aus <code>n</code> × <code>n</code> Werten.
</div>

<!-- hint -->
Nur jeden <code>n</code>-ten Wert zu nehmen, `plane[::n, ::n]`, wäre einfacher, aber dann bestimmt ein Pixel allein die Farbe von vier. Für den Mittelwert hilft `reshape`: Eine Ebene der Höhe `height` und Breite `width` lässt sich als Array mit vier Achsen lesen, `plane.reshape(height // n, n, width // n, n)`. Die zweite Achse ist dann die Zeile innerhalb eines Quadrats, die vierte die Spalte.
<!-- /hint -->
<!-- hint -->
`mean(axis=(1, 3))` mittelt über die Zeilen und Spalten innerhalb jedes Quadrats und lässt die beiden anderen Achsen stehen.

```python
height, width = plane.shape
```
<!-- /hint -->

<!-- solution -->
```python
def subsample(plane, n=2):
    height, width = plane.shape
    return plane.reshape(height // n, n, width // n, n).mean(axis=(1, 3))
```
<!-- /solution -->

_task(subsample)

```bash
python3 chroma.py
```

<!-- tutorial-step
write-file: video/chroma.py <- file:stufen/chroma-2.py
check-task: subsample
terminal-run: python3 chroma.py && ls detail*
terminal-wait-for-prompt: 60
wait-for-text: detail-luma.png
-->

Öffne die drei Ausschnitte:

<div class='gallery columns-3'>
<figure>
<!-- tutorial-screenshot
image-file: video/detail.png 1
-->
<img src='detail.webp' alt='Ein vergrößerter Ausschnitt: das Mädchen mit Wanderstab und roter Jacke, vor ihr der Hund, unten das Gras in der Sonne, scharf bis in die Halme.'>
<figcaption>Das Original</figcaption></figure>
<figure>
<!-- tutorial-screenshot
image-file: video/detail-chroma.png 1
-->
<img src='detail-chroma.webp' alt='Derselbe Ausschnitt mit Cb und Cr in Quadraten aus 2 × 2 gemittelt. Er sieht aus wie das Original, ein Unterschied ist nicht zu erkennen.'>
<figcaption>Farbe gröber: Cb und Cr in 2 × 2 gemittelt</figcaption></figure>
<figure>
<!-- tutorial-screenshot
image-file: video/detail-luma.png 1
-->
<img src='detail-luma.webp' alt='Derselbe Ausschnitt mit Y in Quadraten aus 2 × 2 gemittelt. Er ist deutlich unschärfer, mit treppigen Kanten am Gesicht, am Stab und am Hund.'>
<figcaption>Helligkeit gröber: Y in 2 × 2 gemittelt</figcaption></figure>
</div>

Mit gröberer Farbe sieht der Ausschnitt aus wie das Original. Mit gröberer Helligkeit ist er sichtbar unscharf, obwohl er sogar mehr Zahlen behält: Y für jedes vierte Pixel und Cb und Cr für jedes Pixel sind 96.000 + 2 · 384.000 = 864.000 Werte, bei 4:2:0 sind es 576.000.

<div class='hint try'>
<strong>Ausprobieren:</strong> Mach die Farbe noch gröber: <code>upsample(subsample(ycbcr[..., 1], 4), 4)</code>, ebenso für Cr, und dann mit 8. Ab wann siehst du einen Unterschied? Achte auf die Ränder der roten Jacke und auf den Stab davor.
</div>

### Schon gespart

Warum `slope.png` und kein Bild aus `slope.mp4`? In `write_video` steht `-pix_fmt yuv420p`: Jede MP4-Datei dieser Seite ist schon 4:2:0, wie fast jedes Video im Internet, auf Blu-ray und im Fernsehen. Ein Bild aus `slope.mp4` hat Cb und Cr also schon gemittelt, und `subsample` fände nichts mehr zu sparen. `slope.png` ist auch aus dem Film von YouTube gemacht, der seine Farbe in 4:2:0 hat, aber für 2.048 Pixel Breite: 1.024 Werte je Zeile. Verkleinert auf 960 Pixel bekommt fast jedes Pixel wieder eine eigene Farbe.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib im Hauptprogramm aus, wie weit <code>small_chroma</code> im Mittel vom Original abweicht: <code>print(np.abs(from_ycbcr(small_chroma).astype(int) - frame).mean())</code>. Dann nimm statt <code>slope.png</code> das erste Bild aus <code>slope.mp4</code>, <code>read_frames('slope.mp4')[0]</code> mit <code>from video import read_frames</code>. Wie ändert sich die Zahl?
</div>

<!-- stage: stufen/chroma-2.py as chroma.py -->
