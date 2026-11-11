<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# ffmpeg: Ton und Video

<p class='abstract'>
ffmpeg wandelt fast jede Ton- und Videodatei in fast jede andere um, komprimiert sie mit einem Codec deiner Wahl, schneidet, verkleinert und misst. Im Workspace ist es installiert, zusammen mit <code>ffprobe</code>, das zeigt, was in einer Datei steckt. Hier stehen die Befehle aus <a href='/audio-compression'>Audio komprimieren</a> und <a href='/video-compression'>Video komprimieren</a> zum Nachschlagen.
</p>

## Grundform

<!-- part: grundform -->
### Ein Befehl

Jeder Befehl hat dieselbe Form: erst die Eingabe hinter `-i`, dann die Einstellungen für die Ausgabe, zuletzt die Ausgabedatei. Ihre Endung entscheidet, was ffmpeg schreibt.

```bash
ffmpeg -i musik.wav musik.flac
```

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Schalter</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>-i datei</code></td><td>die Eingabe; mehrere <code>-i</code> für mehrere Eingaben</td></tr>
<tr><td><code>-y</code></td><td>überschreibt die Ausgabe, ohne zu fragen</td></tr>
<tr><td><code>-v error</code></td><td>schreibt nur Fehler, nicht die vielen Zeilen über jede Datei (auch <code>-loglevel error</code>)</td></tr>
<tr><td><code>-hide_banner</code></td><td>lässt nur die Zeilen am Anfang über die Version und den Bau weg</td></tr>
</tbody>
</table>
</div>

Die Reihenfolge zählt: Ein Schalter vor `-i` gilt für die Eingabe, einer danach für die Ausgabe.
<!-- /part -->

<!-- part: ffprobe -->
### Was in einer Datei steckt

`ffprobe` liest eine Datei und sagt, was darin ist: Codec, Abtastrate, Kanäle, Größe der Bilder, Länge.

```bash
ffprobe -v error -show_entries format=duration -of csv=p=0 musik.mp3
ffprobe -v error -show_entries stream=codec_name,sample_rate,channels -of default=noprint_wrappers=1 musik.opus
ffprobe -v error -select_streams v -show_entries frame=pict_type,pkt_size -of json video.mp4
```

Der erste Befehl gibt nur die Länge in Sekunden, der zweite Codec, Abtastrate und Kanäle, der dritte für jedes Bild eines Videos seine Art (I, P oder B) und wie viele Bytes es braucht. `-of json` gibt die Antwort als JSON, das ein Programm leicht lesen kann.
<!-- /part -->

<!-- part: codecs -->
### Was ffmpeg kann

ffmpeg zeigt selbst, was es kann. Jede Liste erklärt am Anfang ihre Buchstaben; mit `grep` findest du darin, was du suchst.

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er zeigt</th></tr>
</thead>
<tbody>
<tr><td><code>ffmpeg -hide_banner -codecs</code></td><td>alle Codecs, die ffmpeg kennt, über 500, und welche Encoder und Decoder es für jeden hat</td></tr>
<tr><td><code>ffmpeg -hide_banner -encoders</code></td><td>womit ffmpeg schreiben kann: die Namen für <code>-c:a</code> und <code>-c:v</code></td></tr>
<tr><td><code>ffmpeg -hide_banner -decoders</code></td><td>womit ffmpeg lesen kann</td></tr>
<tr><td><code>ffmpeg -hide_banner -formats</code></td><td>die Dateiformate, etwa <code>mp4</code>, <code>wav</code>, <code>matroska</code>; D heißt lesen, E schreiben</td></tr>
<tr><td><code>ffmpeg -hide_banner -filters</code></td><td>alle Filter für <code>-vf</code> (Bild) und <code>-af</code> (Ton)</td></tr>
<tr><td><code>ffmpeg -hide_banner -pix_fmts</code></td><td>die Pixelformate, etwa <code>rgb24</code> und <code>yuv420p</code></td></tr>
<tr><td><code>ffmpeg -hide_banner -h encoder=libx264</code></td><td>alle Einstellungen eines Encoders; <code>-h filter=scale</code> die eines Filters</td></tr>
</tbody>
</table>
</div>

```bash
ffmpeg -hide_banner -codecs | grep -E ' h264 | opus '
```

```text
 DEV.LS h264    H.264 / AVC / MPEG-4 AVC / MPEG-4 part 10 (decoders: h264 …) (encoders: libx264 …)
 DEAIL. opus    Opus (Opus Interactive Audio Codec) (decoders: opus libopus) (encoders: opus libopus)
```

Die sechs Zeichen vorn: D, ffmpeg kann ihn lesen; E, schreiben; V oder A, Video oder Ton; I, jedes Bild für sich; L, mit Verlust; S, ohne Verlust. Ein **Codec** ist die Art, die Daten zu codieren, etwa H.264; ein **Encoder** ist ein Programm, das sie so schreibt. Für H.264 gibt es mehrere, `libx264` ist das bekannteste, und sein Name ist es, der hinter `-c:v` steht.
<!-- /part -->

## Ton

<!-- part: ton -->
### Umwandeln und komprimieren

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er macht</th></tr>
</thead>
<tbody>
<tr><td><code>ffmpeg -i musik.wav musik.flac</code></td><td>FLAC: verlustfrei kleiner, die Samples kommen genau zurück</td></tr>
<tr><td><code>ffmpeg -i musik.wav -b:a 128k musik.mp3</code></td><td>MP3 mit 128 kbit/s; die Endung wählt den Codec</td></tr>
<tr><td><code>ffmpeg -i musik.wav -c:a aac -b:a 64k musik.m4a</code></td><td>AAC mit 64 kbit/s</td></tr>
<tr><td><code>ffmpeg -i musik.wav -c:a libopus -b:a 32k musik.opus</code></td><td>Opus mit 32 kbit/s</td></tr>
<tr><td><code>ffmpeg -i musik.mp3 musik.wav</code></td><td>zurück in Samples, zum Anhören oder Rechnen</td></tr>
</tbody>
</table>
</div>

`-c:a` wählt den Codec für den Ton (a für audio), `-b:a` die Bitrate. Ohne `-c:a` nimmt ffmpeg den üblichen Codec der Endung. Opus rechnet immer mit 48.000 Samples in der Sekunde: Aus einer Datei mit 44.100 Hz wird beim Zurückwandeln eine mit 48.000 Hz. Eine MP3-Datei ist am Ende etwas länger als das Original, weil der Encoder am Anfang und am Ende Stille dazulegt.
<!-- /part -->

<!-- part: schneiden -->
### Schneiden, Kanäle, Abtastrate

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Schalter</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>-ss 1.5</code></td><td>beginnt bei 1,5 Sekunden (vor <code>-i</code>: springt dorthin, ohne alles davor zu lesen)</td></tr>
<tr><td><code>-t 2</code></td><td>nimmt 2 Sekunden</td></tr>
<tr><td><code>-ac 1</code></td><td>ein Kanal, mono; <code>-ac 2</code> stereo</td></tr>
<tr><td><code>-ar 8000</code></td><td>8.000 Samples in der Sekunde, wie im Telefon</td></tr>
</tbody>
</table>
</div>

```bash
ffmpeg -ss 1 -t 2 -i stimme.wav -ac 1 -ar 8000 stueck.wav
```
<!-- /part -->

## Video

<!-- part: video -->
### Codec und Qualität

```bash
ffmpeg -i video.mp4 -c:v libx264 -crf 23 -pix_fmt yuv420p klein.mp4
```

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Schalter</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>-c:v libx264</code></td><td>H.264 als Codec für das Bild (v für video); <code>libaom-av1</code> für AV1</td></tr>
<tr><td><code>-crf 23</code></td><td>eine feste Qualität: kleiner ist besser und größer, bei H.264 von 0 bis 51, üblich ist 23</td></tr>
<tr><td><code>-b:v 1500k</code></td><td>statt einer Qualität eine Bitrate</td></tr>
<tr><td><code>-preset slow</code></td><td>sucht länger und spart dafür Bytes; von <code>ultrafast</code> bis <code>veryslow</code></td></tr>
<tr><td><code>-g 24</code></td><td>höchstens 24 Bilder bis zum nächsten I-Frame; <code>-g 1</code>: nur I-Frames</td></tr>
<tr><td><code>-bf 0</code></td><td>keine B-Frames</td></tr>
<tr><td><code>-pix_fmt yuv420p</code></td><td>Farbe in 4:2:0, wie fast jedes Video; ohne das spielen manche Programme die Datei nicht ab</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: bilder -->
### Bilder heraus und hinein

```bash
mkdir -p bilder
ffmpeg -i video.mp4 -vf fps=2 bilder/%04d.png
ffmpeg -framerate 24 -i bilder/%04d.png -c:v libx264 -pix_fmt yuv420p neu.mp4
ffmpeg -i video.mp4 -vf "select=eq(n\,10)" -frames:v 1 bild-10.png
```

Die erste Zeile schreibt zwei Bilder je Sekunde, `0001.png`, `0002.png` und so weiter: `%04d` steht für eine Zahl mit vier Stellen. Die zweite macht aus Bildern wieder ein Video mit 24 Bildern in der Sekunde. Die dritte holt genau ein Bild heraus, das mit dem Index 10, das elfte.
<!-- /part -->

<!-- part: filter -->
### Filter

`-vf` bekommt eine Kette von Filtern, durch Kommas getrennt; jeder bekommt das Bild des vorigen.

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Filter</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>scale=640:-2</code></td><td>640 Pixel breit, die Höhe passend und gerade</td></tr>
<tr><td><code>crop=320:180:0:0</code></td><td>schneidet 320 × 180 Pixel aus, links oben beginnend</td></tr>
<tr><td><code>fps=2</code></td><td>zwei Bilder in der Sekunde</td></tr>
<tr><td><code>select=eq(n\,10)</code></td><td>nur das Bild mit dem Index 10</td></tr>
<tr><td><code>noise=alls=14:allf=t</code></td><td>Rauschen, das sich von Bild zu Bild ändert, wie Filmkorn</td></tr>
</tbody>
</table>
</div>

```bash
ffmpeg -i video.mp4 -vf "crop=320:180:0:0,scale=640:-2" ausschnitt.mp4
```
<!-- /part -->

<!-- part: copy -->
### Ohne neu zu codieren

```bash
ffmpeg -ss 1 -t 5 -i video.mp4 -c copy stueck.mp4
```

`-c copy` kopiert die komprimierten Daten, wie sie sind: schnell und ohne Verlust, aber ein Schnitt kann nur an einem I-Frame beginnen, und Filter gehen so nicht.
<!-- /part -->

## Mit Python

<!-- part: pipe -->
### Pixel und Samples durch eine Pipe

ffmpeg kann statt einer Datei rohe Zahlen schreiben, in eine **Pipe**, aus der ein Python-Programm liest, und umgekehrt. `-f rawvideo -pix_fmt rgb24` heißt: nur Pixel, drei Bytes je Pixel; `-` heißt: in die Pipe statt in eine Datei.

```python
import subprocess

import numpy as np

WIDTH, HEIGHT = 640, 360


def read_frames(path):
    command = ['ffmpeg', '-v', 'error', '-i', path, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-']
    data = subprocess.run(command, capture_output=True, check=True).stdout
    return np.frombuffer(data, dtype=np.uint8).reshape(-1, HEIGHT, WIDTH, 3)


def write_video(path, frames, fps=24):
    command = ['ffmpeg', '-v', 'error', '-y', '-f', 'rawvideo', '-pix_fmt', 'rgb24',
               '-s', f'{WIDTH}x{HEIGHT}', '-r', str(fps), '-i', '-',
               '-c:v', 'libx264', '-crf', '18', '-pix_fmt', 'yuv420p', path]
    subprocess.run(command, input=frames.astype(np.uint8).tobytes(), check=True)
```

Beim Lesen weiß ffmpeg die Größe aus der Datei, beim Schreiben nicht: Darum bekommt es `-s` und `-r` vor dem `-i -`. Für Ton geht es genauso: `-f s16le -ac 1 -ar 44100 -` gibt die Samples als Zahlen mit 16 Bit, Little Endian, wie in einer WAV-Datei; `-f f32le` als Kommazahlen.
<!-- /part -->

## Messen

<!-- part: psnr -->
### Wie nah ist das Ergebnis am Original?

```bash
ffmpeg -hide_banner -i klein.mp4 -i video.mp4 -lavfi psnr -f null - 2>&1 | grep -o "PSNR.*"
```

Das PSNR vergleicht die beiden Videos Bild für Bild, in dB: je höher, desto näher. `-f null -` schreibt keine Datei; ffmpeg rechnet nur. Weil ffmpeg seine Meldungen auf `stderr` schreibt, leitet `2>&1` sie in die Pipe zu `grep`.
<!-- /part -->

## Zum Nachschlagen

- [ffmpeg](https://ffmpeg.org/ffmpeg.html): alle Schalter, mit den Regeln für ihre Reihenfolge, auf Englisch.
- [ffmpeg-filters](https://ffmpeg.org/ffmpeg-filters.html): jeder Filter für Bild und Ton mit seinen Einstellungen.
- [ffprobe](https://ffmpeg.org/ffprobe.html): was ffprobe zeigen kann und in welchen Formaten.
- [H.264](https://trac.ffmpeg.org/wiki/Encode/H.264), [AV1](https://trac.ffmpeg.org/wiki/Encode/AV1), [MP3](https://trac.ffmpeg.org/wiki/Encode/MP3) und [AAC](https://trac.ffmpeg.org/wiki/Encode/AAC): Anleitungen im Wiki von ffmpeg, welche Einstellungen für welchen Zweck taugen.
