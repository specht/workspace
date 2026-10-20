<div class='meta'>
image: halle.webp
image_alt: "Eine Halle aus Marmor unter einem hohen Satteldach, zwei Reihen Säulen, ganz hinten im hellen Licht eine Statue der Athena"
kind: projekt
topic: hidden
duration: 2 Doppelstunden
reference: [workspace]
template: https://github.com/specht/backrooms-starter.git
template_name: der Backrooms-Vorlage
</div>

# Die Backrooms

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

<p class='abstract'>
Du bist durch eine Wand gefallen. Dahinter liegen die Backrooms: gelbe Tapete, feuchter Teppich, das Summen der Leuchten, Räume ohne Ende. Du kartierst sie in Textdateien, baust eigene Räume mit Türen, Säulen, schrägen Decken und Statuen und siehst zu, wie dein Browser das Licht berechnet, das zwischen den Wänden hin- und herfällt. Am Rechner, mit Rot-Cyan-Brille oder mit einer VR-Brille.
</p>

<div class='found-note'>
<p>Wenn du das hier liest, bist du durchgefallen. Nicht durch eine Prüfung. Durch die Wirklichkeit. Irgendwo hat eine Wand nicht gehalten, was sie versprochen hat, und jetzt bist du hier.</p>
<p>Keine Panik. Der Teppich ist feucht, aber er trägt. Die Leuchten summen, aber sie leuchten. Und du bist nicht der erste Mensch hier: Wir haben angefangen, alles aufzuschreiben.</p>
<cite>Erste Seite des Feldhandbuchs der Kartografen</cite>
</div>

<table class='table'>
<tr><th>Level</th><td>0, »Die Lobby«</td></tr>
<tr><th>Überlebensschwierigkeit</th><td>Klasse 1: sicher, stabil, keine Entitäten gesichtet</td></tr>
<tr><th>Ausdehnung</th><td>so groß, wie du es baust</td></tr>
<tr><th>Ausgänge</th><td>keine bekannten. Den Tab zu schließen, soll helfen (unbestätigt).</td></tr>
</table>

Die Backrooms sind eine Legende aus dem Internet. 2019 stellte jemand in einem Forum das Foto eines leeren Büroraums mit gelber Tapete ein, und jemand anderes schrieb dazu: Wer an der falschen Stelle aus der Wirklichkeit »noclippt«, landet hier, in endlosen leeren Räumen. Noclip heißt ein Cheat aus alten Ego-Shootern wie Quake: Mit ihm läuft man durch Wände. Seitdem haben unzählige Leute weitere Levels erfunden, Geschichten geschrieben und Videos gedreht.

Hier baust du deine eigenen Backrooms. Du beschreibst Räume in Textdateien: wo die Wände stehen, wo die Leuchten hängen, wo eine Tür in den nächsten Raum führt. Ein Programm im Browser liest sie, baut daraus ein Bauwerk in 3D und berechnet das Licht: wohin es fällt, wo Schatten liegen und wie es von Wänden, Boden und Decke zurückgeworfen wird. Programmieren musst du dafür nicht. Wer will, findet das ganze Programm im Ordner `engine/` und kann darin herumschrauben.

## Ausrüstung

<div class='found-note'>
<p>Was du brauchst: eine Karte (du hast keine), eine Lampe (es gibt hier genug) und einen Ordner, in den du alles schreibst, was du findest. Den Ordner gibt es schon. Er lag hier, als wir ankamen.</p>
<cite>Feldhandbuch, Seite 2</cite>
</div>

### Die Vorlage klonen

Alles, was du brauchst, steckt in der Backrooms-Vorlage:

_include(workspace#repository-klonen)

Danach liegt die Vorlage in `/workspace/backrooms-starter`:

<!-- tutorial-screenshot
# Klonen wie im Handbuch »Workspace« (oben eingebunden), dann die
# geöffnete Vorlage im Explorer, level.ini im Editor
show-left-sidebar
clone-start: https://github.com/specht/backrooms-starter.git @ 6e939829b8dfd2f55137e3bcdf4564fe168c3b5a
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the repository?
click: Open
wait-for-text: level.ini
open-file: level.ini
sleep: 1
-->

<img class='full' src='vorlage.webp' alt='Die geklonte Vorlage im Explorer: die Ordner engine, lobby, rooms, statues und vendor, dazu index.html, level.ini und README.md; im Editor ist level.ini geöffnet.'>

Darin liegen zwei Levels. Deins beginnt klein, in `level.ini` und im Ordner `rooms`: ein Zimmer, ein Gang und zwei farbige Räume. In `lobby` liegt ein ganzes Level zum Ausprobieren; dazu später mehr.

_task(geklont)

### Eintreten

_include(workspace#live-server)

Die Vorschau zeigt `index.html`. Zuerst ist es dunkel, nur feine Linien zeigen die Kanten der Räume. Dann gehen die Leuchten an, eine nach der anderen, die nächsten zuerst, und oben links wächst ein Balken: Das Licht wird berechnet.

<!-- tutorial-screenshot
# Die Vorschau, während das Licht gebacken wird
go-live
tab: preview
viewport: 1536x864
zoom: 1.5
hook: baking
-->

<img class='full' src='backen.webp' alt='Das Zimmer, noch unfertig beleuchtet; oben links der Name des Levels, »Baking light« mit einem Fortschrittsbalken und darunter die Schalter.'>

Wie lange das dauert, hängt von der Grafikkarte deines Rechners ab. Ist es fertig, steht dort »Light baked in …«:

<!-- tutorial-screenshot
# fertig gebacken (die Seite neu, im Bild davor stand sie still); der Kasten
# oben links nur im Bild davor, beim Backen
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open
hook: light
hook: info off
-->

<img class='full' src='zimmer.webp' alt='Das Zimmer im warmen Licht seiner Leuchte, gelbe Tapete, dunkle Sockelleiste, durch die offene Tür sieht man in den Gang.'>

Klick ins Bild, dann siehst du dich mit der Maus um. <kbd>Esc</kbd> gibt die Maus wieder frei.

<table class='table'>
<tr><th>Taste</th><th>Was passiert</th></tr>
<tr><td><kbd>W</kbd> <kbd>A</kbd> <kbd>S</kbd> <kbd>D</kbd></td><td>laufen, mit <kbd>Shift</kbd> rennen</td></tr>
<tr><td><kbd>↑</kbd> <kbd>↓</kbd> <kbd>←</kbd> <kbd>→</kbd></td><td>vor und zurück laufen, nach links und rechts drehen</td></tr>
<tr><td><kbd>M</kbd> oder <kbd>Tab</kbd></td><td>die Automap: das ganze Bauwerk als Drahtgitter</td></tr>
<tr><td><kbd>Shift</kbd> <kbd>R</kbd></td><td>zurück zum Anfang</td></tr>
<tr><td><kbd>P</kbd></td><td>den Kasten oben links aus- und wieder einblenden</td></tr>
</table>

Wo du stehst, merkt sich die Adresse der Seite. Lädst du neu, stehst du wieder dort, solange sich der Grundriss nicht geändert hat.

<div class='hint try'>
<strong>Ausprobieren:</strong> Drück <kbd>M</kbd>. Die Automap zeigt das Bauwerk als Drahtgitter, jeden Raum mit seinem Namen, die Leuchten als gelbe Punkte; der rote Pfeil bist du. Zieh mit der Maus, um die Karte zu drehen, mit <kbd>Shift</kbd>, um sie zu verschieben, und zoom mit dem Mausrad. Lauf ein paar Schritte, während sie offen ist, und schau nach unten: Was macht die Karte? <kbd>V</kbd> zeigt statt des Drahtgitters die Räume selbst von oben, ohne Decken, mit ihrem Licht. <kbd>Esc</kbd> schließt die Karte.
</div>

So sieht die Automap aus, wenn du am Start nach Nordosten und etwas nach unten schaust und mit dem Mausrad ein Stück herauszoomst:

<!-- tutorial-screenshot
# die Automap über dem ganzen Level: vom Start aus nach Nordosten, nach unten geneigt
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=5.00,17.00,-38,-35
hook: light
hook: key m
hook: wheel 450
hook: info off
-->

<img class='full' src='automap.webp' alt='Die Automap über dem ganzen Level: Zimmer, Gang, Blau und Rot als hellblaues Drahtgitter von schräg oben, jeder Raum mit seinem Namen, gelbe Punkte für die Leuchten, ein roter Pfeil mit Ring und Strahl im Zimmer; Linien hinter Wänden sind blass.'>

Und mit <kbd>V</kbd>, die Räume selbst:

<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=5.00,17.00,-38,-35
hook: light
hook: key m v
hook: wheel 450
hook: info off
-->

<img class='full' src='automap-raeume.webp' alt='Dieselbe Automap mit den Räumen von oben, ohne Decken: gelbe Tapete und Teppich im Zimmer und im Gang, der blaue Raum blau, der rote rot, darüber das hellblaue Drahtgitter und die Namen.'>

## Kartografie

<div class='found-note'>
<p>Wir kartieren in Feldern. Ein Feld ist ein Meter. Eine Raute ist eine Wand, ein Punkt ist Teppich. Was nicht auf der Karte steht, gibt es nicht. Das ist die einzige Regel, auf die man sich hier verlassen kann.</p>
<cite>Notiz, mit Kugelschreiber auf die Tapete geschrieben</cite>
</div>

### Ein Raum

Öffne `rooms/zimmer.room`. Das ist der Raum, in dem du aufgewacht bist:

```room
# Ein kleines Zimmer. Du weißt nicht, wie du hierhergekommen bist.

[room]
baseboard = 0.08, #86763c
name = Zimmer
height = 2.6
look = east              # am @ schaust du zur Tür

[map]
#######
#.....#
#.@.L.1
#.....#
#.....#
#######

[L]
light = #ffe7b8
power = 0.9

[1]
door = gang 1
```

Eine Raumdatei besteht aus Abschnitten in eckigen Klammern. In `[room]` steht, wie der Raum aussieht: sein Name, wie hoch die Decke ist (2,6 m), eine dunkle Sockelleiste unten an den Wänden, 8 cm hoch, und wohin du am Anfang schaust. Ein `#` mit einem Leerzeichen davor und danach beginnt einen Kommentar; eine Farbe wie `#86763c` hat kein Leerzeichen dahinter.

In `[map]` steht der Grundriss, von oben gesehen, oben ist Norden. Hier zählt jedes Zeichen, Kommentare gibt es darin nicht:

<table class='table'>
<tr><th>Zeichen</th><th>Bedeutung</th></tr>
<tr><td><code>#</code></td><td>Wand</td></tr>
<tr><td><code>.</code></td><td>Boden</td></tr>
<tr><td><code>@</code></td><td>hier fängst du an, nur einmal im ganzen Level</td></tr>
<tr><td>Leerzeichen</td><td>nichts, außerhalb des Raums</td></tr>
<tr><td>ein Buchstabe</td><td>eine Leuchte, ein Block oder eine Statue; was es ist, steht im Abschnitt mit diesem Buchstaben</td></tr>
<tr><td>eine Ziffer</td><td>eine Tür in einen anderen Raum, beschrieben im Abschnitt mit dieser Ziffer</td></tr>
</table>

`[L]` sagt, dass das `L` eine Leuchte ist, mit warmem, gelblichem Licht. `[1]` macht aus der `1` am rechten Rand eine Tür.

### Türen

Eine Tür ist ein Feld, das zwei Räumen gehört. `door = gang 1` heißt: Diese `1` ist dasselbe Feld wie die `1` in `rooms/gang.room`. Das Programm legt die beiden Räume so nebeneinander, dass die Türen aufeinanderliegen. Ihre Wände dürfen sich dabei überdecken, ihre Böden nicht.

So entsteht aus vielen kleinen Dateien ein Bauwerk. `level.ini` nennt nur den ersten Raum (`start = zimmer`). Das Programm liest ihn, dann jeden Raum, zu dem eine seiner Türen führt, dann deren Nachbarn, bis es keine neuen Türen mehr gibt. Eine Datei, zu der keine Tür führt, liest es gar nicht.

_task(tuer)

## Ein eigener Raum

<div class='found-note'>
<p>Gestern war im Gang neben dem Zimmer eine Wand. Heute ist dort eine Öffnung. Ich habe nachgezählt: Das Feld ist dasselbe. Die Wand ist weg. Dahinter ist eine Kammer, die es gestern nicht gab.</p>
<cite>Seite aus einem durchnässten Notizbuch</cite>
</div>

### Eine Kammer anbauen

Leg im Ordner `rooms` eine neue Datei `kammer.room` an:

```room
# Eine Kammer, die gestern noch nicht da war.

[room]
name = Kammer
height = 2.2
wallpaper = #9a8a4c

[map]
#####
5...#
#.L.#
#...#
#####

[L]
light = #ffb070
power = 0.9
flicker = 1
```

Die Leuchte ist orange und schwächer als die anderen, und mit `flicker = 1` flackert sie ab und zu. Die Tapete ist eine Farbe als Hexadezimalzahl wie in CSS: Rot, Grün und Blau, je zwei Ziffern.

Noch führt keine Tür in die Kammer, also gibt es sie nicht. Öffne `rooms/gang.room`. Weit unten im Grundriss ist links die `1`, die Tür zum Zimmer. Ersetze in dieser Zeile das `#` am rechten Rand durch eine `5`:

```room
~ #...#
1...5
~ #...#
```

und schreib ganz unten in die Datei, wohin sie führt:

```room
[5]
door = kammer 5
```

Speichere beide Dateien und lade die Vorschau neu. Die Kammer liegt östlich vom Gang, gegenüber der Tür zum Zimmer. In `kammer.room` steht kein Abschnitt `[5]`: Es genügt, wenn einer der beiden Räume die Tür beschreibt.

<!-- tutorial-screenshot
# Die Kammer: die Räume wie auf der Seite, dann aus ihrer Ecke zur Tür
write-file: rooms/zimmer.room <- file:stufen/zimmer.room
write-file: rooms/gang.room <- file:stufen/gang.room
write-file: rooms/kammer.room <- file:stufen/kammer.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=16.00,19.00,56,0
hook: light
hook: info off
-->

<img class='full' src='kammer.webp' alt='Die Kammer: niedrig, mit dunklerer Tapete im orangefarbenen Licht ihrer Leuchte; durch die Tür in der Ecke fällt Licht aus dem Gang.'>

Stimmt etwas nicht, steht oben links in Rot, in welcher Datei und in welcher Zeile das Programm hängen geblieben ist, auf Englisch, zum Beispiel `rooms/kammer.room: room kammer overlaps room rot`. Das heißt: Der Raum läge auf dem Boden eines anderen. Schieb die Tür an eine andere Stelle oder mach den Raum kleiner.

## Ein Saal, Schritt für Schritt

<div class='found-note'>
<p>Hinter der Kammer ist noch eine Tür. Dahinter: ein Saal, leer und hoch. Jedes Mal, wenn ich zurückkomme, steht etwas mehr darin.</p>
<cite>Seite aus einem durchnässten Notizbuch, andere Handschrift</cite>
</div>

Hinter der Kammer baust du einen Saal, Schritt für Schritt: erst den Grundriss, dann eine schräge Decke, Säulen, farbiges Licht, eine Statue und Marmor, zum Schluss einen Gang mit schrägem Boden. Nach jedem Schritt speicherst du und lädst die Vorschau neu.

### Der Grundriss

Gib der Kammer in `rooms/kammer.room` eine zweite Tür, an der rechten Wand, in der Zeile der ersten:

```room
~ #####
5...6
~ #.L.#
```

und schreib unten dazu, wohin sie führt:

```room
[6]
door = saal 1
```

Dann leg `rooms/saal.room` an:

```room
# Ein Saal hinter der Kammer.

[room]
name = Saal
height = 3

[map]
#############
#...........#
#...L...L...#
1...........#
#...........#
#...........#
#...L...L...#
#...........#
#############

[L]
light = #fff3cf
```

Ein Saal, 11 m lang und 7 m breit, mit vier Leuchten in zwei Reihen. Weil `[L]` nur einmal beschrieben ist, sind alle vier gleich.

<!-- tutorial-screenshot
write-file: rooms/kammer.room <- file:stufen/kammer-2.room
write-file: rooms/saal.room <- file:stufen/saal-1.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-1.webp' alt='Der leere Saal von seiner Tür aus: gelbe Wände, eine flache Decke mit vier Leuchten in zwei Reihen, Teppich.'>

### Schräge Decken

Zwei Höhen machen die Decke schräg, `slope` sagt, wie. `gable` ist ein Satteldach: an den Wänden 3 m, in der Mitte 7 m hoch, der First läuft an der längeren Seite entlang. Die Leuchten liegen dann so schräg wie die Decke über ihnen. Weil sie jetzt höher hängen, bekommen sie mit `power` mehr Kraft:

```room
~ [room]
~ name = Saal
height = 3, 7
slope = gable
```

```room
~ [L]
~ light = #fff3cf
power = 1.6
```

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-2.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-2.webp' alt='Derselbe Saal unter einem Satteldach: Die Decke steigt von beiden Wänden zur Mitte hin an, die Leuchten liegen schräg in den Dachflächen, die Stirnwand ist fünfeckig.'>

Eine Decke kann auch nur in eine Richtung ansteigen, wie ein Pultdach. Mit `slope = east` statt `gable` wird die Decke des Saals nach Osten immer höher, also nach rechts im Grundriss:

```room
~ height = 3, 7
slope = east
```

An der Westwand ist sie dann 3 m hoch, an der Ostwand 7 m. Statt `east` geht auch `north`, `south` oder `west`. Auch hier liegen die Leuchten so schräg wie die Decke. So sähe der Saal damit aus, von der Seite; für die nächsten Schritte bleibt er beim Satteldach:

<!-- tutorial-scope -->

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-pult.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=23.00,15.20,180,14
hook: light
hook: info off
-->

<img class='full' src='saal-pult.webp' alt='Der leere Saal von der Seite: Die Decke steigt von rechts nach links gleichmäßig an, die Leuchten liegen schräg in ihr.'>

<!-- /tutorial-scope -->

### Säulen

Ein Buchstabe im Grundriss kann auch ein Block sein. Vier Säulen, je zwei an den langen Seiten, zwischen den Leuchten:

```room
~ #...........#
#...L.S.L.S.#
~ 1...........#
~ #...........#
~ #...........#
#...L.S.L.S.#
~ #...........#
```

```room
[S]
block = 0.7, 7, 0.7      # höher als die Decke: Die Säule verschwindet in ihr
```

`block = 0.7, 7, 0.7` ist 0,7 m breit, 7 m hoch und 0,7 m tief, mitten auf seinem Feld. Ist ein Block höher als die Decke, verschwindet er in ihr. `block = 1` füllt das ganze Feld einen Meter hoch, eine Kiste. Mit `base = 2` schwebt ein Block zwei Meter über dem Boden, mit `color = #6a5a3a` bekommt er eine eigene Farbe; ohne sieht er aus wie die Wände seines Raums. Mit `material = marble` ist er aus Marmor, auch in einem Raum mit Tapete; es gehen alle Arten, die es für Wände gibt (weiter unten im Kapitel). Durch Blöcke läuft man nicht hindurch, und sie werfen Schatten.

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-3.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-3.webp' alt='Der Saal mit vier Säulen, die bis in das Dach reichen; an Boden und Wänden ihre weichen Schatten.'>

### Farbiges Licht

Die Leuchte hinten rechts soll rot leuchten. Sie bekommt einen eigenen Buchstaben:

```room
~ #...........#
#...L.S.R.S.#
~ #...........#
```

```room
[R]
light = #ff3020
power = 3
size = 1.2, 1.2
```

`light` ist die Farbe, `size` die Größe in Metern: Eine größere Leuchte wirft weichere Schatten. Mit `flicker = 1` flackert eine Leuchte ab und zu, wie die in der Kammer. So sähe der Saal mit der roten Leuchte aus; für die nächsten Schritte bleiben alle vier weiß: Das rote Licht fällt auf Wände, Boden und Säulen und wird von ihnen zurückgeworfen, rot gefärbt.

<!-- tutorial-scope -->

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-4.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-4.webp' alt='Der Saal mit einer roten Leuchte hinten rechts: Die rechte Wand, der Boden und die Säulen dort glühen rot, nach vorn wird es wieder gelb.'>

<!-- /tutorial-scope -->

### Eine Statue

<div class='found-note'>
<p>In der großen Halle steht eine Frau aus Stein, sechs Meter hoch. Sie schaut zum Eingang. Ich bin dreimal um sie herumgegangen. Sie schaut immer zum Eingang.</p>
<cite>Letzter Eintrag einer Kartografin, Halle</cite>
</div>

In die Mitte des Saals kommt Athena, auf einem Sockel:

```room
~ 1...........#
#.....M.....#
~ #...........#
```

```room
[M]
model = statues/athena.glb
height = 2.2
rotation = 270
material = marble
pedestal = 0.9, 0.8, 0.9
```

Eine Statue ist ein 3D-Modell im Format GLB. `height` macht sie so hoch wie angegeben, `rotation` dreht sie in Grad um ihre senkrechte Achse (hier zur Tür), `material = marble` macht sie aus Marmor statt aus den Farben des Modells, und `pedestal` stellt sie auf einen Sockel: 0,9 m breit, 0,8 m hoch, 0,9 m tief. Auch Statuen werfen Schatten und bekommen Bounce-Licht, und wo eine Statue sich selbst verdeckt, ist es auf ihr dunkler.

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-5.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-5.webp' alt='Athena aus weißem Marmor auf einem hellen Sockel in der Mitte des Saals, zwischen den Säulen, im warmen Licht der vier Leuchten.'>

Athena ist der 3D-Scan eines Gipsabgusses einer antiken Statue, aus dem [Statens Museum for Kunst](https://open.smk.dk/artwork/image/KAS255) in Kopenhagen, gemeinfrei. Ohne `material = marble` behält ein Modell seine eigenen Farben. Wie du eigene Modelle holst, Möbel zum Beispiel, steht im Kapitel »Licht« bei den Wandlampen.

### Marmor

Zum Schluss Wände und Boden aus Marmor:

```room
~ height = 3, 7
~ slope = gable
walls = marble
floor = marble
```

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-6.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='saal-6.webp' alt='Der fertige Saal: Wände und Boden aus weißem, geädertem Marmor, die Säulen hell, Athena auf ihrem Sockel.'>

### Andere Wände und Böden

Marmor ist nur eine Möglichkeit. `walls` kennt sechs Arten, `floor` fünf, jede in der Farbe aus `wallpaper` (Wände) und `carpet` (Boden), Marmor in seiner eigenen:

<table class='table'>
<tr><th>Wert</th><th>Wände (<code>walls</code>)</th><th>Boden (<code>floor</code>)</th></tr>
<tr><td><code>wallpaper</code>, <code>carpet</code></td><td>Tapete mit Rautenmuster</td><td>Teppich</td></tr>
<tr><td><code>marble</code></td><td>Marmor</td><td>Marmor</td></tr>
<tr><td><code>wood</code></td><td>senkrechte Bretter</td><td>Dielen</td></tr>
<tr><td><code>tiles</code></td><td>Fliesen</td><td>Fliesen</td></tr>
<tr><td><code>velvet</code></td><td>Samt in senkrechten Streifen, wie im Kino</td><td></td></tr>
<tr><td><code>plain</code></td><td>einfarbig</td><td>einfarbig</td></tr>
</table>

Für Fliesen wie in einem Schwimmbad zum Beispiel `walls = tiles`, `floor = tiles`, `wallpaper = #e6ecef` und `carpet = #c4ced3`, dazu eine helle Decke mit `ceiling = #e8ecee`. So sieht der leere Saal vom Anfang mit jeder Art aus, Holz und Samt mit Wandlampen statt Leuchten an der Decke; wie das geht, steht im Kapitel »Licht« (ein Klick zeigt ein Bild groß):

<!-- tutorial-scope -->

<div class='gallery columns-3'>
<figure>
<img src='saal-1.webp' alt='Der leere Saal mit gelber Tapete, Teppich und flacher Decke mit vier Leuchten.'>
<figcaption><code>wallpaper</code> und <code>carpet</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-marmor.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='saal-marmor.webp' alt='Der leere Saal mit Wänden und Boden aus weißem, geädertem Marmor.'>
<figcaption><code>marble</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-holz.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='saal-holz.webp' alt='Der leere Saal mit Wänden aus senkrechten, hellbraunen Brettern und einem Boden aus dunkleren Dielen, beleuchtet von Wandlampen an den beiden langen Wänden.'>
<figcaption><code>wood</code>, mit Wandlampen</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-fliesen.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='saal-fliesen.webp' alt='Der leere Saal ganz in kleinen weißen Fliesen an den Wänden und hellgrauen auf dem Boden, wie ein Schwimmbad ohne Wasser.'>
<figcaption><code>tiles</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-einfarbig.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='saal-einfarbig.webp' alt='Der leere Saal mit einfarbig blaugrauen Wänden und einem dunkelgrauen Boden.'>
<figcaption><code>plain</code></figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-samt.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='saal-samt.webp' alt='Der leere Saal wie ein Kino: Wände mit dunkelrotem Samt in senkrechten Streifen, ein dunkler Teppich, eine dunkle Decke, Licht nur von Wandlampen an den langen Wänden.'>
<figcaption><code>velvet</code>, mit Wandlampen</figcaption>
</figure>
</div>

<!-- /tutorial-scope -->

### Schräge Böden

<div class='found-note'>
<p>Der Gang hinter dem Saal steigt an. Das merkt man an den Beinen, nicht an den Augen: Die Decke hängt schief, die Leuchten auch, und irgendwann weiß ich nicht mehr, ob ich hinauf- oder hinuntergehe.</p>
<cite>Bleistiftnotiz, sehr klein geschrieben</cite>
</div>

Auch ein Boden kann schräg sein. Gib dem Saal an der rechten Wand eine zweite Tür:

```room
~ #...........#
#.....M.....2
~ #...........#
```

```room
[2]
door = rampe 1
```

und leg `rooms/rampe.room` an:

```room
# Eine Rampe hinter dem Saal.

[room]
name = Rampe
ramp = east, 4          # der Boden steigt nach Osten um 4 m
height = 2.6

[map]
####################
#..................#
#...L.....L.....L..#
1..................#
#...L.....L.....L..#
#..................#
####################

[L]
light = #fff3cf
power = 1.6
```

`ramp = east, 4` lässt den Boden nach Osten um 4 m ansteigen, vom ersten Feld hinter der Wand bis zum letzten. `height` misst die Decke über dem Boden, an jeder Stelle: Sie steigt mit, überall 2,6 m über dir, und die Leuchten mit ihr. Wie hoch ein Raum liegt, rechnet das Programm aus seinen Türen aus: An einer Tür liegen beide Böden gleich hoch. Eine Tür am oberen Ende, in der Ostwand, führte in einen Raum 4 m über dem Saal.

<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-7.room
write-file: rooms/rampe.room <- file:stufen/rampe-1.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=29.60,18.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='rampe-1.webp' alt='Ein langer Gang, dessen Boden nach vorn gleichmäßig ansteigt; die Decke steigt mit, die Leuchten in ihr liegen so schräg wie der Boden.'>

### Boden und Decke zusammen

Mit `slope` und zwei Höhen kippt die Decke außerdem, und weil die Höhen über dem Boden gelten, kippt sie gegen den Boden. Jede Richtung fühlt sich anders an. So sähe die Rampe mit dreien davon aus; für den Rest der Seite bleibt sie bei der letzten.

<!-- tutorial-scope -->

**Enger und enger.** Die Decke steigt nach Westen, gegen die Rampe: unten, an der Westwand, 6 m über dem Boden, oben, an der Ostwand, nur noch 2,4 m. Weil der Boden zugleich um 4 m steigt, bleibt die Decke fast waagerecht, und der Gang wird nach oben hin immer niedriger:

```room
~ ramp = east, 4          # der Boden steigt nach Osten um 4 m
height = 2.4, 6         # über dem Boden: an der Ostwand 2,4 m, an der Westwand 6 m
slope = west
```

<!-- tutorial-screenshot
write-file: rooms/rampe.room <- file:stufen/rampe-2.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=29.60,18.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='rampe-2.webp' alt='Derselbe Gang unter einer Decke, die fast waagerecht bleibt, während der Boden ansteigt: Hinten treffen sich Boden und Decke beinahe.'>

<!-- /tutorial-scope -->

<!-- tutorial-scope -->

**Hinunter.** Mit einer negativen Zahl fällt der Boden, hier um 3 m, unter einem Satteldach:

```room
ramp = east, -3         # der Boden fällt nach Osten um 3 m
height = 2.4, 3.6
slope = gable
```

<!-- tutorial-screenshot
write-file: rooms/rampe.room <- file:stufen/rampe-3.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=29.60,18.00,-90,-6
hook: light
hook: info off
-->

<img class='full' src='rampe-3.webp' alt='Ein Gang unter einem Satteldach, dessen Boden nach vorn abfällt; die Tür am Ende liegt tief unten.'>

<!-- /tutorial-scope -->

**Quer dazu.** Der Boden steigt nach vorn, die Decke kippt zur Seite, und die Leuchten hängen schief in ihr. Wo ist oben?

```room
~ ramp = east, 4          # der Boden steigt nach Osten um 4 m
height = 2.4, 5.5       # über dem Boden: an der Südwand 2,4 m, an der Nordwand 5,5 m
slope = north           # die Decke steigt zur Seite hin an, quer zur Rampe
```

<!-- tutorial-screenshot
write-file: rooms/rampe.room <- file:stufen/rampe.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=29.60,18.00,-90,6
hook: light
hook: info off
-->

<img class='full' src='rampe.webp' alt='Ein langer Gang, dessen Boden nach vorn ansteigt, während die Decke zur Seite kippt; die Leuchten hängen schief in ihr, die Sockelleisten laufen schräg nach oben.'>

Die Lobby hat einen ganzen Weg aus solchen Räumen, die Schieflage mitten darin (siehe »Die Lobby«).

## Dein eigener Raum

Jetzt bist du dran: Bau einen Raum nach deiner Idee und schließ ihn mit einer Tür an. Was du in `[room]` einstellen kannst:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>height</code></td><td>Deckenhöhe über dem Boden in Metern; zwei Zahlen für eine schräge Decke</td></tr>
<tr><td><code>ramp</code></td><td>ein schräger Boden: Richtung und um wie viele Meter er ansteigt, zum Beispiel <code>east, 4</code></td></tr>
<tr><td><code>slope</code></td><td><code>flat</code>, <code>north</code>, <code>east</code>, <code>south</code>, <code>west</code> oder <code>gable</code></td></tr>
<tr><td><code>walls</code></td><td><code>wallpaper</code> (Tapete), <code>marble</code> (Marmor), <code>wood</code> (Holz), <code>tiles</code> (Fliesen), <code>velvet</code> (Samt) oder <code>plain</code> (einfarbig)</td></tr>
<tr><td><code>floor</code></td><td><code>carpet</code> (Teppich), <code>marble</code>, <code>wood</code>, <code>tiles</code> oder <code>plain</code></td></tr>
<tr><td><code>wallpaper</code>, <code>carpet</code>, <code>ceiling</code></td><td>die Farben von Wänden, Boden und Decke</td></tr>
<tr><td><code>baseboard</code></td><td>die Sockelleiste: Höhe und Farbe; ohne Angabe 8 cm und halb so hell wie die Tapete, <code>none</code>: keine</td></tr>
<tr><td><code>look</code></td><td>wohin du am <code>@</code> schaust: <code>north</code>, <code>east</code>, <code>south</code>, <code>west</code> oder Grad</td></tr>
</table>

Alle Einträge stehen in der `README.md` der Vorlage. Der Dateiname ist der Name des Raums, den eine Tür nennt: nur Kleinbuchstaben, Ziffern, `-` und `_`.

_task(eigener-raum)

## Licht

<div class='found-note'>
<p>Die Leuchten gehen nie aus. Ich habe versucht, eine zu erreichen, um sie abzuschrauben. Sie ist warm. Nicht heiß, warm. Wie etwas, das atmet.</p>
<p>Seltsam ist die Decke. Die Leuchten hängen direkt darunter und strahlen nach unten, und trotzdem ist die Decke hell.</p>
<cite>Funkspruch, Ende fehlt</cite>
</div>

### Leuchten

Eine Leuchte hängt knapp unter der Decke, mitten über ihrem Feld, unter einer schrägen Decke so schräg wie sie:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>light</code></td><td>ihre Farbe, zum Beispiel <code>#fff3cf</code></td></tr>
<tr><td><code>power</code></td><td>wie hell sie ist, 1.0 ohne Angabe</td></tr>
<tr><td><code>size</code></td><td>Breite und Länge in Metern, eine Zahl für eine quadratische; ohne Angabe <code>0.85</code>, 85 cm im Quadrat</td></tr>
<tr><td><code>flicker</code></td><td><code>1</code>: Sie flackert ab und zu</td></tr>
</table>

Farbiges Licht färbt alles, was es trifft, auch über die Tür hinaus in den Nachbarraum: Im Gang siehst du, wie rotes und blaues Licht aus zwei Türen fällt.

### Lampen an der Wand

<div class='found-note'>
<p>In der Kammer steht jetzt ein Sofa. Niemand hat es gebracht. An den Wänden hängen zwei Lampen, und sie brennen, obwohl nirgends ein Kabel ist.</p>
<cite>Seite aus einem durchnässten Notizbuch, dritte Handschrift</cite>
</div>

Eine Leuchte hängt immer unter der Decke. Licht kann aber auch aus einem Modell kommen: einer Wandlampe, einer Stehlampe, einer Laterne. Die Kammer wird ein Ruheraum. Statt der flackernden Leuchte an der Decke hängen zwei Wandlampen an den Wänden, und an der Südwand steht ein Sofa. Ersetze `rooms/kammer.room` durch:

```room
# Eine Kammer, die gestern noch nicht da war. Jetzt steht ein Sofa darin.

[room]
name = Kammer
height = 2.2
wallpaper = #9a8a4c

[map]
#####
5...6
#W.E#
#.S.#
#####

[W]
model = statues/industrial_wall_lamp/industrial_wall_lamp_1k.gltf   # Poly Haven, CC0
height = 0.43
rotation = 90
offset = -0.42, 1.4, 0   # an die Westwand, 1,4 m hoch
light = #ffd9a0
power = 1.2

[E]
model = statues/industrial_wall_lamp/industrial_wall_lamp_1k.gltf   # Poly Haven, CC0
height = 0.43
rotation = 270
offset = 0.42, 1.4, 0    # an die Ostwand
light = #ffd9a0
power = 1.2

[S]
model = statues/sofa_03/sofa_03_1k.gltf   # Poly Haven, CC0
height = 0.93
rotation = 180
offset = 0, 0, -0.07     # an die Südwand

[6]
door = saal 1
```

`light = #ffd9a0` bei einem Modell macht es zu einer Lampe. Ihr Licht kommt aus den Teilen, die im Modell selbst leuchten, bei dieser Lampe aus der Glühbirne hinter dem Glas, und es strahlt nach allen Seiten, nicht nur nach unten wie eine Leuchte unter der Decke. `power` und `flicker` gelten wie bei einer Leuchte. Die Lampe selbst hält ihr Licht nicht auf.

`offset = x, y, z` schiebt ein Modell aus der Mitte seines Felds, in Metern und wie in den anderen 3D-Aktivitäten: x nach Osten (im Grundriss nach rechts), y nach oben, z nach Süden (im Grundriss nach unten), mit einer negativen Zahl in die andere Richtung. Die Lampe im Feld `W` rückt 42 cm nach Westen an die Wand und hängt 1,4 m hoch. Das Sofa rückt 7 cm nach Süden: Es ist 1,12 m tief, ein Feld nur 1 m, und ragte sonst in die Wand. `offset` gibt es auch für Blöcke. Welche Seite eines Modells vorn ist, legt fest, wer es gebaut hat; `rotation` in Schritten von 90 dreht es, bis es stimmt. Die Lampe an der Westwand ist um 90° gedreht, damit ihre Rückseite an der Wand liegt.

<!-- tutorial-screenshot
write-file: rooms/kammer.room <- file:stufen/kammer-lampen.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=14.00,17.00,-150,-12
hook: light
hook: info off
-->

<img class='full' src='kammer-lampen.webp' alt='Die Kammer, nur von zwei Wandlampen beleuchtet: An der rechten Wand hängt eine ovale Lampe mit Gitter und leuchtet warm, an der Südwand steht ein braunes Sofa mit gemusterten Kissen, die Decke ist hell.'>

### Modelle von Poly Haven

Sofa und Wandlampe liegen schon in `statues/`. Beide sind von [Poly Haven](https://polyhaven.com/models). Dort steht jedes Modell unter CC0: Du darfst es verwenden, wofür du willst, ohne zu fragen. So holst du dir weitere:

1. Wähl auf der Seite des Modells beim Download die Auflösung **1K** und das Format **glTF**. glTF und GLB kann das Programm lesen. 1K genügt, weil es vom Modell nur die Farbe der Oberfläche nimmt: 2K oder 4K sähen genauso aus und wären nur größer.
2. Ein Modell in glTF besteht aus mehreren Dateien: einer `.gltf`, einer `.bin` und Bildern im Ordner `textures`. Sie gehören zusammen in einen eigenen Ordner in `statues`, etwa `statues/sessel/`, so wie sie ankommen.
3. In der Raumdatei steht dann `model = statues/sessel/DATEINAME.gltf`. Wie groß das Modell ist, steht auf seiner Seite bei den Maßen; die Höhe davon gehört in `height`.

Kommt beim Herunterladen eine ZIP-Datei an, zieh sie in VS Code in den Ordner `statues` und pack sie im Terminal in einen eigenen Ordner aus:

```bash
cd /workspace/backrooms-starter/statues
unzip DATEINAME.zip -d sessel
```

Weitere Modelle unter CC0 findest du bei den [3D-Scans der Smithsonian Institution](https://3d.si.edu/collections) und bei [Sketchfab](https://sketchfab.com/search?features=downloadable&licenses=7c23a1ba438d4306920229c12afcb5f9&type=models), die Suche dort schon auf CC0 und herunterladbare Modelle eingestellt; beide als GLB herunterladen. Nimm nur Modelle, deren Lizenz das erlaubt, und schreib in einem Kommentar neben `model =` dazu, woher sie kommen.

### Was das Programm rechnet

Viele Spiele rechnen das Licht für jedes Bild neu, 60-mal in der Sekunde, weil sich dort Dinge bewegen, und müssen dafür stark vereinfachen. Hier bewegt sich nur du, die Leuchten bleiben, wo sie sind. Darum rechnet das Programm das Licht einmal aus, wenn das Level lädt, und klebt es dann wie eine zweite Tapete auf jede Fläche. Diese Tapete aus Licht heißt **Lightmap**, und das Ausrechnen nennt man **Backen**. Schritt für Schritt, im Saal mit seinen Säulen:

**Die Punkte.** Die Lightmap hat auf jedem Meter Wand, Boden und Decke 16 Punkte in jeder Richtung, alle 6,25 cm einen. Nur für diese Punkte rechnet die Grafikkarte aus, wie hell es dort ist; dazwischen mischt sie beim Zeichnen die vier nächsten. Mit <kbd>G</kbd> siehst du die Punkte, mit <kbd>T</kbd> ohne Tapete und Teppich deutlicher:

<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t g
-->

<img class='full' src='licht-punkte.webp' alt='Der Saal ganz in Hellgrau, übersät mit einem gleichmäßigen Raster aus dunklen Punkten auf Wänden, Boden, Säulen und Satteldach; vorn sind die Punkte groß und weit auseinander, hinten klein und dicht.'>

_task(lichtpunkte)

Wo ein Modell nah an einer Wand oder auf dem Boden steht, wirft es Schatten, für die 6,25 cm zu grob sind. Dort bekommt die Fläche ein zweites, feineres Stück Lightmap, mit bis zu viermal so vielen Punkten in jeder Richtung, das zu seinem Rand hin weich in das grobe übergeht. In der Kammer, hinter dem Sofa:

<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=15.00,17.00,180,-25
hook: light
hook: info off
hook: key t g
-->

<img class='full' src='licht-punkte-sofa.webp' alt='Das Sofa in der Kammer ganz in Hellgrau; auf der Wand dahinter und auf dem Boden um es herum liegen die Punkte viel dichter als weiter oben an der Wand und vorn auf dem Boden.'>

**Direktes Licht.** Für jeden Punkt nimmt das Programm einen zufälligen Punkt auf jeder Leuchte und schickt einen Strahl dorthin. Steht etwas im Weg, eine Wand, eine Säule oder ein Modell, kommt von dieser Leuchte nichts an. Sonst kommt Licht an, umso weniger, je weiter die Leuchte weg ist (doppelt so weit: ein Viertel) und je schräger es auf die Fläche fällt. Das wiederholt es 64-mal, jedes Mal mit einem anderen Punkt auf der Leuchte, und nimmt den Durchschnitt. Ein Punkt, der eine Leuchte nur halb sieht, ist in etwa der Hälfte der Durchgänge beleuchtet: Er liegt im **Halbschatten**. Darum werfen große Leuchten weiche Schatten und kleine scharfe. Nur mit direktem Licht sieht der Saal so aus:

<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t b
-->

<img class='full' src='licht-direkt.webp' alt='Der Saal in Grau, nur mit direktem Licht: helle Flecken auf dem Boden unter den Leuchten, die Säulen werfen weiche Schatten, das Satteldach und alles, was keine Leuchte sieht, fast schwarz.'>

Wie weich ein Schatten ist, hängt also davon ab, wie groß die Leuchte ist. Mit `size = 0.2` und mit `size = 2` (und `power` so, dass jede gleich viel Licht gibt, denn eine kleinere Leuchte gibt auch weniger):

<!-- tutorial-scope -->

```room
[L]
light = #fff3cf
power = 29
size = 0.2
```

<div class='gallery columns-2'>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-klein.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t b
-->
<img src='licht-klein.webp' alt='Der Saal nur mit direktem Licht kleiner Leuchten: Die Säulen werfen scharf begrenzte Schatten auf Boden und Wände.'>
<figcaption><code>size = 0.2</code>: scharfe Schatten</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
write-file: rooms/saal.room <- file:stufen/saal-gross.room
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t b
-->
<img src='licht-gross.webp' alt='Derselbe Saal mit großen Leuchten: Die Schatten der Säulen sind breit und weich und laufen ohne Kante aus.'>
<figcaption><code>size = 2</code>: weiche Schatten</figcaption>
</figure>
</div>

<!-- /tutorial-scope -->

**Bounce-Licht.** Die Decke ist schwarz: Keine Leuchte strahlt nach oben. In Wirklichkeit ist sie trotzdem hell, weil jede Fläche einen Teil des Lichts zurückwirft, das auf sie fällt, in ihrer eigenen Farbe: der Teppich bräunliches Licht, eine rote Wand rotes. Ist das direkte Licht fertig, schickt das Programm von jedem Punkt Strahlen in zufällige Richtungen und schaut, wie hell die Flächen sind, die sie treffen. Was dabei herauskommt, ist das Licht, das einmal zurückgeworfen wurde, ein **Bounce**. Damit das schnell geht, sammelt es das zurückgeworfene Licht vorher in Würfeln von 25 cm, und weil zufällige Strahlen ein körniges Ergebnis geben, glättet es das Bounce-Licht am Ende. Das Ganze dreimal: Beim zweiten Bounce werfen die Flächen auch das Licht des ersten zurück und so weiter, darum steht beim Backen »bounce 1 of 3«. Mit einem, zwei und drei Bounces (in `level.ini` mit `bounces = 1` und so weiter):

<!-- tutorial-scope -->

<div class='gallery columns-3'>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open bounces=1&view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t
-->
<img src='licht-bounces-1.webp' alt='Der Saal in Hellgrau mit einem Bounce: Das Satteldach ist nur schwach beleuchtet, die Schatten der Säulen sind tief.'>
<figcaption>1 Bounce</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open bounces=2&view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t
-->
<img src='licht-bounces-2.webp' alt='Derselbe Saal mit zwei Bounces: Das Dach ist heller, die Schatten der Säulen sind weicher.'>
<figcaption>2 Bounces</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t
-->
<img src='licht-ohne-texturen.webp' alt='Derselbe Saal mit drei Bounces: das Dach hell, alles weich ausgeleuchtet.'>
<figcaption>3 Bounces</figcaption>
</figure>
</div>

<!-- /tutorial-scope -->

**Kontaktschatten.** Wo zwei Flächen aneinanderstoßen, in Ecken, an der Sockelleiste oder unter einem Block, kommt von vielen Seiten kein Bounce-Licht, weil die andere Fläche im Weg ist. Zwei kurze Strahlen von 80 cm prüfen bei jedem der 64 Durchgänge, wie viel Platz um einen Punkt herum ist: Trifft einer etwas, dunkelt das Bounce-Licht dort ab, umso mehr, je näher es ist, und sanft auslaufend, damit keine harte Linie entsteht. Auf Englisch heißt das **Ambient Occlusion**. Nur das Bounce-Licht, ohne und mit Kontaktschatten:

<div class='gallery columns-2'>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t l o
-->
<img src='licht-bounce-ohne-kontakt.webp' alt='Der Saal in Grau, nur mit Bounce-Licht und ohne Kontaktschatten: gleichmäßig hell, die Füße der Säulen und die Ecken kaum dunkler als die Flächen.'>
<figcaption><kbd>T</kbd> <kbd>L</kbd> <kbd>O</kbd>: ohne Kontaktschatten</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key t l
-->
<img src='licht-bounce.webp' alt='Derselbe Saal mit Kontaktschatten: die Ecken zwischen Wänden, Boden und Dach, die Füße der Säulen und der Boden unter dem Sockel dunkler.'>
<figcaption><kbd>T</kbd> <kbd>L</kbd>: mit Kontaktschatten</figcaption>
</figure>
</div>

**Alles zusammen.** Direktes Licht, Bounce-Licht und Kontaktschatten, mal der Farbe von Wänden und Boden:

<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->

<img class='full' src='licht-alles.webp' alt='Der Saal aus Marmor im warmen Licht der Leuchten: helle Wände und heller Boden, die Säulen mit ihren weichen Schatten, Athena auf ihrem Sockel, das Satteldach hell.'>

**Beim Zeichnen: Tone Mapping und Bloom.** Die Lightmap enthält das Licht, wie es wirklich ist: Unter einer Leuchte ist es viel heller als in einer Ecke, und die Leuchte selbst ist noch einmal viel heller. Ein Bildschirm kann das nicht zeigen, er kennt nur Helligkeiten von 0 bis 1. Das **Tone Mapping** drückt große Werte sanft zusammen, wie eine Kamera: Helles bleibt hell, ohne dass alles darüber gleich weiß wird. Davor kommt **Bloom** (<kbd>H</kbd>): Was viel heller ist als Weiß, die Leuchten vor allem, strahlt weich in seine Umgebung, so wie in einer Kamera oder im Auge, wenn man in eine Lampe schaut. Dafür verkleinert das Programm das Bild mehrmals auf die Hälfte, verwischt die hellsten Stellen und legt sie wieder vergrößert darüber. Das kostet bei jedem Bild ein wenig Zeit, darum fehlt Bloom mit der Rot-Cyan-Brille, in VR und in der Automap; dort hat jede Leuchte einen eigenen, einfachen Schein.

<div class='gallery columns-2'>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
hook: key h
-->
<img src='licht-ohne-bloom.webp' alt='Der Saal ohne Bloom: Die Leuchten sind scharf begrenzte helle Rechtecke im Dach.'>
<figcaption><kbd>H</kbd>: ohne Bloom</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open view=18.00,17.00,-90,8
hook: light
hook: info off
-->
<img src='licht-bloom.webp' alt='Derselbe Saal mit Bloom: Um jede Leuchte liegt ein weicher heller Schein.'>
<figcaption>mit Bloom</figcaption>
</figure>
</div>

**Warum es so echt aussieht.** Die Räume sind schlicht: gelbe Tapete, Teppich, ein paar Säulen. Dass sie trotzdem wie ein Foto wirken, liegt fast nur am Licht. In Wirklichkeit fällt Licht nicht nur von der Lampe auf den Boden, es prallt von allen Flächen ab, färbt sich dabei, macht Decken hell und Ecken dunkel, und Schatten haben weiche Ränder. Genau das rechnet das Programm aus, mit rund tausend Strahlen für jeden Punkt der Lightmap. Ein Spiel, das 60 Bilder in der Sekunde zeichnen muss, hat dafür keine Zeit und schummelt; das sieht man. Hier geht es, weil sich nichts bewegt: Keine Lampe wandert, keine Tür geht auf, keine Figur läuft durchs Bild. Das Licht wird einmal gebacken und stimmt dann für immer, und es gibt nichts, das sich anders verhielte als das Licht und den Trick verriete. Die Backrooms sind leer, und genau darum sehen sie so echt aus.

Statuen und andere Modelle bekommen ihr Licht nicht aus der Lightmap: Das Programm rechnet es für jede Ecke ihres Netzes aus, mit denselben Leuchten und demselben Bounce-Licht.

### Die Teile einzeln ansehen

Oben links steht für jeden Teil ein Schalter mit seiner Taste; grün heißt an. Ein Klick auf den Schalter oder die Taste schaltet ihn um:

<table class='table'>
<tr><th>Taste</th><th>Schalter</th><th>Was er zeigt</th></tr>
<tr><td><kbd>T</kbd></td><td>Textures</td><td>Tapete, Teppich und Decke; aus: alles hellgrau, nur das Licht</td></tr>
<tr><td><kbd>L</kbd></td><td>Direct light</td><td>das direkte Licht der Leuchten</td></tr>
<tr><td><kbd>B</kbd></td><td>Bounce light</td><td>das zurückgeworfene Licht</td></tr>
<tr><td><kbd>O</kbd></td><td>Occlusion</td><td>die Kontaktschatten</td></tr>
<tr><td><kbd>F</kbd></td><td>Flicker</td><td>das Flackern</td></tr>
<tr><td><kbd>E</kbd></td><td>Edges</td><td>die Kanten aller Flächen als Drahtgitter</td></tr>
<tr><td><kbd>G</kbd></td><td>Light points</td><td>die Punkte der Lightmap</td></tr>
<tr><td><kbd>H</kbd></td><td>Bloom</td><td>der helle Schein um die Leuchten</td></tr>
<tr><td><kbd>3</kbd></td><td>Anaglyph</td><td>das Bild für eine Rot-Cyan-Brille</td></tr>
<tr><td><kbd>M</kbd></td><td>Automap</td><td>die Karte</td></tr>
<tr><td><kbd>Shift</kbd> <kbd>L</kbd></td><td>Rebake light</td><td>das Licht noch einmal von vorn backen</td></tr>
<tr><td></td><td>Save light</td><td>das gebackene Licht als Datei herunterladen (siehe unten)</td></tr>
</table>

Geh zurück ins Zimmer (<kbd>Shift</kbd> <kbd>R</kbd>) und schau nach oben, zur Decke.

_task(decke)

Probier es selbst: Schalte mit <kbd>T</kbd> die Texturen aus, dann siehst du nur noch das Licht. Schalte mit <kbd>L</kbd> und <kbd>B</kbd> abwechselnd das direkte und das Bounce-Licht aus. Schalte mit <kbd>O</kbd> die Kontaktschatten an und aus und schau dabei in eine Ecke am Boden.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib der Leuchte im Zimmer <code>size = 0.2</code> und <code>power = 16</code> und lade neu. Eine kleinere Leuchte gibt auch weniger Licht, darum die größere Zahl. Sieh dir den Lichtfleck an, den sie durch die Tür in den Gang wirft: Seine Ränder werden scharf. Dann <code>size = 2</code> mit <code>power = 0.16</code>: Sie werden ganz weich. Mit <code>bounces = 0</code> in <code>level.ini</code> unter <code>[level]</code> backt das Programm gar kein Bounce-Licht; wie sieht der Gang dann aus? Stell beides danach wieder zurück.
</div>

_task(weiche-schatten)

### Licht aufheben

Backen dauert, darum hebt die Vorschau das fertige Licht im Browser auf. Lädst du neu, ohne etwas zu ändern, steht oben links »Light loaded from this browser«, und es wird hell, ohne dass gebacken wird. Änderst du auch nur ein Zeichen in einem Raum, passt das aufgehobene Licht nicht mehr, und es wird neu gebacken. Ob es passt, sagt ein Fingerabdruck: eine kurze Zahl, ausgerechnet aus allen Dateien des Levels, den Statuen und dem Teil des Programms, der backt.

»Save light« lädt das Licht als Datei `level.light` herunter. Zieh sie aus deinem Download-Ordner in den Explorer von VS Code, neben `level.ini`. Dann lädt das Level sein Licht aus dieser Datei, auch in einem anderen Browser, und du kannst deine Backrooms als fertige Webseite weitergeben. Nach jeder Änderung am Level musst du sie neu speichern; eine veraltete Datei meldet die Vorschau oben links.

## Die Lobby

<div class='found-note'>
<p>Wir haben einen Teil vollständig kartiert. Vierzehn Räume, ein Flur, der immer höher wird, ein Weg, auf dem nichts gerade ist, und am Ende eine Halle, in der man leiser spricht. Wir nennen ihn die Lobby. Die Karte liegt bei.</p>
<cite>Feldhandbuch, eingelegtes Blatt</cite>
</div>

Im Ordner `lobby` liegt ein ganzes Level: ein Flur, der sich auf neun Meter Breite und Höhe öffnet, ein Säulengang, eine Galerie mit Sockeln, ein Lager, eine Halle aus Marmor mit einer Statue und, vom roten Raum aus, ein Weg über Rampen in die Halle, mitten darin die Schieflage, in der kein Boden und keine Decke gerade ist. Häng dafür an die Adresse der Vorschau `?level=lobby/level.ini` an, zum Beispiel:

```text
…/index.html?level=lobby/level.ini
```

Das Licht der Lobby ist schon gebacken, in `lobby/level.light` (siehe »Licht aufheben«), darum ist es sofort hell. Ihre Räume stehen in `lobby/rooms/`: Schau nach, wie sie gebaut sind, und nimm dir, was du brauchen kannst.

<!-- tutorial-scope -->

<div class='gallery columns-2'>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open level=lobby/level.ini&view=27.00,13.00,-90,18
hook: light
hook: info off
-->
<img src='flur.webp' alt='Der Flur der Lobby: Er öffnet sich nach vorn zu einem hohen, hellen Raum mit zwei großen Leuchten unter der Decke; in der Stirnwand eine schmale Tür.'>
<figcaption>Der Flur wird weiter und höher</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open level=lobby/level.ini&view=58.00,26.00,180,8
hook: light
hook: info off
-->
<img src='halle.webp' alt='Die Halle: Marmorwände unter einem hohen Satteldach, zwei Reihen Säulen, ganz hinten im hellen Licht die Statue der Athena.'>
<figcaption>Die Halle mit Athena</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open level=lobby/level.ini&view=24.00,28.00,-90,10
hook: light
hook: info off
-->
<img src='aufgang.webp' alt='Der Aufgang: ein Gang, dessen Boden nach vorn ansteigt, während die Decke zur Seite kippt; hinten eine Tür in einen hellen Raum.'>
<figcaption>Der Aufgang</figcaption>
</figure>
<figure>
<!-- tutorial-screenshot
tab: preview
viewport: 1536x864
zoom: 1.5
hook: open level=lobby/level.ini&view=38.50,31.50,30,10
hook: light
hook: info off
-->
<img src='schieflage.webp' alt='Die Schieflage: ein Raum, dessen Boden zur Seite ansteigt und dessen Decke nach hinten kippt; keine Kante ist waagerecht, die Leuchten hängen schief, links eine Tür.'>
<figcaption>Die Schieflage</figcaption>
</figure>
</div>

<!-- /tutorial-scope -->

## Zeigen und veröffentlichen

<div class='found-note'>
<p>Wer eine Tür findet, soll sie offen lassen. Für die Nächsten.</p>
<cite>Mit Kreide an einen Türrahmen geschrieben</cite>
</div>

_include(workspace#live-apps)

Speichere vorher das Licht (»Save light«, siehe »Licht aufheben«) und leg `level.light` neben `level.ini`: Dann ist es bei allen sofort hell, und niemand muss warten, bis gebacken ist.

_include(workspace#subdomain)

## Mit Brille

<div class='found-note'>
<p>Mit der Brille sind die Gänge tiefer. Ich weiß nicht, ob die Brille sie tiefer zeigt oder ob sie tiefer werden, wenn man sie aufsetzt.</p>
<cite>Zettel, an eine Rot-Cyan-Brille geklebt</cite>
</div>

<kbd>3</kbd> schaltet auf ein Bild für eine Rot-Cyan-Brille um: Jedes Auge sieht die Räume von seinem eigenen Platz aus, und dein Gehirn macht daraus Tiefe. Wie das funktioniert, steht in der Aktivität [3D-Grafik mit Anaglyphen](/anaglyph). Auch die Automap ist dann räumlich, wie ein Modell auf dem Tisch.

Öffnest du die Seite im Browser einer VR-Brille, steht unten »ENTER VR«. Dann läufst du mit dem linken Stick, drehst dich mit dem rechten, und die Grifftaste stellt die Automap als kleines Modell vor dich hin. Damit das geht, muss die Brille die Adresse deiner Vorschau öffnen können.

## Der Maschinenraum

<div class='found-note'>
<p>Hinter einer Tür, die auf keiner Karte steht, haben wir einen Raum voller Kabel gefunden. Es summt dort lauter als anderswo. Wir glauben, dass hier alles gemacht wird.</p>
<cite>Feldhandbuch, letzte beschriebene Seite</cite>
</div>

Das Programm steht im Ordner `engine/`, in JavaScript, und benutzt die 3D-Bibliothek [three.js](https://threejs.org/). Die liegt in `vendor/`: Die Seite lädt nichts aus dem Internet.

<table class='table'>
<tr><th>Datei</th><th>Was sie tut</th></tr>
<tr><td><code>level.js</code></td><td>liest <code>level.ini</code> und die Räume und legt sie an ihren Türen aneinander</td></tr>
<tr><td><code>geometry.js</code></td><td>baut daraus Böden, Decken, Wände, Sockelleisten und Blöcke und legt fest, wo auf der Lightmap jede Fläche liegt</td></tr>
<tr><td><code>bake.js</code></td><td>backt das Licht auf der Grafikkarte, mit Shadern in GLSL</td></tr>
<tr><td><code>textures.js</code></td><td>malt Tapete, Teppich und Marmor, mit Zufall und ohne ein einziges Bild</td></tr>
<tr><td><code>main.js</code></td><td>lädt alles, schaltet die Leuchten an und zeichnet jedes Bild</td></tr>
<tr><td><code>controls.js</code>, <code>automap.js</code></td><td>Laufen, Umsehen und die Automap</td></tr>
</table>

Was ein Shader ist, steht im [Handbuch zu GLSL](/reference/glsl). Wenn du am Programm etwas änderst und nichts mehr geht: Die Konsole des Browsers (<kbd>F12</kbd>) sagt, in welcher Zeile es hakt, und `git diff` zeigt, was du geändert hast.

<div class='found-note'>
<p>Wenn du einen Ausgang findest, schreib ihn auf. Wenn du keinen findest, bau einen Raum, in dem es sich zu warten lohnt.</p>
<cite>Feldhandbuch, Rückseite</cite>
</div>
