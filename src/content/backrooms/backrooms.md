<div class='meta'>
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

Danach liegt die Vorlage in `/workspace/backrooms-starter`.

_task(geklont)

### Eintreten

_include(workspace#live-server)

Die Vorschau zeigt `index.html`. Zuerst ist es dunkel, nur feine Linien zeigen die Kanten der Räume. Dann gehen die Leuchten an, eine nach der anderen, die nächsten zuerst, und oben links wächst ein Balken: Das Licht wird berechnet. Wie lange das dauert, hängt von der Grafikkarte deines Rechners ab. Ist es fertig, steht dort »Light baked in …«.

Klick ins Bild, dann siehst du dich mit der Maus um. <kbd>Esc</kbd> gibt die Maus wieder frei.

<table class='table'>
<tr><th>Taste</th><th>Was passiert</th></tr>
<tr><td><kbd>W</kbd> <kbd>A</kbd> <kbd>S</kbd> <kbd>D</kbd></td><td>laufen, mit <kbd>Shift</kbd> rennen</td></tr>
<tr><td><kbd>↑</kbd> <kbd>↓</kbd> <kbd>←</kbd> <kbd>→</kbd></td><td>vor und zurück laufen, nach links und rechts drehen</td></tr>
<tr><td><kbd>M</kbd> oder <kbd>Tab</kbd></td><td>die Automap: das ganze Bauwerk als Drahtgitter</td></tr>
<tr><td><kbd>R</kbd></td><td>zurück zum Anfang</td></tr>
</table>

Wo du stehst, merkt sich die Adresse der Seite. Lädst du neu, stehst du wieder dort, solange sich der Grundriss nicht geändert hat.

<div class='hint try'>
<strong>Ausprobieren:</strong> Drück <kbd>M</kbd>. Die Automap zeigt jeden Raum in seiner eigenen Farbe und mit seinem Namen, die Leuchten als gelbe Punkte; der rote Pfeil bist du. Zieh mit der Maus, um die Karte zu drehen, mit <kbd>Shift</kbd>, um sie zu verschieben, und zoom mit dem Mausrad. Laufen kannst du dabei weiter. Wie viele Räume zählst du? <kbd>Esc</kbd> schließt die Karte.
</div>

## Kartografie

<div class='found-note'>
<p>Wir kartieren in Feldern. Ein Feld ist ein Meter. Eine Raute ist eine Wand, ein Punkt ist Teppich. Was nicht auf der Karte steht, gibt es nicht. Das ist die einzige Regel, auf die man sich hier verlassen kann.</p>
<cite>Notiz, mit Kugelschreiber auf die Tapete geschrieben</cite>
</div>

### Ein Raum

Öffne `rooms/zimmer.room`. Das ist der Raum, in dem du aufgewacht bist:

```ini
# Ein kleines Zimmer. Du weißt nicht, wie du hierhergekommen bist.

[room]
baseboard = 0.08, #5a4626
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

Eine Raumdatei besteht aus Abschnitten in eckigen Klammern. In `[room]` steht, wie der Raum aussieht: sein Name, wie hoch die Decke ist (2,6 m), eine dunkle Sockelleiste unten an den Wänden, 8 cm hoch, und wohin du am Anfang schaust. Alles nach einem `#` mit Leerzeichen davor ist ein Kommentar.

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

```ini
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
power = 0.5
flicker = 1
```

Die Leuchte ist orange und schwach, und mit `flicker = 1` flackert sie ab und zu. Die Tapete ist eine Farbe als Hexadezimalzahl wie in CSS: Rot, Grün und Blau, je zwei Ziffern.

Noch führt keine Tür in die Kammer, also gibt es sie nicht. Öffne `rooms/gang.room`. Weit unten im Grundriss ist links die `1`, die Tür zum Zimmer. Ersetze in dieser Zeile das `#` am rechten Rand durch eine `5`:

```ini
~ #...#
1...5
~ #...#
```

und schreib ganz unten in die Datei, wohin sie führt:

```ini
[5]
door = kammer 5
```

Speichere beide Dateien und lade die Vorschau neu. Die Kammer liegt östlich vom Gang, gegenüber der Tür zum Zimmer. In `kammer.room` steht kein Abschnitt `[5]`: Es genügt, wenn einer der beiden Räume die Tür beschreibt.

Stimmt etwas nicht, steht oben links in Rot, in welcher Datei und in welcher Zeile das Programm hängen geblieben ist, auf Englisch, zum Beispiel `rooms/kammer.room: room kammer overlaps room rot`. Das heißt: Der Raum läge auf dem Boden eines anderen. Schieb die Tür an eine andere Stelle oder mach den Raum kleiner.

<div class='hint try'>
<strong>Ausprobieren:</strong> Schreib in den Abschnitt <code>[room]</code> der Kammer <code>height = 2.2, 4</code> und <code>slope = east</code>. Die Decke steigt nach Osten an, von 2,2 auf 4 Meter. Statt <code>east</code> geht auch <code>north</code>, <code>south</code> und <code>west</code>, und <code>gable</code> macht ein Satteldach, in der Mitte am höchsten. Sieh dir die Schatten an der schrägen Decke an.
</div>

### Bau weiter

Jetzt bist du dran: Bau einen Raum nach deiner Idee und schließ ihn mit einer Tür an. Ein paar Dinge, die du in `[room]` einstellen kannst:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>height</code></td><td>Deckenhöhe in Metern; zwei Zahlen für eine schräge Decke</td></tr>
<tr><td><code>slope</code></td><td><code>flat</code>, <code>north</code>, <code>east</code>, <code>south</code>, <code>west</code> oder <code>gable</code></td></tr>
<tr><td><code>walls</code></td><td><code>wallpaper</code> (Tapete), <code>marble</code> (Marmor) oder <code>plain</code> (einfarbig)</td></tr>
<tr><td><code>floor</code></td><td><code>carpet</code> (Teppich), <code>marble</code> oder <code>plain</code></td></tr>
<tr><td><code>wallpaper</code>, <code>carpet</code>, <code>ceiling</code></td><td>die Farben von Wänden, Boden und Decke</td></tr>
<tr><td><code>baseboard</code></td><td>eine Sockelleiste: Höhe und Farbe</td></tr>
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

Eine Leuchte hängt knapp unter der Decke, mitten über ihrem Feld:

<table class='table'>
<tr><th>Eintrag</th><th>Bedeutung</th></tr>
<tr><td><code>light</code></td><td>ihre Farbe, zum Beispiel <code>#fff3cf</code></td></tr>
<tr><td><code>power</code></td><td>wie hell sie ist, 1.0 ohne Angabe</td></tr>
<tr><td><code>size</code></td><td>Breite und Länge in Metern, ohne Angabe <code>0.6, 1.2</code></td></tr>
<tr><td><code>flicker</code></td><td><code>1</code>: Sie flackert ab und zu</td></tr>
</table>

Farbiges Licht färbt alles, was es trifft, auch über die Tür hinaus in den Nachbarraum: Im Gang siehst du, wie rotes und blaues Licht aus zwei Türen fällt.

### Was das Programm rechnet

Viele Spiele rechnen das Licht für jedes Bild neu, 60-mal in der Sekunde, weil sich dort Dinge bewegen, und müssen dafür stark vereinfachen. Hier bewegt sich nur du, die Leuchten bleiben, wo sie sind. Darum rechnet das Programm das Licht einmal aus, wenn das Level lädt, und klebt es dann wie eine zweite Tapete auf jede Fläche. Diese Tapete aus Licht heißt **Lightmap**, und das Ausrechnen nennt man **Backen**.

Die Lightmap hat auf jedem Meter Wand, Boden und Decke 16 Punkte in jeder Richtung, alle 6,25 cm einen. Für jeden Punkt rechnet die Grafikkarte aus, wie hell es dort ist.

_task(lichtpunkte)

**Direktes Licht.** Für jeden Punkt nimmt das Programm einen zufälligen Punkt auf jeder Leuchte und schickt einen Strahl dorthin. Steht etwas im Weg, eine Wand oder eine Säule, kommt von dieser Leuchte nichts an. Sonst kommt Licht an, umso weniger, je weiter die Leuchte weg ist (doppelt so weit: ein Viertel) und je schräger es auf die Fläche fällt. Das wiederholt es 64-mal, jedes Mal mit einem anderen Punkt auf der Leuchte, und nimmt den Durchschnitt. Ein Punkt, der eine Leuchte nur halb sieht, ist in etwa der Hälfte der Durchgänge beleuchtet: Er liegt im **Halbschatten**.

**Bounce-Licht.** Jede Fläche wirft einen Teil des Lichts zurück, das auf sie fällt, in ihrer eigenen Farbe: eine gelbe Tapete gelbes Licht. Ist das direkte Licht fertig, schickt das Programm von jedem Punkt Strahlen in zufällige Richtungen und schaut, wie hell die Flächen sind, die sie treffen. Was dabei herauskommt, ist das Licht, das einmal zurückgeworfen wurde, ein **Bounce**. Das Ganze dreimal: Beim zweiten Bounce werfen die Flächen auch das Licht des ersten zurück und so weiter. Darum steht beim Backen »bounce 1 of 3«.

**Kontaktschatten.** Wo zwei Flächen aneinanderstoßen, in Ecken, an der Sockelleiste oder unter einem Block, kommt von vielen Seiten kein Bounce-Licht, weil die andere Fläche im Weg ist. Kurze Strahlen prüfen für jeden Punkt, wie viel Platz um ihn herum ist, und dunkeln das Bounce-Licht dort ab. Auf Englisch heißt das **Ambient Occlusion**.

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
<tr><td><kbd>3</kbd></td><td>Anaglyph</td><td>das Bild für eine Rot-Cyan-Brille</td></tr>
<tr><td><kbd>M</kbd></td><td>Automap</td><td>die Karte</td></tr>
<tr><td><kbd>Shift</kbd> <kbd>L</kbd></td><td>Rebake light</td><td>das Licht noch einmal von vorn backen</td></tr>
</table>

Geh zurück ins Zimmer (<kbd>R</kbd>) und schau nach oben, zur Decke.

_task(decke)

Probier es aus: Schalte mit <kbd>T</kbd> die Texturen aus, dann siehst du nur noch das Licht. Schalte mit <kbd>L</kbd> und <kbd>B</kbd> abwechselnd das direkte und das Bounce-Licht aus. Schalte mit <kbd>O</kbd> die Kontaktschatten an und aus und schau dabei in eine Ecke am Boden.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib der Leuchte im Zimmer <code>size = 0.2, 0.2</code> und <code>power = 16</code> und lade neu. Eine kleinere Leuchte gibt auch weniger Licht, darum die größere Zahl. Sieh dir den Lichtfleck an, den sie durch die Tür in den Gang wirft: Seine Ränder werden scharf. Dann <code>size = 2, 2</code> mit <code>power = 0.16</code>: Sie werden ganz weich. Mit <code>bounces = 0</code> in <code>level.ini</code> unter <code>[level]</code> backt das Programm gar kein Bounce-Licht; wie sieht der Gang dann aus? Stell beides danach wieder zurück.
</div>

_task(weiche-schatten)

## Säulen, Sockel und Statuen

<div class='found-note'>
<p>In der großen Halle steht eine Frau aus Stein, sechs Meter hoch. Sie schaut zum Eingang. Ich bin dreimal um sie herumgegangen. Sie schaut immer zum Eingang.</p>
<cite>Letzter Eintrag einer Kartografin, Halle</cite>
</div>

### Blöcke

Ein Buchstabe im Grundriss kann auch ein Block sein:

```ini
[S]
block = 0.9, 6, 0.9
```

Das ist eine Säule: 0,9 m breit, 6 m hoch und 0,9 m tief, mitten auf ihrem Feld. `block = 1` füllt das ganze Feld einen Meter hoch, eine Kiste. Mit `base = 2` schwebt ein Block zwei Meter über dem Boden, mit `color = #6a5a3a` bekommt er eine eigene Farbe; ohne sieht er aus wie die Wände seines Raums. Durch Blöcke läuft man nicht hindurch, und sie werfen Schatten.

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell in der Kammer neben die Leuchte einen Block, zum Beispiel <code>[K]</code> mit <code>block = 0.5, 1.2, 0.5</code>, und sieh dir seinen Schatten an der Wand an. Was passiert mit dem Schatten, wenn du den Block weiter von der Leuchte weg stellst?
</div>

### Statuen

In `rooms/halle.room` steht Athena:

```ini
[M]
model = statues/athena.glb   # Athena Giustiniani, Abguss im SMK Kopenhagen, gemeinfrei
height = 6
rotation = 180            # zum Eingang
material = marble
```

Eine Statue ist ein 3D-Modell im Format GLB. `height` macht sie so hoch wie angegeben, `rotation` dreht sie in Grad um ihre senkrechte Achse, `material = marble` macht sie aus Marmor statt aus den Farben des Modells. Mit `pedestal = 1, 0.8, 1` steht sie auf einem Sockel, 0,8 m hoch. Auch Statuen werfen Schatten und bekommen Bounce-Licht, und wo eine Statue sich selbst verdeckt, ist es auf ihr dunkler.

Athena ist der 3D-Scan eines Gipsabgusses einer antiken Statue, aus dem Statens Museum for Kunst in Kopenhagen. Das Museum stellt solche Scans gemeinfrei zur Verfügung. Willst du eigene Modelle verwenden, nimm nur welche, deren Lizenz das erlaubt, zum Beispiel CC0, und schreib in einem Kommentar neben `model =` dazu, woher sie kommen. Mehr steht in der `README.md` der Vorlage.

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
