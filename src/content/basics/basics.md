<div class='meta'>
image: vs-code-welcome.webp:0:50
</div>

# Die Basics

<p class='abstract'>
Eine kleine Tour durch die Benutzeroberfläche von Visual Studio Code, gefolgt von ein paar grundlegenden Hinweisen zur Arbeit mit Dateien und Verzeichnissen. Lerne die wichtigsten Shortcuts und Funktionen zum Bearbeiten von und Navigieren innerhalb von Textdateien kennen.
</p>

Wenn du den Workspace öffnest, siehst du die Entwicklungsumgebung Visual Studio Code:

<img class='full' src='vs-code-welcome.webp' data>

## Die Benutzeroberfläche

Am linken Rand sieht du einige Icons, die wichtig sind:

<table class='table'>
<td><img class='sq-icon' src='explorer.webp'></td>
<td>Explorer (<span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>E</span>) &ndash; hier siehst du alle Dateien und Unterordner des aktuell geöffneten Projekts</td>
</tr>
<tr>
<td><img class='sq-icon' src='search.webp'></td>
<td>Search (<span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>F</span>) &ndash; hier kannst du in allen Dateien innerhalb deines Projektes suchen</td>
</tr>
<tr>
<td><img class='sq-icon' src='source-control.webp'></td>
<td>Source Control (<span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>G</span>) &ndash; hier geht es um Versionsverwaltung mit Git</td>
</tr>
<tr>
<td><img class='sq-icon' src='run-and-debug.webp'></td>
<td>Run and Debug (<span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>D</span>) &ndash; hier können selbst geschriebene Programme ausgeführt werden</td>
</tr>
<tr>
<td><img class='sq-icon' src='extensions.webp'></td>
<td>Extensions (<span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>X</span>) &ndash; hier findest du viele nützliche Erweiterungen</td>
</tr>
</table>

Rechts oben siehst du mehrere Buttons, mit denen du das Layout anpassen oder verschiedene Bereiche ein- und ausblenden kannst:

<table class='table'>
<tr>
<td><img class='sq-icon' src='customize-layout.webp'></td>
<td>Layout anpassen</td>
</tr>
<tr>
<td style='width: 1em;'><img class='sq-icon' src='primary-side-bar.webp'></td>
<td>Linke Seitenleiste (<span class='key'>Strg</span><span class='key'>B</span>)</td>
</tr>
<tr>
<td><img class='sq-icon' src='panel.webp'></td>
<td>Panel (<span class='key'>Strg</span><span class='key'>J</span>) &ndash; hier ist für uns vor allem das Terminal interessant</td>
</tr>
<tr>
<td><img class='sq-icon' src='secondary-side-bar.webp'></td>
<td>Rechte Seitenleiste (<span class='key'>Strg</span><span class='key'>Alt</span><span class='key'>B</span>)</td>
</tr>
</table>

## Dateien, Verzeichnisse, Pfade

<img src='directory-tree.webp' class='r' style='width: 15em;'>

Wenn du am Computer arbeitest, speicherst du deine Arbeit in Dateien. Jede Datei hat einen Dateinamen (der normalerweise auch eine Erweiterung wie z. B. `.jpg` oder `.html` enthält) und befindet sich in einem Verzeichnis, das sich wiederum in einem anderen Verzeichnis befinden kann. So entsteht ein Verzeichnisbaum.

Der vollständige Pfad zu einer Datei setzt sich dann aus allen Verzeichnissen und dem Dateinamen zusammen, z. B:

`/workspace/pictures/monkey.jpg`

Unter Linux ist das oberste Verzeichnis immer `/`, unter Windows beginnt es immer mit einem Laufwerksbuchstaben wie z. B. `C:\`. Ein weiterer Unterschied zu Linux ist, dass unter Windows `\` statt `/` verwendet wird, um Verzeichnisse im Pfad voneinander abzutrennen.

Im Hackschule Workspace ist es so, dass du deine Projekte und Dateien im Verzeichnis `/workspace` ablegen kannst – es ist dein Home-Verzeichnisd. Du kannst dir in diesem Verzeichnis eine beliebige Verzeichnisstruktur anlegen.

## Shortcuts

Zum Schluss noch ein paar Tipps zur Tastatur: es gibt einige praktische Funktionen, die es quasi überall gibt:

<table class='table'>
<tr>
<td style='width: 1em;'><span class='key'>Strg</span><span class='key'>C</span></td>
<td>Kopieren (copy)</td>
</tr>
<tr>
<td><span class='key'>Strg</span><span class='key'>V</span></td>
<td>Einfügen (paste)</td>
</tr>
<tr>
<td><span class='key'>Strg</span><span class='key'>X</span></td>
<td>Ausschneiden (cut)</td>
</tr>
<tr>
<td><span class='key'>Strg</span><span class='key'>Z</span></td>
<td>Rückgängig (undo)</td>
</tr>
</table>

<div class='hint'>
<p>
Dazu noch ein wichtiger Hinweis: In den unteren Ecken deiner Tastatur findest du ein paar sogenannte <em>Modifikatortasten</em>, z. B.
<span class='key'>Strg</span>,
<span class='key'>Shift</span>,
<span class='key'>Alt</span>,
<span class='key'>Win</span> und
<span class='key'>AltGr</span>. Diese Tasten haben meistens keinen Effekt, wenn man sie alleine drückt, sondern sie werden für Tastenkombinationen verwendet, die meistens aus einer oder mehreren Hilfstasten und einer »richtigen« Taste bestehen. Um einen ausgewählten Text mit <span class='key'>Strg</span><span class='key'>C</span> zu kopieren, musst du also erst <span class='key'>Strg</span> gedrückt halten und dann einmal <span class='key'>C</span> tippen.
</p>
<p>
Du kannst Modifikatortasten auch verwenden, um Sonderzeichen einzugeben, genauso wie du Großbuchstaben mit <span class='key'>Shift</span> (auch: Umschalttaste) eingeben kannst.
</p>
</div>


## Bearbeiten von Text

Gerade am Anfang kann es sein, dass du beim Tippen und Navigieren in einer Datei relativ langsam bist. Es ist aber wie mit so vielen Dingen: mit der Zeit kommt die Übung. 🤠 Wenn du eine gewissen Übung hast, bist du mit der Tastatur oft schneller, als wenn du erst zur Maus greifen musst, um an eine bestimmte Stelle zu klicken.

Du kannst Webseiten wie [keybr.com](https://www.keybr.com/) nutzen, um deine Tippgeschwindigkeit zu trainieren. Darüberhinaus wirst du schneller, wenn du ein paar wesentliche Tasten deiner Tastatur kennenlernst:

### Navigation

Die folgenden Shortcuts funktionieren nicht nur in Visual Studio Code, sondern in fast allen anderen Programmen:

<table class='table'>
<tr>
<td style='width: 14em;'><span class='key'>←</span> / <span class='key'>↑</span> / <span class='key'>→</span> / <span class='key'>↓</span></td>
<td>bewegt den Cursor</td>
</tr>
<tr>
<td><span class='key'>Pos1</span> / <span class='key'>Ende</span></td>
<td>springt an den Anfang / das Ende einer Zeile</td>
</tr>
<tr>
<td><span class='key'>Bild↑</span> / <span class='key'>Bild↓</span></td>
<td>springt eine Seite hoch / runter</td>
</tr>
<tr>
<td><span class='key'>Strg</span><span class='key'>←</span> / <span class='key'>Strg</span><span class='key'>→</span></td>
<td>springt zum vorherigen / nächsten Wort</td>
</tr>
<tr>
<td><span class='key'>Strg</span><span class='key'>Pos1</span> / <span class='key'>Strg</span><span class='key'>Ende</span></td>
<td>springt an den Anfang / das Ende des Dokuments</td>
</tr>
<tr>
</table>

### Text auswählen und löschen

Halte <span class='key'>Shift</span> gedrückt, während du durch einen Text navigierst, um Text zu markieren.

Zum Löschen von einzelnen Zeichen gibt es zwei verschiedene Tasten, deren Unterschied man kennen sollte: <span style='width: 3em; display: inline-block;' class='key'>⟵</span> (Backspace) löscht das Zeichen links vom Cursor, während <span class='key'>Entf</span> das Zeichen rechts neben dem Cursor löscht. Oft gibt es Shortcuts, um ganze Zeilen zu löschen – in Visual Studio Code mit <span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>K</span>.


### Tastaturlayouts

Vielleicht fragst du dich irgendwann, wieso beim Programmieren oft so viele merkwürdige und kompliziert zu tippende Zeichen einzugeben sind. Denke bitte daran, dass die Welt der Computer und Programmiersprachen vor allem eine US-amerikanisch geprägte Welt ist. 
Die fehlenden Umlaute in der englischen Sprache bieten die Möglichkeit, die Tastatur anders zu belegen und Zeichen wie `[`, `]`, `{`, `}`, `@`, `\`, `~` und `|` direkt zu erreichen, ohne dass man sich dabei die Finger verrenken muss. Das bedeutet, dass die deutsche Tastaturbelegung nicht unbedingt die beste Tastaturbelegung für das Programmieren ist.

Hier siehst du eine Tastatur mit US-Layout:

<img class='full' src='us-layout.webp'>

 Es gibt spezielle Tastaturlayouts, die extra für Programmiererinnen und Programmierer entwickelt wurden, z. B. das [Neo-Tastaturlayout](https://neo-layout.org/), das [Colemak-Layout](https://colemak.com/) oder das [Dvorak-Layout](https://dvorak-keyboard.com/). Wenn du dich für das Thema interessierst, kannst du dich gerne mal damit beschäftigen. Es ist aber kein Muss, um programmieren zu lernen. Viel wichtiger ist es, dass du programmieren übst, egal mit welcher Tastatur. 🤓
