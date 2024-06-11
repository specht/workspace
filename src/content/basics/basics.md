<div class='meta'>
section: programming
</div>

# Die Basics

<p class='abstract'>
Mach dich mit Visual Studio Code vertraut und lerne die wichtigsten Funktionen kennen.
</p>

Wenn du den Workspace öffnest, siehst du die Entwicklungsumgebung Visual Studio Code:

<img class='full' src='vs-code-welcome.webp'>

## Die Benutzeroberfläche

Am linken Rand sieht du einige Icons, die wichtig sind:

<style>
    .table td {
        white-space: normal;
    }
</style>

<table class='table'>
<tr>
<td style='width: 1em;'><img class='sq-icon' src='menu.webp'></td>
<td>Das Menü</td>
</tr>
<tr>
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

Es gibt an mehreren Rändern Bereiche, die du ein- und ausblenden kannst. Nutze dafür die Buttons rechts oben:

<table class='table'>
<tr>
<td style='width: 1em;'><img class='sq-icon' src='primary-side-bar.webp'></td>
<td>Primary Side Bar (<span class='key'>Strg</span><span class='key'>B</span>)</td>
</tr>
<tr>
<td><img class='sq-icon' src='panel.webp'></td>
<td>Panel (<span class='key'>Strg</span><span class='key'>J</span>) &ndash; hier ist für uns vor allem das Terminal interessant</td>
</tr>
<tr>
<td><img class='sq-icon' src='secondary-side-bar.webp'></td>
<td>Secondary Side bar (<span class='key'>Strg</span><span class='key'>Alt</span><span class='key'>B</span>)</td>
</tr>
</table>

## Pfade, Dateien, Verzeichnisse

<img src='directory-tree.webp' class='r' style='width: 15em;'>

Wenn du am Computer arbeitest, speicherst du deine Arbeit in Dateien. Jede Datei hat einen Dateinamen (der normalerweise auch eine Erweiterung enthält) und befindet sich in einem Verzeichnis, das sich wiederum in einem anderen Verzeichnis befinden kann. So entsteht ein Verzeichnisbaum.

Der vollständige Pfad zu einer Datei setzt sich dann aus allen Verzeichnissen und dem Dateinamen zusammen, z. B:

`/home/abc/shower.js/pictures/monkey.jpg`

Unter Linux ist das oberste Verzeichnis immer `/`, unter Windows beginnt es immer mit einem Laufwerksbuchstaben wie z. B. `C:\`. Ein weiterer Unterschied zu Linux ist, dass unter Windows `\` statt `/` verwendet wird, um Verzeichnisse im Pfad voneinander abzutrennen.

Im Hackschule Workspace ist es so, dass du deine Projekte und Dateien im Verzeichnis `/workspace` ablegen kannst. Du kannst dir in diesem Verzeichnis eine beliebige Verzeichnisstruktur anlegen.

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

Dazu noch ein wichtiger Hinweis: In den unteren Ecken deiner Tastatur findest du ein paar sogenannte _Hilfstasten_, z. B.
<span class='key'>Strg</span>,
<span class='key'>Shift</span>,
<span class='key'>Alt</span>,
<span class='key'>Win</span> und
<span class='key'>AltGr</span>. Diese Tasten haben meistens keinen Effekt, wenn man sie alleine drückt, sondern sie werden für Tastenkombinationen verwendet, die meistens aus einer oder mehreren Hilfstasten und einer »richtigen« Taste bestehen. Um einen ausgewählten Text mit <span class='key'>Strg</span><span class='key'>C</span> zu kopieren, musst du also erst <span class='key'>Strg</span> gedrückt halten und dann einmal <span class='key'>C</span> tippen.

Du kannst Hilfstasten auch verwenden, um Sonderzeichen einzugeben, genauso wie du Großbuchstaben mit <span class='key'>Shift</span> (auch: Umschalttaste) eingeben kannst.
