<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Tastatur-Shortcuts

<p class='abstract'>
Die Shortcuts aus dem <a href='/keyboard'>Keyboard-Tutorial</a> zum Nachschlagen, für Windows und Linux mit deutscher Tastatur. Was in der Spalte »wo« <em>überall</em> heißt, funktioniert auch in Word, im Browser, in E-Mail-Programmen und den meisten anderen Programmen; der Rest ist Visual Studio Code.
</p>

<div class='hint'>
Auf einem Mac nimmst du <kbd>Cmd</kbd> statt <kbd>Strg</kbd> und <kbd>Option</kbd> statt <kbd>Alt</kbd>. Wo sich ein Shortcut nach System oder Tastatur unterscheidet, steht es dabei.
</div>

## Im Text bewegen

<!-- part: bewegen -->
### Den Cursor bewegen

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th><th>wo</th></tr>
<tr><td><kbd>←</kbd> <kbd>→</kbd> <kbd>↑</kbd> <kbd>↓</kbd></td><td>ein Zeichen oder eine Zeile weiter</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>←</kbd> <kbd>Strg</kbd><kbd>→</kbd></td><td>ein Wort weiter</td><td>überall</td></tr>
<tr><td><kbd>Pos1</kbd> <kbd>Ende</kbd></td><td>an den Anfang oder das Ende der Zeile; <kbd>Pos1</kbd> zweimal springt über die Einrückung hinweg</td><td>überall</td></tr>
<tr><td><kbd>Bild↑</kbd> <kbd>Bild↓</kbd></td><td>eine Bildschirmseite weiter</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Pos1</kbd> <kbd>Strg</kbd><kbd>Ende</kbd></td><td>an den Anfang oder das Ende der Datei</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>G</kbd></td><td>zu einer Zeilennummer springen</td><td>VS Code</td></tr>
<tr><td><kbd>Strg</kbd><kbd>↑</kbd> <kbd>Strg</kbd><kbd>↓</kbd></td><td>den Bildausschnitt verschieben, der Cursor bleibt stehen</td><td>VS Code</td></tr>
<tr><td><kbd>Alt</kbd><kbd>Z</kbd></td><td>lange Zeilen am Fensterrand umbrechen, an und aus</td><td>VS Code</td></tr>
</table>
<!-- /part -->

## Markieren

<!-- part: markieren -->
### Text auswählen

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th><th>wo</th></tr>
<tr><td><kbd>Shift</kbd> + Bewegung</td><td>markiert, so weit der Cursor sich bewegt: <kbd>Shift</kbd><kbd>→</kbd> ein Zeichen, <kbd>Strg</kbd><kbd>Shift</kbd><kbd>→</kbd> ein Wort, <kbd>Shift</kbd><kbd>Ende</kbd> bis zum Zeilenende</td><td>überall</td></tr>
<tr><td>Doppelklick, Dreifachklick</td><td>ein Wort, eine ganze Zeile</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>A</kbd></td><td>alles</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>L</kbd></td><td>die ganze Zeile; noch einmal nimmt die nächste dazu</td><td>VS Code</td></tr>
</table>
<!-- /part -->

## Bearbeiten

<!-- part: bearbeiten -->
### Löschen, kopieren, rückgängig

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th><th>wo</th></tr>
<tr><td>Rücktaste, <kbd>Entf</kbd></td><td>löscht links oder rechts vom Cursor, oder die Markierung</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Rücktaste</kbd> <kbd>Strg</kbd><kbd>Entf</kbd></td><td>löscht ein ganzes Wort</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Z</kbd></td><td>rückgängig</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Shift</kbd><kbd>Z</kbd> oder <kbd>Strg</kbd><kbd>Y</kbd></td><td>wiederholen, was du rückgängig gemacht hast</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>C</kbd> <kbd>Strg</kbd><kbd>X</kbd> <kbd>Strg</kbd><kbd>V</kbd></td><td>kopieren, ausschneiden, einfügen; in VS Code ohne Markierung die ganze Zeile</td><td>überall</td></tr>
<tr><td><kbd>Strg</kbd><kbd>S</kbd></td><td>speichern; ein Punkt im Tab heißt »noch nicht gespeichert«</td><td>überall</td></tr>
</table>
<!-- /part -->

<!-- part: zeilen -->
### Ganze Zeilen

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th><th>wo</th></tr>
<tr><td><kbd>Alt</kbd><kbd>↑</kbd> <kbd>Alt</kbd><kbd>↓</kbd></td><td>verschiebt die Zeile oder die markierten Zeilen</td><td>VS Code</td></tr>
<tr><td><kbd>Shift</kbd><kbd>Alt</kbd><kbd>↓</kbd></td><td>verdoppelt die Zeile</td><td>VS Code</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Shift</kbd><kbd>K</kbd></td><td>löscht die Zeile</td><td>VS Code</td></tr>
<tr><td><kbd>Tab</kbd> <kbd>Shift</kbd><kbd>Tab</kbd></td><td>rückt die markierten Zeilen ein oder aus</td><td>VS Code</td></tr>
</table>
<!-- /part -->

## Suchen und Ersetzen

<!-- part: suchen -->
### In einer Datei suchen

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th><th>wo</th></tr>
<tr><td><kbd>Strg</kbd><kbd>F</kbd></td><td>suchen</td><td>überall</td></tr>
<tr><td><kbd>F3</kbd> <kbd>Shift</kbd><kbd>F3</kbd></td><td>nächster und voriger Treffer (in der Suche auch <kbd>Enter</kbd>)</td><td>überall</td></tr>
<tr><td><kbd>Esc</kbd></td><td>schließt die Suche</td><td>überall</td></tr>
<tr><td><kbd>Alt</kbd><kbd>C</kbd></td><td>Groß- und Kleinschreibung beachten, an und aus</td><td>VS Code</td></tr>
<tr><td><kbd>Alt</kbd><kbd>W</kbd></td><td>nur ganze Wörter finden, an und aus</td><td>VS Code</td></tr>
<tr><td><kbd>Strg</kbd><kbd>H</kbd></td><td>suchen und ersetzen; <kbd>Tab</kbd> wechselt ins Ersetzen-Feld</td><td>VS Code, Word</td></tr>
<tr><td><kbd>Enter</kbd> im Ersetzen-Feld</td><td>ersetzt den aktuellen Treffer</td><td>VS Code</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Alt</kbd><kbd>Enter</kbd></td><td>ersetzt alle Treffer</td><td>VS Code</td></tr>
</table>
<!-- /part -->

## Mehrere Stellen auf einmal

<!-- part: mehrere-cursor -->
### Mehrere Cursor

Mit mehreren Cursorn tippst du an vielen Stellen gleichzeitig. Das gibt es in Visual Studio Code, nicht in den meisten anderen Programmen.

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th></tr>
<tr><td><kbd>Strg</kbd><kbd>Alt</kbd><kbd>Shift</kbd><kbd>↓</kbd> (<kbd>↑</kbd>)</td><td>ein Cursor mehr darunter (darüber). Unter Linux <kbd>Strg</kbd><kbd>Shift</kbd><kbd>↓</kbd>, auf dem Mac <kbd>Cmd</kbd><kbd>Option</kbd><kbd>↓</kbd>.</td></tr>
<tr><td><kbd>Alt</kbd> + Klick</td><td>ein Cursor mehr an der angeklickten Stelle</td></tr>
<tr><td><kbd>Strg</kbd><kbd>D</kbd></td><td>markiert das nächste Vorkommen des markierten Worts dazu</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Shift</kbd><kbd>L</kbd></td><td>markiert alle Vorkommen auf einmal</td></tr>
<tr><td><kbd>Esc</kbd></td><td>zurück zu einem einzigen Cursor</td></tr>
</table>
<!-- /part -->

## Code

<!-- part: code -->
### Kommentieren und einrücken

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th></tr>
<tr><td><kbd>Strg</kbd><kbd>#</kbd></td><td>schaltet Kommentare für die Zeile oder die markierten Zeilen an und aus. Mit US-Tastatur <kbd>Strg</kbd><kbd>/</kbd>.</td></tr>
<tr><td><kbd>Tab</kbd> <kbd>Shift</kbd><kbd>Tab</kbd></td><td>rückt einen markierten Block ein oder aus</td></tr>
<tr><td><kbd>Alt</kbd><kbd>↑</kbd> <kbd>Alt</kbd><kbd>↓</kbd></td><td>verschiebt einen markierten Block als Ganzes; markiere dafür vollständige Zeilen</td></tr>
</table>
<!-- /part -->

## Visual Studio Code bedienen

<!-- part: fenster -->
### Dateien, Fenster und Befehle

<table class='table table-sm reference-table'>
<tr><th>Tasten</th><th>Was passiert</th></tr>
<tr><td><kbd>Strg</kbd><kbd>P</kbd></td><td>eine Datei nach ihrem Namen öffnen</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Shift</kbd><kbd>P</kbd> oder <kbd>F1</kbd></td><td>jeden Befehl von VS Code nach seinem Namen finden</td></tr>
<tr><td><kbd>Strg</kbd><kbd>B</kbd></td><td>Seitenleiste ein- und ausblenden</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Shift</kbd><kbd>E</kbd></td><td>den Explorer zeigen</td></tr>
<tr><td><kbd>Strg</kbd><kbd>J</kbd></td><td>das Terminal ein- und ausblenden</td></tr>
<tr><td><kbd>Strg</kbd><kbd>Bild↓</kbd> <kbd>Strg</kbd><kbd>Bild↑</kbd></td><td>zum nächsten und vorigen Tab</td></tr>
<tr><td><kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>Strg</kbd><kbd>O</kbd></td><td>einen Ordner öffnen</td></tr>
<tr><td><kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>F</kbd></td><td>den Ordner schließen</td></tr>
<tr><td><kbd>F2</kbd></td><td>im Explorer: umbenennen</td></tr>
<tr><td><kbd>Entf</kbd></td><td>im Explorer: löschen (mit Rückfrage)</td></tr>
</table>

<div class='hint warning'>
Der Workspace läuft im Browser, und manche Shortcuts gehören dem Browser: <kbd>Strg</kbd><kbd>W</kbd> schließt nicht den Tab in VS Code, sondern den ganzen Browser-Tab mit deinem Workspace. Schließe Dateien mit dem Kreuz im Tab. Auch <kbd>Strg</kbd><kbd>N</kbd> und <kbd>Strg</kbd><kbd>T</kbd> öffnen ein neues Browserfenster oder einen neuen Browser-Tab.
</div>
<!-- /part -->
