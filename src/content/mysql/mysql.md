<div class='meta'>
image: select-land.webp:25:0
</div>

# Einführung in SQL

<p class='abstract'>
In diesem Kapitel lernst du, wie du MySQL-Datenbanken abfragen kannst. Dazu verwenden wir die Programmiersprache SQL, die speziell für Datenbanken entwickelt wurde. SQL steht für Structured Query Language und wird ausgesprochen wie »Sequel«.
</p>

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Workspace ohne geöffneten Ordner
show-left-sidebar
-->

<img class='full' src='fresh-start.webp' alt=''>

Wir erstellen nun ein Verzeichnis für unser MySQL-Experiment. Klicke dazu auf »Open Folder« und gib `mysql-terra` ein:

<!-- tutorial-screenshot
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: mysql-terra
-->

<img class='full' src='open-folder.webp' alt=''>

Da das Verzeichnis noch nicht existiert, wirst du gefragt, ob das Verzeichnis erstellt werden soll. Bestätige die Frage mit <kbd>Enter</kbd>:

<!-- tutorial-screenshot
press: Enter
wait-for-text: create it
-->

<img class='full' src='open-folder-confirm.webp' alt=''>

Öffne anschließend das Terminal:

<!-- tutorial-screenshot
press: Enter
sleep: 1
click: Next Up
terminal-open
-->

<img class='full' src='got-terminal.webp' alt=''>

<img src='../common/maximize-terminal.webp' class='r' style='width: 25em;' alt=''>

Du kannst das Terminal auch maximieren, indem du auf das Symbol in der rechten oberen Ecke des Terminals klickst.

## Beispieldaten herunterladen

Um MySQL kennenzulernen, benötigen wir eine Datenbank. Wir verwenden die Beispieldatenbank `terra1`, die du dir herunterladen kannst, indem du folgenden Befehl eingibst:

```bash
wget https://github.com/specht/workspace-files/raw/main/terra1.sql
```

Die Ausgabe sollte in etwa so aussehen:

<!-- tutorial-screenshot
terminal-maximize
terminal-run: clear
terminal-run: wget https://github.com/specht/workspace-files/raw/main/terra1.sql
terminal-wait-for-prompt
wait-for-file: mysql-terra/terra1.sql
sleep: 0.5
-->

<img class='full' src='wget.webp' alt=''>

Klicke auf die Datei, um sie zu öffnen. Es befinden sich Daten zu Ländern und Städten darin.

<!-- tutorial-screenshot
open-file: terra1.sql
press: Control+G
press: 2
press: 3
press: 7
press: 7
press: Enter
press: Control+J
sleep: 0.5
-->

<img class='full' src='terra1-sql.webp' alt=''>

## Beispieldaten importieren

Um die Beispieldatenbank in MySQL zu importieren, gib einfach den folgenden Befehl ein:

```bash
mysql < terra1.sql
```

<!-- tutorial-screenshot
press: Control+B
press: Control+J
terminal-maximize
terminal-run: clear
terminal-run: mysql < terra1.sql
terminal-wait-for-prompt
sleep: 0.5
crop-terminal-lines: auto
-->

<img class='full' src='sql-import.webp' alt=''>

## Tabellenaufbau anzeigen

Um mit MySQL zu arbeiten, verwenden wir `mycli`, einen MySQL-Client, der speziell für die Kommandozeile entwickelt wurde. Starte `mycli`, indem du den folgenden Befehl eingibst:

```bash
mycli
```

Die Ausgabe sollte in etwa so aussehen:

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: mycli
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='mycli-prompt.webp' alt=''>

Um dir die Tabellen in der Datenbank anzeigen zu lassen, gib den Befehl `SHOW TABLES;` ein und drücke <kbd>Enter</kbd>:

<!-- tutorial-screenshot
press: Control+L
type: SHOW TABLES;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='show-tables.webp' alt=''>

<div class='hint'>
Die Groß- und Kleinschreibung spielt bei SQL-Befehlen keine Rolle. In diesem Artikel schreiben wir die Befehle in Großbuchstaben, um sie hervorzuheben. Du kannst sie aber auch in Kleinbuchstaben schreiben.
</div>

Du siehst nun die beiden Tabellen `land` und `ort`. Um dir den Aufbau einer Tabelle anzeigen zu lassen, kannst du den Befehl `DESCRIBE` verwenden. Gib z. B. den Befehl `DESCRIBE land;` ein, um dir den Aufbau der Tabelle `land` anzeigen zu lassen:

<!-- tutorial-screenshot
press: Control+L
type: DESCRIBE land;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='describe-land.webp' alt='Die Tabelle land enthält Spalten für Kürzel, Name, Einwohnerzahl, Fläche, Hauptstadt und Kontinent mit ihren Datentypen.'>

Du siehst nun die Spalten der Tabelle `land` und deren Datentypen.

<div class='hint task'>
Untersuche die Tabelle <code>ort</code> &ndash; welche Spalten gibt es und welche Datentypen haben sie?
</div>

## Daten anzeigen

Um dir die Daten in einer Tabelle anzeigen zu lassen, kannst du den Befehl `SELECT` verwenden. Gib z. B. den Befehl `SELECT * FROM land;` ein, um dir alle Daten in der Tabelle `land` anzeigen zu lassen:

<!-- tutorial-screenshot
press: Control+L
type: SELECT * FROM land;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-land.webp' alt='Das Abfrageergebnis listet alle Länder mit ihren gespeicherten Daten auf.'>

Du siehst nun alle Einträge in der Tabelle `land`. Eine Zeile entspricht einem Land in der Tabelle. Nutze die Pfeiltasten <kbd>←</kbd><kbd>↑</kbd><kbd>→</kbd><kbd>↓</kbd> sowie <kbd>Bild↑</kbd><kbd>Bild↓</kbd>, um durch die Tabelle zu navigieren.

<div class='hint'>
Drücke <kbd>Q</kbd>, um zur Eingabeaufforderung von <code>mycli</code> zurückzukehren (genau wie bei <code>less</code>).
</div>

Der Befehl `SELECT *` bedeutet, dass alle Spalten ausgewählt werden sollen. Wenn du nur bestimmte Spalten anzeigen möchtest, kannst du diese explizit angeben. Gib z. B. den Befehl `SELECT name, hauptstadt FROM land;` ein, um dir nur die Spalten `name` und `hauptstadt` anzeigen zu lassen:

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT name, hauptstadt FROM land;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-land-columns.webp' alt='Das Abfrageergebnis enthält nur die Namen der Länder und ihrer Hauptstädte.'>

## Zeilen filtern

Um nur bestimmte Zeilen anzuzeigen, kannst du den Befehl `WHERE` verwenden. Gib z. B. den Befehl `SELECT * FROM land WHERE name = 'Deutschland';` ein, um dir nur das Land Deutschland anzeigen zu lassen:

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM land WHERE name = 'Deutschland';
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-land-where.webp' alt='Die gefilterte Tabelle enthält nur den Datensatz für Deutschland.'>

<div class='hint'>
Statt der einfachen Anführungszeichen <code>'</code> kannst du auch doppelte Anführungszeichen <code>"</code> verwenden. Das ist besonders nützlich, wenn du Anführungszeichen in deinem Text hast, nach dem du filtern möchtest. Wichtig ist nur,
dass du am Anfang und am Ende dieselben Anführungszeichen verwendest.
</div>

<div class='hint task'>
Wie viele Einwohner hat Griechenland? Welche Zahl erhältst du und wie ist sie zu interpretieren?
</div>

Du kannst auch mehrere Bedingungen kombinieren. Gib z. B. den Befehl `SELECT * FROM land WHERE einwohner < 5 AND flaeche > 1000000;` ein, um dir alle Länder anzeigen zu lassen, die weniger als 5 Millionen Einwohner haben und eine Fläche von mehr als 1 Million Quadratkilometern:

<!-- tutorial-screenshot
press: Control+L
type: SELECT * FROM land WHERE einwohner < 5 AND flaeche > 1000000;
press: Enter
sleep: 0.5
press: Q
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-land-where-and.webp' alt='Die kombinierte Bedingung liefert zwei sehr große Länder mit weniger als fünf Millionen Einwohnern.'>

Wir sehen also nun die beiden Länder mit der niedrigsten Bevölkerungsdichte, die mindestens 1 Mio. km<sup>2</sup> groß sind.

## Zeilen sortieren

Um die Zeilen in einer Tabelle zu sortieren, kannst du den Befehl `ORDER BY` verwenden. Gib z. B. den Befehl `SELECT * FROM land ORDER BY einwohner;` ein, um dir die Länder nach ihrer Einwohnerzahl sortiert anzeigen zu lassen:

<!-- tutorial-screenshot
press: Control+L
type: SELECT * FROM land ORDER BY einwohner;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-land-order-by.webp' alt='Die Länder sind nach Einwohnerzahl aufsteigend sortiert.'>

Du siehst nun die Länder nach ihrer Einwohnerzahl sortiert. Standardmäßig wird aufsteigend sortiert. Um absteigend zu sortieren, füge das Schlüsselwort `DESC` (für »descending«) hinzu. Gib z. B. den Befehl `SELECT * FROM land ORDER BY einwohner DESC;` ein, um die Länder nach ihrer Einwohnerzahl absteigend sortiert anzeigen zu lassen:

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM land ORDER BY einwohner DESC;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-land-order-by-desc.webp' alt='Die Länder sind nach Einwohnerzahl absteigend sortiert.'>

## Ausgabe begrenzen

Um die Anzahl der Zeilen zu begrenzen, die angezeigt werden, kannst du den Befehl `LIMIT` verwenden. Gib z. B. den Befehl `SELECT * FROM land LIMIT 5;` ein, um dir nur die ersten 5 Länder anzeigen zu lassen:

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM land LIMIT 5;
press: Enter
sleep: 0.5
press: Q
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-land-limit.webp' alt='Das begrenzte Abfrageergebnis enthält die ersten fünf Länder.'>

## Zusammenfassung

Du hast jetzt die einzelnen Bestandteile einer SQL-Abfrage kennengelernt:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead>
<tr><th>Befehl</th><th>Bedeutung</th></tr>
</thead>
<tbody>
<tr><td><code>SELECT</code></td><td>Was / Welche Spalten sollen angezeigt werden?</td></tr>
<tr><td><code>FROM</code></td><td>Woher / Aus welcher Tabelle sollen die Daten kommen?</td></tr>
<tr><td><code>WHERE</code></td><td>Filter / Welche Zeilen sollen angezeigt werden?</td></tr>
<tr><td><code>ORDER&nbsp;BY</code></td><td>Sortierung / In welcher Reihenfolge sollen die Zeilen angezeigt werden?</td></tr>
<tr><td><code>LIMIT</code></td><td>Anzahl / Wie viele Zeilen sollen angezeigt werden?</td></tr>
</tbody>
</table>
</div>

Du kannst diese Befehle kombinieren, um genau die Daten zu erhalten, die du benötigst.

<div class='hint'>
Um <code>mycli</code> zu beenden, gib einfach den Befehl <code>exit</code> ein oder verwende die Tastenkombination <kbd>Strg</kbd><kbd>D</kbd>.
</div>

### Aufgaben

Beantworte die folgenden Fragen, indem du eine geeignete SQL-Abfrage formulierst. Notiere jeweils die Abfrage und dein Ergebnis. Formuliere die Abfrage möglichst so, dass du nur die benötigten Spalten erhältst.

1. Gib eine Tabelle aller Länder aus. Dabei sollen Name, Einwohner und die Hauptstadt angezeigt werden und die Tabelle soll nach der Einwohnerzahl absteigend sortiert sein.

2. Wie viele Länder gibt es in der Tabelle `land`?<br>_Hinweis: Du kannst bei <code>SELECT</code> statt Spaltennamen auch Funktionen verwenden, z. B. <code>SELECT COUNT(Name) ...</code> statt <code>SELECT Name ...</code>_.

3. Wie viele Länder liegen in Europa?

4. Welche Länder haben das Wort »arm« oder »bein« im Namen?<br>_Hinweis: Du kannst nicht nur nach exakten Begriffen suchen, sondern auch nach Teilen von Begriffen, z. B. <code>WHERE Name LIKE '%land%'</code>, wenn du nach allen Ländern suchen möchtest, die den Begriff »land« im Namen haben._

5. Welcher Ort hat die meisten Einwohner, wie viele Einwohner sind es und in welchem Land liegt dieser Ort?

6. Wie viele Orte gibt es in Frankreich?<br>_Hinweis: Es gibt natürlich mehr Orte in Frankreich, als in dieser Tabelle stehen, aber wir wollen diese Frage in Bezug auf die uns zur Verfügung stehenden Daten beantworten._

7. Gib die 10 bevölkerungsreichsten Orte in Frankreich aus.

8. Gib jeweils drei Orte an, die den Begriff »arm« oder »bein« im Namen haben.

9. Welche Orte in Deutschland liegen westlich von Aachen (6,046° Ost)?