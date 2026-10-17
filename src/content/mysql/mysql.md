<div class='meta'>
image: select-country.webp:25:0
kind: grundlagen
language: SQL
reference: [sql, datensaetze]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Einführung in SQL

<p class='abstract'>
In diesem Kapitel lernst du, wie du MySQL-Da­ten­ban­ken abfragen kannst. Dazu verwenden wir SQL, die Structured Query Language. Wir beginnen mit einfachen Abfragen und kombinieren die einzelnen Bausteine anschließend zu eigenen Fragen an die Daten.
</p>

## Vorbereitung

### Einen Ordner anlegen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Workspace ohne geöffneten Ordner
show-left-sidebar
-->

<img class='full' src='fresh-start.webp' alt='Der Workspace ohne geöffneten Ordner, links die leere Seitenleiste mit dem Knopf Open Folder.'>

Wir erstellen nun ein Verzeichnis für unser MySQL-Experiment. Klicke dazu auf »Open Folder« und gib `sql-einfuehrung` ein:

<!-- tutorial-screenshot
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: sql-einfuehrung
crop-bottom: 55%
-->

<img class='full' src='open-folder.webp' alt='Der Dialog zum Öffnen eines Ordners, im Eingabefeld steht /workspace/sql-einfuehrung.'>

Da das Verzeichnis noch nicht existiert, wirst du gefragt, ob das Verzeichnis erstellt werden soll. Bestätige die Frage mit <kbd>Enter</kbd>:

<!-- tutorial-screenshot
press: Enter
wait-for-text: create it
crop-bottom: 83%
-->

<img class='full' src='open-folder-confirm.webp' alt='Die Rückfrage, ob der Ordner sql-einfuehrung angelegt werden soll.'>

Öffne anschließend das Terminal:

<!-- tutorial-screenshot
press: Enter
sleep: 1
click: Next Up
terminal-open
-->

<img class='full' src='got-terminal.webp' alt='Der Workspace mit dem geöffneten Ordner sql-einfuehrung und einem Terminal im unteren Bereich.'>

<img src='../common/maximize-terminal.webp' class='r' style='width: 25em;' alt='Das Symbol zum Maximieren rechts oben im Terminal.'>

Du kannst das Terminal auch maximieren, indem du auf das Symbol in der rechten oberen Ecke des Terminals klickst.

### Beispieldaten laden

Um MySQL kennenzulernen, benötigen wir eine Datenbank. Wir verwenden die Länder der Welt: alle 197 Länder mit ihren Kontinenten, gut 4.700 Städte und die Grenzen zwischen den Ländern, zusammengestellt aus [Wikidata](https://www.wikidata.org/); was genau drinsteht, beschreibt das Handbuch unter [Datensätze](/reference/datensaetze#countries). Du lädst sie herunter, indem du folgenden Befehl eingibst:

```bash_copy
wget https://github.com/specht/workspace-files/raw/main/country-archive/mysql.sql
```

Die Ausgabe sollte in etwa so aussehen:

<!-- tutorial-screenshot
terminal-maximize
terminal-run: clear
terminal-run: wget https://github.com/specht/workspace-files/raw/main/country-archive/mysql.sql
terminal-wait-for-prompt
wait-for-file: sql-einfuehrung/mysql.sql
sleep: 0.51
-->

<img class='full' src='wget.webp' alt='Das Terminal nach dem Download: wget hat die Datei mysql.sql gespeichert.'>

Klicke auf die Datei, um sie zu öffnen. Sie enthält die Befehle, die die Tabellen anlegen und mit Daten füllen, hier die Zeilen der Länder:

<!-- tutorial-screenshot
open-file: mysql.sql
press: Control+G
press: 1
press: 2
press: 1
press: Enter
press: Control+J
sleep: 0.5
-->

<img class='full' src='country-sql.webp' alt='Die Datei mysql.sql im Editor: ein INSERT-Befehl mit einer Zeile pro Land, mit Kürzel, Name, Einwohnerzahl, Fläche und Kontinent, etwa Deutschland.'>

Um die Beispieldatenbank in MySQL zu importieren, gib einfach den folgenden Befehl ein:

```bash
mysql < mysql.sql
```

Der Import geht relativ schnell. Zum Schluss lässt er die Datenbank die neuen Tabellen kurz durchsehen und meldet für jede ein `OK`:

<!-- tutorial-screenshot
press: Control+B
press: Control+J
terminal-maximize
terminal-run: clear
terminal-run: mysql < mysql.sql
terminal-wait-for-prompt
sleep: 0.5
crop-terminal-lines: auto
-->

<img class='full' src='sql-import.webp' alt='Das Terminal nach dem Import: unter einer Kopfzeile vier Zeilen, eine für jede Tabelle, jede mit dem Status OK.'>

## Die Datenbank erkunden

### mycli starten

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

<img class='full' src='mycli-prompt.webp' alt='mycli ist gestartet und wartet mit seiner Eingabeaufforderung auf einen Befehl.'>

### Tabellen und Spalten

Um dir die Tabellen in der Datenbank anzeigen zu lassen, gib den Befehl `SHOW TABLES;` ein und drücke <kbd>Enter</kbd>:

<!-- tutorial-screenshot
press: Control+L
type: SHOW TABLES;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='show-tables.webp' alt='SHOW TABLES listet die vier Tabellen border, city, continent und country.'>

Du siehst die vier Tabellen `border`, `city`, `continent` und `country`.

<div class='hint'>
SQL-Befehle schreiben wir in diesem Tutorial zur besseren Les­bar­keit in Großbuchstaben. Für MySQL spielt die Groß- und Kleinschreibung der hier ver­wen­deten SQL-Schlüsselwörter keine Rolle.
</div>

Mit `DESCRIBE` kannst du untersuchen, welche Spalten eine Tabelle besitzt:

```sql
DESCRIBE country;
```

<!-- tutorial-screenshot
press: Control+L
type: DESCRIBE country;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='describe-country.webp' alt='Die Tabelle country enthält die Spalten code, name, population, area und continent mit ihren Datentypen; code ist als Primärschlüssel markiert.'>

Jede Spalte hat einen Namen und einen Datentyp. So erkennst du, welche Informationen in der Tabelle gespeichert werden: `code` ist das Kürzel des Landes aus drei Buchstaben (`DEU` für Deutschland), `population` die Einwohnerzahl, `area` die Fläche. Die Namen der Spalten sind englisch, wie fast überall in der Programmierung; die Namen der Länder und Kontinente stehen auf Deutsch darin.

<div class='hint task tasks-off-only'>
Untersuche auch die Tabelle <code>city</code> &ndash; welche Spalten gibt es und welche Datentypen haben sie?
</div>

_task(stadt-land)

Kurz aufgeschrieben sieht die ganze Datenbank so aus, eine Zeile pro Tabelle mit ihren Spalten in Klammern:

_schema(countries)

Unterstrichen ist der **Primärschlüssel**: die Spalte, deren Wert jede Zeile eindeutig macht, in `country` das Kürzel `code`, in `city` eine Nummer, `id`. Der Name reicht bei Städten nicht, denn er kommt mehrfach vor: Springfield gibt es in den Vereinigten Staaten gleich dreimal. Der Pfeil ↑ markiert einen **Fremdschlüssel**, eine Spalte, die auf eine Zeile einer anderen Tabelle verweist: `country_code` in `city` nennt das Land, in dem die Stadt liegt, mit seinem Kürzel; `continent` in `country` den Kontinent mit seinem Namen. In `border` steht jede Grenze als Paar von zwei Ländern, und erst beide Spalten zusammen sind der Primärschlüssel. `is_capital` in `city` ist `1`, wenn die Stadt die Hauptstadt ihres Landes ist, sonst `0`.

## Abfragen mit SELECT

### Spalten auswählen

Mit `SELECT` fragst du Daten aus einer Tabelle ab. Ein Stern `*` steht dabei für alle Spalten:

```sql
SELECT * FROM country;
```

<!-- tutorial-screenshot
press: Control+L
type: SELECT * FROM country;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-country.webp' alt='Das Abfrageergebnis listet die Länder mit Kürzel, Name, Einwohnerzahl, Fläche und Kontinent auf, beginnend mit Afghanistan.'>

Du siehst nun alle Einträge in der Tabelle `country`. Eine Zeile entspricht einem Land in der Tabelle. Nutze die Pfeiltasten <kbd>←</kbd><kbd>↑</kbd><kbd>→</kbd><kbd>↓</kbd> sowie <kbd>Bild↑</kbd><kbd>Bild↓</kbd>, um durch die Ausgabe zu navigieren.

<div class='hint'>
Bei langen Ergebnissen zeigt <code>mycli</code> die Ausgabe seitenweise an. Drücke <kbd>Q</kbd>, um zur SQL-Eingabe zurückzukehren.
</div>

Meistens brauchst du nicht alle Spalten. Schreibe die gewünschten Spaltennamen hinter `SELECT`, zum Beispiel:

```sql
SELECT name, population FROM country;
```

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT name, population FROM country;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-country-columns.webp' alt='Das Abfrageergebnis enthält nur die Namen der Länder und ihre Einwohnerzahlen.'>

### Zeilen filtern: WHERE

Mit `WHERE` wählst du nur die Zeilen aus, die eine Bedingung erfüllen:

```sql
SELECT * FROM country WHERE name = 'Deutschland';
```

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM country WHERE name = 'Deutschland';
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-country-where.webp' alt='Die gefilterte Tabelle enthält nur die Zeile für Deutschland: DEU, gut 83 Millionen Einwohner, Europa.'>

Texte stehen in SQL in einfachen Anführungszeichen, zum Beispiel `'Deutschland'`.

<div class='hint task tasks-off-only'>
Wie viele Einwohner hat Griechenland? Und was bedeutet die Zahl in der Spalte <code>area</code>?
</div>

_task(griechenland)

_task(flaeche-einheit)

Bedingungen lassen sich kombinieren. Mit `AND` müssen beide Bedingungen erfüllt sein:

```sql
SELECT * FROM country
WHERE population < 5000000 AND area > 1000000;
```

<!-- tutorial-screenshot
press: Control+L
type: SELECT * FROM country WHERE population < 5000000 AND area > 1000000;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-country-where-and.webp' alt='Die kombinierte Bedingung liefert die Mongolei und Mauretanien, zwei sehr große Länder mit weniger als fünf Millionen Einwohnern.'>

Das Ergebnis enthält genau die Länder, die beide Bedingungen erfüllen. Mit `OR` genügt dagegen eine der Bedingungen, zum Beispiel:

```sql
SELECT name FROM country
WHERE continent = 'Europa' OR continent = 'Asien';
```

<!-- tutorial-screenshot
press: Control+L
type: SELECT name FROM country WHERE continent = 'Europa' OR continent = 'Asien';
press: Enter
sleep: 0.5
-->

<img class='full' src='select-country-where-or.webp' alt='Das Ergebnis listet die Namen der Länder in Europa und in Asien.'>

### Sortieren: ORDER BY

Mit `ORDER BY` sortierst du ein Abfrageergebnis. Ohne weitere Angabe wird aufsteigend sortiert:

```sql
SELECT * FROM country ORDER BY population;
```

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM country ORDER BY population;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-country-order-by.webp' alt='Die Länder sind nach Einwohnerzahl aufsteigend sortiert, oben die Vatikanstadt.'>

Für eine absteigende Sortierung ergänzt du `DESC`:

```sql
SELECT * FROM country ORDER BY population DESC;
```

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM country ORDER BY population DESC;
press: Enter
sleep: 0.5
-->

<img class='full' src='select-country-order-by-desc.webp' alt='Die Länder sind nach Einwohnerzahl absteigend sortiert, oben China und Indien.'>

_task(kleinste-laender)

### Begrenzen: LIMIT

Mit `LIMIT` begrenzt du die Anzahl der ausgegebenen Zeilen:

```sql
SELECT * FROM country LIMIT 5;
```

<!-- tutorial-screenshot
press: Q
press: Control+L
type: SELECT * FROM country LIMIT 5;
press: Enter
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-country-limit.webp' alt='Das begrenzte Abfrageergebnis enthält die ersten fünf Länder.'>

`ORDER BY` und `LIMIT` lassen sich gut kombinieren. Damit kannst du zum Beispiel nach einer Spalte sortieren und anschließend nur die ersten zehn Treffer ausgeben.

_task(top5-laender)

## Zählen und suchen

### Zeilen zählen: COUNT

`COUNT(*)` zählt, wie viele Zeilen ein Abfrageergebnis enthält:

```sql
SELECT COUNT(*) FROM country;
```

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT COUNT(*) FROM country;
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-count.webp' alt='COUNT zählt die Zeilen der Tabelle country: 197.'>

Auch `COUNT` kannst du mit `WHERE` kombinieren. So lässt sich zum Beispiel zählen, wie viele Datensätze eine bestimmte Bedingung erfüllen:

```sql
SELECT COUNT(*) FROM country WHERE continent = 'Asien';
```

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT COUNT(*) FROM country WHERE continent = 'Asien';
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-count-where.webp' alt='COUNT zählt die Länder in Asien: 49.'>

_task(hundert-millionen)

### Text durchsuchen: LIKE

Mit `LIKE` kannst du Text nach einem Muster durchsuchen. Das Prozentzeichen `%` steht dabei für beliebig viele Zeichen. Die folgende Abfrage findet Ländernamen, in denen `land` vorkommt:

```sql
SELECT name FROM country
WHERE name LIKE '%land%'
ORDER BY name;
```

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT name FROM country WHERE name LIKE '%land%' ORDER BY name;
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-like.webp' alt='LIKE findet Ländernamen, in denen die Zeichenfolge land vorkommt, von Deutschland bis Thailand.'>

`'land%'` würde dagegen nur Namen finden, die mit `land` beginnen, und `'%land'` nur Namen, die damit enden.

_task(ka-laender)

### Jeden Wert einmal: DISTINCT

Wenn du nur sehen möchtest, welche **verschiedenen** Werte in einer Spalte vorkommen, verwendest du `DISTINCT`:

```sql
SELECT DISTINCT continent FROM country
ORDER BY continent;
```

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT DISTINCT continent FROM country ORDER BY continent;
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='select-distinct.webp' alt='DISTINCT zeigt jeden Kontinent, auf dem Länder liegen, nur einmal: sechs Zeilen von Afrika bis Südamerika.'>

Sechs Kontinente, nicht sieben: Antarktika steht zwar in der Tabelle `continent`, aber kein Land liegt dort.

Hätte jemand bei einem Land `Europe` eingetragen statt `Europa`, käme hier ein siebter Kontinent heraus, und jede Abfrage nach `'Europa'` würde das Land übersehen. In dieser Datenbank kann das nicht passieren: `continent` in `country` ist ein Fremdschlüssel, und die Datenbank nimmt dort nur einen Wert an, den es in der Tabelle `continent` gibt. Probier es aus und lege ein Land auf einem Kontinent an, den es nicht gibt:

```sql
INSERT INTO country (code, name, continent)
VALUES ('ATL', 'Atlantis', 'Atlantik');
```

<!-- tutorial-screenshot
press: Control+L
terminal-run: INSERT INTO country (code, name, continent) VALUES ('ATL', 'Atlantis', 'Atlantik');
sleep: 0.5
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='insert-foreign-key.webp' alt='MySQL lehnt die neue Zeile ab: Cannot add or update a child row: a foreign key constraint fails.'>

MySQL lehnt die Zeile ab: Ein Fremdschlüssel wäre verletzt (*a foreign key constraint fails*), und nichts wird gespeichert. Jeder Kontinent steht nur einmal in der Datenbank, in `continent`; alle anderen Tabellen verweisen auf diese eine Zeile.

_task(kontinente-grosse-laender)

## In welcher Reihenfolge arbeitet die Datenbank?

Eine Abfrage schreibst du mit `SELECT` zuerst. Die Datenbank fängt aber woanders an: Sie holt zuerst die Zeilen der Tabelle aus `FROM`, wirft mit `WHERE` die weg, die nicht passen, und wählt erst danach mit `SELECT` die Spalten aus. Dann sortiert sie mit `ORDER BY` und schneidet mit `LIMIT` ab. Die Zeitlupe unten führt eine Abfrage so aus, Teil für Teil, und zeigt nach jedem Schritt die Tabelle, die gerade entstanden ist. Mit <kbd>←</kbd> und <kbd>→</kbd> gehst du durch die Schritte; die Abfrage kannst du ändern.

<div class='query-stepper' data-stepper data-query="SELECT name, ROUND(population / area) AS density
FROM country
WHERE continent = 'Europa'
ORDER BY density DESC
LIMIT 5;"></div>

<script src='/include/query_stepper.js'></script>

`density` ist hier die Bevölkerungsdichte, die Zahl der Menschen pro Quadratkilometer, gerundet. Den Namen bekommt die Spalte mit `AS` bei `SELECT`, und weil `ORDER BY` danach kommt, darf es ihn schon benutzen.

_task(reihenfolge)

_task(dichte-where)

Mehr Beispiele, auch mit `GROUP BY` und `JOIN`, die später drankommen, hat die SQL-Zeitlupe auf einer eigenen Seite:

_tool(query-stepper)

## Zusammenfassung

Du kennst jetzt die wichtigsten Bausteine, die wir für die folgenden Aufgaben benötigen:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead>
<tr><th>Baustein</th><th>Bedeutung</th></tr>
</thead>
<tbody>
<tr><td><code>SELECT</code></td><td>Welche Spalten oder berechneten Werte sollen ausgegeben werden?</td></tr>
<tr><td><code>FROM</code></td><td>Aus welcher Tabelle kommen die Daten?</td></tr>
<tr><td><code>WHERE</code></td><td>Welche Zeilen erfüllen die Bedingung?</td></tr>
<tr><td><code>AND</code> / <code>OR</code></td><td>Wie werden mehrere Bedingungen verknüpft?</td></tr>
<tr><td><code>ORDER&nbsp;BY</code></td><td>In welcher Reihenfolge werden die Zeilen ausgegeben?</td></tr>
<tr><td><code>LIMIT</code></td><td>Wie viele Zeilen werden höchstens ausgegeben?</td></tr>
<tr><td><code>COUNT(*)</code></td><td>Wie viele Zeilen enthält das Ergebnis?</td></tr>
<tr><td><code>LIKE</code></td><td>Welche Texte passen zu einem Muster?</td></tr>
<tr><td><code>DISTINCT</code></td><td>Welche unterschiedlichen Werte kommen vor?</td></tr>
</tbody>
</table>
</div>

Die Bausteine lassen sich kombinieren. Eine typische Abfrage kann beispielsweise filtern, sortieren und die Ausgabe anschließend begrenzen.

Im Handbuch: _uses(mycli, select, where, sortieren, aggregieren)

<div class='hint'>
Um <code>mycli</code> zu beenden, gib <code>exit</code> ein oder verwende <kbd>Strg</kbd><kbd>D</kbd>.
</div>

## Aufgaben

Beantworte die folgenden Fragen mit geeigneten SQL-Abfragen. Gib möglichst nur die Spalten aus, die du für die Antwort tatsächlich brauchst.

<div class='tasks-off-only'>
<p>Notiere jeweils deine Abfrage und das Ergebnis.</p>
<ol>
<li>Gib alle Länder mit Name, Einwohnerzahl und Fläche aus. Sortiere die Tabelle nach der Einwohnerzahl absteigend.</li>
<li>Wie viele Länder gibt es in der Tabelle <code>country</code>?</li>
<li>Wie viele Länder in Afrika haben mehr als 50 Millionen Einwohner?</li>
<li>Welche Länder haben die Zeichenfolge <code>arm</code> oder <code>bein</code> im Namen?</li>
<li>Welche Stadt hat die meisten Einwohner, wie viele Einwohner hat sie und in welchem Land liegt sie?</li>
<li>Wie viele Städte aus Frankreich sind in der Tabelle <code>city</code> gespeichert?</li>
<li>Gib die zehn bevölkerungsreichsten Städte Frankreichs aus.</li>
<li>Finde jeweils drei Städte, deren Name die Zeichenfolge <code>arm</code> beziehungsweise <code>heim</code> enthält.</li>
<li>Welche Städte in Deutschland liegen nördlicher als Hamburg (53,55° Nord)?</li>
<li>Welche Hauptstädte liegen auf der Südhalbkugel?</li>
</ol>
</div>

<p class='tasks-on-only'>Schreib deine Abfrage in das Feld unter der Aufgabe, so wie du sie in <code>mycli</code> eingeben würdest. Der Workspace führt sie in deiner eigenen Datenbank aus, zeigt dir dein Ergebnis und sagt, ob es passt. Er liest dabei nur; deine Daten bleiben, wie sie sind.</p>

_task(laender-sortiert)

_task(anzahl-laender)

_task(afrika-grosse-laender)

_task(arm-bein)

_task(groesste-stadt)

_task(staedte-frankreich)

_task(top10-frankreich)

_task(drei-arm)

_task(drei-heim)

_task(noerdlich-hamburg)

_task(hauptstaedte-sueden)

<!-- tutorial-step
# Jede SQL-Aufgabe nimmt ihre Lösung an und lehnt einen typischen Fehler ab, auf den Daten, die die Seite lädt
check-task: griechenland <- SELECT population FROM country WHERE name = 'Griechenland';
check-task-fails: griechenland <- SELECT population FROM country WHERE name = 'Greece';
check-task: kleinste-laender <- SELECT name, population FROM country ORDER BY population;
check-task-fails: kleinste-laender <- SELECT name, population FROM country ORDER BY population DESC;
check-task: top5-laender <- SELECT name FROM country ORDER BY population DESC LIMIT 5;
check-task-fails: top5-laender <- SELECT name FROM country ORDER BY population LIMIT 5;
check-task: hundert-millionen <- SELECT COUNT(*) FROM country WHERE population > 100000000;
check-task-fails: hundert-millionen <- SELECT COUNT(*) FROM country WHERE population > 100;
check-task: ka-laender <- SELECT name FROM country WHERE name LIKE 'Ka%';
check-task-fails: ka-laender <- SELECT name FROM country WHERE name LIKE '%Ka%';
check-task: kontinente-grosse-laender <- SELECT DISTINCT continent FROM country WHERE population > 100000000;
check-task-fails: kontinente-grosse-laender <- SELECT continent FROM country WHERE population > 100000000;
check-task: laender-sortiert <- SELECT name, population, area FROM country ORDER BY population DESC;
check-task-fails: laender-sortiert <- SELECT name, population, area FROM country ORDER BY population;
check-task: anzahl-laender <- SELECT COUNT(*) FROM country;
check-task-fails: anzahl-laender <- SELECT COUNT(*) FROM city;
check-task: afrika-grosse-laender <- SELECT COUNT(*) FROM country WHERE continent = 'Afrika' AND population > 50000000;
check-task-fails: afrika-grosse-laender <- SELECT COUNT(*) FROM country WHERE continent = 'Afrika' OR population > 50000000;
check-task: arm-bein <- SELECT name FROM country WHERE name LIKE '%arm%' OR name LIKE '%bein%';
check-task-fails: arm-bein <- SELECT name FROM country WHERE name LIKE '%arm%' AND name LIKE '%bein%';
check-task: groesste-stadt <- SELECT name, population, country_code FROM city ORDER BY population DESC LIMIT 1;
check-task-fails: groesste-stadt <- SELECT name, population, country_code FROM city ORDER BY population LIMIT 1;
check-task: staedte-frankreich <- SELECT COUNT(*) FROM city WHERE country_code = 'FRA';
check-task-fails: staedte-frankreich <- SELECT COUNT(*) FROM city WHERE country_code = 'Frankreich';
check-task: top10-frankreich <- SELECT name, population FROM city WHERE country_code = 'FRA' ORDER BY population DESC LIMIT 10;
check-task-fails: top10-frankreich <- SELECT name, population FROM city WHERE country_code = 'FRA' LIMIT 10;
check-task: drei-arm <- SELECT name FROM city WHERE name LIKE '%arm%' LIMIT 3;
check-task-fails: drei-arm <- SELECT name FROM city LIMIT 3;
check-task: drei-heim <- SELECT name FROM city WHERE name LIKE '%heim%' LIMIT 3;
check-task-fails: drei-heim <- SELECT name FROM city WHERE name LIKE 'heim%' LIMIT 3;
check-task: noerdlich-hamburg <- SELECT name FROM city WHERE country_code = 'DEU' AND latitude > 53.55;
check-task-fails: noerdlich-hamburg <- SELECT name FROM city WHERE country_code = 'DEU' AND latitude < 53.55;
check-task: hauptstaedte-sueden <- SELECT name FROM city WHERE is_capital AND latitude < 0;
check-task-fails: hauptstaedte-sueden <- SELECT name FROM city WHERE latitude < 0;
-->
