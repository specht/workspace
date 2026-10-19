<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# SQL

<p class='abstract'>
Abfragen und Änderungen für MySQL zum Nachschlagen: Daten auswählen, filtern, sortieren, zählen und verbinden, Tabellen anlegen und füllen, und wie ein Programm Werte sicher in eine Abfrage gibt. Die Beispiele stammen aus den Datensätzen der Aktivitäten: die Länder der Welt und die Filmdatenbank (siehe <a href='/reference/datensaetze'>Datensätze</a>).
</p>

## Mit der Datenbank arbeiten

<!-- part: mycli -->
<!-- short: `mycli` öffnet deine Datenbank; `;` schließt eine Abfrage ab, <kbd>Strg</kbd><kbd>D</kbd> beendet. -->
### mycli

`mycli` ist ein MySQL-Client für das Terminal. Er meldet sich mit deinen Zugangsdaten aus dem Profil von selbst an und öffnet deine Datenbank:

```bash
mycli
```

- Eine Abfrage endet mit `;`, erst dann führt `mycli` sie aus. Lange Abfragen dürfen über mehrere Zeilen gehen.
- Beim Tippen schlägt `mycli` Befehle, Tabellen und Spalten vor; <kbd>Tab</kbd> übernimmt den Vorschlag.
- Lange Ergebnisse zeigt `mycli` seitenweise; <kbd>Q</kbd> führt zurück zur Eingabe.
- `exit` oder <kbd>Strg</kbd><kbd>D</kbd> beendet `mycli`.

Eine weitere Datenbank aus deinem Profil öffnest du mit ihrem Namen, eine Datei mit SQL führst du aus, indem du sie an `mysql` gibst:

```bash
mycli db_k7q4bcd9fghj
mysql < film.sql
```
<!-- /part -->

<!-- part: tabellen-ansehen -->
<!-- short: `SHOW TABLES;` listet die Tabellen, `DESCRIBE country;` zeigt Spalten und Typen. -->
### Tabellen und Spalten ansehen

```sql
SHOW TABLES;
DESCRIBE country;
```

`SHOW TABLES` listet die Tabellen der Datenbank, `DESCRIBE` die Spalten einer Tabelle mit ihren Datentypen, ob sie leer bleiben dürfen (`NULL`) und welche zum Schlüssel gehören (`PRI`, `MUL`).
<!-- /part -->

## Abfragen

<!-- part: select -->
<!-- short: `SELECT spalten FROM tabelle;` – `*` für alle Spalten, `AS` für einen anderen Namen. -->
### SELECT und FROM

```sql
SELECT name, population FROM country;
SELECT * FROM country;
SELECT name, population / area AS density FROM country;
```

Nach `SELECT` stehen die Spalten, die das Ergebnis haben soll, nach `FROM` die Tabelle. `*` heißt »alle Spalten«. Eine Spalte kann auch ein berechneter Wert sein; `AS` gibt ihr einen Namen.
<!-- /part -->

<!-- part: where -->
<!-- short: `WHERE` filtert Zeilen: `=`, `<>`, `<`, `AND`, `OR`, `IN (…)`, `BETWEEN`, `LIKE`, `IS NULL`. -->
### Zeilen filtern: WHERE

```sql
SELECT * FROM country WHERE continent = 'Europa';
SELECT name FROM country WHERE population < 5000000 AND area > 1000000;
SELECT name FROM country WHERE continent IN ('Europa', 'Asien');
SELECT name FROM country WHERE name LIKE '%land%';
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Schreibweise</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>=</code> <code>&lt;&gt;</code> <code>&lt;</code> <code>&lt;=</code> <code>&gt;</code> <code>&gt;=</code></td><td>gleich, ungleich, kleiner, kleiner oder gleich, …</td></tr>
<tr><td><code>AND</code> <code>OR</code> <code>NOT</code></td><td>Bedingungen verknüpfen; Klammern legen fest, was zuerst gilt: <code>(a OR b) AND c</code></td></tr>
<tr><td><code>IN ('Europa', 'Asien')</code></td><td>einer der Werte</td></tr>
<tr><td><code>BETWEEN 10 AND 20</code></td><td>von 10 bis 20, beide eingeschlossen</td></tr>
<tr><td><code>LIKE '%land%'</code></td><td>Textmuster: <code>%</code> steht für beliebig viele Zeichen, <code>_</code> für genau eines</td></tr>
<tr><td><code>IS NULL</code> · <code>IS NOT NULL</code></td><td>kein Wert eingetragen. <code>= NULL</code> trifft nie zu.</td></tr>
</tbody>
</table>
</div>

Texte stehen in einfachen Anführungszeichen, Zahlen ohne.
<!-- /part -->

<!-- part: sortieren -->
<!-- short: `ORDER BY spalte DESC` sortiert absteigend, `LIMIT 10` nimmt die ersten zehn Zeilen. -->
### Sortieren und begrenzen: ORDER BY, LIMIT

```sql
SELECT name, population FROM country ORDER BY population DESC LIMIT 10;
SELECT name, continent FROM country ORDER BY continent, name;
```

`ORDER BY` sortiert aufsteigend, mit `DESC` absteigend; nach mehreren Spalten, wenn die erste gleich ist. `LIMIT` gibt höchstens so viele Zeilen aus. Ohne `ORDER BY` ist die Reihenfolge nicht festgelegt.
<!-- /part -->

<!-- part: aggregieren -->
<!-- short: `COUNT(*)`, `SUM`, `AVG`, `MIN`, `MAX`; `GROUP BY` bildet Gruppen, `HAVING` filtert sie. -->
### Zählen und zusammenfassen

```sql
SELECT COUNT(*) FROM country WHERE continent = 'Asien';
SELECT DISTINCT continent FROM country;
SELECT continent, COUNT(*) AS countries, SUM(population) AS people
FROM country
GROUP BY continent
HAVING COUNT(*) >= 10
ORDER BY countries DESC;
```

`COUNT(*)` zählt Zeilen, `SUM`, `AVG`, `MIN` und `MAX` rechnen über eine Spalte. `DISTINCT` gibt jeden Wert nur einmal aus. `GROUP BY` fasst Zeilen mit gleichem Wert zu einer Gruppe zusammen; dann rechnen die Funktionen je Gruppe. `WHERE` filtert Zeilen vor dem Gruppieren, `HAVING` danach die Gruppen.
<!-- /part -->

<!-- part: join -->
<!-- short: `JOIN tabelle ON a.id = b.a_id` setzt Zeilen aus zwei Tabellen zusammen, über den Fremdschlüssel. -->
### Tabellen verbinden: JOIN

```sql
SELECT movie.title, genre.name
FROM movie
JOIN movie_genre ON movie.id = movie_genre.movie_id
JOIN genre ON movie_genre.genre_id = genre.id
WHERE movie.title = 'Back to the Future';
```

`JOIN … ON …` setzt jede Zeile der einen Tabelle mit den Zeilen der anderen zusammen, für die die Bedingung gilt, meist Fremdschlüssel = Primärschlüssel. Spalten schreibt man dann mit ihrer Tabelle davor (`movie.title`). Braucht eine Abfrage dieselbe Tabelle zweimal, bekommt sie Namen: `JOIN job AS ja ON …`. `LEFT JOIN` behält auch Zeilen ohne Partner; deren Spalten aus der anderen Tabelle sind dann `NULL`.
<!-- /part -->

<!-- part: entfernung -->
<!-- short: `ST_Distance_Sphere(POINT(länge, breite), POINT(länge, breite))` gibt die Entfernung auf der Erdkugel in Metern. -->
### Entfernungen auf der Erde

```sql
SELECT b.name, ROUND(ST_Distance_Sphere(POINT(a.longitude, a.latitude), POINT(b.longitude, b.latitude)) / 1000) AS km
FROM city AS a
JOIN city AS b ON b.id <> a.id
WHERE a.name = 'Berlin'
ORDER BY km
LIMIT 5;
```

`POINT(x, y)` macht aus zwei Zahlen einen Punkt, auf der Erde mit der Länge (`longitude`) zuerst und der Breite (`latitude`) danach. `ST_Distance_Sphere` gibt die Entfernung zweier Punkte auf einer Kugel vom Radius der Erde, in Metern, also Luftlinie. `CROSS JOIN` setzt jede Zeile der einen Tabelle mit jeder der anderen zusammen; `JOIN … ON b.id <> a.id` tut dasselbe ohne die Paare einer Stadt mit sich selbst. Für viele Millionen Punkte bekommt die Lage eine eigene Spalte vom Typ `POINT` mit einem `SPATIAL INDEX`, damit die Datenbank nicht jede Zeile ausrechnen muss.
<!-- /part -->

## Tabellen und Daten ändern

<!-- part: create-table -->
<!-- short: `CREATE TABLE name (spalte TYP, …, PRIMARY KEY (id));` – `AUTO_INCREMENT` zählt selbst. -->
### Tabellen anlegen: CREATE TABLE

```sql
CREATE TABLE entry (
  id       INT NOT NULL AUTO_INCREMENT,
  name     VARCHAR(80) NOT NULL,
  message  TEXT NOT NULL,
  created  DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (id)
);
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Schreibweise</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>INT</code> · <code>DECIMAL(6,2)</code></td><td>ganze Zahl · Kommazahl mit 6 Stellen, davon 2 nach dem Komma</td></tr>
<tr><td><code>VARCHAR(80)</code> · <code>TEXT</code></td><td>Text bis 80 Zeichen · langer Text</td></tr>
<tr><td><code>DATE</code> · <code>DATETIME</code></td><td>Datum · Datum mit Uhrzeit</td></tr>
<tr><td><code>NOT NULL</code></td><td>Die Spalte braucht immer einen Wert.</td></tr>
<tr><td><code>DEFAULT CURRENT_TIMESTAMP</code></td><td>ohne Angabe der Zeitpunkt des Einfügens</td></tr>
<tr><td><code>AUTO_INCREMENT</code></td><td>MySQL vergibt die nächste Nummer selbst.</td></tr>
<tr><td><code>PRIMARY KEY (id)</code></td><td>bestimmt jede Zeile eindeutig</td></tr>
<tr><td><code>UNIQUE (name)</code></td><td>kein Wert doppelt</td></tr>
<tr><td><code>FOREIGN KEY (movie_id) REFERENCES movie (id)</code></td><td>Fremdschlüssel: nur Werte, die es in <code>movie.id</code> gibt</td></tr>
</tbody>
</table>
</div>

`DROP TABLE entry;` löscht die Tabelle samt Inhalt. Wie ein ER-Modell zu Tabellen wird, steht unter [ER-Modelle](/reference/er-modelle).
<!-- /part -->

<!-- part: insert -->
<!-- short: `INSERT INTO tabelle (spalten) VALUES (werte);` fügt eine Zeile ein. -->
### Zeilen einfügen: INSERT

```sql
INSERT INTO entry (name, message) VALUES ('Ada', 'Schönes Gästebuch!');
INSERT INTO entry (name, message) VALUES ('Alan', 'Hallo'), ('Grace', 'Hi');
```

Nach dem Tabellennamen stehen die Spalten, nach `VALUES` die Werte in derselben Reihenfolge, je Zeile ein Paar Klammern. Spalten mit `AUTO_INCREMENT` oder `DEFAULT` darf man weglassen.
<!-- /part -->

<!-- part: update-delete -->
<!-- short: `UPDATE … SET … WHERE …` ändert, `DELETE FROM … WHERE …` löscht; ohne `WHERE` alle Zeilen. -->
### Ändern und löschen: UPDATE, DELETE

```sql
UPDATE entry SET message = 'Schönes Gästebuch :)' WHERE id = 1;
DELETE FROM entry WHERE id = 2;
```

Beide wirken auf jede Zeile, auf die `WHERE` zutrifft. **Ohne `WHERE` ändert oder löscht der Befehl alle Zeilen.** Wer unsicher ist, schreibt zuerst ein `SELECT` mit derselben Bedingung und sieht nach, welche Zeilen getroffen werden.
<!-- /part -->

## SQL in Programmen

<!-- part: platzhalter -->
<!-- short: Werte von außen nie in den SQL-Text kleben: Platzhalter `?` und die Werte getrennt übergeben. -->
### Werte über Platzhalter

Ein Programm, das eine Abfrage aus Text und Eingaben zusammenklebt, lässt die Eingabe mitbestimmen, was die Datenbank tut: Ein Anführungszeichen im Namen beendet den Text im SQL, und was danach kommt, liest MySQL als SQL. Das heißt **SQL-Injection**. Die Abhilfe ist immer dieselbe: Die Abfrage enthält Platzhalter, die Werte gehen getrennt an die Datenbank und werden nie als SQL gelesen.

```php
// PHP mit PDO
$stmt = $db->prepare('INSERT INTO entry (name, message) VALUES (?, ?)');
$stmt->execute([$name, $message]);
```

```ruby
# Ruby mit mysql2
client.prepare('SELECT * FROM entry WHERE name = ?').execute(name)
```

```python
# Python mit mysql.connector
cursor.execute('SELECT * FROM entry WHERE name = %s', (name,))
```

Platzhalter stehen nur für Werte, nicht für Tabellen- oder Spaltennamen. Muss ein Programm einen Namen wählen, nimmt es ihn aus einer festen Liste erlaubter Namen.
<!-- /part -->

<!-- part: fehler -->
<!-- short: Fehlermeldungen von MySQL mit ihrer Nummer: 1064 Schreibweise, 1146 Tabelle fehlt, 1054 Spalte fehlt, … -->
## Wenn MySQL etwas meldet

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Meldung</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>1064</code> You have an error in your SQL syntax</td><td>Die Schreibweise stimmt nicht. Die Meldung zitiert die Stelle, ab der MySQL nichts mehr versteht; oft fehlt kurz davor ein Komma, eine Klammer oder ein Anführungszeichen.</td></tr>
<tr><td><code>1146</code> Table … doesn't exist</td><td>Die Tabelle gibt es in dieser Datenbank nicht: Tippfehler, andere Datenbank, oder die Daten sind noch nicht geladen.</td></tr>
<tr><td><code>1054</code> Unknown column …</td><td>Die Spalte gibt es nicht. <code>DESCRIBE</code> zeigt die richtigen Namen. Auch ein Text ohne Anführungszeichen wird als Spaltenname gelesen.</td></tr>
<tr><td><code>1062</code> Duplicate entry …</td><td>Der Wert steht schon in einer Spalte mit <code>PRIMARY KEY</code> oder <code>UNIQUE</code>.</td></tr>
<tr><td><code>1452</code> Cannot add or update a child row</td><td>Ein Fremdschlüssel verweist auf eine Zeile, die es nicht gibt.</td></tr>
<tr><td><code>1045</code> · <code>1044</code> Access denied</td><td>Falscher Benutzername oder falsches Passwort, oder dieser Zugang darf die Datenbank nicht benutzen.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Zum Nachschlagen

- [MySQL 8.4 Reference Manual](https://dev.mysql.com/doc/refman/8.4/en/): das offizielle Handbuch zur Version im Workspace, auf Englisch. Jede Anweisung mit allen Möglichkeiten steht unter [SQL Statements](https://dev.mysql.com/doc/refman/8.4/en/sql-statements.html), die Datentypen unter [Data Types](https://dev.mysql.com/doc/refman/8.4/en/data-types.html).
- [Functions and Operators](https://dev.mysql.com/doc/refman/8.4/en/functions.html): alle Funktionen für Texte, Zahlen, Datum und Zusammenfassen; `ST_Distance_Sphere` steht bei den [Spatial Convenience Functions](https://dev.mysql.com/doc/refman/8.4/en/spatial-convenience-functions.html).
- [Server Error Message Reference](https://dev.mysql.com/doc/mysql-errors/8.4/en/server-error-reference.html): jede Fehlernummer mit ihrer Meldung, wenn sie oben nicht steht.
- [mycli](https://www.mycli.net/): Tastenkürzel, Einstellungen und Spezialbefehle des Clients.
- Platzhalter in Programmen: [Prepared Statements mit PDO](https://www.php.net/manual/de/pdo.prepared-statements.php) (PHP, auf Deutsch), [mysql2](https://github.com/brianmario/mysql2) (Ruby) und [Connector/Python](https://dev.mysql.com/doc/connector-python/en/) (Python).
