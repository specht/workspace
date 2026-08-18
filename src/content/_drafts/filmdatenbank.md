
<div class='meta'>
image: erd-movie-genre.webp:0:50
</div>

# Filmdatenbank mit MySQL

<p class='abstract'>
Im letzten Kapitel hast du eine vorhandene Datenbank mit <code>SELECT</code>, <code>WHERE</code>, <code>ORDER BY</code>, <code>COUNT</code> und weiteren SQL-Befehlen abgefragt. Jetzt geht es um die Frage, wie eine relationale Datenbank überhaupt aufgebaut wird. Du modellierst eine Filmdatenbank aus echten, aufbereiteten IMDb-Daten, lernst n:m-Beziehungen und Fremdschlüssel kennen und verbindest die Tabellen anschließend mit <code>JOIN</code>. Am Ende wird das Modell um Personen und Tätigkeiten erweitert – und die Abfragen werden so beziehungsreich, dass eine Graphdatenbank als nächster Schritt plötzlich sehr naheliegt.
</p>

## Ein Film ist mehr als eine Tabellenzeile

Die Daten für dieses Kapitel liegen in einem kleinen Projekt-Repository. Klone es über **Clone Repository**:

```text
https://github.com/specht/videothek.git
```

Bestätige `/workspace/` als Ziel und öffne das geklonte Repository.

<!-- tutorial-screenshot
close-folder
clone-start: https://github.com/specht/videothek.git
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the cloned repository?
click: Open
wait-for-text: movies.txt
open-file: movies.txt
hide-bottom-panel
left-sidebar-width: 280
-->

<img class='full' src='movies-json.webp' alt='Im Editor ist die Datei movies.txt mit mehreren Filmdatensätzen im JSON-Format geöffnet.'>

Die Datei `movies.txt` enthält pro Zeile einen Film. Wenn man den Datensatz für **Nosferatu** formatiert, sieht er so aus:

```json
{
  "id": 2,
  "title": "Nosferatu",
  "year": 1922,
  "runtime": 95,
  "genres": [4, 5],
  "rating": 7.8,
  "original_title": "Nosferatu, eine Symphonie des Grauens",
  "crew": {
    "actor": [9134, 5201, 10241, 9136, 9123, 6640, 3005, 5180, 7866, 5607],
    "director": [1573],
    "writer": [9650, 4890],
    "producer": [5214],
    "composer": [2076, 4501],
    "cinematographer": [2068]
  }
}
```

Das ist praktisch für den Datenaustausch: Ein Film kann direkt eine Liste seiner Genres und beteiligten Personen enthalten. In einer relationalen Datenbank wollen wir diese Informationen aber nicht einfach als verschachtelte Listen in eine Tabellenzelle schreiben.

Schau dir zusätzlich `genres.txt` an:

```json
{"id":4,"name":"Fantasy"}
{"id":5,"name":"Horror"}
```

Damit lässt sich der Filmdatensatz schon lesen: Die Genre-IDs `4` und `5` bedeuten, dass **Nosferatu** zu den Genres **Fantasy** und **Horror** gehört.

<div class='hint task'>
Welche unterschiedlichen Arten von Dingen stecken im Filmdatensatz? Welche davon würdest du als eigene Tabellen modellieren? Überlege zunächst nur für Filme und Genres – die Personen in <code>crew</code> kommen später.
</div>

## Filme und Genres modellieren

Ein Film kann zu mehreren Genres gehören:

```text
Nosferatu → Fantasy
Nosferatu → Horror
```

Umgekehrt gehört ein Genre natürlich ebenfalls zu vielen Filmen:

```text
Horror → Nosferatu
Horror → Alien
Horror → The Shining
...
```

Zwischen **Film** und **Genre** besteht also eine **n:m-Beziehung**: Auf beiden Seiten können jeweils viele Datensätze beteiligt sein.

Eine relationale Datenbank stellt eine solche Beziehung mit einer eigenen Zwischentabelle dar:

```text
movie ← movie_genre → genre
```

In `movie_genre` steht nicht noch einmal der Filmtitel oder der Name des Genres. Dort werden nur die IDs der beiden zusammengehörenden Datensätze gespeichert.

### ER-Diagramm erstellen

Wir verwenden den **ERD Editor** in Visual Studio Code, um das Modell als Entity-Relationship-Diagramm zu zeichnen. Falls die Erweiterung noch nicht installiert ist, öffne mit <kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd> die Erweiterungen, suche nach **ERD Editor** und installiere sie.

Erstelle anschließend eine Datei `filmdatenbank.erd`.

Baue zunächst diese drei Tabellen:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead>
<tr><th>Tabelle</th><th>Spalten</th></tr>
</thead>
<tbody>
<tr>
<td><code>movie</code></td>
<td><code>id</code>, <code>title</code>, <code>original_title</code>, <code>german_title</code>, <code>year</code>, <code>runtime</code>, <code>rating</code></td>
</tr>
<tr>
<td><code>genre</code></td>
<td><code>id</code>, <code>name</code></td>
</tr>
<tr>
<td><code>movie_genre</code></td>
<td><code>movie_id</code>, <code>genre_id</code></td>
</tr>
</tbody>
</table>
</div>

Verwende sinnvolle Datentypen:

- IDs, Erscheinungsjahr und Laufzeit: `INT`
- Titel und Namen: `VARCHAR(255)`
- Bewertung: `FLOAT`
- `original_title` und `german_title` dürfen leer sein; die anderen Filmdaten sollen nicht leer sein.

Verbinde `movie` und `genre` über `movie_genre`.

<!-- manual screenshot: ERD Editor interaction is intentionally not scripted -->
<img class='full' src='erd-movie-genre.webp' alt='ER-Diagramm mit movie und genre, die über die Zwischentabelle movie_genre in einer n:m-Beziehung verbunden sind.'>

Achte besonders auf die Schlüssel:

- `movie.id` ist der **Primärschlüssel** von `movie`.
- `genre.id` ist der **Primärschlüssel** von `genre`.
- `movie_genre.movie_id` ist ein **Fremdschlüssel** auf `movie.id`.
- `movie_genre.genre_id` ist ein **Fremdschlüssel** auf `genre.id`.
- **Zusammen** bilden `(movie_id, genre_id)` den Primärschlüssel von `movie_genre`.

<div class='hint'>
Ein Primärschlüssel muss einen Datensatz eindeutig identifizieren. In <code>movie_genre</code> reicht weder <code>movie_id</code> noch <code>genre_id</code> allein: Ein Film kann mehrere Genres haben und ein Genre gehört zu mehreren Filmen. Eindeutig ist erst die Kombination aus beiden IDs. Das nennt man einen <strong>zusammengesetzten Primärschlüssel</strong>.
</div>

<div class='hint task'>
Warum wäre eine einzelne Spalte <code>genres</code> in der Tabelle <code>movie</code>, in der zum Beispiel <code>'Fantasy,Horror'</code> steht, für spätere Abfragen ungünstig?
</div>

## Vom Diagramm zu SQL

Speichere dein Diagramm. Klicke anschließend im ERD Editor mit der rechten Maustaste auf den Hintergrund und wähle **Export → Schema SQL**. Speichere das Ergebnis als `filmdatenbank.sql`.

Der erzeugte SQL-Code sollte inhaltlich ungefähr so aussehen:

```sql
CREATE TABLE genre
(
  id   INT          NOT NULL,
  name VARCHAR(255) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE movie
(
  id             INT          NOT NULL,
  title          VARCHAR(255) NOT NULL,
  original_title VARCHAR(255) NULL,
  german_title   VARCHAR(255) NULL,
  year           INT          NOT NULL,
  runtime        INT          NOT NULL,
  rating         FLOAT        NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE movie_genre
(
  movie_id INT NOT NULL,
  genre_id INT NOT NULL,
  PRIMARY KEY (movie_id, genre_id)
);

ALTER TABLE movie_genre
  ADD CONSTRAINT FK_movie_TO_movie_genre
    FOREIGN KEY (movie_id)
    REFERENCES movie (id);

ALTER TABLE movie_genre
  ADD CONSTRAINT FK_genre_TO_movie_genre
    FOREIGN KEY (genre_id)
    REFERENCES genre (id);
```

<!-- tutorial-screenshot
write-file: videothek/filmdatenbank.sql <- previous-code
open-file: filmdatenbank.sql
hide-bottom-panel
show-left-sidebar
left-sidebar-width: 280
-->

<img class='full' src='schema-sql.webp' alt='Der aus dem ER-Diagramm exportierte SQL-Code enthält CREATE TABLE, Primärschlüssel und Fremdschlüssel.'>

Das Diagramm ist also nicht die Datenbank selbst. Es beschreibt das Modell. Der Export übersetzt dieses Modell in SQL:

- `CREATE TABLE` erzeugt Tabellen.
- `PRIMARY KEY` legt Primärschlüssel fest.
- `FOREIGN KEY` legt fest, auf welchen Datensatz einer anderen Tabelle verwiesen wird.
- `REFERENCES movie (id)` bedeutet zum Beispiel: Eine `movie_id` in `movie_genre` muss zu einer vorhandenen Film-ID passen.

<div class='hint'>
Die genaue Formatierung und die Namen automatisch erzeugter Constraints können je nach Version des ERD Editors etwas anders aussehen. Entscheidend ist die Struktur.
</div>

## Schema anlegen und Filmdaten laden

Führe nun dein exportiertes Schema aus:

```bash
mysql < filmdatenbank.sql
```

Die Tabellen existieren jetzt, enthalten aber noch keine Filme. Die aufbereiteten Daten liegen bereits als SQL-`INSERT`-Anweisungen vor. Für den ersten Teil des Kapitels laden wir nur Filme, Genres und ihre Beziehungen:

```bash
mysql < movie-genre-data.sql
```

Starte danach `mycli` und überprüfe die Tabellen:

```bash
mycli
```

```sql
SHOW TABLES;
```

<!-- tutorial-screenshot
show-bottom-panel
terminal-open
terminal-maximize
terminal-run: mysql < filmdatenbank.sql
terminal-wait-for-prompt
terminal-run: mysql < movie-genre-data.sql
terminal-wait-for-prompt
terminal-run: mycli
terminal-run: SHOW TABLES;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='show-film-tables.webp' alt='mycli zeigt die drei Tabellen genre, movie und movie_genre.'>

<div class='hint task'>
Wie viele Filme wurden importiert? Wie viele Genres gibt es? Formuliere zwei passende Abfragen mit <code>COUNT(*)</code>.
</div>

## JOIN: Beziehungen wieder zusammensetzen

Im SQL-Einführungskapitel kamen die Daten für eine Abfrage jeweils aus **einer** Tabelle. Jetzt ist die Information absichtlich auf mehrere Tabellen verteilt.

Nehmen wir wieder **Nosferatu**. Die Tabelle `movie` kennt den Film:

```sql
SELECT id, title, year
FROM movie
WHERE title = 'Nosferatu';
```

Sie kennt aber nicht die Namen seiner Genres.

Die Tabelle `movie_genre` enthält die Verbindung zwischen Film und Genre. Deshalb verbinden wir im ersten Schritt `movie` mit `movie_genre`:

```sql
SELECT movie.title, movie_genre.genre_id
FROM movie
JOIN movie_genre
  ON movie.id = movie_genre.movie_id
WHERE movie.title = 'Nosferatu';
```

<!-- tutorial-screenshot
terminal-run: SELECT movie.title, movie_genre.genre_id FROM movie JOIN movie_genre ON movie.id = movie_genre.movie_id WHERE movie.title = 'Nosferatu';
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='join-nosferatu-ids.webp' alt='Die erste JOIN-Abfrage zeigt Nosferatu zweimal, jeweils mit einer der zugehörigen Genre-IDs.'>

Der wichtige Teil ist:

```sql
JOIN movie_genre
  ON movie.id = movie_genre.movie_id
```

`JOIN` sagt: **Verbinde Zeilen aus zwei Tabellen.**

`ON` sagt: **Woran erkennst du, welche Zeilen zusammengehören?**

Hier gehört eine Zeile aus `movie_genre` genau dann zu einem Film, wenn `movie.id` und `movie_genre.movie_id` gleich sind.

Das Ergebnis enthält jetzt die Genre-IDs. Für die **Namen** der Genres brauchen wir noch die Tabelle `genre`.

Wir folgen also der nächsten Beziehung aus dem ER-Diagramm:

```text
movie → movie_genre → genre
```

Und fügen einen zweiten `JOIN` hinzu:

```sql
SELECT movie.title, genre.name
FROM movie
JOIN movie_genre
  ON movie.id = movie_genre.movie_id
JOIN genre
  ON movie_genre.genre_id = genre.id
WHERE movie.title = 'Nosferatu';
```

<!-- tutorial-screenshot
terminal-run: SELECT movie.title, genre.name FROM movie JOIN movie_genre ON movie.id = movie_genre.movie_id JOIN genre ON movie_genre.genre_id = genre.id WHERE movie.title = 'Nosferatu';
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='join-nosferatu-genres.webp' alt='Die Abfrage verbindet movie, movie_genre und genre und zeigt die Genrenamen von Nosferatu.'>

Das ist die zentrale Idee dieses Kapitels:

> **Jeder `JOIN ... ON ...`-Schritt folgt einer Beziehung im relationalen Modell.**

Wenn du im ER-Diagramm den Weg von einer Tabelle zur anderen findest, kannst du daraus meist auch den Weg für deine `JOIN`s ableiten.

## Filme eines Genres finden

Jetzt können wir eine Frage stellen, die nur mit allen drei Tabellen sinnvoll zu beantworten ist:

> Welche Animationsfilme sind in der Datenbank?

```sql
SELECT movie.title, movie.year, movie.rating
FROM movie
JOIN movie_genre
  ON movie.id = movie_genre.movie_id
JOIN genre
  ON movie_genre.genre_id = genre.id
WHERE genre.name = 'Animation'
ORDER BY movie.rating DESC, movie.year DESC
LIMIT 10;
```

<!-- tutorial-screenshot
terminal-run: SELECT movie.title, movie.year, movie.rating FROM movie JOIN movie_genre ON movie.id = movie_genre.movie_id JOIN genre ON movie_genre.genre_id = genre.id WHERE genre.name = 'Animation' ORDER BY movie.rating DESC, movie.year DESC LIMIT 10;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='animation-top.webp' alt='Eine JOIN-Abfrage zeigt zehn besonders hoch bewertete Animationsfilme mit Jahr und Bewertung.'>

Beachte, welche Aufgaben die Teile der Abfrage übernehmen:

```text
FROM / JOIN / ON  → Tabellen und Beziehungen
WHERE             → gewünschtes Genre
ORDER BY          → Reihenfolge
LIMIT             → Anzahl der Ergebnisse
```

<div class='hint task'>
Formuliere Abfragen für diese Fragen:

1. Welche Genres hat <strong>Metropolis</strong> aus dem Jahr 1927?
2. Welche zehn am besten bewerteten Science-Fiction-Filme enthält die Datenbank?
3. Welche Horrorfilme ab dem Jahr 2000 haben eine Bewertung von mindestens 8,0?
</div>

## GROUP BY: Aus vielen Filmen wird eine Zahl pro Genre

Mit `COUNT(*)` kannst du Zeilen zählen. In Verbindung mit `GROUP BY` kannst du ein Ergebnis aber in Gruppen aufteilen und **jede Gruppe einzeln zählen**.

Wie viele Filme gibt es pro Genre?

```sql
SELECT genre.name, COUNT(*) AS movies
FROM genre
JOIN movie_genre
  ON genre.id = movie_genre.genre_id
GROUP BY genre.id, genre.name
ORDER BY movies DESC;
```

`GROUP BY genre.id, genre.name` bedeutet: Alle Ergebniszeilen mit demselben Genre werden zu einer Gruppe zusammengefasst. `COUNT(*)` wird dann für jede dieser Gruppen getrennt berechnet.

Für einen übersichtlichen Screenshot beschränken wir die Ausgabe auf die ersten zehn Genres:

```sql
SELECT genre.name, COUNT(*) AS movies
FROM genre
JOIN movie_genre
  ON genre.id = movie_genre.genre_id
GROUP BY genre.id, genre.name
ORDER BY movies DESC
LIMIT 10;
```

<!-- tutorial-screenshot
terminal-run: SELECT genre.name, COUNT(*) AS movies FROM genre JOIN movie_genre ON genre.id = movie_genre.genre_id GROUP BY genre.id, genre.name ORDER BY movies DESC LIMIT 10;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='films-per-genre.webp' alt='Die GROUP-BY-Abfrage zeigt die zehn Genres mit den meisten Filmen und die jeweilige Anzahl.'>

Du kannst auch andere Aggregatfunktionen auf Gruppen anwenden. Zum Beispiel berechnet `AVG(...)` einen Durchschnitt:

```sql
SELECT genre.name,
       COUNT(*) AS movies,
       ROUND(AVG(movie.rating), 2) AS average_rating
FROM genre
JOIN movie_genre
  ON genre.id = movie_genre.genre_id
JOIN movie
  ON movie_genre.movie_id = movie.id
GROUP BY genre.id, genre.name
ORDER BY average_rating DESC;
```

<div class='hint task'>
Welche drei Fragen an die Filmdatenbank kannst du selbst formulieren, für die <code>GROUP BY</code> sinnvoll ist? Schreibe anschließend die passenden SQL-Abfragen.
</div>

## Das Modell reicht noch nicht

Bisher haben wir den Bereich `crew` aus dem ursprünglichen JSON-Datensatz ignoriert:

```json
"crew": {
  "actor": [9134, 5201, 10241, 9136, 9123, 6640, 3005, 5180, 7866, 5607],
  "director": [1573],
  "writer": [9650, 4890],
  "producer": [5214],
  "composer": [2076, 4501],
  "cinematographer": [2068]
}
```

Jetzt wollen wir Fragen beantworten wie:

- Wer führte bei einem Film Regie?
- In welchen Filmen spielte eine bestimmte Person mit?
- Welche Personen haben an demselben Film gearbeitet?
- Welche Schauspieler:innen kommen besonders häufig in den Filmen einer Regisseurin oder eines Regisseurs vor?

Dazu müssen wir unser Modell erweitern.

Eine Person kann an vielen Filmen beteiligt sein. An einem Film arbeiten viele Personen. Außerdem kann dieselbe Person bei demselben Film mehr als eine Tätigkeit haben.

Wir brauchen deshalb drei weitere Tabellen:

```text
crew
job
movie_crew
```

`crew` enthält Personen. `job` enthält Tätigkeiten wie `actor`, `director` oder `writer`. `movie_crew` verbindet **Film, Person und Tätigkeit**.

<div class='hint task'>
Versuche zunächst selbst, die drei neuen Tabellen in deinem ER-Diagramm zu ergänzen.

Überlege dabei:

- Welche Spalten braucht <code>crew</code>?
- Welche Spalten braucht <code>job</code>?
- Welche drei Fremdschlüssel braucht <code>movie_crew</code>?
- Warum besteht der Primärschlüssel von <code>movie_crew</code> aus drei Spalten?
</div>

Eine mögliche Lösung sieht so aus:

<!-- manual screenshot: ERD Editor interaction is intentionally not scripted -->
<img class='full' src='erd-complete.webp' alt='Das vollständige ER-Diagramm ergänzt crew und job und verbindet beide zusammen mit movie über die Tabelle movie_crew.'>

Verwende diese Spalten:

```text
crew
  id
  name
  birth_year
  death_year

job
  id
  title

movie_crew
  movie_id
  crew_id
  job_id
```

Alle drei Spalten von `movie_crew` sind Fremdschlüssel und bilden **gemeinsam** den Primärschlüssel:

```text
(movie_id, crew_id, job_id)
```

Dadurch kann dieselbe Person bei einem Film zum Beispiel gleichzeitig als `actor` und `director` vorkommen, aber dieselbe Kombination aus Film, Person und Tätigkeit nicht doppelt gespeichert werden.

## Die Datenbank erweitern, nicht neu aufbauen

Wir wollen die bereits importierten Filme nicht löschen und die Datenbank nicht noch einmal von vorne aufbauen. Die Erweiterung besteht nur aus drei neuen Tabellen und ihren Fremdschlüsseln.

Exportiere das erweiterte ER-Diagramm erneut und vergleiche den SQL-Code mit deinem ersten Export. Die neu hinzugekommenen Teile entsprechen inhaltlich diesem SQL:

<!-- screenshot-code: schema-extension -->
```sql
CREATE TABLE crew
(
  id         INT          NOT NULL,
  name       VARCHAR(255) NOT NULL,
  birth_year INT          NOT NULL,
  death_year INT          NULL,
  PRIMARY KEY (id)
);

CREATE TABLE job
(
  id    INT          NOT NULL,
  title VARCHAR(255) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE movie_crew
(
  movie_id INT NOT NULL,
  crew_id  INT NOT NULL,
  job_id   INT NOT NULL,
  PRIMARY KEY (movie_id, crew_id, job_id)
);

ALTER TABLE movie_crew
  ADD CONSTRAINT FK_movie_TO_movie_crew
    FOREIGN KEY (movie_id)
    REFERENCES movie (id);

ALTER TABLE movie_crew
  ADD CONSTRAINT FK_crew_TO_movie_crew
    FOREIGN KEY (crew_id)
    REFERENCES crew (id);

ALTER TABLE movie_crew
  ADD CONSTRAINT FK_job_TO_movie_crew
    FOREIGN KEY (job_id)
    REFERENCES job (id);
```

Speichere nur diese neuen Anweisungen als `filmdatenbank-erweiterung.sql` und führe sie aus:

```bash
mysql < filmdatenbank-erweiterung.sql
```

Danach können die vorbereiteten Personen-, Tätigkeits- und Zuordnungsdaten geladen werden:

```bash
mysql < crew-data.sql
```

<!-- tutorial-screenshot
write-file: videothek/filmdatenbank-erweiterung.sql <- previous-code (schema-extension)
press: Control+D
terminal-wait-for-prompt
terminal-run: mysql < filmdatenbank-erweiterung.sql
terminal-wait-for-prompt
terminal-run: mysql < crew-data.sql
terminal-wait-for-prompt
terminal-run: mycli
terminal-run: SHOW TABLES;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='show-all-film-tables.webp' alt='mycli zeigt nach der Erweiterung die sechs Tabellen crew, genre, job, movie, movie_crew und movie_genre.'>

Jetzt enthält die Datenbank das vollständige relationale Modell, ohne dass die schon vorhandenen Tabellen neu erstellt oder die Filmdaten erneut importiert werden mussten.

## Durch mehrere Beziehungen zu einer Person

Wer führte bei **Nosferatu** Regie?

Im ER-Diagramm führt der Weg von einem Film zu einer Person über `movie_crew`:

```text
movie → movie_crew → crew
```

Die Tätigkeit steht zusätzlich in `job`:

```text
             job
              ↑
movie → movie_crew → crew
```

Daraus entsteht eine Abfrage mit drei `JOIN`s:

```sql
SELECT crew.name
FROM movie
JOIN movie_crew
  ON movie.id = movie_crew.movie_id
JOIN crew
  ON movie_crew.crew_id = crew.id
JOIN job
  ON movie_crew.job_id = job.id
WHERE movie.title = 'Nosferatu'
  AND movie.year = 1922
  AND job.title = 'director';
```

<!-- tutorial-screenshot
terminal-run: SELECT crew.name FROM movie JOIN movie_crew ON movie.id = movie_crew.movie_id JOIN crew ON movie_crew.crew_id = crew.id JOIN job ON movie_crew.job_id = job.id WHERE movie.title = 'Nosferatu' AND movie.year = 1922 AND job.title = 'director';
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='nosferatu-director.webp' alt='Eine Abfrage über movie, movie_crew, crew und job ermittelt die Regie von Nosferatu.'>

Auch hier ist die Abfrage keine Zauberei: Jeder `JOIN` folgt wieder einer Verbindung im ER-Diagramm.

<div class='hint task'>
Formuliere Abfragen für diese Fragen:

1. Wer spielte in <strong>Nosferatu</strong> mit?
2. Bei welchen Filmen führte <strong>Christopher Nolan</strong> Regie?
3. An welchen Filmen war <strong>Charlie Chaplin</strong> beteiligt und welche Tätigkeit hatte er jeweils?
4. Welche zehn Personen kommen am häufigsten als <code>director</code> in der Datenbank vor?
</div>

## Zusammenfassung

In diesem Kapitel hast du nicht nur neue SQL-Syntax gelernt, sondern eine relationale Datenbank **modelliert**:

- Ein **Primärschlüssel** identifiziert einen Datensatz eindeutig.
- Ein **Fremdschlüssel** verweist auf einen Datensatz in einer anderen Tabelle.
- Eine **n:m-Beziehung** wird in einer relationalen Datenbank durch eine Zwischentabelle dargestellt.
- Ein **zusammengesetzter Primärschlüssel** kann aus mehreren Spalten bestehen.
- Mit `JOIN ... ON ...` setzt du zusammengehörende Daten aus mehreren Tabellen wieder zusammen.
- Jeder `JOIN` kann als Schritt entlang einer Beziehung im ER-Diagramm verstanden werden.
- Mit `GROUP BY` und Aggregatfunktionen wie `COUNT` oder `AVG` wertest du Gruppen von Datensätzen aus.

Damit können wir schon ziemlich anspruchsvolle Fragen beantworten. Zum Schluss probieren wir aber noch zwei Fragen, bei denen nicht mehr ein einzelner Film oder eine einzelne Person im Mittelpunkt steht, sondern die **Beziehungen zwischen Personen**.

## Welche Schauspieler:innen arbeiten besonders oft zusammen?

Eine eigentlich ganz einfache Frage lautet:

> **Welche zwei Schauspieler:innen haben besonders oft gemeinsam in einem Film gespielt?**

Die relationale Datenbank kann diese Frage beantworten. Die Abfrage sieht allerdings so aus:

```sql
SELECT
  actor1.name AS actor_1,
  actor2.name AS actor_2,
  COUNT(DISTINCT movie.id) AS movies_together
FROM movie
JOIN movie_crew AS acting1
  ON movie.id = acting1.movie_id
JOIN job AS job1
  ON acting1.job_id = job1.id
JOIN crew AS actor1
  ON acting1.crew_id = actor1.id
JOIN movie_crew AS acting2
  ON movie.id = acting2.movie_id
JOIN job AS job2
  ON acting2.job_id = job2.id
JOIN crew AS actor2
  ON acting2.crew_id = actor2.id
WHERE job1.title = 'actor'
  AND job2.title = 'actor'
  AND actor1.id < actor2.id
GROUP BY
  actor1.id, actor1.name,
  actor2.id, actor2.name
HAVING COUNT(DISTINCT movie.id) >= 2
ORDER BY movies_together DESC, actor1.name, actor2.name
LIMIT 20;
```

Du musst diese Abfrage nicht Zeile für Zeile nachvollziehen. Wenn du möchtest, kannst du sie aber in `mycli` ausprobieren.

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT actor1.name AS actor_1, actor2.name AS actor_2, COUNT(DISTINCT movie.id) AS movies_together FROM movie JOIN movie_crew AS acting1 ON movie.id = acting1.movie_id JOIN job AS job1 ON acting1.job_id = job1.id JOIN crew AS actor1 ON acting1.crew_id = actor1.id JOIN movie_crew AS acting2 ON movie.id = acting2.movie_id JOIN job AS job2 ON acting2.job_id = job2.id JOIN crew AS actor2 ON acting2.crew_id = actor2.id WHERE job1.title = 'actor' AND job2.title = 'actor' AND actor1.id < actor2.id GROUP BY actor1.id, actor1.name, actor2.id, actor2.name HAVING COUNT(DISTINCT movie.id) >= 2 ORDER BY movies_together DESC, actor1.name, actor2.name LIMIT 20;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='actor-pairs.webp' alt='Eine lange SQL-Abfrage ermittelt Schauspielerpaare, die besonders häufig gemeinsam in Filmen vorkommen.'>

Warum tauchen `movie_crew`, `job` und `crew` jeweils zweimal auf? Weil wir dieselben Tabellen gleichzeitig für **zwei verschiedene Schauspieler:innen** verwenden. Deshalb bekommen sie unterschiedliche Aliase wie `acting1` und `acting2` beziehungsweise `actor1` und `actor2`.

Die Bedingung

```sql
actor1.id < actor2.id
```

verhindert außerdem, dass dasselbe Paar zweimal gezählt wird:

```text
Person A + Person B
Person B + Person A
```

## Gibt es feste Teams aus Regie und Kamera?

Noch eine Frage, die bei Filmen durchaus interessant ist:

> **Welche Regisseur:innen und Kameraleute haben besonders häufig zusammengearbeitet?**

Auch dafür gibt es eine SQL-Abfrage:

```sql
SELECT
  director.name AS director,
  cinematographer.name AS cinematographer,
  COUNT(DISTINCT movie.id) AS movies_together
FROM movie
JOIN movie_crew AS directing
  ON movie.id = directing.movie_id
JOIN job AS director_job
  ON directing.job_id = director_job.id
JOIN crew AS director
  ON directing.crew_id = director.id
JOIN movie_crew AS camera
  ON movie.id = camera.movie_id
JOIN job AS camera_job
  ON camera.job_id = camera_job.id
JOIN crew AS cinematographer
  ON camera.crew_id = cinematographer.id
WHERE director_job.title = 'director'
  AND camera_job.title = 'cinematographer'
GROUP BY
  director.id, director.name,
  cinematographer.id, cinematographer.name
HAVING COUNT(DISTINCT movie.id) >= 2
ORDER BY movies_together DESC, director.name, cinematographer.name
LIMIT 20;
```

Auch diese Abfrage darfst du einfach ausprobieren, ohne sie vollständig auseinanderzunehmen.

<!-- tutorial-screenshot
press: Control+L
terminal-run: SELECT director.name AS director, cinematographer.name AS cinematographer, COUNT(DISTINCT movie.id) AS movies_together FROM movie JOIN movie_crew AS directing ON movie.id = directing.movie_id JOIN job AS director_job ON directing.job_id = director_job.id JOIN crew AS director ON directing.crew_id = director.id JOIN movie_crew AS camera ON movie.id = camera.movie_id JOIN job AS camera_job ON camera.job_id = camera_job.id JOIN crew AS cinematographer ON camera.crew_id = cinematographer.id WHERE director_job.title = 'director' AND camera_job.title = 'cinematographer' GROUP BY director.id, director.name, cinematographer.id, cinematographer.name HAVING COUNT(DISTINCT movie.id) >= 2 ORDER BY movies_together DESC, director.name, cinematographer.name LIMIT 20;
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='director-camera-pairs.webp' alt='Eine lange SQL-Abfrage zeigt Regie-Kamera-Paare, die mehrfach gemeinsam an Filmen gearbeitet haben.'>

Die Fragen waren einfach. Die SQL-Abfragen sind es nicht.

Das liegt nicht daran, dass SQL oder relationale Datenbanken schlecht wären. Im Gegenteil: Für klar strukturierte Daten, eindeutige Schlüssel und verlässliche Beziehungen zwischen Tabellen ist unser relationales Modell sehr gut geeignet.

Aber bei unseren letzten Fragen waren plötzlich die **Beziehungen selbst** das Interessante:

- Wer hat mit wem gespielt?
- Welche Personen arbeiten immer wieder zusammen?
- Welche Regie-Kamera-Teams gibt es?
- Über welche Filme und Personen sind zwei Schauspieler:innen miteinander verbunden?

Graphdatenbanken wurden für Daten und Abfragen entwickelt, bei denen solche Beziehungen selbst im Mittelpunkt stehen. Dort werden Beziehungen nicht erst über Zwischentabellen und mehrere `JOIN`s rekonstruiert, sondern sind ein direkter Bestandteil des Datenmodells.

Genau deshalb schauen wir uns als Nächstes eine Graphdatenbank an. Im nächsten Kapitel verwenden wir **dieselben Filme, Genres und Personen** noch einmal – diesmal in **Neo4j**.
