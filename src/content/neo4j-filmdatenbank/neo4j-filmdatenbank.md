<div class='meta'>
image: neo4j-browser-path.webp:0:100
image_alt: "Der Neo4j Browser mit einer Cypher-Abfrage und dem Ergebnis als Graph aus Personen und Filmen"
kind: projekt
language: Cypher
duration: 2 Doppelstunden
curriculum: "Oberstufe: Datenbanken (Datenmodellierung als Graph, Abfragen mit Cypher im Vergleich zu SQL)"
topics: [datenbanken-modellierung, datenbanken-abfragen, graphen]
requires: [filmdatenbank]
reference: [cypher, datensaetze]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Filmdatenbank als Graph mit Neo4j

<p class='abstract'>
In der »Filmdatenbank mit MySQL« hast du Filme, Genres und Personen auf Tabellen verteilt und mit <code>JOIN</code> wieder zusammengesetzt. Jetzt speicherst du dieselben Daten als <strong>Graph</strong> in Neo4j: Filme, Personen und Genres werden zu Knoten, ihre Beziehungen zu direkten Verbindungen. Du lernst die Abfragesprache <strong>Cypher</strong> kennen und siehst, warum Fragen wie »Wer arbeitet oft mit wem?« in einem Graphen kurz werden.
</p>

## Vom Tabellenmodell zum Graphen

### Dieselben Daten, ein anderes Modell

Am Ende der Filmdatenbank hattest du sechs Tabellen: `movie`, `genre`, `movie_genre`, `crew`, `job` und `movie_crew`. Für die Genres eines Films führte der Weg durch eine Zwischentabelle, für die Menschen sogar durch zwei:

```text
movie → movie_genre → genre

movie → movie_crew → crew
             ↓
            job
```

Das funktioniert. Aber die letzte Frage dort, wer besonders oft mit wem gespielt hat, brauchte jede Tabelle zweimal. Es ging nicht mehr um einen Film oder eine Person, sondern um die **Verbindungen** zwischen ihnen. Eine Graphdatenbank speichert genau diese Verbindungen.

Bevor du siehst, wie der Graph aussieht, rate:

_task(zwischentabelle)

<!-- after: zwischentabelle -->
_task(taetigkeit)
<!-- /after -->

<!-- after: taetigkeit -->
Im Graphen gibt es drei Arten von **Knoten**: `Movie`, `Person` und `Genre`. Zwischen ihnen liegen **Beziehungen**, und jede Beziehung hat einen **Typ**:

```cypher
(:Movie)-[:IN_GENRE]->(:Genre)

(:Person)-[:ACTOR]->(:Movie)
(:Person)-[:DIRECTOR]->(:Movie)
(:Person)-[:WRITER]->(:Movie)
(:Person)-[:PRODUCER]->(:Movie)
(:Person)-[:COMPOSER]->(:Movie)
(:Person)-[:CINEMATOGRAPHER]->(:Movie)
(:Person)-[:EDITOR]->(:Movie)
```

Die Typen sind die Tätigkeiten aus der Tabelle `job`, nur in Großbuchstaben, so wie man sie in Cypher üblicherweise schreibt.
<!-- /after -->

<div class='hint'>
Man muss eine relationale Datenbank nicht immer so in einen Graphen übersetzen. Wir wählen dieses Modell, weil bei unseren Filmdaten gerade die Verbindungen zwischen Filmen, Genres und Personen interessant sind.
</div>

### Knoten haben Eigenschaften

Ein Knoten hat ein **Label**, das sagt, was er darstellt, und **Eigenschaften** mit Werten. »Back to the Future« sieht im Graphen ungefähr so aus:

```cypher
(:Movie {
  id: 233,
  title: "Back to the Future",
  german_title: "Zurück in die Zukunft",
  year: 1985,
  runtime: 115
})
```

`Movie` ist das Label, `id`, `title`, `german_title`, `year` und `runtime` sind Eigenschaften. Ein Genre und eine Person sehen so aus:

```cypher
(:Genre { id: 5, name: "Comedy" })

(:Person { id: 2493, name: "Michael J. Fox", birth_year: 1961 })
```

Eine Beziehung verbindet zwei Knoten und zeigt in eine Richtung:

```cypher
(:Person { name: "Robert Zemeckis" })-[:DIRECTOR]->(:Movie { title: "Back to the Future" })
```

Runde Klammern stehen für Knoten, eckige für Beziehungen. Der Name hinter dem Doppelpunkt ist bei einem Knoten sein Label, bei einer Beziehung ihr Typ. Das ist schon Cypher: Genau so schreibst du gleich deine Abfragen.

_person(eifrem-svensson-neubauer)

## Loslegen

### Die Filmdaten laden

Schalte zuerst in deinem [Profil](/profil) unter »Datenbanken« **Neo4j** ein. Du hast dann eine eigene, leere Neo4j-Datenbank, und oben im Menü erscheint der Knopf »Neo4j«. Im Workspace stehen deine Zugangsdaten in den Umgebungsvariablen `NEO4J_URI`, `NEO4J_USERNAME`, `NEO4J_PASSWORD` und `NEO4J_DATABASE`; `neo4j_bolt` und `cypher-shell` weiter unten lesen sie von selbst.

Öffne ein Terminal und wechsle in den Ordner aus der Filmdatenbank (`mkdir -p` legt ihn an, falls es ihn noch nicht gibt):

```bash
mkdir -p ~/filmdatenbank
cd ~/filmdatenbank
```

Lade dort die Filmdaten herunter…

```bash_copy
wget https://github.com/specht/workspace-files/raw/main/movie-archive/movies-neo4j.dump
```

…und lade sie in deine Datenbank:

```bash
neo4j_bolt load movies-neo4j.dump
```

<!-- tutorial-screenshot
# movies-neo4j.dump geladen
hide-left-sidebar
show-bottom-panel
terminal-open
terminal-maximize
terminal-run: mkdir -p ~/filmdatenbank && cd ~/filmdatenbank
terminal-run: wget -q https://github.com/specht/workspace-files/raw/main/movie-archive/movies-neo4j.dump
terminal-wait-for-prompt
terminal-run: neo4j_bolt load movies-neo4j.dump
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='neo4j-load.webp' alt='Im Terminal lädt neo4j_bolt die Knoten und Beziehungen aus movies-neo4j.dump.'>

`movies-neo4j.dump` ist eine Textdatei: Jede Zeile beginnt mit `n` für einen Knoten oder mit `r` für eine Beziehung. `neo4j_bolt` legt erst alle Knoten an und verbindet sie dann. Die Daten stammen, wie die für MySQL, aus [Wikidata](https://www.wikidata.org/); was drinsteht, beschreibt das Handbuch unter [Datensätze](/reference/datensaetze#movies).

<div class='hint'>
<code>neo4j_bolt load</code> erwartet eine leere Datenbank. Hast du schon etwas ausprobiert, setze deine Neo4j-Datenbank im Profil zurück und lade dann noch einmal.
</div>

### Zwei Werkzeuge für Abfragen

Abfragen schreibst du in **Cypher**. Dafür gibt es zwei Werkzeuge:

- Den **Neo4j Browser** öffnest du mit dem Knopf »Neo4j« oben im Menü. Melde dich dort mit dem Benutzernamen und dem Passwort aus deinem Profil an. Er zeigt Ergebnisse als Graph zum Anschauen oder als Tabelle.
- **`cypher-shell`** läuft im Terminal des Workspace und meldet sich mit deinen Umgebungsvariablen von selbst an. Er zeigt Ergebnisse als Tabelle, wie `mycli` für MySQL. Mit `:exit` beendest du ihn.

Fang im Neo4j Browser an:

```cypher
MATCH (n)
RETURN n
LIMIT 25;
```

<!-- tutorial-screenshot
# Neo4j Browser: die ersten 25 Knoten als Graph
neo4j-browser
tab: preview
# Same width and scale as the VS Code shots, a little taller for the graph
viewport: 1029x700
click: selector:button[aria-label="Database overview"]
click: selector:[data-testid=main-editor] .cm-content
type: MATCH (n) RETURN n LIMIT 25;
press: Control+Enter
wait-for-text: Started streaming 25 records
sleep: 2
click: selector:button[aria-label="Zoom in"]
click: selector:button[aria-label="Zoom in"]
move-mouse: Results overview
sleep: 2
variants: dark
-->

<img class='full' src='neo4j-browser-graph.webp' alt='Neo4j Browser: Das Ergebnis von MATCH (n) RETURN n LIMIT 25 als Graph aus Filmknoten.'>

Die Abfrage bedeutet:

```text
MATCH (n)   finde Knoten, und nenne jeden gefundenen Knoten n
RETURN n    gib diese Knoten zurück
LIMIT 25    höchstens 25
```

`n` ist ein frei gewählter Name, wie ein Alias in SQL. Mit einem Label findest du nur Knoten einer Art:

```cypher
MATCH (m:Movie)
RETURN m
LIMIT 10;
```

Probier das auch für `Person` und `Genre` aus.

## Erste Abfragen

### Einen bestimmten Film finden

Wie in SQL filterst du mit `WHERE`:

```cypher
MATCH (m:Movie)
WHERE m.title = 'Back to the Future' AND m.year = 1985
RETURN m;
```

Kürzer schreibst du die Eigenschaften direkt in das Muster:

```cypher
MATCH (m:Movie {title: 'Back to the Future', year: 1985})
RETURN m;
```

Beide Abfragen finden denselben Film. Willst du nur einige Eigenschaften, nennst du sie hinter `RETURN`. Das Ergebnis ist dann eine Tabelle, auch im Terminal:

```cypher
MATCH (m:Movie {title: 'Back to the Future', year: 1985})
RETURN m.title, m.german_title, m.runtime;
```

<!-- tutorial-screenshot
# cypher-shell mit einer ersten Abfrage
terminal-run: clear
terminal-run: cypher-shell
wait-for-text: student@student>
press: Control+L
type: MATCH (m:Movie {title: 'Back to the Future', year: 1985}) RETURN m.title, m.german_title, m.runtime;
press: Enter
wait-for-text: 1 row
crop-terminal-lines: auto
-->

<img class='full' src='cypher-shell-film.webp' alt='cypher-shell zeigt Titel, deutschen Titel und Laufzeit von Back to the Future als Tabelle.'>

Jetzt du. Die folgenden Aufgaben prüft der Workspace selbst: Schreib die Abfrage in das Feld und drück <kbd>Strg</kbd>+<kbd>Enter</kbd>. Sie läuft in deiner eigenen Neo4j-Datenbank, angemeldet als du, und darf nur lesen. Verglichen wird das Ergebnis, nicht der Text deiner Abfrage.

_task(koenig-der-loewen)

### Beziehungen folgen

Jetzt kommt der Unterschied zu MySQL. Welche Genres hat »Back to the Future«? In MySQL brauchtest du dafür zwei `JOIN`s, über `movie_genre` zu `genre`. Im Graphen liegt die Beziehung schon da, und du schreibst sie als Muster hin:

```cypher
MATCH (m:Movie {title: 'Back to the Future', year: 1985})-[:IN_GENRE]->(g:Genre)
RETURN m, g;
```

<!-- Screenshot von Hand: Neo4j Browser mit Back to the Future und seinen Genres -->

Lies das Muster wie eine kleine Zeichnung:

- `(m:Movie {…})` ist der Filmknoten,
- `-[:IN_GENRE]->` eine Beziehung dieses Typs, in Pfeilrichtung,
- `(g:Genre)` der Genreknoten am anderen Ende.

Brauchst du nur die Namen, gibst du nur sie zurück:

```cypher
MATCH (m:Movie {title: 'Back to the Future', year: 1985})-[:IN_GENRE]->(g:Genre)
RETURN g.name;
```

Das ist die Idee von Cypher: **Du beschreibst den Ausschnitt des Graphen, den du suchst, und Neo4j findet alle Stellen, an denen er vorkommt.**

_task(richtung)

### Alle Beteiligten eines Films

Zu einem Film führen Beziehungen verschiedener Typen. Lässt du den Typ weg, passt jede:

```cypher
MATCH (p:Person)-[r]->(m:Movie {title: 'Back to the Future', year: 1985})
RETURN p, r, m;
```

Die Beziehung hat jetzt einen Namen, `r`. Damit fragst du ihren Typ ab:

```cypher
MATCH (p:Person)-[r]->(m:Movie {title: 'Back to the Future', year: 1985})
RETURN type(r) AS job, p.name
ORDER BY job, p.name;
```

In MySQL war die Tätigkeit eine eigene Tabelle, die du mit einem weiteren `JOIN` nachschlagen musstest. Hier ist sie Teil der Beziehung.

### Filme eines Genres

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(:Genre {name: 'Animation'})
RETURN m.title, m.year
ORDER BY m.year DESC
LIMIT 10;
```

Ein Knoten im Muster braucht keinen Namen, wenn du ihn später nicht mehr verwendest, wie hier das Genre. Zum Vergleich der Weg in SQL:

```text
SQL:     movie JOIN movie_genre JOIN genre WHERE genre.name = 'Animation'
Cypher:  (:Movie)-[:IN_GENRE]->(:Genre {name: 'Animation'})
```

_task(scifi)

_task(drei-stunden)

_task(comedy-scifi)

### Zählen

Auch Cypher kennt `count()`, `avg()`, `min()` und `max()`. Wie viele Filme gibt es pro Genre?

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name, count(m) AS movies
ORDER BY movies DESC;
```

Ein `GROUP BY` gibt es nicht. Cypher gruppiert von selbst nach allem, was hinter `RETURN` steht und nicht gezählt wird, hier nach `g.name`. Auch Durchschnitte gehen so:

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name, count(m) AS movies, round(avg(m.runtime)) AS minutes
ORDER BY minutes DESC;
```

### Wer führte Regie?

In MySQL brauchte die Frage »Wer führte bei ›Back to the Future‹ Regie?« drei `JOIN`s. Im Graphen ist das gesuchte Muster fast schon die Antwort:

```cypher
MATCH (p:Person)-[:DIRECTOR]->(:Movie {title: 'Back to the Future', year: 1985})
RETURN p.name;
```

_task(nolan)

_task(zimmer)

_task(viel-regie)

_task(muster)

## Regeln für den Graphen

Neo4j wird oft »schemalos« genannt: Du musst keine Tabellen anlegen, bevor du Knoten speicherst. Regeln sind trotzdem sinnvoll. In MySQL hatte jede Tabelle einen Primärschlüssel; auch hier soll es keine zwei Filme mit derselben `id` geben. Lege dafür drei **Schlüssel** an:

```cypher
CREATE CONSTRAINT movie_id IF NOT EXISTS FOR (m:Movie) REQUIRE m.id IS NODE KEY;
CREATE CONSTRAINT genre_id IF NOT EXISTS FOR (g:Genre) REQUIRE g.id IS NODE KEY;
CREATE CONSTRAINT person_id IF NOT EXISTS FOR (p:Person) REQUIRE p.id IS NODE KEY;
```

Ein Schlüssel verlangt: Jeder Knoten mit diesem Label hat die Eigenschaft, und kein Wert kommt doppelt vor. Das ist, was ein Primärschlüssel in MySQL tut. Versuch danach, einen zweiten Film mit der `id` von »Back to the Future« anzulegen, und lies die Fehlermeldung:

```cypher
MATCH (m:Movie {title: 'Back to the Future', year: 1985})
CREATE (:Movie {id: m.id, title: 'Doppelt'});
```

Für Eigenschaften, nach denen du oft suchst, legst du **Indizes** an. Sie beschleunigen die Suche, verbieten aber nichts:

```cypher
CREATE INDEX movie_title IF NOT EXISTS FOR (m:Movie) ON (m.title);
CREATE INDEX person_name IF NOT EXISTS FOR (p:Person) ON (p.name);
```

`SHOW CONSTRAINTS;` und `SHOW INDEXES;` zeigen, was es gibt.

<div class='hint'>
Ein Gegenstück zum Fremdschlüssel braucht der Graph nicht: Eine Beziehung verbindet immer zwei Knoten, die es wirklich gibt. Eine Beziehung zu einer <code>id</code>, die es nicht gibt, kann gar nicht entstehen.
</div>

## Muster über mehrere Schritte

### Wer spielt mit wem?

Jetzt die Frage, mit der die Filmdatenbank aufgehört hat: Welche zwei Schauspielerinnen oder Schauspieler standen besonders oft gemeinsam vor der Kamera? Im Graphen ist das ein Muster aus zwei Personen und einem gemeinsamen Film:

```cypher
(:Person)-[:ACTOR]->(:Movie)<-[:ACTOR]-(:Person)
```

_task(paare)

In Cypher:

```cypher
MATCH (a:Person)-[:ACTOR]->(m:Movie)<-[:ACTOR]-(b:Person)
WHERE a.id < b.id
RETURN a.name, b.name, count(m) AS together
ORDER BY together DESC
LIMIT 10;
```

<!-- after: paare -->
Wie in SQL sorgt `a.id < b.id` dafür, dass jedes Paar einmal vorkommt und niemand mit sich selbst. Der Rest ist das Muster selbst: zwei Personen, ein Film, zweimal `ACTOR`. Vergleich das mit der SQL-Abfrage, die jede Tabelle zweimal brauchte.
<!-- /after -->

<!-- tutorial-screenshot
# Neo4j Browser: die Schauspielpaare als Tabelle
tab: preview
viewport: 1029x700
click: selector:[data-testid=main-editor] .cm-content
press: Control+A
type: :clear
press: Control+Enter
press: Control+A
type: MATCH (a:Person)-[:ACTOR]->(m:Movie)<-[:ACTOR]-(b:Person) WHERE a.id < b.id RETURN a.name, b.name, count(m) AS together ORDER BY together DESC LIMIT 10;
press: Control+Enter
wait-for-text: Started streaming 10 records
click: Table
wait-for-text: together
move-mouse: selector:[data-testid=main-editor]
sleep: 1
variants: dark
-->

<img class='full' src='neo4j-browser-pairs.webp' alt='Neo4j Browser: Tabelle der zehn Schauspielpaare mit den meisten gemeinsamen Filmen.'>

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass die Zeile <code>WHERE a.id &lt; b.id</code> weg. Wie oft steht jetzt jedes Paar in der Liste, und was ist bei den beiden Zeilen eines Paars verschieden?
</div>

### Regie und Kamera

Gibt es Paare aus Regie und Kamera, die immer wieder zusammenarbeiten? Das Muster ändert sich nur bei den Typen:

```cypher
MATCH (d:Person)-[:DIRECTOR]->(m:Movie)<-[:CINEMATOGRAPHER]-(c:Person)
WITH d, c, count(m) AS together
WHERE together >= 3
RETURN d.name AS director, c.name AS cinematographer, together
ORDER BY together DESC;
```

`WITH` reicht Zwischenergebnisse weiter, hier die Zahl gemeinsamer Filme. Das `WHERE` danach filtert die fertigen Paare, so wie `HAVING` in SQL. In MySQL hätte die Frage `movie_crew`, `job` und `crew` je zweimal gebraucht; hier unterscheiden die Typen `DIRECTOR` und `CINEMATOGRAPHER` die beiden Rollen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Tausch <code>CINEMATOGRAPHER</code> gegen <code>COMPOSER</code>: Wer lässt die Musik seiner Filme immer wieder von derselben Person schreiben? Und dann gegen <code>WRITER</code>: Warum steht jetzt in manchen Zeilen zweimal derselbe Name?
</div>

### Wege durch den Graphen

Ein Graph kann auch nach **Wegen** gefragt werden: Wie hängen zwei Personen über Filme zusammen?

```cypher
MATCH (a:Person {name: 'Dwayne Johnson'}), (b:Person {name: 'Eddie Murphy'})
MATCH path = shortestPath((a)-[:ACTOR*..8]-(b))
RETURN path;
```

`[:ACTOR*..8]` heißt: ein Weg aus höchstens acht `ACTOR`-Beziehungen, in welche Richtung auch immer. Er führt also nur über Filme und die Menschen, die darin mitgespielt haben: Dwayne Johnson stand mit Samuel L. Jackson für »The Other Guys« vor der Kamera, und Samuel L. Jackson mit Eddie Murphy für »Coming to America«. Im Neo4j Browser siehst du das Ergebnis als Kette aus Personen und Filmen. Ohne `:ACTOR` darf der Weg jede Beziehung nehmen und führt dann über das Genre `Action`. Das stimmt, sagt aber nichts darüber, wer mit wem gearbeitet hat.

<!-- tutorial-screenshot
# Neo4j Browser: der kürzeste Weg von Dwayne Johnson zu Eddie Murphy
tab: preview
viewport: 1029x700
click: selector:[data-testid=main-editor] .cm-content
press: Control+A
type: :clear
press: Control+Enter
press: Control+A
type: MATCH (a:Person {name: 'Dwayne Johnson'}), (b:Person {name: 'Eddie Murphy'}) MATCH path = shortestPath((a)-[:ACTOR*..8]-(b)) RETURN path;
press: Control+Enter
wait-for-text: Started streaming 1 record
move-mouse: selector:[data-testid=main-editor]
sleep: 3
variants: dark
-->

<img class='full' src='neo4j-browser-path.webp' alt='Neo4j Browser: der Weg von Dwayne Johnson über The Other Guys, Samuel L. Jackson und Coming to America zu Eddie Murphy.'>

<div class='hint task'>
Such dir zwei Personen aus, die nichts miteinander zu tun zu haben scheinen. Über welche Filme und welche anderen Menschen sind sie verbunden? Findet Neo4j keinen Weg, gibt es in unseren Daten keinen mit höchstens acht Schritten.
</div>

_task(vergleich)

## Zusammenfassung

- **Knoten** stehen für Filme, Personen und Genres, ihr **Label** sagt, welche Art. **Eigenschaften** speichern Werte wie Titel oder Jahr.
- **Beziehungen** verbinden zwei Knoten direkt und haben einen **Typ** wie `IN_GENRE`, `ACTOR` oder `DIRECTOR`. Zwischentabellen und die Tabelle `job` aus MySQL werden zu Beziehungen und ihren Typen.
- `MATCH` beschreibt ein Muster, `WHERE` filtert, `RETURN` sagt, was herauskommt. Gruppiert wird von selbst, `WITH` reicht Zwischenergebnisse weiter.
- **Schlüssel** (`IS NODE KEY`) übernehmen die Aufgabe von Primärschlüsseln, **Indizes** beschleunigen Suchen.

Eine Graphdatenbank ist nicht besser als eine relationale. MySQL hat dir mit Tabellen, Primär- und Fremdschlüsseln eine klare Struktur gegeben. Aber sobald die Verbindungen selbst die Frage sind, wer mit wem, über welche Wege, lassen sie sich als Graph direkt hinschreiben.

<!-- tutorial-step
# Jede Cypher-Aufgabe nimmt ihre Lösung an und lehnt einen typischen Fehler ab
check-task: koenig-der-loewen <- MATCH (m:Movie {title: 'The Lion King', year: 1994}) RETURN m.runtime, m.german_title
check-task-fails: koenig-der-loewen <- MATCH (m:Movie {title: 'The Lion King'}) RETURN m.runtime, m.german_title
check-task: scifi <- MATCH (m:Movie)-[:IN_GENRE]->(g:Genre) WHERE g.name = 'Sci-Fi' AND m.year >= 2010 RETURN m.title, m.year ORDER BY m.year DESC
check-task-fails: scifi <- MATCH (m:Movie)-[:IN_GENRE]->(g:Genre) WHERE g.name = 'Sci-Fi' AND m.year >= 2010 RETURN m.title, m.year ORDER BY m.year
check-task: drei-stunden <- MATCH (m:Movie) WHERE m.runtime > 180 RETURN m.title, m.runtime
check-task-fails: drei-stunden <- MATCH (m:Movie) WHERE m.runtime > 3 RETURN m.title, m.runtime
check-task: comedy-scifi <- MATCH (c:Genre {name: 'Comedy'})<-[:IN_GENRE]-(m:Movie)-[:IN_GENRE]->(s:Genre {name: 'Sci-Fi'}) RETURN m.title
check-task-fails: comedy-scifi <- MATCH (m:Movie)-[:IN_GENRE]->(g:Genre) WHERE g.name = 'Comedy' OR g.name = 'Sci-Fi' RETURN DISTINCT m.title
check-task: nolan <- MATCH (p:Person {name: 'Christopher Nolan'})-[:DIRECTOR]->(m:Movie) RETURN m.title, m.year ORDER BY m.year
check-task-fails: nolan <- MATCH (p:Person {name: 'Christopher Nolan'})-[]->(m:Movie) RETURN DISTINCT m.title, m.year ORDER BY m.year
check-task: zimmer <- MATCH (p:Person {name: 'Hans Zimmer'})-[r]->(m:Movie) RETURN m.title, type(r)
check-task-fails: zimmer <- MATCH (p:Person {name: 'Hans Zimmer'})-[:COMPOSER]->(m:Movie) RETURN m.title
check-task: viel-regie <- MATCH (p:Person)-[:DIRECTOR]->(m:Movie) WITH p, count(m) AS n WHERE n >= 15 RETURN p.name, n ORDER BY n DESC
check-task-fails: viel-regie <- MATCH (p:Person)-[:DIRECTOR]->(m:Movie) RETURN p.name, count(m) AS n ORDER BY n DESC
-->
