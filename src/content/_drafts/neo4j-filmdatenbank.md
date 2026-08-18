<div class='meta'>
image: neo4j-nosferatu.webp:0:50
</div>

# Filmdatenbank als Graph mit Neo4j

<p class='abstract'>
Im letzten Kapitel hast du dieselben Filme, Genres und Personen in einer relationalen MySQL-Datenbank modelliert. Beziehungen wurden dort mit Fremdschlüsseln und Zwischentabellen gespeichert und mit <code>JOIN</code> wieder zusammengesetzt. Jetzt modellieren wir dieselben Daten als Graphen in <strong>Neo4j</strong>. Dabei werden Filme, Personen und Genres zu Knoten – und ihre Beziehungen zu direkten Verbindungen im Graphen. Du lernst die Abfragesprache <strong>Cypher</strong> kennen und siehst am Ende, warum Fragen nach häufig zusammenarbeitenden Personen in einer Graphdatenbank plötzlich viel natürlicher aussehen.
</p>

## Dieselben Daten, ein anderes Modell

Im letzten Kapitel bestand unsere Filmdatenbank am Ende aus sechs Tabellen:

```text
movie
genre
movie_genre
crew
job
movie_crew
```

Für die Genres führte ein Weg durch eine Zwischentabelle:

```text
movie → movie_genre → genre
```

Bei Personen war der Weg noch länger:

```text
             job
              ↑
movie → movie_crew → crew
```

Das funktioniert gut. Aber die letzten Fragen des Kapitels drehten sich immer stärker um die **Beziehungen selbst**:

- Welche Schauspieler:innen haben oft gemeinsam gespielt?
- Welche Regisseur:innen und Kameraleute arbeiten immer wieder zusammen?
- Über welche Filme sind zwei Personen miteinander verbunden?

Eine Graphdatenbank modelliert genau solche Verbindungen direkt.

Für unsere Filmdatenbank verwenden wir drei Arten von **Knoten**:

```text
Movie
Person
Genre
```

Zwischen diesen Knoten liegen **Beziehungen**:

```text
(Movie)-[:IN_GENRE]->(Genre)

(Person)-[:ACTOR]->(Movie)
(Person)-[:DIRECTOR]->(Movie)
(Person)-[:WRITER]->(Movie)
(Person)-[:PRODUCER]->(Movie)
(Person)-[:COMPOSER]->(Movie)
(Person)-[:CINEMATOGRAPHER]->(Movie)
```

Damit verschwinden gleich zwei Dinge aus dem relationalen Modell:

- Die Zwischentabellen `movie_genre` und `movie_crew` werden zu Beziehungen.
- Die Tabelle `job` wird nicht mehr benötigt, weil die Tätigkeit direkt im **Typ der Beziehung** steckt.

<div class='hint'>
Das bedeutet nicht, dass man eine relationale Datenbank immer auf diese Weise in einen Graphen „übersetzen“ muss. Wir wählen dieses Modell, weil bei unseren Filmdaten gerade die Beziehungen zwischen Filmen, Genres und Personen interessant sind.
</div>

## Knoten haben Eigenschaften

Ein Knoten kann Eigenschaften besitzen. Der Film **Nosferatu** sieht gedanklich ungefähr so aus:

```text
(:Movie {
  id: 2,
  title: "Nosferatu",
  year: 1922,
  runtime: 95,
  rating: 7.8,
  original_title: "Nosferatu, eine Symphonie des Grauens"
})
```

Dabei bedeutet:

```text
Movie        → Label des Knotens
id           → Eigenschaft
title        → Eigenschaft
year         → Eigenschaft
...
```

Ein Genre ist ebenfalls ein Knoten:

```text
(:Genre {
  id: 5,
  name: "Horror"
})
```

Und eine Person zum Beispiel:

```text
(:Person {
  id: 1573,
  name: "...",
  birth_year: ...
})
```

Zwischen zwei Knoten kann eine Beziehung liegen:

```text
(:Movie)-[:IN_GENRE]->(:Genre)
```

oder:

```text
(:Person)-[:DIRECTOR]->(:Movie)
```

Die eckigen Klammern stehen für die **Beziehung**, der Name hinter dem Doppelpunkt für ihren **Typ**.

<div class='hint task'>
Vergleiche das Graphmodell mit dem ER-Diagramm aus dem letzten Kapitel. Welche Tabellen sind zu Knoten geworden? Welche Tabellen sind vollständig verschwunden?
</div>

## Die Filmdaten in Neo4j laden

Wir verwenden genau denselben aufbereiteten IMDb-Datensatz wie im MySQL-Kapitel.

Falls das Repository noch nicht geöffnet ist, klone es über **Clone Repository**:

```text
https://github.com/specht/videothek.git
```

Bestätige `/workspace/` als Ziel und öffne das Repository.

Darin liegt die Datei:

```text
videothek-data.neo4j
```

Sie ist ein logischer Dump für `neo4j_bolt`. Im Workspace sind Host, Benutzername, Passwort und Datenbank bereits als Umgebungsvariablen eingerichtet. Deshalb reicht zum Import:

```bash
neo4j_bolt load videothek-data.neo4j
```

<div class='hint'>
Der Import erwartet eine leere Neo4j-Datenbank. Falls du dort bereits experimentiert hast, kannst du deine Neo4j-Datenbank vorher im Workspace-Profil zurücksetzen.
</div>

<!-- tutorial-screenshot
show-bottom-panel
terminal-open
terminal-maximize
terminal-run: neo4j_bolt load videothek-data.neo4j
terminal-wait-for-prompt
crop-terminal-lines: auto
crop-terminal-skip-bottom: 1
-->

<img class='full' src='neo4j-load.webp' alt='neo4j_bolt lädt die vorbereiteten Film-, Personen- und Genredaten in die Neo4j-Datenbank.'>

`neo4j_bolt` kümmert sich beim Laden darum, zunächst die Knoten und anschließend die Beziehungen zwischen ihnen anzulegen.

## Neo4j Browser öffnen

Für die Abfragen verwenden wir den **Neo4j Browser**. Öffne ihn über den Neo4j-Bereich im Workspace.

Der Browser ist nicht nur eine Visualisierung. Du kannst dort Cypher-Abfragen schreiben und Ergebnisse je nach Abfrage als Graph oder als Tabelle betrachten.

Beginnen wir mit einer sehr einfachen Abfrage:

```cypher
MATCH (n)
RETURN n
LIMIT 25;
```

<!-- manual screenshot: Neo4j Browser with MATCH (n) RETURN n LIMIT 25 in graph view -->
<img class='full' src='neo4j-first-graph.webp' alt='Der Neo4j Browser zeigt die ersten Knoten der Filmdatenbank als Graphen.'>

Die Abfrage bedeutet:

```text
MATCH (n)    → finde Knoten und nenne jeden gefundenen Knoten n
RETURN n     → gib diese Knoten zurück
LIMIT 25     → höchstens 25 Ergebnisse
```

Das `n` ist nur ein frei gewählter Variablenname.

Wir können zusätzlich angeben, welches **Label** ein Knoten haben soll:

```cypher
MATCH (m:Movie)
RETURN m
LIMIT 10;
```

Jetzt werden nur Knoten mit dem Label `Movie` gefunden.

<div class='hint task'>
Probiere entsprechende Abfragen für <code>Person</code> und <code>Genre</code> aus.
</div>

## Einen bestimmten Film finden

Wie in SQL können wir nach Eigenschaften filtern:

```cypher
MATCH (m:Movie)
WHERE m.title = 'Nosferatu'
  AND m.year = 1922
RETURN m;
```

Eine häufig verwendete Kurzschreibweise ist, Eigenschaften direkt in das Muster zu schreiben:

```cypher
MATCH (m:Movie {title: 'Nosferatu', year: 1922})
RETURN m;
```

Beide Abfragen suchen denselben Film.

Wenn wir nicht den ganzen Knoten, sondern nur bestimmte Eigenschaften zurückgeben wollen:

```cypher
MATCH (m:Movie {title: 'Nosferatu', year: 1922})
RETURN m.title, m.year, m.runtime, m.rating;
```

Der Browser zeigt dieses Ergebnis sinnvollerweise als Tabelle.

<div class='hint task'>
Finde <strong>Metropolis</strong> aus dem Jahr 1927 und gib Titel, Laufzeit und Bewertung zurück.
</div>

## Beziehungen folgen

Jetzt kommt der entscheidende Unterschied zum vorherigen Kapitel.

Welche Genres hat **Nosferatu**?

In MySQL mussten wir den Weg

```text
movie → movie_genre → genre
```

mit zwei `JOIN`s zurückgehen.

Im Graphen ist die Beziehung bereits vorhanden:

```text
(Movie)-[:IN_GENRE]->(Genre)
```

Deshalb können wir genau dieses Muster suchen:

```cypher
MATCH (m:Movie {title: 'Nosferatu', year: 1922})
      -[:IN_GENRE]->
      (g:Genre)
RETURN m, g;
```

<!-- manual screenshot: Neo4j Browser showing Nosferatu connected to its genres -->
<img class='full' src='neo4j-nosferatu-genres.webp' alt='Der Neo4j Browser zeigt Nosferatu und seine Genre-Knoten mit IN_GENRE-Beziehungen.'>

Lies die mittlere Zeile fast wie eine kleine Zeichnung:

```text
(m:Movie)-[:IN_GENRE]->(g:Genre)
```

- `(m:Movie)` ist ein Filmknoten.
- `[:IN_GENRE]` ist eine Beziehung dieses Typs.
- `->` zeigt die Richtung der Beziehung.
- `(g:Genre)` ist der verbundene Genreknoten.

Wenn wir nur die Namen brauchen:

```cypher
MATCH (m:Movie {title: 'Nosferatu', year: 1922})
      -[:IN_GENRE]->
      (g:Genre)
RETURN g.name;
```

Das ist die zentrale Idee von Cypher:

> **Eine Abfrage kann als Muster des gesuchten Graphen geschrieben werden.**

<div class='hint task'>
Formuliere eine Abfrage, die die Genres von <strong>Metropolis</strong> aus dem Jahr 1927 zurückgibt.
</div>

## Alle Beteiligten eines Films sichtbar machen

Bei einem Film führen verschiedene Beziehungstypen von Personen zum Film.

Wir können zunächst alle diese Beziehungen zulassen:

```cypher
MATCH (p:Person)-[r]->(m:Movie {title: 'Nosferatu', year: 1922})
RETURN p, r, m;
```

<!-- manual screenshot: Neo4j Browser showing Nosferatu with people and relationship types -->
<img class='full' src='neo4j-nosferatu-crew.webp' alt='Der Neo4j Browser zeigt Personen rund um Nosferatu mit Beziehungen wie ACTOR und DIRECTOR.'>

Die Beziehung hat diesmal eine eigene Variable `r`.

Damit können wir auch ihren Typ abfragen:

```cypher
MATCH (p:Person)-[r]->(m:Movie {title: 'Nosferatu', year: 1922})
RETURN p.name, type(r) AS job
ORDER BY job, p.name;
```

Hier sieht man einen großen Unterschied zum relationalen Modell:

```text
MySQL:
movie ← movie_crew → crew
          ↓
         job

Neo4j:
(Person)-[:DIRECTOR]->(Movie)
(Person)-[:ACTOR]->(Movie)
```

Die Tätigkeit muss nicht über eine weitere Tabelle nachgeschlagen werden. Sie ist Teil der Beziehung.

## Filme eines Genres finden

Welche besonders gut bewerteten Animationsfilme enthält die Datenbank?

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre {name: 'Animation'})
RETURN m.title, m.year, m.rating
ORDER BY m.rating DESC, m.year DESC
LIMIT 10;
```

<!-- manual screenshot: Browser table with top Animation movies -->
<img class='full' src='neo4j-animation-top.webp' alt='Eine Cypher-Abfrage zeigt zehn besonders hoch bewertete Animationsfilme.'>

Vergleiche das gedanklich mit der SQL-Abfrage aus dem vorherigen Kapitel:

```text
SQL:
movie
  JOIN movie_genre
  JOIN genre
  WHERE genre.name = ...

Cypher:
(Movie)-[:IN_GENRE]->(Genre {name: ...})
```

<div class='hint task'>
Formuliere Cypher-Abfragen für diese Fragen:

1. Welche zehn am besten bewerteten Science-Fiction-Filme enthält die Datenbank?
2. Welche Horrorfilme ab dem Jahr 2000 haben eine Bewertung von mindestens 8,0?
3. Welche Filme gehören gleichzeitig zu den Genres <strong>Drama</strong> und <strong>Crime</strong>?
</div>

## Zählen und gruppieren

Cypher besitzt ebenfalls Aggregatfunktionen wie `count()` und `avg()`.

Wie viele Filme gibt es pro Genre?

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name, count(m) AS movies
ORDER BY movies DESC
LIMIT 10;
```

<!-- manual screenshot: Browser table showing films per genre -->
<img class='full' src='neo4j-films-per-genre.webp' alt='Cypher zeigt die zehn Genres mit den meisten Filmen.'>

Ein ausdrückliches `GROUP BY` gibt es hier nicht.

Cypher erkennt an der `RETURN`-Zeile:

```cypher
RETURN g.name, count(m)
```

dass nach `g.name` gruppiert und für jede Gruppe `count(m)` berechnet werden soll.

Auch Durchschnittswerte funktionieren:

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name,
       count(m) AS movies,
       round(avg(m.rating), 2) AS average_rating
ORDER BY average_rating DESC;
```

<div class='hint task'>
Welche drei Fragen an die Filmdatenbank kannst du formulieren, bei denen eine Aggregatfunktion wie <code>count()</code>, <code>avg()</code>, <code>min()</code> oder <code>max()</code> sinnvoll ist?
</div>

## Wer führte Regie?

Im relationalen Modell brauchten wir für die Frage

> Wer führte bei **Nosferatu** Regie?

drei `JOIN`s:

```text
movie → movie_crew → crew
                  ↘ job
```

Im Graphmodell existiert die gesuchte Beziehung direkt:

```cypher
MATCH (p:Person)-[:DIRECTOR]->(m:Movie {title: 'Nosferatu', year: 1922})
RETURN p.name;
```

Das Muster

```text
(Person)-[:DIRECTOR]->(Movie)
```

ist fast schon die Antwort auf die Frage.

Wer spielte in **Nosferatu** mit?

```cypher
MATCH (p:Person)-[:ACTOR]->(m:Movie {title: 'Nosferatu', year: 1922})
RETURN p.name
ORDER BY p.name;
```

<div class='hint task'>
Formuliere Abfragen für diese Fragen:

1. Bei welchen Filmen führte <strong>Christopher Nolan</strong> Regie?
2. An welchen Filmen war <strong>Charlie Chaplin</strong> beteiligt und welche Tätigkeit hatte er jeweils?
3. Welche zehn Personen kommen am häufigsten mit einer <code>DIRECTOR</code>-Beziehung vor?
</div>

<div class='hint'>
Für die zweite Aufgabe kannst du die Beziehung in einer Variablen speichern und ihren Typ mit <code>type(r)</code> ausgeben.
</div>

## Constraints: Auch ein Graph braucht Regeln

Neo4j wird manchmal als „schemalos“ bezeichnet. Das bedeutet aber nicht, dass sinnvolle Regeln für die Daten überflüssig wären.

Im relationalen Modell hatten unsere Tabellen Primärschlüssel:

```text
movie.id
genre.id
crew.id
```

Auch im Graphen verwenden wir diese IDs weiter:

```text
(:Movie {id: ...})
(:Genre {id: ...})
(:Person {id: ...})
```

Wir wollen nicht versehentlich zwei `Movie`-Knoten mit derselben ID anlegen.

Im Workspace verwenden wir deshalb Schlüssel-Constraints:

```cypher
CREATE CONSTRAINT movie_id IF NOT EXISTS
FOR (m:Movie)
REQUIRE m.id IS NODE KEY;
```

```cypher
CREATE CONSTRAINT genre_id IF NOT EXISTS
FOR (g:Genre)
REQUIRE g.id IS NODE KEY;
```

```cypher
CREATE CONSTRAINT person_id IF NOT EXISTS
FOR (p:Person)
REQUIRE p.id IS NODE KEY;
```

Ein solcher Schlüssel sagt für das jeweilige Label:

```text
Die Eigenschaft id muss vorhanden sein
und ihr Wert muss eindeutig sein.
```

Das kommt der Aufgabe eines Primärschlüssels in unserem relationalen Modell sehr nahe.

Für Eigenschaften, nach denen wir häufig suchen, können wir zusätzlich Indizes anlegen:

```cypher
CREATE INDEX movie_title IF NOT EXISTS
FOR (m:Movie)
ON (m.title);
```

```cypher
CREATE INDEX genre_name IF NOT EXISTS
FOR (g:Genre)
ON (g.name);
```

```cypher
CREATE INDEX person_name IF NOT EXISTS
FOR (p:Person)
ON (p.name);
```

Mit

```cypher
SHOW CONSTRAINTS;
```

und

```cypher
SHOW INDEXES;
```

kannst du dir die vorhandenen Regeln und Indizes anzeigen lassen.

<div class='hint'>
Ein direktes Gegenstück zum SQL-Fremdschlüssel brauchen die Beziehungen unseres Graphen nicht. Eine Beziehung verbindet immer tatsächlich vorhandene Knoten. Es kann also keine Beziehung geben, deren Ziel nur aus einer nicht vorhandenen ID besteht.
</div>

<!-- TODO before publication:
Decide whether Neo4jBolt logical dumps should preserve schema. If dump/load gains
constraint/index support, either let the prepared dump restore the schema and keep
the CREATE statements above as explanation, or explicitly make the classroom dump
data-only so students still create the schema themselves.
-->

## Welche Schauspieler:innen arbeiten besonders oft zusammen?

Jetzt kommen wir zu der Frage, mit der das MySQL-Kapitel geendet hat:

> **Welche zwei Schauspieler:innen haben besonders oft gemeinsam in einem Film gespielt?**

In SQL mussten wir dafür `movie_crew`, `job` und `crew` jeweils zweimal verwenden.

Im Graphen suchen wir dagegen direkt dieses Muster:

```text
(Person)-[:ACTOR]->(Movie)<-[:ACTOR]-(Person)
```

In Cypher:

```cypher
MATCH (actor1:Person)-[:ACTOR]->(movie:Movie)<-[:ACTOR]-(actor2:Person)
WHERE actor1.id < actor2.id
WITH actor1, actor2, count(DISTINCT movie) AS movies_together
WHERE movies_together >= 2
RETURN actor1.name AS actor_1,
       actor2.name AS actor_2,
       movies_together
ORDER BY movies_together DESC, actor_1, actor_2
LIMIT 20;
```

<!-- manual screenshot: Browser table with actor pairs -->
<img class='full' src='neo4j-actor-pairs.webp' alt='Die Cypher-Abfrage zeigt Schauspielerpaare, die besonders häufig gemeinsam in Filmen vorkommen.'>

Die Abfrage ist nicht trivial. Aber ihr Kern ist sofort sichtbar:

```cypher
(actor1:Person)-[:ACTOR]->(movie:Movie)<-[:ACTOR]-(actor2:Person)
```

Das ist genau die gesuchte Situation:

```text
Schauspieler:in 1 → gemeinsamer Film ← Schauspieler:in 2
```

Wie in der SQL-Abfrage verhindert

```cypher
actor1.id < actor2.id
```

dass jedes Paar zweimal vorkommt:

```text
Person A + Person B
Person B + Person A
```

`WITH` berechnet anschließend die Anzahl gemeinsamer Filme. Die zweite `WHERE`-Bedingung behält nur Paare mit mindestens zwei gemeinsamen Filmen.

Vergleiche vor allem nicht die Anzahl der Schlüsselwörter, sondern die Art, wie die Beziehung beschrieben wird:

```text
SQL:
Tabellen mehrfach verbinden und über IDs wieder zusammensetzen

Cypher:
das gesuchte Beziehungsmuster hinschreiben
```

## Gibt es feste Teams aus Regie und Kamera?

Die zweite große Abschlussfrage aus dem MySQL-Kapitel lautete:

> **Welche Regisseur:innen und Kameraleute haben besonders häufig zusammengearbeitet?**

Das gesuchte Muster ist:

```text
(Regie)-[:DIRECTOR]->(Film)<-[:CINEMATOGRAPHER]-(Kamera)
```

Daraus wird:

```cypher
MATCH (director:Person)-[:DIRECTOR]->(movie:Movie)
      <-[:CINEMATOGRAPHER]-(cinematographer:Person)
WITH director,
     cinematographer,
     count(DISTINCT movie) AS movies_together
WHERE movies_together >= 2
RETURN director.name AS director,
       cinematographer.name AS cinematographer,
       movies_together
ORDER BY movies_together DESC, director, cinematographer
LIMIT 20;
```

<!-- manual screenshot: Browser table with director/cinematographer pairs -->
<img class='full' src='neo4j-director-camera-pairs.webp' alt='Eine Cypher-Abfrage zeigt Regie-Kamera-Paare, die mehrfach gemeinsam an Filmen gearbeitet haben.'>

Im relationalen Modell mussten wir für dieselbe Frage zwei verschiedene Rollen durch mehrere Aliase auseinanderhalten:

```text
directing
director_job
director
camera
camera_job
cinematographer
```

Im Graphmodell unterscheiden bereits die Beziehungstypen:

```text
DIRECTOR
CINEMATOGRAPHER
```

die beiden Rollen.

Genau für solche Fragen lohnt sich die andere Datenmodellierung.

## Wege durch den Graphen

Graphdatenbanken können nicht nur direkte Nachbarschaften untersuchen. Man kann auch nach ganzen **Pfaden** suchen.

Wähle zwei Personen aus der Datenbank und probiere zum Beispiel:

```cypher
MATCH (a:Person {name: 'PERSON A'}),
      (b:Person {name: 'PERSON B'})
MATCH path = shortestPath((a)-[*..8]-(b))
RETURN path;
```

Ersetze `PERSON A` und `PERSON B` durch zwei vorhandene Namen.

Der Ausdruck

```text
[*..8]
```

bedeutet: Der Pfad darf aus bis zu acht Beziehungen bestehen.

<!-- manual screenshot: Browser visualization of a shortest path between two people -->
<img class='full' src='neo4j-shortest-path.webp' alt='Der Neo4j Browser visualisiert einen kurzen Verbindungspfad zwischen zwei Personen über Filme.'>

Gerade hier ist die Graphansicht des Neo4j Browsers besonders hilfreich: Das Ergebnis ist nicht nur eine Tabelle mit IDs, sondern ein sichtbarer Weg durch Filme, Personen und ihre Beziehungen.

<div class='hint task'>
Suche zwei Personen, zwischen denen ein interessanter kurzer Pfad existiert. Welche Filme und welche anderen Personen verbinden sie?
</div>

## Zusammenfassung

In diesem Kapitel hast du dieselben Filmdaten auf eine zweite Art modelliert und abgefragt:

- **Knoten** repräsentieren Dinge wie Filme, Personen und Genres.
- **Labels** wie `Movie`, `Person` und `Genre` beschreiben Arten von Knoten.
- **Eigenschaften** speichern Werte wie Titel, Jahr oder Name.
- **Beziehungen** verbinden Knoten direkt miteinander.
- **Beziehungstypen** wie `IN_GENRE`, `ACTOR` oder `DIRECTOR` tragen Bedeutung.
- Mit `MATCH` beschreibst du ein Muster, das Neo4j im Graphen suchen soll.
- Aggregatfunktionen wie `count()` und `avg()` funktionieren auch in Cypher.
- Constraints schützen wichtige Eigenschaften wie IDs.
- Indizes beschleunigen häufige Suchen.
- Besonders bei Fragen über Zusammenarbeit und Verbindungspfade kann Cypher die gesuchte Struktur direkt als Graphmuster ausdrücken.

Relationale Datenbanken sind deshalb nicht „schlechter“ als Graphdatenbanken. In MySQL war unser Modell sehr klar: Tabellen, Primärschlüssel und Fremdschlüssel sorgen für eine saubere Struktur.

Aber bei den letzten Fragen war nicht mehr ein einzelner Film oder eine einzelne Person das eigentlich Interessante.

Es waren die **Beziehungen**.

Und genau dort spielt eine Graphdatenbank ihre Stärke aus.
