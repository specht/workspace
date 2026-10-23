<div class='meta'>
kind: reference
search: Neo4j, Cypher, MATCH, WHERE, RETURN, Knoten, Beziehung, Label, Muster, count, WITH, shortestPath, CREATE, MERGE, SET, DELETE, Constraint, Index
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Cypher für Neo4j

<p class='abstract'>
Cypher zum Nachschlagen, so wie in <a href='/neo4j-filmdatenbank'>Filmdatenbank als Graph mit Neo4j</a>: Muster aus Knoten und Beziehungen, Abfragen, Zählen und Gruppieren, Wege durch den Graphen, Daten ändern, Schlüssel und Indizes. Zum Vergleich steht jeweils dabei, wie dasselbe in SQL aussieht; SQL selbst steht unter <a href='/reference/sql'>SQL</a>.
</p>

## Muster

<!-- part: cypher-muster -->
<!-- short: (m:Movie {title: '…'}) ist ein Knoten, -[:ACTOR]-> eine Beziehung; ein Muster beschreibt, was gefunden werden soll. -->
Eine Abfrage beschreibt, wie das Gesuchte im Graphen aussieht, als Zeichnung aus Text:

<table class='table table-sm reference-table'>
<tr><th>Muster</th><th>Bedeutung</th></tr>
<tr><td><code>(m)</code></td><td>ein Knoten, irgendeiner; <code>m</code> ist sein Name in dieser Abfrage</td></tr>
<tr><td><code>(m:Movie)</code></td><td>ein Knoten mit dem Label <code>Movie</code></td></tr>
<tr><td><code>(m:Movie {year: 1985})</code></td><td>mit einer Eigenschaft, die genau diesen Wert hat</td></tr>
<tr><td><code>(:Genre {name: 'Comedy'})</code></td><td>ohne Namen, wenn er später nicht gebraucht wird</td></tr>
<tr><td><code>(p)-[:ACTOR]-&gt;(m)</code></td><td>eine Beziehung vom Typ <code>ACTOR</code> von <code>p</code> zu <code>m</code></td></tr>
<tr><td><code>(p)-[r]-&gt;(m)</code></td><td>eine Beziehung irgendeines Typs, mit dem Namen <code>r</code>; <code>type(r)</code> nennt ihren Typ</td></tr>
<tr><td><code>(a)-[:ACTOR]-&gt;(m)&lt;-[:ACTOR]-(b)</code></td><td>zwei Personen, die im selben Film spielen</td></tr>
<tr><td><code>(a)-[:ACTOR*..8]-(b)</code></td><td>ein Weg aus höchstens 8 Beziehungen, in beliebiger Richtung (ohne Pfeilspitze)</td></tr>
</table>

Labels schreibt man groß (`Movie`), Beziehungstypen ganz groß (`IN_GENRE`), Eigenschaften klein (`title`). Groß- und Kleinschreibung zählt: `:movie` findet nichts.
<!-- /part -->

## Abfragen

<!-- part: cypher-abfragen -->
<!-- short: MATCH Muster, WHERE Bedingung, RETURN was, ORDER BY, LIMIT; wie SELECT … FROM … WHERE in SQL. -->
```cypher
MATCH (p:Person)-[:COMPOSER]->(m:Movie)
WHERE p.birth_year >= 1970 AND m.runtime < 100
RETURN DISTINCT p.name, p.birth_year
ORDER BY p.birth_year DESC, p.name
LIMIT 10;
```

<table class='table table-sm reference-table'>
<tr><th>Teil</th><th>Was er tut</th><th>In SQL</th></tr>
<tr><td><code>MATCH muster</code></td><td>sucht alle Stellen im Graphen, auf die das Muster passt</td><td><code>FROM</code> und <code>JOIN</code></td></tr>
<tr><td><code>WHERE bedingung</code></td><td>behält nur die, für die sie gilt: <code>=</code>, <code>&lt;&gt;</code>, <code>&lt;</code>, <code>AND</code>, <code>OR</code>, <code>NOT</code>, <code>IS NULL</code></td><td><code>WHERE</code></td></tr>
<tr><td><code>RETURN a, b.name AS name</code></td><td>was ausgegeben wird; <code>DISTINCT</code> ohne Wiederholungen</td><td><code>SELECT</code></td></tr>
<tr><td><code>ORDER BY x DESC, y</code></td><td>sortiert, <code>DESC</code> absteigend; bei gleichem <code>x</code> nach <code>y</code></td><td><code>ORDER BY</code></td></tr>
<tr><td><code>LIMIT 10</code>, <code>SKIP 10</code></td><td>nur die ersten 10, die ersten 10 überspringen</td><td><code>LIMIT</code>, <code>OFFSET</code></td></tr>
</table>

Texte stehen in einfachen oder doppelten Anführungszeichen. `WHERE m.title CONTAINS 'Future'`, `STARTS WITH` und `ENDS WITH` suchen Teile von Texten, `=~ '(?i).*zukunft.*'` mit einem regulären Ausdruck, ohne Groß- und Kleinschreibung. Im Neo4j Browser zeigt `RETURN m` den Graphen, `RETURN m.title` eine Tabelle.
<!-- /part -->

<!-- part: cypher-zaehlen -->
<!-- short: count, avg, min, max, sum, collect; gruppiert wird nach allem anderen in RETURN; WITH … WHERE wie HAVING. -->
### Zählen und gruppieren

```cypher
MATCH (m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name, count(m) AS movies, round(avg(m.runtime)) AS minutes
ORDER BY movies DESC;
```

`count`, `avg`, `min`, `max`, `sum` und `collect` (alle Werte als Liste) fassen zusammen. Gruppiert wird nach allem anderen, was in `RETURN` steht, hier nach `g.name`; ein `GROUP BY` wie in SQL gibt es nicht. Eine Bedingung an ein gezähltes Ergebnis, in SQL `HAVING`, schreibt man mit `WITH`, das ein Zwischenergebnis weitergibt:

```cypher
MATCH (d:Person)-[:DIRECTOR]->(m:Movie)<-[:CINEMATOGRAPHER]-(c:Person)
WITH d, c, count(m) AS together
WHERE together >= 3
RETURN d.name, c.name, together
ORDER BY together DESC;
```
<!-- /part -->

<!-- part: cypher-wege -->
<!-- short: Ein Muster über mehrere Knoten ersetzt viele JOINs; shortestPath findet den kürzesten Weg. -->
### Über mehrere Schritte

```cypher
MATCH (:Person {name: 'Michael J. Fox'})-[:ACTOR]->(m:Movie)-[:IN_GENRE]->(g:Genre)
RETURN g.name, count(m) AS movies
ORDER BY movies DESC;
```

Ein Muster darf über mehrere Knoten gehen: von einer Person zu ihren Filmen und weiter zu deren Genres. In SQL wäre das eine lange Kette von `JOIN`s.

```cypher
MATCH (a:Person {name: 'Dwayne Johnson'}), (b:Person {name: 'Eddie Murphy'})
MATCH path = shortestPath((a)-[:ACTOR*..8]-(b))
RETURN path, length(path);
```

`shortestPath` findet den kürzesten Weg zwischen zwei Knoten, hier über gemeinsame Filme und höchstens 8 Beziehungen lang. `length(path)` zählt seine Beziehungen, `nodes(path)` gibt seine Knoten als Liste.

Ein Weg mit offener Länge (`*`) ohne Obergrenze kann in einem großen Graphen sehr lange dauern: Gib immer eine an, wie `*..8`.
<!-- /part -->

## Daten ändern

<!-- part: cypher-aendern -->
<!-- short: CREATE legt an, MERGE nur, wenn es das noch nicht gibt, SET ändert, DELETE löscht; DETACH DELETE mit allen Beziehungen. -->
<table class='table table-sm reference-table'>
<tr><th>Abfrage</th><th>Was sie tut</th></tr>
<tr><td><code>CREATE (:Genre {id: 99, name: 'Heimatfilm'})</code></td><td>legt einen Knoten an</td></tr>
<tr><td><code>MATCH (m:Movie {id: 233}), (g:Genre {id: 99})</code><br><code>CREATE (m)-[:IN_GENRE]-&gt;(g)</code></td><td>legt eine Beziehung zwischen zwei gefundenen Knoten an</td></tr>
<tr><td><code>MERGE (g:Genre {name: 'Heimatfilm'})</code></td><td>findet den Knoten, oder legt ihn an, wenn es ihn noch nicht gibt</td></tr>
<tr><td><code>MATCH (m:Movie {id: 233}) SET m.runtime = 116</code></td><td>ändert eine Eigenschaft oder legt sie an</td></tr>
<tr><td><code>MATCH (g:Genre {id: 99}) DELETE g</code></td><td>löscht einen Knoten, aber nur, wenn keine Beziehung mehr an ihm hängt</td></tr>
<tr><td><code>MATCH (g:Genre {id: 99}) DETACH DELETE g</code></td><td>löscht ihn mit allen seinen Beziehungen</td></tr>
</table>

<div class='hint warning'>
<code>MATCH (n) DETACH DELETE n</code> löscht den ganzen Graphen, ohne Rückfrage. Die Filmdaten holst du danach nur zurück, indem du sie <a href='/neo4j-filmdatenbank#die-filmdaten-laden'>neu lädst</a>.
</div>
<!-- /part -->

<!-- part: cypher-regeln -->
<!-- short: CREATE CONSTRAINT … REQUIRE … IS NODE KEY wie ein Primärschlüssel; CREATE INDEX beschleunigt die Suche. -->
### Schlüssel und Indizes

```cypher
CREATE CONSTRAINT movie_id IF NOT EXISTS FOR (m:Movie) REQUIRE m.id IS NODE KEY;
CREATE INDEX movie_title IF NOT EXISTS FOR (m:Movie) ON (m.title);
SHOW CONSTRAINTS;
SHOW INDEXES;
```

Ein **Schlüssel** verlangt, dass jeder Knoten mit dem Label die Eigenschaft hat und kein Wert doppelt vorkommt, wie ein Primärschlüssel in SQL. Ein **Index** macht die Suche nach einer Eigenschaft schnell und verbietet nichts.
<!-- /part -->

## SQL und Cypher

<!-- part: cypher-sql -->
<!-- short: Dieselben Fragen in SQL und in Cypher nebeneinander. -->
<table class='table table-sm reference-table'>
<tr><th>Frage</th><th>SQL</th><th>Cypher</th></tr>
<tr><td>ein Film</td><td><code>SELECT * FROM movie WHERE title = '…'</code></td><td><code>MATCH (m:Movie {title: '…'}) RETURN m</code></td></tr>
<tr><td>die Genres eines Films</td><td><code>movie JOIN movie_genre JOIN genre</code></td><td><code>(m:Movie)-[:IN_GENRE]-&gt;(g:Genre)</code></td></tr>
<tr><td>Filme je Genre</td><td><code>GROUP BY genre.name</code></td><td><code>RETURN g.name, count(m)</code></td></tr>
<tr><td>nur große Gruppen</td><td><code>HAVING count(*) &gt;= 3</code></td><td><code>WITH …, count(m) AS n WHERE n &gt;= 3</code></td></tr>
<tr><td>ein Weg über viele Schritte</td><td>ein <code>JOIN</code> je Schritt, oder rekursiv</td><td><code>shortestPath((a)-[*..8]-(b))</code></td></tr>
</table>
<!-- /part -->

## Zum Nachschlagen

- [Neo4j Cypher Cheat Sheet](https://neo4j.com/docs/cypher-cheat-sheet/current/): jede Klausel mit einem kurzen Beispiel, auf Englisch.
- [Neo4j Cypher Manual](https://neo4j.com/docs/cypher-manual/current/): die ganze Sprache, mit allen Funktionen, auf Englisch.
