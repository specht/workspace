Lernende fragen dieselben Filmdaten wie in der »Filmdatenbank mit MySQL« als Graph ab, in Neo4j mit Cypher, und vergleichen die Abfragen mit SQL.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die Aktivität setzt voraus, dass die Lernenden <code>JOIN</code> und <code>GROUP BY</code> kennen, denn die Vergleiche mit SQL tragen das Kapitel.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> vom Tabellenmodell zum Graphen, ein kleiner Graph von Hand mit <code>CREATE</code>, die Daten laden, erste Abfragen mit <code>MATCH</code>.</li>
<li><b>2. Doppelstunde:</b> Regeln für den Graphen, Muster über mehrere Schritte und der Vergleich mit SQL.</li>
</ul></td></tr>
<tr><th>Ordner</th><td><code>/workspace/filmdatenbank</code> (wie im MySQL-Kapitel); darin nur die heruntergeladene <code>movies-neo4j.dump</code></td></tr>
<tr><th>Daten</th><td><code>movie-archive/movies-neo4j.dump</code> aus <code>specht/workspace-files</code>, gebaut von <code>src/scripts/collect-movies</code> zusammen mit den MySQL-Dateien, also dieselben Filme, Genres und Personen. Knoten <code>Movie</code>, <code>Person</code>, <code>Genre</code>; Beziehungen <code>IN_GENRE</code> und je eine pro Tätigkeit (<code>ACTOR</code>, <code>DIRECTOR</code>, <code>WRITER</code>, <code>PRODUCER</code>, <code>COMPOSER</code>, <code>CINEMATOGRAPHER</code>, <code>EDITOR</code>). Der Dump enthält nur Daten; Schlüssel und Indizes legen die Lernenden selbst an.</td></tr>
<tr><th>Werkzeuge</th><td><code>neo4j_bolt load</code> zum Laden und <code>cypher-shell</code> für Tabellen im Terminal; beide lesen <code>NEO4J_URI</code>, <code>NEO4J_USERNAME</code>, <code>NEO4J_PASSWORD</code> und <code>NEO4J_DATABASE</code> aus dem Workspace. Der Neo4j Browser (Knopf »Neo4j« im Menü, sobald Neo4j im Profil eingeschaltet ist) zeigt Graphen; angemeldet wird dort mit den Daten aus dem Profil, die eigene Datenbank ist die Heimdatenbank.</td></tr>
<tr><th>Was gelesen wird</th><td><ul>
<li>Sieben Abfrage-Aufgaben prüft der Workspace selbst: Die Abfrage läuft in der eigenen Neo4j-Datenbank der Person, angemeldet als sie, nur lesend und höchstens 5 Sekunden lang, und ihr Ergebnis wird mit dem einer Musterabfrage verglichen.</li>
<li>Schreibende Abfragen lehnt der Server ab, bevor sie Neo4j erreichen.</li>
<li>Die Abfragen sehen Sie in der Klassenübersicht. Die übrigen Aufgaben fragen nach dem Modell und nach Mustern.</li>
</ul></td></tr>
</table>

#### Wo es hakt

Die Pfeilrichtung (eine Abfrage in die falsche Richtung liefert nichts, ohne Fehler) und dass gleiche Namen im Muster denselben Knoten bedeuten. Die Aufgaben »Die Pfeilrichtung« und »Das richtige Muster« zielen darauf. Bei den Paaren hilft es, die Vorhersage (sechs Zeilen für drei Personen) an der Tafel aufzulösen, bevor `a.id < b.id` erklärt wird.

#### Der kleine Graph von Hand

Vor dem Laden legen die Lernenden im Neo4j Browser vier Knoten und drei Beziehungen mit `CREATE` an (Labels, Eigenschaften, Richtung). Die Vorhersage »Zemeckis noch einmal« zeigt, dass ein zweites `CREATE` mit demselben Namen einen zweiten Knoten anlegt; danach löschen sie den doppelten mit `DETACH DELETE` und hängen den Film mit `MATCH … CREATE` an den vorhandenen. Das Kapitel »Regeln für den Graphen« greift das mit `NODE KEY` wieder auf.

#### Neu anfangen

`neo4j_bolt load` lädt nur in eine Datenbank ohne Knoten und bricht sonst mit `There are nodes in this database, exiting now.` ab. Die Seite lässt den kleinen Graphen deshalb vor dem Laden mit `MATCH (n) DETACH DELETE n;` löschen. Das löscht Knoten und Beziehungen, aber keine Schlüssel und Indizes. Ganz von vorn, auch ohne Schlüssel, beginnt man, indem man Neo4j im Profil zurücksetzt.
