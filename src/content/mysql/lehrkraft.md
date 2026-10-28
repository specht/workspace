Der Einstieg in Datenbanken: Lernende laden die Länder der Welt in ihre eigene MySQL-Datenbank und fragen sie mit SQL ab, in `mycli` im Terminal. Drei Tabellen (`country`, `city`, `continent`) reichen für `SELECT`, `WHERE`, `ORDER BY`, `LIMIT`, `COUNT`, `LIKE` und `DISTINCT`, dazu die Reihenfolge, in der die Datenbank eine Abfrage abarbeitet. Zum Schluss ändern sie die Daten mit `INSERT`, `UPDATE` und `DELETE`. Vorkenntnisse braucht es keine außer dem Terminal; [Mit Dateien arbeiten](/working-with-files) hilft. Weiter geht es mit [SQL: Gruppieren und Verbinden](/sql-joins), in derselben Datenbank.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Daten laden und erkunden, <code>SELECT</code>, <code>WHERE</code>, <code>ORDER BY</code> und <code>LIMIT</code>, jeweils mit ihren Aufgaben.</li>
<li><b>2. Doppelstunde:</b> <code>COUNT</code>, <code>LIKE</code>, <code>DISTINCT</code> mit ihren Aufgaben, dann die Reihenfolge mit dem Query Stepper.</li>
<li><b>Rest der 2. oder eine 3. Doppelstunde:</b> »Daten ändern«: Weimar und Görlitz einfügen, eine Einwohnerzahl mit <code>UPDATE</code> ändern, eine Vorhersage zu <code>UPDATE</code> ohne <code>WHERE</code>, Transaktionen, zum Schluss die eingefügten Städte mit einem <code>DELETE</code> wieder löschen. Danach hat <code>city</code> wieder dieselben 4748 Zeilen wie nach dem Import.</li>
<li>Unter jedem Abschnitt stehen erst die Aufgaben zu ihm allein, dann eine bis drei, die ihn mit dem Vorigen verbinden (»Nördlich von Hamburg« braucht <code>WHERE</code> mit <code>AND</code>, »Die zehn größten Städte Frankreichs« dazu <code>ORDER BY</code> und <code>LIMIT</code>). Wer schnell ist, löst sie gleich; die anderen holen sie nach, wenn die Bausteine sitzen.</li>
</ul></td></tr>
<tr><th>Daten</th><td><code>countries-basic.sql</code> aus <a href='https://github.com/specht/workspace-files/tree/main/country-archive'><code>specht/workspace-files</code></a>, aus Wikidata vom Oktober 2026: 197 Länder, die Städte ab 100.000 Einwohnern und die Hauptstädte. Die Lernenden laden die Datei mit <code>wget</code> und <code>mysql &lt; …</code> in ihre eigene Datenbank; jede Person hat eine, und niemand sieht die der anderen. Die Seite zeigt nach dem Import die Tabellen mit ein paar Zeilen.</td></tr>
<tr><th>Prüfung</th><td>Drei Arten:<ul>
<li>Fragt eine Aufgabe nach einem Wert (eine Zahl, ein Name, eine Auswahl), tragen die Lernenden nur das Ergebnis ein.</li>
<li>Soll eine Abfrage Zeilen ausgeben, kopieren sie die fertige Abfrage in die Karte: Der Server führt sie in ihrer eigenen Datenbank aus, angemeldet als sie, dort auch die Musterabfrage, und vergleicht die Ergebnisse, nicht den Text. <code>COUNT(name)</code> statt <code>COUNT(*)</code>, andere Spaltennamen oder eine Unterabfrage statt <code>ORDER BY … LIMIT 1</code> zählen genauso.</li>
<li>Angenommen wird nur eine einzelne Abfrage mit <code>SELECT</code> oder <code>WITH</code>. Fehlen Tabellen oder Zeilen, sagt die Karte, dass zuerst die Beispieldaten geladen werden müssen.</li>
<li>Bei den drei Aufgaben zum Ändern schreiben die Lernenden <code>INSERT</code>, <code>UPDATE</code> und <code>DELETE</code> in <code>mycli</code>; die Karte hat nur »Prüfen«. Sie liest <code>city</code>, angemeldet als die Person, und zählt Zeilen, die eine Bedingung erfüllen: Weimar mit seinen Werten, genau eine Stadt mit der neuen Einwohnerzahl (nicht alle), keine Stadt unter 100.000 Einwohnern, die keine Hauptstadt ist, und alle 197 Hauptstädte.</li>
</ul></td></tr>
<tr><th>Lösung</th><td>Jede Aufgabe zeigt nach der richtigen Antwort eine Lösung; die Musterabfragen stehen in <a href='https://github.com/specht/workspace/blob/main/src/content/mysql/tasks.yaml'><code>tasks.yaml</code></a>.</td></tr>
</table>

#### Ausprobieren, dann eintragen

Ein Kasten vor der ersten Aufgabe sagt es den Lernenden: In `mycli` entwickeln sie ihre Abfrage, sehen jedes Ergebnis und ändern sie, so oft sie wollen; in die Karte kommt erst die fertige Abfrage oder der Wert. Wer direkt in der Karte tippt, sieht keine Zeilen und rät.

#### Die Aufgaben

Die Fragen nach einem Wert verraten keine Abfrage: »Griechenland« will die Einwohnerzahl, »Mehr als 100 Millionen« eine Anzahl. Die Rückmeldungen erkennen häufige Fehler am Ergebnis, etwa aufsteigend statt absteigend sortiert oder eine fehlende Bedingung. Drei Aufgaben sind Vorhersagen, die man vor dem Weiterlesen beantwortet: die Reihenfolge der Klauseln, ein Spaltenname aus `SELECT` in `WHERE` und ein `UPDATE` ohne `WHERE`.

#### Eigene Aufgaben

Im selben Repository liegt eine sorgfältig zusammengestellte Musikdatenbank, <a href='https://github.com/specht/workspace-files/tree/main/music-archive'><code>music-archive/music.sql</code></a>: rund 3.100 Alben von gut 1.000 Künstlerinnen, Künstlern und Bands, mit Titeln, Genres und wer in welcher Band spielt, aus Discogs. Keine Aktivität benutzt sie; sie eignet sich für eigene Aufgaben, eine Klassenarbeit oder als zweiter Datensatz für dieselben Abfragen. Wie man sie lädt und was in den Spalten steht, steht im Handbuch unter [Datensätze](/reference/datensaetze).

#### Was schiefgeht

- **Doppelte Anführungszeichen und Groß- und Kleinschreibung.** Texte stehen in SQL in einfachen Anführungszeichen; `WHERE name = 'deutschland'` findet trotzdem etwas, weil MySQL beim Vergleich nicht auf Groß und Klein achtet. Das überrascht, wenn man es von Programmiersprachen anders kennt.
- **`LIKE` ohne `%`.** `name LIKE 'Ka'` sucht genau den Namen »Ka«. Erst `'Ka%'` heißt »fängt mit Ka an«.
- **Ein Name aus `SELECT` in `WHERE`.** `WHERE density > 100` scheitert, weil `WHERE` vor `SELECT` läuft; die Vorhersage »Ein Name, den es noch nicht gibt« und der Query Stepper zeigen es. In `ORDER BY` geht der Name.
- **`UPDATE` oder `DELETE` ohne `WHERE`.** `mycli` im Workspace fragt vor jedem `DELETE` und vor einem `UPDATE` ohne `WHERE` (»You're about to run a destructive command«); wer trotzdem `y` tippt, hat alle Zeilen geändert. Die Aufgabe »Weimar 2025« erkennt das, wenn mehr als eine Stadt die neue Zahl hat.
- **Eine Transaktion ohne `COMMIT`.** Wer nach `START TRANSACTION` ändert, sieht die Änderung in `mycli`, die Prüfung aber erst nach `COMMIT`; beendet die Person `mycli` vorher, ist die Änderung verworfen.
- **Daten neu laden.** Wer eine Tabelle geändert oder gelöscht hat, lädt die Datei einfach noch einmal mit `mysql < countries-basic.sql`; sie löscht die drei Tabellen und legt sie neu an. Eingefügte Städte sind danach weg; für »Weimar 2025« und »Wieder wie geladen« fügt man Weimar dann noch einmal ein.
