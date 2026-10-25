Lernende entwerfen eine Datenbank für Filme und ihre Mitwirkenden: ein ER-Modell, daraus Tabellen in MySQL, die echten Daten laden und mit SQL abfragen. Die Filmdatenbank löst die alte »Videothek« ab. Statt eines Diagramm-Editors mit Maus schreiben die Lernenden ihr ER-Modell als Text; die ER-Werkstatt (VS Code-Erweiterung im Workspace) zeichnet es als Chen-, Krähenfuß- und Tabellendiagramm und erzeugt das SQL. Ruby, ein Import-Skript und ein Programm mit Menüs braucht es nicht mehr.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> vom Datensatz zum Modell, mit der ER-Werkstatt (<code>film.er</code>).</li>
<li><b>2. Doppelstunde:</b> vom Modell zur Datenbank (Tabellen anlegen, Daten laden) und der erste <code>JOIN</code>.</li>
<li><b>3. Doppelstunde:</b> Menschen und Tätigkeiten, die Beziehung mit einer Rolle, und die Zusammenfassung.</li>
</ul></td></tr>
<tr><th>Ordner</th><td><code>/workspace/filmdatenbank</code>, von den Lernenden selbst angelegt; darin <code>film.er</code>, das erzeugte <code>film.sql</code> und die geladenen Datendateien</td></tr>
<tr><th>Daten</th><td><code>movie-archive/movies-data-films.sql</code> und <code>movies-data-crew.sql</code> aus <code>specht/workspace-files</code>, gebaut mit <code>src/scripts/collect-movies</code>. Die Inhalte stammen aus Wikidata (CC0); die Auswahl der Filme steht in der <code>README.md</code> daneben</td></tr>
<tr><th>Was gelesen wird</th><td>Aufgaben »Filme und Genres« und »Menschen und Tätigkeiten«: <code>film.er</code>, gelesen mit dem Kern der ER-Werkstatt im laufenden Workspace. Alle anderen: die eigene MySQL-Datenbank, angemeldet als die Person, nur lesend</td></tr>
<tr><th>Was nie passiert</th><td>Die Prüfungen legen keine Tabellen an und laden keine Daten. Das tun die Lernenden selbst im Terminal.</td></tr>
</table>

#### Feste Namen

Die Datendateien schreiben mit `INSERT INTO movie (id, title, …)`. Deshalb verlangen die Aufgaben genau die Namen aus dem Text (`movie`, `genre`, `crew`, `job`, `movie_genre`, `movie_crew` und ihre Spalten). Wer anders benennt, bekommt beim Laden `Unknown column` und von der Karte einen Satz dazu, welche Spalte fehlt. Eigene Modelle (siehe letzte Aufgabe) sind frei.

#### Neu anlegen statt erweitern

`film.sql` beginnt mit `DROP TABLE IF EXISTS`. Nach der Erweiterung um Personen müssen deshalb beide Datendateien noch einmal geladen werden; »Alles neu angelegt und geladen« meldet eine leere Tabelle mit genau diesem Hinweis. Der Unterschied zu einer echten Datenbank (`ALTER TABLE`) steht als Hinweis im Text und eignet sich für ein Unterrichtsgespräch.

#### Wo es hakt

Die Kardinalität (`n:m`, und warum aus `n:1` eine Spalte statt einer Tabelle wird) und der zusammengesetzte Primärschlüssel. Die Vorhersage-Aufgabe »Was wäre bei n:1?« ist dafür da; es lohnt, sie gemeinsam aufzulösen. Bei den Abfragen ist es das fehlende `ON` und `COUNT` in `WHERE` statt `HAVING`; beides erkennt die Karte.

#### Tipps in Stufen

»Viel Regie« hat zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Zum Weiterarbeiten

Die letzte Aufgabe sammelt Ideen für Erweiterungen. Eine Videothek mit Kundinnen, Exemplaren und Ausleihen (mit Datum) ist ein gutes eigenes Modell: Es braucht eine 1:n-Beziehung und Attribute an einer Beziehung. Die Diagramme lassen sich mit »Als SVG speichern« für Präsentationen sichern.
