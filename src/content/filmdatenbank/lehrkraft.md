Lernende entwerfen eine Datenbank für Filme und ihre Mitwirkenden: ein ER-Modell, daraus Tabellen in MySQL, die echten Daten laden und mit SQL abfragen. Die Filmdatenbank löst die alte »Videothek« ab. Statt eines Diagramm-Editors mit Maus schreiben die Lernenden ihr ER-Modell als Text; die ER-Werkstatt (VS Code-Erweiterung im Workspace) zeichnet es als Chen-, Krähenfuß- und Tabellendiagramm und erzeugt das SQL. Ruby, ein Import-Skript und ein Programm mit Menüs braucht es nicht mehr.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> vom Datensatz zum Modell, mit der ER-Werkstatt (<code>film.er</code>).</li>
<li><b>2. Doppelstunde:</b> vom Modell zur Datenbank (Tabellen anlegen, Daten laden) und der erste <code>JOIN</code>.</li>
<li><b>3. Doppelstunde:</b> Menschen und Tätigkeiten, die Beziehung mit einer Rolle.</li>
<li><b>4. Doppelstunde:</b> eine Videothek. Die Lernenden schreiben das Modell selbst (eine 1:n-Beziehung, eine n:m-Beziehung mit Attributen, deren Datum zum Schlüssel gehört), dann <code>NULL</code> und <code>IS NULL</code>, <code>LEFT JOIN</code> und die Zusammenfassung.</li>
<li><b>Extra, für den Leistungskurs etwa eine Doppelstunde:</b> Normalisierung. Eine breite Tabelle eines Filmclubs aus den eigenen Daten, die drei Anomalien, dann Schritt für Schritt erste, zweite und dritte Normalform bis zu den Tabellen der Aktivität. Der Grundkurs kann das Kapitel auslassen.</li>
</ul></td></tr>
<tr><th>Ordner</th><td><code>/workspace/filmdatenbank</code>, von den Lernenden selbst angelegt; darin <code>film.er</code>, das erzeugte <code>film.sql</code> und die geladenen Datendateien</td></tr>
<tr><th>Daten</th><td><code>movie-archive/movies-data-films.sql</code>, <code>movies-data-crew.sql</code> und <code>movies-data-rental.sql</code> aus <code>specht/workspace-files</code>, gebaut mit <code>src/scripts/collect-movies</code>. Die Filme und Personen stammen aus Wikidata (CC0); die Auswahl der Filme steht in der <code>README.md</code> daneben. Die Videothek (Exemplare, Kundschaft, Ausleihen) ist mit festem Startwert ausgewürfelt (<code>rental.rb</code>); die Namen sind erfunden</td></tr>
<tr><th>Was gelesen wird</th><td>Aufgaben »Filme und Genres«, »Menschen und Tätigkeiten« und »Exemplare und Ausleihen«: <code>film.er</code>, gelesen mit dem Kern der ER-Werkstatt im laufenden Workspace. Alle anderen: die eigene MySQL-Datenbank, angemeldet als die Person, nur lesend</td></tr>
<tr><th>Normalisierung</th><td>Die Lernenden legen in ihrer eigenen Datenbank vier Tabellen an (<code>club_list</code>, <code>club_cast</code>, <code>club_movie</code>, <code>club_director</code>), mit <code>CREATE TABLE … AS SELECT</code> aus <code>movie</code>, <code>crew</code>, <code>job</code> und <code>movie_crew</code>. Sie stören <code>film.sql</code> und die anderen Aufgaben nicht; am Ende des Kapitels steht, wie man sie mit einem <code>DROP TABLE</code> wieder löscht</td></tr>
<tr><th>Was nie passiert</th><td>Die Prüfungen legen keine Tabellen an und laden keine Daten. Das tun die Lernenden selbst im Terminal.</td></tr>
</table>

#### Feste Namen

Die Datendateien schreiben mit `INSERT INTO movie (id, title, …)`. Deshalb verlangen die Aufgaben genau die Namen aus dem Text (`movie`, `genre`, `crew`, `job`, `movie_genre`, `movie_crew` und ihre Spalten). Wer anders benennt, bekommt beim Laden `Unknown column` und von der Karte einen Satz dazu, welche Spalte fehlt. Eigene Modelle (siehe letzte Aufgabe) sind frei.

#### Neu anlegen statt erweitern

`film.sql` beginnt mit `DROP TABLE IF EXISTS`. Nach der Erweiterung um Personen müssen deshalb beide Datendateien noch einmal geladen werden; »Alles neu angelegt und geladen« meldet eine leere Tabelle mit genau diesem Hinweis. Der Unterschied zu einer echten Datenbank (`ALTER TABLE`) steht als Hinweis im Text und eignet sich für ein Unterrichtsgespräch.

#### Wo es hakt

Die Kardinalität (`n:m`, und warum aus `n:1` eine Spalte statt einer Tabelle wird) und der zusammengesetzte Primärschlüssel. Die Vorhersage-Aufgabe »Was wäre bei n:1?« ist dafür da; es lohnt, sie gemeinsam aufzulösen. Bei den Abfragen ist es das fehlende `ON` und `COUNT` in `WHERE` statt `HAVING`; beides erkennt die Karte.

#### Was die Schlüssel verhindern

Nach dem Laden der Daten versuchen die Lernenden drei falsche `INSERT` (eine doppelte Zuordnung, ein Genre und ein Film, die es nicht gibt) und sagen dann vorher, was `DELETE FROM genre WHERE name = 'Sci-Fi'` tut. Danach ändern sie den Fremdschlüssel auf `movie_id` mit `ALTER TABLE … DROP FOREIGN KEY movie_genre_ibfk_1, ADD FOREIGN KEY … ON DELETE CASCADE` und löschen einen Film in einer Transaktion. Der Name `movie_genre_ibfk_1` ist der, den MySQL für das `film.sql` der ER-Werkstatt vergibt; er steht auch in der Fehlermeldung des falschen `INSERT`. Das `CASCADE` verschwindet wieder, wenn `film.sql` in »Menschen und Tätigkeiten« die Tabellen neu anlegt. Wer das `ROLLBACK` vergisst, hat »Back to the Future« gelöscht; dann helfen `mysql < film.sql` und das erneute Laden der Filmdaten.

#### Die Videothek

Anders als bei Genres und Personen steht das Modell der Videothek nicht vor der Aufgabe auf der Seite: Die Lernenden bekommen die Namen der Tabellen und Spalten (die Datendatei braucht sie) und entscheiden Kardinalitäten, Schlüssel und `optional` selbst. Wo es hakt: `loan_date` gehört zum Schlüssel, sonst könnte dieselbe Person ein Exemplar nur einmal ausleihen; die Karte fragt danach, ohne die Lösung zu sagen. Fehlt `optional` bei `return_date`, meldet `mysql` beim Laden `Column 'return_date' cannot be null`, und die Karte »Die Videothek angelegt und geladen« nennt diesen Grund. Die Vorhersage »Gleich NULL?« lohnt sich gemeinsam aufzulösen: Die meisten erwarten die Zahl der ausgeliehenen Exemplare.

#### Tipps in Stufen

»Viel Regie«, »Exemplare und Ausleihen«, »Was gerade ausgeliehen ist« und »Nie ausgeliehen« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Normalisierung

- Das Kapitel ist ein Extra: Seine vier Aufgaben zählen bei niemandem mit, die Seite ist ohne es vollständig. Die Begriffe (funktionale Abhängigkeit, Schlüssel, Nichtschlüsselspalte, erste bis dritte Normalform) stehen einmal im Handbuch unter »ER-Modelle als Text«, Abschnitt »Normalformen«, und die Seite bindet diesen Teil ein.
- Die Anomalien probieren die Lernenden in Transaktionen aus (`START TRANSACTION`, `ROLLBACK`), wie im Kapitel »Daten ändern« der »Einführung in SQL«. Wer das `ROLLBACK` vergisst, hat Nolan mit zwei Geburtsjahren in der Tabelle; das fällt erst bei der dritten Normalform auf (`Duplicate entry '5048'`), und der Kasten »Jetzt bist du dran« sagt, wie man es repariert.
- Die Aufteilung schreiben die Lernenden mit `CREATE TABLE … AS SELECT DISTINCT` und `ALTER TABLE … DROP COLUMN … ADD FOREIGN KEY`; die zweite Normalform macht die Seite vor, die dritte machen sie selbst. Die Karte »Die Regie in eine eigene Tabelle« prüft Spalten, Schlüssel und Fremdschlüssel und dass `director` und `director_birth_year` aus `club_movie` verschwunden sind. `mycli` fragt vor jedem `ALTER TABLE` und `DROP TABLE` nach; man antwortet mit `y`.
- Für ein Unterrichtsgespräch: Die dritte Normalform ist nicht alles, was ein guter Entwurf braucht. Die Tabellen der Aktivität gehen weiter als die des Clubs (Personen mit `id` statt Namen, mehrere Regisseure pro Film über `movie_crew`), und das ER-Modell hat sie ohne einen eigenen Normalisierungsschritt dorthin gebracht.

#### Eigene Projekte

Die letzte Aufgabe sammelt Ideen für Erweiterungen. Die Ideen hier führen sie zu einem eigenen Modell; jede beginnt wieder beim Diagramm, und bewertet wird am Modell, an den Tabellen und an Abfragen, die eine echte Frage beantworten. Die Diagramme lassen sich mit »Als SVG speichern« für eine Präsentation sichern.

- **Ein Kino** (eine Doppelstunde): Säle, Vorstellungen und Tickets. Ein Film hat viele Vorstellungen, ein Saal auch, und zu einer Vorstellung gehören viele Tickets; die Frage »Wie viele Plätze sind noch frei?« braucht `COUNT` und ein `LEFT JOIN` für Vorstellungen ohne verkauftes Ticket.
- **Eine Datenbank für die Schule** (ein Projekt): die Schulbibliothek, die AGs mit ihren Terminen, die Ausleihe von Tablets. Erst das Modell, dann die Tabellen, dann die Normalformen wie in [»Zurück zu deinen Tabellen«](#zurück-zu-deinen-tabellen); erfundene Daten statt echter Namen.
- **Ein Filmquiz aus Abfragen** (eine Doppelstunde): Die Lernenden denken sich Fragen an die Filmdaten aus, »Wer hat mit den meisten anderen gespielt?«, »In welchem Jahrzehnt gab es die meisten Krimis?«, und beantworten sie mit JOIN und [»Zählen pro Gruppe«](#zählen-pro-gruppe). Die Klasse rät zuerst, dann zeigt die Abfrage die Antwort.
