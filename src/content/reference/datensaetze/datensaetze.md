<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Datensätze

<p class='abstract'>
Die Aktivitäten zu Datenbanken arbeiten mit drei Datensätzen aus offenen Quellen: den Ländern der Welt, Filmen und Musik. Hier steht für jeden, welche Tabellen und Spalten er hat, woher die Daten kommen, was drin ist und was nicht, und wie du ihn in deine Datenbank lädst.
</p>

Alle drei liegen im Repository [specht/workspace-files](https://github.com/specht/workspace-files), jeder in einem eigenen Ordner mit denselben Dateien, benannt nach dem Datensatz, etwa für die Länder: `countries.sql` mit Tabellen und Daten, `countries-schema.sql` nur mit den Tabellen, `countries-data.sql` nur mit den Daten (für Tabellen, die du selbst angelegt hast), dazu Teile der Daten zum schrittweisen Laden, `countries-neo4j.dump` für Neo4j und eine `README.md`. Gebaut werden sie von Skripten im Repository des Workspaces (`src/scripts/collect-…`), die man jederzeit neu laufen lassen kann; so bleiben die Zahlen aktuell.

<!-- part: schema -->
## Wie die Tabellen hier stehen

Jede Tabelle steht in einer Zeile, ihre Spalten in Klammern:

_schema(movies)

Unterstrichen ist der **Primärschlüssel**, die Spalte oder die Spalten, deren Werte jede Zeile eindeutig machen. Mit ↑ beginnt ein **Fremdschlüssel**, eine Spalte, die auf eine Zeile einer anderen Tabelle verweist; worauf genau, zeigt ein Tooltip, wenn du mit der Maus darauf zeigst. In `movie_genre` sind beide Spalten zugleich Fremdschlüssel und zusammen der Primärschlüssel: Jedes Paar aus Film und Genre kommt nur einmal vor.
<!-- /part -->

<!-- part: countries -->
## Länder der Welt

Alle 197 Länder der Welt mit ihren Kontinenten, gut 4.700 Städte und die Grenzen zwischen den Ländern. Damit beginnt die Einführung in SQL; die Länder allein reichen für `SELECT`, `WHERE` und `ORDER BY`, mit den Städten und Grenzen kommen `JOIN` und `GROUP BY` dazu.

_schema(countries)

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead><tr><th>Spalte</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>continent.name</code>, <code>area</code></td><td>einer der sieben Kontinente auf Deutsch, von <code>Afrika</code> bis <code>Südamerika</code>, und seine Fläche in km²</td></tr>
<tr><td><code>country.code</code></td><td>das Kürzel aus drei Buchstaben nach ISO 3166 (<code>DEU</code>, <code>FRA</code>); Kosovo hat keines und bekommt <code>XKS</code></td></tr>
<tr><td><code>country.population</code>, <code>area</code></td><td>die Einwohnerzahl, die letzte, die Wikidata kennt, und die Fläche in km²</td></tr>
<tr><td><code>country.continent</code></td><td>der Kontinent; ein Land auf zwei Kontinenten zählt dort, wo seine Hauptstadt liegt: Russland in Europa, die Türkei in Asien, Ägypten in Afrika</td></tr>
<tr><td><code>city.id</code></td><td>eine Nummer, nach Land und Name vergeben; Namen gibt es mehrfach (Springfield dreimal in den Vereinigten Staaten)</td></tr>
<tr><td><code>city.name</code></td><td>der deutsche Name, wo es einen gibt (<code>München</code>, <code>Mailand</code>), sonst der englische</td></tr>
<tr><td><code>city.population</code></td><td>die Einwohner innerhalb der Stadtgrenzen, nicht des Ballungsraums; wo die Grenzen viel Land einschließen, wird die Zahl groß: Chongqing hat 30 Millionen</td></tr>
<tr><td><code>city.latitude</code>, <code>longitude</code></td><td>Breite und Länge in Grad; Norden und Osten positiv, Süden und Westen negativ</td></tr>
<tr><td><code>city.is_capital</code></td><td><code>1</code> für die Hauptstadt, sonst <code>0</code>; jedes Land hat genau eine</td></tr>
<tr><td><code>border</code></td><td>eine Landgrenze, in beide Richtungen: <code>('DEU', 'FRA')</code> und <code>('FRA', 'DEU')</code></td></tr>
</tbody>
</table>
</div>

Jede Grenze steht also zweimal da, mit Absicht. Einmal würde reichen, die Tabelle wäre dann frei von Wiederholungen, aber jede Frage nach den Nachbarn eines Landes müsste in beiden Spalten suchen. So genügt ein einfacher `JOIN`. Der Preis: Die Datenbank kann nicht verhindern, dass jemand nur eine Richtung einträgt und die beiden Hälften einander widersprechen.

**Was drin ist.** Die 193 Mitglieder der Vereinten Nationen, die beiden Beobachterstaaten Vatikanstadt und Palästina, Kosovo und Taiwan. Dänemark und die Niederlande ohne Grönland, die Färöer und die Karibikinseln. Als Stadt zählt jeder Ort ab 100.000 Einwohnern, dazu jede Hauptstadt, auch die kleinste: Ngerulmud, der Regierungssitz von Palau, hat gar keine Einwohner; Stadtteile, Ballungsräume und Landkreise zählen nicht. Die Zahlen stammen nicht alle aus demselben Jahr. Grenzen sind nur die an Land: 39 Länder, meist Inseln, haben keine Nachbarn. Überseegebiete zählen mit, so grenzt Frankreich über Französisch-Guayana an Brasilien. Eine Stadt, die Wikidata zwei Ländern zuordnet, gehört zu dem, dessen Hauptstadt näher liegt: Donezk und die Krim zur Ukraine, Prizren zum Kosovo.

**Woher, und darf ich das?** Alles stammt aus [Wikidata](https://www.wikidata.org/) und steht unter [CC0](https://creativecommons.org/publicdomain/zero/1.0/). Wikidata schreiben viele Menschen; eine falsche Zahl verbessert man am besten dort, beim nächsten Bau ist sie dann richtig.

**Laden.**

```bash_copy
wget https://github.com/specht/workspace-files/raw/main/country-archive/countries.sql
```

```bash
mysql < countries.sql
```

Die Nachbarn eines Landes holt ein `JOIN` von `border` mit `country`:

```sql
SELECT neighbor.name
FROM border
JOIN country AS neighbor ON neighbor.code = border.neighbor_code
WHERE border.country_code = 'DEU';
```

In Neo4j: Knoten `Continent`, `Country` und `City`, `(Country)-[:ON]->(Continent)`, `(City)-[:IN]->(Country)`, `(Country)-[:CAPITAL]->(City)` und `(Country)-[:BORDERS]->(Country)`, einmal pro Paar.

Benutzt in: [Einführung in SQL](/mysql), in der [SQL-Zeitlupe](/query-stepper) (alle Tabellen ganz) und in den Beispielen des Handbuchs zu [SQL](/reference/sql).

<!-- /part -->

<!-- part: movies -->
## Filme

Rund 2.700 bekannte Spielfilme, ihre Genres und die Menschen, die an ihnen mitgearbeitet haben. Die Filmdatenbank baut diese Tabellen erst selbst aus einem ER-Modell und lädt dann die Daten hinein.

_schema(movies)

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead><tr><th>Spalte</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>movie.title</code></td><td>der englische Titel; <code>german_title</code> der deutsche, <code>original_title</code> der Originaltitel, beide nur, wenn sie anders lauten</td></tr>
<tr><td><code>movie.year</code>, <code>runtime</code></td><td>das Jahr der ersten Veröffentlichung, die Länge in Minuten</td></tr>
<tr><td><code>genre.name</code></td><td>eines von 20 Genres auf Englisch, etwa <code>Sci-Fi</code> oder <code>Animation</code>; ein Film kann mehrere haben oder keines</td></tr>
<tr><td><code>crew.name</code>, <code>birth_year</code>, <code>death_year</code></td><td>eine Person, ihr Geburts- und Todesjahr; beide können fehlen</td></tr>
<tr><td><code>job.title</code></td><td>eine Tätigkeit: <code>actor</code>, <code>director</code>, <code>writer</code>, <code>composer</code>, <code>producer</code>, <code>cinematographer</code> oder <code>editor</code></td></tr>
<tr><td><code>movie_crew</code></td><td>wer bei welchem Film was gemacht hat; eine Person steht pro Film und Tätigkeit einmal da</td></tr>
</tbody>
</table>
</div>

**Was drin ist.** Spielfilme, die in IMDb mindestens 100.000 Stimmen haben; nur ihre IMDb-Nummer wird dafür benutzt, alles andere kommt aus [Wikidata](https://www.wikidata.org/). Von den Menschen stehen pro Film die zehn bekanntesten Schauspielerinnen und Schauspieler da (gemessen an der Zahl ihrer Wikipedia-Artikel) und wer Regie geführt, geschrieben, komponiert, produziert, die Kamera geführt oder geschnitten hat. Titel sind nicht eindeutig: Einen Film erkennst du an seiner `id` oder an Titel und Jahr.

**Woher, und darf ich das?** Wikidata steht unter [CC0](https://creativecommons.org/publicdomain/zero/1.0/): Die Daten dürfen ohne Nachfrage kopiert, verändert und weitergegeben werden.

**Laden.** Die Filmdatenbank lädt die Daten in zwei Schritten, wie sie das Modell wachsen lässt:

```bash_copy
wget https://github.com/specht/workspace-files/raw/main/movie-archive/movies-data-films.sql
wget https://github.com/specht/workspace-files/raw/main/movie-archive/movies-data-crew.sql
```

Alles auf einmal, mit den Tabellen: `movie-archive/movies.sql`. In Neo4j: Knoten `Movie`, `Person` und `Genre`, `(Movie)-[:IN_GENRE]->(Genre)` und `(Person)-[:ACTOR|DIRECTOR|…]->(Movie)`.

Benutzt in: [Filmdatenbank mit MySQL](/filmdatenbank), [Filmdatenbank mit Neo4j](/neo4j-filmdatenbank) und in der [SQL-Zeitlupe](/query-stepper) (dort nur Regie und Schauspiel, Schauspielerinnen und Schauspieler ab fünf Filmen).
<!-- /part -->

<!-- part: music -->
## Musik

Rund 3.100 Alben von gut 1.000 Künstlerinnen, Künstlern und Bands, mit ihren Titeln, Genres und wer in welcher Band spielt.

_schema(music)

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead><tr><th>Spalte</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>artist.born</code>, <code>died</code></td><td>Geburts- und Todesjahr, aus dem Profil bei Discogs gelesen; leer bei Bands und wo das Profil nichts sagt</td></tr>
<tr><td><code>album.id</code></td><td>die Nummer des Albums bei Discogs (<em>master release</em>); <code>discogs_release_id</code> die Ausgabe, aus der Land und Titel stammen</td></tr>
<tr><td><code>album.year</code>, <code>country</code></td><td>das Jahr des Albums und wo diese Ausgabe erschienen ist</td></tr>
<tr><td><code>album.versions</code></td><td>wie viele Ausgaben Discogs von dem Album kennt; gut zum Sortieren nach Bekanntheit</td></tr>
<tr><td><code>track.number</code>, <code>duration</code></td><td>die Nummer auf dem Album, ab 1, und die Länge in Sekunden; die Länge kann fehlen</td></tr>
<tr><td><code>album_genre</code></td><td>ein Album kann mehrere Genres haben</td></tr>
<tr><td><code>artist_part_of</code></td><td>wer (<code>member_id</code>) in welcher Band (<code>band_id</code>) spielt oder gespielt hat; beide sind Zeilen von <code>artist</code></td></tr>
</tbody>
</table>
</div>

**Was drin ist.** Alben von einer festen Liste von Künstlerinnen, Künstlern und Bands, jeweils die übliche Ausgabe; keine Sampler, inoffiziellen Ausgaben, Neuauflagen oder Live-Mitschnitte von Tourneen. Ein Album braucht ein Jahr, ein Genre und eine Titelliste.

**Woher, und darf ich das?** Aus den monatlichen Datenexporten von [Discogs](https://data.discogs.com/), die unter [CC0](https://creativecommons.org/publicdomain/zero/1.0/) stehen.

**Laden.**

```bash_copy
wget https://github.com/specht/workspace-files/raw/main/music-archive/music.sql
```

```bash
mysql < music.sql
```

In Neo4j: Knoten `Artist`, `Album`, `Track` und `Genre`, `(Artist)-[:RELEASED]->(Album)`, `(Album)-[:HAS_TRACK]->(Track)`, `(Album)-[:IN_GENRE]->(Genre)` und `(Artist)-[:MEMBER_OF]->(Artist)`.

Noch benutzt ihn keine Aktivität; er eignet sich für eigene Aufgaben zu `JOIN` und `GROUP BY` oder als zweiter Datensatz für dieselben Abfragen wie bei den Filmen.
<!-- /part -->
