<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# ER-Modelle als Text

<p class='abstract'>
Wie du ein ER-Modell in einer <code>.er</code>-Datei schreibst, was die ER-Werkstatt daraus macht und wie sie Beziehungen in Tabellen übersetzt. Zum Nachschlagen; ein Feld zum Ausprobieren steht am Ende.
</p>

_person(chen)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>The Entity-Relationship Model: Toward a Unified View of Data</strong>, Peter Chen, ACM Transactions on Database Systems, 1976. Der Aufsatz, mit dem das ER-Modell bekannt wurde, mit Diagrammen aus Rechtecken für Entitäten und Rauten für Beziehungen: <a href='https://doi.org/10.1145/320434.320440'>bei der ACM</a>.</p>
</div>

## Ein Modell schreiben

<!-- part: aufbau -->
### Aufbau einer Datei

Eine `.er`-Datei besteht aus Entitäten und Beziehungen, in beliebiger Reihenfolge. Was hinter `#` steht, ist ein Kommentar. Leere Zeilen sind egal.

```er
# Filme und ihre Genres
entity movie
  id: int, primary key
  title: text

entity genre
  id: int, primary key
  name: text, unique

relationship has_genre: movie n:m genre
```

Alle Namen schreibst du klein, mit Buchstaben, Ziffern und `_`, ohne Leerzeichen und Umlaute: `german_title`, nicht `deutscher Titel`. Aus den Namen werden später Tabellen und Spalten. Die Schlüsselwörter sind Englisch: `entity`, `relationship`, `primary key`, `optional`, `unique`, `as`.
<!-- /part -->

<!-- part: attribute -->
### Entitäten und Attribute

`entity name` beginnt eine Entität. Jede eingerückte Zeile darunter ist ein Attribut: `name: typ` und danach, mit Kommas, Markierungen.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Typ</th><th>Bedeutung</th><th>In MySQL</th></tr></thead>
<tbody>
<tr><td><code>int</code></td><td>ganze Zahl</td><td><code>INT</code></td></tr>
<tr><td><code>text</code></td><td>Text bis 255 Zeichen</td><td><code>VARCHAR(255)</code></td></tr>
<tr><td><code>varchar(n)</code></td><td>Text bis <code>n</code> Zeichen</td><td><code>VARCHAR(n)</code></td></tr>
<tr><td><code>decimal(p,s)</code></td><td>Kommazahl mit <code>p</code> Stellen, davon <code>s</code> nach dem Komma, z. B. <code>decimal(6,2)</code> für Preise</td><td><code>DECIMAL(p,s)</code></td></tr>
<tr><td><code>float</code></td><td>Kommazahl, ungefähr</td><td><code>FLOAT</code></td></tr>
<tr><td><code>date</code></td><td>Datum</td><td><code>DATE</code></td></tr>
<tr><td><code>datetime</code></td><td>Datum mit Uhrzeit</td><td><code>DATETIME</code></td></tr>
<tr><td><code>bool</code></td><td>wahr oder falsch</td><td><code>BOOLEAN</code></td></tr>
</tbody>
</table>
</div>

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Markierung</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>primary key</code></td><td>gehört zum Primärschlüssel: bestimmt jeden Datensatz eindeutig. Jede Entität braucht einen. Mehrere Attribute mit <code>primary key</code> bilden ihn zusammen. Das ältere <code>key</code> funktioniert noch, die ER-Werkstatt schlägt dann <code>primary key</code> vor.</td></tr>
<tr><td><code>optional</code></td><td>darf leer bleiben (<code>NULL</code>). Ohne die Markierung muss immer ein Wert da sein (<code>NOT NULL</code>).</td></tr>
<tr><td><code>unique</code></td><td>kein Wert darf doppelt vorkommen</td></tr>
</tbody>
</table>
</div>

```er
entity book
  isbn: varchar(17), primary key
  title: text
  price: decimal(6,2), optional
  published: date
```
<!-- /part -->

<!-- part: beziehungen -->
### Beziehungen und Kardinalitäten

```er
relationship name: entity_a KARDINALITÄT entity_b
```

Die Kardinalität sagt, mit wie vielen Datensätzen der anderen Seite einer verbunden sein kann. Du liest sie in Schreibrichtung:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Kardinalität</th><th>Lies</th><th>Beispiel</th></tr></thead>
<tbody>
<tr><td><code>1:1</code></td><td>zu einem <code>a</code> gehört höchstens ein <code>b</code> und umgekehrt</td><td><code>relationship holds: person 1:1 passport</code></td></tr>
<tr><td><code>1:n</code></td><td>zu einem <code>a</code> gehören viele <code>b</code>, zu einem <code>b</code> ein <code>a</code></td><td><code>relationship writes: author 1:n book</code></td></tr>
<tr><td><code>n:1</code></td><td>wie <code>1:n</code>, nur andersherum geschrieben</td><td><code>relationship lives_in: student n:1 city</code></td></tr>
<tr><td><code>n:m</code></td><td>auf beiden Seiten viele</td><td><code>relationship takes: student n:m course</code></td></tr>
</tbody>
</table>
</div>

Eine Beziehung kann eine Entität mit sich selbst verbinden: `relationship friend: person n:m person`.

**Drei oder mehr Entitäten** zählst du mit Kommas auf. So eine Beziehung braucht immer eine eigene Tabelle:

```er
relationship credit: movie, crew, job as movie_crew
```

**Minimum und Maximum** schreibst du als `(min,max)` neben jede Entität. Das Paar sagt, wie oft **diese** Entität an der Beziehung teilnimmt: Das Minimum ist 0 oder 1, das Maximum 1 oder `n`.

```er
relationship has_copy: movie (0,n) : (1,1) copy
```

Ein Film hat beliebig viele Exemplare, auch keines: `(0,n)` bei `movie`. Ein Exemplar gehört zu genau einem Film: `(1,1)` bei `copy`. Das ist dieselbe Beziehung wie `movie 1:n copy`, nur genauer. Vorsicht: Die beiden Schreibweisen lesen sich verschieden herum. Bei `1:n` steht neben `movie` die Zahl der Filme pro Exemplar (`1`), bei `(min,max)` steht neben `movie`, wie oft ein Film teilnimmt (`(0,n)`).

**Attribute einer Beziehung** stehen eingerückt darunter, wie bei einer Entität. Sie landen in der Tabelle, die die Beziehung speichert:

```er
relationship takes: student n:m course
  grade: int, optional
```

**Eigener Tabellenname**: Mit `as name` am Ende bestimmst du, wie die Tabelle einer Beziehung heißt.
<!-- /part -->

## Was die ER-Werkstatt daraus macht

<!-- part: uebersetzung -->
### Vom Modell zu Tabellen

So übersetzt die ER-Werkstatt ein Modell. In der Vorschau steht es für dein Modell unter »So wurde übersetzt«.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Im Modell</th><th>In der Datenbank</th></tr></thead>
<tbody>
<tr><td>Entität</td><td>eine Tabelle mit demselben Namen; die Attribute mit <code>primary key</code> bilden den Primärschlüssel</td></tr>
<tr><td><code>1:n</code> oder <code>n:1</code></td><td>keine eigene Tabelle. Die Tabelle auf der n-Seite bekommt einen Fremdschlüssel auf die 1-Seite, z. B. <code>city_id</code> in <code>student</code>. Er darf nicht leer sein; mit <code>(min,max)</code> darf er es genau dann, wenn das Minimum der n-Seite 0 ist.</td></tr>
<tr><td><code>1:1</code></td><td>keine eigene Tabelle. Die zweite Entität bekommt einen Fremdschlüssel auf die erste, mit <code>UNIQUE</code></td></tr>
<tr><td><code>n:m</code></td><td>eine eigene Tabelle <code>a_b</code> (z. B. <code>movie_genre</code>) mit den Schlüsseln beider Seiten als Fremdschlüssel; zusammen sind sie der Primärschlüssel</td></tr>
<tr><td>drei oder mehr Entitäten</td><td>eine eigene Tabelle, benannt nach allen Entitäten oder nach <code>as</code>; alle Fremdschlüssel zusammen sind der Primärschlüssel</td></tr>
<tr><td>Beziehung einer Entität mit sich selbst</td><td><code>n:m</code>: eine Tabelle mit dem Namen der Beziehung und den Spalten <code>from_…_id</code> und <code>to_…_id</code>. <code>1:n</code>: ein Fremdschlüssel wie <code>mentor_person_id</code> (Name der Beziehung, dann der Tabelle), der leer sein darf</td></tr>
<tr><td>Attribut einer Beziehung</td><td>eine Spalte in der Tabelle, die die Beziehung speichert</td></tr>
</tbody>
</table>
</div>

Ein Fremdschlüssel heißt nach der Tabelle, auf die er zeigt, und deren Schlüssel: `movie_id` zeigt auf `movie.id`.
<!-- /part -->

<!-- part: diagramme -->
### Die drei Diagramme

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Ansicht</th><th>Was du siehst</th></tr></thead>
<tbody>
<tr><td>Chen</td><td>Rechteck = Entität, Raute = Beziehung, Ellipse = Attribut, unterstrichen = Schlüssel. An den Linien steht die Kardinalität, mit <code>(min,max)</code> das Paar neben seiner Entität.</td></tr>
<tr><td>Krähenfuß</td><td>Kästen mit Attributen; am Linienende ein Strich für »eins«, ein Krähenfuß für »viele«. Mit <code>(min,max)</code> steht weiter außen das Minimum: ein Kreis für 0, ein Strich für 1. Jedes Linienende zeigt, wie viele es von <strong>dieser</strong> Entität zu einem Partner gibt, also das <code>(min,max)</code> der anderen Seite. Eine n:m-Beziehung ist noch eine Linie.</td></tr>
<tr><td>Tabellen</td><td>die Tabellen, wie sie in der Datenbank entstehen: jede n:m-Beziehung ist eine eigene Tabelle. <code>PK</code> = Primärschlüssel, <code>FK</code> = Fremdschlüssel.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Normalformen

<!-- part: anomalien -->
### Anomalien

Steht dieselbe Tatsache in mehreren Zeilen, etwa das Geburtsjahr einer Regisseurin bei jedem ihrer Filme, kann eine Tabelle sich selbst widersprechen. Drei Fälle haben Namen:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Anomalie</th><th>Was passiert</th></tr></thead>
<tbody>
<tr><td>Änderungsanomalie (Update-Anomalie)</td><td>Ein <code>UPDATE</code> ändert die Tatsache nur in einem Teil der Zeilen; danach stehen zwei Werte für dasselbe in der Tabelle.</td></tr>
<tr><td>Einfügeanomalie</td><td>Etwas lässt sich nicht eintragen, weil ein anderer Teil der Zeile fehlt: eine Regisseurin ohne Film, wenn der Film zum Primärschlüssel gehört.</td></tr>
<tr><td>Löschanomalie</td><td>Ein <code>DELETE</code> löscht mehr als gemeint: Mit dem letzten Film einer Regisseurin verschwindet auch alles, was die Tabelle über sie wusste.</td></tr>
</tbody>
</table>
</div>

Normalisieren heißt, eine Tabelle so in mehrere aufzuteilen, dass jede Tatsache nur einmal gespeichert ist. Ein `JOIN` setzt sie für eine Abfrage wieder zusammen.
<!-- /part -->

<!-- part: normalformen -->
### Funktionale Abhängigkeit und die drei Normalformen

Eine Spalte B hängt **funktional** von Spalten A ab, geschrieben A → B, wenn in der Tabelle zu jedem Wert von A höchstens ein Wert von B gehört: Wer A kennt, kennt B. In `crew` gilt `id → birth_year`, aber nicht `name → birth_year`: In den Filmdaten heißen drei Personen John Morris, geboren 1926, 1969 und 1984.

Ein **Schlüssel** ist hier jede kleinste Menge von Spalten, von der alle anderen Spalten funktional abhängen; der Primärschlüssel ist einer davon. Eine **Nichtschlüsselspalte** gehört zu keinem Schlüssel.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Normalform</th><th>Verlangt</th><th>So wird die Tabelle aufgeteilt</th></tr></thead>
<tbody>
<tr><td>1. Normalform</td><td>In jeder Zelle steht ein einzelner Wert (atomar), keine Liste wie <code>Cillian Murphy, Elliot Page, …</code>.</td><td>eine Zeile pro Wert; die Spalte mit den einzelnen Werten wird Teil des Schlüssels</td></tr>
<tr><td>2. Normalform</td><td>1. Normalform, und jede Nichtschlüsselspalte hängt vom ganzen Schlüssel ab, nicht schon von einem Teil davon. Verletzt ist sie nur bei einem Schlüssel aus mehreren Spalten.</td><td>was nur von einem Teil des Schlüssels abhängt, kommt mit diesem Teil in eine eigene Tabelle</td></tr>
<tr><td>3. Normalform</td><td>2. Normalform, und keine Nichtschlüsselspalte hängt transitiv vom Schlüssel ab, also über Spalten, die selbst kein Schlüssel sind, wie in <code>movie_id → director_id → director_birth_year</code>.</td><td>die Spalte in der Mitte (<code>director_id</code>) bekommt eine eigene Tabelle mit allem, was von ihr abhängt, und bleibt als Fremdschlüssel zurück</td></tr>
</tbody>
</table>
</div>

Kurz gesagt: Jede Nichtschlüsselspalte sagt etwas über den Schlüssel, den ganzen Schlüssel und nichts als den Schlüssel. So fasste William Kent die zweite und dritte Normalform 1983 zusammen. Spalten wie `actor_1`, `actor_2`, `actor_3` sind dieselbe Liste in anderer Form; viele Bücher zählen auch sie als Verstoß gegen die erste Normalform.

Edgar F. Codd beschrieb die erste Normalform 1970, die zweite und dritte 1971. Es gibt weitere, strengere Normalformen (Boyce-Codd, vierte, fünfte); für die meisten Tabellen ist die dritte das Ziel.
<!-- /part -->

<!-- part: ausprobieren -->
## Ausprobieren

Schreib links ein Modell; rechts erscheint es als Diagramm, darunter die Tabellen und das SQL. Hier wird nichts gespeichert. Für deine eigenen Modelle nimmst du eine `.er`-Datei in VS Code.

<div class='er-playground'>
<textarea class='form-control er-playground-input' spellcheck='false' autocapitalize='off' aria-label='ER-Modell'>entity student
  id: int, primary key
  name: text

entity course
  id: int, primary key
  title: text, unique

relationship takes: student n:m course
  grade: int, optional</textarea>
<div class='er-playground-output'>
<div class='btn-group btn-group-sm er-playground-kinds' role='group' aria-label='Darstellung'>
<button type='button' class='btn btn-outline-secondary active' data-kind='chen'>Chen</button>
<button type='button' class='btn btn-outline-secondary' data-kind='crowfoot'>Krähenfuß</button>
<button type='button' class='btn btn-outline-secondary' data-kind='tables'>Tabellen</button>
</div>
<div class='er-playground-problems' aria-live='polite'></div>
<div class='er-playground-diagram'></div>
</div>
<details class='er-playground-details'><summary>SQL</summary><pre class='er-playground-sql'></pre></details>
<details class='er-playground-details'><summary>So wurde übersetzt</summary><ol class='er-playground-steps'></ol></details>
</div>

<script src='er-core.js'></script>
<script>
window.addEventListener('DOMContentLoaded', function () {
    var core = window.ErCore;
    var box = document.querySelector('.er-playground');
    if (!core || !box) return;
    var input = box.querySelector('.er-playground-input');
    var diagram = box.querySelector('.er-playground-diagram');
    var problems = box.querySelector('.er-playground-problems');
    var sql = box.querySelector('.er-playground-sql');
    var steps = box.querySelector('.er-playground-steps');
    var kind = 'chen';
    var esc = core.esc;
    function code(text) { return esc(text).replace(/`([^`]+)`/g, '<code>$1</code>'); }
    function draw() {
        var text = input.value;
        var compiled = core.compile(text);
        var errors = compiled.model.errors;
        problems.innerHTML = errors.length ? errors.slice().sort(function (a, b) { return a.line - b.line; }).map(function (p) {
            return '<div>Zeile ' + p.line + ': ' + esc(p.msg) + '</div>';
        }).join('') : '';
        if (errors.length) return; // the last good diagram stays
        diagram.innerHTML = core.diagram(text, kind, { background: null }).svg;
        sql.textContent = core.toSQL(compiled.result.tables, { source: 'beispiel.er' });
        steps.innerHTML = compiled.result.steps.map(function (s) { return '<li>' + code(s.text) + '</li>'; }).join('');
    }
    box.querySelectorAll('[data-kind]').forEach(function (button) {
        button.addEventListener('click', function () {
            kind = button.dataset.kind;
            box.querySelectorAll('[data-kind]').forEach(function (b) { b.classList.toggle('active', b === button); });
            draw();
        });
    });
    input.addEventListener('input', draw);
    draw();
});
</script>
<!-- /part -->

## Werkzeuge

<!-- part: werkstatt -->
### Die ER-Werkstatt in VS Code

Eine Datei mit der Endung `.er` öffnet VS Code mit der ER-Werkstatt.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Was</th><th>Wie</th></tr></thead>
<tbody>
<tr><td>Fehler</td><td>rot unterstrichen, mit Erklärung unter der Maus; alle im Bereich »Probleme« (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>M</kbd>)</td></tr>
<tr><td>Diagramm anzeigen</td><td>»ER-Diagramm anzeigen« oben rechts im Editor, oder <kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>V</kbd></td></tr>
<tr><td>Kästen verschieben</td><td>mit der Maus; die Positionen stehen in der versteckten Datei <code>.name.er.layout.json</code> neben dem Modell. »Neu anordnen« ordnet alles wieder automatisch an.</td></tr>
<tr><td>SQL erzeugen</td><td>»SQL erzeugen« schreibt <code>name.sql</code> neben das Modell. Eine Datei, die nicht von der ER-Werkstatt stammt, wird nur nach Rückfrage überschrieben.</td></tr>
<tr><td>Tabellen anlegen</td><td>im Terminal: <code>mysql &lt; name.sql</code>. Die Datei beginnt mit <code>DROP TABLE IF EXISTS</code>: Vorhandene Tabellen mit diesen Namen werden samt Inhalt gelöscht und neu angelegt.</td></tr>
<tr><td>Diagramm sichern</td><td>»Als SVG speichern« schreibt <code>name-chen.svg</code>, <code>name-crowfoot.svg</code> oder <code>name-tables.svg</code></td></tr>
<tr><td>Schema als Text</td><td>Unter dem Diagramm steht eine Zeile pro Entitätstyp (bei Chen und Krähenfuß) oder pro Tabelle (bei Tabellen), der Schlüssel unterstrichen, ein Fremdschlüssel mit ↑. »Kopieren« legt sie als Text in die Zwischenablage, zum Einfügen in ein Arbeitsblatt.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: kommandozeile -->
### Auf der Kommandozeile

Die ER-Werkstatt ist eine JavaScript-Datei, die auch ohne VS Code läuft, zum Beispiel für Skripte. Im Workspace liegt sie im Ordner der Erweiterung; die Quelle ist `src/vscode-extensions/er/core/er-core.js` im Repository des Workspaces.

Merke dir den Pfad zuerst in einer Variablen:

```bash_copy
ER=$(ls /workspace/.extensions/hackschule.hackschule-er-*/core/er-core.js | tail -1)
```

Dann erzeugt `node $ER` dasselbe wie die Befehle in VS Code:

```bash
node $ER --sql film.er > film.sql
node $ER --svg tables film.er > film-tables.svg
node $ER --schema tables film.er      # eine Zeile pro Tabelle; entities: pro Entitätstyp
node $ER --json film.er
```
<!-- /part -->

## Zum Nachschlagen

Die Schreibweise der `.er`-Dateien gehört zur ER-Werkstatt; eine Referenz außerhalb dieser Seite gibt es dafür nicht. Für das SQL, das sie erzeugt:

- [CREATE TABLE](https://dev.mysql.com/doc/refman/8.4/en/create-table.html) im Handbuch von MySQL 8.4, auf Englisch: alles, was in einer Tabellendefinition stehen kann, auch `PRIMARY KEY` und `UNIQUE`.
- [FOREIGN KEY Constraints](https://dev.mysql.com/doc/refman/8.4/en/create-table-foreign-keys.html): wie MySQL Fremdschlüssel prüft und was beim Löschen einer Zeile geschieht, auf die einer zeigt.
- [A Simple Guide to Five Normal Forms in Relational Database Theory](https://www.bkent.net/Doc/simple5.htm), William Kent, 1983, auf Englisch: die Normalformen bis zur fünften, kurz und mit kleinen Beispielen.
- Abfragen auf die fertigen Tabellen stehen bei [SQL](/reference/sql), die Datensätze der Aktivitäten bei [Datensätze](/reference/datensaetze).
