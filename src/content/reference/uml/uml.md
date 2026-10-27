<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Klassendiagramme als Text

<p class='abstract'>
Wie du ein UML-Klassendiagramm in einer <code>.uml</code>-Datei schreibst, wie du das Diagramm liest und was die UML-Werkstatt damit macht. Zum Nachschlagen; ein Feld zum Ausprobieren steht gleich darunter.
</p>

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>UML Distilled</strong>, Martin Fowler, 3. Auflage, Addison-Wesley, 2003. Ein dünnes Buch über die Teile der UML, die man wirklich braucht, mit einem langen Kapitel zu Klassendiagrammen; auf Deutsch als »UML konzentriert«.</p>
</div>

<!-- part: ausprobieren -->
## Ausprobieren

Schreib ein Diagramm ins Feld; daneben oder darunter erscheint es, während du tippst. Hier wird nichts gespeichert. Für deine eigenen Diagramme nimmst du eine `.uml`-Datei in VS Code.

<div class='uml-playground'>
<textarea class='form-control uml-playground-input' wrap='off' spellcheck='false' autocapitalize='off' aria-label='Klassendiagramm'># Musik abspielen
class Track (abstract)
  - title: String
  - seconds: int
  + Track(title: String, seconds: int)
  + label(): String

class Song extends Track
  - artist: String
  + Song(title: String, seconds: int, artist: String)
  + label(): String

class Episode extends Track
  - show: String
  + Episode(title: String, seconds: int, show: String)
  + label(): String

class Playlist
  - name: String
  + Playlist(name: String)
  + add(track: Track): void
  + totalSeconds(): int

association tracks: Playlist 0..* -- 0..* Track</textarea>
<div class='uml-playground-output'>
<div class='uml-playground-problems' aria-live='polite'></div>
<div class='uml-playground-diagram'></div>
</div>
</div>

<script src='uml-core.js'></script>
<script>
window.addEventListener('DOMContentLoaded', function () {
    var core = window.UmlCore;
    var box = document.querySelector('.uml-playground');
    if (!core || !box) return;
    var input = box.querySelector('.uml-playground-input');
    var diagram = box.querySelector('.uml-playground-diagram');
    var problems = box.querySelector('.uml-playground-problems');
    var esc = core.esc;
    function code(text) { return esc(text).replace(/`([^`]+)`/g, '<code>$1</code>'); }
    function draw() {
        var text = input.value;
        var model = core.parse(text);
        var errors = model.errors;
        problems.innerHTML = errors.length ? errors.slice().sort(function (a, b) { return a.line - b.line; }).map(function (p) {
            return '<div>Zeile ' + p.line + ': ' + code(p.msg) + '</div>';
        }).join('') : '';
        diagram.classList.toggle('uml-playground-stale', errors.length > 0);
        if (errors.length) return; // the last good diagram stays, faded
        diagram.innerHTML = core.diagram(text, { background: null }).svg;
    }
    input.addEventListener('input', draw);
    draw();
});
</script>
<!-- /part -->

## Ein Diagramm schreiben

<!-- part: aufbau -->
### Aufbau einer Datei

Eine `.uml`-Datei besteht aus Klassen und Assoziationen, in beliebiger Reihenfolge. Was hinter `#` steht, ist ein Kommentar. Leere Zeilen sind egal.

```uml
# Räume einer Schule
class School
  - name: String
  + School(name: String)

class Room
  - number: String
  - seats: int
  + Room(number: String, seats: int)

association rooms: School 1 -- 1..* Room
```

Die Schlüsselwörter sind Englisch: `class`, `extends`, `abstract`, `association`. Namen beginnen mit einem Buchstaben und enthalten Buchstaben, Ziffern und `_`, keine Leerzeichen. Klassen schreibst du groß (`Room`), Attribute und Methoden so, wie sie in deinem Code heißen.
<!-- /part -->

<!-- part: klassen -->
### Klassen, Attribute und Methoden

`class Name` beginnt eine Klasse. Jede eingerückte Zeile darunter ist ein Attribut oder eine Methode, mit einem Zeichen davor, das sagt, wer sie sehen darf:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Zeile</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>- seats: int</code></td><td>ein Attribut: <code>name: Typ</code>. <code>-</code> heißt <b>privat</b>: Nur die Klasse selbst kommt heran.</td></tr>
<tr><td><code>+ book(hours: int): boolean</code></td><td>eine Methode: ihre Parameter in Klammern, nach dem Doppelpunkt der Typ dessen, was sie zurückgibt. <code>+</code> heißt <b>öffentlich</b>.</td></tr>
<tr><td><code>+ clear(): void</code></td><td><code>void</code>: Die Methode gibt nichts zurück. Den Typ musst du immer angeben.</td></tr>
<tr><td><code>+ Room(number: String, seats: int)</code></td><td>der <b>Konstruktor</b>: heißt wie die Klasse und hat keinen Typ</td></tr>
</tbody>
</table></div>

Ein `#` für »geschützt« (protected) gibt es hier nicht, denn `#` beginnt einen Kommentar.

Die Typen schreibst du wie in Java, auch wenn du in einer anderen Sprache programmierst:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Typ</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>int</code>, <code>long</code></td><td>ganze Zahl</td></tr>
<tr><td><code>double</code>, <code>float</code></td><td>Kommazahl</td></tr>
<tr><td><code>boolean</code></td><td>wahr oder falsch</td></tr>
<tr><td><code>String</code>, <code>char</code></td><td>Text, ein einzelnes Zeichen</td></tr>
<tr><td><code>List&lt;Room&gt;</code>, <code>Set&lt;Room&gt;</code>, <code>Map&lt;String, Room&gt;</code></td><td>eine Liste, eine Menge, ein Dictionary oder Hash; auch <code>ArrayList</code>, <code>Queue</code>, <code>Stack</code> und andere</td></tr>
<tr><td><code>int[]</code></td><td>ein Array</td></tr>
<tr><td><code>Room</code></td><td>jede Klasse aus derselben Datei</td></tr>
</tbody>
</table></div>

Einen anderen Typ unterstreicht die UML-Werkstatt gelb und fragt, ob eine Klasse fehlt oder der Name falsch geschrieben ist.
<!-- /part -->

<!-- part: namen -->
### Namen wie im Code

Im Diagramm stehen die Namen so, wie dein Programm sie schreibt, damit du beides nebeneinander lesen kannst:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Sprache</th><th>Attribut</th><th>Methode, die fragt</th></tr></thead>
<tbody>
<tr><td>Ruby</td><td><code>free_seats</code></td><td><code>available?</code></td></tr>
<tr><td>Python</td><td><code>free_seats</code></td><td><code>is_available</code></td></tr>
<tr><td>JavaScript, Dart, Java</td><td><code>freeSeats</code></td><td><code>isAvailable</code></td></tr>
</tbody>
</table></div>

Eine Methode darf, wie in Ruby, auf `?` oder `!` enden. Die Prüfungen einer Aktivität nehmen `isAvailable`, `is_available` und `available?` als dieselbe Methode.
<!-- /part -->

<!-- part: vererbung -->
### Vererbung und abstrakte Klassen

`extends` hinter dem Namen sagt, dass eine Klasse alles von einer anderen erbt. Die **Unterklasse** schreibt nur hin, was neu ist oder was sie anders macht:

```uml
class Vehicle
  - seats: int
  + Vehicle(seats: int)
  + seats(): int

class Bicycle extends Vehicle
  + Bicycle()
  + seats(): int
```

Steht eine Methode der Oberklasse noch einmal in der Unterklasse, wie `seats()` in `Bicycle`, **überschreibt** die Unterklasse sie: Ein Fahrrad antwortet auf seine Weise.

Eine Klasse, von der es keine eigenen Objekte geben soll, nur Objekte ihrer Unterklassen, ist **abstrakt**: `class Track (abstract)`. Im Diagramm steht dann «abstract» über ihrem Namen, und der Name ist kursiv.
<!-- /part -->

<!-- part: assoziationen -->
### Assoziationen und Multiplizitäten

Eine **Assoziation** sagt, dass Objekte einer Klasse Objekte einer anderen kennen:

```uml
association name: KlasseA mult -- mult KlasseB
```

Die **Multiplizität** steht bei der Klasse, deren Anzahl sie angibt. `School 1 -- 1..* Room` liest du in beide Richtungen: Eine Schule hat einen oder mehrere Räume, ein Raum gehört zu genau einer Schule.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Multiplizität</th><th>Lies</th></tr></thead>
<tbody>
<tr><td><code>1</code></td><td>genau eins</td></tr>
<tr><td><code>0..1</code></td><td>keins oder eins</td></tr>
<tr><td><code>*</code> oder <code>0..*</code></td><td>beliebig viele, auch keins</td></tr>
<tr><td><code>1..*</code></td><td>mindestens eins</td></tr>
<tr><td><code>4</code>, <code>2..6</code></td><td>genau vier, zwei bis sechs</td></tr>
</tbody>
</table></div>

Mit `->` statt `--` kennt nur die linke Klasse die rechte, nicht umgekehrt; im Diagramm hat die Linie dann eine Pfeilspitze:

```uml
association queue: Counter 1 -> 0..* Ticket
```

Im Code wird aus einer Assoziation meist ein Attribut: Die Schule hat eine Liste ihrer Räume. Im Diagramm steht dieses Attribut nicht noch einmal in der Klasse; die Linie sagt es schon.
<!-- /part -->

<!-- part: lesen -->
## Ein Diagramm lesen

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Im Diagramm</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td>ein Kasten mit drei Fächern</td><td>eine Klasse: oben der Name, in der Mitte die Attribute, unten die Methoden</td></tr>
<tr><td>«abstract», der Name kursiv</td><td>eine abstrakte Klasse</td></tr>
<tr><td>eine Linie mit einem leeren Dreieck</td><td>Vererbung; das Dreieck zeigt auf die Oberklasse, die immer darüber steht</td></tr>
<tr><td>eine Linie ohne Spitze</td><td>eine Assoziation; ihr Name steht kursiv in der Mitte, die Multiplizitäten an den Enden</td></tr>
<tr><td>eine Linie mit offener Pfeilspitze</td><td>eine Assoziation, die nur in eine Richtung bekannt ist</td></tr>
</tbody>
</table></div>
<!-- /part -->

<!-- part: er -->
## Klassendiagramm oder ER-Modell?

Beide zeigen Kästen mit Linien, aber sie beschreiben etwas anderes. Ein [ER-Modell](/reference/er-modelle) beschreibt, was in einer Datenbank gespeichert wird und lange bleibt. Ein Klassendiagramm beschreibt die Objekte eines laufenden Programms: was sie wissen und was sie tun. Darum hat ein Klassendiagramm Methoden und ein ER-Modell nicht. Viele Programme haben beides; dazwischen übersetzt oft eine Bibliothek, ein ORM (Object-Relational Mapping), Objekte in Zeilen einer Tabelle und zurück.
<!-- /part -->

## Werkzeuge

<!-- part: werkstatt -->
### Die UML-Werkstatt in VS Code

Eine Datei mit der Endung `.uml` öffnet VS Code mit der UML-Werkstatt.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Was</th><th>Wie</th></tr></thead>
<tbody>
<tr><td>Fehler</td><td>rot unterstrichen, Warnungen gelb, mit Erklärung unter der Maus; alle im Bereich »Probleme« (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>M</kbd>)</td></tr>
<tr><td>Diagramm anzeigen</td><td>»Klassendiagramm anzeigen« oben rechts im Editor, oder <kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>V</kbd>. Das Diagramm ändert sich beim Tippen; solange ein Fehler in der Datei ist, zeigt es blass den letzten Stand ohne Fehler und darüber die Fehler. Ein Klick auf einen Fehler springt in seine Zeile.</td></tr>
<tr><td>Zoomen</td><td>−, + und »Einpassen« über dem Diagramm, oder <kbd>Strg</kbd> und das Mausrad</td></tr>
<tr><td>Diagramm sichern</td><td>»Diagramm als SVG speichern« schreibt <code>name.svg</code> neben die Datei, auf weißem Grund. Eine SVG-Datei, die nicht von der UML-Werkstatt stammt, wird nur nach Rückfrage überschrieben.</td></tr>
</tbody>
</table></div>
<!-- /part -->

<!-- part: kommandozeile -->
### Auf der Kommandozeile

Die UML-Werkstatt ist eine JavaScript-Datei, die auch ohne VS Code läuft. Im Workspace liegt sie im Ordner der Erweiterung; die Quelle ist `src/vscode-extensions/uml/core/uml-core.js` im Repository des Workspaces.

Merke dir den Pfad zuerst in einer Variablen:

```bash_copy
UML=$(ls /workspace/.extensions/hackschule.hackschule-uml-*/core/uml-core.js | tail -1)
```

Dann schreibt `node $UML` das Diagramm als SVG oder als JSON, mit allen Klassen, Attributen, Methoden und Assoziationen:

```bash
node $UML --svg schule.uml > schule.svg
node $UML --json schule.uml
```
<!-- /part -->

## Zum Nachschlagen

Die Schreibweise der `.uml`-Dateien gehört zur UML-Werkstatt; die Diagramme folgen der UML.

- [Die UML 2.5.1](https://www.omg.org/spec/UML/2.5.1) bei der Object Management Group, auf Englisch: die vollständige Beschreibung aller Diagramme; Klassen, Attribute und Methoden stehen in Kapitel 9, Klassen und Assoziationen in Kapitel 11. Zum Nachschlagen einzelner Begriffe, nicht zum Durchlesen.
- [Klassen entwerfen](/supermarkt): die Aktivität, in der du einen Entwurf als Klassendiagramm zeichnest und dann programmierst.
- Für Daten, die gespeichert werden: [ER-Modelle als Text](/reference/er-modelle).
