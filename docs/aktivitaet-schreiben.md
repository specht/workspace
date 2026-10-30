# Eine Aktivität mit Aufgaben schreiben

Eine Anleitung für Lehrkräfte, die für den Hackschule Workspace eine eigene
Aktivität schreiben wollen, an der eigenen Schule oder als Beitrag. Die
vollständige Beschreibung aller Felder steht in [aufgaben.md](aufgaben.md);
hier geht es um den Weg von der Idee bis zur fertigen Seite.

Eine fertige Vorlage liegt in [`docs/vorlage-aktivitaet/`](vorlage-aktivitaet/).
Kopiere sie, statt bei null anzufangen.

Bevor du schreibst: die fünf Regeln, warum der Workspace sich anders anfühlt
als eine Lernplattform (Prüfungen lesen nur, Rückmeldungen zeigen, was da ist,
Ergebnisse sind Dateien, beim ersten Mal ausführlich, nichts wird versteckt),
stehen in [aufgaben.md](aufgaben.md), »Fünf Regeln«; alle Regeln für Seiten
mit Grund und Prüfung in [checkliste.md](checkliste.md).

## 1. Ordner anlegen

```bash
cp -r docs/vorlage-aktivitaet src/content/meine-aktivitaet
cd src/content/meine-aktivitaet
mv vorlage-aktivitaet.md meine-aktivitaet.md
mv vorlage-aktivitaet.webp meine-aktivitaet.webp
```

Der Ordnername ist die Adresse der Seite (`/meine-aktivitaet`) und der Name
der Aktivität. Nur Kleinbuchstaben, Ziffern und `-`. In `tasks.yaml` muss
`activity:` genauso heißen.

Dann die Seite in den Katalog eintragen, in `src/content/sections.yaml` unter
dem passenden Abschnitt:

```yaml
  entries:
    - meine-aktivitaet
```

Danach stimmt das Bild der Wege auf `/lehrplan` nicht mehr: Es wird einmal in
der Entwicklung berechnet und gespeichert. Berechne es auf dem Host mit
`src/scripts/curriculum-graph.rb` neu (etwa 3 Minuten) und committe
`src/ruby/include/curriculum-graph.json` mit. Dasselbe gilt, wenn eine
Aktivität wegfällt oder ihr `requires:` sich ändert; `curriculum_graph_test.rb`
schlägt fehl, solange das Bild veraltet ist.

Eine neue Aktivität, die noch nicht für alle da sein soll, bekommt einen
Punkt vor den Namen (`- .logikgatter`): Sie steht dann nur in der
Entwicklung im Katalog, auf `/lehrplan`, in »Wie es weitergehen kann« und
in »Wird verwendet in« des Handbuchs; in der Produktion steht sie in keiner
Liste (`dev_only_test.rb`), und das gespeicherte Bild der Wege hat für die
Produktion ein eigenes Layout ohne sie. Unter ihrer Adresse bleibt sie auch in
der Produktion erreichbar, für den, der sie kennt (so wollte es der
Maintainer: was nicht auf der Startseite steht, darf erreichbar sein).

Nur eingetragene Seiten gibt es überhaupt. Soll ein ganzer Abschnitt
vorübergehend nicht im Katalog stehen, aber erreichbar bleiben (etwa über
einen Link im Profil), bekommt er `hidden: true`, statt auskommentiert zu
werden. Mit `tiles: true` zeigt ein Abschnitt seine Seiten als kleine Kacheln
mit Logo, Name und Jahr statt als Karten, wie die Programmiersprachen; das
Jahr steht dafür im Titel der Seite in Klammern.

## 2. Die Seite schreiben

`meine-aktivitaet.md` ist Markdown. Oben steht der Meta-Block:

| Feld | Wofür |
| --- | --- |
| `image` | Bild für die Karte im Katalog, selbst 16:9: Die Karte zeigt es überall ganz, auf dem Desktop wie auf dem Handy. Hat das Bild der Seite ein anderes Format, bekommt die Karte ein eigenes `teaser.webp` aus einem Rezept (docs/checkliste.md). Eine Zeichnung, die ganz zu sehen sein soll, bekommt `bild.webp:contain`: Sie wird mit der Farbe ihrer Ecke auf 16:9 aufgefüllt, auch ihre dunkle Fassung. Schau die Startseite hell und dunkel an, auch mit 390 px Breite |
| `kind` | `projekt`, `labor`, `grundlagen`, `uebung`, `galerie` oder `steckbrief` (Programmiersprachen); steht auf der Karte, jede Aktivität im Katalog braucht eines |
| `language` | Die Sprache, in der man programmiert, wenn es eine feste gibt: `Ruby`, `C / raylib`; steht als »Projekt (Ruby)« auf der Karte |
| `duration` | Freier Text, zum Beispiel »2 Doppelstunden« |
| `curriculum` | Wo die Aktivität in den Lehrplänen steht, in den Namen der Pläne von Berlin und Brandenburg: `"Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen); Oberstufe: Algorithmen und Datenstrukturen"`. Nur angemeldete Lehrkräfte sehen sie, oben auf der Seite und auf `/lehrplan`. Form und erlaubte Namen in [lehrplan.md](lehrplan.md) |
| `topics` | Die Themen der Aktivität aus `src/content/curricula/themen.yaml`, zum Beispiel `[grundstrukturen, unterprogramme]`; aus ihnen leiten die Pläne anderer Länder ihre Zeile ab ([plan-bundeslaender.md](plan-bundeslaender.md)) |
| `idea`, `icon` | Nur bei Übungen mit Katas: die Idee in wenigen Worten und ein Icon, ein Bootstrap-Icon (`bi-braces`) oder eine Figur in Grau (`/images/pip/icon-pip.png`, aus `make_icons.py`), für den Streifen der Katas auf der Startseite (checkliste.md, »Katas und Pip«) |
| `requires` | Aktivitäten, die vorher sinnvoll sind |
| `reference` | Handbuch-Seiten, die die Aktivität braucht; erscheinen oben auf der Seite |
| `template` | Adresse einer Vorlage zum Klonen (`https://…`); im Text `${template}`. Der Workspace sagt Lernenden, wenn ihr Klon veraltet ist |
| `template_name` | Name der Vorlage im Genitiv, z. B. `der CPU Sandbox` |
| `class_repo` | `group`, wenn die Seite mit einem Klassen-Repository arbeitet: Die Lehrkraft trägt die Adresse je Gruppe auf der Klassenseite ein, im Text steht `${class_repo}`, und die Seite braucht Teile mit und ohne Klassen-Repository (`<!-- if: class_repo -->`, docs/aufgaben.md). Nie eine feste Adresse |

Danach: ein Titel (`# …`), ein kurzer Absatz `<p class='abstract'>` für die
Karte, dann Abschnitte mit `## …`. Jeder Abschnitt erscheint im
Inhaltsverzeichnis am Rand.

Bilder liegen im selben Ordner und werden mit `<img class='full' src='bild.webp'
alt='…'>` eingebunden. Der `alt`-Text beschreibt, was zu sehen ist; leer
(`alt=''`) nur, wenn das Bild nur schmückt. `full` ist mindestens 1000 Pixel
breit, damit Text in einem Screenshot (Editor, Terminal) auf dem Handy lesbar
bleibt; man schiebt das Bild dann zur Seite. Ein Bild ohne kleine Schrift,
etwa eine 3D-Szene oder ein Foto, bekommt dazu `fit-width`
(`<img class='full fit-width' …>`): Es ist auf dem Handy so breit wie der
Bildschirm.

Screenshots aus dem Workspace macht man nicht von Hand: Ein Rezept
`<!-- tutorial-screenshot … -->` direkt vor dem `<img>` beschreibt, was zu tun
ist, und der Generator erzeugt das Bild, auch neu, wenn sich die Seite oder
das Workspace-Image ändert. Die Rezeptsprache steht in
[src/tutorial-screenshots/README.md](../src/tutorial-screenshots/README.md),
die Handgriffe in [entwicklung.md](entwicklung.md#screenshots-und-videos-der-tutorials).

Schreibt ein Rezept Dateien aus dem Ordner der Aktivität
(`write-file: … <- file:stufen/…`), muss der Code, den die Seite seit dem
vorigen Rezept zeigt, in diesen Dateien stehen; Auslassungen mit `…` und
andere Einrückung sind erlaubt (`page_code_test.rb`). So zeigt die Seite den
Code, den Bilder und Beispiele testen. Ein Block, der mit Absicht anders ist,
etwa ein Gerüst, das die Lernenden ausbauen, steht mit
`<!-- page-code: own -->` in der Zeile vor dem Codeblock.

Dieselben Rezepte geben Lehrkräften an jedem Schritt den ganzen Code: Unter
dem Bild eines Rezepts (bei `tutorial-step` gleich danach, in einer Galerie
nach der Galerie) steht für sie ein kleiner Knopf »Code bis hier«, der die
ganze Datei kopiert (`stage_code.rb`). Das Programm eines Schritts ist, was
das Rezept schreibt: eine Textdatei aus `stufen/` oder die fertige aus
`loesung/`, ein Codeblock der Seite (`previous-code`, `code:…`) oder eine
andere Datei im Ordner der Aktivität, wenn die Seite dieselbe Datei mehr als
einmal schreibt (ein Programm, das wächst). Eine Datei kommt nur dort, wo
sie sich seit dem letzten Mal geändert hat; Bilder, Archive, einmalige
Beispiele, Beispiele aus eingebundenen Teilen des Handbuchs und Programme
unter fünf Zeilen sind keine Schritte. Ein Schritt, den die Seite nur in
Stücken zeigt, bekommt seine Datei in `stufen/` und eine Zeile für sich,
`<!-- stage: stufen/lichtspur-2.c as lichtspur.c -->`; `page_code_test.rb`
prüft, dass der Code der Seite seit dem letzten Schritt darin steht.
Ein Programm, das nur für ein Vergleichsbild geändert ist (dieselbe Szene
ohne Path Tracing neben dem Bild mit), heißt `stufen/vergleich-…`
(`stufen/vergleich-ohne-pathtracing.c`): Es ist nie ein Schritt, der Knopf
bleibt beim Programm der Seite. Schreibt kein Rezept den Schritt selbst,
weil alle Bilder dort Vergleiche sind, nennt ihn eine `stage`-Zeile.
Lernende bekommen weder den Knopf noch den Code; der Server lässt beides für
sie weg.

Rezepte können auch prüfen, ob die Schritte der Seite aufgehen:
`check-task: lock` führt die Prüfung einer Aufgabe aus wie der Knopf »Prüfen«,
`check-task-fails: lock` verlangt, dass sie an dieser Stelle noch scheitert,
`check-task: zeugin <- head -n 23 bitstein/strassen/moewenweg.txt | tail -n 1`
trägt vorher eine Antwort oder einen Befehl in die Karte ein,
`expect-no-text: Traceback` schlägt an, wenn etwas nicht auf dem Bild stehen
darf. Schritte ohne Bild stehen in `<!-- tutorial-step … -->`.
`src/scripts/recreate-screenshots.rb SEITE` geht dann die ganze Seite durch
und bricht beim ersten Schritt ab, der nicht aufgeht. Die Anweisungen stehen in
[src/tutorial-screenshots/README.md](../src/tutorial-screenshots/README.md#assertions-does-the-pages-step-work).

Mehrere Bilder nebeneinander stehen immer in einer Galerie, jedes in einer
`<figure>` mit Alt-Text und Unterschrift; ein Klick öffnet ein Bild groß,
mit seiner Unterschrift (außer es ist ein Link):

```html
<div class='gallery'>
<figure><img src='a.webp' alt='…'><figcaption>Unterschrift</figcaption></figure>
<figure><img src='b.webp' alt='…'><figcaption>Unterschrift</figcaption></figure>
</div>
```

Ohne weitere Klasse verteilen sich die Bilder nach Platz. `columns-2`,
`columns-3` und `columns-4` halten so viele in einer Reihe, für Bilder, die
man vergleicht (auf dem Handy zwei, bei `columns-2` eins); `portrait` ist
für hohe Bilder wie einen Handybildschirm, `tiles` für viele kleine wie die
Modelle eines Kenney-Kits, `centered` für wenige kleine Bilder wie zwei Diagramme
(höchstens 16em breit, in der Mitte, immer in einer Reihe; mit `small` höchstens
11em, für Skizzen mit wenigen großen Beschriftungen), `single` für ein
einzelnes Bild, das man lesen muss, ein Diagramm mit Beschriftungen oder eine
Karte (höchstens 36em breit und nie höher als der größte Teil des
Bildschirms, in der Mitte), `wide-narrow` für ein
breites Bild neben einem schmaleren, etwa gleich hoch (ein Blick in den Raum
und die flache Textur daneben; auf dem Handy untereinander). Mit `hide-lone` fällt von vier Bildern das vierte
weg, wenn drei in eine Reihe passen und es allein in der nächsten stünde: für
Beispiele, bei denen drei genügen. Ein Bild mit der Klasse `inspect`
(`<img class='inspect' …>`) ist zum genauen Hinsehen da, etwa eine ganze
Lightmap: Es öffnet sich auch auf dem Handy, und im großen Bild vergrößern
Mausrad, zwei Finger oder ein Doppelklick die Stelle, auf die sie zeigen;
ziehen verschiebt es, ab anderthalbfacher Größe mit scharfen Pixeln.
Bilder zum Vergleichen sind einzelne Dateien, die
Unterschrift steht in der `<figcaption>`, nicht ins Bild gezeichnet: So lesen
sie auch Screenreader, und sie bricht auf dem Handy um.
`gallery_markup_test.rb` prüft, dass jede Galerie so aussieht und keine
Bootstrap-Reihe Bilder nebeneinanderstellt.

Code-Blöcke bekommen ihre Sprache (```` ```bash ````, ```` ```html ````). Kurzen
Code, an dem Lernende etwas lernen, tippen sie ab; dabei lesen sie jede Zeile.
Einen Kopierknopf bekommt, was abzutippen nichts lehrt: ```` ```bash_copy ````
für Adressen zum Klonen, Downloads, Einrichtungsbefehle und Zeilen mit
Prüfsummen. Dazu kommen große Blöcke ab etwa 15 Zeilen, die in eine Datei
eingefügt werden: ganze Dateien, lange Funktionen, Shader (```` ```c_copy ````,
bei `_include_file` als Sprache `lua_copy`). Erklärt wird so ein Block
trotzdem im Text, Zeile für Zeile. Weitere Endungen: `_wrap` (lange Zeilen
umbrechen) und `_lineno` (Zeilennummern), in beliebiger Reihenfolge.

Voreingestellt hat trotzdem jeder Codeblock einen Kopierknopf. Wählt eine
Lehrkraft auf der Klassenseite für eine Gruppe »Kopierknöpfe: Wo sinnvoll«,
bekommen deren Lernende nur an den mit `_copy` markierten Blöcken einen und
tippen den Rest ab. Darum markiert man die Blöcke trotzdem so und schreibt die
Seite so, als tippten sie ab. Was kein Code ist, bekommt nie einen: Bitfelder
und Hexdumps (`<pre class='spec'>`) und von Hand geschriebene Ausgaben oder
Dateinamen (`<pre class='no-copy'>`).

Kommt neuer Code mitten in eine Datei, zeigt der Block, wo: Zeilen, die mit
`~ ` anfangen, sind **Kontext**, Code, der schon in der Datei steht. Sie
erscheinen blass, lassen sich nicht markieren, und der Kopierknopf lässt sie
weg; `~ …` steht für Code dazwischen. Wo Kontext und neuer Code
aneinanderstoßen, schneidet eine Zickzacklinie den Block, und der Kopierknopf
steht neben der ersten Zeile neuen Codes. Bis zu drei Zeilen davor und danach
reichen. Der Kontext zeigt die Datei, wie sie an dieser Stelle der Seite ist:
Wird Code später ersetzt, ist sein alter Stand der Kontext, nicht der neue.

```c
~ move_player(dt);
~
Vector3 sun = Vector3Normalize((Vector3){ 0.5f, 0.8f, 0.3f });
~
~ Vector3 eyes = { feet.x, feet.y + EYE_HEIGHT, feet.z };
```

Schreiben die Rezepte der Seite die Datei Stufe für Stufe (`write-file: … <-
file:…`), schlägt `src/scripts/code-context.rb SEITE.md` den Kontext vor: Es
sucht jeden Block in der Datei der Stufe und nimmt die Zeilen davor und
danach, ohne den Code, den spätere Blöcke erst hinzufügen. Mit `--apply`
schreibt es die Vorschläge in die Seite; `--previous` nimmt zuerst die Stufe
vor dem Block (wenn das Bild vor dem Code steht), `--existing` macht Zeilen,
die schon dastehen, im Block zu Kontext (Überschriften einer Gliederung).
Die Vorschläge immer lesen: Zeigt ein Block eine Änderung statt einer
Ergänzung, oder ordnet die Stufe die Datei anders, als die Seite sie aufbaut,
entscheidest du selbst.

Im Code sind Namen englisch: Variablen, Funktionen, Konstanten, Dateien,
Adressen (`respond`, `style.css`, `/time`), wie in jedem echten Projekt.
Kommentare und was ein Programm ausgibt, dürfen deutsch sein. Im Text
bleiben Fachwörter, die auch deutsche Entwicklerinnen englisch sagen,
englisch: Handshake, Lock, String, Header.

### Widgets zum Durchschalten

Ein Widget, das Schritt für Schritt durch etwas geht (der Huffman-Baum, die
Tabellen von LZW, der GIF- und der JPEG-Decoder), trägt `data-stepper`, und
seine Knöpfe sagen mit `data-go` (`first`, `prev`, `next`, `last`), was sie
tun. Dann schalten <kbd>←</kbd> und <kbd>→</kbd> es durch, ohne eigenen Code
(`src/static/include/stepper_keys.js`): Bei mehreren Widgets auf einer Seite
das, das am meisten vom Fenster einnimmt; in einem Textfeld bewegen die
Pfeiltasten weiter den Cursor.

`data-stepper` setzt das Widget auch vom Text ab: ein heller Streifen mit
einer Linie links und darüber »Zum Ausprobieren · ← → schalten weiter«
(`default.css`). So sieht man beim Lesen, wo es anfängt und aufhört. Das
Element mit `data-stepper` umfasst darum das ganze Widget, nicht nur seine
Knöpfe.

### In Ruby und in Python

Eine Aktivität, deren Sprache frei ist, gibt es in Ruby und in Python.
Lehrkräfte ordnen auf der Klassenseite (`/klasse`) unter »Sprachen der
Klasse« die Sprachen ihrer Gruppe; jede Aktivität zeigt die erste davon, die
sie anbietet, ohne Einstellung Ruby. Lernende können auf
der Seite mit einem Schalter neben der Art der Aktivität selbst umschalten,
und diese Wahl gilt dann für sie auf allen solchen Seiten.

Im Meta-Block stehen dann beide Sprachen, und `language` ist nur noch, was
die Karte im Katalog zeigt:

```yaml
language: Ruby / Python
languages: [ruby, python]
```

Dart und Java kommen genauso dazu (`languages: [ruby, python, dart, java]`,
`language: Ruby / Python / Dart / Java`), JavaScript ebenso. Wer auf einer
Seite eine dieser drei wählt, behält sie getrennt von der Wahl zwischen Ruby
und Python; eine Klasse kann alle fünf ordnen. Wie eine Seite Dart und Java
bekommt (Varianten, eigene typische Fehler, Starter, Zeiten je Sprache):
[plan-dart-java.md](plan-dart-java.md), »How a page gets Dart and Java«.

Was sich unterscheidet, steht zweimal im Text, eingefasst in Kommentare auf
eigenen Zeilen:

```markdown
<!-- language: ruby -->
Lege darin die Datei `lzw.rb` an:
…
<!-- /language -->
<!-- language: python -->
Lege darin die Datei `lzw.py` an:
…
<!-- /language -->
```

Ein solcher Teil umfasst ganze Absätze, Listen oder Code-Blöcke, nie einen
Teil davon, und keine Überschriften, sonst stünden sie zweimal im
Inhaltsverzeichnis. Auch nie zwischen den Zeilen einer Tabelle: Die
Kommentare werden zu `div`-Elementen, die in einer Tabelle nicht stehen
dürfen; eine Tabelle, die Code nennt, steht dann einmal je Gruppe von
Sprachen (`<!-- language: ruby, python -->`, `<!-- language: dart, java -->`,
wie in `laufzeit`). Alles außerhalb gilt für alle Sprachen. Beide Fassungen
stehen in der Seite, und der Schalter zeigt die eine oder die andere, ohne
die Seite neu zu laden. Vorbild sind `src/content/rle/`, `huffman/` und
`lzw/`.

Ein Teil kann für mehrere Sprachen gelten, `<!-- language: javascript dart
java -->`: So steht in `supermarkt/` ein Klassendiagramm mit `serviceTime`
nur einmal für die drei Sprachen, die so schreiben, und je einmal für Ruby
und Python (`service_time`).

Auf einer Seite ohne `languages` (einer Handbuch-Seite) zeigen dieselben
Kommentare beide Sprachen nebeneinander: Ein Teil für Ruby und einer für
Python, die direkt aufeinander folgen, stehen auf breiten Bildschirmen in
zwei Spalten, auf dem Handy untereinander, jeder mit dem Namen seiner Sprache
darüber (`CodeLanguages.pair`). Vorbild sind »In Programmen« in
`reference/kryptographie/` und die Funktionen in `reference/ruby-python/`.

### Menschen hinter einem Namen

Hat ein Mensch die Sache gemacht, um die es auf einer Seite oder in einem
ihrer Abschnitte geht (die Thompson-Konstruktion, die Kleenesche Hülle, HTML,
Quick Sort, die Sprache selbst), steht dort ein Kasten mit einem freien Foto
und zwei, drei Sätzen, wer das war: nach dem Absatz, der den Namen oder die
Sache nennt, oder am Ende des Abschnitts, wenn ein Absatz in Code oder eine
Aufgabe übergeht. Der Kasten verrät keine Aufgabe; steht er vor ihr nah an
der Lösung, kommt er hinter die Aufgabe. Ein Name allein reicht nicht: Wer
nur nebenbei vorkommt oder mit der Sache der Seite nichts zu tun hat, bekommt
keinen Kasten, und eine Seite sucht sich keine Person, weil es von ihr ein
schönes Foto gibt. Gibt es keine Seite, um deren Sache es geht (George Boole,
Jack Bresenham), gibt es keinen Kasten.
Die Sätze knüpfen an das an, was Lernende aus dem Workspace kennen: »Du suchst
im Terminal mit `grep`? Das hat er geschrieben.« Sie sagen genau, was die
Person gemacht hat, mit dem Namen der Sache (»Git«, »den Huffman-Code«), nie
»das Verfahren dieser Seite« oder »das du hier programmierst«: Derselbe Kasten
steht auch im Handbuch auf der Seite Personen, und es klänge, als hätte die
Person die Seite gemacht.

Alle Personen stehen an einer Stelle, auf der Handbuch-Seite
`src/content/reference/personen/personen.md`, jede als Teil mit Überschrift,
Kasten und Foto im selben Ordner. Eine Seite setzt den Kasten mit
`_person(name)` auf einer eigenen Zeile ein, ohne den Rahmen eines
Handbuch-Bausteins. Auf der Seite Personen steht in jedem Kasten von selbst,
auf welchen Seiten die Person vorkommt; versteckte und zurückgestellte Seiten
zählen nicht mit. Über die Überschriften findet die Suche im Handbuch jeden
Namen. Jede Person auf der Seite Personen steht auf mindestens einer sichtbaren
Seite, damit ihr Kasten dort einen Link »Im Workspace« hat
(`person_boxes_test.rb`). Eine Ausnahme sind versteckte Seiten: Ihr Kasten steht als
`<div class='person'>` mit seinem Foto direkt in der Seite, nicht auf der Seite
Personen, die sonst verriete, dass es sie gibt. Die Seite ordnet die Personen nach dem Geburtsjahr, in Abschnitten
(»Geboren vor 1920«, »1920 bis 1939« …), bei zweien oder dreien nach dem
ältesten; innerhalb eines Jahrs nach dem Namen.

```html
<!-- part: thompson -->
### Ken Thompson

<div class='person'>
<img src='thompson.webp' alt='Ken Thompson 2019: ein Mann mit Brille, …'>
<p><strong>Ken Thompson</strong> (geboren 1943). Du suchst im Terminal mit <code>grep</code>? …</p>
<p class='person-credit'>Foto: A. C. Diller, 2019, CC BY-SA 4.0, zugeschnitten (<a href='…'>Wikimedia Commons</a>)</p>
</div>
<!-- /part -->
```

Was in einem Kasten steht, belegen Quellen in
`src/content/reference/personen/sources.yaml`: je Aussage ein Stück des
Kastens, wörtlich, mit Link und der Stelle aus der Quelle, die es belegt
(der Kopf der Datei erklärt das Format). Die Seite `/person-sources` (nur in
der Entwicklung, für Administratoren) zeigt jeden Satz aller Kästen mit seinen
Quellen und färbt, was noch offen ist, was die Quellen nur teilweise stützen
oder widerlegen und welche Aussagen der Kasten nicht mehr enthält. Ein neuer
oder geänderter Kasten bekommt dort seine Belege. Lesende finden die Quellen hinter einem
kleinen Info-Symbol oben rechts im Kasten (»Mehr dazu«, `person_boxes.rb`):
jede Seite einmal, die meistgenutzten zuerst; Datenquellen wie API-Antworten
stehen nur auf `/person-sources`.

Gibt es kein freies Foto (David Huffman, Edgar F. Codd), steht der Kasten
ohne Bild da, nur mit dem Text, ohne Silhouette und ohne Platzhalter: Ein
Schattenriss sähe aus wie ein Bild, das nicht geladen hat. Ohne `<img>` zeigt
der Kasten den Text über die ganze Breite.

Mehrere Menschen in einem Kasten: `<div class='person pair'>` mit
`<div class='portraits'>` um die Fotos, jedes in
`<div class='portrait'>` mit dem Namen darunter (`<span>Alan Kay</span>`),
damit man sieht, wer wer ist. Die Fotos stehen oben links, der Text läuft
daneben und darunter über die ganze Breite weiter; auf dem Handy stehen sie
über dem Text. Haben mehrere eine Sache
gemacht und nur einer ein Foto (A*: Hart, Nilsson, Raphael), steht dieses
eine Foto im Kasten, und der Text sagt, wer darauf ist. Das Foto darf unter
jeder Lizenz stehen, unter der wir es kostenlos zeigen dürfen (gemeinfrei,
CC0, CC BY, CC BY-SA, auch nicht-kommerzielle: der Workspace ist nicht
kommerziell), meist von Wikimedia Commons; die Zeile darunter nennt Urheber,
Jahr und Lizenz. Fotos, die der Maintainer ausgesucht hat, bleiben; ein
besseres schlägt man vor, statt es zu tauschen. Jedes Porträt ist 4:5, die
Augen auf etwa 40 % der Höhe, 320 × 400 Pixel als WebP. Der Kasten steht im
Fluss der Seite, Foto links, Text rechts, und ist so hoch wie das Größere von
beiden.

### Bücher und Aufsätze zum Weiterlesen

Ein Buch, das genau dort weitermacht, wo ein Abschnitt aufhört (Abrashs
*Graphics Programming Black Book* nach den Texture Mappern, Sanglards
*Game Engine Black Book* nach Wolfenstein 3D), bekommt einen Kasten wie ein
Mensch, mit einem Buch-Icon statt eines Fotos: Titel, Autor, Jahr, zwei
Sätze, was darin steht, und wo man es lesen kann. Ein Cover steht nur
darin, wo es frei ist (das Buch unter CC0 oder CC BY…, mit dem Cover) oder
der Verlag es zum Herunterladen anbietet, etwa im Pressebereich oder auf der
Seite des Buchs; dann wird der Kasten `book with-cover`, das Cover
(`<img class='book-cover' …>`, etwa 240 Pixel breit, mit Alt-Text) steht
an der Stelle des Icons, darunter eine Zeile `<p class='book-credit'>Cover:
Verlag (Quelle)</p>`, und ein Cover, das nicht frei ist, kommt mit
`status: granted` und den Bedingungen in `picture-rights.yaml`; ohne
Eintrag bleibt es weg und das Icon steht da (`picture_rights.rb`). Kein
Cover aus einem Shop, von Google Books oder Open Library. Ein Aufsatz, mit dem etwas angefangen
hat (Whitteds Raytracing von 1980), bekommt denselben Kasten mit
`bi-file-earmark-text` statt `bi-book`: nur der historische Aufsatz, in dem
das Verfahren des Abschnitts zum ersten Mal beschrieben ist, und nur, wo er
gut zum Abschnitt passt, nicht zu jedem Thema irgendein Aufsatz.

```html
<div class='book'>
<i class='bi bi-book'></i>
<p><strong>Titel</strong>, Autor, Jahr. Was darin steht. Wo es zu lesen ist: <a href='…'>…</a>.</p>
</div>
```

Nur Bücher, die man lesen kann, ohne sie zu kaufen, oder die so bekannt
sind, dass eine Bibliothek sie hat; jeder Link ist geprüft.

### Zum Ausprobieren einladen

Wie etwas funktioniert, versteht man oft erst, wenn man es ändert und sieht,
was passiert. Wo Lernende gerade etwas zum Laufen gebracht haben, schlägt ein
Kasten eine kleine Änderung vor und sagt, worauf man achten soll:

```html
<div class='hint try'>
<strong>Ausprobieren:</strong> Ersetze <code>gl_FragColor = …</code> durch
<code>gl_FragColor = vec4(n * 0.5 + 0.5, 1.0);</code>. Jede Seite bekommt die
Farbe ihrer Normale: Welche Farbe haben die Oberseiten, und warum?
Stell die Zeile danach wieder zurück.
</div>
```

Das ist keine Aufgabe, geprüft wird nichts, und die Seite geht mit dem
unveränderten Code weiter. Darum endet der Kasten mit »Stell … wieder
zurück«, wenn die Änderung im Weg wäre. Für eine Frage zum Nachdenken ohne
Änderung gibt es `hint think`.

### Jetzt bist du dran

Code, der auf der Seite steht, kann man abtippen, ohne über ihn
nachzudenken. Wo die Seite gerade erklärt hat, was eine kleine Funktion
tun soll, oft mit der Formel dafür, schreiben die Lernenden sie erst
selbst. Ein Kasten sagt, was zu schreiben ist und wo es hingehört; der Code
der Seite folgt zwischen zwei Markierungen, jede auf einer eigenen Zeile,
unter »Lösung anzeigen«:

````markdown
<div class='hint own' data-check='hit-floor'>
<strong>Jetzt bist du dran:</strong> Schreib unter <code>hit_sphere</code> die
Funktion <code>double hit_floor(vec origin, vec dir)</code>. Sie gibt dieses
<code>t</code> zurück, und <code>INFINITY</code>, wenn der Boden hinter dem
Strahl liegt.
</div>

<!-- solution -->
```c
double hit_floor(vec origin, vec dir)
…
```
<!-- /solution -->
````

Zwischen den Markierungen darf alles stehen: mehrere Codeblöcke, Teile für
eine Sprache, ein Absatz, der die Lösung erklärt. Die Seite geht danach mit
diesem Code weiter, so wie nach einem Kasten `hint try`. Rezepte, Stufen und
`page_code_test.rb` sehen den Code wie jeden anderen.

Vor jeder Lösung stehen Tipps in Stufen (unten), und die Lösung ist ihre
letzte Stufe: Sie lässt sich erst öffnen, wenn alle Tipps aufgeklappt sind
und die Pause nach dem letzten vorbei ist. `solution_hints_test.rb` prüft,
dass keine Lösung ohne Tipps dasteht. Prüft eine Aufgabe unter dem Kasten den
eigenen Code, nennt `data-check` am Kasten ihre `id`; wer sie löst, kann die
Lösung sofort öffnen und mit der eigenen vergleichen.

Geeignet sind Stellen, die klein und abgeschlossen sind und deren Idee die
Seite schon erklärt hat: eine Formel in eine Funktion übersetzen, eine
Bedingung, ein Aufruf einer Funktion, die es schon gibt (ein Schattenstrahl
mit `nearest_hit`), wenige Zeilen in einem Gerüst, das die Seite zeigt
(`// hier: forward, right und up`). Unter die Lösung gehört, wo es geht, eine
Aufgabe, die den eigenen Code prüft (`ruby`, `python`, `c`): Dann wissen die
Lernenden, ob ihre Fassung stimmt, auch bevor das Programm sie aufruft.
Nicht geeignet sind Stellen, an denen etwas Neues erst eingeführt wird,
oder lange Blöcke, nach denen der Rest der Seite nicht mehr passt. Kann man den Code gleich ausprobieren, sag es; wenn
nicht, sag, wann (»Laufen lassen kannst du sie erst gleich mit
`nearest_hit`«). Ein Kasten schließt mit einer Vorhersage
(`prediction: true`) gut zusammen: erst vermuten, was der eigene Code tun
wird, dann starten.

Was die Antwort auf eine Vorhersage verrät, das Bild vom Ergebnis und der
Absatz, der es erklärt, kommt zwischen zwei Markierungen und erscheint erst,
wenn die Aufgabe beantwortet ist (`after_task_test.rb` prüft, dass die
Aufgabe darüber steht):

````markdown
_task(stufen)

<!-- after: stufen -->
<!-- tutorial-screenshot
…
-->
<img class='full fit-width' src='licht.webp' alt='…'>

Nach hinten wird es dunkel. …
<!-- /after -->
````

### Tipps in Stufen

Wo Lernende hängen bleiben können, helfen Tipps in Stufen besser als ein
einziger: erst verstehen, was verlangt ist (ein kleines Beispiel von Hand),
dann der Weg in Worten, dann ein Gerüst mit Lücken. Keine Stufe ist die ganze
Lösung. Eine Aufgabe bekommt sie als Liste in `hint` ([aufgaben.md](aufgaben.md),
»Tipps in Stufen«), ein Kasten »Jetzt bist du dran« vor seiner Lösung, jede Stufe
zwischen zwei Markierungen auf eigenen Zeilen:

````markdown
<div class='hint own'>
<strong>Jetzt bist du dran:</strong> Schreib unter <code>hit_sphere</code> die Funktion …
</div>

<!-- hint -->
Stell `origin.y + t · dir.y = 0` nach `t` um; das steht schon oben.
<!-- /hint -->
<!-- hint -->
Ein Gerüst:

```c
double hit_floor(vec origin, vec dir)
{
    double t = …;   // hier: die Formel von oben
    …
```
<!-- /hint -->

<!-- solution -->
…
````

Die Stufen kommen nacheinander: Die nächste erscheint erst eine Minute,
nachdem die vorige aufgeklappt wurde, ohne Uhr bis dahin, damit man mit
jedem Tipp arbeitet. Nach dem letzten Tipp eines Kastens kommt so die
Lösung. Niemand außer der Person selbst sieht, welche Stufen sie aufgeklappt
hat. Warum es so entschieden ist, steht in
[didaktik.md](didaktik.md).

### Extras für Schnelle

Eine lange Seite hat oft Kapitel, die nicht alle brauchen: einen Ausblick
nach der Abgabe, eine Verfeinerung des fertigen Programms, Hintergrund zum
Lesen. Damit Lernende sehen, was dazugehört und was nicht, steht vor so
einem Kapitel `<!-- extra -->` auf der Zeile direkt vor der Überschrift:

```markdown
<!-- extra -->
## Tiefenschärfe
```

Die Überschrift bekommt dann ein kleines Schild »Extra« mit Sternen
(`bi-stars`), und in beiden Inhaltsverzeichnissen, der Leiste oben und der
Liste am Rand, stehen dieselben Sterne neben dem Kapitel; auf dem Handy ist
sein Strich in der Leiste blasser. Unter dem ersten von mehreren
aufeinanderfolgenden Extras steht eine Zeile, die die Seite selbst
ausrechnet: Gehen die Extras bis zum Ende, »Ab hier Extras für Schnelle: Die
Seite ist ohne sie vollständig.«; kommt danach noch ein Kapitel für alle,
»Extras für Schnelle: dieses und das nächste Kapitel. Wer sie auslässt,
springt zu …« mit einem Link dorthin (`src/ruby/include/extra_chapters.rb`).

Ein Kapitel ist nur dann ein Extra, wenn die Seite ohne es vollständig ist:
Kein späteres Kapitel, das nicht selbst ein Extra ist, baut auf seinem Code
oder seinen Begriffen auf (in den Stufen nachsehen: Steht sein Code in der
Stufe eines späteren Kapitels für alle, gehört es dazu), und die Dauer im
Meta-Block reicht ohne die Extras. Am besten sind es die letzten Kapitel vor
»Zeigen und veröffentlichen« oder »Wie geht es weiter?«. Ein Extra hat keine
Aufgabe, sonst käme »x von y Aufgaben« für alle, die es auslassen, nie ans
Ende; `extra_chapters_test.rb` prüft beides, die Stelle der Marke und dass
kein `_task()` in einem Extra steht. Markiert werden nur ganze Kapitel
(`##`), keine Abschnitte, und die Notiz für Lehrkräfte nennt die Extras im
Ablauf.


### Wie geht es weiter?

Ein Abschnitt `## Wie geht es weiter?` am Ende ist von Hand geschrieben und
sagt, was keine Liste sagt: warum ein nächster Schritt folgt (»Ein Band statt
eines Stacks«), ein Nachbarthema oder Ideen, das eigene Programm
weiterzubauen. Eine bloße Aufzählung der Aktivitäten, die auf dieser
aufbauen, gehört nicht hinein; die steht auf der Startseite und für
Lehrkräfte in der Notiz. Jeder Link darin führt zu einer Aktivität des
Katalogs, einer Handbuch-Seite oder einem Werkzeug (`next_steps_test.rb`).

## 3. Aufgaben schreiben

Aufgaben stehen in `tasks.yaml` neben der Seite und werden mit
`_task(id)` auf einer eigenen Zeile in den Text gesetzt. Gute Aufgaben stehen
dort, wo die Lernenden gerade etwas getan haben, nicht gesammelt am Ende.

Welche Prüfart für welchen Zweck:

| Was du wissen willst | Prüfart |
| --- | --- |
| Hat sie verstanden, was passiert? | `choice`, mit einer Rückmeldung pro falscher Antwort |
| Was wird gleich passieren? (vor dem Ausprobieren) | `choice` mit `prediction: true` |
| Ein Wert, den man ausrechnet | `number` mit `tolerance` |
| Ein Wert, den man im Workspace abliest | `answer` mit `${workspace.ip}` o. Ä., oder `measure` |
| Eine Erklärung in eigenen Worten | `reflection` mit `min_words` |
| Liegt die Datei richtig, hat sie das richtige Format? | `files` |
| Passt das HTML, hat die Präsentation genug Folien? | `html` |
| Ist das CSS fehlerfrei? | `css` |
| Lädt die Seite nichts aus dem Netz? | `references` |
| Ist alles committet und gepusht? | `git` |
| Abgeben | `submit` |
| Schreibt der Befehl die richtige Ausgabe? | `command` |
| Steht in der 3D-Szene, was verlangt ist? | `scene` |
| Tut die Funktion im Workspace, was sie soll? | `ruby`, `python`, `c` |
| Liefert die SQL-Abfrage das Richtige? | `sql` |
| Baut die Schaltung, was sie soll? | `circuit` |

Drei Faustregeln:

- **Rückmeldungen für die typischen Fehler schreiben.** Wer bei einer
  Roundtrip-Frage die Hälfte angibt, hat nur eine Richtung gerechnet. Das
  gehört in `feedback`, nicht in den Hinweis.
- **Die Lösung erst nach dem Lösen.** `solution` erscheint erst nach einer
  richtigen Antwort, als Erklärung. Der Hinweis (`hint`) steht vorher zur
  Verfügung und verrät nicht alles.
- **Prüfarten, die im Workspace nachsehen, nennen ihre Pfade.** Die Karte
  zeigt »Liest `/workspace/…`«. Schreib den Pfad im Text genauso, damit die
  Lernenden wissen, wo die Datei hingehört.

## 4. Was viele Aktivitäten brauchen: das Handbuch

Klonen, Live-Vorschau, SSH-Schlüssel, `git config`: So etwas steht einmal im
Handbuch (`src/content/reference/`) und wird eingebunden:

```markdown
_include(workspace#repository-klonen)
```

Folgt dem Kasten eine Aufgabe, die prüft, ob die Anleitung befolgt wurde,
nenn sie mit `done:`. Ist sie gelöst, schrumpft der Kasten auf eine Zeile:

```markdown
_include(gitlab#git-name, done: git-name)

_task(git-name)
```

Auf der Handbuch-Seite ist ein Teil mit `<!-- part: name -->` und
`<!-- /part -->` markiert. Wenn ein Teil fehlt, den du brauchst, schreib ihn
dort hinzu, statt ihn in deine Seite zu kopieren. Dann profitieren alle
Aktivitäten davon.

Eine Handbuch-Seite enthält die Grundlagen und das Wichtigste, nicht alles.
Für den Rest verweist sie auf die offizielle Referenz oder das Handbuch der
Sprache oder Bibliothek (Spezifikation, offizielle Dokumentation,
Cheatsheet), im passenden Teil oder in einem letzten Abschnitt »Zum
Nachschlagen«. Jeden Link vorher einmal öffnen. Eine Aktivität verweist auf
die Handbuch-Seite, statt deren offizielle Links zu wiederholen: So gibt es
jeden Link nur an einer Stelle, und er veraltet nicht doppelt.

Wo eine Seite einen Standard oder ein Dokument nennt (RFC 6238, RFC 2324,
ISO 3166, einen Aufsatz, eine Spezifikation), ist der Name ein Link darauf,
auch im Fließtext, in einer Aufgabe und in der Notiz für Lehrkräfte: Wer
nachsehen will, wie es dort wirklich steht, soll es mit einem Klick können.
Für RFCs die Fassung beim RFC Editor
(`https://www.rfc-editor.org/rfc/rfc6238.html`), mit Anker auf den
Abschnitt, um den es geht (`#appendix-B`, die Testwerte). Links auf andere
Seiten öffnen sich in einem neuen Tab, damit die Aktivität offen bleibt;
das setzt der Server selbst (`ExternalLinks` in
`src/ruby/include/external_links.rb`, für Seiten, Aufgabenkarten und die
Antworten einer Prüfung), ein `target` im Markdown ist nicht nötig.

## 5. Eine Notiz für Lehrkräfte

`lehrkraft.md` neben der Seite erscheint für Lehrkräfte als aufklappbare Notiz
oben auf der Seite. Jede Aktivität im Katalog hat eine; Lernende sehen sie
nicht. Eine Lehrkraft liest sie beim Planen und schaut in der Stunde schnell
nach, oft auf dem Handy. Darum ist sie ein Blatt zum Nachschlagen, kein
Fließtext ([vorlage-aktivitaet/lehrkraft.md](vorlage-aktivitaet/lehrkraft.md)):

- **Ein kurzer erster Absatz**, ein, zwei Sätze, der sagt, was die Lernenden
  tun: »Lernende bauen …, schreiben …, messen …«. Keine Aufzählung jeder
  Stufe; die steht im Ablauf.
- **Eine Tabelle mit den Fakten**, je Zeile ein Stichwort: Voraussetzungen,
  Sprache (wo es mehrere gibt), Ablauf, Dateien, Prüfung, Abgabe, Lösung.
  Hat eine Zelle mehr als zwei Sätze, wird sie eine Liste; der Ablauf ist
  eine Liste mit einem Punkt je Doppelstunde (`<b>1. Doppelstunde:</b>`) und
  einem für die Schnellen. Die Tabelle ist HTML, also in den Zellen `<code>`,
  `<b>` und `<a href>`, keine Backticks und kein Markdown: Redcarpet ließe sie
  stehen, wie sie sind.
- **Abschnitte mit `####`** für das, was nicht in eine Zeile passt: Was
  schiefgeht, Die Aufgaben (welche Rückmeldung welchen Fehler erkennt),
  Tipps in Stufen. Ihr Inhalt ist eine Liste, ein Punkt je Fehler
  oder Aufgabe, mit dem Stichwort fett vorn (`- **Das Bild ist gespiegelt.**
  …`).
- **Zuletzt `#### Eigene Projekte`**, wo die Aktivität zu einem eigenen
  Projekt führen kann (nicht bei Katas, »Erste Schritte« oder Seiten, die
  nur erklären): drei bis fünf Ideen, mit denen die
  Lehrkraft aus der Aktivität etwas Eigenes der Lernenden macht, damit sie
  nicht als abgearbeitete Anleitung endet. Jede Idee in zwei, drei Sätzen:
  was entsteht, was von der Seite wiederkommt (die DDA, die Kamera-Ebene,
  die Kapselung), wie groß sie ist (`(eine Doppelstunde)`, `(ein Projekt)`)
  und woran man sie ohne Prüfung bewertet: Vorführen, ein Spieltest, das
  Erklären des eigenen Codes. Das Ergebnis ist offen, bei jeder und jedem
  anders; keine Schritte, keine Bildschirmfotos, keine Lösung, nur der
  Ausgangspunkt. Was die Idee braucht und auf der Seite nicht steht, sagt
  sie in einem Satz (eine Karte, die sich ändern lässt). Eine Stelle der
  Seite verlinkt sie mit dem Text ihrer Überschrift ohne Nummer,
  `[»Ein Planet«](#ein-planet)`, oder mit einer Aufgabe, `#aufgabe-march`:
  Auf einem breiten Bildschirm steht die Notiz neben dem Text, und der Link
  bewegt nur den Text (`teacher_note.js`); die Nummer eines Kapitels ändert
  sich, wenn Kapitel dazukommen. `heading_anchors_test.rb` prüft jeden
  solchen Link. Vorbilder: die
  Notizen zum [Raycaster](../src/content/raycaster/lehrkraft.md) und zum
  [Raytracer](../src/content/raytracing/lehrkraft.md).

Auf dem Handy steht das Stichwort einer Zeile über ihrem Text, und kurzer
Code (`01-karte.c`) bricht nicht am Bindestrich um. `teacher_note_test.rb`
prüft, dass es die Notiz gibt, dass ihr erster Absatz »Lernende« nennt und
dass in ihrer Tabelle kein Markdown steht.

## 6. Prüfen, bevor es live geht

```bash
cd src/ruby
ruby test/task_spec_test.rb      # Format aller tasks.yaml
ruby test/task_card_test.rb      # jedes _task() hat eine Aufgabe
ruby test/page_kinds_test.rb     # jede Handbuch-Seite in reference: gibt es
ruby test/task_examples_test.rb  # jede Prüfung nimmt ihre Beispiele an und lehnt Gegenbeispiele ab
ruby test/page_code_test.rb      # der Code auf der Seite steht in den Dateien der Rezepte
ruby test/curriculum_graph_test.rb  # das Bild auf /lehrplan passt zu den Aktivitäten, requires: zeigt nie auf Ältere
```

Dann im Entwicklungsmodus die Seite öffnen. Als Lehrkraft siehst du die
Aufgaben auch dann, wenn `TASKS_ENABLED` aus ist, mit dem Vermerk »Vorschau für
Lehrkräfte«. Probier jede Aufgabe einmal richtig und einmal mit dem typischen
Fehler aus.

Nach dem Einschalten zeigt die [Klassenübersicht](aufgaben.md#klassenübersicht)
unter `/klasse`, wer wo hängt.

## 7. Wenn du etwas änderst

Ändert sich an einer Aufgabe nur der Text (`title`, `prompt`, `hint`,
`feedback`, `solution`), bleiben alle Ergebnisse gültig. Ändert sich `check`,
bekommt die Aufgabe eine neue Version: Bisherige Antworten zeigen »Aufgabe
geändert, bitte neu prüfen«, und die Zählung beginnt neu. Benenne eine
Aufgabe (`id`) nicht um, solange Lernende sie schon bearbeitet haben; ihre
Ergebnisse hängen an der `id`.
