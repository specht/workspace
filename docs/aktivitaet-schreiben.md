# Eine Aktivität mit Aufgaben schreiben

Eine Anleitung für Lehrkräfte, die für den Hackschule Workspace eine eigene
Aktivität schreiben wollen, an der eigenen Schule oder als Beitrag. Die
vollständige Beschreibung aller Felder steht in [aufgaben.md](aufgaben.md);
hier geht es um den Weg von der Idee bis zur fertigen Seite.

Eine fertige Vorlage liegt in [`docs/vorlage-aktivitaet/`](vorlage-aktivitaet/).
Kopiere sie, statt bei null anzufangen.

## Die fünf Regeln

Bevor du schreibst: Jede Aktivität folgt diesen Regeln. Sie sind der Grund,
warum der Workspace sich anders anfühlt als eine Lernplattform.

1. **Prüfungen lesen nur.** Eine Aufgabe ändert nie etwas im Workspace der
   Lernenden. Sie sieht nach.
2. **Die Rückmeldung zeigt, was wirklich da ist.** Nicht »falsch«, sondern
   »In `index.html` Zeile 16 ist `<div>` aus Zeile 15 noch offen.«
3. **Ergebnisse sind Dateien.** Lernende arbeiten mit echten Dateien in echten
   Ordnern. Eine Abgabe ist eine Kopie, über die sie bestimmen.
4. **Ausführlich beim ersten Mal, eine Zeile danach.** Was neu ist, wird
   erklärt; was bekannt ist, steht im Handbuch und wird verlinkt.
5. **Adressen und Werkzeuge bleiben sichtbar.** Pfade, Befehle und Adressen
   stehen im Text, wie sie sind. Nichts wird versteckt.

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
| `image` | Bild für die Karte im Katalog, 16:9 |
| `kind` | `projekt`, `labor`, `grundlagen`, `uebung`, `galerie` oder `steckbrief` (Programmiersprachen); steht auf der Karte, jede Aktivität im Katalog braucht eines |
| `language` | Die Sprache, in der man programmiert, wenn es eine feste gibt: `Ruby`, `C / raylib`; steht als »Projekt (Ruby)« auf der Karte |
| `topic` | Abschnitt im Katalog (Schlüssel aus `src/content/sections.yaml`) |
| `duration` | Freier Text, zum Beispiel »2 Doppelstunden« |
| `requires` | Aktivitäten, die vorher sinnvoll sind |
| `reference` | Handbuch-Seiten, die die Aktivität braucht; erscheinen oben auf der Seite |
| `template` | Adresse einer Vorlage zum Klonen (`https://…`); im Text `${template}`. Der Workspace sagt Lernenden, wenn ihr Klon veraltet ist |
| `template_name` | Name der Vorlage im Genitiv, z. B. `der CPU Sandbox` |
| `class_repo` | Adresse eines Klassen-Repositorys; im Text `${class_repo}` |

Danach: ein Titel (`# …`), ein kurzer Absatz `<p class='abstract'>` für die
Karte, dann Abschnitte mit `## …`. Jeder Abschnitt erscheint im
Inhaltsverzeichnis am Rand.

Bilder liegen im selben Ordner und werden mit `<img class='full' src='bild.webp'
alt='…'>` eingebunden. Der `alt`-Text beschreibt, was zu sehen ist; leer
(`alt=''`) nur, wenn das Bild nur schmückt.

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

Rezepte können auch prüfen, ob die Schritte der Seite aufgehen:
`check-task: lock` führt die Prüfung einer Aufgabe aus wie der Knopf »Prüfen«,
`check-task-fails: lock` verlangt, dass sie an dieser Stelle noch scheitert,
`expect-no-text: Traceback` schlägt an, wenn etwas nicht auf dem Bild stehen
darf. Schritte ohne Bild stehen in `<!-- tutorial-step … -->`.
`src/scripts/recreate-screenshots.rb SEITE` geht dann die ganze Seite durch
und bricht beim ersten Schritt ab, der nicht aufgeht. Die Anweisungen stehen in
[src/tutorial-screenshots/README.md](../src/tutorial-screenshots/README.md#assertions-does-the-pages-step-work).

Mehrere kleine Bilder nebeneinander stehen in einer Galerie; ein Klick
öffnet ein Bild groß, mit seiner Unterschrift:

```html
<div class='screen-gallery'>
<figure><img src='a.webp' alt='…'><figcaption>Unterschrift</figcaption></figure>
<figure><img src='b.webp' alt='…'><figcaption>Unterschrift</figcaption></figure>
</div>
```

Code-Blöcke bekommen ihre Sprache (```` ```bash ````, ```` ```html ````). Kurzen
Code, an dem Lernende etwas lernen, tippen sie ab; dabei lesen sie jede Zeile.
Einen Kopierknopf bekommt, was abzutippen nichts lehrt: ```` ```bash_copy ````
für Adressen zum Klonen, Downloads, Einrichtungsbefehle und Zeilen mit
Prüfsummen. Dazu kommen große Blöcke ab etwa 15 Zeilen, die in eine Datei
eingefügt werden: ganze Dateien, lange Funktionen, Shader (```` ```c_copy ````,
bei `_include_file` als Sprache `lua_copy`). Erklärt wird so ein Block
trotzdem im Text, Zeile für Zeile. Weitere Endungen: `_wrap` (lange Zeilen
umbrechen) und `_lineno` (Zeilennummern), in beliebiger Reihenfolge.

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

### In Ruby und in Python

Eine Aktivität, deren Sprache frei ist, kann es in Ruby und in Python geben.
Lehrkräfte stellen auf der Klassenseite (`/klasse`) unter »Sprache« ein,
welche ihre Gruppe sieht; ohne Einstellung ist es Ruby. Lernende können auf
der Seite mit einem Schalter neben der Art der Aktivität selbst umschalten,
und diese Wahl gilt dann für sie auf allen solchen Seiten.

Im Meta-Block stehen dann beide Sprachen, und `language` ist nur noch, was
die Karte im Katalog zeigt:

```yaml
language: Ruby / Python
languages: [ruby, python]
```

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
Inhaltsverzeichnis. Alles außerhalb gilt für beide Sprachen. Beide Fassungen
stehen in der Seite, und der Schalter zeigt die eine oder die andere, ohne
die Seite neu zu laden. Vorbild sind `src/content/rle/`, `huffman/` und
`lzw/`.

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
| Eine Programmieraufgabe mit Tests | `code` (Code Bites) |
| Liefert die SQL-Abfrage das Richtige? | `sql` |

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

## 5. Eine Notiz für Lehrkräfte

`lehrkraft.md` neben der Seite erscheint für Lehrkräfte als aufklappbare Notiz
oben auf der Seite. Was hineingehört: der Ablauf über die Stunden, was
erfahrungsgemäß schiefgeht, was die Aufgaben genau prüfen. Lernende sehen sie
nicht.

## 6. Prüfen, bevor es live geht

```bash
cd src/ruby
ruby test/task_spec_test.rb      # Format aller tasks.yaml
ruby test/task_card_test.rb      # jedes _task() hat eine Aufgabe
ruby test/page_kinds_test.rb     # jede Handbuch-Seite in reference: gibt es
ruby test/task_examples_test.rb  # jede Prüfung nimmt ihre Beispiele an und lehnt Gegenbeispiele ab
ruby test/page_code_test.rb      # der Code auf der Seite steht in den Dateien der Rezepte
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
