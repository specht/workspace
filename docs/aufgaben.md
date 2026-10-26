# Aktivitäten, Aufgaben und Handbuch

Dieses Dokument beschreibt, wie Inhalte im Workspace aufgebaut sind und wie
Aufgaben geschrieben werden, die der Workspace selbst prüft. Es richtet sich an
alle, die Inhalte schreiben, auch an Lehrkräfte anderer Schulen mit eigener
Installation.

**Stand:** Format 1, Meilenstein M6. Aufgabenkarten auf den Seiten, alle
Prüfarten, Abgaben, die Klassenübersicht und die Startseite für Lernende. Was ab wann läuft, steht in der
Tabelle der Prüfarten.

Wer eine eigene Aktivität schreiben will, fängt mit
[aktivitaet-schreiben.md](aktivitaet-schreiben.md) an; dieses Dokument ist
das Nachschlagewerk dazu.

## Die fünf Bausteine

| Baustein | Was es ist | Wo es liegt |
| --- | --- | --- |
| Aktivität | Text zum Lesen und Mitmachen: Projekt, Labor, Grundlagen oder Galerie | `src/content/<slug>/<slug>.md` |
| Aufgabe | Ein Schritt in einer Aktivität, den der Workspace prüft | `src/content/<slug>/tasks.yaml` |
| Handbuch | Kurz und vollständig, ohne Geschichte; von vielen Aktivitäten verlinkt | `src/content/reference/<slug>/<slug>.md`, Adresse `/reference/<slug>` |
| Werkzeug | Etwas, womit man arbeitet: Workspace, Datenbanken, ER-Werkstatt, die Sandboxes | eigene Seiten oder eigene Projekte |
| Klasse | Gruppen aus den Einladungslisten; eine Person kann in mehreren sein | `data/invitations/*.txt` |

Die Sandboxes bleiben eigenständige Projekte. Der Workspace verlinkt sie und kann lesen, was in einem Workspace liegt, ändert
aber nichts an ihnen.

## Fünf Regeln

Der Workspace soll Einzelheiten nicht verstecken: echte Dateien, echte Pfade,
speichern, Ergebnis ansehen. Aufgaben und Prüfungen halten sich deshalb an
diese Regeln.

1. **Die Prüfung liest nur.** Sie schaut sich die Dateien der Lernenden an wie
   eine Lehrkraft über die Schulter. Sie schreibt, verschiebt und repariert
   nichts und verändert keine Git-Repositories.
2. **Jede Rückmeldung zeigt, was wirklich da ist:** Dateiname, Zeile, Größe,
   Format, Pfad, Git-Stand. Nicht nur »richtig« oder »falsch«.
3. **Ergebnisse sind Dateien im eigenen Ordner.** Eine Bauaufgabe nennt die
   Datei, in der gearbeitet wird. Eine Abgabe ist eine Kopie, die die
   Lernenden sehen, ersetzen und löschen können.
4. **Beim ersten Mal ausführlich, danach eine Zeile.** Klonen, Live Server,
   `git push`: einmal Schritt für Schritt im Handbuch, in der Aktivität
   dann ein Satz mit Link. Wer etwas noch nie gemacht hat, bekommt die lange
   Fassung angezeigt.
5. **Adressen und Werkzeuge bleiben offen.** Vorschau mit URL, Terminal,
   Explorer. Nichts davon verschwindet hinter einer eigenen Oberfläche.

## Dateien einer Aktivität

```text
src/content/showerjs/
├── showerjs.md        Aktivität: Text mit Platzhaltern für Aufgaben
├── tasks.yaml         Aufgaben mit ihren Prüfungen
├── task-examples.yaml Beispiele, die jede Prüfung annehmen und ablehnen muss
├── lehrkraft.md       Notiz für Lehrkräfte (nur für Lehrkräfte sichtbar)
├── lehrkraft/         Lösungsdateien (werden nie ausgeliefert)
└── *.webp             Bilder, auch aus Screenshot-Rezepten

src/content/reference/
├── sections.yaml      Katalog der Handbuch-Seiten
└── html/
    └── html.md
```

Dateien im Inhaltsordner werden nur ausgeliefert, wenn eine Seite sie als Bild
oder Video einbindet. Lösungen können deshalb gefahrlos neben der Aktivität
liegen.

`lehrkraft.md` wird wie die Seite gerendert und erscheint für Lehrkräfte als
aufklappbare »Notiz für Lehrkräfte« über der Aktivität. Andere sehen sie nicht.
Eine Seite ist immer `<slug>.md`; `lehrkraft.md` wird nie zur Seite.

## Metadaten

Der YAML-Block am Anfang einer Seite bekommt neue, freiwillige Felder. Seiten
ohne sie funktionieren unverändert weiter.

```html
<div class='meta'>
kind: labor
duration: 2 Doppelstunden
requires: [working-with-files]
reference: [netzwerk]
template: https://git.nhcham.org/specht/shower.js.git
template_name: der shower.js-Vorlage
image: tcpip.webp
</div>
```

| Feld | Bedeutung |
| --- | --- |
| `kind` | `projekt`, `labor`, `grundlagen`, `galerie`, `uebung`, `steckbrief` oder `reference`; steht als Etikett auf der Karte im Katalog |
| `language` | Die Sprache, in der man auf der Seite programmiert oder schreibt, zum Beispiel `Ruby` oder `C / raylib`; steht in Klammern hinter der Art: »Projekt (Ruby)«. Nur bei Seiten mit einer festen Sprache |
| `duration` | Freier Text, zum Beispiel »2 Doppelstunden«; nur Lehrkräfte sehen ihn, im Streifen über der Seite (zum Planen von Stunden, Lernende brauchen ihn nicht) |
| `requires` | Aktivitäten, die vorher sinnvoll sind (Slugs); Lernende sehen sie als »Baut auf … auf« im Streifen über der Seite und auf der Karte im Katalog. Ein Hinweis, keine Bedingung: nichts wird gesperrt oder umsortiert. Aktivitäten, die nicht im Katalog stehen, fallen weg; `page_kinds_test.rb` prüft, dass jeder Slug im Katalog steht |
| `reference` | Handbuch-Seiten, auf die `_uses()` verweist |
| `search` | Nur Handbuch-Seiten: Wörter, mit Kommas getrennt, unter denen die Suche im Handbuch die Seite findet, ohne dass sie im Titel stehen (`search: Regex, Regexp` für »Reguläre Ausdrücke«); ein Treffer führt an den Anfang der Seite |
| `template` | Adresse der Vorlage zum Klonen (`https://…`); andere Schulen können hier einen Spiegel eintragen. Im Text `${template}`; der Workspace sagt Lernenden, wenn es eine neue Version gibt (siehe unten) |
| `template_name` | Name der Vorlage im Genitiv, für »Es gibt eine neue Version der CPU Sandbox!«; ohne das Feld: »… von `cpu-sandbox`« |
| `class_repo` | `group`: Die Seite arbeitet mit einem Klassen-Repository, dessen Adresse die Lehrkraft je Gruppe auf der Klassenseite einträgt (die Sandboxes). Nie eine Adresse; siehe »Teile mit und ohne Klassen-Repository« |
| `image` | Das Bild der Karte im Katalog, relativ zur Seite; `name.webp:30:0` setzt den Ausschnitt (x und y in Prozent), `name.webp:contain` zeigt eine Zeichnung ganz |
| `image_alt` | Was das Bild der Karte zeigt, in Anführungszeichen (`image_alt: "Ein Huffman-Baum mit …"`); Pflicht zu `image`, `alt_text_test.rb` prüft es. Steht dasselbe Bild mit Alt-Text auf der Seite, gilt dieser |

Über jeder Seite steht ein schmaler Streifen, der sagt, was sie ist:

- **Aktivität:** die Art aus `kind` (ohne `kind`: »Aktivität«), mit der Sprache
  aus `language` in Klammern, die Dauer aus `duration` (nur für
  Lehrkräfte), »Baut auf … auf« mit Links zu den Aktivitäten aus
  `requires` und Links zu den Handbuch-Seiten aus `reference` und aus
  jedem `_include()` der Seite.
- **Handbuch-Seite:** »Handbuch › Abschnitt« und »Wird verwendet in«
  mit allen Aktivitäten, die die Seite nennen oder Teile daraus einbinden.

Alles, was aus dem Handbuch stammt, ist türkis markiert: Handbuch-Seiten
selbst und die eingebetteten Kästen in Aktivitäten. Im Menü führt
»Handbuch« zur Übersicht.

## Platzhalter im Text

Wie die vorhandenen `_include_file()` und `_include_svg()` werden diese
Platzhalter ersetzt, bevor Redcarpet den Text liest.

| Platzhalter | Ergebnis | Ab |
| --- | --- | --- |
| `_task(adresse)` | Die Aufgabe `adresse` aus `tasks.yaml` an dieser Stelle | M2 |
| `_include(workspace#repository-klonen)` | Der Teil `repository-klonen` der Handbuch-Seite `workspace`, eingebettet in einem Kasten mit Link | M1 |
| `_include(gitlab#git-name, done: git-name)` | Wie oben; ist die Aufgabe `git-name` auf dieser Seite gelöst, zeigt der Kasten beim nächsten Laden nur noch eine Zeile | M7 |
| `_uses(ifconfig, ping)` | Kleine Marken, die zu den Teilen `ifconfig` und `ping` der Handbuch-Seiten aus `reference` führen | M1 |
| `${web_root}` | Die Adresse dieser Installation (`WEB_ROOT`), etwa für den Spiegel von Open-Meteo: `${web_root}/open-meteo/api/v1/forecast` | M7 |
| `${class_repo}`, `${git_host}`, `${git_web}`, `${git_ssh}` | Auf einer Seite mit `class_repo: group` die Adresse des Klassen-Repositorys der Gruppe, ihr Server (`git.example.org`), dessen Webseite (`https://git.example.org/`) und was `ssh -T` braucht (`git@git.example.org`, mit `-p` bei anderem Port); erst beim Aufruf eingesetzt, je nach Person. Lehrkräfte, Administration und Besucher sehen Platzhalter (`[Adresse des Klassen-Repositorys]`, `git.example.org`), auf Handbuch-Seiten Lernende den Server ihrer Gruppe | M7 |
| `${git_server_hint}` | Nur wer keine Adresse hat, liest hier » (das ist ein Beispiel: Welchen Server deine Klasse benutzt, sagt dir deine Lehrkraft)«; sonst nichts | M7 |
| `${contact_email}` | Die Kontaktadresse der Installation (`CONTACT_EMAIL` in `credentials.rb`), etwa für »schreib eine E-Mail an«; nie eine Adresse fest in den Text schreiben | M7 |
| `_tool(tcp-console)` | Ein Kasten mit Symbol, Name und Zweck eines Werkzeugs der Website (`tcp-console`, `automatron`, Liste in `tools.rb`), der es in einem neuen Tab öffnet; auf einer Zeile für sich. Die Werkzeuge stehen nicht im Menü, sondern dort im Text, wo man sie braucht, und brauchen eine Anmeldung | M7 |
| `_person(thompson)` | Der Kasten einer Person (Foto, wer sie war, was sie gemacht hat) aus der Handbuch-Seite `reference/personen`, ohne Rahmen eines Baustein-Kastens; auf einer Zeile für sich. Die Seite Personen nennt in jedem Kasten die Seiten, auf denen er steht (docs/aktivitaet-schreiben.md, »Menschen hinter einem Namen«) | M7 |
| `_schema(countries)` | Die Tabellen eines Datensatzes als Schema, eine Zeile pro Tabelle: `movie_genre(↑movie_id, ↑genre_id)`, der Primärschlüssel unterstrichen, ein Fremdschlüssel mit ↑ und im Tooltip, worauf er verweist; auf einer Zeile für sich, in einem Kasten »Die Tabellen«. `_schema(countries-basic, rows)` zeigt unter jeder Zeile die ersten drei Zeilen der Tabelle, für die Stelle direkt nach einem Import; `new: border currency` markiert die Tabellen, die neu sind, und klappt nur sie auf. Die Datensätze (`countries`, `movies`, `music`) und ihre Schlüssel stehen in `src/static/query-stepper/`, wie für den Query Stepper; `src/scripts/query-stepper-data.rb` liest sie aus den SQL-Dateien | M7 |

### Teile für eine Funktion der Installation

Was nur geht, wenn eine Installation eine Funktion eingeschaltet hat, steht
zwischen `<!-- feature: case_server -->` und `<!-- /feature -->`, jede Marke
auf einer Zeile für sich. Ist die Funktion aus, fällt alles dazwischen weg,
bevor die Aufgabenkarten gesucht werden: Seine Aufgaben stehen dann weder auf
der Seite noch in der Klassenübersicht. Bisher gibt es nur `case_server`, den
Fallserver für »Ein Krimi im Terminal« ([entwicklung.md](entwicklung.md#der-fallserver)).
Die Marken dürfen auch Zeilen einer HTML-Tabelle umschließen.

### Teile mit und ohne Klassen-Repository

Eine Seite mit `class_repo: group` im Meta-Block gibt es in zwei Fassungen.
Was nur mit einem Klassen-Repository Sinn hat (Zugang zum Git-Server, das
Klassen-Repository klonen, die Git-Aufgaben, Veröffentlichen), steht
zwischen `<!-- if: class_repo -->` und `<!-- /if -->`; was nur ohne gilt
(den Ordner selbst anlegen, die Arbeit bleibt im Workspace), zwischen
`<!-- if: !class_repo -->` und `<!-- /if -->`. Was nur gilt, wenn die
Adresse über SSH geht (`git@…`, `ssh://…`: der SSH-Schlüssel), steht in
`<!-- if: class_repo_ssh -->`, was nur über HTTPS gilt (Zugangstoken), in
`<!-- if: class_repo_https -->`. Jede Marke steht auf einer Zeile für
sich; die Teile schachteln sich nicht, also endet ein `class_repo`-Teil vor
einem `class_repo_ssh`-Teil und beginnt danach neu.

Beim Laden wird die Seite in jeder Fassung gerendert (mit SSH, mit HTTPS,
sofern sie die beiden unterscheidet, und ohne), mit eigenen
Aufgabenkarten (`ContentParts.filter_conditions`, `ClassRepos` in
`src/ruby/include/class_repos.rb`). Bei jedem Aufruf wählt der Server eine
und setzt die Adresse ein:

- Lernende sehen die Fassung **mit**, wenn eine ihrer Gruppen eine Adresse
  für die Seite hat (die erste solche Gruppe in der Reihenfolge ihrer
  Gruppen), und zwar die mit HTTPS bei einer `https://`-Adresse, sonst die
  mit SSH; ohne Adresse die Fassung **ohne**.
- Lehrkräfte, Administration und nicht Angemeldete sehen die Fassung mit
  SSH, mit Platzhaltern statt der Adresse.

Aufgaben, die in der Fassung einer Person fehlen, zählen für sie nirgends:
nicht auf der Seite, nicht in »x von y Aufgaben« und »Weiter mit« auf der
Startseite. Die Klassenübersicht zählt für jede Gruppe die Fassung, die ihre
Lernenden sehen. Eine Aufgabe darf in beiden Fassungen stehen, aber in jeder
nur einmal; `class_repos_test.rb` prüft die Sandboxes: jede Aufgabe aus
`tasks.yaml` in einer Fassung, die Git-Aufgaben nur mit, die Fragen zum
Thema in allen, `ssh-schluessel` nur mit SSH.

### Teile einer Handbuch-Seite

Eine Handbuch-Seite gliedert sich in Teile. Ein Teil beginnt mit einer
Markierung und endet am nächsten Teil, an `<!-- /part -->` oder am Ende der
Datei:

```markdown
<!-- part: repository-klonen -->
## Repository klonen

Klicke auf »Clone Repository« und füge die Adresse der Vorlage ein:

    ${template}
<!-- /part -->
```

- Auf der Handbuch-Seite selbst wird die Markierung zu einem Sprungziel:
  `/reference/workspace#repository-klonen`.
- Bei `_include()` fällt die erste Überschrift des Teils weg, weil die
  Aktivität ihre eigene schreibt. Relative Bildpfade werden passend
  umgeschrieben.
- Mit `done: id` faltet sich der Kasten, sobald die Aufgabe `id` gelöst ist:
  »✓ Git sagen, wer du bist: erledigt, Aufgabe 2 ist gelöst. Anzeigen«. Das
  passiert nur beim Laden der Seite, nie während jemand liest. Die Aufgabe
  muss auf derselben Seite stehen (ein Test prüft das). So gilt auch für
  eingebundene Anleitungen: beim ersten Mal ausführlich, danach eine Zeile.
- Ohne `done:` faltet sich ein Kasten, wenn die Person ihn schon kennt: wenn
  sie auf einer anderen Seite, die denselben Teil einbindet, mindestens eine
  Aufgabe gelöst hat. Statt des Kastens steht dann eine Zeile, zum Beispiel
  »Kein Ordner offen (Strg+K, dann F), Seitenleiste zu (Strg+B), Terminal auf
  (Strg+J). Kennst du schon aus »Versionsverwaltung mit Git«. Anleitung
  anzeigen«. Die Zeile schreibt der Teil selbst, direkt unter seiner
  Markierung: `<!-- short: … -->` (nur `Code` und `<kbd>`-Tasten werden ausgezeichnet, `${template}`
  wird ersetzt). Ohne `short:` steht dort die Überschrift des Teils. Wer nicht
  angemeldet ist oder keine Aufgaben sieht, bekommt immer den ganzen Kasten.
- `${template}` wird durch das Feld `template` der einbindenden Seite ersetzt,
  auf der Handbuch-Seite selbst durch »[Adresse der Vorlage]«. Andere
  `${…}`, etwa in JavaScript-Beispielen, bleiben unverändert.
- Ein fehlender Teil bricht nichts: Die Seite zeigt »Baustein fehlt«, und das
  Server-Log nennt Datei und Platzhalter.

## tasks.yaml

```yaml
format: 1
activity: tcpip            # muss dem Ordnernamen entsprechen
tasks:
  - id: adresse            # a–z, 0–9 und -, eindeutig in der Aktivität
    title: Deine Adresse
    prompt: |
      Wie lautet die IPv4-Adresse deines Adapters `eth0`?
    check:
      type: answer
      expect: ${workspace.ip}
    hint: Suche unter `eth0` die Zeile, die mit `inet` beginnt.
    feedback:
      - when: { matches: '^127\.' }
        say: Das ist die Adresse von `lo`. Gesucht ist die von `eth0`.
```

### Felder einer Aufgabe

| Feld | Pflicht | Bedeutung |
| --- | --- | --- |
| `id` | ja | Kennung; zusammen mit der Aktivität weltweit eindeutig: `tcpip/adresse` |
| `title` | ja | Kurzer Name, erscheint in Listen und im Klassenboard |
| `prompt` | ja | Die Aufgabe in Markdown |
| `check` | ja | Wie geprüft wird; alles, was über richtig und falsch entscheidet, steht hier |
| `file` | nein | Die Datei, in der gearbeitet wird, relativ zu `/workspace`; wird bei der Aufgabe angezeigt |
| `hint` | nein | Hinweis, den man aufklappen kann; oder eine Liste von 2 bis 5 Tipps in Stufen (siehe »Tipps in Stufen«) |
| `feedback` | nein | Eigene Rückmeldungen für typische Fehler |
| `prediction` | nein | `true`: Vorhersage; nur der erste Versuch zählt fürs Klassenboard, danach darf ausprobiert werden. Was die Antwort verrät, steht auf der Seite zwischen `<!-- after: id -->` und `<!-- /after -->` und erscheint erst nach einer Antwort (aktivitaet-schreiben.md) |
| `solution` | nein | Lösung; nur Lehrkräfte sehen sie |
| `examples` | nein | Nur bei `reflection`: eine Liste von Beispielantworten in Markdown, die nur Lehrkräfte unter der Aufgabe sehen (siehe »Antworten unter offenen Aufgaben«) |
| `exam` | nein | `false`: nie in einer Leistungsüberprüfung; Prüfarten mit Netz sind dort ohnehin gesperrt |
| `starter` | nein | Nur bei `function`: der Code, mit dem die Karte beginnt (höchstens 4000 Zeichen); er muss von selbst scheitern |
| `level` | nein | Nur für Katas gedacht: `1` Einstieg, `2` Übung, `3` Knobelei; die Karte zeigt so viele Punkte |
| `extra` | nein | `true`: für Schnelle und Neugierige; zählt bei niemandem mit (»8 von 10 erledigt, dazu 2 Extras«), die Karte zeigt »Extra«, das Klassenboard die Spalte heller und rechnet sie nicht zu »gelöst«. Eine Aufgabe in einem Extra-Kapitel muss eine sein |
| `ruby`, `python` | nein | Was in dieser Sprache anders ist, auf Seiten in mehreren Sprachen (siehe unten) |

### In mehreren Sprachen

Auf einer Seite mit `languages: [ruby, python]` (siehe
[aktivitaet-schreiben.md](aktivitaet-schreiben.md#in-ruby-und-in-python))
kann eine Aufgabe für eine Sprache anders sein. Unter dem Namen der Sprache
steht nur, was sich ändert: `prompt`, `check`, `hint`, `solution`,
`feedback`, `file` oder `starter`. Alles andere gilt für beide.

```yaml
  - id: kompressor
    title: Der Kompressor
    prompt: Schreib `compress` in `lzw.rb`.
    check:
      type: ruby
      file: lzw/lzw.rb
      cases:
        - compress('ABABABA'): '[65, 66, 256, 258]'
    python:
      prompt: Schreib `compress` in `lzw.py`.
      check:
        type: python
        file: lzw/lzw.py
        cases:
          - compress(b'ABABABA'): '[65, 66, 256, 258]'
```

Die Seite zeigt dann für jede Sprache eine eigene Karte mit derselben
Nummer, und die Prüfung nimmt die Fassung der Karte. Die Aufgabe behält ihre
`id` und hat eine einzige Version, die aus allen `check` zusammen berechnet
wird: Wer sie in Ruby gelöst hat, hat sie auch in Python gelöst, und die
Klassenübersicht zählt sie einmal. Ändert sich der `check` einer Sprache,
ändert sich die Version für beide.

### Variablen

In `expect` können Werte stehen, die der Server pro Person kennt.

| Variable | Wert |
| --- | --- |
| `${workspace.ip}` | IP-Adresse des eigenen Workspace |
| `${workspace.live_url}` | Adresse des eigenen Live Servers, z. B. `https://q7k2m-5500.workspace.hackschule.de` |
| `${person.codeword}` | Ein Codewort, das für jede Person anders ist |

### Rückmeldungen

Jede Prüfung bringt eigene Rückmeldungen mit, die zeigen, was sie gelesen hat.
Mit `feedback` kommen Sätze für typische Fehler dazu. Die Bedingungen in
`when` hängen von der Prüfart ab: `answer` kennt `equals` und `matches`,
`number` kennt `equals` und `between`, `choice` kennt `picked`, `command`
kennt `output_matches`, `scene` kennt `found`, `missing`,
`source_matches` und `problem`.

### Tipps in Stufen

Statt eines Tipps kann `hint` eine Liste sein. Die Karte zeigt dann »Tipp«, danach
»Nächster Tipp«, ohne zu sagen, wie viele es gibt; die nächste Stufe erscheint erst eine Minute, nachdem die vorige
aufgeklappt wurde (`HintSteps::DELAY_SECONDS`), ohne Uhr bis dahin.

````yaml
hint:
  - |
    Rechne das erste Beispiel von Hand: A hat die Länge 1, B und C die Länge 2.
    Welcher Code mit zwei Stellen ist der kleinste, der nicht mit `0` anfängt?
  - |
    Sortiere die Bytes nach der Länge … Wird die Länge größer, kommen hinten
    Nullen dazu; das ist `<<`.
  - |
    Ein Gerüst:

    ```ruby
    def canonical(lengths)
        …
            # hier: hinten Nullen anhängen, wenn length größer als previous ist
    ```
````

Die Stufen gehen vom Verstehen (ein Beispiel von Hand) über den Weg in Worten
zu einem Gerüst mit Lücken. Keine Stufe enthält die ganze Lösung. Was jemand
aufgeklappt hat, merkt sich nur der Browser (`hint_steps.js`), für jedes
Konto getrennt (`data-hint-owner`, ein Hash); der Server erfährt es nicht,
Lehrkräfte sehen es nicht. Die Gründe stehen in
[didaktik.md](didaktik.md). Eine Variante für eine andere Sprache kann eine
eigene Liste haben.

### Versionen

Jede Aufgabe hat eine Version, die aus ihrem `check` berechnet wird. Ändert
sich die Bewertung, ändert sich die Version, und frühere Ergebnisse erscheinen
als »geändert« statt als stilles Häkchen. Tippfehler in `title`, `prompt` oder
`hint` ändern die Version nicht.

## Prüfarten

| Art | Prüft | Liest | Läuft | Ab |
| --- | --- | --- | --- | --- |
| `answer` | Text, Adresse oder regulären Ausdruck | die Eingabe | Server | M2 |
| `choice` | Auswahl aus Antworten | die Eingabe | Server | M2 |
| `number` | Zahl mit Toleranz | die Eingabe | Server | M2 |
| `reflection` | nichts; Freitext geht ans Klassenboard | die Eingabe | Server | M2 |
| `files` | ob es Dateien gibt, Namen, Größe, Format nach den ersten Bytes, ausführbar | Dateien in `/workspace` | Server | M3 |
| `html` | Folien, Überschriften, offene Tags, Bildpfade, alt-Texte | eine HTML-Datei | Server | M3 |
| `css` | eigene Regeln, offene Klammern | eine CSS-Datei | Server | M3 |
| `references` | Adressen in `src`, `href`, `url()`, `@import`: extern oder vorhanden | HTML- und CSS-Dateien | Server | M3 |
| `git` | Remote vorhanden, alles committet, alles gepusht | `git status` eines Repositories | Workspace | M3 |
| `submit` | Abgabe: Kopie von Dateien, optional für die Galerie | Dateien in `/workspace` | Server | M3 |
| `measure` | Messwert der Lernenden gegen eine eigene Messung | Ausgabe eines Befehls | Workspace | M6 |
| `command` | Ausgabe eines Befehls der Lernenden | Ausgabe | Workspace | M6 |
| `python` | Funktionen aus einer Python-Datei der Lernenden: Aufrufe und ihre Ergebnisse | eine `.py`-Datei | Workspace, als Gast | M7 |
| `ruby` | dasselbe für eine Ruby-Datei | eine `.rb`-Datei | Workspace, als Gast | M7 |
| `c` | Funktionen aus einer C-Datei der Lernenden, übersetzt mit `cc`, ohne ihr `main` | eine `.c`-Datei | Workspace, als Gast | M7 |
| `function` | eine Kata: Funktionen aus dem Code, der in der Karte geschrieben wird, wie `python` und `ruby` | der Code der Karte | eigener Container ohne Netz | M8 |
| `pip` | eine Kata mit Pip: ein Plan aus Befehlen, der Pip über eine oder mehrere Karten schickt; Ziel erreicht, nichts schiefgegangen | der Code der Karte | eigener Container ohne Netz, die Welt auf dem Server | M8 |
| `scene` | Szene des Anaglyph-Viewers, zu einem oder mehreren Zeitpunkten | eine `.ini`-Datei | Workspace | M6 |
| `sql` | Ergebnis einer `SELECT`-Abfrage gegen das einer Musterabfrage | Abfrage | eigene MySQL-Datenbank, nur lesend | M6 |
| `cypher` | Ergebnis einer Cypher-Abfrage gegen das einer Musterabfrage | Abfrage | eigene Neo4j-Datenbank, nur lesend | M7 |
| `er` | ER-Modell: Entitäten, Schlüssel, Attribute, Beziehungen, Kardinalitäten und die Tabellen, die daraus werden | eine `.er`-Datei | Workspace (ER-Werkstatt) | M7 |
| `schema` | Tabellen der eigenen Datenbank: Spalten, Primär- und Fremdschlüssel, Indizes, Zeilenzahl | `information_schema` | eigene MySQL-Datenbank, nur lesend | M7 |
| `keyboard` | ein Kapitel des Keyboard-Tutorials: alle Schritte abgehakt | `/workspace/.hs-kbd-tutorial/.state.json` | Server | M7 |
| `story` | BIF-Geschichte: Seiten, Entscheidungen, Bedingungen, Enden, Fehler, erreichbar | `<projekt>/.story-tools/analysis.json`, die BIF selbst schreibt | Server | M7 |

»Server« heißt: Der Server liest die Datei als Text und wertet sie aus, ohne
etwas auszuführen. »Workspace« heißt: Ein festes Prüfskript läuft per
`docker exec` im Workspace der Person, mit ihren Rechten und einer Zeitgrenze.
Alles, was etwas ausführt oder dekodiert, läuft dort, nie auf dem Server: Git
(eine `.git/config` kann Befehle auslösen), Bilder, die Ausdrücke des
Anaglyph-Viewers.

### answer

```yaml
check:
  type: answer
  compare: url                 # text (Standard), url oder regex
  expect: ${workspace.live_url}/pictures/fuji.jpg
```

`expect` darf eine Liste sein. Groß- und Kleinschreibung zählt nur mit
`case_sensitive: true`; bei `url` zählt sie im Pfad immer.

### choice, number, reflection

```yaml
check: { type: choice, options: [A, B, C], correct: 1 }      # gezählt ab 0
check: { type: number, expect: 111, tolerance: 6, unit: ms }
check: { type: reflection, min_words: 20 }
```

Bei `number` trägt man die Zahl ein, nicht den Befehl, der sie findet. Sieht
die Antwort wie ein Befehl aus (eine Pipe `|` oder am Anfang ein Befehl wie
`grep`, `wc`, `cat`, `echo` mit etwas dahinter, `TaskCheck::COMMANDS`), sagt
die Prüfung: »Das ist der Befehl, der die Zahl findet. Führe ihn im Terminal
aus und gib hier die Zahl ein …«, ohne ihn auszuführen und ohne dass es als
falscher Versuch zählt. So wird auch `head -n 5 … | wc -l` nicht als 5
gelesen. Soll ein Befehl die Antwort sein, ist `command` die richtige Art
(unten).

Eine Reflexion hat keine Lösung, aber Beispielantworten: zwei kurze, gute
Antworten, wie sie eine Schülerin oder ein Schüler geben könnte, mit den
Begriffen der Seite. Sie ändern die Version der Aufgabe nicht.

```yaml
examples:
  - |
    `traceroute` schickt zuerst ein Paket mit TTL 1. Der erste Router …
  - |
    Jeder Router verringert die TTL eines Pakets um eins …
```

Jede Person sieht die Optionen einer `choice`-Aufgabe in ihrer eigenen
Reihenfolge, immer derselben (Seed: E-Mail-Adresse und Aufgabe, siehe
`TaskCard.option_order`). »Die dritte Antwort« lässt sich so nicht
weitersagen. Nur die Plätze ändern sich: `correct`, `picked` in `feedback`
und die gespeicherten Antworten zählen weiter in der Reihenfolge aus
`tasks.yaml`. Wo die Reihenfolge selbst etwas bedeutet (»Alle oben
genannten«), schaltet `shuffle: false` das ab.

### files

```yaml
check:
  type: files
  path: shower.js/pictures
  template: [fuji.jpg, monkey.jpg]   # Dateien der Vorlage zählen nicht als neu
  show: listing                      # gelesene Liste immer mit anzeigen
  expect:
    new_files: { min: 1 }
    name: '^[a-z0-9-]+\.(jpe?g|png|webp)$'
    format_matches_extension: true
    max_size: 500kB
```

Für eine einzelne Datei: `exists`, `executable`, `format` (z. B. `elf`) und
`contains`: reguläre Ausdrücke, die irgendwo in einer Zeile der Datei passen
müssen, jeder auf Wunsch mit einem eigenen Satz für den Fall, dass er fehlt.
Unter »Was der Workspace gelesen hat« steht die Zeile, in der jeder gefunden
wurde.

```yaml
check:
  type: files
  path: TIC-80/sterne.lua
  expect:
    format: text
    contains:
      - 'function TIC'
      - { pattern: 'sfx\(', say: 'Noch kein Ton: `sfx(0)` gehört dorthin, wo ein Stern gefangen wird.' }
```

### html, css, references

```yaml
check:
  type: html
  file: shower.js/index.html
  expect:
    parses: true
    slides: { selector: section.slide, min_own: 5, each_has: h2, template_titles: [...] }
    img: { min: 1, src_exists: case_sensitive, alt: required }
  warn: { words_per_slide: 60 }
```

`img` zählt Bilder: `min` wie viele `<img>`, `local` wie viele davon einen
Pfad im Workspace haben statt einer Adresse im Netz (für »ein eigenes Bild«),
`src_exists: case_sensitive` verlangt, dass jeder solche Pfad zu einer Datei
führt, genau so geschrieben, und `alt: required` einen Alt-Text an jedem Bild.

`html` kennt außerdem `elements`: so viele Elemente einer Art (ein einfacher
Selektor wie `li`, `ul li` oder `a.extern`), wahlweise mit einem nicht leeren
Attribut. `say` ersetzt die Rückmeldung, wenn sie fehlen:

```yaml
expect:
  elements:
    - { selector: li, min: 3 }
    - { selector: a, attr: href, say: "So sieht ein Link aus: …" }
```

`css` kennt `after_marker`, `linked_from`, `parses`, `min_rules` und
`declares`. `linked_from: website-test/index.html` verlangt, dass diese Seite
das Stylesheet mit `<link rel="stylesheet">` lädt. `declares` verlangt
Angaben, die wirklich in einer Regel stehen, statt nur ihre Zahl:
die Eigenschaft, wahlweise ihr Wert, der Selektor (einer aus der Liste vor
`{`) und eine Media Query, deren Bedingung das Wort enthält:

```yaml
check:
  type: css
  file: website-test/styles.css
  expect:
    declares:
      - { selector: main, property: display, value: flex }
      - { selector: main, property: flex-direction, value: column, media: max-width }
```

Werte vergleicht die Prüfung ohne Groß- und Kleinschreibung und ohne
`!important`. Mit `style_tags: true` liest `css` nur, was in einer Datei
zwischen `<style>` und `</style>` steht, etwa in einer Svelte-Komponente;
Zeilennummern bleiben die der Datei. Fehlt der Block, sagt die Karte das
(`problem: no_style`). `references` kennt `root`, `scan`, `external: none` und
`local_exist: true`.

### git

```yaml
check:
  type: git
  repo: data-structures-sandbox
  expect:
    has_remote: true
    committed: true
    pushed: true
```

Liest nur, als die Person, in ihrem laufenden Workspace oder, wenn der aus
ist, in einem Lese-Container: dasselbe Image ohne code-server, ohne Netz, der
Ordner der Person nur lesbar eingebunden, nach der Prüfung wieder weg. Gelesen
werden
`git status --porcelain --branch`, `git remote` und die Zahl der Commits
(`src/ruby/include/git_check.rb`). `GIT_OPTIONAL_LOCKS=0` verhindert, dass
dabei auch nur der Index neu geschrieben wird. Der Server selbst führt nie Git
auf einem Repository von Lernenden aus. Die Rückmeldung nennt den Stand und den
nächsten Schritt mit den echten Dateinamen, zum Beispiel: »3 Dateien sind
geändert, aber nicht committet: `lena/stack.py`, … Im Terminal, im Ordner
`structures`: `git add -A` und `git commit -m "…"`. Danach `git push`.«
`pushed` vergleicht mit dem zuletzt bekannten Stand des Remotes; die Prüfung
holt nichts aus dem Netz. Lässt sich der Stand gar nicht lesen, sagt die
Karte das, statt zu raten. `min_commits` zählt nur die Commits der Person selbst, erkannt an der
E-Mail-Adresse aus `git config user.email`; ist keine eingetragen, sagt die
Karte, wie man das nachholt. Ist `repo` nur ein Ordner in einem anderen
Repository, sagt die Karte auch das.

Ohne `repo` und nur mit `identity: true` fragt die Karte, wer Git zu sein
glaubt: `git config user.name` und `user.email`, gelesen in `/workspace` im
Container der Person. Fehlt eins, nennt sie die beiden Befehle; stehen dort
noch die Beispielwerte aus dem Handbuch (»Vorname Nachname«,
»deine.mail@example.org«), sagt sie das (`problem: placeholder`). Mit `repo`
zusammen wird `identity` nach dem Repository geprüft.

```yaml
check:
  type: git
  expect:
    identity: true
```

### submit

```yaml
check:
  type: submit
  paths: [shower.js]           # oder pick: anaglyph/*.ini für eine einzelne Datei
  exclude: [shower.js/.git]
  max_size: 20MB
  gallery: false
  requires: { html_parses: true }
```

Der Server kopiert die Dateien beim Abgeben. Lernende sehen ihre Abgabe, geben
jederzeit neu ab und können sie löschen. Repositories werden dabei nicht
angefasst. Bei `pick` wählen Lernende auf der Karte eine der passenden Dateien
aus ihrem Workspace; die Liste wird beim Anklicken und beim Zurückkehren zur
Seite neu gelesen. `requires: { html_parses: true }` lehnt eine Abgabe ab,
deren HTML-Dateien nicht zusammenpassende Tags haben, und nennt die Zeile.
`requires: { scene_loads: true, min_objects: 5 }` lädt eine abgegebene Szene
mit dem Viewer der Person (wie bei `scene`) und lehnt sie ab, wenn der Viewer
Fehler meldet oder weniger Objekte findet. Läuft der Workspace nicht, nimmt der
Server die Abgabe an und sagt dazu, dass die Szene nicht geprüft wurde. Die Galerie (`gallery: true`) kommt
mit der Klassenübersicht.

`preview: anaglyph` (nur mit `pick: …/*.ini`) zeichnet Anaglyph-Szenen mit dem
Viewer aus `specht/anaglyph`, den der Server in der Version
`AnaglyphViewer::COMMIT` beim ersten Gebrauch von GitHub holt (nie als Kopie in
diesem Repository); Kenney-Kits lädt er bei Bedarf mit dem `download.rb` der
Vorlage. Die Karte zeigt die ausgewählte Datei und danach die Abgabe so, wie
die Galerie sie zeigt: aus der Zeile `camera = …` der Szene, immer bei
1280 × 800, damit das Bild überall gleich aussieht. Auf der Klassenseite steht
unter der Tabelle eine Galerie aller Abgaben; ein Klick zeigt eine Szene groß
im Viewer, drehbar, mit den Pfeiltasten zur nächsten.

Mit `preview: anaglyph` prüft die Karte `requires` selbst, im Browser, mit dem
Bild, das sie ohnehin zeichnet: Meldet der Viewer Fehler oder stehen weniger
als `min_objects` Objekte in der Szene, gibt sie nicht ab. Der Workspace muss
dafür nicht laufen. Der Server liest die Szene dann nicht noch einmal.

Die Ausdrücke einer Szene sind JavaScript. Darum läuft der Viewer nie auf der
Website selbst, sondern auf `abgaben.<host>` wie die Abgaben-Ansicht: in einem
Frame mit `sandbox`, mit einem eigenen, leeren Ursprung, ohne die Cookies der
Website. Die Seite holt sich für jede Szene eine signierte Adresse
(`/api/anaglyph/address`, mit denselben Rechten wie vorher), und der Viewer
schickt Bild, Fehler und die Zahl der Objekte nur als Nachricht zurück. So
erreicht eine Szene auch dann nichts, wenn eine Lehrkraft sie in der Galerie
ansieht. Dafür schickt der Viewer seine Nachrichten in einem leeren Ursprung an
`'*'` (seit `specht/anaglyph` 114bc3b); der Server ergänzt beim Ausliefern nur
einen Ersatz für `sessionStorage`, der in einem leeren Ursprung sonst fehlt.

### measure, command, scene

Beispiele stehen in `src/content/tcpip/tasks.yaml` und
`src/content/anaglyph/tasks.yaml`.

`measure` (seit M6): Lernende tragen einen Messwert ein, für jedes Feld einen.
Danach führt der Server den Befehl aus `run` selbst aus, im laufenden
Workspace der Person, als diese Person, und liest den Wert mit `extract` aus:
`ping_rtt_avg`, `ping_hops` (aus der TTL der Antwort), `ping_reply_country`
(aus der Endung des Rechners, der antwortet), `traceroute_hops`,
`http_header:<Name>` oder `dns_answers` (die Zeilen von `dig +short`, ohne
Punkt am Ende und bei `MX` ohne Priorität; richtig ist jede davon, darum
bleibt eine Aufgabe richtig, wenn sich eine Adresse ändert). Zahlen vergleicht er mit `tolerance` oder
`tolerance_percent`; Werte unter 1 gelten als gleich, wenn beide unter 1
liegen (Loopback). Texte vergleicht er ohne Groß- und Kleinschreibung,
`nginx` passt zu `nginx/1.24.0`, `Belgium` zu `Belgien`. Die Rückmeldung
nennt den eigenen Messwert: »8.8.8.8: Du hast 40 ms eingetragen, meine Messung
ergibt gerade 10,2 ms.« Der Befehl kommt immer aus der Aufgabe, nie von den
Lernenden. Im Prüfungsmodus gibt es kein Netz; deshalb ist `measure` dort
gesperrt.

`command` (seit M6): Lernende tippen einen Befehl. Er läuft in ihrem eigenen
Workspace in `/workspace`, mit `dir: bitstein` in `/workspace/bitstein`: dort,
wo das Terminal der Seite gerade steht, damit Lernende tippen, was sie dort
tippen würden. Nennt der Befehl den Ordner selbst (`bitstein/tatort.txt`,
`cd bitstein && …`), läuft er wie früher in `/workspace`. Das Zeitlimit kommt
aus `timeout` (höchstens 30 Sekunden). Er läuft nicht als die Person, sondern
als Gast (`nobody`, `HOME=/tmp`):
Er kann lesen und ins Netz, aber in `/workspace` nichts anlegen, ändern oder
löschen, und keine Prozesse der Person beenden. Ein `rm -rf ~` in der Karte
scheitert an fehlenden Rechten (Regel 1). Dateien, die nur für die Person
lesbar sind (etwa `~/.ssh`), sieht der Gast nicht. Die
Ausgabe (höchstens 64 kB) wird mit `output` verglichen: `lines`, `matching`,
`not_matching`; Rückmeldungen mit `when: { output_matches: … }`. Unter »Was
der Workspace gelesen hat« steht der Befehl mit seiner Ausgabe.

`scene` (seit M6): prüft eine Szene des Anaglyph-Viewers. Der Server liest
dafür nichts selbst aus: Er schickt ein kleines Skript (`scene_dump.js`) mit
`node` in den laufenden Workspace der Person, als diese Person. Es lädt den
Viewer, der neben der Szene liegt (`anaglyph/include/script.js`), liest die
Szene mit dessen eigenem Parser und rechnet aus, wo jedes Objekt steht, wie
groß es ist und wie es gedreht ist, zu den Zeitpunkten aus `at_times`
(Standard: nur `t = 0`). Gezeichnet wird nichts, und es ist genau der Viewer,
den die Person auch in der Vorschau sieht; Fehlermeldungen des Viewers
erscheinen wörtlich in der Rückmeldung. Ausdrücke wie `t * 30` laufen nur im
Workspace, mit Zeitlimit.

```yaml
check:
  type: scene
  file: anaglyph/sonnensystem.ini
  ignore: [grid]                # Formen, die nicht mitzählen
  at_times: [0, 1, 2, 3]
  background: black             # Farbfamilie des Hintergrunds
  expect:
    - { label: Sonne, shape: sphere, color: [yellow, orange], moves: false }
    - { label: Erde, shape: sphere, color: [blue, cyan], orbits: Sonne }
  source:
    contains: [group]           # Wörter, die im Text vorkommen müssen
```

Jeder Eintrag in `expect` wählt Objekte aus und verlangt etwas von ihnen.
Ausgewählt wird mit `shape`, mit `color` (eine Farbfamilie oder eine Liste
davon) und mit `name` für `name = …` in der Szene. `label` ist nur der Name in
den Rückmeldungen (»Ich finde keine Kugel in Gelb oder Orange für »Sonne««)
und das Ziel von `orbits`.

Farben statt Namen: Das Tutorial führt `name` nirgends ein, Farben dagegen
schon, und eine gelbe Sonne ist eine natürliche Aufgabe. `scene_dump.js`
verfolgt `fill` wie der Viewer, als Stift, der für alle folgenden Objekte gilt
und den eine Gruppe beim `end` zurücksetzt; `background` gilt für die ganze
Szene. Der Server ordnet jede Farbe einer Familie zu
(`src/ruby/include/scene_color.rb`): `white`, `grey`, `black`, `red`,
`orange`, `brown`, `yellow`, `green`, `cyan`, `blue`, `purple`, `pink`. So
zählt `gold`, `#f9c80e` und `1, 0.8, 0` gleichermaßen als gelb. Unter »Was der
Workspace gelesen hat« steht bei jedem Objekt, was geschrieben wurde und was
daraus wurde: »Farbe gold (#ffd700, Gelb)«.

| Feld | Verlangt |
| --- | --- |
| `count`, `min` | genau so viele, mindestens so viele |
| `at: [x, y, z]` | mit `count`: alle dort; sonst: eines dort |
| `size: [b, h, t]` | Größe nach dem Skalieren (Würfel 50, Kugel 100, …), 2 % Toleranz |
| `bottom_y` | Unterkante auf dieser Höhe (»liegt auf dem Gitter«) |
| `rotate_z: { spacing, modulo }` | Drehungen um z in gleichen Abständen |
| `lattice: [3, 3, 3]`, `min_spacing` | vollständiges Gitter, Mindestabstand |
| `rows` | so viele gleich lange Reihen, keine zwei Objekte an derselben Stelle |
| `moves: true/false` | bewegt sich zwischen den Zeitpunkten oder nicht |
| `orbits: <label>` | Abstand zu dem Objekt, das der Eintrag mit diesem `label` findet (oder zum Objekt mit `name = …`), bleibt gleich, die Richtung ändert sich |

Positionen haben eine Toleranz von 2 Einheiten. Unter »Was der Workspace gelesen
hat« steht jedes Objekt mit Zeile, Position und Größe. Die Karte nennt Datei
und Viewer: »Liest `/workspace/anaglyph/balken.ini` mit deinem Viewer
`/workspace/anaglyph/include/script.js`«.

### python, ruby

Ruft Funktionen aus einer Python- oder Ruby-Datei der Lernenden auf und
vergleicht, was herauskommt. Ein festes Skript (`PythonCheck::HARNESS`) lädt die Datei mit
`python3` im Workspace der Person, als Gast wie bei `command` (der Code ist
ihrer, Regel 1), höchstens `timeout` Sekunden (Standard 10, höchstens 30).
Was das Programm selbst ausgibt, verschwindet; Aufrufe in der Datei selbst
stören also nicht.

```yaml
check:
  type: python
  file: automata/lock.py
  cases:
    - opens('1213'): true                 # Aufruf: erwartetes Ergebnis
    - opens('121213'): true
    - call: run(['münze']) == 'offen'      # die lange Form, mit eigenem Satz
      expect: false
      say: Nach einer Münze darf das Drehkreuz noch nicht offen sein.
    - call: run(['tritt'])                 # muss mit einem Fehler abbrechen
      raises: true
```

Aufrufe und Ergebnisse sind Python-Ausdrücke; `true`, `false`, `~` und Zahlen
aus YAML werden zu `True`, `False`, `None` und der Zahl. Ein Text als Ergebnis
braucht darum zwei Paar Anführungszeichen: `"'offen'"`. Prüft eine Aufgabe
Verhalten statt Namen (»wie der neue Zustand heißt, ist egal«), vergleicht der
Aufruf selbst: `run(['münze']) == 'offen'` mit `false`.

Mit `raises: true` statt `expect` muss der Aufruf mit einem Fehler abbrechen
(in Python eine `Exception`, in Ruby ein `StandardError`), etwa ein Rechner bei
einer Klammer, die nie zugeht; welcher Fehler, ist egal. Bricht er nicht ab,
heißt es »… ergibt `5`, richtig wäre ein Fehler.«

Die Rückmeldung nennt den ersten Aufruf, der nicht passt: »`opens('121213')`
ergibt `False`, richtig wäre `True`.«, dazu `say`, wenn es eines gibt. Bricht
ein Aufruf ab, steht der Fehler mit der Zeile in der Datei da, ebenso ein
Syntaxfehler oder ein Fehler beim Laden. Unter »Was der Workspace gelesen
hat« stehen alle Aufrufe mit Haken oder Kreuz und ihrem Ergebnis. Die Datei
wird aus ihrem Ordner geladen, `import` anderer Dateien daneben klappt, und
was jemand mit `pip install` oder `gem install` installiert hat, ist da.

`ruby` funktioniert genauso, mit einer `.rb`-Datei und Ausdrücken in Ruby
(`RubyCheck::HARNESS`). Von der Datei
werden nur die Definitionen geladen: `def`, `class`, `module`, Konstanten,
globale und Instanzvariablen, `require` und `include`. Was sonst auf oberster
Ebene steht, das Programm zum Ausprobieren, läuft dabei nicht; ein Block
`if __FILE__ == $0` ist dafür nicht nötig. Bei `python` ebenso: Importe,
Funktionen, Klassen, Konstanten in Großbuchstaben und die Variablen, die sie
oder die Aufrufe der Prüfung brauchen; ohne `if __name__ == '__main__':`.
Nötig bleibt so ein Block nur, wenn ein anderes Programm die Datei lädt (in
»Endliche Automaten« lädt `convert` das Schloss). Verglichen wird mit
`==`; `true`, `false` und `~` aus YAML werden zu `true`, `false` und `nil`.
Bytes, die mit `pack('C*')` entstehen, sind gleich einem Text in
Anführungszeichen, solange er nur ASCII enthält; für Umlaute vergleicht man
nach `.force_encoding('UTF-8')`.

```yaml
check:
  type: ruby
  file: lzw/lzw.rb
  cases:
    - compress('ABABABA'): '[65, 66, 256, 258]'
    - call: compress('')
      expect: '[]'
      say: Ein leerer Text ergibt keinen einzigen Code.
```

### function

Eine Kata (docs/plan-katas.md): Der Code steht nicht in einer Datei im
Workspace, sondern wird in der Karte geschrieben, in Ruby, Python oder
JavaScript, mit
den Farben der Codeblöcke, Einrückung mit <kbd>Tab</kbd> und
<kbd>Strg</kbd>+<kbd>Enter</kbd> zum Prüfen. **Ausführen** startet ihn einmal
und zeigt, was er ausgibt; **Prüfen** ruft die Funktionen auf und vergleicht,
mit demselben Skript wie `python` und `ruby` und denselben `cases`. Die
Rückmeldung sagt »dein Code« statt eines Dateinamens.

```yaml
- id: ggt
  title: Der größte gemeinsame Teiler
  level: 3
  prompt: |
    Schreib eine Funktion `gcd(a, b)`, die den größten gemeinsamen Teiler
    zweier Zahlen zurückgibt.
  starter: |
    def gcd(a, b)
    end
  check:
    type: function
    language: ruby                # ruby, python oder javascript
    cases:
      - gcd(60, 48): 12
      - call: gcd(7, 7)
        expect: 7
        say: Der größte Teiler einer Zahl mit sich selbst ist sie selbst.
  python:
    starter: |
      def gcd(a, b):
          pass
    check:
      type: function
      language: python
      cases:
        - gcd(60, 48): 12
```

**Was eine Kata leicht machen würde**, schaltet `forbid` ab. In Ruby
`Klasse#methode` oder `Klasse.methode` (`Integer#gcd`, `Array#sort`,
`Math.sqrt`): Ruft der Code der Lernenden sie auf, bricht der Aufruf ab, und
die Rückmeldung sagt »`gcd(60, 48)` (Zeile 2) benutzt `Integer#gcd`. Das gibt
es in dieser Aufgabe nicht: Du schreibst es gerade selbst.« Die Prüfung
selbst darf sie weiter benutzen. In Python eine eingebaute Funktion
(`sorted`), eine Funktion eines Moduls (`math.gcd`), ein eingebauter Typ
(`str`, `list`) oder eine Methode eines solchen (`list.sort`, `str.upper`);
Typen und ihre Methoden lassen sich in Python nicht abschalten, darum genügt
es, wenn der Name im Code vorkommt. In JavaScript eine Funktion oder Methode
mit ihrem Pfad (`Math.max`, `Array.prototype.sort`, `globalThis.String`),
wie in Ruby nur für den Code der Lernenden.

**JavaScript gibt es nur für Katas** (die Einstellung »Sprache« einer Klasse
bleibt bei Ruby und Python): Eine Seite mit
`languages: [ruby, python, javascript]` bietet es im Umschalter an, und wer
dort JavaScript wählt, behält auf anderen Seiten Ruby oder Python
(`u.kata_code_language` neben `u.code_language`). Der Code läuft mit `node`
in einem eigenen Kontext (`JavascriptCheck`); verglichen wird mit
`util.isDeepStrictEqual`, Listen und Objekte also nach ihrem Inhalt. Namen
aus mehreren Wörtern schreibt man in JavaScript zusammen (`maxOfTwo`), die
Varianten der Aufgabe nennen ihre Funktionen so.

**Wo es auf die Geschwindigkeit ankommt** (`timeout` klein, eine große
Eingabe), sagt `slow:`, was die Rückmeldung nach Ablauf der Zeit stattdessen
sagt, statt nach einer Endlosschleife zu fragen.

**Wo eine Rekursion verlangt ist**, sagt `recursive: true`, dass die Funktion
sich selbst aufrufen muss, und `loops: false`, dass sie keine Schleife
benutzen darf. Eine Schleife liefert dieselben Ergebnisse wie eine Rekursion,
die Fälle können sie nicht unterscheiden; darum liest `RecursionCheck` den
Code, sobald die Fälle stimmen: Ruby mit dem Parser Prism (`while`, `until`,
`for`, ein Aufruf mit Block), Python und JavaScript als Text ohne Strings und
Kommentare (`for`, `while`, in JavaScript auch `do` und `forEach`, `map` …).
Die Funktion ist die des ersten Falls. Stimmt es nicht, sagt die Rückmeldung,
was fehlt, und nennt die Schleife.

**Fälle für jede Person** erzeugt `random`: `count` Aufrufe (höchstens 20)
mit Werten aus `args` (ein Bereich `1..1000` oder eine Liste von Werten),
für jede Person und jede Fassung der Aufgabe andere, bei einer zweiten
Prüfung dieselben. Was herauskommen muss, rechnet `reference` im Lauf aus,
in der Sprache der Aufgabe; die Rückmeldung nennt den Wert, nie die
Referenz. So besteht keine Tabelle der festen Fälle.

```yaml
  check:
    type: function
    language: ruby
    forbid: [Integer#gcd, Integer#lcm]
    cases:
      - gcd(60, 48): 12
    random:
      count: 4
      call: gcd(a, b)
      args: { a: 1..1000, b: 1..1000 }
      reference: a.gcd(b)          # in Python: math.gcd(a, b)
  feedback:
    - when: { same_as: "def gcd(a, b) = [a, b].min" }
      say: Die kleinere Zahl ist nur manchmal ein Teiler der größeren.
```

**Typische Fehler** erkennt `feedback` mit `same_as`, dem Gegenstück zu
`same_result_as` bei SQL: eine falsche Lösung, die die Autorin schreibt und
niemand zu sehen bekommt. Sie läuft im selben Container mit denselben
Fällen; kommt bei den Lernenden bei jedem Fall dasselbe heraus wie bei ihr,
ersetzt ihr `say` die Rückmeldung. Eine für jeden Fehler, den Klassen
machen.

Der Code läuft nie im Workspace und nie auf dem Server, sondern jedes Mal in
einem neuen Container aus dem Workspace-Image (dieselben Interpreter), ohne
Netz, schreibgeschützt bis auf ein kleines `/tmp`, als `nobody`, höchstens
`timeout` Sekunden (`KataRunner`, entwicklung.md, »Katas: Läufe im eigenen
Container«). Alle Läufe stehen in einer Warteschlange; solange ein Lauf
wartet, zeigt die Karte, wie viele vor ihm sind. Gespeichert wird der Code
als Antwort, für jede Sprache getrennt, so dass Ruby und Python ihren eigenen
behalten; Lehrkräfte sehen im Klassenboard nur das Ergebnis, nicht den Code.
Was ein Lauf ausgibt, wird nicht gespeichert.

In `task-examples.yaml` steht der Code unter `answer:`; der `starter` jeder
Sprache muss von selbst scheitern und wird automatisch mitgeprüft:

```yaml
examples:
  ggt:
    pass:
      - answer: "def gcd(a, b)\n  b.zero? ? a : gcd(b, a % b)\nend\n"
    fail:
      - answer: "def gcd(a, b) = [a, b].min\n"
        say: "`gcd(60, 48)` ergibt `48`"
    python:
      pass:
        - answer: "def gcd(a, b):\n    while b:\n        a, b = b, a % b\n    return a\n"
```

### pip

Pip unter Funkelbach (plan-katas.md, Abschnitt 2): Die Karte enthält einen
Plan, das ganze Programm ohne Klasse und ohne `import`, und schickt Pip damit
über eine Karte der Stollen. Ein unsichtbarer Vorspann macht die Befehle des
Kapitels zu gewöhnlichen Funktionen (Ruby `move`, `on_stone?`; Python
`move()`, `on_stone()`; JavaScript `move()`, `onStone()`). Jeder Befehl geht
als Zeile an die Welt auf dem Server (`PipWorld`), die ihn ausführt und
antwortet; der Plan kann sie nur über diese Befehle ändern. Die Welt
schreibt jeden Schritt auf, die Prüfung urteilt danach, und die Karte spielt
den Lauf ab, mit der Zeile, bei der Pip gerade ist.

```yaml
check:
  type: pip
  language: ruby
  commands: [move, turn_left, take, on_stone]   # was das Kapitel kennt; say geht immer
  maps:                                         # jede Karte: der Plan von vorn
    - facing: east
      map: |
        #######
        #P.o.L#
        #######
    - generate: maze                            # ein Labyrinth aus dem Seed der Person
      size: [6, 5]                              # Zellen quer und hoch
  goal: { lamp: true, stones: all }             # außerdem gate: open, bello: true, say: stones
  max_lines: 7                                  # nur bei Bedarf: höchstens so viele Zeilen
  dry: true                                     # nur bei Bedarf: eine Pfütze beendet den Lauf
  buttons: [move, turn_left]                    # nur im ersten Kapitel: der Plan wird geklickt
```

Die Zeichen der Karte: `#` Wand, `.` Boden, `P` Pip (Blickrichtung in
`facing`), `o` Funkenstein, `L` Lampe, `G`/`g` Glibber, der waagerecht/senkrecht
hin und her wabert, `H` Hebel, `T` Stacheln im Boden, `p` Pfotenabdruck, `~` Pfütze, `B`
Bello. Die Befehle: `move`, `turn_left`, `take`, `wait`, `pull` (je ein
Schritt), `say` (kein Schritt), die Fragen `on_stone`, `on_lever`,
`front_clear`, `glibber_ahead`, `on_lamp`, `trail_ahead` (Pfotenabdrücke
vorn, auch Bello selbst), `puddle_ahead`, `bello_here`. Nach jedem Schritt
geht jeder Glibber ein Feld weiter. Erreicht Pip die Lampe oder Bello, ist
der Lauf zu Ende, auch mitten in einer Schleife. Mehrere Karten stehen als
Reiter über der Karte, jeder mit dem Aufruf, den die Prüfung dort macht
(»Karte 1 `walk(3)`«); »Ausführen« läuft zuerst auf der Karte des Reiters,
und solange ein Lauf unterwegs ist oder abgespielt wird, wartet der Reiter. Eine Wand,
Stacheln, die aus dem Boden ragen (drei Züge versenken sie, ein vierter lässt sie wieder herausfahren; im Spec heißen sie `gate`), `take` ohne Stein, `pull`
ohne Hebel, ein Glibber, mit `dry: true` eine Pfütze und mehr als 1000
Schritte beenden den Lauf mit diesem Ereignis; die Rückmeldung nennt die Karte und die Zeile (»In Karte
2 in Zeile 6 läuft Pip gegen die Wand.«) und spielt diesen Lauf ab. Wo es um
eine allgemeine Lösung geht, prüfen mehrere Karten (höchstens sechs) mit
verschiedenen Steinen oder Glibbern: Eine feste Folge von Schritten soll an
mindestens einer scheitern. Eine Karte mit `generate` entsteht aus dem Seed
der Person und der Aufgabenversion (`PipMaps`, bisher `maze`: ein Labyrinth
mit genau einem Weg zwischen zwei Zellen, die Lampe am weitesten weg); die
erste Karte steht immer ausgeschrieben da, die Karte zeigt sie. `say: stones`
heißt: Das Letzte, was Pip sagt, ist die Zahl der aufgehobenen Steine.

Eigene Funktionen prüft eine Karte mit `call` und `expect`: Der Plan wird
geladen, wobei seine eigenen Befehle nichts bewirken, dann läuft der Aufruf,
und `expect` nennt, wohin Pip danach schaut und wie viele Felder Pip gegangen
ist (»Nach `turn_right` schaut Pip nach oben statt nach unten.«). `call` ist
in jeder Sprache anders geschrieben (`turn_right`, `turn_right()`,
`turnRight();`); `goal` kann dann fehlen.

Mit `buttons` wird der Plan nicht getippt, sondern geklickt (»Der Eingang«
in Teil 1): Unter der Karte steht für jeden genannten Befehl ein Knopf
(`move`, `turn_left`, `take`) und »Rückgängig«, der Plan daneben lässt sich nur
ansehen. Jeder Klick bewegt Pip sofort, im Browser (`PipButtons` in
pip_world.js), und schreibt seine Zeile in den Plan; ein Schritt gegen die
Wand oder `take` ohne Stein schreibt nichts. Erreicht Pip die Lampe, geht der
Plan von selbst an die gewöhnliche Prüfung; einen Knopf »Ausführen« gibt es
dann nicht. Weil der Browser die Welt nachspielt, hat eine solche Aufgabe genau
eine Karte aus `#`, `.`, `o`, `L` und `P`, ohne `call` und ohne `dry`.

```yaml
maps:
  - facing: east
    call: "turn_right()"
    expect: { facing: south, moves: 0 }
    map: |
      #####
      #.P.#
      #####
```

In `prompt`, `starter`, `hint` und `solution` schreibt jede Sprache die
Befehle auf ihre Art; in `task-examples.yaml` steht der Plan unter `answer:`
wie bei `function`. Eine Karte mitten auf der Seite, etwa für eine Vorhersage,
ist `<div class='pip-world pip-static'><pre class='no-copy'>…</pre></div>`
mit der Karte im selben Format; Ziffern markieren Felder, über die die
Aufgabe spricht. Was nach einer Kata erzählt wird, steht zwischen
`<!-- after: id -->` und `<!-- /after -->` und erscheint, sobald die Kata
gelöst ist.

### tests

Eine Kata, in der man Tests schreibt statt einer Funktion (`TestsCheck`,
Übung »Tests schreiben«). Die Tests sind Zeilen
`assert_equal(erwartet, ergebnis)` (in JavaScript `assertEqual`), die die
Prüfung selbst bereitstellt, ohne Testbibliothek. Sie laufen gegen eine
richtige Fassung der Funktion, bei der alle bestehen müssen, und gegen
fehlerhafte, von denen jede an mindestens einem Test scheitern muss; jede
Fassung läuft für sich, nacheinander im selben Container der Katas.

```yaml
check:
  type: tests
  language: ruby                 # ruby, python oder javascript; je Sprache als Variante
  function: even                 # was die Tests aufrufen; sie dürfen es nicht selbst schreiben
  correct: |
    def even(n)
      n % 2 == 0
    end
  mutants:                       # 1 bis 10
    - code: |
        def even(n)
          n > 0 && n % 2 == 0
        end
      say: Eine Fassung, die bei 0 und negativen Zahlen immer `false` sagt, besteht deine Tests.
```

Scheitert ein Test an der richtigen Fassung, nennt die Rückmeldung die Zeile
und was sie wirklich ergibt; kommt eine fehlerhafte durch, ersetzt ihr `say`
die Rückmeldung. In Python ist `True` dabei nicht `1`. Schreibt jemand die
Funktion selbst hinein, sagt die Prüfung, nur Tests zu schreiben, ohne etwas
auszuführen. Der `starter` (ein erster Test) muss von selbst scheitern, wie
bei `function`; in `task-examples.yaml` stehen Tests unter `answer:`.

### c

Ruft Funktionen aus einer C-Datei der Lernenden auf, wie `python` und `ruby`
(`CCheck`). Ein Skript schreibt im Workspace, als Gast, ein Testprogramm nach
`/tmp`, das die Datei mit `#include` einbindet, ihr `main` umbenennt und
stattdessen die Aufrufe der Prüfung ausführt, und übersetzt es mit `cc`. Das
Programm selbst läuft also nicht, und ein Fenster von PixelRAM geht nicht auf.
`pixelram.h` kommt aus dem Ordner `.pixelram`, den `make` neben der Datei
anlegt; fehlt er, sagt die Prüfung, dass einmal `make` laufen muss. Funktionen
einer Bibliothek, die die Aufrufe nicht brauchen (`screen_open`,
`set_pixel` …), lässt der Linker weg.

`raylib.h`, `raymath.h` und `rlgl.h` kommen aus dem Sysroot von Emscripten im
Workspace (`CCheck::RAYLIB_INCLUDE`); die Funktionen von raymath werden dabei
`static inline`, damit sie ohne die Bibliothek laufen. Braucht eine Funktion
der Lernenden etwas von raylib selbst, eine Taste oder den Zufall, schreibt
`setup` eine kleine Ersatzfunktion mit demselben Namen
(`bool IsKeyDown(int key) { return key == hs_key; }`, die Klötzchenwelt).

```yaml
check:
  type: c
  file: raytracer/raytracer.c
  tolerance: 0.0001                       # für Kommazahlen; Standard 1e-6
  setup: |                                # C-Code vor den Aufrufen, für Hilfsfunktionen
    double floor_color(double x, double z)
    {
        hit h;
        nearest_hit((vec){x, 1, z}, (vec){0, -1, 0}, &h);
        return h.mat.color.x;
    }
  cases:
    - hit_floor((vec){0, 1.2, 5.5}, (vec){0, -0.6, -0.8}): 2
    - reflect((vec){0.6, -0.8, 0}, (vec){0, 1, 0}).y: 0.8   # ein Feld eines struct
    - call: hit_floor((vec){0, 1, 0}, (vec){1, 0, 0})
      expect: INFINITY
      say: Ein waagrechter Strahl trifft den Boden nie.
    - floor_color(0.5, 4.5) != floor_color(1.5, 4.5): true
```

Aufrufe und Ergebnisse sind C-Ausdrücke. Ein Aufruf muss eine Zahl
(`int`, `double`, `bool`, `char` …) oder einen Text (`char *`) ergeben; gibt
eine Funktion ein `struct` zurück, nennt der Aufruf ein Feld (`.y`). Globale
Variablen lassen sich mit dem Komma-Operator setzen:
`(player_angle = 0, update_camera(), plane_y)`. `true`, `false`, `~` und `.inf`
aus YAML werden zu `true`, `false`, `NULL` und `INFINITY`. Zahlen werden als
`double` verglichen, gleich bis auf `tolerance`; unendlich ist gleich
unendlich mit demselben Vorzeichen. Texte vergleicht `strcmp`. Ein Vergleich
in C ist ein `int`; mit `true` oder `false` als Ergebnis zeigt die Rückmeldung
ihn als `true` oder `false`. `raises` gibt es in C nicht.

Jeder Aufruf läuft in einem eigenen Prozess, höchstens zwei Sekunden: Ein
Absturz (»greift auf Speicher zu, der ihm nicht gehört«), eine Endlosschleife
oder ein `exit` trifft nur diesen Aufruf. Was das Programm ausgibt,
verschwindet. Lässt sich die Datei nicht übersetzen, nennt die Rückmeldung die
Zeile und die Meldung des Compilers; fehlt eine Funktion, heißt es »In
`raytracer.c` gibt es `reflect` noch nicht«, passt ein Aufruf nicht zu ihr
(andere Parameter), nennt sie den Aufruf. `setup` ist für Hilfsfunktionen, die
mehrere Aufrufe lesbar machen; was darin nicht zum Programm passt, meldet die
Prüfung als »Die Prüfung passt noch nicht zu deinem Programm«.

### sql

Seit M6. Beispiele stehen in `src/content/mysql/tasks.yaml` (»Einführung in
SQL«, mit den Ländern der Welt aus `country-archive/countries.sql`).

```yaml
check:
  type: sql
  expect_query: SELECT name, population, area FROM country ORDER BY population DESC
  sorted_by: 2 DESC            # Reihenfolge zählt, Gleichstände egal
  tables: { country: 197 }     # die Beispieldaten müssen so da sein
  setup: Lade zuerst die Beispieldaten, wie unter »Beispieldaten laden« beschrieben.
feedback:
  - when: { same_result_as: "SELECT name, population FROM country ORDER BY population DESC" }
    say: Es fehlt noch die Fläche.
  - when: { query_matches: 'order\s+by\s+population\s*;?\s*$' }
    say: Sortiert ist es, aber aufsteigend.
```

Lernende schreiben eine Abfrage in ein Textfeld (<kbd>Strg</kbd>+<kbd>Enter</kbd>
prüft). Der Server führt sie in **ihrer eigenen** MySQL-Datenbank aus,
angemeldet als sie selbst, nie als `root`, und dort auch die Musterabfrage aus
`expect_query`. Verglichen werden die Ergebnisse, nicht der Text der Abfrage:
`COUNT(name)` statt `COUNT(*)`, andere Spaltennamen, eine andere Reihenfolge
der Spalten oder eine Unterabfrage statt `ORDER BY … LIMIT 1` zählen genauso.

Nur lesen (Regel 1):

- Angenommen wird genau eine Abfrage, die mit `SELECT` oder `WITH` beginnt.
  Alles andere (`DELETE`, `DROP`, zwei Befehle mit `;`, `INTO OUTFILE`,
  ausführbare Kommentare `/*! … */`) lehnt der Server ab, ohne es zu senden.
- Sie läuft in `START TRANSACTION READ ONLY` mit `max_execution_time` von
  5 Sekunden; danach wird die Verbindung geschlossen, MySQL rollt zurück.
- Höchstens 5000 Zeilen werden gelesen.

| Feld | Bedeutung |
| --- | --- |
| `expect_query` | Musterabfrage, oder eine Liste davon; eine passende reicht |
| `compare` | `same` (Standard): dieselben Zeilen. `subset`: jede Zeile ist eine der Musterzeilen, keine doppelt (»Finde drei Orte …«), mit `rows` für die Anzahl |
| `sorted_by` | Spalten des Musterergebnisses, nach denen sortiert sein muss, z. B. `2 DESC` oder `1, 3`. Die Richtung kommt aus der Musterabfrage; gleiche Werte dürfen in beliebiger Reihenfolge stehen. Ohne das Feld ist die Reihenfolge egal |
| `extra_columns` | Mehr Spalten als nötig: `wrong` (Standard: nur die gefragten Spalten), `note` (richtig mit Hinweis) oder `ok`. Wer mehrere Spaltenauswahlen gelten lassen will, gibt mehrere `expect_query` an |
| `tables` | Tabellen und ihre Zeilenzahl in den Beispieldaten (`~` für »egal«). Fehlt eine oder stimmt die Zahl nicht, wird nicht geprüft: Sonst wäre nach einem `DELETE` eine leere Antwort richtig |
| `setup` | Satz, wie man die Beispieldaten lädt; steht bei fehlenden Daten dabei |

Rückmeldungen: `same_result_as` (das Ergebnis gleicht dem einer typischen
falschen Abfrage) und `query_matches` (regulärer Ausdruck auf den Text, ohne
Groß- und Kleinschreibung; greift auch bei Fehlermeldungen von MySQL, etwa
`6,046` statt `6.046`). Sonst sagt die Karte, was nicht passt: zu viele oder
zu wenige Zeilen oder Spalten, eine Zeile, die nicht dazugehört (aus dem
eigenen Ergebnis), die Reihenfolge. Fehlermeldungen von MySQL erscheinen
wörtlich, mit einem Satz dazu für die häufigen (Syntax, unbekannte Tabelle
oder Spalte, `GROUP BY`). Unter der Rückmeldung steht immer das eigene
Ergebnis als Tabelle, die ersten 10 Zeilen, so wie MySQL es liefert;
Werte aus der Datenbank werden dabei maskiert. Lehrkräfte sehen die Abfragen
in der Klassenübersicht.

Kann die Karte nicht prüfen, sagt sie genau, woran es liegt, und klappt das
Nachgesehene auf: welche Datenbank auf welchem Server mit welcher Anmeldung,
die Meldung von MySQL (Anmeldung abgelehnt, Datenbank unbekannt, Server nicht
erreichbar), welche Tabellen es gibt und wie viele Zeilen die geforderten
haben, ob die Tabelle in einer anderen eigenen Datenbank liegt, ob der
Workspace läuft und welche Datenbank `/workspace/.my.cnf` nennt (ohne
Passwort). Die Prüfung selbst braucht keinen laufenden Workspace.

### cypher

Seit M7. Beispiele stehen in `src/content/neo4j-filmdatenbank/tasks.yaml`
(»Filmdatenbank als Graph mit Neo4j«, Daten aus `movies-neo4j.dump`).

```yaml
check:
  type: cypher
  expect_query: "MATCH (:Person {name: 'Christopher Nolan'})-[:DIRECTOR]->(m:Movie) RETURN m.title, m.year ORDER BY m.year"
  sorted_by: 2
  nodes: { Movie: 2735, Person: 14445, Genre: 20 }
  setup: Lade zuerst die Filmdaten, wie unter »Die Filmdaten laden« beschrieben.
feedback:
  - when: { same_result_as: "MATCH (:Person {name: 'Christopher Nolan'})-[]->(m:Movie) RETURN DISTINCT m.title, m.year" }
    say: Das sind alle Filme, an denen er beteiligt war. Gefragt ist nur `DIRECTOR`.
```

Wie `sql`, nur für Neo4j: Lernende schreiben eine Abfrage in ein Textfeld,
der Server führt sie in **ihrer eigenen** Neo4j-Datenbank aus, angemeldet als
sie selbst, nie als Administrator, und dort auch die Musterabfrage. Verglichen
werden die Ergebnisse; die Felder `expect_query`, `compare`, `rows`,
`sorted_by`, `extra_columns` und die Rückmeldungen `same_result_as` und
`query_matches` bedeuten dasselbe wie bei `sql`. Statt `tables` gibt es
`nodes`: Labels und die Zahl ihrer Knoten in den Beispieldaten. Ein
`same_result_as` vergleicht nur die Zeilen, auch wenn die Aufgabe eine
Reihenfolge verlangt.

Nur lesen (Regel 1), doppelt:

- Bevor etwas gesendet wird, lehnt der Server Abfragen mit einer Klausel ab,
  die schreibt oder über die Datenbank hinaus greift: `CREATE`, `MERGE`,
  `SET`, `DELETE`, `REMOVE`, `DROP`, `FOREACH`, `LOAD CSV`, `CALL`, `USE` und
  die Verwaltungsbefehle, außerhalb von Zeichenketten und Kommentaren. Eine
  Abfrage braucht ein `RETURN`; `;` zwischen zwei Abfragen gibt es nicht.
- Die Abfrage läuft in einer Transaktion im Lesemodus (`READ`), den Neo4j
  selbst durchsetzt, mit 5 Sekunden Zeitlimit; höchstens 5000 Zeilen werden
  gelesen.

Knoten, Beziehungen und Wege erscheinen im Ergebnis so, wie `cypher-shell`
sie zeigt, Listen als `[…]`. Wer Eigenschaften vergleicht, fragt sie in der
Aufgabe ausdrücklich ab (»Zeig Titel und Jahr«).

### er

Seit M7. Beispiel: `src/content/filmdatenbank/tasks.yaml` (»Filmdatenbank mit MySQL«).
Lernende schreiben ihr Modell als Text in eine `.er`-Datei; die
ER-Werkstatt in VS Code zeigt es als Diagramm und erzeugt das SQL
(`src/vscode-extensions/er/README.md`). Die Prüfung liest die Datei mit dem
Kern der ER-Werkstatt (`core/er-core.js`), derselben Datei, die auch die
Erweiterung benutzt; er läuft mit `node` im Workspace der Person. Ein Modell
bedeutet so überall dasselbe.

```yaml
check:
  type: er
  file: filmdatenbank/film.er
  entities:
    - name: movie
      key: [id]
      attributes: [title, "year: int, optional"]   # wie in der .er-Datei
  relationships:
    - between: [movie, genre]
      cardinality: n:m             # in der Reihenfolge von between
    - between: [movie, crew, job]
      table: movie_crew            # verlangt `as movie_crew`
  tables:                          # was aus dem Modell wird, wie bei schema
    - name: movie_genre
      primary_key: [movie_id, genre_id]
      foreign_keys: ["movie_id -> movie.id", "genre_id -> genre"]
```

Was nicht genannt ist, ist frei: weitere Entitäten, Attribute und Beziehungen
stören nicht. Ein Attribut wird so geschrieben wie im Modell; Typ und
Markierungen (`primary key`, `optional`, `unique`) zählen nur, wenn sie
dastehen; `key` in der Aufgabe und das ältere `key` im Modell meinen dasselbe
wie `primary key`. `key: [id]` unter einer Entität nennt die Attribute ihres
Primärschlüssels.
Eine Beziehung wird an ihren Entitäten erkannt, nicht am Namen; `1:n`
zwischen `[genre, movie]` ist dasselbe wie `n:1` zwischen `[movie, genre]`,
`m` und `n` bedeuten beide »viele«. Fehler im Modell kommen zuerst, mit
Zeilennummer, so wie die Erweiterung sie unterstreicht.

Unter der Rückmeldung steht, was im Modell steht: Entitäten mit ihren
Attributen (Schlüssel fett), Beziehungen und die Tabellen, die daraus werden.

### keyboard

```yaml
check:
  type: keyboard
  section: Text markieren
```

Das Keyboard-Tutorial (die Erweiterung in `src/vscode-extensions/keyboard-tutorial`)
hakt jeden erledigten Schritt in `~/.hs-kbd-tutorial/.state.json` ab. Die
Aufgabe ist gelöst, wenn alle Schritte des Kapitels `section` abgehakt sind;
welche das sind, steht in der `tutorial/sections.yaml` der Erweiterung, aus
der sie auch gebaut wird. Die Rückmeldung nennt die Schritte, die noch
fehlen. Der Server liest nur die Datei, der Workspace muss nicht laufen; die
Klassenübersicht kann das Kapitel deshalb auch für alle auf einmal prüfen.

### story

```yaml
check:
  type: story
  project: bif
  expect:
    pages: 5            # mindestens so viele Seiten
    choices: 4          # Entscheidungen, die zu einer anderen Seite führen
    local_choices: 1    # Entscheidungen mit (.)
    conditions: 1       # condition="…" an Text oder Entscheidungen
    scripts: 1          # <script>-Blöcke
    images: 1
    endings: 1          # erreichbare Seiten ohne Entscheidung zu einer anderen
    all_reachable: true # keine Seite, die man von 1.md aus nicht erreicht
    no_errors: true     # BIF meldet keine Fehler (Warnungen zählen nicht)
    own_story: true     # config.js wählt einen anderen Ordner als pages-starter
```

Für [BIF](https://github.com/specht/bif). Die Erweiterung »BIF Authoring
Tools« im Workspace analysiert die Geschichte bei jedem Speichern und schreibt
das Ergebnis nach `<projekt>/.story-tools/analysis.json`; daraus zeichnet BIF
den Graphen und die Problems-Liste. Der Server liest nur diese Datei und führt
nichts aus. Ob sie zur Geschichte passt, sieht er an den SHA-256-Summen darin
(`inputManifest`): Hat jemand eine Seite geändert, ohne dass der Workspace lief,
heißt es »Öffne deinen Workspace und speichere eine Seite« (`problem: stale`).
Die Rückmeldung nennt Datei und Zeile; die häufigen Meldungen von BIF (fehlende
Seite, unerreichbare Seite, Fehler in Skript, Bedingung oder Ausdruck,
fehlendes Bild) stehen auf Deutsch da, alle anderen so, wie BIF sie schreibt.
Die Datei schreibt der Watcher von `npm run dev`, den die Erweiterung startet
(`schemaVersion: 2`): `nodes` sind die Seiten mit ihren `counts`, `edges` die
Entscheidungen zu anderen Seiten. Skripte, Bedingungen und Bilder stehen nicht
in `summary`; der Server zählt sie aus den `counts` der Seiten zusammen.
Die Klassenübersicht kann `story` für alle auf einmal prüfen.

### schema

Seit M7. Liest den Aufbau der eigenen Datenbank aus `information_schema`,
angemeldet als die Person, nur lesend, wie `sql`. Zeilen zählt die Prüfung
nur für Tabellen mit `rows` oder `min_rows`.

```yaml
check:
  type: schema
  tables:
    - name: movie
      columns: [id, title, year]   # müssen da sein; weitere sind erlaubt
      primary_key: [id]
      min_rows: 1                  # oder rows: genau so viele
    - name: movie_genre
      foreign_keys: ["movie_id -> movie.id", "genre_id -> genre"]
      indexes: [[genre_id]]        # ein Index, der mit diesen Spalten anfängt
  setup: Lege die Tabellen mit `mysql < film.sql` an.
```

`indexes` verlangt für jede Liste einen Index der Tabelle, dessen erste
Spalten genau diese sind; der Primärschlüssel zählt mit, und ein Index auf
`(last_name, first_name)` genügt auch für `[last_name]`.

Ein Fremdschlüssel ohne Spalte nach dem Punkt zeigt auf den Primärschlüssel
der anderen Tabelle. `setup` steht dabei, wenn Tabellen fehlen. Unter der
Rückmeldung stehen alle Tabellen der Datenbank mit Spalten und Typen,
Primärschlüssel fett, Fremdschlüssel und, wo gezählt, die Zeilenzahl. Ist die
Datenbank nicht erreichbar, sagt die Karte dasselbe wie bei `sql`.

## Beispiel: Begleiter für eine Sandbox

Die Sandboxes bleiben eigenständig. Eine kleine Aktivität im Workspace kann
trotzdem begleiten, besonders wenn neue Lernende Git noch nicht kennen: Sie
liest nur, was im eigenen Workspace liegt, und sagt, was als Nächstes fehlt.
Fertige Seiten gibt es für alle drei Sandboxes: `sandbox-datenstrukturen`,
`sandbox-sortieren` und `sandbox-cpu` unter `src/content/`. Was alle drei
brauchen (SSH-Schlüssel, `git config`, abgelehnter Push), steht einmal im
Handbuch unter `reference/gitlab` und wird mit `_include()` eingebunden. Hier
der Kern der Aufgaben:

```yaml
activity: datenstrukturen-start
tasks:
  - id: geklont
    title: Die Sandbox ist da
    prompt: Klone dein Klassen-Repository nach `/workspace/data-structures-sandbox`.
    check:
      type: files
      path: data-structures-sandbox/structures
      expect: { exists: true }

  - id: abgegeben
    title: Committet und gepusht
    prompt: Dein Stand ist committet und gepusht, damit deine Lehrkraft ihn in »Class progress« sieht.
    check:
      type: git
      repo: data-structures-sandbox/structures
      expect: { has_remote: true, committed: true, pushed: true }
```

`repo` ist das Klassen-Repository `structures` im Sandbox-Ordner, nicht die
Sandbox selbst. Die Prüfung hilft nur, wenn die Lernenden im Workspace
arbeiten; auf dem eigenen Rechner kann der Workspace nichts lesen.

Im Klassenboard sieht die Lehrkraft dann nicht nur, wer gepusht hat, sondern
auch, wer Änderungen hat und nie gepusht hat, und warum.

## Aufgaben auf der Seite

`_task(id)` auf einer eigenen Zeile setzt die Aufgabe `id` aus der
`tasks.yaml` neben der Seite als Karte an diese Stelle. Die Karten werden in
der Reihenfolge der Seite nummeriert (»Aufgabe 3«, bei `prediction: true`
»Vermutung 3«). In Codeblöcken bleibt `_task(...)` stehen. Fehlt die Aufgabe in
der `tasks.yaml`, steht an ihrer Stelle ein roter Hinweis.

Die Karte enthält nie, wogegen geprüft wird: `expect`, `correct` und
`solution` bleiben auf dem Server. Die Lösung kommt erst nach einer richtigen
Antwort mit der Rückmeldung.

Der Workspace prüft `answer`, `choice`, `number` und `reflection` (seit M2)
sowie `files`, `html`, `css`, `references`, `git` und `submit` (seit M3).
Karten der übrigen Arten zeigen »Zeig dein Ergebnis deiner Lehrkraft«.

Karten, die im Workspace nachsehen, nennen unter dem Text die echten Pfade, die
sie lesen (»Liest `/workspace/shower.js/index.html`«). Die Rückmeldung nennt
Datei und Zeile, und unter »Was der Workspace gelesen hat« steht, was die
Prüfung gefunden hat: eine Liste der Dateien mit Größe und Format, die Folien
mit ihren Überschriften, die Bilder mit `src` und `alt`, die Ausgabe von
`git status`.

### Wie Dateien gelesen werden

Der Server liest den Workspace-Ordner direkt (`/user/<tag>/workspace`), auch
wenn der Workspace aus ist, über `src/ruby/include/workspace_files.rb`:

- Pfade dürfen nicht absolut sein und kein `..` enthalten.
- Symbolische Links werden nie verfolgt, auch nicht in Ordnern auf dem Weg.
  Eine Karte sagt dann: »… ist ein symbolischer Link. Links liest der
  Workspace nicht.«
- Nach dem Öffnen fragt der Server den Kernel, welche Datei er wirklich
  geöffnet hat, und liest sie nur, wenn sie im Workspace liegt.
- Dateien werden höchstens bis 2 MB gelesen. Bilder werden nicht dekodiert;
  ihr Format erkennt der Server an den ersten Bytes.

HTML und CSS liest ein eigener, kleiner Parser (`html_lite.rb`,
`css_lite.rb`). Er ist absichtlich strenger als ein Browser und nennt die
Zeile, in der Tags oder Klammern nicht zusammenpassen.

### Problem-Codes

Mit `feedback: [{ when: { problem: <code> }, say: … }]` ersetzt eine Aufgabe
die eingebaute Rückmeldung für einen Fall.

| Art | Codes |
| --- | --- |
| `files` | `missing`, `symlink`, `wrong_type`, `wrong_format`, `not_executable`, `missing_text`, `no_new_files`, `bad_name`, `format_mismatch`, `too_big` |
| `html` | `missing`, `parse_error`, `no_slides`, `first_slide_unchanged`, `too_few_slides`, `slide_without_heading`, `too_few_images`, `alt_missing`, `image_case`, `image_missing` |
| `css` | `missing`, `no_style`, `marker_missing`, `parse_error`, `too_few_rules`, `missing_declaration`, `not_linked` |
| `references` | `missing`, `external`, `outside_root`, `local_missing` |
| `git` | `no_repo`, `not_git`, `no_remote`, `no_identity`, `uncommitted`, `too_few_commits`, `detached`, `no_upstream`, `not_pushed` |
| `er` | `missing`, `errors`, `missing_entity`, `key`, `missing_attribute`, `attribute`, `missing_relationship`, `cardinality`, `missing_table`, `missing_column`, `primary_key`, `foreign_key` |
| `schema` | `no_tables`, `missing_table`, `missing_column`, `primary_key`, `foreign_key`, `index`, `rows` |
| `story` | `no_project`, `no_analysis`, `stale`, `starter`, `errors`, `too_few_pages`, `too_few_choices`, `too_few_local_choices`, `too_few_conditions`, `too_few_scripts`, `too_few_images`, `unreachable`, `too_few_endings` |

Text, der nur die Karten ersetzt (etwa eine nummerierte Aufgabenliste),
steht in einem Element mit `class='tasks-off-only'` und verschwindet, sobald
die Karten zu sehen sind. Umgekehrt erscheint `class='tasks-on-only'` nur mit
den Karten. Beides muss HTML sein, weil Markdown in HTML-Blöcken nicht
gelesen wird.

### Einschalten

Aufgaben sind für Lernende zunächst ausgeschaltet. Lehrkräfte sehen sie
trotzdem, mit dem Vermerk »Vorschau für Lehrkräfte«, und können sie
ausprobieren. Für alle eingeschaltet werden sie in `credentials.rb`:

```ruby
TASKS_ENABLED = true
```

Fehlt die Zeile, bleibt alles wie bisher.

### Schnittstelle

| Aufruf | Daten | Antwort |
| --- | --- | --- |
| `POST /api/task/check` | `task` (`tcpip/adresse`), `answer` | `status`, `message`, `solution`, `state` |
| `POST /api/task/states` | `activity` | gespeicherte Antworten der angemeldeten Person |
| `POST /api/task/delete` | `task` | löscht Antwort und Versuche der angemeldeten Person |
| `POST /api/task/group_answers` | `activity`, `group` | für Lehrkräfte der Gruppe: die Antworten auf die offenen Aufgaben, je Aufgabe mit Name, Antwort, Datum und ob sie älter als die Aufgabe ist |
| `POST /api/task/answer_view` | `view` (`examples`, `group`, `off`), `group` | merkt sich, was eine Lehrkraft unter offenen Aufgaben sehen will |

`status` ist `correct`, `wrong`, `sent` (Reflexion gespeichert), `invalid`
(nicht lesbar, z. B. keine Zahl; zählt nicht) oder `unavailable` (gerade nicht
prüfbar, z. B. Workspace aus; zählt nicht).

## Startseite

Über dem Katalog stehen für angemeldete Lernende (nicht für Konten von
Lehrkräften; deren Testkonto ist ein Konto für Lernende) bis zu drei
schmale Teile (`StudentHome`):

- **Von deiner Lehrkraft:** was eine Lehrkraft in der Klassenübersicht für
  eine Gruppe der Person angeheftet hat, mit einem Balken pro Aufgabe (grün
  erledigt, gelb noch falsch, orange geändert, grau offen), »2 von 5
  erledigt« und einem Link zur ersten offenen Aufgabe. Angeheftetes steht
  immer zuerst, auch wenn noch nichts angefangen ist. Das geht mit jeder
  Aktivität aus dem Katalog, auch ohne Aufgaben. Gespeichert wird das als
  `(:Pin {key, group, slug, by, ts})`. Lehrkräfte sehen diesen Teil auch,
  wenn sie Mitglied einer Gruppe mit angehefteten Aktivitäten sind.
- **Weiter mit:** bis zu drei Aktivitäten, in denen die Person zuletzt etwas
  geprüft hat (nach der Zeit der letzten Antwort), die noch nicht fertig
  und nicht angeheftet sind, mit Balken und »1 von 7 Aufgaben«; der Link
  führt zur ersten offenen Aufgabe (`#aufgabe-…`). Nur Aktivitäten aus dem
  Katalog, keine aus versteckten Abschnitten.
- **So geht’s:** solange die Person noch keine Aufgabe geprüft hat, vier
  kurze Schritte: womit anfangen (das Angeheftete, sonst »Such dir unten
  eine Aktivität aus oder frag deine Lehrkraft«; es wird keine bestimmte
  Aktivität empfohlen), »Workspace öffnen«, »Prüfen« und das Handbuch,
  dazu eine Zeile, was Grundlagen, Übung, Labor und Projekt bedeuten. Nach
  der ersten geprüften Aufgabe verschwindet der Teil.

Im Katalog trägt jede Karte ihre Art (`kind`; der Tooltip erklärt sie),
aber keinen Stand: Der Katalog bleibt ruhig, den Stand zeigen die Seite der
Aktivität und »Weiter mit«.
Wer etwas aus `requires` braucht, hat unten auf der Karte eine Zeile »Baut
auf … auf«; die sehen alle, auch ohne Anmeldung.

Ohne `TASKS_ENABLED` sieht die Startseite für Lernende aus wie bisher, ohne
diese Teile; nur die Etiketten der Art und »Baut auf … auf« kommen dazu.
Während einer Leistungsüberprüfung zeigt die Startseite nur sie.

## Klassenübersicht

Unter `/klasse` (Menüpunkt »Klassen«) wählen Lehrkräfte oben die Gruppe.
Wartet jemand aus der Gruppe auf einen Anmelde-Code, steht er gleich darunter,
über allem anderen. Darunter liegen drei Reiter mit je einer Aufgabe; welcher
offen ist, steht in der Adresse (`?tab=`):

- **Fortschritt:** die Aktivität, »Jetzt nachsehen« (ein Menü mit den
  Aufgaben, die im Workspace nachsehen), »Vorlage aktuell?« und die Tabelle.
  Die Titel der Aufgaben stehen aufrecht über ihren Spalten.
- **Lernende:** die Karten des Servers wie auf der Verwaltungsseite (CPU und
  RAM, Speicher) und jede Person mit ihrem Workspace: läuft oder aus, CPU, RAM,
  »Workspace öffnen« und »Nachahmen«. Eine Zeile leuchtet gelb, solange ihr
  Workspace arbeitet, so stark wie seine CPU-Last.
- **Startseite:** was für die Gruppe angeheftet ist.

Die Tabelle unter »Fortschritt« zeigt, wer welche Aufgabe gelöst hat:

| Icon | Bedeutung |
| --- | --- |
| `check` | gelöst |
| Papierflieger | abgegeben oder Reflexion gespeichert |
| `x` mit Zahl | noch nicht gelöst, nach so vielen Versuchen; rot hinterlegt ab 3 Versuchen (»hängt«) |
| `refresh` | die Aufgabe hat sich geändert, die Antwort ist von vorher |
| `minus` | noch nicht bearbeitet |

Der Papierflieger ist »paper-plane« aus [Font Awesome Free](https://fontawesome.com)
(Icons: CC BY 4.0) in `src/static/include/fontawesome/`, als
`<i class='bi bi-paper-plane'>` überall, wo eine Abgabe steht. Die übrigen
Icons stammen aus [Tabler Icons](https://tabler.io/icons) (MIT) und liegen
als Sprite in `src/static/include/tabler/tabler.svg`; welche Icons darin sind,
steht in `icons.txt` daneben. `src/ruby/tools/build_tabler_sprite.rb` baut das
Sprite neu, `Icons.tabler('check')` (`src/ruby/include/icons.rb`) setzt eins
in eine Seite.

Die Zeile »Klasse« zählt, wie viele fertig sind und wie viele hängen; bei
Vermutungen auch, wie viele richtig vermutet haben. Eine Spalte ist gelb, wenn
mindestens ein Drittel derer, die es versucht haben, gerade falsch liegt
(»häufig falsch«), ab drei Personen. Darunter stehen die Antworten und
Reflexionen zum Nachlesen und die Abgaben.

Ein Klick auf eine Abgabe öffnet sie über der Klassenübersicht: links die
Dateien, rechts der Quelltext mit Zeilennummern, Bilder und Medien, und bei
Webseiten »Starten«. Die Seite läuft dann so, wie die Person sie abgegeben hat,
mit ihrem CSS und JavaScript, auch BIF-Geschichten, raylib-Programme und gebaute
SvelteKit-Seiten. Zuerst zeigt der Viewer die `index.html`, die am weitesten
oben liegt, sonst eine andere Seite (aber keine Vorlage aus `layouts/` wie bei
Hugo), sonst die oberste Textdatei; eine `index.html` läuft unter der Adresse
ihres Ordners, damit der Router von SvelteKit `/` findet. PDFs zeigt der Viewer
im Browser. Die Seite kommt von einem eigenen
Host, `abgaben.<host>`, unter einer signierten Adresse, die 12 Stunden gilt
(`SubmissionView`), und jede Antwort trägt `Content-Security-Policy: sandbox`
(nur ein PDF nicht: Chrome zeigt keins in einer Sandbox; als `application/pdf`
mit `nosniff` wird es nie zur Seite):
Die Seite hat einen eigenen, leeren Ursprung, bekommt keine Cookies der Seite
oder eines Workspaces und kann nichts im Namen der Lehrkraft tun. Damit Seiten,
die `localStorage` oder Cookies benutzen, trotzdem laufen, bekommen sie beim
Starten einen Speicher im Arbeitsspeicher vorangestellt; der Quelltext zeigt
die Datei unverändert. »Herunterladen« liefert die Abgabe als `.tar.gz`.

Unter jeder Liste von Abgaben steht **Alle Abgaben löschen**: eine neue Runde
für diese Aufgabe in dieser Gruppe. Nach einer Warnung geht für jedes Mitglied
der Gruppe, was »Abgabe löschen« bei der Person selbst löschen würde (Stand,
Versuche, Datei); andere Aufgaben, andere Gruppen und die eigene Abgabe der
Lehrkraft bleiben. Wer auch in einer anderen Gruppe ist, verliert die Abgabe
dort ebenso.

Die Klassenseite merkt sich am Konto der Lehrkraft die zuletzt gewählte Gruppe
und je Gruppe die zuletzt gewählte Aktivität (`class_prefs` am Knoten `User`);
`/klasse` ohne Angaben öffnet dort, wo sie zuletzt war.

Wer als Lehrkraft eine Gruppe betreut, ohne selbst in ihr zu stehen, sieht
die eigenen Versuche und Abgaben in einer eigenen Zeile »Du (Lehrkraft)« unter
der Klasse, sobald sie eine Aufgabe der Aktivität bearbeitet hat. Die Zeile
zählt nicht zur Klasse.

**Jetzt nachsehen** führt eine Aufgabe, die im Workspace
nachsieht (`files`, `html`, `css`, `references`, `story`, `git`), sofort für alle in
der Gruppe aus, ohne dass jemand »Prüfen« drücken muss. Das Ergebnis erscheint
blau umrandet in der Tabelle und darunter als Liste mit der Rückmeldung, die
die Person selbst sehen würde. Gespeichert wird dabei nichts; der Stand der
Lernenden bleibt, wie er ist. `git` braucht einen laufenden Workspace.

Wer was sieht:

- Administratorinnen und Administratoren sehen alle Gruppen.
- Lehrkräfte mit `+` in einer Einladungsdatei sehen diese Gruppe.
- Steht eine Adresse in mehreren Gruppen (Klasse 10b und WP 10), erscheint sie
  in jeder davon. Für alles andere im Workspace zählt weiter die erste Gruppe.
- Lehrkräfte mit `*` ohne eigene Gruppe sehen keine Klassenübersicht.

### Antworten unter offenen Aufgaben

Unter jeder Reflexion auf einer Aktivitätsseite steht für Lehrkräfte ein
violetter Kasten »Für Lehrkräfte« mit Knöpfen:

- **Beispielantworten:** die `examples` aus `tasks.yaml`, nur bei einer
  Aufgabe, die welche hat. Lehrkräfte mit `*` oder `+` und
  Administratorinnen sehen sie. Sie stehen nur in der Konfiguration der Seite
  für Lehrkräfte (`window.TASK_PAGE.teacher`), nie in der Karte selbst.
  Jede Reflexion soll zwei haben (`examples`, oben).
- **Antworten aus ‹Gruppe›:** was die Mitglieder der Gruppe gespeichert
  haben, nach Namen sortiert, mit Datum. Einen Knopf hat jede Gruppe, für die
  die Aktivität auf `/klasse` angeheftet ist, und die aktuelle Gruppe; auf
  jedem steht, wie viele der Gruppe diese Aufgabe beantwortet haben, »12/25«.
  Andere Gruppen wählt man unter »Andere Gruppe …«; die gewählte wird auch
  die aktuelle Gruppe unter `/klasse`. Die Zahlen und die Antworten holt die
  Seite alle 30 Sekunden neu, solange sie sichtbar ist; »Neu laden« sofort.
  Nur für Lehrkräfte, die die Klassenübersicht der Gruppe sehen dürfen.
- **Ausblenden:** Nur die Knopfleiste bleibt, etwa vor einem Beamer.

Zu Beginn ist der Kasten zu, nur die Knopfleiste ist da; die Seite kann so,
wie sie ist, an den Beamer. Die Wahl gilt für alle Aufgaben und wird am Konto
gespeichert (`class_prefs`, `answer_view`): Die nächste Seite öffnet genauso.
Zeigen die anderen Karten Beispielantworten, bleibt eine Karte ohne welche zu.

## Neue Version einer Vorlage

Sandboxes, der Anaglyph-Viewer, BIF und andere Vorlagen werden
weiterentwickelt, während Lernende mit ihrem Klon arbeiten. Auf der Seite
einer Aktivität mit `template` sieht der Workspace deshalb nach, ob der Klon
in `/workspace/<Name des Repositorys>` die neueste Version enthält. Wenn
nicht, steht unter der Einleitung:

> **Es gibt eine neue Version der CPU Sandbox!** Du solltest `git pull`
> ausführen:
>
>     cd /workspace/cpu-sandbox
>     git pull

Mit einem Knopf »Erledigt? Noch einmal nachsehen« und einem Link auf den
Handbuch-Teil `workspace#neue-version`, der erklärt, was zu tun ist, wenn
`git pull` abbricht.

Der Workspace holt nie selbst etwas. So läuft die Prüfung:

1. Der Server fragt die Adresse aus `template` nach dem aktuellen Commit, so
   wie `git ls-remote` es tut (HTTPS, `<adresse>/info/refs`). Das ist immer
   die öffentliche Adresse von der Seite, nie ein Repository von Lernenden.
   Die Antwort merkt er sich 10 Minuten, einen Fehler 2 Minuten.
2. Ein festes Skript (`TemplateFreshness::SCRIPT`) läuft im laufenden
   Workspace der Person, als diese Person: Ist der Ordner ein eigenes
   Repository, zeigt `origin` auf die Vorlage, und ist der Commit schon in
   `HEAD` enthalten? Es benutzt nur lesende Befehle (`git rev-parse`,
   `git config --get`, `git cat-file -e`, `git merge-base --is-ancestor`),
   ohne Sperrdateien (`GIT_OPTIONAL_LOCKS=0`) und ohne nachzuladen
   (`GIT_NO_LAZY_FETCH=1`).
3. Einen Hinweis gibt es nur, wenn `origin` die Vorlage ist und ihr Commit
   fehlt, auch wenn er schon geholt, aber noch nicht übernommen ist. Eigene
   Commits obendrauf zählen als aktuell. Hat jemand den Ordner auf ein
   eigenes Repository umgestellt, kein Klon, Workspace aus: kein Hinweis.

Wie die Aufgaben erscheint der Hinweis nur, wenn `TASKS_ENABLED` an ist (für
Lehrkräfte immer). In der Klassenübersicht prüft **Vorlage aktuell?** dasselbe
für die ganze Gruppe und listet, wer eine alte Version hat, wer nicht geklont
hat und wessen Workspace nicht läuft. Gespeichert wird nichts.

| Aufruf | Daten | Antwort |
| --- | --- | --- |
| `POST /api/template/status` | `slug` der Seite | `status` (`outdated`, `current`, `absent`, `other`, `unavailable`), `notice` |
| `POST /api/class/template_status` | `group`, `slug` | `results` je E-Mail-Adresse, `reachable`, `folder` |

## Was gespeichert wird

In Neo4j, nur für Antworten, die zählen:

- `(:User)-[:HAS_TASK_STATE]->(:TaskState)` pro Person und Aufgabe: Aufgabe,
  Version der Prüfung, Status, letzte Eingabe, Zahl der Versuche, Status des
  ersten Versuchs, Zeitpunkt der ersten richtigen Antwort, letzte Änderung.
  Der Schlüssel ist ein Hash aus E-Mail-Adresse und Aufgabe.
- `(:TaskAttempt)-[:BY]->(:User)` pro Versuch: Aufgabe, Version, Status und
  Zeitpunkt, ohne den Text der Antwort.
- Abgaben (seit M3) als `.tar.gz` unter `/internal/submissions/`, benannt nach
  dem Schlüssel des `TaskState`. Eine neue Abgabe ersetzt die alte erst, wenn
  sie vollständig geschrieben ist. Symbolische Links werden nicht mit
  abgegeben. Lernende laden ihre Abgabe auf der Karte herunter.

Sehen können das nur die Person selbst und die Lehrkräfte ihrer Gruppen, in
der Klassenübersicht. Lernende löschen
ihre Antwort zu einer Aufgabe auf der Karte (»Meine Antwort löschen«), wann sie
wollen, auch wenn Aufgaben wieder ausgeschaltet sind. Wer ein Konto von Hand
entfernt, löscht die Aufgabendaten mit:

```cypher
MATCH (u:User {email: $email})
OPTIONAL MATCH (u)-[:HAS_TASK_STATE]->(s:TaskState)
OPTIONAL MATCH (a:TaskAttempt)-[:BY]->(u)
DETACH DELETE s, a;
```

Ändert sich die Prüfung einer Aufgabe (eine neue Version), zeigt die Karte
»Aufgabe geändert, bitte neu prüfen«, und die Zählung beginnt neu.

## Eine tasks.yaml prüfen

```bash
cd src/ruby && ruby test/task_spec_test.rb && ruby test/task_card_test.rb
```

Die Tests lesen alle `tasks.yaml` unter `src/content` und melden jede
Abweichung mit Aufgabe und Feld. Außerdem prüfen sie, dass jedes `_task()` auf
einer Seite eine Aufgabe in der `tasks.yaml` daneben hat.

## Beispiele für die Prüfungen

Ob eine Prüfung stimmt, zeigt erst eine Lösung, die sie annimmt, und ein
unfertiger Stand, den sie ablehnt. Beides steht in `task-examples.yaml` neben
der `tasks.yaml`:

```yaml
format: 1
activity: pixelwand
examples:
  lock:
    pass:
      - files: { pixelwand/server.rb: stufen/server5.rb }
    fail:
      - files: { pixelwand/server.rb: stufen/server4.rb }
        say: Mutex.new
    python:
      pass:
        - files: { pixelwand/server.py: stufen/server5.py }
      fail:
        - files: { pixelwand/server.py: stufen/server4.py }
  public:
    skip: public/index.html entsteht erst, wenn hugo die Seite baut.
```

- `files` legt Dateien in einen leeren Workspace-Ordner: links der Pfad, den
  die Prüfung liest, rechts eine Datei aus dem Ordner der Aktivität (meist aus
  `stufen/` oder `loesung/`) oder `{ text: … }` für ein paar Zeilen;
  `executable: true` dazu (`{ from: …, executable: true }`) macht sie
  ausführbar.
- `archive` entpackt vorher ein `.tar.gz` aus dem Ordner der Aktivität in den
  Workspace-Ordner, für Prüfungen, die viele Dateien lesen (die Akten in
  `terminal-mystery`).
- `answer` ist eine Antwort, wie Lernende sie eintippen, für `answer` mit
  `compare: regex` und für `command`: dort der Befehl, den der Test mit `sh`
  im Workspace-Ordner ausführt, wie der Gast in `/workspace`.
- `say` ist ein Stück der Meldung, die ein abgelehntes Beispiel bekommen muss.
  So zeigt der Test auch, dass die richtige Meldung kommt.
- Unter `ruby:` oder `python:` stehen die Beispiele einer Sprachvariante mit
  eigener Prüfung. Eine Variante, die nur anders formuliert, nutzt die
  Beispiele der Aufgabe.
- `skip` sagt, warum eine Prüfung hier nicht getestet werden kann, zum Beispiel
  weil sie liest, was erst ein Build erzeugt.

`choice` und `number` brauchen keine Beispiele: Die richtige Antwort muss
passen, jede andere Option und eine Zahl weit außerhalb der Toleranz nicht.
Die Startpunkte aus `stufen/` sind gute Gegenbeispiele: Die Aufgabe muss sie
ablehnen, sonst ist sie gelöst, bevor jemand etwas getan hat.

```bash
cd src/ruby && ruby test/task_examples_test.rb
```

Der Test führt jedes Beispiel durch dieselbe Prüfung wie der Knopf »Prüfen«;
`ruby`- und `python`-Prüfungen laufen mit den Interpretern auf dem Rechner,
`c` mit seinem `cc`, `command` mit seinem `sh`.
Prüfungen, die einen Workspace oder eine Datenbank brauchen (`sql`, `cypher`,
`git`, `measure`, `scene` …), testet er noch nicht; ob die Schritte einer Seite
zu einer gelösten Aufgabe führen, prüfen `check-task` und `tutorial-step` in
den Screenshot-Rezepten ([aktivitaet-schreiben.md](aktivitaet-schreiben.md)).
Jede Prüfung vom Typ `files`, `html`, `css`, `references`, `ruby`, `python`,
`c`, `command` und jede `answer` mit `compare: regex` braucht Beispiele, sonst
schlägt der Test fehl.
