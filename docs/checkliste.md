# Checkliste für Aktivitäten

Was jede Aktivität erfüllen soll, an einem Ort. Die Regeln sind über die Zeit
entstanden, meist an einer konkreten Seite; hier stehen sie mit dem Grund und
damit, wer sie prüft. Ein Test (`src/ruby/test/…`) schlägt von selbst an,
`src/scripts/audit-activities.rb` zeigt den Stand aller Aktivitäten als
Tabelle, der Rest will gelesen werden.

Wie man eine Aktivität schreibt, steht in
[aktivitaet-schreiben.md](aktivitaet-schreiben.md); die Aufgaben in
[aufgaben.md](aufgaben.md).

## So lässt man eine Aktivität prüfen

Einem Sprachmodell (oder einer Person) gibt man:

> Prüfe die Aktivität `src/content/SLUG` gegen `docs/checkliste.md`. Lies dazu
> die Seite, `tasks.yaml`, `task-examples.yaml`, `lehrkraft.md` und die
> Bilder, führe `src/scripts/audit-activities.rb SLUG` und die Ruby-Tests aus.
> Nenne jede Regel, die nicht erfüllt ist, mit Datei und Zeile und einem
> Vorschlag. Ändere noch nichts.

Für alle auf einmal: `src/scripts/audit-activities.rb` ohne Argument, dann
dieselbe Bitte für jede Zeile, in der etwas fehlt. Für eine neue Aktivität:
»Schreib die Aktivität … nach `docs/aktivitaet-schreiben.md` und
`docs/checkliste.md`.« `AGENTS.md` verweist auf diese Datei; ein Agent, der im
Repository arbeitet, liest sie also ohnehin.

## Aufbau der Seite

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Meta-Block mit `image`, `kind`, `topic`, `duration`; `language` oder `languages`, wenn programmiert wird | Die Karte im Katalog braucht sie | `page_kinds_test.rb`, Audit |
| Ein Absatz `<p class='abstract'>`, der sagt, was man baut, in der Du-Form | Er steht auf der Karte | Audit |
| Das Inhaltsverzeichnis ist nicht flach: längere Seiten gliedern ihre Kapitel (`##`) in Abschnitte (`###`) | Am Rand findet man sich in langen Seiten nur so zurecht | Audit (Spalte »Gliederung«) |
| Ein Bild für die Karte (`teaser.webp` oder das erste Bild), aus einem Rezept, wo es ein Bildschirmfoto ist, mit dunkler Fassung, wenn die Seite dunkel werden kann | Der Katalog ist das Erste, was man sieht | Audit |
| Tabellen sind HTML (`<table class='table'>`), `</table>` und `</div>` auf eigener Zeile | Die Seite kennt keine Markdown-Tabellen | `no_pipe_tables_test.rb` |
| Haken sind Bootstrap-Icons, keine Unicode-Zeichen | Sie passen zu den anderen Icons und jeder Schrift | `no_check_marks_test.rb` |
| Formeln mit `\\(`…`\\)` und `$$`…`$$`, `\_` für Unterstriche | Markdown frisst sonst Backslashes und Unterstriche | Lesen |

## Sprache und Code

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Namen im Code sind Englisch (Variablen, Funktionen, Dateien, Routen); Kommentare und Ausgaben dürfen Deutsch sein | So sieht Code auch später im Beruf aus | Lesen |
| Fachwörter, die man im Deutschen englisch sagt, bleiben englisch (Handshake, Lock, String, Header); Elemente von VS Code heißen wie ihre Beschriftung | Lernende sollen die Wörter wiedererkennen | Lesen |
| »Index« oder »Zählvariable«, nicht »Stelle«, für die Position in einem Array oder eine Schleifenvariable | »Stelle« ist dafür ungenau | Lesen |
| Ist die Sprache frei, gibt es die Aktivität in Ruby und Python (`languages: [ruby, python]`), sonst in Ruby. C bleibt C | Schulen unterrichten das eine oder das andere; die Klasse stellt es ein | Audit (Spalte »Sprachen«) |
| Jeder Codeblock nennt seine Sprache. Ein Kopierknopf nur für Code, den man nicht tippen soll, und für große Blöcke | Abtippen ist Lernen; Abschreiben von Gerüsten nicht | `code_highlight_test.rb` (teilweise), Lesen |
| Neuer Code mitten in einer Datei hat Kontextzeilen (`~ `) | Man sieht, wohin er gehört | `src/scripts/code-context.rb` schlägt sie vor |
| Der Code auf der Seite ist der Code in den Stufen der Rezepte | Bilder und Prüfungen testen, was die Seite zeigt | `page_code_test.rb` |

## Aufgaben

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Es gibt Aufgaben, und sie stehen dort, wo die Lernenden gerade etwas getan haben, nicht gesammelt am Ende | Lehrkräfte sehen den Fortschritt der Klasse; Lernende wissen, ob es stimmt | Audit (Spalte »Aufgaben«), Lesen |
| Der Text verrät die Antwort nicht: Was eine Aufgabe fragt, steht nicht in den Sätzen direkt davor, und das Bild davor zeigt nicht das Ergebnis, nach dem gefragt wird. Eine Vorhersage (`prediction: true`) kommt vor dem Ausprobieren | Sonst lernt man nur Abschreiben | Lesen |
| Rückmeldungen (`feedback`, `say`) für die typischen Fehler, die Lösung (`solution`) erst nach dem Lösen | Die Rückmeldung soll weiterhelfen | Lesen |
| Jede Prüfung, die Dateien liest, hat Beispiele in `task-examples.yaml`: eine Lösung, die sie annimmt, einen Anfang, den sie ablehnt | Sonst merkt niemand, wenn eine Prüfung zu streng oder zu lasch ist | `task_examples_test.rb` |
| Rezepte prüfen mit `check-task` und `check-task-fails`, dass die Schritte der Seite zur gelösten Aufgabe führen, in jeder Sprache | Die Seite und die Prüfung passen zusammen | Rezepte beim Rendern |
| Prüfungen lesen nur, sie ändern nichts im Workspace | Die fünf Regeln in aktivitaet-schreiben.md | Lesen |

## Bilder

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jedes Bild hat einen `alt`-Text, der sagt, was zu sehen ist | Barrierefreiheit, und beim Lesen ohne Bild | `alt_text_test.rb` |
| Bilder aus dem Workspace, aus einer Vorschau oder von einer Seite der Website kommen aus Rezepten, nie von Hand | Nach einem Update von VS Code, des Themes oder der Seite entstehen sie neu | Audit (Spalte »von Hand«), `recreate-screenshots.rb --status` |
| Screenshots ohne linke Seitenleiste, außer sie gehört zum Schritt | Sie nimmt Platz und lässt das Terminal umbrechen | Lesen |
| Bilder, die hell und dunkel aussehen können (Karten, Vorschauen, Zeichnungen), haben eine dunkle Fassung (`variants: dark`) | Die Seite folgt dem Farbschema | Audit (Spalte »dunkel«) |
| Bilder nebeneinander sind eine Galerie (`<div class='gallery'>`, je ein `<figure>` mit `<figcaption>`), Beschriftungen in der Bildunterschrift, nicht im Bild | Sie passen sich der Breite an | `gallery_markup_test.rb` |
| Keine echten Personen, Adressen oder Daten von Lernenden in Bildern; Namen in Bildern sind ausgedacht | Datenschutz | Lesen |

## Für Lehrkräfte

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jede Aktivität im Katalog hat `lehrkraft.md`, und der erste Absatz sagt, was die Lernenden tun (»Lernende …«), mit Voraussetzungen | Nach drei Sätzen weiß eine Lehrkraft, worum es geht | `teacher_note_test.rb` |
| Danach, meist als Tabelle: Ablauf über die Stunden, was die Aufgaben prüfen, Abgabe, typische Stolpersteine | Für die Planung und die Stunde selbst | Lesen |
| Eine neue oder geänderte Aktivität hat einen Eintrag im CHANGELOG, auf Deutsch, für Lernende, Lehrkräfte und Betreibende | Was sich ändert, soll man nachlesen können | Lesen |

## Vorlagen und Daten

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Eine Vorlage zum Klonen hat `template` und `template_name` im Meta-Block; ihr Stand liegt auch im Repository (`vorlage/` oder `stufen/`) | Rezepte und Prüfungen arbeiten damit ohne GitHub | `template_freshness_test.rb` |
| Andere Repositories (Starter, Game Studio) bekommen Patches; `specht/routenplaner-starter` und `specht/workspace-files` darf ein Agent direkt pushen | Der Maintainer entscheidet, was dort landet | AGENTS.md |
| Datensätze kommen aus Skripten in `src/scripts/collect-*` und werden mit `publish-datasets.rb` veröffentlicht, mit Lizenz und Quelle | Man kann sie neu bauen | Lesen |
