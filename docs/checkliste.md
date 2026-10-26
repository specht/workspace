# Checkliste für Aktivitäten

Was jede Aktivität und jede Handbuch-Seite erfüllen soll, an einem Ort: jede
Regel mit dem Grund und damit, wer sie prüft. Ein Test (`src/ruby/test/…`)
schlägt von selbst an, `src/scripts/audit-activities.rb` zeigt den Stand
aller Aktivitäten als Tabelle, der Rest will gelesen werden. Die anderen
Dokumente erklären, wie es geht, und verweisen für die Regeln hierher.

Wie man eine Aktivität schreibt, steht in
[aktivitaet-schreiben.md](aktivitaet-schreiben.md), die Aufgaben in
[aufgaben.md](aufgaben.md), warum Seiten so gebaut sind in
[didaktik.md](didaktik.md), die Widgets in [widgets.md](widgets.md).

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
`docs/checkliste.md`.«

## Aufbau der Seite

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Meta-Block mit `image`, `kind`, `duration`; `language` oder `languages`, wenn programmiert wird (das Thema kommt aus `sections.yaml`) | Die Karte im Katalog braucht sie | `page_kinds_test.rb` (`kind`), Lesen |
| Ein Absatz `<p class='abstract'>`, der sagt, was man baut, in der Du-Form | Er steht auf der Karte | Lesen |
| Das Inhaltsverzeichnis ist nicht flach: lange Kapitel (`##`, im Audit mehr als 800 Wörter Text) gliedern sich in Abschnitte (`###`) | Am Rand findet man sich in langen Seiten nur so zurecht | Audit (Spalte »Gliederung«) |
| Ein Bild für die Karte (`teaser.webp` oder das erste Bild), aus einem Rezept, wo es ein Bildschirmfoto ist, mit dunkler Fassung (`NAME-dark.webp`, aus `variants: dark` oder `src/scripts/render-svg.rb`, nicht invertiert). Eine Übung mit Katas hat stattdessen ein Icon (unten, »Katas und Pip«) | Der Katalog ist das Erste, was man sieht; ein invertiertes Bild verdreht die Farben | Audit (Spalte »Karte«) |
| Kapitel, ohne die die Seite vollständig ist (ein Ausblick, eine Verfeinerung am Ende), sind mit `<!-- extra -->` vor dem `##` als Extra markiert; kein späteres Kapitel für alle baut auf ihnen auf, die Dauer reicht ohne sie, und eine Aufgabe darin ist selbst ein Extra (`extra: true`) | Lernende sehen auf langen Seiten, was dazugehört und was für Schnelle ist | `extra_chapters_test.rb`, Lesen |
| Tabellen sind HTML (`<table class='table'>`), `</table>` und `</div>` auf eigener Zeile | Die Seite kennt keine Markdown-Tabellen; sonst steht der Rest der Seite als Text da | `no_pipe_tables_test.rb`, `html_block_test.rb` |
| Eine Handbuch-Tabelle (`table table-sm reference-table` in `<div class='scroll-x'>`) hat `<thead>` und `<tbody>`; ihre erste Spalte bricht nicht um, lange Befehle und Fehlermeldungen kommen in einen Codeblock oder eine Liste. Ein Vergleich mit drei oder mehr Spalten Code (Ruby neben Python) bekommt `reference-table-wide` und ein `<th>` am Anfang jeder Zeile | Auf dem Handy wird die Tabelle eine Liste, beschriftet mit dem Kopf (`prepare_reference_tables` in `_template.html`) | Lesen |
| Icons kommen aus den Bootstrap Icons (`bi-chevron-right`, `bi-x-lg`), bevor man ein SVG zeichnet oder ein Zeichen wie › oder × nimmt; Haken sind `bi-check-lg`. Ein Icon allein in einem Knopf steht in dessen Mitte (Rand zurücksetzen, `::before { vertical-align: 0 }`, mit Flex zentrieren) | Sie passen zu den anderen Icons und zu jeder Schrift | `no_check_marks_test.rb` (Haken), Lesen |
| Formeln mit `\\(`…`\\)` und `$$`…`$$`, `\_` für Unterstriche (`\\\_` in `\text{…}`); in HTML-Tabellen nur ein Backslash: `\(`…`\)` | Markdown frisst sonst Backslashes und Unterstriche; HTML geht nicht durch Markdown | `formula_escaping_test.rb` |
| Aktivitäten eines versteckten Abschnitts (`sections.yaml`) werden außerhalb dieses Abschnitts nirgends genannt oder verlinkt: nicht im Handbuch, nicht in anderen Aktivitäten, nicht in Texten, die die Seite selbst erzeugt (Vorschläge, »Kennst du schon«), nicht in öffentlichen Dateien wie CSS. Handbuch-Seiten für sie heißen unauffällig | Sie sollen entdeckt werden, nicht beworben | `wording_test.rb`, Lesen |

## Sprache und Text

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Namen im Code sind Englisch (Variablen, Funktionen, Dateien, Routen); Kommentare und Ausgaben dürfen Deutsch sein | So sieht Code auch später im Beruf aus | Lesen |
| Fachwörter, die man im Deutschen englisch sagt, bleiben englisch (Handshake, Lock, String, Header); Elemente von VS Code heißen wie ihre Beschriftung | Lernende sollen die Wörter wiedererkennen | Lesen |
| Datenstrukturen heißen englisch: Stack, Queue, Priority Queue, Heap, Linked List, Hash Table | So sagt man es auch im Deutschen | `wording_test.rb` |
| »Index« oder »Zählvariable«, nicht »Stelle«, für die Position in einem Array oder eine Schleifenvariable; »Stelle« bleibt für Ziffern einer Zahl und Stellen in einem Text | »Stelle« ist dafür ungenau | Lesen |
| Kein »Ding« und keine »Dinge«: was gemeint ist, hat einen Namen (die Leuchte, der Knoten, das Modell) | Das Wort bringt eine Klasse zum Lachen | `wording_test.rb` |
| »multiplizieren«, nicht »malnehmen« | Das ist das Wort aus dem Unterricht | `wording_test.rb` |
| Keine Fragen nach Geschmack oder Stimmung (»Wo wirkt es am unheimlichsten?«); eine Frage hat eine Antwort, die der Bildschirm zeigt | Darauf gibt es nichts zu prüfen oder zu besprechen | Lesen |
| Beispiele ahmen nichts nach, womit jemand in der Klasse zu kämpfen haben kann: Stottern, Lispeln, vertauschte Buchstaben, Akzente, Namen, Körper | Die Klasse liest die Seiten gemeinsam | Lesen |
| Die Seite behauptet nur, dass etwas von selbst wiederkommt, sich selbst wiederherstellt oder jederzeit geht, wenn der Code dafür gelesen oder das Verhalten ausprobiert ist; sonst sagt sie, was die Person tun muss | Eine falsche Zusage kostet Lernende Zeit und Vertrauen | Lesen |
| Ist die Sprache frei, gibt es die Aktivität in Ruby und Python (`languages: [ruby, python]`), sonst in Ruby. C bleibt C | Schulen unterrichten das eine oder das andere; die Klasse stellt es ein | Audit (Spalte »Sprachen«) |
| Jeder Codeblock nennt seine Sprache (`text`, wenn es keine ist). Ein Kopierknopf nur für Code, den man nicht tippen soll, und für große Blöcke | Abtippen hilft beim Lesen; Abschreiben von Gerüsten nicht | `code_highlight_test.rb` (teilweise), Lesen |
| Neuer Code mitten in einer Datei hat Kontextzeilen (`~ `) | Man sieht, wohin er gehört | `src/scripts/code-context.rb` schlägt sie vor |
| Der Code auf der Seite ist der Code in den Stufen der Rezepte | Bilder und Prüfungen testen, was die Seite zeigt | `page_code_test.rb` |
| Web-Apps der Lernenden laufen hinter `/proxy/PORT/` auf einer eigenen Subdomain: absolute Pfade (`/style.css`, `Location: /results`) gehen, die Seite erklärt keine relativen Umwege | Weniger Erklärungen, die nichts mit dem Thema zu tun haben | Lesen |

## Handbuch, Widgets, Links und Menschen

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Im Handbuch stehen Algorithmen als Pseudocode mit einem Beispiel zum Durchklicken, nicht in Ruby oder Python | Lernende sollen verstehen und übersetzen, nicht die Lösung einer Aufgabe kopieren | Lesen |
| Eine Handbuch-Seite enthält die Grundlagen und das Wichtigste und verlinkt für den Rest die offizielle Referenz oder das Handbuch (»Zum Nachschlagen«); Aktivitäten verlinken die Handbuch-Seite, statt deren Links zu wiederholen | Lernende finden das Nötige auf der Seite und wissen, wo die vollständige Antwort steht | Lesen |
| Ein Widget folgt [widgets.md](widgets.md) (es beginnt mit dem, worum es geht, ein Schritt je Zeile Pseudocode, was dasselbe bleibt, bewegt sich) und löst keine Aufgabe seiner Aktivität: Es zeigt andere Beispiele als die Aufgaben, und wo man seine Eingabe ändern kann, gibt auch die Eingabe einer Aufgabe nicht ihre Antwort | Sonst rechnet das Widget, und die Lernenden müssen nicht mehr denken | Lesen |
| Wo ein Standard oder Dokument genannt wird (ein RFC, eine ISO-Norm, ein Aufsatz), ist sein Name ein geprüfter Link darauf, auch in Aufgaben und Notizen für Lehrkräfte. Links auf andere Seiten haben kein `target`: den neuen Tab setzt der Server | Wer nachsehen will, was dort wirklich steht, soll es können, ohne die Aktivität zu verlieren | `external_links_test.rb`, `wording_test.rb` (kein `target`), Lesen |
| Ein Link sagt, was man dort findet, nicht wie offiziell er ist. Eine gute Quelle, die `curl` mit 403 abweist (Shadertoy, ACM), bleibt; der Maintainer prüft sie im Browser | Ein »inoffiziell« macht eine gute Quelle schlecht; ein Bot-Schutz sagt nichts über die Seite | Lesen |
| Wer erfunden hat, worum es auf einer Seite oder in einem Abschnitt geht, bekommt einen Kasten: ein Teil der Seite `reference/personen` (sortiert nach Geburtsjahr), eingefügt mit `_person(name)` nach dem Absatz, der die Person oder ihre Erfindung nennt, nie vor einer Aufgabe, die er verraten würde. Zwei, drei Sätze verbinden die Person mit dem, was Lernende aus dem Workspace kennen; ohne freies Foto nur Text, keine Silhouette | Hinter jeder Erfindung stehen Menschen | `person_boxes_test.rb` (jede Person steht auf einer sichtbaren Seite, kein Kasten beginnt einen Teil), Lesen |
| Fotos in diesen Kästen: jede Lizenz, unter der wir das Bild kostenlos zeigen dürfen (gemeinfrei, CC0, CC BY, CC BY-SA, auch nicht-kommerzielle), mit einer Zeile für Fotograf, Lizenz und »zugeschnitten«; Zuschnitt 4:5. Vom Maintainer ausgesuchte Fotos bleiben, ein besseres wird vorgeschlagen, nicht getauscht | Der Workspace ist nicht kommerziell; die Auswahl ist eine Entscheidung | Lesen |
| Ein Aufsatz bekommt einen Kasten (`<div class='book'>`), wo er das Verfahren des Abschnitts zum ersten Mal beschreibt, ein Buch, wo es genau dort weitermacht; beide mit geprüftem Link. Nicht zu jedem Thema irgendein Aufsatz | Wer weiterlesen will, findet die Quelle | Lesen ([aktivitaet-schreiben.md](aktivitaet-schreiben.md), »Bücher und Aufsätze zum Weiterlesen«) |

## Aufgaben

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Es gibt Aufgaben, und sie stehen dort, wo die Lernenden gerade etwas getan haben, nicht gesammelt am Ende. Ausgenommen sind `about` und `basics`: Sie erklären den Workspace, da gibt es nichts zu prüfen | Lehrkräfte sehen den Fortschritt der Klasse; Lernende wissen, ob es stimmt | Audit (Spalte »Aufgaben«), Lesen |
| Der Text verrät die Antwort nicht: Was eine Aufgabe fragt, steht nicht in den Sätzen direkt davor, und das Bild davor zeigt nicht das Ergebnis, nach dem gefragt wird. Eine Vorhersage (`prediction: true`) kommt vor dem Ausprobieren, und was ihre Antwort verrät, steht danach zwischen `<!-- after: id -->` und `<!-- /after -->` | Sonst lernt man nur Abschreiben | `after_task_test.rb`, Lesen |
| Rückmeldungen (`feedback`, `say`) für die typischen Fehler, die Lösung (`solution`) erst nach dem Lösen | Die Rückmeldung soll weiterhelfen | Lesen |
| Jede Prüfung, die etwas liest, hat Beispiele in `task-examples.yaml`: eine Lösung, die sie annimmt, einen Anfang, den sie ablehnt | Sonst merkt niemand, wenn eine Prüfung zu streng oder zu lasch ist | `task_examples_test.rb` |
| Rezepte prüfen mit `check-task` und `check-task-fails`, dass die Schritte der Seite zur gelösten Aufgabe führen, in jeder Sprache | Die Seite und die Prüfung passen zusammen | Rezepte beim Rendern |
| Prüfungen lesen nur, sie ändern nichts im Workspace | Die fünf Regeln in [aufgaben.md](aufgaben.md) | Lesen |
| Wo die Seite erklärt hat, was eine kleine Funktion tun soll, schreiben die Lernenden sie erst selbst: ein Kasten `hint own` (»Jetzt bist du dran:«), der Code der Seite danach zwischen `<!-- solution -->` und `<!-- /solution -->`, mit Tipps davor, als deren letzte Stufe; wo es geht, prüft eine Aufgabe darunter den eigenen Code, und `data-check` am Kasten nennt sie | Beim Abtippen denkt man nicht nach | `solution_hints_test.rb`, Lesen |
| Wo Lernende hängen bleiben können, gibt es Tipps in Stufen: vom Verstehen über den Weg zum Gerüst, nie die ganze Lösung | Wer feststeckt, kommt weiter, ohne abzuschreiben | Lesen ([didaktik.md](didaktik.md)) |
| Wo etwas gerade läuft, lädt ein Kasten `hint try` (»Ausprobieren:«) zu einer kleinen Änderung ein und sagt, worauf man achten soll; ungeprüft, die Seite geht mit dem unveränderten Code weiter | Wie etwas funktioniert, versteht man, wenn man es ändert | Lesen |

## Katas und Pip

Entstanden beim Bau der Katas und von »Pip unter Funkelbach«
([plan-katas.md](plan-katas.md)).

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Die Aufgabe sagt, was man schreibt und was die Prüfung damit tut: Ruft sie eine Funktion selbst auf, nennt der Text die Aufrufe (»die Prüfung ruft ihn auf drei Karten auf, mit `walk(3)`, `walk(5)` und `walk(1)`«) | Sonst rätselt man, warum Pip drei Schritte geht, obwohl man nur einen Spruch geschrieben hat | Lesen |
| Mehrere Karten einer Aufgabe unterscheiden sich, und jede zeigt, was erwartet wird; jeder Reiter nennt den Aufruf, den die Prüfung auf dieser Karte macht. Zahlen im Aufgabentext gelten für jede Karte, die jemand zu sehen bekommt | Drei gleiche Karten prüfen nichts; jede Person sieht nur ihre eigene Karte | Lesen, `katas.browser.spec.ts` |
| Jede Kata hat einen Knopf, »Ausführen« mit dem Abspiel-Symbol: Er führt den Code aus, zeigt, was er ausgibt (bei Pip: spielt den Lauf ab), und prüft. Der Erfolg kommt bei Pip erst, wenn der Lauf zu Ende gespielt ist | Zwei Knöpfe für fast dasselbe verwirren; ein Lob vor dem Ende verrät das Ergebnis | `katas.browser.spec.ts` (ein Knopf), Lesen |
| Die Stufe einer Kata steht als Wort mit drei Balken neben dem Titel (Einstieg, Übung, Knobelei) | Punkte allein sagen niemandem etwas | `katas.browser.spec.ts` |
| »Ausführen« läuft zuerst auf der Karte im gewählten Reiter, die danach gewählt bleibt; solange ein Lauf unterwegs ist oder abgespielt wird, warten die Reiter | Man probiert gerade diese Karte aus | `pip_check_test.rb`, `katas.browser.spec.ts` |
| Pips Karten einer Aufgabe erscheinen gleich groß, auf dem Desktop neben dem Code; Drehen und Aufheben spielen schneller als ein Schritt, das Tempo gilt für die Seite und die nächste; unter dem Code stehen die Befehle in der Schreibweise der Sprache, ihre Bedeutung beim Überfahren | Beim Wechseln springt nichts; Warten langweilt; man sieht, was man zur Verfügung hat | Lesen (Bildschirmfotos aus `katas.browser.spec.ts`) |
| `say` geht in jeder Pip-Aufgabe; was Pip sagt, kommt Satz für Satz in der Pixelschrift und ganz innerhalb der Karte; ein Satz mit der richtigen Zahl zählt wie die Zahl allein | So verspricht es das Handbuch | `pip_check_test.rb`, `katas.browser.spec.ts` |
| Pips Welt in der 3/4-Ansicht: Figuren aus dem 2D Game Studio, in der Mitte ihres Feldes und hinter dem Fels davor; Seitenwände nur links; Bilder aus `src/static/images/pip/make_sprites.py`, nach einer Änderung die `?v=` erhöhen | Wiedererkennbar; der Browser hält alte Bilder sonst fest | Lesen |
| Eine Geschichte stimmt für deutsche Schulen oder sagt, woher ein Spiel kommt (FizzBuzz ist englisch, »Brot« aus der Grundschule); ihr Ende kündigt nichts an und wirbt für nichts | Sonst stimmt sie nicht für die Klasse; ein Ende soll ein Ende sein | Lesen |
| Eine Übung mit Katas hat im Streifen auf der Startseite ein Icon (`icon:`), kein Bild einer Karte, auf einem Band in ihrem Farbton: Er kommt von ihrem Platz im Streifen, gleichmäßig über den Farbkreis verteilt, ohne Ausnahme (`KataMap.hue`). Pip, Bello und der Mampfer sind in Grau gezeichnet und eingefärbt (`make_icons.py`), etwa so groß wie die anderen Icons | Eine gelöste Kata im Bild verriete die Lösung; ein Streifen in einem Stil | Audit (Spalte »Karte«), Lesen |
| Code in einer Rückmeldung, einer Lösung oder einem Tipp steht in einem dunklen Codeblock mit den Farben der Seite, eine Lösung mit Zeilennummern | Code sieht überall gleich aus | `task_html_test.rb` (gefärbt), Lesen |
| Jede Kata hat eine Lösung; Lehrkräfte setzen sie mit »Lösung einsetzen« ein, Lernende sehen sie erst nach einer richtigen Antwort | Lehrkräfte wollen in der Stunde vorführen können | Lesen |

## Auf dem Handy

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Auf einem Handy (bis 576 px) laufen alle Kästen der Seite von Rand zu Rand (Notiz für Lehrkräfte, Aufgabe, Tipp, Person, Buch, Codeblock); ihr Text beginnt dort, wo die Absätze beginnen. Ebenso der Streifen der Katas: Am Anfang steht die erste Übung bündig mit dem Text, beim Blättern laufen die Übungen unter den Rand. Kästen in Kästen, Code in Listen und Bilder behalten ihren Rahmen | Mehr Platz für Code und Text | Lesen (Bildschirmfotos in iPhone-Breite) |

## Bilder

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jedes Bild hat einen `alt`-Text, der sagt, was zu sehen ist | Barrierefreiheit, und beim Lesen ohne Bild | `alt_text_test.rb` |
| Bilder nebeneinander sind eine Galerie (`<div class='gallery'>`, je ein `<figure>` mit `<figcaption>`), Beschriftungen in der Bildunterschrift, nicht im Bild | Sie passen sich der Breite an | `gallery_markup_test.rb` |
| Bilder aus dem Workspace, aus einer Vorschau oder von einer Seite der Website kommen aus Rezepten (`<!-- tutorial-screenshot -->` als eigener Block vor dem `<img>`), nie von Hand | Nach einem Update entstehen sie neu | Audit (Spalte »von Hand«), `tutorial_screenshot_markup_test.rb`, `recreate-screenshots.rb --status` |
| Rezepte, deren Bild für die folgenden nichts ändert, stehen jedes in einem `<!-- tutorial-scope -->`, mit einer Leerzeile davor, wenn davor ein Bild oder Text steht | Eine Änderung rendert nur dieses Bild neu; ohne Leerzeile steht die Marke als Text da | `scope_markers_test.rb` (Leerzeile), Lesen |
| Screenshots ohne linke Seitenleiste, außer sie gehört zum Schritt | Sie nimmt Platz und lässt das Terminal umbrechen | Lesen |
| Bilder, die hell und dunkel aussehen können, haben eine dunkle Fassung (`variants: dark`) | Die Seite folgt dem Farbschema | Audit (Spalte »dunkel«) |
| In einem Bild einer 3D-Szene steht das, worum es geht, gut im Bild, nicht am Rand und nicht abgeschnitten; die Kamera leicht schräg; jedes Bild nach dem Rendern ansehen. Im Anaglyph-Tutorial bleiben die Modelle Drahtgitter, außer wo es um Schattierung geht oder eine Szene schön aussehen soll | Ein zufälliger Ausschnitt erklärt nichts; Drahtgitter zeigen die Form | Lesen |
| In Diagrammen zielt eine Kante auf die Mitten der Knoten, die sie verbindet; eine Pfeilspitze zeigt in Richtung ihrer Kante, die Spitze auf dem Rand des Knotens; ein gebogener Pfeil kommt auf der Linie zur Mitte an (`dns/dns-tree.svg`, `sql-index/b-tree.svg`) | Schräg ankommende Kanten sehen unordentlich aus | Lesen |
| Beschriftungen in Diagrammen lesen sich wie die Seite: Formeln mit LaTeX gesetzt, Code als Code (`hit.y = 1.35` mit Dezimalpunkt), der deutsche rechte Winkel (Viertelkreis mit Punkt), alles aus `src/scripts/diagram_labels.py`. Achsen: in 3D x nach rechts, y nach oben, z zum Betrachter; in 2D y nach oben, außer auf einem Bildschirm oder einer Karte darauf, wo y wie im Code nach unten geht. Das Skript eines Diagramms liegt daneben in `bilder/` | Lernende erkennen wieder, was im Text und im Code steht | Lesen |
| Keine echten Personen, Adressen oder Daten von Lernenden in Bildern; Namen in Bildern sind ausgedacht | Datenschutz | Lesen |

## Für Lehrkräfte

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jede Aktivität im Katalog hat `lehrkraft.md`, und der erste Absatz sagt in ein, zwei Sätzen, was die Lernenden tun (»Lernende …«) | Nach zwei Sätzen weiß eine Lehrkraft, worum es geht | `teacher_note_test.rb` |
| Danach eine Tabelle (Voraussetzungen, Ablauf je Doppelstunde, Dateien, Prüfung, Abgabe), mit Listen statt langer Zellen und HTML statt Markdown in den Zellen, dann Abschnitte mit `####` (Was schiefgeht, Die Aufgaben) als Listen | Ein Blatt zum Nachschlagen, auch auf dem Handy in der Stunde | `teacher_note_test.rb` (Markdown in der Tabelle), Lesen |

## Vorlagen, Daten und Betrieb

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Eine Vorlage zum Klonen hat `template` und `template_name` im Meta-Block; ihr Stand liegt auch im Repository (`vorlage/` oder `stufen/`) | Rezepte und Prüfungen arbeiten damit ohne GitHub | `template_freshness_test.rb` (Meta-Felder), Lesen |
| Jede Aktivität klont in einen Ordner, den keine andere benutzt; teilen sich Aktivitäten eine Vorlage, klonen die anderen mit `git clone ${template} ~/NAME` in einen eigenen Ordner | VS Code klont sonst still nach `name-1`, und die Seite spricht vom falschen Ordner | `clone_folders_test.rb` |
| Datensätze kommen aus Skripten in `src/scripts/collect-*` und werden mit `publish-datasets.rb` veröffentlicht, mit Lizenz und Quelle | Man kann sie neu bauen | Lesen |
| Ins CHANGELOG kommt unter »Unreleased« eine kurze deutsche Zeile nur für das, was Betreibende einer anderen Installation wissen müssen: neue oder geänderte Werte in `credentials.rb`, Schritte beim Update, Funktionen, die ausgeschaltet beginnen, Änderungen an dem, was gespeichert wird | Betreibende lesen es vor einem Update; der Rest steht in der Git-Geschichte | Lesen |
| Keine echten Namen von Menschen im Repository (Dokumente, Tests, Commits): die Rolle nennen; echte Namen und Adressen nur in `credentials.rb` | Datenschutz; einmal veröffentlicht, ist ein Name schwer wieder herauszubekommen | Lesen |
