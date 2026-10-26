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
| Meta-Block mit `image`, `kind`, `duration`; `language` oder `languages`, wenn programmiert wird (das Thema kommt aus `sections.yaml`) | Die Karte im Katalog braucht sie | `page_kinds_test.rb`, Audit |
| Ein Absatz `<p class='abstract'>`, der sagt, was man baut, in der Du-Form | Er steht auf der Karte | Audit |
| Das Inhaltsverzeichnis ist nicht flach: lange Kapitel (`##`, im Audit mehr als 800 Wörter Text) gliedern sich in Abschnitte (`###`) | Am Rand findet man sich in langen Seiten nur so zurecht | Audit (Spalte »Gliederung«) |
| Ein Bild für die Karte (`teaser.webp` oder das erste Bild), aus einem Rezept, wo es ein Bildschirmfoto ist, mit dunkler Fassung (`NAME-dark.webp`, aus `variants: dark` oder `src/scripts/render-svg.rb`, nicht invertiert), wenn die Seite dunkel werden kann | Der Katalog ist das Erste, was man sieht; ein invertiertes Bild verdreht die Farben | Audit (Spalte »Karte«) |
| Kapitel, ohne die die Seite vollständig ist (ein Ausblick, eine Verfeinerung am Ende), sind mit `<!-- extra -->` vor dem `##` als Extra markiert; kein späteres Kapitel für alle baut auf ihnen auf, die Dauer reicht ohne sie, und eine Aufgabe darin ist selbst ein Extra (`extra: true`, zählt nicht mit) | Lernende sehen auf langen Seiten, was dazugehört und was für Schnelle ist | `extra_chapters_test.rb` (Stelle, Aufgaben nur mit `extra: true`), Lesen |
| Tabellen sind HTML (`<table class='table'>`), `</table>` und `</div>` auf eigener Zeile | Die Seite kennt keine Markdown-Tabellen | `no_pipe_tables_test.rb` |
| Haken sind Bootstrap-Icons, keine Unicode-Zeichen | Sie passen zu den anderen Icons und jeder Schrift | `no_check_marks_test.rb` |
| Formeln mit `\\(`…`\\)` und `$$`…`$$`, `\_` für Unterstriche | Markdown frisst sonst Backslashes und Unterstriche | Lesen |
| In HTML-Tabellen nur ein Backslash: `\(`…`\)` | HTML geht nicht durch Markdown, sonst steht `\\(n^2\\)` auf der Seite | `formula_escaping_test.rb` |

## Sprache und Code

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Namen im Code sind Englisch (Variablen, Funktionen, Dateien, Routen); Kommentare und Ausgaben dürfen Deutsch sein | So sieht Code auch später im Beruf aus | Lesen |
| Fachwörter, die man im Deutschen englisch sagt, bleiben englisch (Handshake, Lock, String, Header); Elemente von VS Code heißen wie ihre Beschriftung | Lernende sollen die Wörter wiedererkennen | Lesen |
| Datenstrukturen heißen englisch: Stack, Queue, Priority Queue, Heap, Linked List, Hash Table | So sagt man es auch im Deutschen | Lesen |
| Im Handbuch stehen Algorithmen als Pseudocode mit einem Beispiel zum Durchklicken, nicht in Ruby oder Python | Lernende sollen verstehen und übersetzen, nicht die Lösung einer Aufgabe kopieren | Lesen |
| Ein Widget löst keine Aufgabe seiner Aktivität: Es zeigt andere Beispiele als die Aufgaben, und wo man seine Eingabe ändern kann, gibt auch die Eingabe einer Aufgabe nicht ihre Antwort | Sonst rechnet das Widget, und die Lernenden müssen nicht mehr denken | Lesen |
| Eine Handbuch-Seite enthält die Grundlagen und das Wichtigste und verlinkt für den Rest die offizielle Referenz oder das Handbuch (»Zum Nachschlagen«) | Lernende finden das Nötige auf der Seite und wissen, wo die vollständige Antwort steht | Lesen |
| »Index« oder »Zählvariable«, nicht »Stelle«, für die Position in einem Array oder eine Schleifenvariable | »Stelle« ist dafür ungenau | Lesen |
| Ist die Sprache frei, gibt es die Aktivität in Ruby und Python (`languages: [ruby, python]`), sonst in Ruby. C bleibt C | Schulen unterrichten das eine oder das andere; die Klasse stellt es ein | Audit (Spalte »Sprachen«) |
| Jeder Codeblock nennt seine Sprache. Ein Kopierknopf nur für Code, den man nicht tippen soll, und für große Blöcke | Abtippen hilft beim Lesen; Abschreiben von Gerüsten nicht | `code_highlight_test.rb` (teilweise), Lesen |
| Neuer Code mitten in einer Datei hat Kontextzeilen (`~ `) | Man sieht, wohin er gehört | `src/scripts/code-context.rb` schlägt sie vor |
| Der Code auf der Seite ist der Code in den Stufen der Rezepte | Bilder und Prüfungen testen, was die Seite zeigt | `page_code_test.rb` |

## Aufgaben

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Es gibt Aufgaben, und sie stehen dort, wo die Lernenden gerade etwas getan haben, nicht gesammelt am Ende. Ausgenommen sind `about` und `basics`: Sie erklären den Workspace, da gibt es nichts zu prüfen | Lehrkräfte sehen den Fortschritt der Klasse; Lernende wissen, ob es stimmt | Audit (Spalte »Aufgaben«), Lesen |
| Der Text verrät die Antwort nicht: Was eine Aufgabe fragt, steht nicht in den Sätzen direkt davor, und das Bild davor zeigt nicht das Ergebnis, nach dem gefragt wird. Eine Vorhersage (`prediction: true`) kommt vor dem Ausprobieren, und was ihre Antwort verrät (Bild, Erklärung, Kasten), steht danach zwischen `<!-- after: id -->` und `<!-- /after -->` | Sonst lernt man nur Abschreiben | `after_task_test.rb`, Lesen |
| Rückmeldungen (`feedback`, `say`) für die typischen Fehler, die Lösung (`solution`) erst nach dem Lösen | Die Rückmeldung soll weiterhelfen | Lesen |
| Jede Prüfung, die Dateien liest, hat Beispiele in `task-examples.yaml`: eine Lösung, die sie annimmt, einen Anfang, den sie ablehnt | Sonst merkt niemand, wenn eine Prüfung zu streng oder zu lasch ist | `task_examples_test.rb` |
| Rezepte prüfen mit `check-task` und `check-task-fails`, dass die Schritte der Seite zur gelösten Aufgabe führen, in jeder Sprache | Die Seite und die Prüfung passen zusammen | Rezepte beim Rendern |
| Prüfungen lesen nur, sie ändern nichts im Workspace | Die fünf Regeln in aktivitaet-schreiben.md | Lesen |
| Wo ein Standard oder Dokument genannt wird (ein RFC, eine ISO-Norm, ein Aufsatz), ist sein Name ein geprüfter Link darauf, auch in Aufgaben und Notizen für Lehrkräfte; Links auf andere Seiten öffnen sich in einem neuen Tab | Wer nachsehen will, was dort wirklich steht, soll es können, ohne die Aktivität zu verlieren | Lesen; den neuen Tab setzt `ExternalLinks` (`external_links_test.rb`) |
| Menschen, Bücher und Aufsätze stehen dort, wo sie hingehören: Wer erfunden hat, worum es in einem Abschnitt geht, bekommt einen Kasten (`_person(name)`), ein historischer Aufsatz, in dem das Verfahren des Abschnitts zum ersten Mal beschrieben ist (nicht zu jedem Thema irgendein Aufsatz), oder ein Buch, das genau dort weitermacht, einen Kasten `book` mit geprüftem Link | Hinter jeder Erfindung stehen Menschen, und wer weiterlesen will, findet die Quelle | Lesen (aktivitaet-schreiben.md, »Bücher und Aufsätze zum Weiterlesen«) |
| Wo die Seite erklärt hat, was eine kleine Funktion tun soll, schreiben die Lernenden sie erst selbst: ein Kasten `hint own` (»Jetzt bist du dran:«), der Code der Seite danach zwischen `<!-- solution -->` und `<!-- /solution -->`, mit Tipps davor, als deren letzte Stufe; wo es geht, prüft eine Aufgabe darunter den eigenen Code (`ruby`, `python`, `c`), und `data-check` am Kasten nennt sie | Beim Abtippen denkt man nicht nach; wer selbst schreibt, merkt, was er noch nicht verstanden hat | `solution_hints_test.rb`, Lesen (aktivitaet-schreiben.md, »Jetzt bist du dran«) |
| Wo Lernende hängen bleiben können (Labs, Kästen »Jetzt bist du dran«), gibt es Tipps in Stufen: vom Verstehen über den Weg zum Gerüst, nie die ganze Lösung | Wer feststeckt, kommt weiter, ohne abzuschreiben; die Pause zwischen den Stufen lässt nachdenken | Lesen ([didaktik.md](didaktik.md)) |
| Wo etwas gerade läuft, lädt ein Kasten `hint try` (»Ausprobieren:«) zu einer kleinen Änderung ein und sagt, worauf man achten soll; ungeprüft, die Seite geht mit dem unveränderten Code weiter | Wie etwas funktioniert, versteht man, wenn man es ändert und sieht, was passiert | Lesen |

## Katas und Pip

Entstanden beim Bau der Katas und von »Pip unter Funkelbach«
([plan-katas.md](plan-katas.md)); was noch nicht überall gilt, steht dabei.

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Die Aufgabe sagt, was man schreibt und was die Prüfung damit tut: Ruft sie eine Funktion selbst auf, nennt der Text die Aufrufe (»die Prüfung ruft ihn auf drei Karten auf, mit `walk(3)`, `walk(5)` und `walk(1)`«) und dass der Plan selbst nichts laufen lassen muss | Sonst rätselt man, warum Pip drei Schritte geht, obwohl man nur einen Spruch geschrieben hat | Lesen |
| Mehrere Karten einer Aufgabe unterscheiden sich, und jede zeigt, was erwartet wird (ein Gang, genau so lang wie `walk(n)` gehen soll); jeder Reiter nennt den Aufruf, den die Prüfung auf dieser Karte macht | Drei gleiche Karten prüfen nichts Neues und verwirren | Lesen, `katas.browser.spec.ts` (»Pip runs on the map of the tab«) |
| Zahlen im Aufgabentext gelten für jede Karte, die jemand zu sehen bekommt (»genau dreimal ziehen«, nicht »drei, vier oder fünf Hebel«) | Jede Person sieht nur ihre eigene Karte | Lesen |
| Jede Kata (`function` und `pip`) hat einen Knopf, »Ausführen« mit dem Abspiel-Symbol: Er führt den Code aus, zeigt, was er ausgibt (bei Pip: spielt den Lauf ab), und prüft. Der Erfolg (grün, Abzeichen) kommt bei Pip erst, wenn der Lauf auf dem Bildschirm zu Ende ist | Zwei Knöpfe für fast dasselbe verwirren; ein Lob vor dem Ende verrät das Ergebnis | `katas.browser.spec.ts` |
| Die Stufe einer Kata steht als Wort mit drei Balken neben dem Titel (Einstieg, Übung, Knobelei), in einer Farbe je Stufe | Punkte allein sagt niemandem etwas | `katas.browser.spec.ts` |
| Beim Abspielen gehen Drehen und Aufheben schneller als ein Schritt; das gewählte Tempo gilt für alle Pip-Karten der Seite und bleibt für die nächste Seite | Warten langweilt; einmal einstellen reicht | Lesen |
| Pip und alles, was steht, verschwinden hinter dem Fels, der davor liegt (der Fels in der unteren Hälfte eines Feldes, an runden Ecken) | Sonst steht Pip auf der Wand | Lesen (Bildschirmfotos) |
| »Ausführen« läuft zuerst auf der Karte im gewählten Reiter, und die Karte bleibt danach gewählt (außer eine andere Karte scheitert, dann zeigt die Karte sie). Solange ein Lauf unterwegs ist oder abgespielt wird, lassen sich die Reiter nicht wechseln | Man probiert gerade diese Karte aus | `pip_check_test.rb`, `katas.browser.spec.ts` |
| Alle Karten einer Aufgabe erscheinen gleich groß (mit Fels aufgefüllt), auf dem Desktop neben dem Code, und füllen den Platz | Beim Wechseln springt nichts; man scrollt nicht zwischen Plan und Karte | Lesen (Bildschirmfotos aus `katas.browser.spec.ts`) |
| Unter dem Code stehen die Befehle der Aufgabe, in der Schreibweise der Sprache, mit ihrer Bedeutung beim Überfahren | Man sieht, was man zur Verfügung hat | `katas.browser.spec.ts` |
| `say` funktioniert in jeder Pip-Aufgabe; was Pip sagt, kommt Satz für Satz in der Pixelschrift, lang genug zum Lesen und ganz innerhalb der Karte; ein Satz mit der richtigen Zahl zählt wie die Zahl allein | So verspricht es das Handbuch; »Ich habe 4 Steine« ist eine richtige Antwort | `pip_check_test.rb`, `katas.browser.spec.ts` |
| Nach einem Lauf steht der Cursor wieder im Code (mit Maus); der Code hat Zeilennummern, und `end`, `}` rücken beim Tippen selbst aus | Weiterschreiben ohne Klick | `katas.browser.spec.ts` |
| Pips Welt in der 3/4-Ansicht: Figuren aus dem 2D Game Studio, in der Mitte ihres Feldes; Seitenwände nur links; Ecken gemischt eng, weit und rund; Licht und Schatten. Bilder aus `src/static/images/pip/make_sprites.py`; nach einer Änderung die `?v=` in `default.css` und `pip_world.js` erhöhen | Wiedererkennbar und lesbar; der Browser hält alte Bilder sonst fest | Lesen |
| Ein Spiel oder Brauch im Aufgabentext stimmt für deutsche Schulen, oder der Text sagt, woher er kommt (FizzBuzz ist ein englisches Zählspiel, »Brot« das aus der Grundschule) | Sonst stimmt die Geschichte nicht für die Klasse | Lesen |
| Das Ende einer Geschichte kündigt keine Fortsetzung an und wirbt nicht für etwas anderes | Es soll ein Ende sein | Lesen |
| Eine Übung mit Katas hat im Streifen auf der Startseite ein Icon (`icon: bi-braces`), kein Bild einer Karte, auf einem Band in ihrem eigenen Farbton (`accent:` in Grad, alle gleich hell und kräftig, entlang des Streifens weiterrückend); Pips Übungen zeigen Pip und Bello in Bernstein, jedes Pixel gleich groß (`make_icons.py`, zweimal 24 Pixel). Grün bleibt für Knöpfe und Fortschritt | Eine gelöste Kata im Bild verriete die Lösung; ein Streifen in einem Stil, nicht in einer Farbe | Lesen |
| Code in einer Rückmeldung, einer Lösung oder einem Tipp steht in einem dunklen Codeblock mit den Farben der Seite, wie jeder andere Code | Hell auf hell ist nicht zu lesen; Code sieht überall gleich aus | Lesen (`task_html_test.rb` prüft die Farben) |
| Jede Kata hat eine Lösung (`solution`); Lehrkräfte setzen sie mit »Lösung einsetzen« in den Code ein, Lernende sehen sie erst nach einer richtigen Antwort | Lehrkräfte wollen in der Stunde vorführen können | Lesen |

## Bilder

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jedes Bild hat einen `alt`-Text, der sagt, was zu sehen ist | Barrierefreiheit, und beim Lesen ohne Bild | `alt_text_test.rb` |
| In Diagrammen zielt eine Kante auf die Mitten der Knoten, die sie verbindet; eine Pfeilspitze zeigt in Richtung ihrer Kante, die Spitze auf dem Rand des Knotens. Ein gebogener Pfeil kommt auf der Linie zur Mitte an (sein letzter Kontrollpunkt liegt darauf), und ein Pfeil beginnt an der Stelle im Knoten, zu der er gehört, nicht in einer Ecke | Kanten, die an einer Kante des Kastens enden oder schräg ankommen, sehen unordentlich aus | Lesen (`dns/dns-tree.svg`, `sql-index/b-tree.svg` als Beispiele) |
| Bilder aus dem Workspace, aus einer Vorschau oder von einer Seite der Website kommen aus Rezepten, nie von Hand | Nach einem Update von VS Code, des Themes oder der Seite entstehen sie neu | Audit (Spalte »von Hand«), `recreate-screenshots.rb --status` |
| Rezepte, deren Bild für die folgenden nichts ändert (eine Galerie von Beispielen, eine Vorschau nach der anderen), stehen jedes in einem `<!-- tutorial-scope -->` | Dann rendert eine Änderung an einem Bild nur dieses neu, nicht alle danach, Videos eingeschlossen | Lesen (`src/tutorial-screenshots/README.md`, »Scopes«) |
| Vor einer Scope-Marke steht eine Leerzeile, wenn die Zeile davor ein Bild oder Text ist | Sonst gehört sie für Markdown zum Absatz davor und steht als Text auf der Seite | `scope_markers_test.rb` |
| Screenshots ohne linke Seitenleiste, außer sie gehört zum Schritt | Sie nimmt Platz und lässt das Terminal umbrechen | Lesen |
| Bilder, die hell und dunkel aussehen können (Karten, Vorschauen, Zeichnungen), haben eine dunkle Fassung (`variants: dark`) | Die Seite folgt dem Farbschema | Audit (Spalte »dunkel«) |
| Bilder nebeneinander sind eine Galerie (`<div class='gallery'>`, je ein `<figure>` mit `<figcaption>`), Beschriftungen in der Bildunterschrift, nicht im Bild | Sie passen sich der Breite an | `gallery_markup_test.rb` |
| Keine echten Personen, Adressen oder Daten von Lernenden in Bildern; Namen in Bildern sind ausgedacht | Datenschutz | Lesen |

## Für Lehrkräfte

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Jede Aktivität im Katalog hat `lehrkraft.md`, und der erste Absatz sagt in ein, zwei Sätzen, was die Lernenden tun (»Lernende …«) | Nach zwei Sätzen weiß eine Lehrkraft, worum es geht | `teacher_note_test.rb` |
| Danach eine Tabelle (Voraussetzungen, Ablauf je Doppelstunde, Dateien, Prüfung, Abgabe), mit Listen statt langer Zellen und HTML statt Markdown in den Zellen, dann Abschnitte mit `####` (Was schiefgeht, Die Aufgaben) als Listen | Ein Blatt zum Nachschlagen, auch auf dem Handy in der Stunde | `teacher_note_test.rb` (Markdown in der Tabelle), Lesen |
| Eine neue oder geänderte Aktivität hat einen Eintrag im CHANGELOG, auf Deutsch, für Lernende, Lehrkräfte und Betreibende | Was sich ändert, soll man nachlesen können | Lesen |

## Vorlagen und Daten

| Regel | Warum | Geprüft durch |
| --- | --- | --- |
| Eine Vorlage zum Klonen hat `template` und `template_name` im Meta-Block; ihr Stand liegt auch im Repository (`vorlage/` oder `stufen/`) | Rezepte und Prüfungen arbeiten damit ohne GitHub | `template_freshness_test.rb` |
| Jede Aktivität klont in einen Ordner, den keine andere benutzt. »Clone Repository« nennt den Ordner wie das Repository; teilen sich Aktivitäten eine Vorlage, behält die einfachste diesen Weg, die anderen klonen im Terminal mit `git clone ${template} ~/NAME` in einen Ordner mit ihrem Namen | VS Code klont in einen vorhandenen Ordner nicht, sondern still nach `name-1`, und die Seite spricht dann vom falschen Ordner | `clone_folders_test.rb` |
| Andere Repositories (Starter, Game Studio) bekommen Patches; `specht/routenplaner-starter`, `specht/backrooms-starter` und `specht/workspace-files` darf ein Agent direkt pushen | Der Maintainer entscheidet, was dort landet | AGENTS.md |
| Datensätze kommen aus Skripten in `src/scripts/collect-*` und werden mit `publish-datasets.rb` veröffentlicht, mit Lizenz und Quelle | Man kann sie neu bauen | Lesen |
