# Changelog

Alle nennenswerten Änderungen am Hackschule Workspace stehen hier. Andere
Schulen mit eigener Installation folgen getaggten Releases statt `main`.

Versionen folgen [Semantic Versioning](https://semver.org/lang/de/):

- **Major** (`2.0.0`): Eine bestehende Installation braucht einen Handgriff
  beim Update, zum Beispiel neue Pflichtwerte in der Konfiguration oder eine
  Datenmigration, die nicht von selbst läuft.
- **Minor** (`1.1.0`): Neue Funktionen und Inhalte; Update ohne Handgriff.
  Neue Funktionen sind zunächst abgeschaltet.
- **Patch** (`1.0.1`): Fehlerkorrekturen.

Jeder Eintrag sagt, was sich für Lernende, Lehrkräfte und Betreibende ändert.

## Unreleased

### Hinzugefügt

- Neue Aktivität »Endliche Automaten in Python« unter »Experimente«:
  Lernende bauen ein Drehkreuz, einen Automaten für »durch 3 teilbar« und ein
  Zahlenschloss als DEA, finden heraus, warum das Schloss bei 121213 nicht
  aufgeht, schreiben das Schloss als NEA, bauen mit der
  Potenzmengenkonstruktion einen DEA daraus und sehen ihn explodieren.
  Automatron dient zum Vergleichen. Zehn Aufgaben, Hinweise für Lehrkräfte.
- Neue Prüfart `python` (`docs/aufgaben.md`): lädt eine Python-Datei der
  Lernenden in ihrem Workspace, als Gast, ruft Funktionen daraus auf und
  vergleicht die Ergebnisse. Die Rückmeldung nennt den ersten Aufruf, der
  nicht passt, mit seinem Ergebnis oder seinem Fehler samt Zeile.
- Format für Aufgaben, die der Workspace selbst prüft: `docs/aufgaben.md`.
  Noch nicht aktiv; Seiten binden noch keine Aufgaben ein.
- `src/ruby/include/task_spec.rb` prüft `tasks.yaml`-Dateien;
  `src/ruby/test/task_spec_test.rb` prüft alle Aufgaben im Inhaltsordner.
- Erste Aufgabendateien für TCP/IP (12), Anaglyphen (12) und shower.js (11).
- Handbuch: eigener Katalog in `src/content/reference/`, Übersicht unter
  `/reference`, Seiten unter `/reference/<slug>`. Erste Seiten: »Workspace:
  Grundlagen« und »HTML-Folien gestalten«.
- Platzhalter `_include(seite#teil)` und `_uses(teil, …)` sowie Teile in
  Handbuch-Seiten (`<!-- part: name -->`), siehe `docs/aufgaben.md`.
- Notizen für Lehrkräfte: `lehrkraft.md` neben einer Seite erscheint für
  Lehrkräfte als aufklappbare Notiz über der Aktivität.
- Neue, freiwillige Felder im Meta-Block: `kind`, `topic`, `duration`,
  `requires`, `reference`, `template`.
- Aktivitäten und Handbuch-Seiten sehen unterschiedlich aus: Über einer
  Aktivität stehen Art, Dauer und die Handbuch-Seiten, die sie nutzt; über
  einer Handbuch-Seite stehen »Handbuch › Abschnitt« und die
  Aktivitäten, die sie verwenden. Inhalte aus dem Handbuch sind türkis markiert.
- Menüpunkt »Handbuch« für alle, auch ohne Anmeldung.
- Aufgabenkarten: `_task(id)` setzt eine Aufgabe aus der `tasks.yaml` in die
  Seite. Der Workspace prüft Antworten, Auswahl, Zahlen und Reflexionen
  (`answer`, `choice`, `number`, `reflection`) und merkt sich das Ergebnis.
  Lernende können ihre Antwort jederzeit selbst löschen. Erste Aufgaben in
  »TCP/IP-Verbindungen« (5 Stück).
- Aufgaben sind für Lernende ausgeschaltet, bis eine Installation
  `TASKS_ENABLED = true` in `credentials.rb` setzt. Lehrkräfte sehen sie
  vorher schon als Vorschau.
- Aufgaben, die im Workspace nachsehen: `files` (Dateien, Formate,
  Dateinamen, Größen), `html` (Tags passen zusammen, Folien, Bilder mit
  `alt`), `css` (Klammern, Doppelpunkte, Semikolons, eigene Regeln),
  `references` (nichts aus dem Netz, alle Pfade stimmen) und `git`
  (committet und gepusht?). Die Rückmeldung nennt Datei und Zeile und zeigt,
  was gelesen wurde. Der Server folgt dabei keinen symbolischen Links und
  führt Git nur im Workspace der Person aus, nur lesend.
- Abgaben (`submit`): eine Kopie der genannten Dateien als `.tar.gz` unter
  `/internal/submissions/`; Lernende laden sie herunter, geben neu ab oder
  löschen sie.
- Neue Aktivitäten »Mit der Data Structures Sandbox arbeiten«, »Mit der
  Sorting Sandbox arbeiten« und »Mit der CPU Sandbox arbeiten« unter »Terminal
  & Git«: begleiten Einrichtung, Klonen und Veröffentlichen und prüfen am Ende
  jeder Stunde, ob alles committet und gepusht ist. Die Adresse des
  Klassen-Repositorys steht jeweils im Meta-Block (`class_repo`).
- Handbuch-Seite »Git und GitLab«: SSH-Schlüssel, Name und E-Mail für Git, was
  tun, wenn ein Push abgelehnt wird. Die drei Sandbox-Seiten binden sie ein.
- Alle elf Aufgaben in »Eine Präsentation in HTML erstellen« und die
  2048-Aufgabe in »TCP/IP-Verbindungen« stehen jetzt auf der Seite.
- Klassenübersicht unter `/klasse` (Menüpunkt »Klassen« für Lehrkräfte mit
  Gruppe): Lernende × Aufgaben einer Aktivität, wer hängt, was häufig falsch
  ist, Antworten zum Nachlesen und Abgaben zum Herunterladen. »Jetzt in den
  Workspaces nachsehen« prüft eine Workspace-Aufgabe sofort für die ganze
  Gruppe, ohne etwas zu speichern.
- Eine Adresse kann in mehreren Gruppen stehen (Klasse und Kurs); die
  Klassenübersicht zeigt sie in jeder. Für alles andere zählt wie bisher die
  erste Gruppe.
- Lehrmaterial listet die Aktivitäten mit einer Notiz für Lehrkräfte.
- Tabler Icons (MIT) als gebündeltes Sprite in `src/static/include/tabler/`,
  zunächst für die Klassenübersicht; Liste in `icons.txt`, neu bauen mit
  `src/ruby/tools/build_tabler_sprite.rb`.
- Code Bites wird zu Übungen: »Programmieren üben« im Katalog mit drei
  Übungen (Zahlen, Arrays, Strings) und 17 Aufgabenkarten der Prüfart `code`.
  Gelöst wird weiter im Code-Editor; wer dort alle Tests besteht, ist auf der
  Karte und in der Klassenübersicht abgehakt. Der Knopf »Code Bites« in der
  Menüleiste und sein Schalter im Profil entfallen; `/codebites` bleibt
  erreichbar.
- Startseite: angemeldete Lernende sehen »Weitermachen« mit ihren
  angefangenen Aktivitäten und der nächsten offenen Aufgabe. Katalogkarten
  zeigen die Art der Aktivität und den eigenen Stand.
- Prüfarten `measure` und `command`: Messwerte vergleicht der Server mit
  einer eigenen Messung im Workspace der Person (ping, traceroute, curl);
  Befehle laufen im eigenen Workspace mit Zeitlimit, die Ausgabe wird
  geprüft. Alle TCP/IP-Aufgaben außer dem Codewort stehen jetzt auf der Seite.
- Anleitung für Lehrkräfte, die eigene Aktivitäten schreiben:
  `docs/aktivitaet-schreiben.md`, mit Vorlage in `docs/vorlage-aktivitaet/`
  (ein Test hält sie gültig). Hinweis darauf im README.
- Startseite: Lernende blenden angefangene Aktivitäten aus, bis sie dort
  weiterarbeiten. Lehrkräfte heften in der Klassenübersicht Aktivitäten für
  eine Gruppe an; diese stehen ganz oben und lassen sich nicht ausblenden.
  Neuer Knoten `Pin` (Constraint
  `Pin/key`).
- Prüfart `scene`: Der Workspace liest eine Anaglyph-Szene mit dem Viewer
  der Person in ihrem eigenen Workspace und prüft Formen, Positionen, Größen,
  Reihen, Gitter und Umlaufbahnen; Fehler des Viewers erscheinen wörtlich.
  Alle zwölf Anaglyph-Aufgaben stehen jetzt auf der Seite; eine abgegebene
  Szene muss sich laden lassen.
- Prüfart `sql`: Lernende schreiben eine `SELECT`-Abfrage; sie läuft in
  ihrer eigenen MySQL-Datenbank, als sie selbst, nur lesend und mit
  Zeitlimit, und wird mit dem Ergebnis einer Musterabfrage verglichen. Die
  Karte zeigt das eigene Ergebnis als Tabelle. Kann sie nicht prüfen, nennt
  sie Datenbank, Anmeldung, die Meldung von MySQL, vorhandene Tabellen, den
  Workspace und `.my.cnf`. »Einführung in SQL« hat jetzt
  13 Aufgaben; die alte Aufgabenliste erscheint nur noch, wenn Aufgaben
  ausgeschaltet sind (`tasks-off-only`).
- Neue Version einer Vorlage: Auf der Seite einer Aktivität mit `template`
  sieht der Workspace nach, ob der Klon in `/workspace` die neueste Version
  enthält, und sagt sonst »Es gibt eine neue Version der CPU Sandbox! Du
  solltest `git pull` ausführen«. Er holt nie selbst etwas. In der
  Klassenübersicht prüft »Vorlage aktuell?« die ganze Gruppe. Neues Feld
  `template_name`; `template` steht jetzt auch bei BIF, LaTeX, PixelRAM,
  Videothek und dem Digits-Experiment. Neuer Handbuch-Teil
  `workspace#neue-version`.
- ER-Werkstatt: VS Code-Erweiterung `hackschule-er` im Workspace-Image.
  ER-Modelle werden als Text in `.er`-Dateien geschrieben; daneben erscheinen
  sie als Chen-, Krähenfuß- und Tabellendiagramm (Kästen verschiebbar), Fehler
  werden unterstrichen, »SQL erzeugen« schreibt das MySQL-Schema, »Als SVG
  speichern« das Diagramm. Kern und Kommandozeile:
  `src/vscode-extensions/er/core/er-core.js`.
- Handbuch-Seite »ER-Modelle als Text« (`/reference/er-modelle`): Syntax,
  Übersetzung in Tabellen, die drei Diagramme, die ER-Werkstatt in VS Code und
  auf der Kommandozeile, mit einem Feld zum Ausprobieren, das denselben Kern
  im Browser nutzt. Skripte neben einer Seite (`<script src='x.js'>`) werden
  wie Bilder über `/cache/` ausgeliefert.
- »Dateien und Verzeichnisse« hat 12 Aufgaben: entpackte Beispieldateien,
  mit `nano` und `vim` geschriebene Dateien, die Wortliste, das übersetzte
  Zork, Zahlen, die man mit `grep`, `wc` und der Tab-Ergänzung findet, und
  eine Notiz für Lehrkräfte.
- Aufgaben für »Versionsverwaltung mit Git« (11: Commits, Merge, Remote,
  Konflikt, jeweils aus `git status` des eigenen Repositorys) und »Statische
  Webseiten mit HTML & CSS« (10, mit Abgabe), jeweils mit Notiz für
  Lehrkräfte.
- Prüfarten `er` (Entitäten, Schlüssel, Attribute, Beziehungen,
  Kardinalitäten und die entstehenden Tabellen eines `.er`-Modells, gelesen mit
  dem Kern der ER-Werkstatt im Workspace) und `schema` (Spalten, Primär- und
  Fremdschlüssel und Zeilenzahl in der eigenen MySQL-Datenbank, nur lesend).
- »Filmdatenbank mit MySQL« ersetzt »Videothek mit MySQL«: vom Modell in der
  ER-Werkstatt über `mysql < film.sql` zu `JOIN`, `GROUP BY` und `HAVING`,
  mit 15 Aufgaben und einer Notiz für Lehrkräfte. `/videothek` leitet weiter.
  Ruby, das Import-Skript und der ERD Editor werden nicht mehr gebraucht.
- Filmdaten aus Wikidata (CC0): `src/scripts/collect-movies` wählt die Filme
  nach der Zahl der IMDb-Stimmen, übernimmt aber nur Daten aus Wikidata. Die
  Spalte `rating` entfällt; `crew.birth_year` darf leer sein.
- Neue Neo4j-Knoten `TaskState` und `TaskAttempt` (Constraint
  `TaskState/key`, Index `TaskAttempt/task`); sie werden beim Start von selbst
  angelegt.
- `_include(seite#teil, done: aufgabe)`: Ist die genannte Aufgabe auf der
  Seite gelöst, zeigt der Handbuch-Kasten beim nächsten Laden nur noch eine
  Zeile mit »Anzeigen«. Die drei Sandbox-Seiten nutzen das für »Git sagen,
  wer du bist«.
- Prüfart `git` ohne `repo` mit `expect: { identity: true }`: Kennt Git
  Namen und E-Mail-Adresse, und sind es nicht die Beispielwerte aus dem
  Handbuch? Neue Aufgabe »Git kennt dich« in den drei Sandbox-Begleitern.
- Auswahlaufgaben mischen ihre Optionen für jede Person anders, aber immer
  gleich (E-Mail-Adresse und Aufgabe als Seed). Antworten, `correct` und
  `picked` zählen weiter nach `tasks.yaml`; `shuffle: false` schaltet das
  für eine Aufgabe ab.
- Tutorial-Screenshots: Vor einem `<video>` erzeugt ein Rezept mit
  `record: 8s` (und `fps:`) das Video, Bild für Bild mit fester Zeit über den
  neuen Hook `setPreviewTime`, kodiert mit ffmpeg. Das Anaglyph-Tutorial hat
  Rezepte für seine Screenshots und alle drei Animationen.
  `src/scripts/recreate-screenshots.rb` kennt `--all` und `--stale`.
- Prüfart `scene` erkennt Farben: `color: yellow` (oder eine Liste von
  Farbfamilien) wählt Objekte nach ihrer Füllfarbe aus, `background: black`
  prüft den Hintergrund, `label` benennt einen Eintrag für Rückmeldungen und
  `orbits`. Die Rückmeldung zeigt bei jedem Objekt, welche Farbe der Viewer
  liest. Feedback in `scene` kann zusätzlich auf `problem` achten.
- Handbuch-Seite »Farben angeben«: Farbnamen, Farbcodes mit `#`, Rot-, Grün-
  und Blauanteile, Kontrast. Das Anaglyph-Tutorial verweist darauf.
- Code-Blöcke mit ```` ```er ```` sind auf der Website eingefärbt wie in der
  ER-Werkstatt (`src/ruby/include/er_lexer.rb`, ein Rouge-Lexer).
- Neue Aktivität »Filmdatenbank als Graph mit Neo4j« (`neo4j-filmdatenbank`),
  die Fortsetzung der Filmdatenbank: dieselben Daten aus `neo4j.dump`, Cypher
  im Vergleich zu SQL, Schlüssel und Indizes, Schauspielpaare, Wege durch den
  Graphen; sechs Aufgaben und eine Notiz für Lehrkräfte. Vorerst nur in der
  Entwicklung im Katalog, bis `neo4j.dump` veröffentlicht ist.
- Code-Blöcke mit ```` ```cypher ```` sind eingefärbt
  (`src/ruby/include/cypher_lexer.rb`): Labels, Beziehungstypen und
  Eigenschaften in denselben Farben wie Entitäten, Beziehungen und Attribute
  in ```` ```er ````. Die Muster in »Filmdatenbank als Graph« sind jetzt echtes
  Cypher (`(:Movie)` statt `(Movie)`).
- Neue Aktivität »Feuerwerk aus Partikeln« (`feuerwerk`) unter
  Computergrafik, in C mit PixelRAM: vom ersten Funken (Position,
  Geschwindigkeit, Schwerkraft) über Explosionen, Farbverläufe und Leuchtspuren
  bis zu Raketen über einer nächtlichen Stadt, die man mit der Maus startet;
  fünf Aufgaben, darunter zwei Rechnungen zur Physik und eine Abgabe.
- »Echtzeitgrafik mit C« (PixelRAM) ist neu geschrieben: Bilder aus Formeln
  (bis zum Sierpinski-Dreieck aus `x & y`), Wellen und Interferenz, das Spiel
  »Lichtspur« für zwei an einer Tastatur, die Beispiele der Vorlage
  (`make PROGRAM=beispiele/tunnel`) und Crystal Caves (`make crystal`). Eine
  Notiz für Lehrkräfte nennt größere Projekte: Raycaster, Raytracer, GIF- und
  JPEG-Decoder.
- Bilder und Videos in Varianten: Im dunklen Modus erscheint die dunkle
  Fassung von selbst. Ein Knopf in der Ecke zeigt die Bilder für die
  Rot-Cyan-Brille (Anaglyphen) oder wie auf einem Röhrenbildschirm (PixelRAM);
  die Wahl gilt für alle Bilder, bis man sie wieder ausschaltet.
- Anaglyphen: Eine Szene bringt ihre Kamera mit (`camera = …`, im Viewer
  »Kamera kopieren«). Die Prüfart `submit` kennt `preview: anaglyph`: Die
  Karte zeigt die gewählte Szene so, wie die Galerie sie zeigt, und die
  Klassenübersicht zeigt eine Galerie aller Abgaben, groß, drehbar und mit
  den Pfeiltasten zum Weiterblättern. Kenney-Modelle funktionieren auch in
  abgegebenen Szenen.
- »Mit der CPU Sandbox arbeiten« zeigt eine Galerie aus acht
  Beispielprogrammen. Die drei Sandbox-Seiten sagen gleich am Anfang, dass sie
  mit Dart, Python, JavaScript oder Java arbeiten.
- Das Keyboard-Tutorial ist eine eigene Aktivität; die Prüfart `keyboard`
  zeigt Lehrkräften in der Klassenübersicht, wer welches Kapitel geschafft hat.
- Handbuch-Seite »Shortcuts« zum Nachschlagen; »Farben angeben« hat einen
  Farbmischer und erklärt RGB und OKLCH.
- Code-Blöcke mit ```` ```ini ```` (Anaglyph-Szenen) sind eingefärbt, wie der
  Viewer sie liest (`src/ruby/include/anaglyph_scene_lexer.rb`): `#` am
  Zeilenanfang ist ein Kommentar, in `fill = #4aa03f` ein Farbcode.
- Der Workspace lässt sich über das Browsermenü als App installieren; eine
  Installationsleiste erscheint nicht.
- Tutorial-Screenshots: Der Generator zeichnet den Mauszeiger (`show-mouse`),
  rendert Varianten (`variants: dark anaglyph crt`), schneidet seitlich und auf
  ein Element zu (`crop-left`, `crop-right`, `crop-element`), rendert Leinwände
  scharf (`sharp-canvas`), macht Bilder aus PDF-Seiten (`pdf-page`) und
  Git-Objektgraphen (`git-graph`), öffnet den Neo4j Browser (`neo4j-browser`)
  und kennt Aktionen einzelner Tutorials (`hook: NAME ARGS`).
  `recreate-screenshots.rb --status` zeigt den Stand aller Seiten, ohne zu
  rendern; wer im Browser Tutorials aufruft, pausiert das Rendern im
  Hintergrund nur, statt es zu stören.
- Kopierknopf für Code-Blöcke, die abzutippen nichts lehrt: Adressen zum
  Klonen, Downloads, Einrichtungsbefehle (```` ```bash_copy ````). Was man
  lernen soll, wird weiter abgetippt; gemischte Blöcke sind geteilt, sodass
  nur die schwer zu tippende Zeile den Knopf hat.
- Handbuch »HTML und CSS«: drei Seiten für Webseiten und Folien zugleich,
  [HTML](/reference/html), [CSS](/reference/css) und
  [Layout](/reference/layout), dazu [Folien mit Shower](/reference/shower)
  für das, was nur Präsentationen brauchen. Jedes Beispiel ist ein erzeugtes
  Bild als Webseite und als Folie; ein Knopf in der Ecke schaltet um. Auf
  der Shower-Aktivität und den Handbuch-Seiten, die man von dort öffnet,
  erscheinen die Folien, sonst die Webseite.
- Aufgaben, die der Workspace selbst entscheidet, statt dass Lernende nur
  hinschauen: eine Liste mit drei Punkten, ein Link, `styles.css` wirklich
  eingebunden, `main` mit Flexbox, eine Media Query, die alles untereinander
  stellt. Dafür kennt die Prüfart `html` jetzt `elements`, `css` kennt
  `declares` und `linked_from`.
- BIF »Interaktive Geschichten schreiben«: 13 Aufgaben entlang der
  Geschichte bis zur eigenen Geschichte und ihrer Abgabe, eine Notiz für
  Lehrkräfte und die neue Prüfart `story`, die liest, was BIF selbst über die
  Geschichte herausgefunden hat (`.story-tools/analysis.json`), ohne etwas
  auszuführen.
- Handbuch »Interaktive Geschichten mit BIF« (`/reference/bif`): die
  Schreibweisen zum Nachschlagen und was BIFs Meldungen unter »Problems«
  bedeuten; die Aktivität bindet die Teile ein.
- MySQL: nach `ORDER BY`, `LIMIT`, `COUNT`, `LIKE` und `DISTINCT` je eine
  kurze Aufgabe.
- Klassenseite: Abgaben öffnen sich im Browser, mit Dateien, Quelltext und
  »Starten« für Webseiten und BIF-Geschichten. Sie laufen auf einem eigenen
  Host, `abgaben.<host>`, in einer Sandbox ohne Zugriff auf die Sitzung der
  Lehrkraft.
- Klassenseite: Wer eine Gruppe betreut, ohne in ihr zu stehen, sieht die
  eigenen Versuche und Abgaben in einer Zeile »Du (Lehrkraft)«.
- Klassenseite: »Anmelde-Codes zeigen« für den Beamer, groß und nur mit
  Namen, ohne E-Mail-Adressen; neue Codes erscheinen von selbst.
- Klassenseite: Reiter »Fortschritt«, »Lernende« und »Startseite«. Unter
  »Lernende« die Karten des Servers wie auf der Verwaltungsseite und jeder
  Workspace mit CPU und RAM; eine Zeile leuchtet gelb, solange ihr Workspace
  arbeitet.
- »Einstieg für Lehrkräfte« (nur für Lehrkräfte): Anmeldung mit den Codes am
  Beamer, die Klassenseite, Abgaben, das Testkonto und was der Server
  speichert, mit Bildern. Die Klassenseite und das Profil weisen darauf hin.
- Klassenseite: »Alle Abgaben löschen« unter den Abgaben einer Aufgabe, für
  eine neue Runde in einer Gruppe, nach einer Warnung.
- Klassenseite: merkt sich am Konto die zuletzt gewählte Gruppe und je Gruppe
  die zuletzt gewählte Aktivität.
- Neue Prüfart `cypher`: Abfragen in der eigenen Neo4j-Datenbank, verglichen
  mit einer Musterabfrage, wie `sql` für MySQL. Nur lesend, doppelt
  abgesichert: Der Server lässt nur lesende Abfragen durch, und Neo4j führt
  sie in einer Lese-Transaktion aus (5 Sekunden, höchstens 5000 Zeilen).
- Neue Aktivität »Ein Spiel in 64 KB« (TIC-80, unter Computergrafik): Lua,
  Konsole und Editoren, Speichern als Textdatei in `/workspace/TIC-80`, der
  Bildschirm, dann Schritt für Schritt das Spiel »Star Catcher« (Figur,
  fallende Sterne, eigene Figur im Sprite-Editor, Leben, Ton), Ideen für ein
  eigenes Spiel und eine Abgabe. Neun Aufgaben, Notiz für Lehrkräfte,
  erzeugte Screenshots aus dem echten TIC-80.
- Handbuch »MiniCPU« für die CPU Sandbox, mit drei Seiten. »MiniCPU«: die
  Stufen 8, 8S und 8X, Register, Flags, jeder Befehl mit Opcode,
  betroffenen Flags, Bytes und Takten, Stack und Aufrufregeln, X und Y,
  Takte, Maschinencode, Assembler. »Geräte und Bildschirm«: alle Ports,
  Joystick, Tastatur, Ton, Timer und Vsync, Pixelformate, Palettenchip,
  Seitenwechsel, die Paletten als Farbfelder. »MiniC«: C für die MiniCPU.
  Die Tabellen erzeugt `src/scripts/minicpu-reference.mjs` aus den Tabellen
  der Sandbox selbst. Die Aktivität »Vom Byte zum 3D-Labyrinth« verweist an
  passender Stelle darauf.
- Handbuch »SQL«: mycli, Tabellen ansehen, `SELECT`, `WHERE`, Sortieren,
  Zählen und Gruppieren, `JOIN`, `CREATE TABLE`, `INSERT`, `UPDATE` und
  `DELETE`, Werte über Platzhalter in Programmen (PHP, Ruby, Python) und
  die häufigen Meldungen von MySQL. »MySQL kennenlernen« und
  »Filmdatenbank« verweisen darauf. Code-Blöcke in PHP sind farbig; ein
  Test hatte die fehlende Sprache bemerkt.
- Neue Aktivität »Eine Klötzchenwelt in 3D« (C mit raylib, unter
  Computergrafik): eine Welt aus Blöcken in der Ich-Perspektive, Kamera aus
  zwei Winkeln, Laufen, Schwerkraft und Kollision, Zielen mit einem Strahl,
  Bauen und Abbauen, ein eigener Licht-Shader in GLSL mit Tag und Nacht,
  Fackeln und Nebel, Texturen aus dem 2D Game Studio. Acht Stufen, Aufgaben,
  Notiz für Lehrkräfte; die Vorlage ist `specht/raylib-starter`, die
  Webseite um das Programm kommt von PixelRAM.
- Handbuch »raylib« (Fenster, 2D, Kamera, 3D, Farben, Eingaben, Zeit,
  Vektoren, Kollision, eigene Flächen, Texturen, Shader, GLSL) und Handbuch
  »PixelRAM«.
- »Eine Facharbeit mit LaTeX« ersetzt die bisherige LaTeX-Seite unter
  derselben Adresse: eine kurze Facharbeit über ein Fadenpendel, von der
  leeren Datei bis zum PDF, mit Titelseite, Gliederung, Formel, Bild,
  Tabelle, Diagramm (TikZ), Quellen mit biblatex und Seitenlayout. 13
  Aufgaben, Notiz für Lehrkräfte. Alles zum Nachschlagen, die Schriften und
  die Vorlagen (Bewerbung, Lebenslauf, Ausarbeitung …) stehen im neuen
  Handbuch »Dokumente mit LaTeX«.
- Neue Aktivitäten unter »Netzwerke«: »Ein eigener Webserver« (Ruby, ohne
  Bibliothek: zuhören, antworten, Pfade und 404, Dateien aus `public`,
  Parameter und ein Zähler mit Mutex) und »Eine Pixelwand für alle«
  (WebSockets mit selbst geschriebenem Handshake, Nachrichten an alle, eine
  gemeinsame Pixelwand für die Klasse über Shared Live Apps). TCP/IP
  verweist auf beide.
- Basics: »VS Code nach deinem Geschmack« mit einer kleinen Galerie
  eingebauter Farbschemata und solcher aus dem Marketplace.
- Feld `language` im Meta-Block: Die Art einer Aktivität nennt ihre
  Sprache, »Projekt (Ruby)« oder »Projekt (C / raylib)«, über der Seite und
  auf der Karte im Katalog. Gesetzt bei 17 Aktivitäten mit einer festen
  Sprache.
- Bilder in einer Galerie (`.screen-gallery`) öffnen sich auf einen Klick
  so groß wie das Fenster, mit ihrer Unterschrift; ← → blättern durch die
  Galerie. Kleine Pixelbilder bleiben scharf, dunkle und CRT-Varianten
  bleiben erhalten. Auf dem Handy öffnet sich nichts.
- TIC-80: ein Abschnitt »Ein Himmel« mit Aufgabe: funkelnde Sterne mit
  `pix`, Sternschnuppen mit Schweif und eine Wiese. Das fertige Spiel ist
  das neue Bild auf der Karte.
- Handbuch-Abschnitt »Netzwerk und Daten« mit drei Seiten. »HTTP«: Anfrage
  und Antwort, Methoden, Statuscodes, Header, Adressen, `curl`, der Reiter
  »Network« im Browser, Formulare und JSON, HTTPS, WebSockets. »JSON«:
  Aufbau, was JSON nicht erlaubt (mit den echten Fehlermeldungen), Ruby,
  JavaScript, Python, `jq`. »YAML«: Aufbau, Listen, lange Texte, die Fallen
  (`no`, `1.10`, `12:30`, `01234` in Ruby und `yq`), Front Matter, `yq` und
  Ruby. Webserver, Pixelwand, TCP/IP und Filmdatenbank verweisen darauf.
- Neue Aktivität »Eine Website mit Hugo« unter »Web & Schnittstellen«: ein
  Lexikon über die Planeten aus Markdown-Seiten mit Front Matter, ein eigenes Layout
  (`baseof.html`, `page.html`, `home.html`, `list.html`) mit Navigation,
  Startseite, CSS und Schlagwörtern, dann `hugo` und `public` zum
  Veröffentlichen auf der eigenen Subdomain. Neun Aufgaben, Notiz für
  Lehrkräfte, ohne fertiges Theme.
- Neue Aktivität »Daten aus dem Netz« unter »Web & Schnittstellen«: der freie
  Wetterdienst Open-Meteo als Beispiel für eine API, erst mit `curl` und
  `jq`, dann aus Ruby mit Fehlerbehandlung und Cache, zum Schluss eine eigene
  Wetterseite mit `fetch`. Erklärt Query-Parameter, Fehler, die Grenzen einer
  API (alle Workspaces einer Schule teilen sich eine Adresse), CORS und
  warum API-Schlüssel nicht in eine Webseite gehören. Acht Aufgaben, Notiz
  für Lehrkräfte. Die Reihen im Abschnitt: Webseiten (HTML & CSS, Hugo,
  Svelte), dann Netzwerk (Daten aus dem Netz, Webserver, Pixelwand).
- Neue Aktivität »Kryptographie im Terminal« unter »Terminal & Git«:
  Hashes mit `sha256sum`, wie Server Passwörter mit Salz und absichtlich
  langsamen Hashfunktionen speichern, Verschlüsselung mit einem Schlüssel
  (AES mit `openssl enc`) und mit zweien (RSA mit `openssl pkeyutl`),
  Unterschriften mit Ed25519 und `ssh-keygen -Y`, Fingerabdrücke und was
  davon in SSH, Git und HTTPS steckt. Ein Partnerteil tauscht öffentliche
  Schlüssel über Port 1234. Zehn Aufgaben, Notiz für Lehrkräfte.
- »SQL Injection« steht wieder im Katalog unter »Datenbanken«. Das Labor
  läuft in einer weiteren Datenbank aus dem Profil mit ihrem eigenen Zugang,
  sodass die Lücke nur diese Testdatenbank erreicht. `login.rb` braucht nur
  noch `mysql2` (`gem install mysql2`) und hat englische Namen; die
  Aufgaben sind Karten, dazu eine Prüfung des Prepared Statements und zwei
  Fragen, warum es schützt und warum ein eigener Zugang den Schaden
  begrenzt. Hinweis auf die Klartext-Passwörter und auf »Kryptographie im
  Terminal«, Notiz für Lehrkräfte.

### Geändert

- Automatron zeichnet schöner: in der Schrift der Seite, mit tiefgestellten
  Nummern (s₀ statt eines losen »s 0«), Bereichen wie `0, 2–9` statt langer
  Listen, Farben für das helle und das dunkle Design und akzeptierenden
  Zuständen in Grün. Kleine Automaten stehen wie im Schulbuch in einer Reihe,
  nur die Übergänge zurück laufen als Bögen.
- Screenshot-Generator: `crop-element` scrollt ein Element unterhalb des
  sichtbaren Bereichs erst ins Bild, statt eine leere Fläche aufzunehmen.
- Sicherheit: Befehle, die Lernende in eine Aufgabe vom Typ `command` tippen,
  laufen nicht mehr als die Person, sondern als Gast (`nobody`): Sie können
  `/workspace` lesen, aber nichts darin ändern oder löschen.
- SQL-Aufgaben: Das eigene Ergebnis steht immer sichtbar als Tabelle unter
  der Rückmeldung. Gefragt sind genau die verlangten Spalten (`extra_columns`
  ist jetzt standardmäßig `wrong`); Fehlermeldungen von MySQL sind auch im
  hellen Modus lesbar und werden umgebrochen.
- Keine Browser-Popups mehr: Löschen fragt in einem eigenen Dialog nach,
  Fehler erscheinen als Meldung auf der Seite.
- Die Kopfleiste hat einen starken Knopf (»Workspace öffnen« bzw.
  »Anmelden«); alle anderen sehen gleich aus, die aktuelle Seite ist markiert.
  Das Profil steht rechts und zeigt den Namen oder die E-Mail-Adresse.
- Die Startseite begrüßt nicht mehr mit »Hallo …«; über den eigenen
  Aktivitäten steht schlicht »Deine Aktivitäten«. Wer angemeldet ist, steht
  wie bisher rechts in der Kopfleiste.
- Das Handbuch zeigt alle Kapitel als gleich große Karten mit ihren
  Überschriften als Links, ohne Bilder, und hat ein Suchfeld. Überschriften auf
  Handbuch-Seiten haben Anker.
- SQL-Aufgaben prüfen nicht, wenn die Musterabfrage nichts findet (meist
  fehlen dann die Daten); sie sagen das und wie man die Daten lädt.
- Die drei Sandboxes haben einen eigenen Abschnitt im Katalog statt
  »Terminal & Git«.
- Sprünge zu einer Aufgabe (»Weitermachen«) landen unter der Abschnittsleiste.
- »Eine Präsentation in HTML erstellen« ist kürzer: Der Referenzteil steht
  jetzt unter `/reference/html-folien`. Alte Links auf Abschnitte der Seite
  führen dorthin weiter. Die Mini-Folien liegen in
  `src/content/reference/html-folien/screenshots/`; `screenshot-range.mjs`
  schreibt dorthin.
- Eine Seite ist immer `<slug>.md` aus ihrem Ordner. Bisher war es die erste
  `.md`-Datei; bei allen vorhandenen Seiten ist das dieselbe.
- Abgaben mit `pick`: Die Liste der Dateien wird neu gelesen, sobald man
  sie anklickt oder zur Seite zurückkehrt. Eine eben im Workspace angelegte
  Szene steht also gleich darin; die Auswahl bleibt, solange die Datei da ist.
- Tutorial-Screenshots werden seltener neu erzeugt: Ein neu gebautes
  Workspace-Image zählt nur noch, wenn sich die code-server-Version ändert,
  und Zuschnitt oder Dateiname eines früheren Rezepts machen spätere nicht
  mehr ungültig. Bestehende Manifeste bleiben gültig.
- Anaglyphen: »Die Latte« heißt jetzt »Der Balken« (`balken.ini`, Aufgabe
  `balken`). »Sonne, Erde, Mond« verlangt eine gelbe Sonne, eine blaue Erde,
  einen weißen Mond und einen schwarzen Hintergrund statt `name = …`, das das
  Tutorial nie eingeführt hat. Wer `latte` schon gelöst hat, sieht die
  Aufgabe wieder offen.
- Filmdatenbank: Statt »The Matrix« dient »Back to the Future« (1985) als
  Beispiel, im Datensatz vom Anfang und in den JOIN-Aufgaben (jetzt
  `genres-film` und `besetzung-film`).
- ER-Werkstatt: Im Chen-Diagramm stehen die Attribute gleichmäßig um ihre
  Entität, zwischen den Linien zu den Beziehungen und in der Reihenfolge der
  Datei. Die Vorschau zoomt (−, +, »Einpassen«, Strg + Mausrad) und springt
  beim Tippen und Ziehen nicht mehr: Die Ansicht bleibt, Kästen bleiben
  stehen, nur neue werden platziert.
- Profil: Der Hinweis nennt die Umgebungsvariable richtig, `NEO4J_DATABASE`
  statt `NEO4J_DB`.
- ER-Werkstatt 0.2.0: Der Primärschlüssel heißt im Modell `primary key`, wie
  in SQL (`id: int, primary key`). `key` allein funktioniert weiter; die
  Erweiterung schlägt dann `primary key` vor. Filmdatenbank, Handbuch und
  Prüfart `er` sprechen von `primary key`. Der Text zum Krähenfuß sagt, warum
  das erste Diagramm keinen Strich zeigt, und zeigt ihn im Tabellendiagramm.
- Code in Aufgabenkarten ist eingefärbt wie im Text der Seite; bisher sah ein
  Code-Block in einer Aufgabe aus wie lauter einzelne Inline-Code-Stücke. Ein
  Code-Block ohne Sprache wird wie ```` ```text ```` dargestellt.
- Das Inhaltsverzeichnis in der Seitenleiste zeigt Kapitel mit ihren
  Abschnitten und steht auf jeder längeren Seite.
- Keine Seite ist breiter als ein Handybildschirm.
- Links haben auf der ganzen Website dasselbe Petrolblau.
- Das Handbuch setzt beim Öffnen den Cursor ins Suchfeld (nicht auf
  Touch-Bildschirmen, wo sonst die Tastatur die Seite verdeckt).
- Der Profil-Knopf zeigt ein Ausweis-Symbol.
- Das 2D Game Studio ist ein eigener Spielplatz (2d.hackschule.de) und hat
  keine Seite mehr im Workspace; damit entfällt auch »Lehrmaterial«. Die
  Liste der Aktivitäten mit einer Notiz für Lehrkräfte steht jetzt im Profil
  unter »Notizen für Lehrkräfte«. `/lehrmaterial` führt dorthin,
  `/lehrmaterial-2d` zum Game Studio.
- »Statische Webseiten mit HTML & CSS« und »Eine Präsentation in HTML
  erstellen« holen ihre Erklärungen aus denselben Handbuch-Abschnitten. Wer
  eine der beiden schon gemacht hat, sieht sie in der anderen nur noch als
  eine Zeile. »HTML-Folien gestalten« ist in den neuen Seiten aufgegangen;
  alte Links führen weiter. KaTeX war in der Vorlage nur scheinbar
  eingerichtet: Das Handbuch zeigt jetzt die drei Zeilen für den `<head>`.
- Die kleinen Verweise auf Handbuch-Abschnitte (`_uses`) nennen deren
  Überschrift statt des internen Namens.
- Auf dem Handy rücken Hinweise, Aufgabenkarten und Code-Blöcke weniger weit
  ein; das Symbol eines Hinweises steht im Text statt daneben.
- Jede Karte im Katalog nennt ihre Art (Projekt, Labor, Grundlagen, Übung,
  bei den Programmiersprachen »Steckbrief«), klein rechts neben dem Titel,
  wenn er Platz lässt; bisher hatten nur elf Karten ein Etikett, auf dem Bild.
  Die Kurztexte sind im Blocksatz, die Karten einer Reihe gleich hoch.
- Silbentrennung macht der Browser für jedes Wort; die Liste
  `hyphenation.txt` mit 177 Wörtern ist weg. Code wird nie getrennt.
- Ein Handbuch-Kasten, der auf eine Zeile zusammengeklappt ist, zeigt die
  Adresse zum Klonen trotzdem, mit Kopierknopf.
- Eine Anaglyph-Szene abzugeben braucht keinen laufenden Workspace mehr: Ob
  sie lädt und genug Objekte hat, prüft die Karte mit dem Bild, das sie
  ohnehin zeichnet. Während der Abgabe zeigt der Knopf, dass er arbeitet.
- »Repository klonen« steht einmal im Handbuch und wird in den Tutorials
  eingebunden statt kopiert.
- LaTeX: Die Hilfsdateien beim Übersetzen landen in `.build`; im Ordner
  bleiben nur `.tex` und `.pdf`.
- Startseite: ruhigerer Katalog, beide Filmdatenbanken in einer Reihe,
  »Bobby Tables« vorerst ausgeblendet. »Pixelgrafik programmieren« heißt
  jetzt »Pixelgrafik mit Ruby«, neben »Echtzeitgrafik mit C«.
- Erzeugte statt von Hand gemachte Screenshots in Git, Dateien und
  Verzeichnisse, TCP/IP, LaTeX, MySQL, beiden Filmdatenbanken, den Sandboxes,
  den Grundlagen, Anaglyphen und PixelRAM. »Go Live« ist ein gemeinsames Bild
  für alle Tutorials, mit Mauszeiger und dem Anhalten des Live Servers.
- Behoben: Vier kleine VS-Code-Symbole fehlten seit August auf 17 Seiten
  (Programmiersprachen, LaTeX, Svelte, Pixelgrafik). KaTeX lud auf
  Handbuch-Seiten nicht. Die drei Übungen (»Programmieren üben«) waren seit
  dem Ausblenden nicht mehr erreichbar, obwohl das Profil auf sie verweist;
  ein Abschnitt kann jetzt mit `hidden: true` aus dem Katalog verschwinden,
  ohne dass seine Seiten wegfallen. `/profil` führt Abgemeldete zur
  Anmeldung statt zu einem Serverfehler. Auf dem Handy scrollen breite
  Formeln und Tabellen für sich, statt die Seite zu verbreitern.
- Barrierefreiheit: Die Symbole in Knöpfen sind als Schmuck markiert. Bisher
  las ein Screenreader das Symbolzeichen mit, etwa »\uF116 Neu starten«.
- Erzeugte Screenshots auch für »Statische Webseiten mit HTML & CSS« (alle
  Bilder, mit den Ständen der Beispielseite in `lehrkraft/`) und für die
  geklonte Shower-Vorlage.
- Erzeugte statt von Hand gemachte Screenshots für alle 19
  Programmiersprachen-Tutorials (Python, C, C++, C#, Java, Go, Rust, Dart,
  JavaScript, Lua, Ruby, BASIC, Fortran, COBOL, Pascal, Smalltalk, Lisp,
  Erlang, Assembler). Die Bilder aus August 2024 zeigten ein altes VS Code,
  eine Eingabeaufforderung wie `abc@992fa5e1a3fe` und ältere Versionen der
  Sprachen; jedes neue Bild wurde neben das alte gelegt und zeigt denselben
  Schritt. Die Fehlerbilder zeigen jetzt den Fehler, den der Text beschreibt
  (`hello-error.*`). Wo eine Erweiterung nach »Trust Publisher & Install«
  fragt, sagt der Text das jetzt. Das Assembler-Tutorial hat das fehlende
  Bubblesort-Programm (`nasm/bubblesort.asm`) bekommen.
- »Pixelgrafik mit Ruby« überarbeitet: acht Aufgaben (das erste Programm,
  Koordinaten, Speicher mit Palette, Bézierkurven, zwei Vorhersagen zu
  `flood_fill` und zur Maske, das gespeicherte PNG), ein neuer Abschnitt
  »Dein eigenes Bild« mit Ideen und einer Abgabe von `bild.rb` und
  `bild.png`, eine Notiz für Lehrkräfte und erzeugte Screenshots. Die Seite
  nennt die Meldung, wenn die Zeichenfläche nicht offen ist.
- »Frontend-Entwicklung mit Svelte« auf SvelteKit 3 gebracht (`sv` 1.1,
  Einstellungen in `vite.config.js`), mit neun Aufgaben, einer Abgabe des
  gebauten Ordners `build`, einer Notiz für Lehrkräfte und erzeugten
  Screenshots aller Schritte, von `npx sv create` bis `npm run build`. Die
  Vorschau läuft über den Reiter »Ports«; dafür braucht der
  Entwicklungsserver `host: '127.0.0.1'`, das die Seite erklärt.
- Prüfart `css` kennt `style_tags: true` und liest dann nur den
  `<style>`-Block, etwa einer Svelte-Komponente. `html` versteht
  Svelte-Ausdrücke wie `disabled={state < 3}` in Attributen.
- Behoben: Dateien, die das Keyboard-Tutorial geöffnet hatte, tauchten
  später immer wieder in <kbd>Strg</kbd><kbd>P</kbd> auf. Sie sind jetzt
  ausgeschlossen (`**/.hs-kbd-tutorial/**` in `files.exclude`); VS Code
  nimmt sie beim nächsten Start auch aus dem gespeicherten Verlauf.
- Der Knopf »TIC-80« im Menü zeigt TIC-80s kleinen Computer als Strich-Icon
  wie die übrigen Icons; der Tab von TIC-80 zeigt das bunte Original.
- TIC-80 1.3.1 statt einer Entwicklerversion von Juli, mit Forth als neuer
  Sprache. Speichern im Workspace ist robuster: TIC-80 schickt nur die
  Dateien, die es geändert hat. Hat sich eine Datei außerhalb von TIC-80
  geändert (VS Code, `git pull`), bleibt sie, und der Stand aus TIC-80
  landet daneben, etwa in `spiel (TIC-80).tic`. Bisher blockierte jede
  Änderung im Ordner `TIC-80` alle weiteren Speicherungen, bis man neu lud,
  und das verlor den Stand im Browser. Was außerhalb geändert wurde, sieht
  TIC-80, sobald das Fenster wieder den Fokus hat. Meldungen auf Deutsch.
  Der CRT-Filter (Röhrenfernseher-Look) ist anfangs aus, damit Code im
  Editor scharf bleibt; in den Einstellungen lässt er sich einschalten.
- Git-Aufgaben (»Git kennt dich«, »Committet und gepusht« …) prüfen auch,
  wenn der Workspace aus ist: in einem Lese-Container mit demselben Image,
  ohne Netz, der Ordner nur lesbar. Bisher hieß es dann »Starte zuerst
  deinen Workspace«, auch auf der Klassenseite.
- »Filmdatenbank als Graph mit Neo4j«: Die Abfrage-Übungen aus den Kästen
  sind jetzt sieben geprüfte Aufgaben. »Toy Story« von 1995, nach dem eine
  Übung fragte, gibt es in den Daten nicht; jetzt ist es »The Lion King« von
  1994 (es gibt auch eine Fassung von 2019). Statt der »zehn Personen mit den
  meisten Filmen«, bei denen Platz 9 bis 11 gleichauf liegen, fragt die
  Aufgabe nach allen mit mindestens 15 Filmen.
- Behoben: Rückmeldungen für typische Fehler (`same_result_as`) griffen bei
  SQL-Aufgaben mit verlangter Reihenfolge nie.
- Auf dem Tablet bleiben die Schalter auf breiten Bildern (CRT-Filter,
  Anaglyph, dunkel) rechts oben stehen, während man das Bild zur Seite
  schiebt; bisher lagen sie am rechten Rand des Bildes, außer Sicht.
- Handbuch »Interaktive Geschichten«: die 17 Abschnitte in sechs Gruppen
  (Eine Geschichte schreiben, Was sich die Geschichte merkt, Ordnung im
  Projekt, Gestalten, Für Fortgeschrittene, Wenn etwas nicht klappt), mit
  Unterabschnitten für »Aussehen anpassen«. Adressen und eingebundene Teile
  bleiben gleich.
- Die Verweise ins Handbuch unter Abschnitten (»Mehr zum Text auf Folien:
  Überschriften und Absätze …«) sind kleine Knöpfe im Türkis des Handbuchs
  mit dessen Buch-Symbol statt beiger Pillen.
- Die Leiste mit den Kapiteln, die oben erscheint, sobald die Kopfzeile aus
  dem Bild ist, hat links »Nach oben« (Chevron): zurück zur Kopfzeile mit
  dem Weg zur Startseite und dem ganzen Menü. Die Linien unter den Kapiteln
  gehören jetzt zu ihren Einträgen und bleiben auch beim seitlichen Scrollen
  darunter; an einem Rand, hinter dem noch Kapitel kommen, blendet die
  Leiste weich aus. Solange sie verborgen ist,
  springt Tab nicht hinein.
- Klassenseite: »Zurück« im Browser schließt die Anmelde-Codes und den
  Abgabe-Viewer, statt die Seite zu verlassen. »Vorlage aktuell?« steht auf
  einer Linie mit »Jetzt nachsehen«.
- C#: Die Seite übersetzt und startet Programme mit `dotnet run Datei.cs`
  (.NET 10) statt mit Mono (`mcs`, `mono`). Workspaces starten mit
  `DOTNET_CLI_TELEMETRY_OPTOUT=1`: .NET schickt keine Nutzungsdaten an
  Microsoft, zeigt kein Begrüßungsbanner und legt kein HTTPS-Zertifikat an.
- Das Profil hat keinen Abschnitt »Notizen für Lehrkräfte« mehr; die
  Notizen stehen oben auf den Aktivitäten. `/lehrmaterial` führt zum
  »Einstieg für Lehrkräfte«.
- Lange Code-Blöcke (ab 20 Zeilen) stehen in derselben etwas kleineren
  Schrift wie die Listings aus `_include_file`.
- Die Kurzzeile eines eingeklappten Handbuch-Kastens zeigt Tasten als
  `<kbd>`, wie im übrigen Text; vorher standen die Tags als Text da.
- Handbuch »Workspace«: neuer Teil »Erweiterung installieren«, mit der Frage
  »Trust Publisher & Install«. »Farben« nennt die RGB-Schreibweise von
  Pixelflow Canvas.
- Inhaltsverzeichnisse aufgeräumt. Handbuch »HTML« (Aufbau einer Seite,
  Text, Bilder) und »CSS« (Grundlagen, Gestalten) gliedern sich in Gruppen;
  »Tastatur-Shortcuts« und »Terminal« haben keine Abschnitte mehr mit nur
  einem Unterpunkt. Bei den Aktivitäten fassen »Vorbereitung« (LaTeX) und
  »TIC-80 kennenlernen« den Anfang zusammen; »Statische Website« ordnet
  »Ein eigenes Bild verwenden« unter HTML ein, Flexbox und kleine
  Bildschirme unter CSS. Anker und Teile bleiben gleich.
- Ein Handbuch-Kasten in einer Aktivität erscheint nicht mehr im
  Inhaltsverzeichnis der Seite; seine Zwischenüberschriften stehen immer
  eine Stufe unter dem Abschnitt, egal wie tief der Teil im Handbuch sitzt.
- SQL- und Cypher-Aufgaben: Das Antwortfeld färbt die Abfrage beim Tippen
  wie die Code-Blöcke der Seite (dieselben Farben, dieselben Schlüsselwörter
  wie Rouge; bei Cypher Labels, Beziehungstypen und Eigenschaften wie in
  den Beispielen). Ein fehlendes Anführungszeichen fällt sofort auf. Das
  Feld wächst mit der Abfrage.
- Ein Test prüft, dass jede Überschrift einer Handbuch-Seite auch als
  Überschrift erscheint: Ein `</table></div>` am Zeilenende ließ Redcarpet
  sonst den Rest der Seite als rohen Text ausgeben.
- Haken sind Symbole aus Bootstrap Icons statt Unicode-Zeichen: in den
  Flag-Spalten des MiniCPU-Handbuchs und bei »Deine Kopie … ist aktuell« auf
  der Startseite. `no_check_marks_test.rb` hält das so (Code ausgenommen).
- MiniCPU-Assembler in Code-Blöcken (```` ```minicpu ````) ist farbig wie im
  Editor der CPU Sandbox: Befehle, Register, Marken, Direktiven, Zahlen,
  Kommentare. Vorher blieben die Blöcke im Handbuch ungefärbt (```` ```asm ````
  kannte die Seite nicht), die Aktivität nutzte das x86-Schema von NASM.
  Ein Test prüft, dass jede Sprache an einem Code-Block bekannt ist; dabei
  fiel ```` ```javascript ```` auf (p5.js), das jetzt wie `js` gefärbt wird.
- Handbuch »MiniCPU« mit Screenshots aus der CPU Sandbox: Register und
  Flags nach `SUB A, B`, der Stack nach `CALL`, Adressen und Bytes im
  Editor, die Takte, die Geräteleiste, der Bildschirm mit seiner Leiste und
  der Reiter »Assembly« für ein C-Programm. Die Programme sind die
  Beispiele auf der Seite. Das C-Beispiel hatte Kommentare hinter
  `#pragma`, die MiniC nicht versteht; sie stehen jetzt darüber.
- Handbuch-Tabellen auf dem Handy: Jede Zeile steht als Block untereinander,
  kurze Werte nebeneinander und mit ihrer Spaltenüberschrift beschriftet
  (»Bytes: 2«). Nichts ist mehr breiter als der Bildschirm.
- Ein Test für die Erweiterungen der Leistungsüberprüfungen braucht kein
  `zip` mehr, das im Ruby-Image fehlt.
- Die Listen der erzeugten Screenshots (`.tutorial-screenshots.json`) sind
  nach Namen sortiert. Ein Rendern in anderer Reihenfolge ändert die Datei
  nicht mehr.
- Profil, MySQL: Jede Datenbank ist eine Karte mit Name, Art, Tabellen,
  Einträgen und Größe, ihrem Knopf zum Zurücksetzen oder Löschen und ihren
  Zugangsdaten. Unter den Karten steht, wie viele weitere Datenbanken noch
  möglich sind. Auf dem Handy bleiben Feld und Knöpfe in einer Zeile (die
  Knöpfe als Symbole). Der Hinweis nennt die richtige Variable
  `MYSQL_DATABASE` statt `MYSQL_DB`.
- Jede weitere MySQL-Datenbank aus dem Profil hat einen eigenen Zugang
  (Benutzername wie die Datenbank), der nur diese Datenbank erreicht. Das
  Profil zeigt ihn unter der Datenbank, mit Knöpfen zum Kopieren. Gedacht
  für Programme, die andere benutzen, etwa ein geteiltes Gästebuch.
  Bestehende weitere Datenbanken bekommen ihren Zugang, wenn das Profil sie
  das nächste Mal anzeigt; mit der Datenbank verschwindet auch er.
- Behoben: Jede angemeldete Person konnte über das Profil eine beliebige
  MySQL-Datenbank löschen, wenn sie deren Namen kannte. Gelöscht werden
  jetzt nur die eigene Datenbank (zurückgesetzt) und die selbst
  angelegten; Namen aus anderen Zeichen als Buchstaben, Ziffern und `_`
  werden abgelehnt.
- Behoben: »Git kennt dich« schlug fehl, obwohl `git config --global` Name
  und E-Mail kannte. Die Prüfung lief mit dem `HOME` des Images (`/config`)
  statt mit dem der Lernenden (`/workspace`) und fand `.gitconfig` nicht.
- Behoben: Lehrkräfte konnten mit einer E-Mail-Adresse jeden Workspace
  öffnen, nicht nur die ihrer Gruppen.
- Lernende in mehreren Gruppen (etwa Klasse 12 und LK) sehen alle
  Lehrkräfte dieser Gruppen: Anmelde-Code und Workspace. Der Name muss nur in
  einer Zeile der Einladungen stehen.
- Lehrkräfte finden auf der Klassenseite die Anmelde-Codes ihrer Gruppe und
  bei jeder Person »Workspace öffnen« und »Nachahmen«; mit »Zurück zu
  deinem Konto« geht es zurück. Vorher nennen sie in einem Satz den Grund,
  den die Person bei der nächsten Anmeldung liest. Jede Lehrkraft hat ein
  Testkonto in einer eigenen Testgruppe, um alles aus Sicht der Lernenden
  auszuprobieren. Die Verwaltungsseite ist nur noch für Admins.
  Beschreibung: `docs/teacher-access.md`.
- Behoben: Lehrkräfte konnten mit einer Adresse jede Person in ihre
  Leistungsüberprüfung aufnehmen, nicht nur ihre eigenen Lernenden.
- Aufgabenkarten: klarer und lebendiger. Farbe zeigt den Stand, Antworten
  sind Kacheln, ein gelöster Haken erscheint animiert neben dem Knopf. Was
  ein Check liest, steht hinter einem (i); Einzelheiten eines Ergebnisses
  und die Lösung öffnen sich erst auf Nachfrage.
- Farben: Der Farbmischer springt nicht mehr, und der Hinweis auf Farben,
  die ein Bildschirm nicht zeigen kann, ist weg.
- Handbuch: Die Seite heißt »Tastatur-Shortcuts«.
- Klassenseite: Die Titel der Aufgaben stehen aufrecht und ganz über ihren
  Spalten, »Jetzt nachsehen« ist ein Menü, und beim Öffnen eines Workspaces
  dreht sich das Code-Symbol wie auf der Startseite.
- Abgaben zeigen überall denselben gefalteten Papierflieger (Font Awesome
  Free, CC BY 4.0).
- VS Code startet mit dem Farbschema »Dark 2026«. Wer noch das bisherige
  Standardschema hatte, bekommt das neue; ein selbst gewähltes Schema
  bleibt. Alle Screenshots sind neu gerendert.
- Dunkler Modus: Git-Graphen, ER-Diagramme und Bilder mit schwarzer
  Schrift auf Weiß passen sich an (`class='invert-dark'`); Neo4j-Browser,
  BIF-Entwickleransicht und die Karten von Anaglyph, BIF und »Über den
  Workspace« haben dunkle Varianten.
- Code in GLSL (```` ```glsl ````) ist farbig, ```` ```sh ```` wie
  `bash`.
- Kopierknopf auch für große Blöcke ab etwa 15 Zeilen, die in eine Datei
  eingefügt werden (ganze Dateien, lange Funktionen, Shader); kurzer Code
  wird weiter abgetippt. Regel in `docs/aktivitaet-schreiben.md`.
- Code in den neuen Aktivitäten (raylib, Webserver, Pixelwand) und in der
  raylib-Vorlage hat englische Namen; die Kommentare bleiben deutsch.
- Screenshot-Generator: Hooks bekommen `writeFile`; `pdf-page-full:` zeigt
  eine ganze PDF-Seite mit ihren Rändern (die Seiten der Facharbeit).
- »Ein eigener Webserver«: Lernende probieren den Path-Traversal-Angriff
  mit `curl --path-as-is` selbst aus, mit und ohne die Prüfung. »Statuszeile«
  heißt dort jetzt »Status-Line«, wie »Request-Line«.
- Code in YAML (```` ```yaml ````) ist farbig.
- »Eine Pixelwand für alle« nutzt faye-websocket statt eines selbst
  geschriebenen Handshakes und Frame-Parsers: Die Seite beginnt mit einem
  Echo in zehn Zeilen, der Handshake bleibt mit `curl` sichtbar. Neuer
  Abschnitt »Viele Threads«: Vier Threads arbeiten eine Warteschlange ab,
  ein Zähler verzählt sich bei 200 Pixeln auf einmal (`flood.js`), ein Lock
  behebt es. Die Seite sagt deutlich, dass das Überschütten fremder Server
  ein Angriff ist.
- Handbuch »HTTP«: Die Teile einer Adresse als Grafik aus HTML und CSS,
  mit Klammern und Namen unter jedem Teil.
- Screenshot-Generator: `preview-port: 1313 /axolotl/` öffnet eine Seite
  unterhalb der Wurzel einer App.
- »Ein eigener Webserver« bekommt eine sechste Stufe, »Ein Formular
  annehmen«: eine Umfrage mit `method="post"`. Der Server hebt die Header in
  einem Hash auf, liest den Body so lang, wie `Content-Length` sagt, zählt
  die Stimmen unter einem Lock und leitet mit `303 See Other` auf das
  Ergebnis weiter (Post/Redirect/Get). Drei neue Aufgaben; die Klasse kann
  über Shared Live Apps abstimmen. Das Handbuch »HTTP« nennt `303 See Other`.
- »TCP/IP-Verbindungen« ist die erste Hälfte von TCP → HTTP → Webserver:
  Neu sind die Schichten (Netzzugang, IP, TCP, Anwendung), Ports und der
  Handshake, den Lernende mit `curl -w '%{time_connect}'` messen und mit
  `ping` vergleichen. `netcat` geht jetzt auch allein mit zwei Terminals,
  und »HTTP von Hand« holt mit `nc -C info.cern.ch 80` die erste Webseite
  der Welt von 1992. Der Server mit der Endung `.pe` ist jetzt ein Rätsel
  (die Antwort kommt aus Belgien) statt ein Beispiel für einen fernen
  Server. Fünf neue Aufgaben, eine Notiz für Lehrkräfte, alle Bilder mit
  Alt-Text und aus Rezepten; nicht mehr benutzte Dateien sind entfernt.
- »Git und GitHub«: Die Seite behauptete, ohne `git mv` bemerke Git eine
  Umbenennung nicht. Jetzt erklärt sie, dass Git Umbenennungen gar nicht
  speichert, sondern am gleichen Inhalt erkennt; `mv` und `git add -A`
  ergeben denselben Commit.
- Sicherheit: Was der Server als root in den Ordner einer Person schreibt
  oder dort liest (`.hackschule`, `.bashrc`, `.gitconfig`, `.my.cnf`,
  `.myclirc`, die VS-Code-Einstellungen, die Marker und `.git/info/exclude`
  in Prüfungen, die TIC-80-Dateien), geht jetzt durch `UserFiles`: Jeder
  Pfad wird Ordner für Ordner geöffnet, ohne einem symbolischen Link zu
  folgen, und nur reguläre Dateien werden gelesen oder geschrieben. Bisher
  konnte ein Link im eigenen Workspace den Server dazu bringen, fremde
  Dateien zu überschreiben und der Person zu übergeben. Ein abgelehnter
  Pfad steht im Log; der Workspace startet trotzdem. TIC-80 schreibt
  Dateien nicht mehr per Umbenennen, sondern direkt.
- Sicherheit: Der Anaglyphen-Viewer für die Vorschau auf der Aufgabenkarte
  und die Galerie der Klassenseite läuft nicht mehr auf der Website, sondern
  auf `abgaben.<host>` in einem Frame mit `sandbox`, wie die
  Abgaben-Ansicht. Die Ausdrücke einer Szene sind JavaScript; bisher liefen
  sie mit der Sitzung dessen, der die Galerie ansah, also auch einer
  Lehrkraft. Die Seite holt für jede Szene eine signierte Adresse
  (`/api/anaglyph/address`), der Viewer schickt Bild, Fehler und die Zahl
  der Objekte als Nachricht zurück. Unter `/anaglyph-viewer/` antwortet die
  Website selbst nicht mehr. Der Viewer (`specht/anaglyph` 114bc3b) schickt
  seine Nachrichten dafür selbst richtig; der Server ergänzt nur einen Ersatz
  für `sessionStorage`.
- Sicherheit: Rückmeldungen der Aufgaben enthalten Text aus den Dateien der
  Lernenden (eine CSS-Zeile, ein Branch-Name, der Name einer Szene). Das
  HTML, das Aufgabenkarte und Klassenseite davon anzeigen, wird jetzt nach
  dem Markdown auf eine kurze Liste harmloser Elemente und Attribute
  gefiltert (`TaskHtml`): kein Skript, keine Event-Attribute, keine
  `javascript:`-Links, keine Frames. `<kbd>`, Code, Listen, Links und
  Bootstrap-Icons der Autorinnen und Autoren bleiben. Bisher konnte eine
  präparierte Datei Skript in die Sitzung einer Lehrkraft bringen, die
  »Jetzt nachsehen« drückte.
- Sicherheit: Das Cookie `hs_server_sid`, mit dem nginx den Zugang zu einem
  Workspace prüft, geht nicht mehr an code-server und an Apps auf
  `<tag>-<port>` weiter, ebenso `hs_watch_tag`; eigene Cookies einer App
  bleiben. Bisher konnte ein Programm im Workspace das Cookie einer
  Lehrkraft lesen, die ihn ansah. Beim Abmelden bekommt die Person einen
  neuen Wert, ein abgegriffener gilt dann nicht mehr; andere Geräte
  bekommen den neuen bei ihrer nächsten Anfrage.
- Sicherheit, Anmeldung: Der Anmeldecode steht nicht mehr im Log. Gibt es
  für eine Adresse keine Einladung, sagt die Anmeldeseite das jetzt
  ausdrücklich. Neue Grenzen: höchstens 20
  Codes pro Adresse und Tag, und pro Absender-Adresse 120 Code-Anfragen
  und 300 Anmeldeversuche in zehn Minuten (großzügig, weil eine ganze
  Schule über eine Adresse ins Netz geht). Die Zähler liegen nur im
  Speicher.
- Sicherheit, Prüfungen: Starten, Beenden und Schüler zuordnen geht nur
  noch für die eigene Prüfung (oder als Admin). Beim Löschen einer Prüfung
  wird ihre Erweiterungsliste nur entfernt, wenn die Prüfung wirklich der
  Lehrkraft gehörte. Den Startzustand eines fremden Workspace sehen nur
  noch Lehrkräfte dieser Person, und ein Startfehler erscheint im Browser
  nur noch als »Der Workspace ließ sich nicht starten.«; die Einzelheiten,
  in denen Passwörter stehen konnten, stehen nur im Log.
- Neue Bilder auf den Karten von »Daten aus dem Netz«, »Ein eigener
  Webserver«, »Eine Pixelwand für alle«, »TCP/IP-Verbindungen« und
  »Kryptographie im Terminal«: eigene Rezepte am Ende der Seite
  (`<img class='teaser' hidden>`), mit wenigen großen Dingen statt einer
  ganzen Seite, die auf der Karte unlesbar klein wird. Die Ergebnisseite der
  Umfrage im Webserver zeichnet jetzt für jede Antwort einen Balken mit
  `<progress>`.
- Schönere Seiten zum Abschreiben: Der Webserver bekommt ein `style.css`
  mit einer weißen Karte, Antworten als Kacheln und Balken in der Farbe der
  Seite; die Wetterseite zeigt die Temperatur groß und die Tage als Karten
  und baut ihr HTML nur noch mit `textContent`; die Pixelwand steht in der
  Mitte, mit runden Farbknöpfen.
- Kleinigkeiten aus der Durchsicht: Das Symbol für Erweiterungen im Handbuch
  »Workspace« (und in den Seiten, die es einbinden) erscheint wieder;
  Anmeldung, Profil, Admin und Lehrmaterial heißen im Browser-Tab nicht
  mehr »Mit dem Handy anmelden«, sondern »Anmelden«; jede Seite lädt
  `moment.js` nicht mehr, das nirgends benutzt wurde; der Link von raylib
  zu den Wellen in PixelRAM springt an die richtige Stelle.
- Sachfehler und Tippfehler: Rust steht mit 2015 (Version 1.0) im Titel und
  im Text, Common Lisp und Smalltalk nennen beide Jahre, C# schreibt
  Hejlsberg richtig, COBOL nennt den CODASYL-Ausschuss und Grace Hoppers
  FLOW-MATIC, die NASM-Seite sagt, dass es um NASM geht; die
  Ziffernerkennung ist von »boostrap«, »Kenrel«, »beboachten« und »des die«
  befreit.
- Auswahlaufgaben: In 28 Aufgaben war die richtige Antwort deutlich die
  längste und damit leicht zu erraten; die falschen Antworten sind jetzt
  ebenso ausführlich und plausibel. Zwölf Aufgaben ohne jede Rückmeldung
  bekommen eine zu den naheliegenden Fehlern, als Frage statt als Lösung.
- Datei-Prüfungen, die richtige Lösungen ablehnten: Hugo nimmt auch
  `.Site.RegularPages`, `site.Pages` und `{{- … -}}`; raylib nimmt einen
  Rahmen um den Zielblock auch über eine eigene Variable und eine Textur
  unter anderem Namen. Wo eine Prüfung einen bestimmten Namen verlangt
  (`sky` in TIC-80, `vy` in raylib, `Thread.new` im Webserver), steht er
  jetzt in der Aufgabe.
- Sicherheit, ohne Anmeldung erreichbare Dienste: Automatron bleibt offen,
  rechnet aber nur noch Ausdrücke bis 200 Zeichen und Automaten bis 60
  Zustände (»Der Automat wird zu groß zum Zeichnen«), zeichnet höchstens
  zehn Sekunden pro Bild, höchstens zwei Anfragen gleichzeitig und pro
  Absender 60 in zehn Minuten. Bisher konnte ein einziger Ausdruck den
  Server minutenlang beschäftigen. QR-Anmeldungen und Workspaces hinter
  einem Freigabe-Link lassen sich pro Absender 30-mal in zehn Minuten
  starten.

### Entfernt

- Der Endpunkt, der Keras-Modelle für die Ziffernerkennung in TensorFlow.js
  umwandelte. Die Aktivität wandelt ihr Modell inzwischen selbst in ONNX um;
  der Endpunkt nahm ohne Anmeldung bis zu 100 MB an und legte sie dauerhaft
  ab.

- »Hades in der Altgriechisch-App« steht nicht mehr im Katalog. Die Seite
  brauchte eine echte Anmeldung bei der App einer Schule; was sie über
  `POST` zeigte, steht jetzt im Webserver.

### Für Betreibende

- Der Ordner `/internal/tfjs` (alte Uploads für den entfernten
  Keras-Endpunkt) kann gelöscht werden.

- Das Workspace-Image bekommt `php-mysql` (PDO-Treiber `pdo_mysql` und
  `mysqli`), damit PHP-Programme die MySQL-Datenbanken erreichen. Eine
  eigene, kleine Schicht kurz vor den Erweiterungen; Image neu bauen
  (`./build-image.sh`).
- TIC-80 einmal neu bauen: `./build-tic80.sh` (TIC-80 1.3.1, sokol-Schicht;
  `tic80.js` und `tic80.wasm` stehen nicht in Git). Das Build-Image braucht
  jetzt `gcc-multilib` für Forth; `docker/tic80/Dockerfile` installiert es.
- Keine Änderung nötig. Die Prüfarten `scene` und `er` brauchen `node` im
  Workspace-Image (ist dort schon vorhanden). Für die ER-Werkstatt muss das
  Workspace-Image neu gebaut werden (`build-image.sh`).
- Die Filmdatenbank lädt ihre Daten mit `wget` aus
  `specht/workspace-files/movie-archive/`; andere Schulen können die Dateien
  mit `src/scripts/collect-movies` selbst bauen. Die Datendateien enden mit
  `ANALYZE TABLE`: Ohne aktuelle Statistik dauert »Wer spielt mit wem?« direkt
  nach dem Import 50 Sekunden statt einer. Neu erzeugen geht ohne Netz mit
  `./collect-movies.rb --offline --output …/workspace-files/movie-archive`.
- Für den Hinweis auf neue Versionen braucht der Server ausgehende
  HTTPS-Verbindungen zu den Adressen aus `template` (GitHub, GitLab).
  Kommt keine Antwort, erscheint einfach kein Hinweis.
- Das Workspace-Image enthält `neo4j_bolt` (Gem, Version im Dockerfile als
  `NEO4J_BOLT_VERSION`). Neu bauen mit `build-image.sh`.
- Neue ER-Werkstatt `hackschule-er-0.2.0.vsix`: Workspace-Image neu bauen.
- `build-image.sh` bricht ab, bevor es `hs_code_server:latest` taggt, wenn eine
  `.vsix` aus `docker/code/svc-code-server-run` im Image fehlt. Bisher startete
  ein Workspace dann still ohne die Erweiterung.
- Workspace-Image neu bauen (`build-image.sh`): `emcc` warnt nicht mehr bei
  jedem Übersetzen über zwei GCC-Versionen, was Lernende für einen Fehler im
  eigenen Programm hielten.
- Ruby-Image neu bauen (`./config.rb build ruby`): Es braucht jetzt `wget`
  für die Kenney-Modelle der Anaglyph-Galerie.
- Für Vorschau und Galerie der Anaglyph-Abgaben braucht der Server
  ausgehende HTTPS-Verbindungen zu `codeload.github.com` (der Viewer in der
  Version `AnaglyphViewer::COMMIT`, beim ersten Gebrauch nach
  `/internal/anaglyph-viewer/`) und zu `kenney.nl` (Modelle, erst wenn eine
  Szene sie braucht). Ohne Netz gibt es keine Bilder; Abgaben funktionieren
  trotzdem.
- Abgaben im Browser kommen von `abgaben.<host>`. Der Platzhalter-Eintrag
  im DNS und das Zertifikat für `*.<host>` decken das schon ab; die
  nginx-Konfiguration schreibt der Server selbst.
- Screenshot-Generator neu bauen (nur Entwicklung):
  `./config.rb build tutorial_screenshots && ./config.rb up -d
  tutorial_screenshots`.
- Das Workspace-Image bekommt `yq` (mikefarah/yq, Version und SHA-256 im
  Dockerfile): Workspace-Image neu bauen (`./build-image.sh`).
- Das Workspace-Image bekommt `xxd`, das Dateien Byte für Byte in
  Hexadezimal zeigt (und zurück). Workspace-Image neu bauen
  (`./build-image.sh`).
