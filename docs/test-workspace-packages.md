# Workspace-Pakete für Leistungsüberprüfungen

Eine Leistungsüberprüfung wird als komprimiertes Verzeichnis (`.tar.gz` oder
`.tar.bz2`) hochgeladen. Der Inhalt des Archivs wird für jede Schülerin und
jeden Schüler in einen eigenen, frischen Prüfungs-Workspace entpackt.

Das Archiv sollte **direkt den Inhalt des Workspace** enthalten und nicht noch
ein zusätzliches übergeordnetes Verzeichnis.

## Minimales Paket

Ein minimales Paket kann so aussehen:

```text
.
├── .workspace/
│   └── config.yaml
├── .gitignore
├── AUFGABE.md
└── ...
```

Die Dateien und Verzeichnisse nach `AUFGABE.md` sind vollständig
unterrichtsabhängig. Das Paket ist nicht an eine Programmiersprache gebunden.

Wechsle **in den Aufgabenordner**. Dieses Kommando legt das gleichnamige
Archiv eine Ebene höher an und packt seinen gesamten Inhalt direkt hinein,
einschließlich `.workspace`, `.gitignore` und `.git`, sofern vorhanden:

```bash
tar -czf "../$(basename "$PWD").tar.gz" .
```

Der Name des Aufgabenordners muss nicht eingetippt werden. Enthält das Paket
`.git`, muss in `.workspace/config.yaml` `git.mode: preserve` stehen. Bei
`git.mode: fresh` oder `none` wird ein mitgeliefertes `.git` abgelehnt.
Packe keine privaten Schlüssel, Zugangsdaten oder lokalen Cache-Ordner ein.

Für normale Leistungsüberprüfungen gehört kein `.git`-Verzeichnis in das Paket.
Für Aufgaben, bei denen ein vorhandener Git-Verlauf selbst Teil der Aufgabe ist,
kann das Repository aber bewusst mitgepackt und mit `git.mode: preserve` erhalten
werden.

## `.workspace/config.yaml`

Beispiel:

```yaml
workspace_package: 1

exam:
  color: blue
  setup:
    once:
      - dart --disable-analytics
    # Optional: on every new container start, not every browser reload.
    # every_start:
    #   - echo "Prüfungs-Workspace gestartet"

git:
  mode: fresh

extensions:
  - Dart-Code.dart-code

print:
  exclude:
    - "tests/**"
    - "**/test_runner.*"

vscode_config:
  editor.minimap.enabled: false
```

### Erweiterungen

Optional lassen sich benötigte VS-Code-Erweiterungen deklarieren:

```yaml
extensions:
  - Dart-Code.dart-code
  - id: example.extension
    version: "1.2.3"
```

Die Versionsangabe ist optional. Beim Upload lädt der Workspace die angegebene
Version oder löst die aktuelle Version auf, lädt außerdem die deklarierten
Abhängigkeiten und speichert die geprüften VSIX-Dateien serverseitig. Bei einem
Fehler erscheint eine Meldung; das Paket wird nicht als Prüfung freigegeben.
Im Prüfungs-Workspace und in der Vorschau werden die Dateien offline installiert.
Bereits hochgeladene Prüfungen behalten ihre festgehaltenen Versionen.

### Einmalige und wiederholte Startbefehle

Unter `exam.setup.once` kannst du Befehle deklarieren, die für jede Person und
auch für die eigene Vorschau genau einmal erfolgreich ausgeführt werden sollen.
`exam.setup.every_start` wird bei **jedem neuen Containerstart** ausgeführt,
nicht bei einem bloßen Neuladen der Browserseite. Beide Optionen sind optional:

```yaml
exam:
  setup:
    once:
      - dart --disable-analytics
    every_start:
      - echo "Prüfungs-Workspace gestartet"
```

Die Befehle laufen in `/workspace` als normaler Workspace-Benutzer (`HOME=/workspace`),
**nicht als root**. Sie laufen offline, nach der Installation der deklarierten
Erweiterungen und vor dem Öffnen von code-server. Ein fehlgeschlagener Befehl
verhindert den Editorstart; er wird beim nächsten Containerstart erneut versucht.
Ein `once`-Befehl wird erst nach erfolgreichem Abschluss als erledigt markiert.
Die Markierungen liegen im persistenten, prüfungsspezifischen `/config`-Mount
und sind weder Teil der Abgabe noch im normalen Workspace vorhanden.

Beim Upload werden maximal zwölf Befehle pro Liste mit jeweils höchstens
2048 Bytes akzeptiert. Jeder Befehl hat beim Start ein Zeitlimit von 120 Sekunden.
Da diese Befehle auf dem Server ausgeführt werden, sollten nur vertrauenswürdige
Prüfungspakete hochgeladen werden. Eine bereits hochgeladene Prüfung muss erneut
hochgeladen werden, damit neue Setup-Befehle in ihr festgeschrieben werden.

### Prüfungsfarbe

`exam.look` wählt, was die Farbe färbt:

- `theme` (ohne Angabe, wie bisher): das integrierte VS-Code-Farbschema
  `Tomorrow Night Blue` bzw. `Red` für die gesamte Oberfläche einschließlich
  Editor, Willkommensseite und Panels; `exam.color` ist `blue` (ohne Angabe)
  oder `red`.
- `frame`: ein Rahmen in einer Signalfarbe, die man von hinten im Raum sieht
  (Titelleiste, Activity Bar, Reiterleiste, Statusleiste), um einen dunklen
  Editor (`Default Dark Modern`); `exam.color` ist `yellow`, `orange`,
  `magenta`, `green`, `blue` oder `red` (`EXAM_FRAME_COLORS` in
  `test_workspace_package.rb`). Eigene `workbench.colorCustomizations` und ein
  eigenes `workbench.colorTheme` unter `vscode_config` gehen vor.

Änderungen gelten für neu erzeugte Prüfungs-Workspaces, nicht rückwirkend für
bereits entpackte Prüfungen.

### Druckausgabe

Ohne weitere Konfiguration werden alle rekursiv gefundenen UTF-8-Textdateien
gedruckt. Binärdateien, versteckte Verzeichnisse auf jeder Ebene (z. B.
`.cache`, `.git`, `.workspace`, `src/.cache`) sowie interne Workspace-Dateien
wie `.test_init` werden automatisch ausgelassen, ebenso was Werkzeuge im
Home-Ordner anlegen (`.lesshst`, `.bash_history`, `.python_history` und andere
`.*history`, `.wget-hsts`, `.viminfo`). Andere versteckte Dateien wie
`.gitignore` können dagegen gedruckt werden.

Mit `print.exclude` lassen sich zusätzliche Dateien über Glob-Muster
ausschließen:

```yaml
print:
  exclude:
    - "tests/**"
    - "**/test_runner.*"
    - "material/loesung.txt"
```

Optional kann die Auswahl mit `print.include` eingeschränkt werden:

```yaml
print:
  include:
    - "src/**"
    - "README.md"
  exclude:
    - "**/test_runner.*"
```

Mehrere kurze Dateien werden in der Druckansicht hintereinander gesetzt und
nicht künstlich auf einzelne Seiten verteilt. Zwischen verschiedenen
Schülerinnen und Schülern beginnt dagegen eine neue Seite.

### VS-Code-Konfiguration

Unter `vscode_config` können normale VS-Code-Einstellungen ergänzt werden.
Diese Einstellungen werden nur für den Prüfungs-Workspace angewendet.

## Git

`git.mode` steuert, was der Workspace mit Git macht. Ohne Angabe wird
`fresh` verwendet.

### `fresh` – normales Prüfungs-Repository

```yaml
git:
  mode: fresh
```

Dies ist der Standard. Das Paket darf kein `.git`-Verzeichnis enthalten. Beim
ersten Start legt der Workspace ein neues lokales Repository an, setzt
repository-lokal `user.name` und `user.email` auf die jeweilige Person und
erzeugt den ersten Commit:

```text
Ausgangszustand
```

Damit funktionieren unmittelbar die üblichen Git-Werkzeuge, zum Beispiel:

```bash
git status
git diff
git diff HEAD
git restore DATEI
git log
```

Schülerinnen und Schüler können während der Leistungsüberprüfung weitere
normale Commits anlegen. Die Checkpoints-Erweiterung ist dafür nicht
erforderlich.

### `preserve` – vorhandenes Repository ist Teil der Aufgabe

```yaml
git:
  mode: preserve
```

Hier muss das Paket ein normales `.git`-Verzeichnis enthalten. Der Workspace
erhält Repository-Historie, Branches, Tags, HEAD und Index unverändert. Er
initialisiert kein neues Repository und erzeugt keinen `Ausgangszustand`-Commit.

Lediglich die repository-lokalen Einstellungen `user.name`, `user.email` und
`commit.gpgSign` werden für die jeweilige Person gesetzt. Bereits vorhandene
Commits und deren Autorinnen bzw. Autoren werden dadurch nicht verändert.

Dieser Modus eignet sich zum Beispiel für Aufgaben zu Branches, Merge-Konflikten,
`git log`, `git diff`, `git bisect`, Tags oder zum Reparieren einer vorhandenen
Historie.

### `none` – Git ist selbst Teil der Aufgabe

```yaml
git:
  mode: none
```

Der Workspace führt keinerlei Git-Einrichtung durch. Das Paket darf auch hier
kein `.git`-Verzeichnis enthalten. Damit kann beispielsweise `git init` selbst
Teil der Prüfungsaufgabe sein.

Ein vorhandenes `.git` wird nie stillschweigend gelöscht. Bei `fresh` oder
`none` führt es stattdessen zu einem Konfigurationsfehler mit dem Hinweis auf
`git.mode: preserve`. Bei `preserve` ist ein fehlendes `.git` ebenfalls ein
Fehler.

### Repositorys in Unterordnern

Ein `.git` in einem Unterordner (etwa `projekt/.git`, eine Git-Aufgabe in
diesem Ordner) ist in jedem Modus erlaubt. Bei `fresh` und `preserve` bekommt
jedes solche Repository wie das oberste `user.name`, `user.email` und
`commit.gpgSign`, und das Repository zum Wiederherstellen von `fresh` trägt
den Unterordner in `.git/info/exclude` ein, statt ihn als eingebettetes
Repository zu committen (`TestWorkspacePackage.nested_repositories`, höchstens
20, nicht durch Links, nicht in versteckte Ordner, höchstens sechs Ebenen tief;
`initialize_test_git` in `main.rb`). Bei `none` richtet der Workspace nichts ein.

Workspace-eigene Dateien werden bei `fresh` und `preserve` über
`.git/info/exclude` ausgeblendet, ohne die vom Lehrer bereitgestellte
`.gitignore` zu verändern. Nach dem Commit »Ausgangszustand« (bei `preserve`
gleich nach der Einrichtung) kommt dort `/.*` dazu: Versteckte Dateien und
Ordner oben im Workspace, die Git noch nicht verfolgt, erscheinen nicht in
`git status`. Gemeint sind die Caches, die Werkzeuge im Home-Ordner anlegen
(`.dartServer`, `.dart-tool`, `.lesshst`, `.vscode-oss-shared`). Was das Paket
mitbringt, ist dann schon verfolgt und bleibt es; eine `.gitignore`,
`.gitattributes` oder ein `.vscode`, das die Schülerin oder der Schüler anlegt,
erscheint weiterhin (`write_hidden_git_exclude`).

## Aus einem Ordner des Workspace

Statt ein Archiv hochzuladen, baut die Lehrkraft das Paket im Reiter mit
»Neue Leistungsüberprüfung« aus einem Ordner oben in ihrem eigenen Workspace
(`exam_builder.rb`, `exam_builder_page.rb`, der Dialog in `exams.js`). Der
Baum des Ordners wird als root gelesen, nur Namen und Größen, ohne einem Link
zu folgen; versteckte Dateien, Build-Ordner (`ExamBuilder::ARTIFACT_DIRS` und
Ähnliches) und Git-Verläufe beginnen nicht angekreuzt und werden nicht
geöffnet (angekreuzt gehen sie ganz mit), Links lassen sich nicht ankreuzen.
Je Eintrag gibt es ein Häkchen »Drucken«: Was abgewählt ist, wird zu
`print.exclude` (ein Ordner als `ordner/**`, Sonderzeichen maskiert); Muster
einer `config.yaml`, die schon im Ordner lag, bleiben, wenn sie zu keinem Pfad
des Ordners passen (sie meinen Dateien, die die Klasse anlegt). Farbe, Rahmen
und Git (`fresh` oder `none`; ein angekreuztes `.git` oben im Ordner heißt
`preserve`) kommen aus dem Dialog; alle übrigen Angaben einer vorhandenen
`config.yaml` (Erweiterungen, Setup, `vscode_config`, `print.include`) bleiben.
Gepackt wird mit `tar` als Workspace-Benutzer (`setpriv`), mit einer Liste der
Dateien und ohne Rekursion; ein Link, der nach dem Lesen des Baums
hineingetauscht wurde, reicht also nicht weiter als die Lehrkraft lesen kann,
und die Prüfung des Pakets lehnt Links ab. Das Paket geht danach denselben Weg
wie ein hochgeladenes (`create_exam` in `exams_page.rb`). Höchstens 10 000
Dateien und 100 MB.

### Vom Rechner der Lehrkraft

Statt eines Ordners im Workspace nimmt der Dialog einen Ordner, eine ZIP- oder
eine `.tar.gz`-Datei vom Rechner, gezogen oder gewählt
(`/api/exam_builder/drop`). Was ankommt, liegt als Entwurf in
`/internal/exam-drafts/<fs_tag>/<id>/files`, nur für diese Lehrkraft, bis die
Leistungsüberprüfung angelegt ist; danach wird er gelöscht, und Entwürfe, die
älter als ein Tag sind, löscht `ExamBuilder.expire_drafts` (mit jedem neuen
Entwurf und alle zehn Minuten im Hintergrund). Ein Ordner kommt in Stücken zu
höchstens 100 Dateien (Rack nimmt 128 Dateien je Anfrage); der Browser nimmt
den Namen des Ordners selbst von den Pfaden ab und schickt ihn als Namen mit.
Ein Archiv wird vor dem Auspacken geprüft wie ein hochgeladenes Paket: `.tar.gz`
mit `TestWorkspaceExtensions.validate_archive!`, ZIP mit
`src/ruby/tools/unzip_safe.py` (keine Links, kein Pfad aus dem Ordner, höchstens
10 000 Dateien und 100 MB). Liegt oben im Archiv nur ein Ordner (eine ZIP eines
Ordners), wird er selbst der Entwurf. Die Dateien eines Entwurfs schreibt die
Seite selbst; gepackt wird er ohne `setpriv`.

### Erweiterungen für die Sprache

Liegen Dateien einer Sprache im Paket, für die `ExamBuilder::LANGUAGE_EXTENSIONS`
eine Erweiterung kennt (bisher `.dart`: `Dart-Code.dart-code`), bietet der
Dialog sie an, angekreuzt; nennt eine `config.yaml` sie schon, steht sie fest
angekreuzt da. Angekreuzt kommt sie zu `extensions` dazu und wird beim
Hochladen wie jede andere geladen. Weitere Sprachen kommen in die Tabelle, wenn
ihre Erweiterung auf Open VSX mit ihren Abhängigkeiten geprüft ist.

## Netz

Ein Prüfungs-Workspace hängt am selben Netz wie jeder Workspace
(`workspace_user`, eine Bridge mit NAT), kommt aber nicht hinaus:

- Er bekommt ein leeres `/etc/resolv.conf` (`workspace_runtime.rb`): Namen
  lösen sich nicht auf; die Datenbanken stehen als feste Einträge in
  `/etc/hosts`.
- Er bekommt eine MAC-Adresse mit dem Präfix `02:68:73`
  (`WorkspaceRuntime::DirectDocker::EXAM_MAC_PREFIX`) und das Label
  `hackschule.workspace.exam=true`. Die `peer_firewall` verwirft in ihrer
  Bridge-Tabelle (Kette `exam_egress`, Hook `input`) jedes IP-Paket, das eine
  solche Adresse an den Host schickt, also alles, was das Netz der Workspaces
  verlässt: das Internet, auch über eine IP-Adresse, und die Ports des Hosts.
  Die Regel ist statisch und steht von der ersten Installation der Tabelle an
  darin; zwischen dem Start eines Prüfungs-Workspace und dem nächsten Abgleich
  der Firewall gibt es keine Lücke. Antworten auf Verbindungen, die jemand
  anderes geöffnet hat, gehen durch (`ct state established,related`), und
  Verkehr innerhalb der Bridge (nginx zu code-server, die Datenbanken) läuft
  nicht über diesen Hook. Die Adresse ändern können die Lernenden nicht (kein
  `NET_ADMIN`, kein `NET_RAW`, `no-new-privileges`).
- `network.allow` in `config.yaml` nennt Ausnahmen (Name oder IPv4-Adresse,
  optional `:Port`, sonst 80 und 443, höchstens zehn). Sie werden beim Hochladen
  geprüft und im Manifest festgehalten (`TestWorkspaceExtensions.network_allow`),
  nie aus der Kopie der Lernenden gelesen, die `config.yaml` ändern könnten.
  Beim Start löst das Backend die Namen auf, trägt sie mit `--add-host` ein und
  schreibt `ip:port` in das Label `hackschule.workspace.exam.allow`; die
  Firewall nimmt daraus `mac . ip . port` in die Menge `exam_allowed`. Eine
  Ausnahme kommt also ein, zwei Sekunden nach dem Start, nie zu früh.

Getestet: `workspace_runtime_test.rb` (MAC, Label, Namen), die Ausnahmen in
`test_workspace_package_test.rb` und `test_workspace_extensions_test.rb`; die
Regel selbst von Hand gegen die Bridge der Entwicklung (ein Container mit
dieser MAC erreicht den Host nicht mehr, MySQL und Neo4j weiter).

## Wer den Ordner der Lehrkraft lesen kann

Die Aufgaben liegen vor der Leistungsüberprüfung im Workspace der Lehrkraft.
`e2e/tests/workspace-isolation.browser.spec.ts` prüft mit dem Workspace einer
Lehrkraft und dem einer Schülerin, beide laufend: Die Adresse öffnet nur mit
dem eigenen `hs_server_sid` (nginx), nicht für andere und nicht ohne Anmeldung;
ein WebSocket von einer Seite der Schülerin (Subdomain `<tag>-<port>`) mit dem
Cookie der Lehrkraft lehnt code-server ab (anderer Origin, 403; mit dem
eigenen Origin 101); aus dem Container der Schülerin ist der der Lehrkraft
nicht erreichbar (`peer_firewall`), und gemountet sind nur ihre eigenen Ordner,
kein `/user`. In den Workspace einer Lehrkraft sieht keine andere Lehrkraft
(`LookIn.allowed?`), die Administration mit Grund, den die Lehrkraft sieht.
Ein geteilter Workspace (`/api/share_server`) öffnet alles für jeden mit dem
Link; der Dialog »Neue Leistungsüberprüfung« warnt dann und beendet das Teilen
(`/api/unshare_server`). Das Paket kommt erst mit »Starten« in die Workspaces
der Teilnehmenden (`/api/start_server` verlangt eine laufende Prüfung).

## Klasse und Teilnehmende

Hochgeladen wird ein Paket auf der Klassenseite, im Reiter
»Leistungsüberprüfungen« (`class_exams_html`, `exams.js`). Es gehört dann zu
dieser Klasse (`Test.group`), und alle aus der Klasse nehmen teil, ohne
Lehrkräfte und ohne Testkonten (außer in einer Testgruppe; `exam_students`).
Im Dialog »Teilnehmende« steht die Klasse oben und offen; dort nimmt man
Einzelne heraus, und aus den anderen Gruppen darunter fügt man Einzelne hinzu.
Pakete, die früher im Profil hochgeladen wurden, haben noch keine Klasse: Sie
stehen in den Klassen der Lehrkraft, in denen jemand der Teilnehmenden ist
(solange niemand teilnimmt, in jeder), mit den Klassen der Teilnehmenden und
»Dieser Klasse zuordnen« (`/api/test_assign_group`); wer teilnimmt, bleibt dabei.

## Vor dem Einsatz prüfen

Auf der Klassenseite, im Reiter »Leistungsüberprüfungen« der Klasse, hat jedes
Paket »Ausprobieren« und eine Druckansicht der Vorschau. »Ausprobieren«
(`/pruefung-ansehen?tag=…`, `exams_page.rb`) zeigt die Startseite, wie sie die
Teilnehmenden während der Prüfung sehen (`running_tests_html`, das Menü
ausgeblendet), mit einem Band zurück zur Klasse; ihr Knopf startet die Vorschau
der Lehrkraft (`/api/start_test_demo` mit `keep: 'yes'`: weiter, wo sie war),
»Von vorn beginnen« ohne `keep` setzt sie zurück und holt sie frisch aus dem
Paket. Die Vorschau ist eine eigene Instanz (`Main.test_demo_tag`), getrennt von
den Workspaces der Teilnehmenden.

Bei den Ergebnissen kann anschließend entweder die gesamte Gruppe oder gezielt
eine einzelne Person gedruckt werden.

## Ergebnisse herunterladen

Unter »Ergebnisse« gibt es neben dem Druck alle Dateien als ein ZIP
(`/api/zip_for_test/TAG`, `exams_page.rb`), zum Aufheben und um die Lösungen
später auszuführen; der Ausdruck bleibt die Dokumentation. Im Archiv steht
ein Ordner je Person, benannt wie auf dem Ausdruck, mit allen Dateien ihres
Prüfungs-Workspace, auch denen, die `print.exclude` vom Druck ausnimmt
(Tests, Daten). Es fehlen nur die versteckten Ordner, in denen code-server
seinen Zustand ablegt (`.local`, `.cache` und ähnliche); `.git` und
`.workspace` bleiben. Wer den Prüfungs-Workspace nie geöffnet hat, steht in
`nicht-geoeffnet.txt`. Das Archiv entsteht beim Herunterladen
(`tools/zip_stream.py` als Workspace-Benutzer, wie »Meine Daten
herunterladen«) und wird nicht gespeichert.
