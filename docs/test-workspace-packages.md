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

Wechsle in das Verzeichnis, **in dem** der Aufgabenordner liegt. Dieses
Beispiel legt das Archiv neben dem Aufgabenordner an, enthält aber direkt dessen
Inhalt einschließlich `.workspace`, `.gitignore` und anderer versteckter Dateien:

```bash
tar --exclude='./.git' \
    -czf 2026-09-23-workspace-package.tar.gz \
    -C 2026-09-23-workspace-package .
```

`--exclude='./.git'` ist für `git.mode: fresh` gedacht. Bei `git.mode: preserve`
muss diese Option entfallen, damit die vorhandene Git-Historie mit ins Archiv
kommt. Packe keine privaten Schlüssel, Zugangsdaten oder lokalen Cache-Ordner ein.

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

git:
  mode: fresh

print:
  exclude:
    - "tests/**"
    - "**/test_runner.*"

vscode_config:
  editor.minimap.enabled: false
```

### Prüfungsfarbe

`exam.color` kann `blue` oder `red` sein. Wenn die Angabe fehlt, wird `blue`
verwendet. Die Einstellung wählt das integrierte VS-Code-Farbschema
`Tomorrow Night Blue` bzw. `Red` für die gesamte Oberfläche einschließlich
Editor, Willkommensseite und Panels. Änderungen gelten für neu erzeugte
Prüfungs-Workspaces, nicht rückwirkend für bereits entpackte Prüfungen.

### Druckausgabe

Ohne weitere Konfiguration werden alle rekursiv gefundenen UTF-8-Textdateien
gedruckt. Binärdateien, versteckte Verzeichnisse auf jeder Ebene (z. B.
`.cache`, `.git`, `.workspace`, `src/.cache`) sowie interne Workspace-Dateien
wie `.test_init` werden automatisch ausgelassen. Einzelne versteckte Dateien
wie `.gitignore` können dagegen gedruckt werden.

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

Workspace-eigene Dateien werden bei `fresh` und `preserve` über
`.git/info/exclude` ausgeblendet, ohne die vom Lehrer bereitgestellte
`.gitignore` zu verändern.

## Vor dem Einsatz prüfen

Im Profil der Lehrkraft gibt es für jedes hochgeladene Paket eine
Workspace-Vorschau und eine Druckansicht der Vorschau. Es empfiehlt sich,
beides vor der Leistungsüberprüfung einmal zu öffnen.

Bei den Ergebnissen kann anschließend entweder die gesamte Gruppe oder gezielt
eine einzelne Person gedruckt werden.
