<div class='meta'>
image: git-logo.png
</div>

# Versionsverwaltung mit Git

<p class='abstract'>
Git ist ein Versionsverwaltungssystem, das von Linus Torvalds entwickelt wurde. Git wird für die Verwaltung von Quellcode in Softwareprojekten eingesetzt und ist eines der bekanntesten Versionsverwaltungssysteme.
</p>

## Verzeichnisse vorbereiten

In diesem Tutorial werden wir ein Git-Repository erstellen und grundlegende Git-Befehle verwenden. Zuerst erstellen wir ein neues Verzeichnis für unser Projekt und wechseln in dieses Verzeichnis:

```bash
cd
mkdir git-tutorial
cd git-tutorial
mkdir alice
cd alice
```

(Verwende die Tab-Ergänzung, um den Pfad schneller einzugeben.)


## Git-Repository initialisieren

Um ein neues Git-Repository zu erstellen, verwenden wir den Befehl `git init`:

```bash
git init
```

Dieser Befehl erstellt ein neues verstecktes Verzeichnis namens `.git`, das alle notwendigen Dateien für das Repository enthält. Gib den Befehl `ls -la` ein, um das versteckte Verzeichnis anzuzeigen:

(SCREENSHOT)

## Datei erstellen

Erstelle eine Datei `hello.c` mit folgendem Inhalt:

_include_file(hello.c, c)

## Datei zum Staging-Bereich hinzufügen

Um die Datei `hello.c` zum Staging-Bereich hinzuzufügen, verwenden wir den Befehl `git add`:

```bash
git add hello.c
```

Damit ist die Datei für den nächsten Commit vorgemerkt. Wir können dies mit dem Befehl `git status` überprüfen:

```bash
git status
```

(SCREENSHOT)

## Änderungen committen

Um die Änderungen zu committen, verwenden wir den Befehl `git commit` mit der Option `-m`, um eine Commit-Nachricht hinzuzufügen:

```bash
git commit -m "initial commit"
```

<div class='hint info'>
Falls du eine Fehlermeldung bezüglich deines Namens und deiner E-Mail-Adresse erhältst, kannst du diese mit den folgenden Befehlen konfigurieren:

```bash
git config --global user.name "Dein Name"
git config --global user.email "deine.email@domain"
</div>