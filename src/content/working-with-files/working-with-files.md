# Dateien und Verzeichnisse

<p class='abstract'>
In diesem Kapitel lernst du, wie du mit Dateien und Verzeichnissen in der Kommandozeile arbeiten kannst. Wir werden einige der wichtigsten Befehle kennenlernen, die wir verwenden können, um Dateien und Verzeichnisse zu erstellen, zu löschen, zu kopieren, zu verschieben und zu bearbeiten. Wir werden auch lernen, wie wir den Inhalt von Dateien anzeigen und analysieren können.
</p>

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

Öffne als nächstes das Terminal, indem du den Shortcut <span class='key'>Strg</span><span class='key'>J</span> drückst. Dein Workspace sollte jetzt ungefähr so aussehen:

IMAGE_MISSING

## Grundlagen

Ein Computer speichert Daten in Dateien. Eine Datei ist eine Sammlung von
Daten, die auf einem Speichermedium gespeichert sind. Dateien können
verschiedene Formate haben, z. B. Textdateien, Bilddateien, Audiodateien
oder Videodateien.

Dateien werden in Verzeichnissen organisiert. Ein Verzeichnis ist eine
Sammlung von Dateien und anderen Verzeichnissen. Verzeichnisse können
verschachtelt werden, d. h. ein Verzeichnis kann ein anderes Verzeichnis
enthalten.

## Beispieldateien herunterladen

Gib folgenden Befehl ein, um eine Datei herunterzuladen, die ein paar Beispieldateien enthält:

```bash
wget #{WEB_ROOT}/dl/working-with-files.tar.gz
```

## Befehle

In der Kommandozeile verwenden wir Befehle, um mit Dateien und Verzeichnissen
zu arbeiten. Hier sind einige der wichtigsten Befehle, die wir verwenden
werden:

### Überblick verschaffen

- `pwd`: Zeigt das aktuelle Verzeichnis an.
- `tree`: Zeigt die Verzeichnisstruktur an.
- `file`: Zeigt den Dateityp einer Datei an.
- `cd`: Wechselt das aktuelle Verzeichnis.
- `ls`: Listet Dateien und Verzeichnisse auf.

### Dateien anzeigen

- `cat`: Zeigt den Inhalt einer Datei an.
- `less`: Zeigt den Inhalt einer Datei seitenweise an.
- `hd`: Zeigt den hexadezimalen Inhalt einer Datei an.

### Dateien erstellen und bearbeiten

- `touch`: Erstellt eine leere Datei.
- `nano`, `vim`, `emacs`: Bearbeitet eine Datei.

### Dateien analysieren, durchsuchen und filtern

- `wc`: Zählt die Anzahl der Zeilen, Wörter und Zeichen in einer Datei.
- `grep`: Sucht nach einem Muster in einer Datei.
- `sort`: Sortiert die Zeilen einer Datei.
- `uniq`: Entfernt doppelte Zeilen aus einer Datei.
- `head`: Zeigt die ersten Zeilen einer Datei an.
- `tail`: Zeigt die letzten Zeilen einer Datei an.
- `diff`: Zeigt den Unterschied zwischen zwei Dateien an.
- `sha1sum`: Berechnet den SHA-1-Hashwert einer Datei.
- Umleiten von stdout

### Verzeichnisse analysieren und durchsuchen

- `du`: Zeigt die Größe von Dateien und Verzeichnissen an.
- `find`: Sucht nach Dateien und Verzeichnissen.

### Dateien (und Verzeichnisse) kopieren, verschieben und löschen

- `rm`: Löscht eine Datei.
- `cp`: Kopiert eine Datei oder ein Verzeichnis.
- `mv`: Verschiebt eine Datei oder ein Verzeichnis.
- `mkdir`: Erstellt ein neues Verzeichnis.
- `rmdir`: Löscht ein Verzeichnis.

### Dateien archivieren und extrahieren

- `tar`: Archiviert und extrahiert Dateien.
- `gzip` und `bzip2`: Komprimiert und dekomprimiert Dateien.
- `zip` und `unzip`: Komprimiert und dekomprimiert Dateien.

### Dateien aus dem Internet herunterladen

- `wget`: Lädt Dateien aus dem Internet herunter.
- `curl`: Lädt Dateien aus dem Internet herunter.
