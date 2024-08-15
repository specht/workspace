# Dateien und Verzeichnisse



<p class='abstract'>
In diesem Kapitel lernst du, wie du mit Dateien und Verzeichnissen in der Kommandozeile arbeiten kannst. Wir werden einige der wichtigsten Befehle kennenlernen, die wir verwenden können, um Dateien und Verzeichnisse zu erstellen, zu löschen, zu kopieren, zu verschieben und zu bearbeiten. Wir werden auch lernen, wie wir den Inhalt von Dateien anzeigen und analysieren können.
</p>

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

Öffne als nächstes das Terminal, indem du den Shortcut <span class='key'>Strg</span><span class='key'>J</span> drückst. Dein Workspace sollte jetzt ungefähr so aussehen:

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

Gib folgenden Befehl ein, um eine Datei herunterzuladen,die ein paar Beispieldateien enthält:

```bash
wget #{WEB_ROOT}/dl/working-with-files.tar.bz2
```

## Befehle

In der Kommandozeile verwenden wir Befehle, um mit Dateien und Verzeichnissen
zu arbeiten. Hier sind einige der wichtigsten Befehle, die wir verwenden
werden:

- `file`: Zeigt den Dateityp einer Datei an.
- `tar`: Archiviert und extrahiert Dateien.
- `cd`: Wechselt das aktuelle Verzeichnis.
- `ls`: Listet Dateien und Verzeichnisse auf.
- `mkdir`: Erstellt ein neues Verzeichnis.
- `rmdir`: Löscht ein Verzeichnis.
- `rm`: Löscht eine Datei.
- `cat`: Zeigt den Inhalt einer Datei an.
- `less`: Zeigt den Inhalt einer Datei seitenweise an.
- `wc`: Zählt die Anzahl der Zeilen, Wörter und Zeichen in einer Datei.
- `sha1sum`: Berechnet den SHA-1-Hashwert einer Datei.
- `cp`: Kopiert eine Datei oder ein Verzeichnis.
- `mv`: Verschiebt eine Datei oder ein Verzeichnis.
- `touch`: Erstellt eine leere Datei.
- `nano`, `vim`, `emacs`: Bearbeitet eine Datei.
- `pwd`: Zeigt das aktuelle Verzeichnis an.
- Umleiten von stdout
- `head`: Zeigt die ersten Zeilen einer Datei an.
- `tail`: Zeigt die letzten Zeilen einer Datei an.
- `hexdump`: Zeigt den hexadezimalen Inhalt einer Datei an.

## Beispiele

Hier sind einige Beispiele, wie wir die oben genannten Befehle verwenden
können, um mit Dateien und Verzeichnissen zu arbeiten:

- `file picture.jpg`: Zeigt den Dateityp der Datei `picture.jpg` an.
- `tar -czvf archive.tar.gz directory`: Archiviert das Verzeichnis `directory`
  in die Datei `archive.tar.gz`.
- `cd /home/user`: Wechselt das aktuelle Verzeichnis nach `/home/user`.
- `ls -l`: Listet Dateien und Verzeichnisse im aktuellen Verzeichnis auf.

Picture of RMS
Text of Alice in Wonderland
Video of Jay
Source of Dungeon
https://en.wikipedia.org/wiki/Dungeon_(video_game)

Commands:

file
tar
cd
ls
tree
hidden files
mkdir
rmdir
rm
cat
less (mention more)
wc
sha1sum
cp
mv
touch
nano / vim / emacs
pwd
redirect stdout
head
tail
hexdump
