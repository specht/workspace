<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Terminal: Dateien und Verzeichnisse

<p class='abstract'>
Die Befehle aus <a href='/working-with-files'>Dateien und Verzeichnisse</a> zum Nachschlagen. Die Beispiele beziehen sich auf die Beispieldateien von dort, laufen aber genauso mit deinen eigenen Dateien. Zu fast jedem Befehl zeigt <code>man befehl</code> alle Optionen.
</p>

## Sich zurechtfinden

<!-- part: orientieren -->
### Wo bin ich, was liegt hier?

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>pwd</code></td><td>zeigt das aktuelle Verzeichnis (»print working directory«)</td></tr>
<tr><td><code>ls</code></td><td>listet das aktuelle Verzeichnis auf</td></tr>
<tr><td><code>ls -l</code></td><td>mit Rechten, Besitzer, Größe in Bytes und Datum; ein <code>d</code> vorne heißt Verzeichnis</td></tr>
<tr><td><code>ls -lh</code></td><td>Größen lesbar: <code>9.8M</code> statt <code>10180401</code></td></tr>
<tr><td><code>ls -la</code></td><td>auch versteckte Dateien, deren Name mit <code>.</code> beginnt</td></tr>
<tr><td><code>tree</code></td><td>alle Unterverzeichnisse als Baum; <code>tree sandbox</code> nur einen Teil</td></tr>
<tr><td><code>file *</code></td><td>den Typ jeder Datei, nach ihrem Inhalt, nicht nach der Endung</td></tr>
</tbody>
</table>
<!-- /part -->

<!-- part: cd -->
### Das Verzeichnis wechseln

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Wohin</th></tr>
</thead>
<tbody>
<tr><td><code>cd working-with-files</code></td><td>in ein Unterverzeichnis</td></tr>
<tr><td><code>cd ..</code></td><td>eine Ebene nach oben</td></tr>
<tr><td><code>cd ~</code> oder nur <code>cd</code></td><td>ins Home-Verzeichnis, im Workspace <code>/workspace</code></td></tr>
<tr><td><code>cd ~/working-with-files</code></td><td>von überall aus an diese Stelle</td></tr>
<tr><td><code>cd -</code></td><td>zurück ins vorige Verzeichnis</td></tr>
</tbody>
</table>

Pfade mit Leerzeichen brauchen Anführungszeichen oder einen Backslash vor jedem Leerzeichen: `cd "United States"` oder `cd United\ States`. Die <kbd>Tab</kbd>-Taste setzt die Backslashes von selbst.
<!-- /part -->

<!-- part: anzeigen -->
## Inhalt anzeigen

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>cat alice.txt</code></td><td>gibt die ganze Datei aus; bei langen Dateien bleibt nur das Ende stehen</td></tr>
<tr><td><code>less alice.txt</code></td><td>seitenweise lesen: Pfeiltasten, <kbd>Bild↑</kbd> <kbd>Bild↓</kbd>, <kbd>/</kbd> sucht, <kbd>Q</kbd> beendet</td></tr>
<tr><td><code>head words.txt</code></td><td>die ersten 10 Zeilen; <code>head -n 3</code> die ersten 3</td></tr>
<tr><td><code>tail words.txt</code></td><td>die letzten 10 Zeilen; <code>tail -f log.txt</code> zeigt neue Zeilen, sobald sie dazukommen</td></tr>
<tr><td><code>hd alice.txt | less</code></td><td>die einzelnen Bytes in hexadezimaler Schreibweise</td></tr>
</tbody>
</table>

`less` zeigt bei Bildern und Archiven, was darin steckt (Bildgröße, Dateiliste). Bei anderen Dateien fragt es, ob es sie trotzdem zeigen soll: Mit <kbd>Y</kbd> siehst du die Bytes als Zeichen, lesbar ist das nicht.
<!-- /part -->

<!-- part: editoren -->
## Editoren im Terminal

<table class='table table-sm reference-table'>
<thead>
<tr><th>Editor</th><th>Öffnen</th><th>Speichern</th><th>Beenden</th></tr>
</thead>
<tbody>
<tr><td><code>nano</code></td><td><code>nano hello.txt</code></td><td><kbd>Strg</kbd><kbd>O</kbd>, dann <kbd>Enter</kbd></td><td><kbd>Strg</kbd><kbd>X</kbd></td></tr>
<tr><td><code>vim</code></td><td><code>vim hello.txt</code>, schreiben nach <kbd>I</kbd></td><td><kbd>Esc</kbd>, dann <code>:w</code> und <kbd>Enter</kbd></td><td><kbd>Esc</kbd>, dann <code>:q</code> und <kbd>Enter</kbd>; <code>:q!</code> ohne Speichern</td></tr>
<tr><td><code>emacs</code></td><td><code>emacs hello.txt</code></td><td><kbd>Strg</kbd><kbd>X</kbd>, dann <kbd>Strg</kbd><kbd>S</kbd></td><td><kbd>Strg</kbd><kbd>X</kbd>, dann <kbd>Strg</kbd><kbd>C</kbd></td></tr>
</tbody>
</table>

`touch hello.txt` legt eine leere Datei an oder setzt bei einer vorhandenen das Datum auf jetzt. `vimtutor` übt `vim` in etwa 30 Minuten.
<!-- /part -->

## Suchen, zählen, sortieren

<!-- part: grep -->
### Suchen mit `grep`

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Findet</th></tr>
</thead>
<tbody>
<tr><td><code>grep everybody alice.txt</code></td><td>die Zeilen, in denen <code>everybody</code> steht</td></tr>
<tr><td><code>grep -i everybody alice.txt</code></td><td>auch <code>Everybody</code> und <code>EVERYBODY</code></td></tr>
<tr><td><code>grep -n …</code></td><td>mit Zeilennummern</td></tr>
<tr><td><code>grep -c …</code></td><td>nur die Zahl der Zeilen</td></tr>
<tr><td><code>grep -ri welcome .</code></td><td>in allen Dateien hier und in allen Unterverzeichnissen</td></tr>
<tr><td><code>grep -o -E "[A-Za-z]+" alice.txt</code></td><td>nur die Treffer selbst, hier jedes Wort einzeln; <code>-E</code> für reguläre Ausdrücke</td></tr>
</tbody>
</table>
<!-- /part -->

_person(thompson)

<!-- part: pipelines -->
### Pipelines und Umleitungen

Mit `|` wird die Ausgabe eines Befehls zur Eingabe des nächsten, mit `>` landet sie in einer Datei.

<table class='table table-sm reference-table'>
<thead>
<tr><th>Baustein</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>wc alice.txt</code></td><td>zählt Zeilen, Wörter und Bytes; <code>wc -l</code> nur Zeilen</td></tr>
<tr><td><code>sort</code></td><td>sortiert Zeilen alphabetisch</td></tr>
<tr><td><code>uniq</code></td><td>fasst gleiche Zeilen zusammen, die direkt untereinander stehen (deshalb vorher <code>sort</code>); <code>-i</code> ohne Groß- und Kleinschreibung</td></tr>
<tr><td><code>… | less</code></td><td>eine lange Ausgabe seitenweise</td></tr>
<tr><td><code>… &gt; words.txt</code></td><td>Ausgabe in eine Datei, die dabei überschrieben wird</td></tr>
<tr><td><code>… &gt;&gt; words.txt</code></td><td>Ausgabe ans Ende einer Datei anhängen</td></tr>
</tbody>
</table>

```bash
grep -o -E "[A-Za-z]+" alice.txt | sort | uniq -i | wc -l
```
<!-- /part -->

_person(mcilroy)

<!-- part: du -->
### Wie viel Platz?

`du -h` zeigt, wie viel Platz jedes Verzeichnis belegt, die letzte Zeile ist die Summe. `du -h -d 1` geht nur eine Ebene tief, `du -sh` nennt nur die Summe.
<!-- /part -->

<!-- part: archive -->
## Herunterladen, packen und entpacken

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>wget ADRESSE</code></td><td>lädt eine Datei ins aktuelle Verzeichnis</td></tr>
<tr><td><code>tar xvf archiv.tar.gz</code></td><td>entpackt ein tar-Archiv (auch <code>.tar.bz2</code>, <code>.tgz</code>); <code>x</code> extract, <code>v</code> zeigt jede Datei, <code>f</code> dann der Dateiname</td></tr>
<tr><td><code>tar cvzf archiv.tar.gz ordner</code></td><td>packt einen Ordner in ein Archiv (<code>c</code> create, <code>z</code> mit gzip)</td></tr>
<tr><td><code>unzip zork.zip</code></td><td>entpackt ein ZIP-Archiv</td></tr>
<tr><td><code>gzip alice.txt</code></td><td>komprimiert zu <code>alice.txt.gz</code>; <code>gzip -d</code> entpackt</td></tr>
<tr><td><code>bzip2 alice.txt</code></td><td>komprimiert stärker zu <code>alice.txt.bz2</code>; <code>bzip2 -d</code> entpackt</td></tr>
</tbody>
</table>

`gzip` und `bzip2` ersetzen die Datei durch die gepackte Fassung und umgekehrt; danach gibt es nur noch eine von beiden.
<!-- /part -->

## Aufräumen

<!-- part: kopieren -->
### Anlegen, kopieren, verschieben

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>mkdir sandbox</code></td><td>legt ein Verzeichnis an</td></tr>
<tr><td><code>mkdir -p a/b/c</code></td><td>auch alle Verzeichnisse dazwischen</td></tr>
<tr><td><code>cp jay.webm sandbox</code></td><td>kopiert eine Datei in ein Verzeichnis</td></tr>
<tr><td><code>cp ../alice.txt .</code></td><td>kopiert hierher; <code>.</code> ist das aktuelle Verzeichnis</td></tr>
<tr><td><code>cp -r zork-master sandbox</code></td><td>kopiert ein Verzeichnis mit allem, was darin ist</td></tr>
<tr><td><code>mv jay.webm nested</code></td><td>verschiebt eine Datei</td></tr>
<tr><td><code>mv alice.txt alice-in-wonderland.txt</code></td><td>benennt um</td></tr>
</tbody>
</table>

`cp` und `mv` überschreiben eine vorhandene Datei am Ziel ohne Rückfrage. Mit `-i` fragen sie vorher.
<!-- /part -->

<!-- part: loeschen -->
### Löschen

<table class='table table-sm reference-table'>
<thead>
<tr><th>Befehl</th><th>Was er tut</th></tr>
</thead>
<tbody>
<tr><td><code>rmdir empty</code></td><td>löscht ein Verzeichnis, aber nur, wenn es leer ist</td></tr>
<tr><td><code>rm alice.txt</code></td><td>löscht eine Datei</td></tr>
<tr><td><code>rm -r zork-master</code></td><td>löscht ein Verzeichnis mit allem, was darin ist</td></tr>
</tbody>
</table>

<div class='hint danger'>
Im Terminal gibt es keinen Papierkorb: Was <code>rm</code> löscht, ist weg. Hast du einen Checkpoint oder einen Git-Commit, holst du dort den alten Stand zurück.
</div>
<!-- /part -->

<!-- part: tasten -->
## Weniger tippen

<table class='table table-sm reference-table'>
<thead>
<tr><th>Taste</th><th>Was sie tut</th></tr>
</thead>
<tbody>
<tr><td><kbd>Tab</kbd></td><td>ergänzt Befehle, Datei- und Verzeichnisnamen; zweimal zeigt alle Möglichkeiten</td></tr>
<tr><td><kbd>↑</kbd> <kbd>↓</kbd></td><td>holt frühere Befehle zurück</td></tr>
<tr><td><kbd>Strg</kbd><kbd>C</kbd></td><td>bricht den laufenden Befehl ab</td></tr>
<tr><td><kbd>Strg</kbd><kbd>L</kbd> oder <code>clear</code></td><td>leert das Terminal</td></tr>
<tr><td><kbd>Q</kbd></td><td>beendet <code>less</code>, <code>man</code> und lange Ausgaben von <code>git log</code></td></tr>
<tr><td><code>*</code></td><td>steht für beliebige Zeichen: <code>ls *.txt</code>, <code>file *</code></td></tr>
<tr><td><code>./zork</code></td><td>startet ein Programm im aktuellen Verzeichnis</td></tr>
</tbody>
</table>

Diese Tasten gehören zur Shell, dem Programm, das im Terminal deine Befehle liest und ausführt; im Workspace ist das Bash.

_person(fox)

<!-- /part -->

<!-- part: farben -->
## Farben im Terminal

Nicht jedes Zeichen, das ein Programm ausgibt, zeigt das Terminal an. Das Zeichen ESC (Nummer 27) leitet eine Anweisung ein: `ESC[32m` heißt »ab hier grün«, `ESC[1m` »ab hier fett«, `ESC[0m` »zurück zum Normalen«. Solche Folgen heißen **ANSI-Escape-Sequenzen**, nach der Norm, die sie 1979 festschrieb. Mehrere Zahlen trennt ein Semikolon: `ESC[1;32m` ist fett und grün.

<table class='table table-sm reference-table'>
<thead>
<tr><th>Zahl</th><th>ab hier</th></tr>
</thead>
<tbody>
<tr><td><code>0</code></td><td>wieder normal: alle Farben und Schriftarten aus</td></tr>
<tr><td><code>1</code>, <code>2</code>, <code>3</code>, <code>4</code></td><td>fett, blass, kursiv, unterstrichen</td></tr>
<tr><td><code>7</code></td><td>Schrift und Hintergrund vertauscht</td></tr>
<tr><td><code>30</code> bis <code>37</code></td><td>Schriftfarbe: schwarz, rot, grün, gelb, blau, magenta, türkis, weiß</td></tr>
<tr><td><code>90</code> bis <code>97</code></td><td>dieselben acht Farben, hell</td></tr>
<tr><td><code>40</code> bis <code>47</code>, <code>100</code> bis <code>107</code></td><td>Hintergrund in denselben Farben</td></tr>
<tr><td><code>39</code>, <code>49</code></td><td>Schrift- oder Hintergrundfarbe wieder die des Terminals</td></tr>
<tr><td><code>38;5;n</code></td><td>Schriftfarbe <code>n</code> aus einer Palette von 256 (<code>48;5;n</code> für den Hintergrund)</td></tr>
<tr><td><code>38;2;r;g;b</code></td><td>jede Farbe aus Rot, Grün und Blau, je 0 bis 255 (<code>48;2;r;g;b</code> für den Hintergrund)</td></tr>
</tbody>
</table>

Wie die acht Farben genau aussehen, legt das Terminal fest; im dunklen und im hellen Farbschema von VS Code ist Gelb nicht dasselbe Gelb. Wer eine bestimmte Farbe will, nimmt `38;2;r;g;b`.

So schreibt jede Sprache ESC und damit ein grünes »Hallo«:

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Sprache</th><th>ESC</th><th>Ein grünes »Hallo«</th></tr>
</thead>
<tbody>
<tr><td>Bash</td><td><code>\e</code></td><td><code>printf '\e[32mHallo\e[0m\n'</code></td></tr>
<tr><td>Ruby</td><td><code>\e</code></td><td><code>puts "\e[32mHallo\e[0m"</code></td></tr>
<tr><td>Python</td><td><code>\033</code> (oktal) oder <code>\x1b</code></td><td><code>print("\033[32mHallo\033[0m")</code></td></tr>
<tr><td>JavaScript</td><td><code>\x1b</code></td><td><code>console.log("\x1b[32mHallo\x1b[0m");</code></td></tr>
<tr><td>Dart</td><td><code>\x1B</code></td><td><code>print('\x1B[32mHallo\x1B[0m');</code></td></tr>
<tr><td>Java</td><td><code>\u001B</code></td><td><code>System.out.println("\u001B[32mHallo\u001B[0m");</code></td></tr>
<tr><td>C</td><td><code>\033</code> (oktal)</td><td><code>printf("\033[32mHallo\033[0m\n");</code></td></tr>
</tbody>
</table>
</div>

<div class='hint'>
Schließ jede Farbe mit <code>ESC[0m</code> ab, sonst färbt sie alles danach mit, bis ins Terminal hinein, nachdem das Programm beendet ist. Leitest du die Ausgabe in eine Datei um, stehen die Sequenzen als Zeichen darin; <code>less -R</code> zeigt sie farbig an. Viele Programme lassen die Farben weg, wenn die Variable <code>NO_COLOR</code> gesetzt ist (<a href='https://no-color.org/'>no-color.org</a>).
</div>

<!-- /part -->

## Zum Nachschlagen

Ausführlicher als `man`, mit Beispielen und zum Durchblättern, auf Englisch:

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/): das Handbuch der Shell im Workspace: Pipelines, Umleitungen, Platzhalter wie `*`, Variablen und Schleifen.
- [GNU Coreutils](https://www.gnu.org/software/coreutils/manual/): die Grundbefehle `ls`, `cp`, `mv`, `rm`, `mkdir`, `cat`, `head`, `sort`, `uniq`, `wc`, `du` und viele mehr, jeder mit allen Optionen.
- [nano](https://www.nano-editor.org/docs.php): Handbuch und Übersicht der Tastenkürzel des Editors; für `vim` die [Dokumentation von Vim](https://www.vim.org/docs.php).
