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

<style>
    .ansi-demo {
        background: #1e1e1e;
        color: #cccccc;
        border-radius: 0.5em;
        padding: 0.8em 1em;
        margin: 0.8em 0 1.2em;
        font-family: '0xProto Nerd Font Mono', monospace;
        font-size: 0.9em;
        overflow-x: auto;
    }
    .ansi-demo table {
        border-collapse: collapse;
    }
    .ansi-demo th {
        color: #9aa1ac;
        font-weight: normal;
        text-align: left;
        padding: 0.2em 1.2em 0.4em 0;
    }
    .ansi-demo td {
        padding: 0.2em 1.2em 0.2em 0;
        white-space: nowrap;
    }
    .ansi-demo .ansi-bg {
        padding: 0 0.3em;
    }
    .ansi-styles span {
        margin-right: 1.4em;
        white-space: nowrap;
    }
    /* the 16 colours, the cube one row per red level, the greys */
    .ansi-palette {
        display: grid;
        grid-template-columns: repeat(var(--cols), 2.15em);
        gap: 1px;
        width: max-content;
        font-size: 0.7em;
        text-align: center;
        margin-bottom: 0.6em;
    }
    .ansi-palette:last-child {
        margin-bottom: 0;
    }
    /* three squares per row, two on a phone */
    .ansi-cube {
        display: flex;
        flex-wrap: wrap;
        gap: 0.6em;
        max-width: 31em;
        margin-bottom: 0.6em;
    }
    .ansi-cube .ansi-palette {
        margin-bottom: 0;
    }
    .ansi-palette span {
        padding: 0.25em 0;
    }
    .ansi-gradient {
        display: flex;
        height: 1.6em;
        border-radius: 0.3em;
        overflow: hidden;
    }
    .ansi-gradient span {
        flex: 1;
    }
</style>

Die acht Farben, wie sie im dunklen Farbschema von VS Code aussehen, mit ihren Zahlen für die Schrift, die helle Schrift, den Hintergrund und den hellen Hintergrund:

<div class='ansi-demo'>
<table>
<thead><tr><th>Farbe</th><th>Schrift</th><th>hell</th><th>Hintergrund</th><th>hell</th></tr></thead>
<tbody>
<tr><td>schwarz</td><td><span style='color:#000000'>30 Hallo</span></td><td><span style='color:#666666'>90 Hallo</span></td><td><span class='ansi-bg' style='background:#000000;color:#ffffff'>40 Hallo</span></td><td><span class='ansi-bg' style='background:#666666;color:#ffffff'>100 Hallo</span></td></tr>
<tr><td>rot</td><td><span style='color:#cd3131'>31 Hallo</span></td><td><span style='color:#f14c4c'>91 Hallo</span></td><td><span class='ansi-bg' style='background:#cd3131;color:#ffffff'>41 Hallo</span></td><td><span class='ansi-bg' style='background:#f14c4c;color:#ffffff'>101 Hallo</span></td></tr>
<tr><td>grün</td><td><span style='color:#0dbc79'>32 Hallo</span></td><td><span style='color:#23d18b'>92 Hallo</span></td><td><span class='ansi-bg' style='background:#0dbc79;color:#ffffff'>42 Hallo</span></td><td><span class='ansi-bg' style='background:#23d18b;color:#000000'>102 Hallo</span></td></tr>
<tr><td>gelb</td><td><span style='color:#e5e510'>33 Hallo</span></td><td><span style='color:#f5f543'>93 Hallo</span></td><td><span class='ansi-bg' style='background:#e5e510;color:#000000'>43 Hallo</span></td><td><span class='ansi-bg' style='background:#f5f543;color:#000000'>103 Hallo</span></td></tr>
<tr><td>blau</td><td><span style='color:#2472c8'>34 Hallo</span></td><td><span style='color:#3b8eea'>94 Hallo</span></td><td><span class='ansi-bg' style='background:#2472c8;color:#ffffff'>44 Hallo</span></td><td><span class='ansi-bg' style='background:#3b8eea;color:#ffffff'>104 Hallo</span></td></tr>
<tr><td>magenta</td><td><span style='color:#bc3fbc'>35 Hallo</span></td><td><span style='color:#d670d6'>95 Hallo</span></td><td><span class='ansi-bg' style='background:#bc3fbc;color:#ffffff'>45 Hallo</span></td><td><span class='ansi-bg' style='background:#d670d6;color:#000000'>105 Hallo</span></td></tr>
<tr><td>türkis</td><td><span style='color:#11a8cd'>36 Hallo</span></td><td><span style='color:#29b8db'>96 Hallo</span></td><td><span class='ansi-bg' style='background:#11a8cd;color:#ffffff'>46 Hallo</span></td><td><span class='ansi-bg' style='background:#29b8db;color:#000000'>106 Hallo</span></td></tr>
<tr><td>weiß</td><td><span style='color:#e5e5e5'>37 Hallo</span></td><td><span style='color:#e5e5e5'>97 Hallo</span></td><td><span class='ansi-bg' style='background:#e5e5e5;color:#000000'>47 Hallo</span></td><td><span class='ansi-bg' style='background:#e5e5e5;color:#000000'>107 Hallo</span></td></tr>
</tbody>
</table>
</div>

Dazu die Schriftarten; `0` schaltet alles wieder aus, `39` und `49` nur die Schrift- und die Hintergrundfarbe:

<div class='ansi-demo ansi-styles'><span style='font-weight:bold'>1 fett</span><span style='opacity:0.55'>2 blass</span><span style='font-style:italic'>3 kursiv</span><span style='text-decoration:underline'>4 unterstrichen</span><span style='background:#cccccc;color:#1e1e1e;padding:0 0.3em'>7 vertauscht</span></div>

Mit `38;5;n` wird die Schrift eine von 256 Farben, mit `48;5;n` der Hintergrund: die 16 von oben, ein Würfel aus 6 × 6 × 6 Mischungen von Rot, Grün und Blau (ein Quadrat je Stufe Rot, darin Grün nach unten und Blau nach rechts) und 24 Graustufen. `ESC[38;5;208m` ist Orange:

<div class='ansi-demo'><div class='ansi-palette' style='--cols: 16'><span style='background:#000000;color:#ffffff' title='0'>0</span><span style='background:#cd3131;color:#ffffff' title='1'>1</span><span style='background:#0dbc79;color:#ffffff' title='2'>2</span><span style='background:#e5e510;color:#000000' title='3'>3</span><span style='background:#2472c8;color:#ffffff' title='4'>4</span><span style='background:#bc3fbc;color:#ffffff' title='5'>5</span><span style='background:#11a8cd;color:#ffffff' title='6'>6</span><span style='background:#e5e5e5;color:#000000' title='7'>7</span><span style='background:#666666;color:#ffffff' title='8'>8</span><span style='background:#f14c4c;color:#ffffff' title='9'>9</span><span style='background:#23d18b;color:#000000' title='10'>10</span><span style='background:#f5f543;color:#000000' title='11'>11</span><span style='background:#3b8eea;color:#ffffff' title='12'>12</span><span style='background:#d670d6;color:#000000' title='13'>13</span><span style='background:#29b8db;color:#000000' title='14'>14</span><span style='background:#ffffff;color:#000000' title='15'>15</span></div><div class='ansi-cube'><div class='ansi-palette' style='--cols: 6'><span style='background:#000000;color:#ffffff' title='16'>16</span><span style='background:#00005f;color:#ffffff' title='17'>17</span><span style='background:#000087;color:#ffffff' title='18'>18</span><span style='background:#0000af;color:#ffffff' title='19'>19</span><span style='background:#0000d7;color:#ffffff' title='20'>20</span><span style='background:#0000ff;color:#ffffff' title='21'>21</span><span style='background:#005f00;color:#ffffff' title='22'>22</span><span style='background:#005f5f;color:#ffffff' title='23'>23</span><span style='background:#005f87;color:#ffffff' title='24'>24</span><span style='background:#005faf;color:#ffffff' title='25'>25</span><span style='background:#005fd7;color:#ffffff' title='26'>26</span><span style='background:#005fff;color:#ffffff' title='27'>27</span><span style='background:#008700;color:#ffffff' title='28'>28</span><span style='background:#00875f;color:#ffffff' title='29'>29</span><span style='background:#008787;color:#ffffff' title='30'>30</span><span style='background:#0087af;color:#ffffff' title='31'>31</span><span style='background:#0087d7;color:#ffffff' title='32'>32</span><span style='background:#0087ff;color:#ffffff' title='33'>33</span><span style='background:#00af00;color:#ffffff' title='34'>34</span><span style='background:#00af5f;color:#ffffff' title='35'>35</span><span style='background:#00af87;color:#ffffff' title='36'>36</span><span style='background:#00afaf;color:#ffffff' title='37'>37</span><span style='background:#00afd7;color:#ffffff' title='38'>38</span><span style='background:#00afff;color:#ffffff' title='39'>39</span><span style='background:#00d700;color:#ffffff' title='40'>40</span><span style='background:#00d75f;color:#ffffff' title='41'>41</span><span style='background:#00d787;color:#000000' title='42'>42</span><span style='background:#00d7af;color:#000000' title='43'>43</span><span style='background:#00d7d7;color:#000000' title='44'>44</span><span style='background:#00d7ff;color:#000000' title='45'>45</span><span style='background:#00ff00;color:#000000' title='46'>46</span><span style='background:#00ff5f;color:#000000' title='47'>47</span><span style='background:#00ff87;color:#000000' title='48'>48</span><span style='background:#00ffaf;color:#000000' title='49'>49</span><span style='background:#00ffd7;color:#000000' title='50'>50</span><span style='background:#00ffff;color:#000000' title='51'>51</span></div><div class='ansi-palette' style='--cols: 6'><span style='background:#5f0000;color:#ffffff' title='52'>52</span><span style='background:#5f005f;color:#ffffff' title='53'>53</span><span style='background:#5f0087;color:#ffffff' title='54'>54</span><span style='background:#5f00af;color:#ffffff' title='55'>55</span><span style='background:#5f00d7;color:#ffffff' title='56'>56</span><span style='background:#5f00ff;color:#ffffff' title='57'>57</span><span style='background:#5f5f00;color:#ffffff' title='58'>58</span><span style='background:#5f5f5f;color:#ffffff' title='59'>59</span><span style='background:#5f5f87;color:#ffffff' title='60'>60</span><span style='background:#5f5faf;color:#ffffff' title='61'>61</span><span style='background:#5f5fd7;color:#ffffff' title='62'>62</span><span style='background:#5f5fff;color:#ffffff' title='63'>63</span><span style='background:#5f8700;color:#ffffff' title='64'>64</span><span style='background:#5f875f;color:#ffffff' title='65'>65</span><span style='background:#5f8787;color:#ffffff' title='66'>66</span><span style='background:#5f87af;color:#ffffff' title='67'>67</span><span style='background:#5f87d7;color:#ffffff' title='68'>68</span><span style='background:#5f87ff;color:#ffffff' title='69'>69</span><span style='background:#5faf00;color:#ffffff' title='70'>70</span><span style='background:#5faf5f;color:#000000' title='71'>71</span><span style='background:#5faf87;color:#000000' title='72'>72</span><span style='background:#5fafaf;color:#000000' title='73'>73</span><span style='background:#5fafd7;color:#000000' title='74'>74</span><span style='background:#5fafff;color:#000000' title='75'>75</span><span style='background:#5fd700;color:#000000' title='76'>76</span><span style='background:#5fd75f;color:#000000' title='77'>77</span><span style='background:#5fd787;color:#000000' title='78'>78</span><span style='background:#5fd7af;color:#000000' title='79'>79</span><span style='background:#5fd7d7;color:#000000' title='80'>80</span><span style='background:#5fd7ff;color:#000000' title='81'>81</span><span style='background:#5fff00;color:#000000' title='82'>82</span><span style='background:#5fff5f;color:#000000' title='83'>83</span><span style='background:#5fff87;color:#000000' title='84'>84</span><span style='background:#5fffaf;color:#000000' title='85'>85</span><span style='background:#5fffd7;color:#000000' title='86'>86</span><span style='background:#5fffff;color:#000000' title='87'>87</span></div><div class='ansi-palette' style='--cols: 6'><span style='background:#870000;color:#ffffff' title='88'>88</span><span style='background:#87005f;color:#ffffff' title='89'>89</span><span style='background:#870087;color:#ffffff' title='90'>90</span><span style='background:#8700af;color:#ffffff' title='91'>91</span><span style='background:#8700d7;color:#ffffff' title='92'>92</span><span style='background:#8700ff;color:#ffffff' title='93'>93</span><span style='background:#875f00;color:#ffffff' title='94'>94</span><span style='background:#875f5f;color:#ffffff' title='95'>95</span><span style='background:#875f87;color:#ffffff' title='96'>96</span><span style='background:#875faf;color:#ffffff' title='97'>97</span><span style='background:#875fd7;color:#ffffff' title='98'>98</span><span style='background:#875fff;color:#ffffff' title='99'>99</span><span style='background:#878700;color:#ffffff' title='100'>100</span><span style='background:#87875f;color:#ffffff' title='101'>101</span><span style='background:#878787;color:#ffffff' title='102'>102</span><span style='background:#8787af;color:#ffffff' title='103'>103</span><span style='background:#8787d7;color:#000000' title='104'>104</span><span style='background:#8787ff;color:#000000' title='105'>105</span><span style='background:#87af00;color:#000000' title='106'>106</span><span style='background:#87af5f;color:#000000' title='107'>107</span><span style='background:#87af87;color:#000000' title='108'>108</span><span style='background:#87afaf;color:#000000' title='109'>109</span><span style='background:#87afd7;color:#000000' title='110'>110</span><span style='background:#87afff;color:#000000' title='111'>111</span><span style='background:#87d700;color:#000000' title='112'>112</span><span style='background:#87d75f;color:#000000' title='113'>113</span><span style='background:#87d787;color:#000000' title='114'>114</span><span style='background:#87d7af;color:#000000' title='115'>115</span><span style='background:#87d7d7;color:#000000' title='116'>116</span><span style='background:#87d7ff;color:#000000' title='117'>117</span><span style='background:#87ff00;color:#000000' title='118'>118</span><span style='background:#87ff5f;color:#000000' title='119'>119</span><span style='background:#87ff87;color:#000000' title='120'>120</span><span style='background:#87ffaf;color:#000000' title='121'>121</span><span style='background:#87ffd7;color:#000000' title='122'>122</span><span style='background:#87ffff;color:#000000' title='123'>123</span></div><div class='ansi-palette' style='--cols: 6'><span style='background:#af0000;color:#ffffff' title='124'>124</span><span style='background:#af005f;color:#ffffff' title='125'>125</span><span style='background:#af0087;color:#ffffff' title='126'>126</span><span style='background:#af00af;color:#ffffff' title='127'>127</span><span style='background:#af00d7;color:#ffffff' title='128'>128</span><span style='background:#af00ff;color:#ffffff' title='129'>129</span><span style='background:#af5f00;color:#ffffff' title='130'>130</span><span style='background:#af5f5f;color:#ffffff' title='131'>131</span><span style='background:#af5f87;color:#ffffff' title='132'>132</span><span style='background:#af5faf;color:#ffffff' title='133'>133</span><span style='background:#af5fd7;color:#ffffff' title='134'>134</span><span style='background:#af5fff;color:#ffffff' title='135'>135</span><span style='background:#af8700;color:#ffffff' title='136'>136</span><span style='background:#af875f;color:#000000' title='137'>137</span><span style='background:#af8787;color:#000000' title='138'>138</span><span style='background:#af87af;color:#000000' title='139'>139</span><span style='background:#af87d7;color:#000000' title='140'>140</span><span style='background:#af87ff;color:#000000' title='141'>141</span><span style='background:#afaf00;color:#000000' title='142'>142</span><span style='background:#afaf5f;color:#000000' title='143'>143</span><span style='background:#afaf87;color:#000000' title='144'>144</span><span style='background:#afafaf;color:#000000' title='145'>145</span><span style='background:#afafd7;color:#000000' title='146'>146</span><span style='background:#afafff;color:#000000' title='147'>147</span><span style='background:#afd700;color:#000000' title='148'>148</span><span style='background:#afd75f;color:#000000' title='149'>149</span><span style='background:#afd787;color:#000000' title='150'>150</span><span style='background:#afd7af;color:#000000' title='151'>151</span><span style='background:#afd7d7;color:#000000' title='152'>152</span><span style='background:#afd7ff;color:#000000' title='153'>153</span><span style='background:#afff00;color:#000000' title='154'>154</span><span style='background:#afff5f;color:#000000' title='155'>155</span><span style='background:#afff87;color:#000000' title='156'>156</span><span style='background:#afffaf;color:#000000' title='157'>157</span><span style='background:#afffd7;color:#000000' title='158'>158</span><span style='background:#afffff;color:#000000' title='159'>159</span></div><div class='ansi-palette' style='--cols: 6'><span style='background:#d70000;color:#ffffff' title='160'>160</span><span style='background:#d7005f;color:#ffffff' title='161'>161</span><span style='background:#d70087;color:#ffffff' title='162'>162</span><span style='background:#d700af;color:#ffffff' title='163'>163</span><span style='background:#d700d7;color:#ffffff' title='164'>164</span><span style='background:#d700ff;color:#ffffff' title='165'>165</span><span style='background:#d75f00;color:#ffffff' title='166'>166</span><span style='background:#d75f5f;color:#ffffff' title='167'>167</span><span style='background:#d75f87;color:#ffffff' title='168'>168</span><span style='background:#d75faf;color:#ffffff' title='169'>169</span><span style='background:#d75fd7;color:#000000' title='170'>170</span><span style='background:#d75fff;color:#000000' title='171'>171</span><span style='background:#d78700;color:#000000' title='172'>172</span><span style='background:#d7875f;color:#000000' title='173'>173</span><span style='background:#d78787;color:#000000' title='174'>174</span><span style='background:#d787af;color:#000000' title='175'>175</span><span style='background:#d787d7;color:#000000' title='176'>176</span><span style='background:#d787ff;color:#000000' title='177'>177</span><span style='background:#d7af00;color:#000000' title='178'>178</span><span style='background:#d7af5f;color:#000000' title='179'>179</span><span style='background:#d7af87;color:#000000' title='180'>180</span><span style='background:#d7afaf;color:#000000' title='181'>181</span><span style='background:#d7afd7;color:#000000' title='182'>182</span><span style='background:#d7afff;color:#000000' title='183'>183</span><span style='background:#d7d700;color:#000000' title='184'>184</span><span style='background:#d7d75f;color:#000000' title='185'>185</span><span style='background:#d7d787;color:#000000' title='186'>186</span><span style='background:#d7d7af;color:#000000' title='187'>187</span><span style='background:#d7d7d7;color:#000000' title='188'>188</span><span style='background:#d7d7ff;color:#000000' title='189'>189</span><span style='background:#d7ff00;color:#000000' title='190'>190</span><span style='background:#d7ff5f;color:#000000' title='191'>191</span><span style='background:#d7ff87;color:#000000' title='192'>192</span><span style='background:#d7ffaf;color:#000000' title='193'>193</span><span style='background:#d7ffd7;color:#000000' title='194'>194</span><span style='background:#d7ffff;color:#000000' title='195'>195</span></div><div class='ansi-palette' style='--cols: 6'><span style='background:#ff0000;color:#ffffff' title='196'>196</span><span style='background:#ff005f;color:#ffffff' title='197'>197</span><span style='background:#ff0087;color:#ffffff' title='198'>198</span><span style='background:#ff00af;color:#ffffff' title='199'>199</span><span style='background:#ff00d7;color:#ffffff' title='200'>200</span><span style='background:#ff00ff;color:#ffffff' title='201'>201</span><span style='background:#ff5f00;color:#ffffff' title='202'>202</span><span style='background:#ff5f5f;color:#000000' title='203'>203</span><span style='background:#ff5f87;color:#000000' title='204'>204</span><span style='background:#ff5faf;color:#000000' title='205'>205</span><span style='background:#ff5fd7;color:#000000' title='206'>206</span><span style='background:#ff5fff;color:#000000' title='207'>207</span><span style='background:#ff8700;color:#000000' title='208'>208</span><span style='background:#ff875f;color:#000000' title='209'>209</span><span style='background:#ff8787;color:#000000' title='210'>210</span><span style='background:#ff87af;color:#000000' title='211'>211</span><span style='background:#ff87d7;color:#000000' title='212'>212</span><span style='background:#ff87ff;color:#000000' title='213'>213</span><span style='background:#ffaf00;color:#000000' title='214'>214</span><span style='background:#ffaf5f;color:#000000' title='215'>215</span><span style='background:#ffaf87;color:#000000' title='216'>216</span><span style='background:#ffafaf;color:#000000' title='217'>217</span><span style='background:#ffafd7;color:#000000' title='218'>218</span><span style='background:#ffafff;color:#000000' title='219'>219</span><span style='background:#ffd700;color:#000000' title='220'>220</span><span style='background:#ffd75f;color:#000000' title='221'>221</span><span style='background:#ffd787;color:#000000' title='222'>222</span><span style='background:#ffd7af;color:#000000' title='223'>223</span><span style='background:#ffd7d7;color:#000000' title='224'>224</span><span style='background:#ffd7ff;color:#000000' title='225'>225</span><span style='background:#ffff00;color:#000000' title='226'>226</span><span style='background:#ffff5f;color:#000000' title='227'>227</span><span style='background:#ffff87;color:#000000' title='228'>228</span><span style='background:#ffffaf;color:#000000' title='229'>229</span><span style='background:#ffffd7;color:#000000' title='230'>230</span><span style='background:#ffffff;color:#000000' title='231'>231</span></div></div><div class='ansi-palette' style='--cols: 24'><span style='background:#080808;color:#ffffff' title='232'>232</span><span style='background:#121212;color:#ffffff' title='233'>233</span><span style='background:#1c1c1c;color:#ffffff' title='234'>234</span><span style='background:#262626;color:#ffffff' title='235'>235</span><span style='background:#303030;color:#ffffff' title='236'>236</span><span style='background:#3a3a3a;color:#ffffff' title='237'>237</span><span style='background:#444444;color:#ffffff' title='238'>238</span><span style='background:#4e4e4e;color:#ffffff' title='239'>239</span><span style='background:#585858;color:#ffffff' title='240'>240</span><span style='background:#626262;color:#ffffff' title='241'>241</span><span style='background:#6c6c6c;color:#ffffff' title='242'>242</span><span style='background:#767676;color:#ffffff' title='243'>243</span><span style='background:#808080;color:#ffffff' title='244'>244</span><span style='background:#8a8a8a;color:#ffffff' title='245'>245</span><span style='background:#949494;color:#000000' title='246'>246</span><span style='background:#9e9e9e;color:#000000' title='247'>247</span><span style='background:#a8a8a8;color:#000000' title='248'>248</span><span style='background:#b2b2b2;color:#000000' title='249'>249</span><span style='background:#bcbcbc;color:#000000' title='250'>250</span><span style='background:#c6c6c6;color:#000000' title='251'>251</span><span style='background:#d0d0d0;color:#000000' title='252'>252</span><span style='background:#dadada;color:#000000' title='253'>253</span><span style='background:#e4e4e4;color:#000000' title='254'>254</span><span style='background:#eeeeee;color:#000000' title='255'>255</span></div></div>

Mit `38;2;r;g;b` (Hintergrund `48;2;r;g;b`) wird es jede Farbe aus Rot, Grün und Blau, je 0 bis 255, so wie in CSS; `ESC[38;2;255;128;0m` ist ebenfalls Orange. Damit lassen sich Verläufe malen:

<div class='ansi-demo'><div class='ansi-gradient'><span style='background:#ff5000'></span><span style='background:#f3550b'></span><span style='background:#e85a16'></span><span style='background:#dd5f21'></span><span style='background:#d2642c'></span><span style='background:#c76a37'></span><span style='background:#bc6f42'></span><span style='background:#b1744d'></span><span style='background:#a67958'></span><span style='background:#9b7e63'></span><span style='background:#90846e'></span><span style='background:#858979'></span><span style='background:#798e85'></span><span style='background:#6e9390'></span><span style='background:#63999b'></span><span style='background:#589ea6'></span><span style='background:#4da3b1'></span><span style='background:#42a8bc'></span><span style='background:#37adc7'></span><span style='background:#2cb3d2'></span><span style='background:#21b8dd'></span><span style='background:#16bde8'></span><span style='background:#0bc2f3'></span><span style='background:#00c8ff'></span></div></div>

Wie die acht Farben genau aussehen, legt das Terminal fest; im hellen Farbschema von VS Code ist Gelb ein anderes Gelb als oben. Wer eine bestimmte Farbe will, nimmt `38;2;r;g;b`.

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
