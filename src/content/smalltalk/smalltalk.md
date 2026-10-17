<div class='meta'>
image: smalltalk-logo.png
kind: steckbrief
needs_contrast: light
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Smalltalk <span style='font-size: 80%;'>(1980)</span>

<div class='floatright fr-wide' style='width: 40em;'>
    <img src='smalltalk-team.webp' alt='Drei Porträtfotos nebeneinander, Alan Kay, Adele Goldberg und Dan Ingalls'>
    <p>Alan Kay, Adele Goldberg und Dan Ingalls</p>
</div>

<p class='abstract'>
Smalltalk wurde in den 1970er Jahren von Alan Kay, Adele Goldberg und Dan Ingalls am Xerox Palo Alto Research Center (PARC) entwickelt, 1980 als Smalltalk-80 veröffentlicht und gilt als eine der ersten objektorientierten Programmiersprachen. Smalltalk wurde ursprünglich für die Entwicklung von grafischen Benutzeroberflächen und interaktiven Anwendungen verwendet.
</p>

Ein bekanntes Beispiel ist das erste grafische Benutzerinterface des Xerox Alto Computers. Heutzutage ist Smalltalk weniger verbreitet, aber immer noch relevant in bestimmten Nischen, insbesondere in der Forschung und Lehre zur objektorientierten Programmierung. Es wird auch in einigen spezialisierten industriellen Anwendungen und von Enthusiasten verwendet, die die Sprache für ihre Einfachheit und Eleganz schätzen.

## Hello, world!

Smalltalk ist eine Skriptsprache, was bedeutet, dass der Code zur Laufzeit interpretiert wird. Du hast zwei Möglichkeiten, Smalltalk-Code auszuführen:

1. Du kannst Smalltalk-Code direkt in der Smalltalk-Shell ausführen.
2. Du kannst Smalltalk-Code in einer Textdatei speichern und dann ausführen.

**Möglichkeit 1:** Smalltalk-Code in der Smalltalk-Shell ausführen

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.webp' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Ein frischer Workspace ohne Ordner, das Terminal offen
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='code-with-terminal.webp' alt='Ein frischer Workspace mit der Willkommensseite, darunter das offene Terminal mit der Eingabeaufforderung student@workspace:~$.'>

Starte nun die Smalltalk-Shell, indem du `gst` eingibst und dann <kbd>Enter</kbd> drückst. Du solltest eine Ausgabe wie diese sehen:

<!-- tutorial-screenshot
press: Control+l
terminal-run: gst
wait-for-text: st>
sleep: 0.5
crop-terminal-lines: auto
-->

<img class='full' src='gst.webp' alt='Im Terminal läuft gst, meldet »GNU Smalltalk ready« und wartet mit st&gt; auf Smalltalk-Code.'>

Jetzt kannst du Smalltalk-Code direkt in der Shell eingeben und ausführen. Schreibe einfach `Transcript show: 'Hello, World!'; cr.` und drücke <kbd>Enter</kbd>. Du solltest die Ausgabe `Hello, world!` sehen.

Du kannst die Smalltalk-Shell wieder beenden, indem du `ObjectMemory quit` eingibst und <kbd>Enter</kbd> drückst oder einfach <kbd>Strg</kbd><kbd>D</kbd> drückst.

**Möglichkeit 2:** Smalltalk-Code in einer Textdatei speichern und ausführen

Smalltalk-Programme werden in Textdateien mit der Endung `.st` geschrieben. Ein Smalltalk-Interpreter liest anschließend den Quelltext und führt ihn aus.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Die Shell wieder verlassen, das Terminal schließen
terminal-run: ObjectMemory quit
terminal-wait-for-prompt
press: Control+l
press: Control+j
show-left-sidebar
sleep: 0.5
-->

<img class='full' src='fresh-start.webp' alt='Der Workspace ohne geöffneten Ordner, links »No Folder Opened«, rechts die Willkommensseite mit »New File…«.'>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File« (oder bestätige einfach mit <kbd>Enter</kbd>).

<!-- tutorial-screenshot
click: New File...
wait-for-text: Text File
sleep: 0.5
crop-bottom: 77%
-->

<img class='full' src='choose-filename.webp' alt='Nach »New File…« fragt oben ein Feld nach dem Dateityp, »Text File« ist ausgewählt.'>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.st, smalltalk)

Da Visual Studio Code noch nicht weiß, dass es sich um Smalltalk-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
# Das Programm in eine noch namenlose Datei
press: Enter
sleep: 1
hide-left-sidebar
type: <- file:hello.st
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit Transcript show: &#39;Hello, World!&#39;; cr., ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.st` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.st`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.st
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.st.'>

<div class='hint'>
Achte darauf, dass du deine Datei nicht aus Versehen <code>hello.st.</code> (mit einem Punkt am Ende) nennst, da Visual Studio Code als Dateiname <code>Transcript show: 'Hello, World!'; cr.</code> vorschlägt.
</div>

Da Smalltalk standardmäßig nicht von Visual Studio Code unterstützt wird, müssen wir noch eine passende Erweiterung installieren. Klicke dazu auf das Erweiterungs-Symbol <img src='../basics/extensions.webp' style='border-radius: 4px; height: 1.5em;' alt='vier Quadrate, eines davon abgehoben'> in der Seitenleiste oder drücke <kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd>. Suche nach der Erweiterung »Pharo Language Support« und installiere sie. Die Erweiterung bringt eine zweite Erweiterung mit. Weil beide Herausgeber nicht verifiziert sind, fragt Visual Studio Code, ob du ihnen vertraust; bestätige mit »Trust Publishers & Install«.

<!-- tutorial-screenshot
# Speichern, dann die Erweiterung im Marketplace
press: Enter
wait-for-file: hello.st
sleep: 1
press: Control+Shift+x
wait-for-text: Marketplace
type: Pharo Language Support
sleep: 3
crop-bottom: 50%
-->

<img class='full' src='smalltalk-syntax.webp' alt='Links der Marketplace mit der Suche nach Pharo Language Support, ganz oben die Erweiterung von badetitou mit »Install«; rechts hello.st, noch einfarbig.'>

Alternativ kannst du auch <kbd>Strg</kbd><kbd>P</kbd> drücken und `ext install badetitou.pharo-language-server` eingeben, um die Erweiterung zu installieren.

Anschließend solltest du dein Smalltalk-Programm farbig sehen:

<!-- tutorial-screenshot
# Die Erweiterung installieren, wie es der Text als zweiten Weg
# nennt; danach ist der Code farbig
press: Control+p
type: ext install badetitou.pharo-language-server
wait-for-text: Press Enter to install extension
press: Enter
# Der Herausgeber ist nicht verifiziert: VS Code fragt nach
wait-for-text: Trust Publishers & Install
click: Trust Publishers & Install
sleep: 10
# Neu öffnen: erst dann kennt VS Code die Sprache
close-tab: hello.st
open-file: hello.st
hide-left-sidebar
sleep: 2
crop-bottom: 78%
-->

<img class='full' src='syntax-highlighting.webp' alt='hello.st, jetzt mit farbiger Syntax.'>

### Skript ausführen

Um unser Programm auszuführen, müssen wir den Smalltalk-Interpreter aufrufen (in unserem Fall `gst`) und ihm den Dateinamen unseres Programms übergeben.

Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst und gib folgenden Befehl ein:

```bash
gst hello.st
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>gst he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.st</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
press: Control+l
terminal-run: gst hello.st
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello.webp' alt='Im Terminal gibt gst hello.st die Zeile Hello, World! aus.'>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird Lisp eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `show` das Wort `sho` zu schreiben:

```smalltalk
Transcript sho: 'Hello, World!'; cr.
```

Speichere die Datei und führe das Skript erneut aus:

```bash
gst hello.st
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Smalltalk sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<!-- tutorial-screenshot
# Der Fehler aus dem Text; die lange Meldung braucht das ganze Terminal
write-file: hello.st <- file:hello-error.st
terminal-maximize
press: Control+l
terminal-run: gst hello.st
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello-error.webp' alt='gst meldet zu hello.st: TextCollector error: did not understand #sho, mit hello.st:1 in der letzten Zeile.'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 1) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

### Shebang `#!`

Bisher musst du, um dein Skript auszuführen, immer den Smalltalk-Interpreter explizit aufrufen. Wenn du dein Skript wie ein normales Programm ausführen möchtest, kannst du ein sogenanntes Shebang am Anfang deiner Datei hinzufügen. Das Shebang besteht aus einer Raute `#` gefolgt von einem Ausrufezeichen `!` und dem Pfad zum Smalltalk-Interpreter. In unserem Fall sieht das so aus:

```smalltalk
#!/usr/bin/env gst
~ Transcript show: 'Hello, World!'; cr.
```

Füge diese Zeile ganz oben in deinem Skript ein und speichere die Datei. Bevor wir das Skript ausführen können, müssen wir es noch ausführbar machen. Das machen wir mit dem Befehl `chmod`:

```bash
chmod +x hello.st
```
Jetzt können wir das Skript direkt ausführen:

```bash
./hello.st
```
<!-- tutorial-screenshot
# Das Shebang über dem Programm; Editor und Terminal zusammen
write-file: hello.st <- file:hello-shebang.st
click: selector:.part.panel [aria-label*="Restore Panel"]
open-file: hello.st
terminal-open
press: Control+l
terminal-run: chmod +x hello.st
terminal-wait-for-prompt
terminal-run: ./hello.st
terminal-wait-for-prompt
# Mitten im Editor zeigt nichts einen Tooltip (in der Statusleiste
# liegt dort »Spaces: 4«)
move-mouse: selector:.part.editor
sleep: 0.5
-->

<img class='full' src='shebang.webp' alt='Oben hello.st mit #!/usr/bin/env gst in der ersten Zeile, unten im Terminal chmod +x hello.st und ./hello.st, das Hello, World! ausgibt.'>

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen. An diesem Beispiel kannst du sehen, wie man in Smalltalk Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.st, smalltalk)

Speichere die Datei unter dem Namen `factor.st` und führe sie aus:

```bash
gst factor.st
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: factor.st <- file:factor.st
terminal-maximize
press: Control+l
terminal-run: gst factor.st
wait-for-text: number:
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='gst factor.st fragt nach einer Zahl und gibt für 123 die Faktoren 3 und 41 aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Anders als andere Programmiersprachen kann Smalltalk auch Zahlen wie 3000000000 und 123456789123456789 in Sekundenbruchteilen zerlegen, allerdings beendet sich das Programm nicht (probier es gern aus, du kannst das Programm mit <kbd>Strg</kbd><kbd>C</kbd> abbrechen). Falls du einen sachdienlichen Hinweis hast, schreib bitte eine E-Mail an <a href='mailto:${contact_email}'>${contact_email}</a>.

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Smalltalk Arrays verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.st, smalltalk)

Speichere das Skript unter dem Namen `bubblesort.st` und führe es aus:

<!-- tutorial-screenshot
write-file: bubblesort.st <- file:bubblesort.st
press: Control+l
terminal-run: gst bubblesort.st
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='gst bubblesort.st gibt zehn Zahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert. Versuche, den Quelltext so zu verändern, dass statt 10 Zahlen 100 oder mehr Zahlen sortiert werden.

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Smalltalk-Skript schreiben und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Smalltalk wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Skripte schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!
