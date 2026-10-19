<div class='meta'>
image: python-logo.png
kind: steckbrief
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Python <span style='font-size: 80%;'>(1991)</span>

<div class='floatright' style='width: 16em;'>
    <img src='guido.webp' alt='Foto von Guido van Rossum, dem Erfinder von Python, lachend mit einem Becher in der Hand'>
    <p>Guido van Rossum</p>
</div>

<p class='abstract'>
Python, entwickelt von Guido van Rossum und erstmals 1991 veröffentlicht, ist eine der beliebtesten Programmiersprachen der Welt. Python wird aufgrund seiner Einfachheit und Lesbarkeit in vielen Bereichen eingesetzt, darunter Webentwicklung, Datenanalyse, künstliche Intelligenz und maschinelles Lernen.
</p>

Bekannte Beispiele für Anwendungen, die in Python entwickelt wurden, sind das Web-Framework Django, die Datenanalysebibliothek Pandas und das maschinelle Lernframework TensorFlow. Heutzutage ist Python äußerst relevant und weit verbreitet, da es eine aktive Community und eine Vielzahl von Bibliotheken und Frameworks bietet, die die Entwicklung in verschiedenen Domänen unterstützen.

## Hello, world!

Python ist eine Skriptsprache, was bedeutet, dass der Code zur Laufzeit interpretiert wird. Du hast zwei Möglichkeiten, Python-Code auszuführen:

1. Du kannst Python-Code direkt in der Python-Shell ausführen.
2. Du kannst Python-Code in einer Textdatei speichern und dann ausführen.

**Möglichkeit 1:** Python-Code in der Python-Shell ausführen

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Ein frischer Workspace ohne Ordner, das Terminal offen
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='code-with-terminal.webp' alt='Ein frischer Workspace mit der Willkommensseite, darunter das offene Terminal mit der Eingabeaufforderung student@workspace:~$.'>

Starte nun die Python-Shell, indem du `python3` eingibst und dann <kbd>Enter</kbd> drückst. Du solltest eine Ausgabe wie diese sehen:

<!-- tutorial-screenshot
press: Control+l
terminal-run: python3
wait-for-text: >>>
crop-terminal-lines: auto
-->

<img class='full' src='python-repl.webp' alt='Im Terminal läuft python3, zeigt seine Version und wartet mit &gt;&gt;&gt; auf Python-Code.'>

Jetzt kannst du Python-Code direkt in der Shell eingeben und ausführen. Schreibe einfach `print("Hello, world!")` und drücke <kbd>Enter</kbd>. Du solltest die Ausgabe `Hello, world!` sehen.

Du kannst die Python-Shell wieder beenden, indem du `exit()` eingibst und <kbd>Enter</kbd> drückst oder einfach <kbd>Strg</kbd><kbd>D</kbd> drückst.

**Möglichkeit 2:** Python-Code in einer Textdatei speichern und ausführen

Python-Programme werden in Textdateien mit der Endung `.py` geschrieben. Ein Python-Interpreter liest anschließend den Quelltext und führt ihn aus.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Die Python-Shell wieder verlassen, das Terminal schließen
terminal-run: exit()
terminal-wait-for-prompt
terminal-run: clear
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

_include_file(hello.py, python)

Da Visual Studio Code noch nicht weiß, dass es sich um Python-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
press: Enter
sleep: 1
hide-left-sidebar
type: print("Hello, World!")
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit print("Hello, World!"), ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.py` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.py`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.py
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.py.'>

Sobald du die Datei gespeichert hast, wird sie automatisch als Python-Datei erkannt und die Syntax wird hervorgehoben.

<!-- tutorial-screenshot
press: Enter
wait-for-file: hello.py
sleep: 1.5
crop-bottom: 78%
-->

<img class='full' src='syntax-highlighting.webp' alt='Die gespeicherte Datei hello.py, jetzt mit farbiger Syntax.'>

### Skript ausführen

Um unser Programm auszuführen, müssen wir den Python-Interpreter aufrufen und ihm den Dateinamen unseres Programms übergeben.

Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst und gib folgenden Befehl ein:

```bash
python3 hello.py
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>python3 he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.py</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
press: Control+l
terminal-run: python3 hello.py
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello.webp' alt='Im Terminal gibt python3 hello.py die Zeile Hello, World! aus.'>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird Python eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `print` das Wort `prin` zu schreiben:

```python
prin("Hello, World!")
```

Speichere die Datei und führe das Skript erneut aus:

```bash
python3 hello.py
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Python sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<!-- tutorial-screenshot
write-file: hello.py <- code:prin(
terminal-maximize
press: Control+l
terminal-run: python3 hello.py
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello-error.webp' alt='Python meldet zu prin("Hello, World!") einen Traceback: Zeile 1, NameError: name »prin« is not defined. Did you mean: »print«?'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 1) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

### Shebang `#!`

Bisher musst du, um dein Skript auszuführen, immer den Python-Interpreter explizit aufrufen. Wenn du dein Skript wie ein normales Programm ausführen möchtest, kannst du ein sogenanntes Shebang am Anfang deiner Datei hinzufügen. Das Shebang besteht aus einer Raute `#` gefolgt von einem Ausrufezeichen `!` und dem Pfad zum Python-Interpreter. In unserem Fall sieht das so aus:

```python
#!/usr/bin/env python3
~ print("Hello, World!")
```

Füge diese Zeile ganz oben in deinem Skript ein und speichere die Datei. Bevor wir das Skript ausführen können, müssen wir es noch ausführbar machen. Das machen wir mit dem Befehl `chmod`:

```bash
chmod +x hello.py
```
Jetzt können wir das Skript direkt ausführen:

```bash
./hello.py
```

<!-- tutorial-screenshot
# Das Shebang über dem print; Editor und Terminal zusammen
write-file: hello.py <- file:hello-shebang.py
click: selector:.part.panel [aria-label*="Restore Panel"]
open-file: hello.py
terminal-open
press: Control+l
terminal-run: chmod +x hello.py
terminal-wait-for-prompt
terminal-run: ./hello.py
terminal-wait-for-prompt
move-mouse: selector:.part.statusbar
sleep: 0.5
-->

<img class='full' src='shebang.webp' alt='Oben hello.py mit #!/usr/bin/env python3 in der ersten Zeile, unten im Terminal chmod +x hello.py und ./hello.py, das Hello, World! ausgibt.'>

## Python-Skripte direkt in Visual Studio Code ausführen

Visual Studio Code bietet auch eine integrierte Möglichkeit, Python-Skripte direkt auszuführen, ohne dass du das Skript jedesmal im Terminal ausführen musst. Dazu musst du die Python-Erweiterung installieren. Klicke dazu auf das Extensions-Symbol <img src='../basics/extensions.svg' style='border-radius: 4px; height: 1.5em;' alt='vier Quadrate, eines davon abgehoben'> in der Seitenleiste und suche nach »Python«. Klicke auf »Install«, um die Erweiterung zu installieren.

<!-- tutorial-screenshot
# Die Erweiterung im Marketplace, nicht installiert
press: Control+Shift+x
wait-for-text: Marketplace
type: python
wait-for-text: ms-python
sleep: 1.5
crop-bottom: 50%
-->

<img class='full' src='python-ext.webp' alt='Links der Marketplace mit der Suche nach python, ganz oben die Erweiterung »Python« von ms-python mit dem Knopf »Install«; rechts hello.py.'>

Du kannst nun dein Skript einfach direkt aus dem Editor starten, indem du rechts oben auf das Run-Symbol <img src='../basics/run-button.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Dreieck, das nach rechts zeigt'> klickst.

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen.
An diesem Beispiel kannst du sehen, wie man in Python Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.py, python)

Speichere die Datei unter dem Namen `factor.py` und führe sie aus:

<!-- tutorial-screenshot
write-file: factor.py <- file:factor.py
hide-left-sidebar
terminal-open
press: Control+l
terminal-run: python3 factor.py
wait-for-text: Enter a number
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='python3 factor.py fragt nach einer Zahl und gibt für 123 die Primfaktoren 3 und 41 aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Anders als andere Programmiersprachen kann Python auch die Zahl 3000000000 in Sekundenbruchteilen zerlegen. Wenn du allerdings eine sehr große Zahl wie 123456789123456789 verwendest, dauert die Berechnung sehr lange (probier es gern aus, du kannst das Programm mit <kbd>Strg</kbd><kbd>C</kbd> abbrechen).

<div class='hint try'>
<strong>Ausprobieren:</strong> Warum dauert es bei <code>123456789123456789</code> so lange? Ersetze <code>num = num / i</code> durch <code>num = num // i</code> und probier die Zahl noch einmal. Starte danach <code>python3</code> ohne Dateinamen und vergleiche <code>7 / 2</code> mit <code>7 // 2</code> und <code>123456789123456789 / 3</code> mit <code>123456789123456789 // 3</code>. Was ist der Unterschied? Mit <kbd>Strg</kbd><kbd>D</kbd> beendest du <code>python3</code>.
</div>

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Python Arrays verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.py, python)

Speichere das Skript unter dem Namen `bubblesort.py` und führe es aus:

<!-- tutorial-screenshot
write-file: bubblesort.py <- file:bubblesort.py
press: Control+l
terminal-run: python3 bubblesort.py
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='python3 bubblesort.py gibt zehn Zufallszahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere <code>bubblesort.py</code> und starte das Skript noch einmal:
<ul>
<li>Sortiere 100 Zahlen statt 10: Welche Zahl musst du dafür ändern?</li>
<li>Sortiere absteigend, die größte Zahl zuerst: Welches Zeichen musst du dafür ändern?</li>
<li>Ersetze die Zeile mit <code>random.randint</code> durch <code>array = ["Kiwi", "Apfel", "Birne"]</code>. Klappt das Sortieren auch mit Wörtern? Und was meldet Python bei <code>array = [3, "Apfel", 1]</code>?</li>
</ul>
</div>

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Python-Skript schreiben und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Python wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Skripte schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!
