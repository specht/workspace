<div class='meta'>
image: fortran-logo.png
kind: steckbrief
curriculum: "Sek I: Geschichte der Informatik (Lebensläufe und Leistungen bedeutender Persönlichkeiten); Oberstufe: Algorithmen und Softwareentwicklung (ein Programm übersetzen, Fehlermeldungen lesen, einfache Datentypen, Bubblesort ändern)"
topics: [programmiersprachen, geschichte]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Fortran <span style='font-size: 80%;'>(1953)</span>

<p class='abstract'>
FORTRAN, kurz für "Formula Translation," wurde ab 1953 von einem Team um John Backus bei IBM entwickelt; der erste Compiler kam 1957 und ist eine der ältesten Programmiersprachen. Sie wurde ursprünglich für wissenschaftliche und technische Anwendungen entwickelt und wird heute noch in diesen Bereichen eingesetzt. Fortran ist eine imperative Programmiersprache, die speziell für numerische Berechnungen optimiert ist. Sie bietet eine Vielzahl von Funktionen und Bibliotheken für mathematische und wissenschaftliche Berechnungen.
</p>

_person(backus)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>The FORTRAN Automatic Coding System</strong>, John Backus und andere, Western Joint Computer Conference, 1957. Der Aufsatz, in dem sie ihr fertiges System für den IBM 704 vorstellten: die Sprache, und wie der Übersetzer daraus Programme macht, die so gut sein sollten wie von Hand geschrieben. Das Computer History Museum hat ihn als <a href='https://archive.computerhistory.org/resources/text/Fortran/102663113.05.01.acc.pdf'>PDF</a> frei ins Netz gestellt.</p>
</div>

FORTRAN wurde vor allem in Bereichen eingesetzt, die intensive numerische Berechnungen erfordern, wie zum Beispiel in der Wettervorhersage, der numerischen Strömungsmechanik und der linearen Algebra. Ein bekanntes Programm in Fortran ist das Weather Research and Forecasting (WRF) Model, das zur Wettervorhersage und Klimaforschung eingesetzt wird. FORTRAN wird auch in der Hochenergiephysik, der Astronomie und der Chemie eingesetzt.

## Hello, world!

Fortran-Programme werden in Textdateien mit der Endung `.f90` geschrieben. Diese Dateien werden anschließend von einem Compiler in ausführbare Dateien übersetzt, die auf deinem Computer direkt ausgeführt werden können. Es gibt eine Vielzahl von Compilern, die du verwenden kannst, aber wir werden hier den freien »GNU Fortran Compiler« `gfortran` verwenden, der auf den meisten Systemen verfügbar ist.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Ein frischer Workspace ohne Ordner
show-left-sidebar
sleep: 0.5
-->

<img class='full' src='fresh-start.webp' alt='Der Workspace ohne geöffneten Ordner, links »No Folder Opened«, rechts die Willkommensseite mit »New File…«.'>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File«.

<!-- tutorial-screenshot
click: New File...
wait-for-text: Text File
sleep: 0.5
crop-bottom: 77%
-->

<img class='full' src='choose-filename.webp' alt='Nach »New File…« fragt oben ein Feld nach dem Dateityp, »Text File« ist ausgewählt.'>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.f90, fortran)

Da Visual Studio Code noch nicht weiß, dass es sich um Fortran-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
# Das Programm in eine noch namenlose Datei
press: Enter
sleep: 1
hide-left-sidebar
type: <- file:hello.f90
sleep: 0.5
crop-bottom: 60%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit dem Programm HelloWorld in drei Zeilen, ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.f90` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.f90`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.f90
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.f90.'>

Da Fortran standardmäßig nicht von Visual Studio Code unterstützt wird, müssen wir noch eine passende Erweiterung installieren. Klicke dazu auf das Erweiterungs-Symbol <img src='../basics/extensions.svg' style='border-radius: 4px; height: 1.5em;' alt='vier Quadrate, eines davon abgehoben'> in der Seitenleiste oder drücke <kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd>. Suche nach der Erweiterung »Photran« und installiere sie. Weil der Herausgeber nicht verifiziert ist, fragt Visual Studio Code, ob du ihm vertraust; bestätige mit »Trust Publisher & Install«.

<!-- tutorial-screenshot
# Speichern, dann die Erweiterung im Marketplace
press: Enter
wait-for-file: hello.f90
sleep: 1
press: Control+Shift+x
wait-for-text: Marketplace
type: photran
sleep: 3
crop-bottom: 50%
-->

<img class='full' src='fortran-syntax.webp' alt='Links der Marketplace mit der Suche nach photran, ganz oben die Erweiterung Photran von FIUBA mit »Install«; rechts hello.f90, noch einfarbig.'>

Alternativ kannst du auch <kbd>Strg</kbd><kbd>P</kbd> drücken und `ext install fiuba.photran-lsp-client-vscode` eingeben, um die Erweiterung zu installieren.

Anschließend solltest du dein Fortran-Programm farbig sehen:

<!-- tutorial-screenshot
# Die Erweiterung installieren, wie es der Text als zweiten Weg
# nennt; danach ist der Code farbig
press: Control+p
type: ext install fiuba.photran-lsp-client-vscode
wait-for-text: Press Enter to install extension
press: Enter
# Der Herausgeber ist nicht verifiziert: VS Code fragt nach
wait-for-text: Trust Publisher & Install
click: Trust Publisher & Install
sleep: 10
# Neu öffnen: erst dann kennt VS Code die Sprache
close-tab: hello.f90
open-file: hello.f90
hide-left-sidebar
sleep: 2
crop-bottom: 56%
-->

<img class='full' src='syntax-highlighting.webp' alt='hello.f90 mit farbiger Syntax, program HelloWorld, print *, "Hello, World!", end program HelloWorld.'>

### Kompilieren und ausführen

Bevor wir das Programm ausführen können, müssen wir es kompilieren. Dadurch wird der Quelltext in Maschinencode übersetzt, den dein Computer ausführen kann.

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='lets-compile.webp' alt='Oben hello.f90, darunter das offene Terminal mit der Eingabeaufforderung.'>

Um das Programm zu kompilieren, gib folgenden Befehl ein:

```bash
gfortran hello.f90 -o hello
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>gfortran he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.f90</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Wenn du keinen Fehler gemacht hast, wird das Programm erfolgreich kompiliert und die ausführbare Datei `hello` wird im selben Verzeichnis erstellt. Du kannst dies überprüfen, indem du dir die Dateien im aktuellen Verzeichnis mit `ls` oder `ls -l` anzeigen lässt:

<!-- tutorial-screenshot
press: Control+l
terminal-run: gfortran hello.f90 -o hello
terminal-wait-for-prompt
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls.webp' alt='Im Terminal übersetzt gfortran hello.f90 -o hello das Programm, danach zeigt ls -l die ausführbare Datei hello in Grün und hello.f90.'>

Die grüne Datei `hello` ist die ausführbare Datei – im Unterschied zu Windows, wo ausführbare Dateien die Endung `.exe` haben, haben ausführbare Dateien unter Linux keine Endung. Um das Programm auszuführen, gib folgenden Befehl ein:

```bash
./hello
```

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben. Du kannst beide Schritte auch in einem Befehl kombinieren:

```bash
gfortran hello.f90 -o hello && ./hello
```

<div class='hint'>
Die Zeichenkombination <code>&amp;&amp;</code> sorgt dafür, dass der zweite Befehl nur ausgeführt wird, wenn der erste erfolgreich war.
</div>

_task(hallo)

_task(uebersetzt)

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird der Compiler eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `print` das Wort `prin` zu schreiben:

```fortran
~ program HelloWorld
    prin *, "Hello, World!"
~ end program HelloWorld
```

Speichere die Datei und führe den Compiler erneut aus:

```bash
gfortran hello.f90 -o hello
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Der Compiler sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden. Lies sie in deinem Terminal:

_task(fehler)

<!-- after: fehler -->

<!-- tutorial-screenshot
# Der Fehler aus dem Text; die lange Meldung braucht das ganze Terminal
write-file: hello.f90 <- file:hello-error.f90
terminal-maximize
press: Control+l
terminal-run: gfortran hello.f90 -o hello
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='error.webp' alt='gfortran zeigt Zeile 2 von hello.f90 mit prin *, "Hello, World!", markiert die Stelle und meldet Error: Unclassifiable statement at (1).'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 2) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

<!-- /after -->

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen. An diesem Beispiel kannst du sehen, wie man in C Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.f90, fortran)

Speichere die Datei unter dem Namen `factor.f90`. Kompiliere das Programm:

```bash
gfortran factor.f90 -o factor
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: factor.f90 <- file:factor.f90
terminal-maximize
press: Control+l
terminal-run: gfortran factor.f90 -o factor
terminal-wait-for-prompt
terminal-run: ./factor
wait-for-text: number:
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='gfortran übersetzt factor.f90, ./factor fragt nach einer Zahl und gibt für 123 die Primfaktoren 3 und 41 untereinander aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben.

_task(ueberlauf)

<!-- after: ueberlauf -->

Das Programm bricht schon beim Einlesen ab: `Integer overflow while reading item 1`. Eine `integer`-Variable hat in `gfortran` 32 Bit und reicht nur bis 2147483647.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere die Zeile zu <code>integer(kind=8) :: n, i</code>, übersetze neu und gib <code>3000000000</code> noch einmal ein. Mit 64 Bit passt die Zahl hinein, und das Programm gibt ihre Faktoren aus. Warum ist es danach noch eine ganze Weile nicht fertig? Schau dir die Zeile <code>do i = 2, n</code> an. Mit <kbd>Strg</kbd><kbd>C</kbd> brichst du es ab.
</div>

<!-- /after -->

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Fortran Arrays verwendet  und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.f90, fortran)

Speichere die Datei unter dem Namen `bubblesort.f90`. Kompiliere das Programm:

```bash
gfortran bubblesort.f90 -o bubblesort
```
Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: bubblesort.f90 <- file:bubblesort.f90
press: Control+l
terminal-run: gfortran bubblesort.f90 -o bubblesort
terminal-wait-for-prompt
terminal-run: ./bubblesort
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='gfortran übersetzt bubblesort.f90, ./bubblesort gibt zehn Zufallszahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert.

<div class='hint try'>
<strong>Ausprobieren:</strong> Sortiere 100 Zahlen statt 10: Ändere <code>bubblesort.f90</code>, übersetze neu und starte das Programm. Warum musst du dafür nur eine einzige Zahl ändern?
</div>

_task(absteigend)

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Fortran-Programm schreiben, kompilieren und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Fortran wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Programme schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!

