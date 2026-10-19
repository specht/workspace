<div class='meta'>
image: nasm-logo.png
kind: steckbrief
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Netwide Assembler <span style='font-size: 80%;'>(1996)</span>

<div class='floatright' style='width: 24em;'>
    <img src='tatham.webp' alt='Foto von Simon Tatham, einem der Autoren von NASM'>
    <p>Simon Tatham</p>
</div>

<p class='abstract'>
Assembler ist eine maschinennahe Programmiersprache, die direkt auf der Maschinenebene arbeitet und es ermöglicht, Programme zu schreiben, die von der CPU direkt ausgeführt werden. Hier schreibst du ein Programm für x86-64 unter Linux und übersetzt es mit NASM, dem Netwide Assembler.
</p>

Assembler wird häufig für die Entwicklung von Betriebssystemen, Gerätetreibern und eingebetteten Systemen verwendet, da es eine hohe Effizienz und direkte Kontrolle über die Hardware bietet. Bekannte Beispiele für die Verwendung von Assembler sind der Linux-Kernel, der Teile seines Codes in Assembler enthält, und die Firmware von Mikrocontrollern in eingebetteten Systemen. Auch viele frühe Computerspiele wie »Prince of Persia« und »RollerCoaster Tycoon« wurden in Assembler geschrieben. Heutzutage ist Assembler immer noch relevant, insbesondere in Bereichen, die eine hohe Leistung und präzise Hardwaresteuerung erfordern, wie in der Systemprogrammierung und bei der Entwicklung von Echtzeitsystemen.

NASM (Netwide Assembler) ist ein freier Assembler, der auf vielen Plattformen verfügbar ist und die x86-Architektur unterstützt. Er wurde 1996 von Simon Tatham entwickelt und ist eine der beliebtesten Assembler-Tools in der Open-Source-Community.

## Hello, world!

Assembler-Programme werden in Textdateien mit der Endung `.asm` oder `.s` geschrieben. Diese Dateien werden anschließend von einem Assembler in ausführbare Dateien übersetzt, die auf deinem Computer direkt ausgeführt werden können. Es gibt eine Vielzahl von Assemblern, die du verwenden kannst, aber wir werden hier den »Netwide Assembler« (NASM) verwenden, der auf vielen Plattformen verfügbar ist.

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

_include_file(hello.asm, nasm)

### Warum einfach, wenn's auch kompliziert geht?

Wie du sehen kannst, ist der Assembler-Code für ein einfaches »Hello, world!«-Programm im Vergleich zu anderen Programmierspracen relativ lang. Das liegt daran, dass Assembler eine sehr maschinennahe Sprache ist und du viele Details explizit angeben musst.

In den Zeilen 9 bis 13 wird ein Systemaufruf (`syscall`) verwendet, um die Funktion `write` aufzurufen und den Text »Hello, world!« auf der Standardausgabe auszugeben. Die Register `rax`, `rdi`, `rsi` und `rdx` enthalten die Argumente für den Systemaufruf:

- `rax` enthält die Nummer des Systemaufrufs (1 für `write`)
- `rdi` enthält den Dateideskriptor (1 für die Standardausgabe)
- `rsi` enthält die Adresse des Texts
- `rdx` enthält die Länge des Texts

Eine Übersicht über alle Linux-Systemaufrufe für die x86_64-Architektur findest du hier: [Linux x86_64 System Call Reference Table](https://hackeradam.com/x86-64-linux-syscalls/).

<div class='hint'>
Die Frage, warum man in Assembler programmieren sollte, wenn es doch so viel einfacher geht, ist berechtigt. Assembler ist eine sehr mächtige Programmiersprache, die es dir erlaubt, die Hardware deines Computers direkt zu kontrollieren. Das kann in bestimmten Situationen sehr nützlich sein, z. B. wenn du extrem schnelle oder ressourcenschonende Programme schreiben möchtest. Außerdem kann es sehr lehrreich sein, Assembler zu lernen, da du ein tiefes Verständnis dafür bekommst, wie Computer auf der Ebene der CPU funktionieren. Unabhängig davon spielt Assembler auch in der Softwareentwicklung eine wichtige Rolle, da die Quelltexte kompilierter Programmiersprachen (z. B. C und C++) im ersten Schritt in Assembler-Code übersetzt werden.
</div>

### Syntax-Highlighting aktivieren

Da Visual Studio Code noch nicht weiß, dass es sich um Assembler-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
# Das Programm in eine noch namenlose Datei
press: Enter
sleep: 1
hide-left-sidebar
type: <- file:hello.asm
# Nach dem Einfügen steht der Text eine Zeile hochgescrollt
press: Control+Home
sleep: 0.5
# Alle 18 Zeilen
crop-bottom: 14%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit dem Assembler-Programm, 18 Zeilen von section .data bis syscall, ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.asm` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.asm`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.asm
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.asm.'>

Da Assembler standardmäßig nicht von Visual Studio Code unterstützt wird, müssen wir noch eine passende Erweiterung installieren. Klicke dazu auf das Erweiterungs-Symbol <img src='../basics/extensions.svg' style='border-radius: 4px; height: 1.5em;' alt='vier Quadrate, eines davon abgehoben'> in der Seitenleiste oder drücke <kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd>. Suche nach der Erweiterung »The Netwide Assembler« und installiere sie. Weil der Herausgeber nicht verifiziert ist, fragt Visual Studio Code, ob du ihm vertraust; bestätige mit »Trust Publisher & Install«.

<!-- tutorial-screenshot
# Speichern, dann die Erweiterung im Marketplace
press: Enter
wait-for-file: hello.asm
sleep: 1
press: Control+Shift+x
wait-for-text: Marketplace
type: The Netwide Assembler
sleep: 3
crop-bottom: 50%
-->

<img class='full' src='nasm-syntax.webp' alt='Links der Marketplace mit der Suche nach The Netwide Assembler, mit Erweiterungen für Assembler und dem Knopf »Install«; rechts hello.asm, noch einfarbig.'>

Alternativ kannst du auch <kbd>Strg</kbd><kbd>P</kbd> drücken und `ext install rights.nas-vscode` eingeben, um die Erweiterung zu installieren.

Anschließend solltest du dein Assembler-Programm farbig sehen:

<!-- tutorial-screenshot
# Die Erweiterung installieren, wie es der Text als zweiten Weg
# nennt; danach ist der Code farbig
press: Control+p
type: ext install rights.nas-vscode
wait-for-text: Press Enter to install extension
press: Enter
# Der Herausgeber ist nicht verifiziert: VS Code fragt nach
wait-for-text: Trust Publisher & Install
click: Trust Publisher & Install
sleep: 10
# Neu öffnen: erst dann kennt VS Code die Sprache
close-tab: hello.asm
open-file: hello.asm
hide-left-sidebar
sleep: 2
crop-bottom: 14%
-->

<img class='full' src='syntax-highlighting.webp' alt='hello.asm mit farbiger Syntax, von section .data über mov rax, 1 bis zum letzten syscall.'>

### Kompilieren und ausführen

Bevor wir das Programm ausführen können, müssen wir es kompilieren und linken. Dazu verwenden wir den Netwide Assembler `nasm`, um den Assembler-Code in eine Objektdatei zu übersetzen, und den GNU Linker `ld`, um die Objektdatei anschließend in eine ausführbare Datei zu linken.

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='lets-compile.webp' alt='Oben hello.asm, darunter das offene Terminal mit der Eingabeaufforderung.'>

Um das Programm zu kompilieren, gib folgenden Befehl ein:

```bash
nasm -f elf64 hello.asm
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>nasm -f elf64 he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.asm</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Wenn du keinen Fehler gemacht hast, wird das Programm erfolgreich kompiliert und die Objektdatei `hello.o` wird im selben Verzeichnis erstellt. Du kannst dies überprüfen, indem du dir die Dateien im aktuellen Verzeichnis mit `ls` oder `ls -l` anzeigen lässt:

<!-- tutorial-screenshot
press: Control+l
terminal-run: nasm -f elf64 hello.asm
terminal-wait-for-prompt
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls.webp' alt='Im Terminal erzeugt nasm -f elf64 hello.asm die Objektdatei; ls -l zeigt hello.asm und hello.o.'>

Um eine ausführbare Datei zu erstellen, müssen wir die Objektdatei mit dem GNU Linker `ld` linken. Gib dazu folgenden Befehl ein:

```bash
ld hello.o -o hello
```

Nun sollte eine ausführbare Datei mit dem Namen `hello` im Verzeichnis liegen. Du kannst dies überprüfen, indem du dir die Dateien im aktuellen Verzeichnis mit `ls` oder `ls -l` anzeigen lässt:

<!-- tutorial-screenshot
# Beide Befehle mit Ausgabe passen nur ins ganze Terminal
terminal-maximize
press: Control+l
terminal-run: ld hello.o -o hello
terminal-wait-for-prompt
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls2.webp' alt='ld hello.o -o hello linkt die Objektdatei; ls -l zeigt jetzt dazu die ausführbare Datei hello in Grün.'>

Die grüne Datei `hello` ist die ausführbare Datei – im Unterschied zu Windows, wo ausführbare Dateien die Endung `.exe` haben, haben ausführbare Dateien unter Linux keine Endung. Um das Programm auszuführen, gib folgenden Befehl ein:

```bash
./hello
```

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben. Du kannst alle drei Schritte (kompilien, linken, ausführen) auch in einem Befehl kombinieren:

```bash
nasm -f elf64 hello.asm && ld hello.o -o hello && ./hello
```

<div class='hint'>
Die Zeichenkombination <code>&amp;&amp;</code> sorgt dafür, dass der nächste Befehl nur ausgeführt wird, wenn der vorherige Befehl erfolgreich war.
</div>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird der Compiler eine Fehlermeldung ausgeben. Versuche zum Beispiel, in Zeile 9 statt `rax` das Wort `ray` zu schreiben:

```nasm
~     global _start
~
~ _start:
~     ; write the string to stdout
    mov ray, 1
~     mov rdi, 1
~     mov rsi, hello
~     mov rdx, 14
```

Speichere die Datei und führe den Assembler erneut aus:

```bash
nasm -f elf64 hello.asm
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um einen vorherigen Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Der Assembler sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<!-- tutorial-screenshot
# Der Fehler aus dem Text; die lange Meldung braucht das ganze Terminal
write-file: hello.asm <- file:hello-error.asm
terminal-maximize
press: Control+l
terminal-run: nasm -f elf64 hello.asm
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello-error.webp' alt='nasm meldet hello.asm:9: error: symbol »ray« not defined.'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 9) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen. An diesem Beispiel kannst du sehen, wie man in Assembler Benutzereingaben verarbeitet, Schleifen verwendet sowie Strings in Zahlen umwandelt und anders herum.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.asm, nasm)

Speichere die Datei unter dem Namen `factor.asm`. Kompiliere und linke das Programm:

```bash
nasm -f elf64 factor.asm && ld factor.o -o factor
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: factor.asm <- file:factor.asm
terminal-maximize
press: Control+l
terminal-run: nasm -f elf64 factor.asm
terminal-wait-for-prompt
terminal-run: ld factor.o -o factor
terminal-wait-for-prompt
terminal-run: ./factor
wait-for-text: number:
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='nasm und ld bauen factor, ./factor fragt nach einer Zahl und gibt für 123 die Primfaktoren 3 und 41 aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib statt 123 die Zahl <code>3000000000</code> ein, danach die Zahl <code>123456789123456789</code>. Was passiert jeweils, und was könnte der Grund dafür sein? Was steht nach dem Ende des Programms noch im Terminal, und woher kommt es?
</div>

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Assembler Zufallszahlen vom Betriebssystem holt (mit dem Systemaufruf `getrandom`), auf einzelne Elemente eines Arrays zugreift, Zahlen in Ziffern umwandelt und Unterprogramme mit `call` und `ret` aufruft. Ein Durchlauf, in dem nichts vertauscht wurde, beendet das Sortieren schon früher.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.asm, nasm)

Speichere die Datei unter dem Namen `bubblesort.asm`. Übersetze und linke das Programm:

```bash
nasm -f elf64 bubblesort.asm && ld bubblesort.o -o bubblesort
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: bubblesort.asm <- file:bubblesort.asm
press: Control+l
terminal-run: nasm -f elf64 bubblesort.asm && ld bubblesort.o -o bubblesort
terminal-wait-for-prompt
terminal-run: ./bubblesort
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='nasm und ld bauen bubblesort, ./bubblesort gibt zehn Zufallszahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere <code>bubblesort.asm</code>, übersetze und linke neu und starte das Programm:
<ul>
<li>Sortiere 20 Zahlen statt 10. Welche Zeilen musst du dafür ändern? Achte nicht nur auf die 10.</li>
<li>Ersetze <code>jbe no_swap</code> durch <code>jae no_swap</code>. Was ändert sich, und warum?</li>
</ul>
</div>

## Zusammenfassung

Wie du siehst, ist Assembler eine sehr aufwendige Programmiersprache: Schon das Ausgeben einer Zahl braucht ein eigenes Unterprogramm.

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Assembler-Programm schreiben, kompilieren, linken und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Assembler wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Programme schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!

