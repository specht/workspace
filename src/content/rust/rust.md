<div class='meta'>
image: rust-logo.png
kind: steckbrief
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Rust <span style='font-size: 80%;'>(2015)</span>

<div class='floatright' style='width: 16em;'>
    <img src='hoare.webp' alt='Foto von Graydon Hoare, der Rust erfunden hat'>
    <p>
        Graydon Hoare
    </p>
</div>

<p class='abstract'>
Rust, ab 2006 von Graydon Hoare entwickelt und 2015 als Version 1.0 veröffentlicht, ist eine kompilierte Programmiersprache, die für die Entwicklung von Systemsoftware und Webanwendungen konzipiert wurde. Rust legt besonderen Wert auf Sicherheit, Geschwindigkeit und Parallelität, was sie ideal für die Entwicklung von Betriebssystemen, Browser-Engines und anderen leistungsintensiven Anwendungen macht.
</p>

Bekannte Beispiele für die Verwendung von Rust sind der Browser-Engine-Teil von Mozilla Firefox, genannt Servo, und das Betriebssystem Redox. Heutzutage ist Rust äußerst relevant, da sie von einer aktiven Community unterstützt wird und eine wachsende Anzahl von Bibliotheken und Frameworks bietet, die die Entwicklung sicherer und effizienter Software ermöglichen.

## Hello, world!

Rust-Programme werden in Textdateien mit der Endung `.rs` geschrieben. Diese Dateien werden anschließend von einem Compiler in ausführbare Dateien übersetzt, die auf deinem Computer direkt ausgeführt werden können.

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

_include_file(hello.rs, rust)

Da Visual Studio Code noch nicht weiß, dass es sich um Rust-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
# Das Programm in eine noch namenlose Datei
press: Enter
sleep: 1
hide-left-sidebar
type: <- file:hello.rs
sleep: 0.5
crop-bottom: 60%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit fn main() und println!("Hello, world!"), ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.rs` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.rs`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.rs
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.rs.'>

Sobald du die Datei gespeichert hast, wird sie automatisch als Rust-Datei erkannt und die Syntax wird hervorgehoben.

<!-- tutorial-screenshot
press: Enter
wait-for-file: hello.rs
sleep: 1.5
crop-bottom: 56%
-->

<img class='full' src='syntax-highlighting.webp' alt='Die gespeicherte Datei hello.rs, jetzt mit farbiger Syntax.'>

### Kompilieren und ausführen

Bevor wir das Programm ausführen können, müssen wir es kompilieren. Dadurch wird der Quelltext in Maschinencode übersetzt, den dein Computer ausführen kann.

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='lets-compile.webp' alt='Oben hello.rs, darunter das offene Terminal mit der Eingabeaufforderung.'>

Vorher müssen wir noch Rust installieren. Gib folgenden Befehl im Terminal ein:

```bash_copy
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Du wirst anschließend gefragt, ob du die Standard-Installation durchführen willst &ndash; du kannst diese Frage bestätigen, indem du Enter drückst. Wenn die Installation vorbei ist, musst du dich abmelden. Drücke dafür <kbd>Strg</kbd><kbd>D</kbd> und öffne das Terminal erneut. Wenn du anschließend `rustc --version` eingibst, solltest du sehen, dass Rust bereit ist und sich mit einer Versionsnummer meldet.

Um das Programm zu kompilieren, gib folgenden Befehl ein:

```bash
rustc hello.rs
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>rustc he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.rs</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Wenn du keinen Fehler gemacht hast, wird das Programm erfolgreich kompiliert und die ausführbare Datei `hello` wird im selben Verzeichnis erstellt. Du kannst dies überprüfen, indem du dir die Dateien im aktuellen Verzeichnis mit `ls` oder `ls -l` anzeigen lässt:

<!-- tutorial-screenshot
# Rust installieren, wie es der Text beschreibt: rustup, Enter für die
# Standard-Installation, abmelden und das Terminal neu öffnen
terminal-run: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
wait-for-text: Proceed with standard installation
press: Enter
terminal-wait-for-prompt: 300
press: Control+d
sleep: 1
terminal-open
press: Control+l
terminal-run: rustc hello.rs
terminal-wait-for-prompt
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls.webp' alt='Im Terminal übersetzt rustc hello.rs das Programm, danach zeigt ls -l die ausführbare Datei hello in Grün und hello.rs.'>

Die grüne Datei `hello` ist die ausführbare Datei – im Unterschied zu Windows, wo ausführbare Dateien die Endung `.exe` haben, haben ausführbare Dateien unter Linux keine Endung. Um das Programm auszuführen, gib folgenden Befehl ein:

```bash
./hello
```

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben. Du kannst beide Schritte auch in einem Befehl kombinieren:

```bash
rustc hello.rs && ./hello
```

<div class='hint'>
Die Zeichenkombination <code>&amp;&amp;</code> sorgt dafür, dass der zweite Befehl nur ausgeführt wird, wenn der erste erfolgreich war.
</div>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird der Compiler eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `println` das Wort `printn` zu schreiben:

```rust
~ fn main() {
    printn!("Hello, world!");
~ }
```

Speichere die Datei und führe den Compiler erneut aus:

```bash
rustc hello.rs
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Der Compiler sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<!-- tutorial-screenshot
# Der Fehler aus dem Text; die lange Meldung braucht das ganze Terminal
write-file: hello.rs <- file:hello-error.rs
terminal-maximize
press: Control+l
terminal-run: rustc hello.rs
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello-error.webp' alt='rustc meldet error: cannot find macro »printn« in this scope, zeigt hello.rs Zeile 2 und schlägt mit help print! statt printn! vor.'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 2) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen. An diesem Beispiel kannst du sehen, wie man in Rust Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.rs, rust)

Speichere die Datei unter dem Namen `factor.rs`. Kompiliere das Programm:

```bash
rustc factor.rs
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: factor.rs <- file:factor.rs
terminal-maximize
press: Control+l
terminal-run: rustc factor.rs
terminal-wait-for-prompt
terminal-run: ./factor
wait-for-text: number:
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='rustc übersetzt factor.rs, ./factor fragt nach einer Zahl und gibt für 123 die Primfaktoren 3 und 41 aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Anders als andere Programmiersprachen kann Rust auch die Zahl 3000000000 in Sekundenbruchteilen zerlegen. Auch mit einer sehr großen Zahl wie 123456789123456789 kommt das Programm problemlos zurecht. Falls du auf eine Zahl stoßen solltest, die so groß ist, dass das Programm sehr lange braucht, kannst du das Programm mit <kbd>Strg</kbd><kbd>C</kbd> abbrechen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib <code>-12</code> ein. Was meldet das Programm, und warum? Tipp: <code>u64</code> in <code>let number: u64</code> steht für eine ganze Zahl ohne Vorzeichen (»unsigned«) mit 64 Bit. Was passiert, wenn du <code>u64</code> durch <code>i64</code> ersetzt, neu übersetzt und noch einmal <code>-12</code> eingibst?
</div>

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Rust Arrays verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.rs, rust)

Speichere die Datei unter dem Namen `bubblesort.rs`. Kompiliere das Programm:

```bash
rustc bubblesort.rs
```
Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<!-- tutorial-screenshot
write-file: bubblesort.rs <- file:bubblesort.rs
press: Control+l
terminal-run: rustc bubblesort.rs
terminal-wait-for-prompt
terminal-run: ./bubblesort
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='rustc übersetzt bubblesort.rs, ./bubblesort gibt zehn Zufallszahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere <code>bubblesort.rs</code>, übersetze neu und starte das Programm:
<ul>
<li>Sortiere 100 Zahlen statt 10: Welche Zahlen musst du dafür ändern?</li>
<li>Sortiere absteigend, die größte Zahl zuerst: Welches Zeichen musst du dafür ändern?</li>
<li>Lösch das <code>mut</code> in <code>let mut arr</code>. Was meldet der Compiler, und was schlägt er vor? Stell das <code>mut</code> danach wieder her.</li>
</ul>
</div>

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Rust-Programm schreiben, kompilieren und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Rust wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Programme schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!

