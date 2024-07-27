<div class='meta'>
image: cpp-logo.png
</div>

<div class='floatright mt-5' style='width: 12em;'>
    <img src='stroustrup.webp'>
    <p>Bjarne Stroustrup</p>
</div>

# C++ <span style='font-size: 80%;'>(1983)</span>

<p class='abstract'>
C++ ist eine Programmiersprache, die im Jahr 1983 von Bjarne Stroustrup entwickelt wurde. Sie ist eine Erweiterung der Programmiersprache C und wird in vielen Bereichen eingesetzt, darunter Spieleentwicklung, Betriebssysteme und Anwendungen, die hohe Leistung erfordern.
In diesem Kapitel lernst du, wie du ein einfaches Programm in C++ schreibst und ausführst.
</p>

<!-- ## Eigenschaften

- **Objektorientierung**: C++ ist eine objektorientierte Programmiersprache, die auf der Verwendung von Objekten und Klassen basiert.
- **Generische Programmierung**: C++ unterstützt generische Programmierung, was es ermöglicht, Algorithmen und Datenstrukturen unabhängig von den verwendeten Datentypen zu schreiben.
- **Hohe Performance**: C++ ist eine der schnellsten Programmiersprachen und wird häufig für rechenintensive Anwendungen eingesetzt.
- **Portabilität**: C++-Code ist in der Regel portabel und kann auf verschiedenen Plattformen und Betriebssystemen ausgeführt werden.
- **Modularität**: C++ unterstützt die modulare Programmierung, was es ermöglicht, den Code in separate Module oder Dateien aufzuteilen.
- **Standardbibliothek**: C++ verfügt über eine umfangreiche Standardbibliothek, die eine Vielzahl von Funktionen und Datentypen bereitstellt. -->

## Hello, world!

C++ ist eine Weiterentwicklung der [Programmiersprache C](/c) und hat viele Ähnlichkeiten mit dieser. Das heisst, dass man auch jedes C-Programm in C++ kompilieren kann. C++-Programme werden in Textdateien mit der Endung `.cpp` oder `.cc` geschrieben. Diese Dateien werden anschließend von einem Compiler in ausführbare Dateien übersetzt, die auf deinem Computer direkt ausgeführt werden können. Es gibt eine Vielzahl von Compilern, die du verwenden kannst, aber wir werden hier den freien »GNU C Compiler« `gcc` verwenden, der auf den meisten Systemen verfügbar ist.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File«.

<img class='full' src='choose-filename.webp'>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.cpp, cpp)

Da Visual Studio Code noch nicht weiß, dass es sich um C++-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<img class='full' src='no-syntax-highlighting.webp'>

Drücke nun <span class='key'>Strg</span><span class='key'>S</span>, um die Datei zu speichern. Gib `hello.cpp` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.cpp`.

<img class='full' src='enter-filename.webp'>

Sobald du die Datei gespeichert hast, wird sie automatisch als C++-Datei erkannt und die Syntax wird hervorgehoben.

<img class='full' src='syntax-highlighting.webp'>

### Kompilieren und ausführen

Bevor wir das Programm ausführen können, müssen wir es kompilieren. Dadurch wird der Quelltext in Maschinencode übersetzt, den dein Computer ausführen kann.

Öffne dazu ein Terminal, indem du entweder <span class='key'>Strg</span><span class='key'>J</span> drückst oder das Panel-Symbol <img src='../basics/panel.webp' style='border-radius: 4px; height: 1.5em;'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<img class='full' src='cpp-lets-compile.webp'>

Um das Programm zu kompilieren, gib folgenden Befehl ein:

```bash
g++ hello.cpp -o hello
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>g++ he</code> und drücke <span class='key'>Tab</span>, um den Dateinamen automatisch zu <code>hello.cpp</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Wenn du keinen Fehler gemacht hast, wird das Programm erfolgreich kompiliert und die ausführbare Datei `hello` wird im selben Verzeichnis erstellt. Du kannst dies überprüfen, indem du dir die Dateien im aktuellen Verzeichnis mit `ls` oder `ls -l` anzeigen lässt:

<img class='full' src='ls.webp'>

Die grüne Datei `hello` ist die ausführbare Datei – im Unterschied zu Windows, wo ausführbare Dateien die Endung `.exe` haben, haben ausführbare Dateien unter Linux keine Endung. Um das Programm auszuführen, gib folgenden Befehl ein:

```bash
./hello
```

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben. Du kannst beide Schritte auch in einem Befehl kombinieren:

```bash
g++ hello.cpp -o hello && ./hello
```

<div class='hint'>
Die Zeichenkombination <code>&amp;&amp;</code> sorgt dafür, dass der zweite Befehl nur ausgeführt wird, wenn der erste erfolgreich war.
</div>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird der Compiler eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `cout` das Wort `coud` zu schreiben:

```cpp
std::coud << "Hello, World!\n";
```

Speichere die Datei und führe den Compiler erneut aus:

```bash
g++ hello.cpp -o hello
```

<div class='hint'>
Nutze die Pfeiltaste hoch <span class='key'>↑</span>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Der Compiler sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<img class='full' src='error.webp'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 4) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

## Primfaktorenzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen. An diesem Beispiel kannst du sehen, wie man in C++ Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <span class='key'>Strg</span><span class='key'>Shift</span><span class='key'>N</span> und schreibe den folgenden Code hinein:

_include_file(factor.cpp, cpp)

Speichere die Datei unter dem Namen `factor.cpp`. Kompiliere das Programm:

```bash
g++ factor.cpp -o factor
```

Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<img class='full' src='try-factor.webp'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Probiere aus, was passiert, wenn du die Zahl 3000000000 eingibst. Was könnte der Grund dafür sein?

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in C++ Listen verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.cpp, cpp)

Speichere die Datei unter dem Namen `bubblesort.cpp`. Kompiliere das Programm:

```bash
g++ bubblesort.cpp -o bubblesort
```
Falls du keine Fehlermeldung erhältst, kannst du das Programm ausführen und testen:

<img class='full' src='bubblesort.webp'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert. Versuche, den Quelltext so zu verändern, dass statt 10 Zahlen 100 oder mehr Zahlen sortiert werden.

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches C++-Programm schreiben, kompilieren und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um C++ wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Programme schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!

