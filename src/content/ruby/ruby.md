<div class='meta'>
image: ruby-logo.png
kind: steckbrief
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Ruby <span style='font-size: 80%;'>(1995)</span>

<div class='floatright' style='width: 16em;'>
    <img src='matz.webp' alt='Foto von Yukihiro Matsumoto, dem Erfinder von Ruby'>
    <p>
        Yukihiro Matsumoto<br>
        <span style='font-size: 90%; opacity: 0.7;'>(Foto von Marcin Bajer)</span>
    </p>
</div>

<p class='abstract'>
Ruby, entwickelt Mitte der 1990er Jahre von Yukihiro "Matz" Matsumoto, ist eine dynamische, objektorientierte Programmiersprache, die für ihre Einfachheit und Produktivität bekannt ist.
</p>

Ruby wird häufig für die Webentwicklung verwendet, insbesondere durch das populäre Web-Framework Ruby on Rails, das die Entwicklung von Webanwendungen erheblich beschleunigt. Bekannte Beispiele für Anwendungen, die mit Ruby on Rails entwickelt wurden, sind GitHub, Shopify und Basecamp. Heutzutage ist Ruby immer noch relevant, insbesondere in der Webentwicklung, da es eine aktive Community und eine Vielzahl von Bibliotheken und Tools bietet, die die Entwicklung effizienter und angenehmer machen.

<!-- ## Eigenschaften

- **Dynamische Typisierung**: Ruby ist eine dynamisch typisierte Sprache, was bedeutet, dass Variablen ihren Datentyp zur Laufzeit ändern können.
- **Objektorientierung**: Ruby ist eine objektorientierte Programmiersprache, die auf der Verwendung von Objekten und Klassen basiert.
- **Hohe Lesbarkeit**: Ruby legt Wert auf eine klare und verständliche Syntax, die es ermöglicht, den Code leicht zu lesen und zu verstehen.
- **Webentwicklung**: Ruby wird häufig für die Entwicklung von Webanwendungen eingesetzt und bietet eine Vielzahl von Frameworks und Bibliotheken für die Webentwicklung.
- **Community**: Ruby hat eine aktive und engagierte Community, die eine Vielzahl von Bibliotheken und Frameworks entwickelt hat.
- **Rails**: Ruby on Rails ist ein beliebtes Webframework, das auf Ruby basiert und die Entwicklung von Webanwendungen erleichtert. -->

## Hello, world!

Ruby ist eine Skriptsprache, was bedeutet, dass der Code zur Laufzeit interpretiert wird. Du hast zwei Möglichkeiten, Ruby-Code auszuführen:

1. Du kannst Ruby-Code direkt in der Ruby-Shell `irb` ausführen.
2. Du kannst Ruby-Code in einer Textdatei speichern und dann ausführen.

**Möglichkeit 1:** Ruby-Code in der Ruby-Shell ausführen

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.svg' style='border-radius: 4px; height: 1.5em;' alt='ein Rechteck mit einem Streifen unten'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Ein frischer Workspace ohne Ordner, das Terminal offen
hide-left-sidebar
terminal-open
sleep: 0.5
-->

<img class='full' src='code-with-terminal.webp' data-scroll='33%' alt='Ein frischer Workspace mit der Willkommensseite, darunter das offene Terminal mit der Eingabeaufforderung student@workspace:~$.'>

Starte nun die Ruby-Shell, indem du `irb` eingibst und dann <kbd>Enter</kbd> drückst. Du solltest eine Ausgabe wie diese sehen:

<!-- tutorial-screenshot
press: Control+l
terminal-run: irb
wait-for-text: irb(main)
sleep: 0.5
crop-terminal-lines: auto
-->

<img class='full' src='irb.webp' data-scroll='5%' alt='Im Terminal läuft irb und wartet mit irb(main):001&gt; auf Ruby-Code.'>

Jetzt kannst du Ruby-Code direkt in der Shell eingeben und ausführen. Schreibe einfach `puts "Hello, world!"` und drücke <kbd>Enter</kbd>. Du solltest die Ausgabe `Hello, world!` sehen.

Du kannst die Ruby-Shell wieder beenden, indem du `exit` eingibst und <kbd>Enter</kbd> drückst oder einfach <kbd>Strg</kbd><kbd>D</kbd> drückst.

**Möglichkeit 2:** Ruby-Code in einer Textdatei speichern und ausführen

Ruby-Programme werden in Textdateien mit der Endung `.rb` geschrieben. Ein Ruby-Interpreter liest anschließend den Quelltext und führt ihn aus.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- tutorial-screenshot
# Die Shell wieder verlassen, das Terminal schließen
terminal-run: exit
terminal-wait-for-prompt
press: Control+l
press: Control+j
show-left-sidebar
sleep: 0.5
-->

<img class='full' src='fresh-start.webp' data-scroll='28%' alt='Der Workspace ohne geöffneten Ordner, links »No Folder Opened«, rechts die Willkommensseite mit »New File…«.'>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File« (oder bestätige einfach mit <kbd>Enter</kbd>).

<!-- tutorial-screenshot
click: New File...
wait-for-text: Text File
sleep: 0.5
crop-bottom: 77%
-->

<img class='full' src='choose-filename.webp' data-scroll='20%' alt='Nach »New File…« fragt oben ein Feld nach dem Dateityp, »Text File« ist ausgewählt.'>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.rb, ruby)

Da Visual Studio Code noch nicht weiß, dass es sich um Ruby-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<!-- tutorial-screenshot
# Das Programm in eine noch namenlose Datei
press: Enter
sleep: 1
hide-left-sidebar
type: <- file:hello.rb
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='no-syntax-highlighting.webp' alt='Die neue, noch namenlose Datei Untitled-1 mit puts "Hello, World!", ganz in einer Farbe; ein weißer Punkt am Reiter zeigt, dass sie nicht gespeichert ist.'>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.rb` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.rb`.

<!-- tutorial-screenshot
press: Control+s
wait-for-text: Save As
press: Control+a
type: /workspace/hello.rb
sleep: 0.5
crop-bottom: 82%
-->

<img class='full' src='enter-filename.webp' alt='Der Dialog »Save As« mit dem Pfad /workspace/hello.rb.'>

Sobald du die Datei gespeichert hast, wird sie automatisch als Ruby-Datei erkannt und die Syntax wird hervorgehoben.

<!-- tutorial-screenshot
press: Enter
wait-for-file: hello.rb
sleep: 1.5
crop-bottom: 78%
-->

<img class='full' src='syntax-highlighting.webp' alt='Die gespeicherte Datei hello.rb, jetzt mit farbiger Syntax.'>

### Skript ausführen

Um unser Programm auszuführen, müssen wir den Ruby-Interpreter aufrufen und ihm den Dateinamen unseres Programms übergeben.

Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst und gib folgenden Befehl ein:

```bash
ruby hello.rb
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>ruby he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.rb</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben:

<!-- tutorial-screenshot
hide-left-sidebar
terminal-open
press: Control+l
terminal-run: ruby hello.rb
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello.webp' alt='Im Terminal gibt ruby hello.rb die Zeile Hello, World! aus.'>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird Ruby eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `puts` das Wort `pust` zu schreiben:

```ruby
pust "Hello, World!"
```

Speichere die Datei und führe das Skript erneut aus:

```bash
ruby hello.rb
```

<div class='hint'>
Nutze die Pfeiltaste hoch <kbd>↑</kbd>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Ruby sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<!-- tutorial-screenshot
# Der Fehler aus dem Text; die lange Meldung braucht das ganze Terminal
write-file: hello.rb <- file:hello-error.rb
terminal-maximize
press: Control+l
terminal-run: ruby hello.rb
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='hello-error.webp' alt='Ruby meldet zu pust "Hello, World!": hello.rb:1, undefined method »pust« (NoMethodError), markiert das Wort und fragt »Did you mean? puts«.'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 1) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

### Shebang `#!`

Bisher musst du, um dein Skript auszuführen, immer den Ruby-Interpreter explizit aufrufen. Wenn du dein Skript wie ein normales Programm ausführen möchtest, kannst du ein sogenanntes Shebang am Anfang deiner Datei hinzufügen. Das Shebang besteht aus einer Raute `#` gefolgt von einem Ausrufezeichen `!` und dem Pfad zum Ruby-Interpreter. In unserem Fall sieht das so aus:

```ruby
#!/usr/bin/env ruby
~ puts "Hello, World!"
```

Füge diese Zeile ganz oben in deinem Skript ein und speichere die Datei. Bevor wir das Skript ausführen können, müssen wir es noch ausführbar machen. Das machen wir mit dem Befehl `chmod`:

```bash
chmod +x hello.rb
```
Jetzt können wir das Skript direkt ausführen:

```bash
./hello.rb
```

<!-- tutorial-screenshot
# Das Shebang über dem Programm; Editor und Terminal zusammen
write-file: hello.rb <- file:hello-shebang.rb
click: selector:.part.panel [aria-label*="Restore Panel"]
open-file: hello.rb
terminal-open
press: Control+l
terminal-run: chmod +x hello.rb
terminal-wait-for-prompt
terminal-run: ./hello.rb
terminal-wait-for-prompt
move-mouse: selector:.part.statusbar
sleep: 0.5
-->

<img class='full' src='shebang.webp' alt='Oben hello.rb mit #!/usr/bin/env ruby in der ersten Zeile, unten im Terminal chmod +x hello.rb und ./hello.rb, das Hello, World! ausgibt.'>

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen.
An diesem Beispiel kannst du sehen, wie man in Ruby Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <kbd>Strg</kbd><kbd>Alt</kbd><kbd>N</kbd> und schreibe den folgenden Code hinein:

_include_file(factor.rb, ruby)

Speichere die Datei unter dem Namen `factor.rb` und führe sie aus:

<!-- tutorial-screenshot
write-file: factor.rb <- file:factor.rb
terminal-maximize
press: Control+l
terminal-run: ruby factor.rb
wait-for-text: number:
terminal-run: 123
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='try-factor.webp' alt='ruby factor.rb fragt nach einer Zahl und gibt für 123 die Primfaktoren 3 und 41 aus.'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Anders als andere Programmiersprachen kann Ruby auch die Zahl 3000000000 in Sekundenbruchteilen zerlegen. Auch mit einer sehr großen Zahl wie 123456789123456789 kommt das Programm problemlos zurecht. Falls du auf eine Zahl stoßen solltest, die so groß ist, dass das Programm sehr lange braucht, kannst du das Programm mit <kbd>Strg</kbd><kbd>C</kbd> abbrechen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib statt einer Zahl <code>abc</code> ein. Was antwortet das Programm? Starte <code>irb</code> und probiere <code>"abc".to_i</code>, <code>"12abc".to_i</code> und <code>Integer("abc")</code>. Welche Variante würdest du nehmen, wenn das Programm falsche Eingaben bemerken soll? Mit <code>exit</code> beendest du <code>irb</code>.
</div>

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Ruby Listen verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.rb, ruby)

Speichere das Skript unter dem Namen `bubblesort.rb` und führe es aus:

<!-- tutorial-screenshot
write-file: bubblesort.rb <- file:bubblesort.rb
press: Control+l
terminal-run: ruby bubblesort.rb
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='bubblesort.webp' alt='ruby bubblesort.rb gibt zehn Zufallszahlen aus und darunter dieselben Zahlen sortiert.'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert.

<div class='hint try'>
<strong>Ausprobieren:</strong> Ändere <code>bubblesort.rb</code> und starte das Skript noch einmal:
<ul>
<li>Sortiere 100 Zahlen statt 10: Welche Zahl musst du dafür ändern?</li>
<li>Sortiere absteigend, die größte Zahl zuerst: Welches Zeichen musst du dafür ändern?</li>
<li>Ersetze die Zeile mit <code>Array.new</code> durch <code>numbers = ["Kiwi", "Apfel", "Birne", "banane"]</code>. Wo landet »banane«, und warum? Und was meldet Ruby bei <code>numbers = [3, "Apfel", 1]</code>?</li>
</ul>
</div>

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Ruby-Skript schreiben und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Ruby wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Skripte schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!
