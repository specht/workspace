<div class='meta'>
image: lua-logo.png
</div>

# Lua <span style='font-size: 80%;'>(1993)</span>

<div class='floatright' style='width: 30em;'>
    <img src='lua-team.webp'>
    <p>
        Waldemar Celes, Roberto Ierusalimschy, Luiz Henrique de Figueiredo
        <span style='font-size: 90%; opacity: 0.7;'>(Foto von Ernani d'Almeida, 2011)</span>
    </p>
</div>

<p class='abstract'>
Lua, entwickelt in den frühen 1990er Jahren von Waldemar Celes, Roberto Ierusalimschy und Luiz Henrique de Figueiredo in Brasilien, ist eine leichte, eingebettete Skriptsprache, die für ihre Einfachheit und Effizienz bekannt ist.
</p>

Lua wird häufig in der Spieleentwicklung verwendet, um Spielmechaniken und Skripting zu implementieren, mit bekannten Beispielen wie World of Warcraft und Angry Birds. Darüber hinaus findet Lua Anwendung in eingebetteten Systemen und als Skriptsprache in Software wie Adobe Lightroom. Heutzutage ist Lua immer noch relevant, insbesondere in der Spieleindustrie und bei eingebetteten Anwendungen, da sie eine schnelle Ausführung und einfache Integration in andere Software bietet.

## Hello, world!

Lua ist eine Skriptsprache, was bedeutet, dass der Code zur Laufzeit interpretiert wird. Du hast zwei Möglichkeiten, Lua-Code auszuführen:

1. Du kannst Lua-Code direkt in der Lua-Shell ausführen.
2. Du kannst Lua-Code in einer Textdatei speichern und dann ausführen.

**Möglichkeit 1:** Lua-Code in der Lua-Shell ausführen

Öffne dazu ein Terminal, indem du entweder <span class='key'>Strg</span><span class='key'>J</span> drückst oder das Panel-Symbol <img src='../basics/panel.webp' style='border-radius: 4px; height: 1.5em;'> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<img class='full' src='code-with-terminal.webp'>

Starte nun die Lua-Shell, indem du `lua` eingibst und dann <span class='key'>Enter</span> drückst. Du solltest eine Ausgabe wie diese sehen:

<img class='full' src='lua-shell.webp'>

Jetzt kannst du Lua-Code direkt in der Shell eingeben und ausführen. Schreibe einfach `print("Hello, world!")` und drücke <span class='key'>Enter</span>. Du solltest die Ausgabe `Hello, world!` sehen.

Du kannst die Lua-Shell wieder beenden, indem du <span class='key'>Strg</span><span class='key'>D</span> drückst.

**Möglichkeit 2:** Lua-Code in einer Textdatei speichern und ausführen

Lua-Programme werden in Textdateien mit der Endung `.lua` geschrieben. Der Lua-Interpreter liest anschließend den Quelltext und führt ihn aus.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File« (oder bestätige einfach mit <span class='key'>Enter</span>).

<img class='full' src='choose-filename.webp'>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.lua, lua)

Da Visual Studio Code noch nicht weiß, dass es sich um Lua-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<img class='full' src='no-syntax-highlighting.webp'>

Drücke nun <span class='key'>Strg</span><span class='key'>S</span>, um die Datei zu speichern. Gib `hello.lua` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.lua`.

<img class='full' src='enter-filename.webp'>

Sobald du die Datei gespeichert hast, wird sie automatisch als Lua-Datei erkannt und die Syntax wird hervorgehoben.

<img class='full' src='syntax-highlighting.webp'>

### Skript ausführen

Um unser Programm auszuführen, müssen wir den Lua-Interpreter aufrufen und ihm den Dateinamen unseres Programms übergeben.

Öffne dazu ein Terminal, indem du <span class='key'>Strg</span><span class='key'>J</span> drückst und gib folgenden Befehl ein:

```bash
lua hello.lua
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>lua he</code> und drücke <span class='key'>Tab</span>, um den Dateinamen automatisch zu <code>hello.lua</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben:

<img class='full' src='hello.webp'>

### Fehler finden und beheben

Wenn du einen Fehler im Code machst, wird Ruby eine Fehlermeldung ausgeben. Versuche zum Beispiel, statt `print` das Wort `prin` zu schreiben:

```lua
prin("Hello, World!")
```

Speichere die Datei und führe das Skript erneut aus:

```bash
lua hello.lua
```

<div class='hint'>
Nutze die Pfeiltaste hoch <span class='key'>↑</span>, um den letzten Befehl erneut einzugeben. So kannst du schnell dein Programm testen, nachdem du es verändert hast.
</div>

Lua sollte eine Fehlermeldung ausgeben, die dir hilft, den Fehler zu finden:

<img class='full' src='hello-error.webp'>

Es lohnt sich, die Fehlermeldungen genau zu lesen, um den Fehler zu finden und zu beheben. Achte auf die Zeilennummer (in diesem Beispiel 1) und den Text, der dir sagt, was falsch ist. Denke daran, den Fehler wieder zu beheben, bevor du das nächste Beispiel ausprobierst.

### Shebang `#!`

Bisher musst du, um dein Skript auszuführen, immer den Lua-Interpreter explizit aufrufen. Wenn du dein Skript wie ein normales Programm ausführen möchtest, kannst du ein sogenanntes Shebang am Anfang deiner Datei hinzufügen. Das Shebang besteht aus einer Raute `#` gefolgt von einem Ausrufezeichen `!` und dem Pfad zum Lua-Interpreter. In unserem Fall sieht das so aus:

```lua
#!/usr/bin/env lua
```

Füge diese Zeile ganz oben in deinem Skript ein und speichere die Datei. Bevor wir das Skript ausführen können, müssen wir es noch ausführbar machen. Das machen wir mit dem Befehl `chmod`:

```bash
chmod +x hello.lua
```
Jetzt können wir das Skript direkt ausführen:

```bash
./hello.lua
```

<img class='full' src='shebang.webp'>

## Primfaktorzerlegung

Im zweiten Beispiel wollen wir eine Zahl in ihre Primfaktoren zerlegen.
An diesem Beispiel kannst du sehen, wie man in Lua Benutzereingaben verarbeitet und Schleifen verwendet.
Erstelle eine neue Datei mit <span class='key'>Strg</span><span class='key'>Alt</span><span class='key'>N</span> und schreibe den folgenden Code hinein:

_include_file(factor.lua, lua)

Speichere die Datei unter dem Namen `factor.lua` und führe sie aus:

<img class='full' src='try-factor.webp'>

Das Programm hat die Zahl 123 in ihre Primfaktoren zerlegt und ausgegeben. Wenn du allerdings eine sehr große Zahl wie 3000000000 verwendest, dauert die Berechnung sehr lange (probier es gern aus, du kannst das Programm mit <span class='key'>Strg</span><span class='key'>C</span> abbrechen).

## Bubblesort

Im dritten Beispiel wollen wir eine Liste von 10 Zufallszahlen sortieren. Dafür verwenden wir den [Bubblesort-Algorithmus](https://de.wikipedia.org/wiki/Bubblesort), der zwar nicht besonders effizient ist, aber sehr einfach zu verstehen und zu implementieren. Der Bubblesort-Algorithmus funktioniert, indem er die Liste mehrmals durchläuft und benachbarte Elemente vertauscht, wenn sie in der falschen Reihenfolge sind.

An diesem Beispiel kannst du sehen, wie man in Lua Listen verwendet, Funktionen verwendet und Schleifen verschachtelt.

Erstelle eine neue Datei und schreibe den folgenden Code hinein:

_include_file(bubblesort.lua, lua)

Speichere das Skript unter dem Namen `bubblesort.lua` und führe es aus:

<img class='full' src='bubblesort.webp'>

Das Programm hat eine Liste von 10 Zufallszahlen sortiert. Versuche, den Quelltext so zu verändern, dass statt 10 Zahlen 100 oder mehr Zahlen sortiert werden.

## Zusammenfassung

In diesem Kapitel hast du an drei Beispielen gesehen, wie man ein einfaches Lua-Skript schreiben und ausführen kann. Das ist natürlich nur ein erster Eindruck. Um Lua wirklich zu beherrschen, musst du noch viel mehr lernen – am besten, indem du eigene Skripte schreibst und ausprobierst. Die Buchhandlungen, Bibliotheken und Youtube sind voll von Material für dich. Viel Spaß beim Programmieren!