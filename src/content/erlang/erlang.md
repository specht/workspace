<div class='meta'>
image: erlang-logo.png
</div>

# Erlang <span style='font-size: 80%;'>(1986)</span>

<div class='floatright' style='width: 30em;'>
    <img src='erlang-team.webp' alt=''>
    <p>Joe Armstrong, Robert Verding und Mike Williams</p>
</div>

<p class='abstract'>
Erlang ist eine funktionale Programmiersprache, die 1986 von Joe Armstrong, Robert Verding und Mike Williams bei Ericsson entwickelt wurde. Sie wurde speziell für die Entwicklung von verteilten, fehlertoleranten und echtzeitfähigen Systemen konzipiert. Erlang ist bekannt für seine leichte Parallelisierung und seine Fähigkeit, mit Fehlern umzugehen, ohne das gesamte System zum Absturz zu bringen.
</p>

<!-- ## Eigenschaften

- **Funktionale Programmiersprache**: Erlang ist eine funktionale Programmiersprache, die auf der rekursiven Funktionstheorie basiert. -->

## Hello, world!

Erlang ist eine Skriptsprache, was bedeutet, dass der Code zur Laufzeit interpretiert wird. Du hast zwei Möglichkeiten, Erlang-Code auszuführen:

1. Du kannst Erlang-Code direkt in der Erlang-Shell ausführen.
2. Du kannst Erlang-Code in einer Textdatei speichern und dann ausführen.

**Möglichkeit 1:** Erlang-Code in der Erlang-Shell ausführen

Öffne dazu ein Terminal, indem du entweder <kbd>Strg</kbd><kbd>J</kbd> drückst oder das Panel-Symbol <img src='../basics/panel.webp' style='border-radius: 4px; height: 1.5em;' alt=''> rechts oben drückst. Dein Fenster sollte jetzt ungefähr so aussehen:

<img class='full' src='code-with-terminal.webp' alt=''>

Starte nun die Erlang-Shell, indem du `erl` eingibst und dann <kbd>Enter</kbd> drückst. Du solltest eine Ausgabe wie diese sehen:

<img class='full' src='erl.webp' alt=''>

Jetzt kannst du Erlang-Code direkt in der Shell eingeben und ausführen. Schreibe einfach `io:format("Hello world!~n").` und drücke <kbd>Enter</kbd>. Du solltest die Ausgabe `Hello, world!` sehen.

Du kannst die Erlang-Shell wieder beenden, indem du zweimal hintereinander <kbd>Strg</kbd><kbd>C</kbd> drückst.

**Möglichkeit 2:** Erlang-Code in einer Textdatei speichern und ausführen

Erlang-Programme werden in Textdateien mit der Endung `.erl` geschrieben. Ein Erlang-Interpreter liest anschließend den Quelltext und führt ihn aus.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp' alt=''>

### Quelltext schreiben

Klicke auf »New File« und wähle als Dateityp »Text File« (oder bestätige einfach mit <kbd>Enter</kbd>).

<img class='full' src='choose-filename.webp' alt=''>

Schreibe nun den folgenden Code in die Datei:

_include_file(hello.erl, erlang)

Da Visual Studio Code noch nicht weiß, dass es sich um Erlkang-Quelltext handelt, ist dein Programm momentan noch einfarbig, aber das wird sich gleich ändern. An dem weißen Punkt erkennst du, dass deine Änderungen noch nicht gespeichert sind.

<img class='full' src='no-syntax-highlighting.webp' alt=''>

Drücke nun <kbd>Strg</kbd><kbd>S</kbd>, um die Datei zu speichern. Gib `hello.erl` ein – der vollständige Pfad zu deiner Datei lautet dann `/workspace/hello.erl`.

<img class='full' src='enter-filename.webp' alt=''>

<div class='hint'>
Achte darauf, dass du deine Datei nicht aus Versehen <code>hello.erl.</code> (mit einem Punkt am Ende) nennst, da Visual Studio Code als Dateiname <code>io:format("Hello world!~n").</code> vorschlägt.
</div>

Da Smalltalk standardmäßig nicht von Visual Studio Code unterstützt wird, müssen wir noch eine passende Erweiterung installieren. Klicke dazu auf das Erweiterungs-Symbol <img src='../basics/extensions.webp' style='border-radius: 4px; height: 1.5em;' alt=''> in der Seitenleiste oder drücke <kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd>. Suche nach der Erweiterung »Erlang« und installiere sie.

<img class='full' src='erlang-syntax.webp' alt=''>

Alternativ kannst du auch <kbd>Strg</kbd><kbd>P</kbd> drücken und `ext install pgourlain.erlang` eingeben, um die Erweiterung zu installieren.

Anschließend solltest du dein Erlang-Programm farbig sehen:

<img class='full' src='syntax-highlighting.webp' alt=''>

### Skript ausführen

Um unser Programm auszuführen, müssen wir den Erlang-Interpreter aufrufen (in unserem Fall `erl`) und ihm den Dateinamen unseres Programms übergeben.

Öffne dazu ein Terminal, indem du <kbd>Strg</kbd><kbd>J</kbd> drückst und gib folgenden Befehl ein:

```bash
erl -noshell -s hello start -s init stop
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>gst he</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>hello.st</code> vervollständigen zu lassen. Du kannst danach ganz normal weiterschreiben.
</div>

Das Programm sollte die Nachricht `Hello, World!` im Terminal ausgeben:

<img class='full' src='hello.webp'>