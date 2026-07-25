# Hackschule Keyboard Tutorial

Eine interaktive VS-Code-Erweiterung, mit der Schülerinnen und Schüler grundlegende Tastatur-, Navigations- und Textbearbeitungsfertigkeiten praktisch üben.

Das Tutorial umfasst acht Kapitel:

1. Die Tastatur
2. Navigieren im Dokument
3. Text markieren
4. Text bearbeiten
5. Suchen und Ersetzen
6. Mehrere Stellen gleichzeitig bearbeiten
7. Code bearbeiten
8. Dateien, Ordner und Tabs

Die vollständige inhaltliche Übersicht steht in [`CURRICULUM.md`](CURRICULUM.md).

## Entwicklung

1. Abhängigkeiten installieren: `npm install`
2. Das Projekt in VS Code öffnen.
3. Mit `F5` eine Extension Development Host-Instanz starten.
4. In der Activity Bar das Hackschule-Tastatursymbol öffnen.

## Tastaturbelegung in der Schulumgebung

Die Anzeigesprache von VS Code und die Tastaturbelegung sind zwei getrennte Einstellungen. Eine deutsche Oberfläche ist für den Unterricht sinnvoll, behebt aber keine vertauschten Tastenkombinationen.

Für Linux-Arbeitsplätze und VS Code Web sollte die deutsche Tastaturbelegung bereits aktiv sein, bevor der Editor geöffnet wird. Falls VS Code bei Tastenkombinationen weiterhin die physische US-Tastenposition verwendet (zum Beispiel `Alt+Y` statt `Alt+Z`), empfiehlt sich in den zentral bereitgestellten Benutzereinstellungen:

```json
{
    "keyboard.dispatch": "keyCode"
}
```

Anschließend muss das VS-Code-Fenster neu geladen werden. Die Übung zum Zeilenumbruch nennt `Alt+Y` zusätzlich als Ausweichmöglichkeit, damit niemand an einer falsch erkannten Belegung hängen bleibt.

## Aufbau eines Tutorial-Schritts

Die Reihenfolge der Schritte steht in `tutorial/sections.yaml`. Jeder Schlüssel verweist auf eine gleichnamige HTML-Datei im Verzeichnis `tutorial/`.

Ein Schritt kann am Anfang Metadaten enthalten:

```html
<yaml>
file: tutorial/example.txt
cursor: [1, 1]
</yaml>
```

Das Übungsdokument wird pro Schritt unter `~/.hs-kbd-tutorial/steps/` kopiert. Beim normalen Wechsel zwischen Schritten bleibt der Arbeitsstand erhalten. **Neu starten** stellt die ursprüngliche Datei wieder her.

Im `<script>`-Block können die vom Extension Host gelieferten Ereignisse verarbeitet werden:

```js
handleOnDidChangeTextEditorSelection = function (event) {
    const selection = event.selections[0];
    setCheckBox("s0", selectionEquals(selection, 0, 0, 0, 5));
    checkTaskSolved();
};
```

Hilfsfunktionen wie `checkBox`, `setCheckBox`, `getCheckBox`, `selectionEquals`, `isCursorAt`, `addStepEventListener` und `onStepCleanup` stehen jedem Schritt zur Verfügung.

Der Fortschritt wird in `~/.hs-kbd-tutorial/.state.json` gespeichert.

## Datei- und Ordnerübungen

Das Kapitel **Dateien, Ordner und Tabs** verwendet einen einzigen isolierten Arbeitsordner unter `~/.hs-kbd-tutorial/Tastatur-Tutorial`. Seine Struktur bleibt während des gesamten Kapitels sichtbar. Die einzelnen Schritte öffnen oder verändern nur die jeweils benötigten Einträge.

Beim Öffnen und Schließen dieses Ordners lädt VS Code Web das Fenster neu. Die Erweiterung speichert deshalb den Übergang, aktiviert sich nach dem Neustart und öffnet die Tutorial-Ansicht automatisch wieder. Am Ende des Kapitels wird der Tutorial-Arbeitsordner geschlossen, sodass kein Übungsordner geöffnet bleibt.
