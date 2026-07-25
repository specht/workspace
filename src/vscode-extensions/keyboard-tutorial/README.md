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
