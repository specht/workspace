<div class='meta'>
image: git-clone.webp
kind: reference
</div>

# Workspace: Grundgriffe

<p class='abstract'>
Handgriffe, die viele Aktivitäten brauchen: eine Vorlage aus einem Git-Repository klonen und die Live-Vorschau starten. Aktivitäten binden diese Abschnitte ein, damit sie überall gleich aussehen.
</p>

<!-- part: repository-klonen -->
## Repository klonen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp' alt='Ein leerer Workspace ohne geöffneten Ordner.'>

Klicke auf »Clone Repository« und füge die Adresse der Vorlage ein:

```bash
${template}
```

<img class='full' src='git-clone.webp' alt='Das Eingabefeld für die Adresse des Repositorys.'>

Bestätige mit <kbd>Enter</kbd>. Danach wählst du das Verzeichnis, in dem das Repository gespeichert werden soll: den Ordner `/workspace`.

<img class='full' src='choose-folder.webp' alt='Die Auswahl des Zielordners /workspace.'>

Das Repository wird jetzt in einen Unterordner von `/workspace` geklont, der so heißt wie das Repository. Beantworte die Frage, ob es jetzt geöffnet werden soll, mit »Open«:

<img class='full' src='open-yes-or-no.webp' alt='Die Frage, ob das geklonte Repository geöffnet werden soll.'>
<!-- /part -->

<!-- part: live-server -->
## Live-Vorschau starten

<img src='../../common/go-live.webp' class='r' style='width: 21em;' alt='Der Eintrag »Go Live« unten rechts in der Statusleiste.'>

Im Workspace ist die Erweiterung »Live Server« installiert. Unten rechts findest du den Eintrag »Go Live«. Klicke darauf, um die Vorschau zu starten. Sie öffnet sich in einem neuen Browsertab.

Der Live Server ist ein kleiner Webserver in deinem Workspace. Er liefert die Dateien aus deinem geöffneten Ordner aus, und die Adresse in der Vorschau zeigt, welche Datei du gerade siehst.

<div style='clear: both;'></div>

<div class='hint'>
Falls sich die Vorschau einmal nicht mehr aktualisiert, lade die Seite neu: <kbd>Strg</kbd><kbd>R</kbd> oder <kbd>F5</kbd>.
</div>
<!-- /part -->
