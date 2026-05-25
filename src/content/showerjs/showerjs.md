<div class='meta'>
image: showerjs.webp:0:80
</div>

<style>
.shower-mini {
    float: right;
    width: min(28rem, 45%);
    margin: 0.2rem 0 1.2rem 1.5rem;
}

.shower-mini-slide {
    aspect-ratio: 16 / 9;
    position: relative;
    overflow: hidden;

    background: white;
    color: #222;
    border-radius: 0.35rem;
    box-shadow: 0 0.35rem 1.2rem rgba(0, 0, 0, 0.25);

    padding: 7%;
    font-family: system-ui, sans-serif;
    font-size: clamp(0.55rem, 1.45vw, 0.9rem);
}

.shower-mini-slide h3 {
    margin: 0 0 0.6em;
    font-size: 1.8em;
    line-height: 1.1;
}

.shower-mini-slide p {
    margin: 0 0 0.6em;
    line-height: 1.35;
}

.shower-mini-slide img {
    max-width: 100%;
}

.shower-mini-slide .right {
    float: right;
    width: 38%;
    margin: 0 0 0.6em 0.8em;
}

.shower-mini-slide .place {
    position: absolute;
    transform: translate(-50%, -50%);
    left: 50%;
    top: 50%;
}

.shower-mini-slide .top {
    top: 12%;
}

.shower-mini-slide .bottom {
    top: 88%;
}

.shower-mini-slide .left {
    left: 15%;
}

.shower-mini-slide .right-place {
    left: 85%;
}

.shower-mini-slide .cover {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
    object-fit: cover;
    max-width: none;
}

.shower-mini-slide .over-image {
    position: relative;
    z-index: 1;
    color: white;
    text-shadow: 0 0.08em 0.25em black;
}

.shower-mini-clear {
    clear: both;
}

@media (max-width: 760px) {
    .shower-mini {
        float: none;
        width: 100%;
        margin: 1rem 0;
    }
}
</style>

# Eine Präsentation in HTML erstellen

<p class='abstract'>
Erstelle eine Präsentation mit shower.js in HTML und CSS. Mach dich unabhängig von PowerPoint und Keynote und erstelle eine Präsentation, die in jedem modernen Browser vom Stick oder von der Cloud aus funktioniert.
</p>

<img class='full fit-width' src='showerjs.webp'>

In diesem Tutorial erstellen wir eine Präsentation mit Hilfe von [shower.js](https://github.com/shower/shower) auf der Grundlage von HTML und CSS. Das heisst, dass jeder Webbrowser deine Präsentation abspielen kann und du keine besondere Software brauchst, um deine Präsentation abzuspielen. Du kannst shower.js hier [in Aktion sehen](https://shwr.me/).

## Repository klonen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

Wir holen uns eine Vorlage mit Hilfe von Git, damit wir gleich loslegen können. Klicke dazu auf »Clone Repository« und füge die folgende URL ein:

```bash
https://git.nhcham.org/specht/shower.js.git
```

<img class='full' src='git-clone.webp'>

Bestätige anschließend mit <span class='key'>Enter</span>.
Anschließend musst du ein Verzeichnis wählen, in dem das Repository gespeichert werden soll. Wähle dafür den Ordner `/workspace`.

<img class='full' src='choose-folder.webp'>

Das Repository wird jetzt ins Verzeichnis `/workspace/shower.js` geklont. Öffne anschließend das Verzeichnis, indem du die Frage,
ob das Repository jetzt geöffnet werden soll, mit »Open« beantwortest:

<img class='full' src='open-yes-or-no.webp'>

Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='freshly-cloned.webp'>

Links siehst du die Verzeichnisse und Dateien, die gerade heruntergeladen wurden. Öffne die Datei `index.html`, indem du darauf klickst.

## Live Server-Erweiterung installieren

Bevor wir mit dem Schreiben der Präsentation beginnen, installieren wir eine Erweiterung, die uns eine Live-Vorschau unserer Präsentation zeigt.
Öffne dazu links die Extensions, suche die Erweiterung »Live Server« und klicke auf »Install«, um die Erweiterung zu installieren. Anschließend sollte dein Workspace so aussehen:

<img class='full' src='live-server.webp'>

<img src='go-live.webp' class='r' style='width: 21em;'>

Öffne nun wieder den Explorer und öffne die Datei `index.html`. Unten rechts findest du jetzt den Eintrag »Go Live«.
Drück dort drauf und die Vorschau deiner Präsentation sollte sich in einem neuen Browsertab öffnen.

Wenn du alles richtig gemacht hast, sollte dein Fenster so aussehen:

<div style='clear: both;'></div>

<img class='full' src='live-preview.webp'>

Zieh das Tab aus dem Browser und ordne beide Fenster nebeneinander an, um deine Änderungen live in der Vorschau zu sehen:

<img class='full' src='side-by-side.webp'>

## Präsentation schreiben

Schau dir die Vorlage an und versuche, sie zu verstehen. Falls du nach Hilfe suchst: die Technologien, die hier verwendet werden, heißen HTML und CSS.
Du kannst dann damit beginnen, die Vorlage an deine Bedürfnisse anzupassen und deine Präsentation zu schreiben.

Speichere deine Änderungen mit <span class='key'>Strg</span><span class='key'>S</span>, um den Effekt im Vorschaufenster zu sehen, falls dein Workspace Änderungen nicht schon automatisch speichert (das kannst du im Menü unter »File« / »Auto Save« einstellen).

<div class='hint'>
Wenn du die Datei <code>index.html</code> bearbeitest, musst du darauf achten, dass du vorsichtig bist, da die Syntax relativ wichtig ist und eine fehlende spitze Klammer dazu führen kann, dass viele Folien auf einmal nicht mehr sichtbar sind. Gehe also behutsam vor und verwende zur Not <span class='key'>Strg</span><span class='key'>Z</span>, um Änderungen rückgängig zu machen. Wenn du dir unsicher bist, ob deine Änderungen funktionieren, speichere sie und schau dir das Ergebnis in der Vorschau an. Du kannst sie immer noch rückgängig machen.
</div>

<div class='hint melting'>
Falls sich deine Vorschau einmal nicht mehr aktualisieren sollte, kannst du die Seite einfach neu laden. Drück dafür einfach <span class='key'>Strg</span><span class='key'>R</span> oder <span class='key'>F5</span>.
</div>

## Präsentation halten

Wenn du deine Präsentation halten möchtest, kannst du sie einfach im Browser öffnen und mit den Pfeiltasten navigieren. Du kannst auch die Maus benutzen, um durch die Präsentation zu scrollen. Der besondere Vorteil bei dieser Art von Präsentation ist, dass sie in jedem modernen Browser funktioniert und du keine spezielle Software brauchst, um sie abzuspielen. Du kannst einfach das ganze Verzeichnis auf einen Stick kopieren und deine Präsentation überall abspielen, wo es einen modernen Webbrowser gibt.

## Nachschlagen: Folien gestalten

### Der Grundaufbau einer Folie
### Text und Überschriften
### Listen und Stichpunkte
### Wörter hervorheben

### Bilder einfügen
### Bilder vergrößern, verkleinern und zuschneiden
### Bilder links oder rechts platzieren
### Ein Bild als Hintergrund verwenden

### Farben ändern
### Schriftgrößen ändern
### Abstände ändern
### Elemente genau platzieren
### Zwei Spalten verwenden

### Dinge nacheinander einblenden
### Wiederverwendbare CSS-Klassen schreiben
### Häufige Fehler finden