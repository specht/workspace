<div class='meta'>
section: programming
</div>

# Erstelle eine Präsentation in HTML!

<p class='abstract'>
Erstelle eine Präsentation mit shower.js in HTML und CSS. Mach dich unabhängig von PowerPoint und Keynote und erstelle eine Präsentation, die in jedem modernen Browser vom Stick oder von der Cloud aus funktioniert.
</p>

<img class='full' src='showerjs.webp'>

In diesem Tutorial erstellen wir eine Präsentation mit Hilfe von [shower.js](https://github.com/shower/shower) auf der Grundlage von HTML und CSS. Das heisst, dass jeder Webbrowser deine Präsentation abspielen kann.

## Los geht's!

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='vscode-empty-project.webp'>

### Repository klonen

<img class='r' style='width: 20em;' src='vscode-clone-from-git.webp'>

Wir holen uns eine Vorlage mit Hilfe von Git, damit wir gleich loslegen können.
Öffne dazu die Seite [https://git.nhcham.org/specht/shower.js](https://git.nhcham.org/specht/shower.js), klicke auf den blauen Button »Code« und wähle die HTTPS-URL.

Wähle »Clone Repository« in deinem Workspace, füge die URL mit <span class='key'>Strg</span><span class='key'>V</span> ein und bestätige mit Enter.

<img class='full' style='width: 40em;' src='vscode-enter-clone-uri.webp'>

Anschließend müssen wir einen Speicherort für das Verzeichnis wählen. Wählen dafür den Ordner `/workspace`. Das Repository wird jetzt ins Verzeichnis `/workspace/shower.js` geklont. Öffne anschließend das Verzeichnis, indem du entweder die Frage, ob das Repository jetzt geöffnet werden soll, mit »Open« beantwortest oder indem du alternativ das Verzeichnis selbst öffnest (<span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>Strg</span><span class='key'>O</span>).

Öffne die Datei `index.html` &ndash; dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='index-opened.webp'>

### Live Server installieren

Als nächstes müssen wir eine Erweiterung installieren, die uns eine Live-Vorschau unserer Präsentation zeigen kann. Öffne dazu links die Extensions, suche die Erweiterung »Live Server« und klicke auf »Install«, um die Erweiterung zu installieren. Anschließend sollte dein Workspace so aussehen:

<img class='full' src='live-server-installed.webp'>

Öffne nun wieder den Explorer und öffne die `index.html`. Unten rechts findest du jetzt den Eintrag »Go Live«. Drück dort drauf und deine Präsentation wird sich in einem neuen Tab öffnen, das du mit der Maus einfach aus dem Browser lösen kannst, um beide Fenster nebeneinander anzuordnen.

<img class='full' src='side-by-side.webp'>

### Schreib deine Präsentation

Du kannst nun damit beginnen, die Vorlage an deine Bedürfnisse anzupassen. Falls du nach Hilfe suchst: die Technologien, die hier verwendet werden, heißen HTML und CSS. Wenn du die `index.html` bearbeitest, musst du darauf achten, dass du vorsichtig bist, da die Syntax relativ wichtig ist und eine fehlende spitze Klammer dazu führen kann, dass viele Folien auf einmal nicht mehr sichtbar sind. Gehe also behutsam vor und verwende zur Not <span class='key'>Strg</span><span class='key'>Z</span>, um Änderungen rückgängig zu machen. Speichere deine Änderungen mit <span class='key'>Strg</span><span class='key'>S</span>, um den Effekt im Vorschaufenster zu sehen, falls dein Workspace Änderungen nicht schon automatisch speichert (das kannst du im Menü unter File / Auto Save einstellen).

#### HTML-Syntax

Generell ist es so, dass bei HTML Text in sogenannte Tags eingeschlossen wird. Schreibt man z. B. `H<sub>2</sub>O`, sorgen die `<sub>`-Tags dafür, dass die 2 tiefer gestellt wird: H<sub>2</sub>O. Ein öffnendes Tag wie `<sub>` muss immer mit einem passenden schließenden Tag `</sub>` geschlossen werden, wobei Tags beliebig geschachtelt werden können.

