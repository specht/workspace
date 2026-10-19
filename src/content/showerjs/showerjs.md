<div class='meta'>
image: showerjs.webp:0:80
image_alt: "Eine orange Schreibmaschine, darunter der Platzhalter »Titel deiner Präsentation«"
kind: projekt
language: HTML
topic: writing
duration: 2 bis 3 Doppelstunden
reference: [workspace, html, css, layout, shower]
template: https://git.nhcham.org/specht/shower.js.git
template_name: der shower.js-Vorlage
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

<!-- Die Beispiele aus dem Handbuch erscheinen hier als Folie (media_variants.js). -->
<div data-media-context='folie' hidden></div>

# Eine Präsentation in HTML erstellen

<p class='abstract'>
Erstelle eine Präsentation mit shower.js in HTML und CSS. Mach dich unabhängig von PowerPoint und Keynote und erstelle eine Präsentation, die in jedem modernen Browser vom Stick oder aus der Cloud funktioniert.
</p>

<img class='full fit-width' src='showerjs.webp' alt='Die Titelfolie einer shower.js-Präsentation: »Shower Presentation Engine«, darunter »Brought to you by Vadim Makeev«, im Hintergrund Hände an einer orangefarbenen Schreibmaschine auf einem Holztisch, oben rechts die Foliennummer 1.'>

In diesem Tutorial erstellst du eine Präsentation mit [shower.js](https://github.com/shower/shower). Sie besteht aus HTML und CSS, wie eine Webseite: Jeder Browser kann sie abspielen, du brauchst keine besondere Software. Hier kannst du shower.js [in Aktion sehen](https://shwr.me/).

Hast du schon eine [Webseite mit HTML und CSS](/static-website) gebaut, kennst du das meiste. Die Abschnitte aus dem Handbuch, die du dort schon gelesen hast, stehen hier nur noch als eine Zeile.

## Repository klonen

Wir holen uns eine Vorlage mit Hilfe von Git, damit du gleich loslegen kannst.

_include(workspace#repository-klonen)

Das Repository liegt jetzt in `/workspace/shower.js`. Dein Workspace sollte ungefähr so aussehen:

<!-- tutorial-screenshot
# Klonen wie im Handbuch »Workspace« (oben eingebunden), dann die geöffnete
# Vorlage mit index.html
show-left-sidebar
clone-start: https://git.nhcham.org/specht/shower.js.git @ 50a071df026545793cfa29853e5e4ed6b1ece36e
press: Enter
wait-for-input-value: /workspace/
press: Enter
wait-for-text: Would you like to open the repository?
click: Open
wait-for-text: index.html
open-file: index.html
close-tab: Welcome
sleep: 1
-->

<img class='full' src='freshly-cloned.webp' alt='Der Explorer zeigt die geklonte Vorlage mit den Ordnern include, pictures und shower sowie index.html und styles.css; rechts ist index.html geöffnet.'>

Öffne die Datei `index.html`, indem du links darauf klickst.

_task(vorlage)

## Vorschau starten

_include(workspace#live-server)

Wenn du alles richtig gemacht hast, sollte dein Fenster so aussehen:

<!-- tutorial-screenshot
# Die Vorlage in der Vorschau: die Übersicht aller Folien
go-live
tab: preview
-->

<img class='full' src='live-preview.webp' alt='Die Vorlage in der Vorschau: eine Übersicht aller Folien, die erste mit einem Titelbild.'>

Zieh das Tab aus dem Browser und ordne beide Fenster nebeneinander an, dann siehst du jede Änderung sofort:

<!-- tutorial-screenshot
# VS Code und die Vorschau nebeneinander, wie zwei schmale Fenster: links
# index.html ohne Seitenleiste, rechts die Übersicht der Folien
hide-left-sidebar
viewport: 760x700
beside: preview
-->
<img class='full' src='side-by-side.webp' alt='Links VS Code mit index.html, rechts daneben die Vorschau der Präsentation.'>

_task(adresse)

## Folien schreiben

Für eine Präsentation brauchst du zwei Sprachen: **HTML** beschreibt den Inhalt (Überschriften, Texte, Listen, Bilder), **CSS** das Aussehen (Farben, Größen, Abstände). In der Vorlage stehen deine Folien in `index.html`, ihr Aussehen in `styles.css`.

_include(shower#folie)

Schau dir die Folien der Vorlage an: Sie zeigen, was möglich ist. Ändere die erste Folie zu deiner Titelfolie, und lösche die Beispielfolien, sobald du sie nicht mehr brauchst.

<div class='hint'>
Speichere mit <kbd>Strg</kbd><kbd>S</kbd>, dann zeigt die Vorschau die Änderung. Unter »File« / »Auto Save« speichert VS Code von selbst.
</div>

_task(titelfolie)

_include(html#listen)

_include(shower#gute-folien)

_task(fuenf-folien)

Mehr zum Text auf Folien: _uses(text, hervorheben, links)

## Bilder

_include(html#bilddateien)

_task(umbenennen)

_task(eigenes-bild)

_include(html#img)

_task(bild-auf-folie)

Bild neben Text, als Hintergrund, im Zuschnitt: _uses(vorlage, bildgroesse)

## Gestalten

_include(css#regeln)

_task(eigene-regel)

_include(css#wirkt-nicht)

<div class='hint try'>
<strong>Ausprobieren:</strong> Rechtsklicke in der Vorschau auf die Überschrift einer Folie und wähle »Untersuchen«. Klicke bei einer ihrer Regeln auf einen Wert, etwa die Farbe, und ändere ihn: Die Folie ändert sich sofort. Was ist von deiner Änderung noch da, wenn du die Vorschau neu lädst, und was steht in <code>styles.css</code>? Warum gehört eine Regel, die bleiben soll, trotzdem dorthin?
</div>

Was du sonst gestalten kannst: _uses(farben, schrift, abstaende, spalten, platzieren, seitenzahl, formeln)

## Präsentation halten

_include(shower#praesentieren)

_task(alles-im-ordner)

_task(file-adresse)

Du kannst deine Präsentation auch online stellen, zum Beispiel direkt in der Hackschule. <a href="/custom-subdomain">Hier erfährst du, wie das geht.</a>

## Abgeben

_task(abgabe)

<script>
// Die Abschnitte zum Gestalten stehen im Handbuch. Alte Links wie
// /showerjs#bilder-einfügen führen dorthin weiter, wenn es den Abschnitt
// hier nicht mehr gibt.
window.addEventListener('load', function () {
    setTimeout(function () {
        var id = decodeURIComponent(window.location.hash.slice(1));
        if (id && !document.getElementById(id)) {
            window.location.replace('/reference/html' + window.location.hash);
        }
    }, 300);
});
</script>

<!-- tutorial-screenshot
# Das Bild oben auf der Seite und für die Karte: die erste Folie der Vorlage,
# angeklickt, so groß wie beim Vortrag (die Vorschau läuft seit oben)
preview-reload
tab: preview
viewport: 1280x720
click: selector:section.slide:first-of-type
sleep: 1.5
move-mouse: selector:body
-->
<img class='teaser' hidden src='showerjs.webp' alt='Die Titelfolie der shower.js-Vorlage: oben eine orangefarbene Schreibmaschine vor hellblauem Grund, darunter »Titel deiner Präsentation« und »Dein Name«.'>

