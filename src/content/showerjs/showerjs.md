<div class='meta'>
image: showerjs.webp:0:80
</div>

<style>
/*
 * Mini-Folien im Referenzteil
 * --------------------------------
 * Die Mini-Folien verwenden dieselbe 16:9-Grundfläche wie die Shower-Vorlage:
 * 1024 × 576 Pixel. Vorschau und Lightbox skalieren diese Fläche nur optisch.
 * Dadurch bleibt das Layout beim Vergrößern exakt gleich.
 */
.shower-mini,
.mini-lightbox-content {
    --mini-scale: 1;
    --mini-slide-width: 1024px;
    --mini-slide-height: 576px;
}

.shower-mini {
    box-sizing: border-box;
    float: right;
    position: relative;

    width: min(28rem, 45%);
    aspect-ratio: 16 / 9;
    margin: 0.2rem 0 1.2rem 1.5rem;

    overflow: hidden;
    border-radius: 10px;
    box-shadow: 0 0.35rem 1.15rem rgba(0, 0, 0, 0.22);

    color: inherit;
    text-decoration: none;
    cursor: zoom-in;

    transition:
        border-color 160ms ease,
        box-shadow 160ms ease,
        filter 160ms ease;
}

.shower-mini::before {
    content: '';
    position: absolute;
    inset: 0;
    z-index: 1;

    border-radius: inherit;
    box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.12);
    pointer-events: none;

    transition: box-shadow 160ms ease;
}

.shower-mini:hover,
.shower-mini:focus-visible {
    box-shadow: 0 0.55rem 1.45rem rgba(0, 0, 0, 0.3);
}

.shower-mini:hover::before,
.shower-mini:focus-visible::before {
    box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.26);
}

.shower-mini:focus-visible {
    outline: 0.18rem solid currentColor;
    outline-offset: 0.3rem;
}

.shower-mini-slide {
    box-sizing: border-box;
    position: relative;
    overflow: hidden;

    width: var(--mini-slide-width);
    height: var(--mini-slide-height);
    padding: 70px;

    background: white;
    color: #222;
    border-radius: 10px;

    font-family: 'PT Sans', sans-serif;
    font-size: 34px;
    line-height: 1.35;

    transform: scale(var(--mini-scale));
    transform-origin: top left;
}

.shower-mini > .shower-mini-slide,
.mini-lightbox-content > .shower-mini-slide {
    position: absolute;
    top: 0;
    left: 0;
}

.shower-mini-slide .mini-title {
    margin: 0 0 0.35em;

    font-family: 'PT Sans Narrow', 'PT Sans', sans-serif;
    font-size: 64px;
    font-weight: bold;
    line-height: 1.05;
}

.shower-mini-slide p {
    margin: 0 0 0.6em;
}

.shower-mini-slide ul,
.shower-mini-slide ol {
    margin: 0 0 0 1.25em;
    padding: 0;
}

.shower-mini-slide li {
    margin: 0.18em 0;
}

.shower-mini-slide img {
    max-width: 100%;
}


/* Shower-Ribbon mit Foliennummer für Mini-Folien.
 * Diese Vorschau übernimmt die Maße der offiziellen Shower-Vorlage:
 * links 875px, 50px breit, 100px hoch und rotes Lesezeichen.
 * In echten Shower-Folien steht die Zahl in counter(slide); in den Mini-Folien
 * setzen wir sie mit data-page, damit die Beispiele unabhängig funktionieren.
 */
.shower-mini-slide {
    --color-blue: #4b86c2;
    --color-red: #cc0000;
    --color-yellow: #fafaa2;
    --color-grey: #585a5e;
    --ribbon-size: 50px;
}

.shower-mini-slide[data-page]::after {
    position: absolute;
    top: 0;
    left: 875px;
    z-index: 2;

    box-sizing: border-box;
    width: var(--ribbon-size);
    height: calc(var(--ribbon-size) * 2);
    padding-top: 15px;

    clip-path: polygon(0% 0%, 100% 0%, 100% 100%, 50% 80%, 0% 100%);
    background-color: var(--color-red);
    color: white;

    font-family: 'PT Sans', sans-serif;
    font-size: 25px;
    font-weight: normal;
    line-height: 2;
    text-align: center;

    content: attr(data-page);
}

.shower-mini-slide.mini-blue-ribbon::after {
    background-color: var(--color-blue);
}

.shower-mini-slide.mini-round-page::after {
    top: 32px;
    left: auto;
    right: 55px;

    display: grid;
    place-items: center;

    width: 68px;
    height: 68px;
    padding-top: 0;

    clip-path: none;
    border-radius: 50%;
    background-color: var(--color-blue);

    font-size: 31px;
    line-height: 1;
}

.shower-mini-slide.mini-plain-page::after {
    top: auto;
    left: auto;
    right: 60px;
    bottom: 32px;

    width: auto;
    height: auto;
    padding-top: 0;

    clip-path: none;
    background: transparent;
    color: var(--color-grey);

    font-size: 28px;
    line-height: 1;
}

.shower-mini-slide.mini-no-ribbon::after {
    visibility: hidden;
}

/* Hilfsklassen für typische Beispiele. */
.shower-mini-slide .mini-right {
    float: right;
    width: 38%;
    margin: 0 0 0.6em 0.8em;
}

.shower-mini-slide .mini-place {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
}

.shower-mini-slide .mini-top {
    top: 12%;
}

.shower-mini-slide .mini-bottom {
    top: 88%;
}

.shower-mini-slide .mini-left {
    left: 15%;
}

.shower-mini-slide .mini-right-place {
    left: 85%;
}

.shower-mini-slide .mini-cover {
    position: absolute;
    inset: 0;

    width: 100%;
    height: 100%;
    max-width: none;
    object-fit: cover;
}

.shower-mini-slide .mini-over-image {
    position: relative;
    z-index: 1;

    color: white;
    text-shadow: 0 0.08em 0.25em black;
}

.shower-mini-clear {
    clear: both;
}

/* Eine einzige wiederverwendbare Lightbox für alle Mini-Folien. */
.mini-lightbox {
    width: 100vw;
    height: 100vh;
    max-width: none;
    max-height: none;
    margin: 0;
    padding: 2rem;

    border: 0;
    background: transparent;
    opacity: 0;

    transition: opacity 180ms ease;
}

.mini-lightbox::backdrop {
    background: rgba(0, 0, 0, 0.75);
    opacity: 0;
    transition: opacity 180ms ease;
}

.mini-lightbox[open] {
    display: grid;
    place-items: center;
}

.mini-lightbox.is-visible,
.mini-lightbox.is-visible::backdrop {
    opacity: 1;
}

.mini-lightbox-content {
    position: relative;

    width: min(1024px, calc(100vw - 4rem), calc((100vh - 4rem) * 16 / 9));
    aspect-ratio: 16 / 9;

    overflow: hidden;
    border-radius: 10px;
    box-shadow: 0 0.8rem 3rem rgba(0, 0, 0, 0.45);

    opacity: 0;
    transform: scale(0.96);
    transition:
        opacity 180ms ease,
        transform 180ms ease;
}

.mini-lightbox.is-visible .mini-lightbox-content {
    opacity: 1;
    transform: scale(1);
}

.mini-lightbox-close {
    position: fixed;
    top: 1rem;
    right: 1.4rem;
    z-index: 1;

    border: 0;
    background: transparent;
    color: white;

    font-family: 'PT Sans', sans-serif;
    font-size: 3rem;
    line-height: 1;
    cursor: pointer;
}

@media (max-width: 760px) {
    .shower-mini {
        float: none;
        width: 100%;
        margin: 1rem 0;
    }
}

@media (prefers-reduced-motion: reduce) {
    .shower-mini,
    .shower-mini::before,
    .mini-lightbox,
    .mini-lightbox::backdrop,
    .mini-lightbox-content {
        transition: none;
    }
}
</style>
<script>
document.addEventListener('DOMContentLoaded', () => {
    const SLIDE_WIDTH = 1024;
    const ANIMATION_TIME = 180;

    const lightbox = document.createElement('dialog');
    lightbox.className = 'mini-lightbox';
    lightbox.innerHTML = `
        <button class='mini-lightbox-close' type='button' aria-label='Vorschau schließen'>×</button>
        <div class='mini-lightbox-content'></div>
    `;

    document.body.appendChild(lightbox);

    const content = lightbox.querySelector('.mini-lightbox-content');
    const closeButton = lightbox.querySelector('.mini-lightbox-close');

    function setSlideScale(container) {
        const width = container.clientWidth || container.offsetWidth;
        if (width > 0) {
            container.style.setProperty('--mini-scale', width / SLIDE_WIDTH);
        }
    }

    function updateAllScales() {
        document.querySelectorAll('.shower-mini').forEach(setSlideScale);
        if (lightbox.open) {
            setSlideScale(content);
        }
    }

    function prepareMiniSlides() {
        document.querySelectorAll('.shower-mini').forEach((mini) => {
            mini.setAttribute('role', 'button');
            mini.setAttribute('tabindex', '0');
            mini.setAttribute('aria-label', 'Beispiel vergrößern');
            setSlideScale(mini);
        });
    }

    function openMiniSlide(mini) {
        const slide = mini.querySelector('.shower-mini-slide');
        if (!slide) return;

        content.replaceChildren(slide.cloneNode(true));
        lightbox.classList.remove('is-visible');

        if (typeof lightbox.showModal === 'function') {
            lightbox.showModal();
        } else {
            lightbox.setAttribute('open', '');
        }

        setSlideScale(content);

        requestAnimationFrame(() => {
            setSlideScale(content);
            lightbox.classList.add('is-visible');
        });
    }

    function closeMiniSlide() {
        if (!lightbox.open) return;

        lightbox.classList.remove('is-visible');

        window.setTimeout(() => {
            if (typeof lightbox.close === 'function') {
                lightbox.close();
            } else {
                lightbox.removeAttribute('open');
            }

            content.replaceChildren();
        }, ANIMATION_TIME);
    }

    document.addEventListener('click', (event) => {
        const mini = event.target.closest('.shower-mini');
        if (!mini) return;

        event.preventDefault();
        openMiniSlide(mini);
    });

    document.addEventListener('keydown', (event) => {
        const mini = event.target.closest('.shower-mini');

        if (mini && (event.key === 'Enter' || event.key === ' ')) {
            event.preventDefault();
            openMiniSlide(mini);
        }
    });

    closeButton.addEventListener('click', closeMiniSlide);

    lightbox.addEventListener('click', (event) => {
        if (event.target === lightbox) {
            closeMiniSlide();
        }
    });

    lightbox.addEventListener('cancel', (event) => {
        event.preventDefault();
        closeMiniSlide();
    });

    if ('ResizeObserver' in window) {
        const observer = new ResizeObserver(updateAllScales);
        document.querySelectorAll('.shower-mini').forEach((mini) => observer.observe(mini));
        observer.observe(content);
    }

    window.addEventListener('resize', updateAllScales);

    prepareMiniSlides();
    updateAllScales();
});
</script>

# Eine Präsentation in HTML erstellen

<p class='abstract'>
Erstelle eine Präsentation mit shower.js in HTML und CSS. Mach dich unabhängig von PowerPoint und Keynote und erstelle eine Präsentation, die in jedem modernen Browser vom Stick oder von der Cloud aus funktioniert.
</p>

<img class='full fit-width' src='showerjs.webp'>

In diesem Tutorial erstellen wir eine Präsentation mit Hilfe von [shower.js](https://github.com/shower/shower) auf der Grundlage von HTML und CSS. Das heißt, dass jeder Webbrowser deine Präsentation abspielen kann und du keine besondere Software brauchst. Du kannst shower.js hier [in Aktion sehen](https://shwr.me/).

## Repository klonen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

Wir holen uns eine Vorlage mit Hilfe von Git, damit du gleich loslegen kannst. Klicke dazu auf »Clone Repository« und füge die folgende URL ein:

```bash
https://git.nhcham.org/specht/shower.js.git
```

<img class='full' src='git-clone.webp'>

Bestätige anschließend mit <span class='key'>Enter</span>. Danach musst du ein Verzeichnis wählen, in dem das Repository gespeichert werden soll. Wähle dafür den Ordner `/workspace`.

<img class='full' src='choose-folder.webp'>

Das Repository wird jetzt ins Verzeichnis `/workspace/shower.js` geklont. Öffne anschließend das Verzeichnis, indem du die Frage, ob das Repository jetzt geöffnet werden soll, mit »Open« beantwortest:

<img class='full' src='open-yes-or-no.webp'>

Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='freshly-cloned.webp'>

Links siehst du die Verzeichnisse und Dateien, die gerade heruntergeladen wurden. Öffne die Datei `index.html`, indem du darauf klickst.

## Live Server-Erweiterung installieren

Bevor wir mit dem Schreiben der Präsentation beginnen, installieren wir eine Erweiterung, die uns eine Live-Vorschau unserer Präsentation zeigt. Öffne dazu links die Extensions, suche die Erweiterung »Live Server« und klicke auf »Install«. Anschließend sollte dein Workspace so aussehen:

<img class='full' src='live-server.webp'>

<img src='go-live.webp' class='r' style='width: 21em;'>

Öffne nun wieder den Explorer und öffne die Datei `index.html`. Unten rechts findest du jetzt den Eintrag »Go Live«. Drück dort drauf und die Vorschau deiner Präsentation sollte sich in einem neuen Browsertab öffnen.

Wenn du alles richtig gemacht hast, sollte dein Fenster so aussehen:

<div style='clear: both;'></div>

<img class='full' src='live-preview.webp'>

Zieh das Tab aus dem Browser und ordne beide Fenster nebeneinander an, um deine Änderungen live in der Vorschau zu sehen:

<img class='full' src='side-by-side.webp'>

## Präsentation schreiben

Schau dir die Vorlage an und versuche, sie zu verstehen. Die Präsentation steht in der Datei `index.html`. Sie besteht aus einzelnen Folien, die du verändern, löschen oder ergänzen kannst.

Für diese Präsentation verwendest du zwei Sprachen:

- **HTML** beschreibt den Inhalt: Überschriften, Texte, Listen, Bilder und Folien.
- **CSS** beschreibt das Aussehen: Farben, Größen, Abstände und Positionen.

In der Vorlage sind Inhalt und Aussehen getrennt:

- Deine Folien stehen in der Datei `index.html`.
- Deine eigenen Gestaltungsregeln schreibst du in die Datei `styles.css`.

Öffne deshalb am besten zusätzlich die Datei `styles.css`.
Am Ende der Datei findest du diesen Kommentar:

```css
/*
 * Eigene CSS-Regeln
 *
 * Hier kannst du das Aussehen deiner Folien verändern:
 * Farben, Größen, Abstände, Schatten und Positionen.
 *
 * Schreibe deine eigenen CSS-Regeln unter diese Zeile.
 */
```

Wenn in diesem Tutorial später CSS-Beispiele gezeigt werden, gehören sie normalerweise **unter diesen Kommentar in `styles.css`**.
So bleibt `index.html` übersichtlich: Dort stehen die Folien, während `styles.css` für das Aussehen zuständig ist.

Du kannst jetzt damit beginnen, die Vorlage an deine Bedürfnisse anzupassen und deine Präsentation zu schreiben.

Speichere deine Änderungen mit <span class='key'>Strg</span><span class='key'>S</span>, um den Effekt im Vorschaufenster zu sehen, falls dein Workspace Änderungen nicht schon automatisch speichert. Das kannst du im Menü unter »File« / »Auto Save« einstellen.

<div class='hint'>
Wenn du die Datei <code>index.html</code> bearbeitest, musst du vorsichtig sein, weil die Syntax relativ wichtig ist. Schon eine fehlende spitze Klammer kann dazu führen, dass mehrere Folien auf einmal nicht mehr sichtbar sind. Gehe also behutsam vor und verwende zur Not <span class='key'>Strg</span><span class='key'>Z</span>, um Änderungen rückgängig zu machen. Wenn du dir unsicher bist, ob deine Änderungen funktionieren, speichere sie und schau dir das Ergebnis in der Vorschau an. Du kannst sie immer noch rückgängig machen.
</div>

<div class='hint melting'>
Falls sich deine Vorschau einmal nicht mehr aktualisieren sollte, kannst du die Seite einfach neu laden. Drück dafür <span class='key'>Strg</span><span class='key'>R</span> oder <span class='key'>F5</span>.
</div>

## Präsentation halten

Wenn du deine Präsentation halten möchtest, kannst du sie einfach im Browser öffnen und mit den Pfeiltasten navigieren. Du kannst auch die Maus benutzen, um durch die Präsentation zu scrollen.

Der besondere Vorteil bei dieser Art von Präsentation ist, dass sie in jedem modernen Browser funktioniert und du keine spezielle Software brauchst. Du kannst einfach das ganze Verzeichnis auf einen Stick kopieren und deine Präsentation überall abspielen, wo es einen modernen Webbrowser gibt.

## Referenz: Folien gestalten

<!--
Dieser Referenzteil soll später wie ein Nachschlagewerk funktionieren, nicht wie ein linearer Lehrgang.
Jeder Unterabschnitt sollte ein kleines Problem lösen: kurze Erklärung, Mini-Folie, kopierbares Beispiel und ein Hinweis auf typische Fehler.
-->

In diesem Abschnitt findest du kurze Beispiele für typische Dinge, die du beim Gestalten deiner Folien brauchst. Du musst nicht alles der Reihe nach lesen. Such dir einfach die Stelle heraus, die zu deinem Problem passt.

### Grundlagen

<!--
Hier sollten nur die HTML-Grundbausteine stehen, die man braucht, um überhaupt eine saubere Folie zu schreiben.
Ziel: Die Schülerinnen und Schüler sollen bestehende Folien sicher ändern können, bevor CSS und Layout dazukommen.
-->

#### Der Grundaufbau einer Folie

<!--
Dieser Abschnitt führt <section class="slide"> als Rahmen einer Folie ein.
Gezeigt werden sollten: Überschrift, Liste, öffnende und schließende Tags, Einrückung und die Idee, dass alles zwischen <section> und </section> zu genau einer Folie gehört.
Die Mini-Folie zeigt zusätzlich schon das rote Standard-Ribbon von Shower mit einer Beispiel-Foliennummer, damit die Vorschau wie die echte Vorlage wirkt.
-->

Eine Folie ist in shower.js ein eigener Abschnitt in der Datei `index.html`.
Dieser Abschnitt beginnt mit `<section class="slide">` und endet mit `</section>`.
Alles, was zwischen diesen beiden Zeilen steht, gehört zu dieser einen Folie.

<div class='shower-mini'>
<div class='shower-mini-slide' data-page='7'>
<div class='mini-title'>Meine Lieblingstiere</div>
<ol>
<li>🐧 Pinguine</li>
<li>🦊 Füchse</li>
<li>🐬 Delfine</li>
<li>🦉 Eulen</li>
</ol>
</div>
</div>

```html
<section class="slide">
    <h2>Meine Lieblingstiere</h2>
    <ol>
        <li>🐧 Pinguine</li>
        <li>🦊 Füchse</li>
        <li>🐬 Delfine</li>
        <li>🦉 Eulen</li>
    </ol>
</section>
```

Die wichtigsten Teile sind:

- `<section class="slide">` beginnt eine neue Folie.
- `<h2>` ist die Überschrift der Folie.
- `<ol>` beginnt eine nummerierte Liste.
- Jedes `<li>` ist ein Punkt in der Liste.
- `</section>` beendet die Folie.

Die meisten HTML-Tags bestehen aus einem **öffnenden Tag** und einem **schließenden Tag**:

```html
<h2>Meine Überschrift</h2>
```

Das öffnende Tag ist `<h2>`. Das schließende Tag ist `</h2>` – beim schließenden Tag steht also zusätzlich ein `/` vor dem Namen.

<div class='hint'>
Wenn du ein Tag öffnest, musst du es meistens auch wieder schließen.
Fehlt ein schließendes Tag, kann es passieren, dass die Folie oder sogar mehrere Folien danach falsch angezeigt werden.
</div>

Einrückungen sind für den Browser nicht unbedingt nötig, aber sie helfen dir beim Lesen.
Man sieht dadurch besser, welche Teile zusammengehören:

```html
<section class="slide">
    <h2>Überschrift</h2>
    <p>Dieser Text gehört zur Folie.</p>
</section>
```

<div class='shower-mini-clear'></div>

#### Text und Überschriften

<!--
Hier sollten <h2>, <p> und eventuell <br> erklärt werden.
Wichtig wäre: Eine Folie hat meistens eine <h2>-Überschrift; längere Texte gehören in Absätze; zu viel Text macht Folien schnell unlesbar.
-->

#### Listen und Stichpunkte

<!--
Hier sollten <ul>, <ol> und <li> eingeführt werden.
Sinnvoll wäre ein Vergleich zwischen ungeordneter Liste und nummerierter Liste sowie ein Hinweis, dass jeder Listenpunkt ein eigenes <li> braucht.
-->

#### Wörter hervorheben

<!--
Hier sollten einfache Inline-Tags wie <strong>, <em>, <code>, <sub> und <sup> gezeigt werden.
Ziel: Einzelne Wörter hervorheben, ohne gleich eigenes CSS schreiben zu müssen.
-->

### Bilder

<!--
Dieser Block sollte Bilder Schritt für Schritt einführen: erst einfügen, dann Größe und Zuschnitt, dann Platzierung, dann Hintergrundbilder und Lesbarkeit.
Die Beispiele sollten möglichst mit denselben Bilddateien funktionieren, die ohnehin im Tutorial vorkommen.
-->

#### Bilder einfügen

<!--
Hier sollte <img src="..."> erklärt werden.
Wichtig wären Dateinamen, relative Pfade, Dateiendungen und ein kurzer Hinweis auf alt-Text, ohne den Abschnitt zu überfrachten. Wir sollten auch erklären, wie man ein Bild entweder herunterladen und per Drag & Drop in das richtige Verzeichnis ziehen oder direkt in VS Code mit <span class='key'>Strg</span><span class='key'>V</span> einfügen kann (wenn das geht). Für Profis erklären wir, wie man ein Bild mit wget im Terminal herunterlädt.
-->

#### Bilder vergrößern, verkleinern und zuschneiden

<!--
Hier sollten typische Größenprobleme gelöst werden: Bild zu groß, Bild zu klein, falsches Seitenverhältnis.
Mögliche Beispiele: width per style, vorhandene Hilfsklassen der Vorlage und object-fit für zugeschnittene Bilder.
-->

#### Bilder links oder rechts platzieren

<!--
Hier sollten einfache Platzierungen neben Text gezeigt werden.
Passend zur bisherigen Vorlage könnten Klassen wie class='r' bzw. class='l' oder kleine eigene CSS-Klassen erklärt werden.
-->

#### Ein Bild als Hintergrund verwenden

<!--
Hier sollte gezeigt werden, wie ein Bild die ganze Folie füllt.
Wichtig: Der Unterschied zwischen normalem Bild im Textfluss und Hintergrund-/Cover-Bild, außerdem der Umgang mit Zuschnitt.
-->

#### Text auf Bildern lesbar machen

<!--
Hier sollten Kontrastprobleme gelöst werden: Textschatten, halbtransparenter Kasten, dunkle Überlagerung oder helle Fläche.
Ziel: Nicht nur zeigen, dass es geht, sondern warum Lesbarkeit wichtiger als ein schönes Bild ist.
-->

### Gestaltung

<!--
Hier beginnt CSS als Werkzeug zur Gestaltung.
Die Abschnitte sollten bewusst klein bleiben: erst einzelne Eigenschaften ändern, dann wiederverwendbare Klassen.
-->

#### Farben ändern

<!--
Hier sollten color und background erklärt werden.
Gute Reihenfolge: erst ein einzelnes Element färben, dann eine eigene Klasse für wiederholbare Gestaltung.
-->

#### Schriftgrößen ändern

<!--
Hier sollten font-size und eventuell line-height gezeigt werden.
Wichtig wäre ein Hinweis, dass größere Schrift oft besser ist als mehr Text und dass zu viele verschiedene Größen unruhig wirken.
-->

#### Schriftarten ändern

<!--
Hier sollte font-family erklärt werden, aber eher vorsichtig.
Sinnvoll wäre: vorhandene Schriftarten nutzen, nicht zu viele Schriftarten mischen, Überschriften und normalen Text unterscheiden.
-->

#### Abstände ändern

<!--
Hier sollten margin und padding auf sehr einfache Weise erklärt werden.
Ziel: Schülerinnen und Schüler sollen Abstand erzeugen, ohne leere Absätze oder viele <br>-Tags zu missbrauchen.
-->

#### Seitenzahl-Ribbon ändern oder ausblenden

<!--
Hier soll erklärt werden, dass die Standardvorlage das rote Seitenzahl-Ribbon schon automatisch mitbringt.
Wichtig ist jetzt besonders: In der Starter-Vorlage steht dafür kein <style>-Block in index.html, sondern die eigenen Regeln gehören in styles.css.
Zuerst sollte man das Default-Verhalten verstehen, dann kleine CSS-Änderungen ausprobieren: Farbe ändern, eine andere Form verwenden, die Seitenzahl an eine andere Stelle setzen oder sie ausblenden.
Dabei sollte klar bleiben, dass man normalerweise kein neues HTML für die Zahl schreibt, sondern das vorhandene ::after-Styling der Vorlage anpasst.
-->

In der Standardvorlage von shower.js erscheint rechts oben automatisch ein rotes Ribbon mit der Foliennummer.
Du musst dafür normalerweise **nichts** in deine Folie schreiben.
Die Vorlage erzeugt dieses Ribbon selbst mit Hilfe dieser CSS-Regel:

<div class='shower-mini'>
<div class='shower-mini-slide' data-page='12'>
<div class='mini-title'>Ausflug nach Berlin</div>
<ul>
<li>🏛️ Reichstag besuchen</li>
<li>🚲 Mit dem Rad durch die Stadt</li>
<li>🍟 Currywurst probieren</li>
</ul>
</div>
</div>

```css
.slide::after {
    position: absolute;
    top: 0;
    left: 875px;
    padding-top: 15px;
    box-sizing: border-box;
    width: var(--ribbon-size);
    height: calc(var(--ribbon-size) * 2);
    clip-path: polygon(0% 0%, 100% 0%, 100% 100%, 50% 80%, 0% 100%);
    background-color: var(--color-red);
    color: white;
    text-align: center;
    counter-increment: slide;
    content: counter(slide);
}
```

Die Zeile `content: counter(slide);` ist besonders wichtig.
Sie sorgt dafür, dass die Foliennummer automatisch eingesetzt wird.
Du schreibst die Zahl also **nicht** selbst in den HTML-Code der Folie.

Die Variablen `--color-red` und `--ribbon-size` kommen ebenfalls aus der Vorlage.
`var(--color-red)` bedeutet: »Nimm den Wert der CSS-Variable `--color-red`.«
So kann die Vorlage Farben und Größen an einer zentralen Stelle verwalten.

<div class='shower-mini-clear'></div>

##### Farbe des Standard-Ribbons ändern

<div class='shower-mini'>
<div class='shower-mini-slide mini-blue-ribbon' data-page='13'>
<div class='mini-title'>Blaues Ribbon</div>
<p>Die Form bleibt gleich, aber die Farbe ändert sich.</p>
</div>
</div>

Wenn du nur die Farbe ändern möchtest, kannst du das vorhandene Ribbon behalten und nur die Hintergrundfarbe überschreiben.
Schreibe diese Regel in `styles.css` unter den Kommentar `Eigene CSS-Regeln`:

```css
.slide::after {
    background-color: #4b86c2;
}
```

Du kannst auch die vorhandene Shower-Farbvariable verwenden:

```css
.slide::after {
    background-color: var(--color-blue);
}
```

<div class='shower-mini-clear'></div>

##### Das Ribbon größer oder kleiner machen

Das Standard-Ribbon verwendet die Variable `--ribbon-size`.
In der Vorlage steht sie auf `50px`.
Wenn du das Ribbon größer machen möchtest, kannst du diese Variable überschreiben:

```css
.shower {
    --ribbon-size: 65px;
}
```

Das verändert Breite und Höhe des Ribbons gleichzeitig, weil die Standardregel damit rechnet:

```css
width: var(--ribbon-size);
height: calc(var(--ribbon-size) * 2);
```

<div class='hint'>
Wenn du nur eine Kleinigkeit ändern möchtest, ist es oft besser, eine Variable zu überschreiben, statt die ganze <code>.slide::after</code>-Regel zu kopieren.
</div>

##### Eine runde Seitenzahl verwenden

<div class='shower-mini'>
<div class='shower-mini-slide mini-round-page' data-page='14'>
<div class='mini-title'>Runde Seitenzahl</div>
<p>Hier wird aus dem Ribbon ein Kreis.</p>
</div>
</div>

Das Ribbon muss nicht wie ein Lesezeichen aussehen.
Du kannst daraus auch einen Kreis machen.
Auch diese Regel gehört in `styles.css`:

```css
.slide::after {
    top: 32px;
    left: auto;
    right: 55px;

    display: grid;
    place-items: center;

    width: 68px;
    height: 68px;
    padding-top: 0;

    clip-path: none;
    border-radius: 50%;
    background-color: var(--color-blue);

    line-height: 1;
}
```

Wichtig sind hier vor allem drei Dinge:

- `clip-path: none;` entfernt die Lesezeichenform.
- `border-radius: 50%;` macht aus der Fläche einen Kreis.
- `left: auto; right: 55px;` platziert die Seitenzahl von rechts aus.

<div class='shower-mini-clear'></div>

##### Nur eine kleine Zahl unten rechts anzeigen

<div class='shower-mini'>
<div class='shower-mini-slide mini-plain-page' data-page='15'>
<div class='mini-title'>Unauffällige Seitenzahl</div>
<p>Die Nummer steht nur noch klein unten rechts.</p>
</div>
</div>

Manchmal soll die Seitenzahl sehr unauffällig sein.
Dann kannst du Hintergrund und Form komplett entfernen:

```css
.slide::after {
    top: auto;
    left: auto;
    right: 60px;
    bottom: 32px;

    width: auto;
    height: auto;
    padding-top: 0;

    clip-path: none;
    background: transparent;
    color: var(--color-grey);

    font-size: 28px;
    line-height: 1;
}
```

<div class='shower-mini-clear'></div>

##### Die Seitenzahl auf einer einzelnen Folie ausblenden

In der Starter-Vorlage gibt es dafür schon eine CSS-Regel in `styles.css`:

```css
.slide.nopagenumber::after {
    visibility: hidden;
}
```

Du musst also nur der Folie in `index.html` die zusätzliche Klasse `nopagenumber` geben:

```html
<section class="slide nopagenumber">
    <h2>Startfolie ohne Seitenzahl</h2>
    <p>Auf dieser Folie soll kein Ribbon erscheinen.</p>
</section>
```

Achte darauf, dass beide Klassen im selben `class`-Attribut stehen:

```html
class="slide nopagenumber"
```

`slide` macht den Abschnitt zu einer Folie.
`nopagenumber` blendet nur auf dieser Folie die Seitenzahl aus.

##### Die Seitenzahl überall ausblenden

Wenn du die Seitenzahl **auf allen Folien** ausblenden möchtest, kannst du diese Regel in `styles.css` ergänzen:

```css
.slide::after {
    visibility: hidden;
}
```

<div class='hint'>
Am einfachsten ist es, das Standard-Ribbon erst einmal so zu lassen, wie es ist.
Wenn du später sicherer mit CSS wirst, kannst du sein Aussehen Schritt für Schritt verändern, statt es komplett neu zu bauen.
</div>

<div class='shower-mini-clear'></div>

#### Elemente genau platzieren

<!--
Hier sollten absolute Positionierung oder vorhandene Shower-Hilfsklassen wie place/top/left erklärt werden.
Wichtig: Nur für gezielte Layouts verwenden, nicht als Ersatz für normalen Textfluss.
-->

#### Zwei Spalten verwenden

<!--
Hier sollte ein sehr einfaches Zwei-Spalten-Layout gezeigt werden.
Am besten mit CSS Grid oder Flexbox, aber so reduziert, dass man das Muster kopieren und anpassen kann.
-->

### Mehr Möglichkeiten

<!--
Dieser Block sollte Dinge enthalten, die nicht direkt nötig sind, aber Präsentationen deutlich stärker machen.
Die Beispiele sollten erst kommen, wenn HTML-Struktur und einfaches CSS sitzen.
-->

#### Dinge nacheinander einblenden

<!--
Hier sollte class="next" erklärt werden.
Wichtig: Nur sparsam verwenden; Einblendungen sollen den Vortrag unterstützen und nicht jede Folie unnötig kompliziert machen.
-->

#### Wiederverwendbare CSS-Klassen schreiben

<!--
Hier sollte der Schritt von Inline-Style zu eigener Klasse kommen.
Ziel: Wiederholung vermeiden und den Schülerinnen und Schülern zeigen, dass CSS-Klassen eigene kleine Werkzeuge sind.
-->

#### Häufige Fehler finden

<!--
Hier sollten typische Fehler gesammelt werden: vergessenes schließendes Tag, falscher Dateiname, fehlende Anführungszeichen, CSS-Regel an falscher Stelle.
Sinnvoll wäre eine kleine Checkliste für die Fehlersuche in VS Code und Browser-Vorschau.
-->

### Profi-Tipps

<!--
Dieser Teil sollte Arbeitsweisen behandeln, nicht neue HTML/CSS-Bausteine.
Er ist für die Schülerinnen und Schüler gedacht, die ihre Präsentation sichern, teilen oder veröffentlichen wollen.
-->

#### Änderungen mit Git sichern

<!--
Hier sollte sehr praktisch erklärt werden, wann man einen Commit macht und warum das vor größeren Änderungen hilft.
Ziel: Git als Sicherheitsnetz zeigen, nicht als abstraktes Versionskontrollsystem.
-->

#### Präsentation veröffentlichen

<!--
Hier sollte auf /custom-subdomain hingewiesen werden, damit die Schülerinnen und Schüler ihre Präsentation online stellen können, ohne einen eigenen Server zu brauchen.
-->