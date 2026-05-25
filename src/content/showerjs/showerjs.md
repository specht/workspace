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


/* Shower-artiger Ribbon mit Foliennummer für Mini-Folien.
 * Er orientiert sich an der Standardvorlage: rotes Lesezeichen rechts oben.
 */
.shower-mini-slide[data-page]::after {
    content: attr(data-page);
    position: absolute;
    top: 0;
    right: 40px;
    z-index: 2;

    display: flex;
    align-items: flex-start;
    justify-content: center;

    box-sizing: border-box;
    width: 82px;
    height: 160px;
    padding-top: 20px;

    background: #d80000;
    clip-path: polygon(0 0, 100% 0, 100% calc(100% - 36px), 50% 100%, 0 calc(100% - 36px));
    color: white;

    font-family: 'PT Sans', sans-serif;
    font-size: 42px;
    font-weight: normal;
    line-height: 1;
    text-align: center;
}

.shower-mini-slide.mini-no-ribbon::after {
    display: none;
}

.shower-mini-slide.mini-blue-ribbon::after {
    background: #4b86c2;
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
        const width = container.getBoundingClientRect().width;
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
Zuerst sollte man das Default-Verhalten verstehen, dann kleine CSS-Änderungen ausprobieren: Farbe ändern, auf einzelnen Folien ausblenden oder überall ausblenden.
Wichtig ist dabei, dass man dafür normalerweise kein neues HTML für die Zahl schreibt, sondern das vorhandene ::after-Styling der Vorlage anpasst.
-->

In der Standardvorlage von shower.js erscheint rechts oben automatisch ein rotes Ribbon mit der Foliennummer.
Du musst dafür normalerweise **nichts** in deine Folie schreiben.
Die Vorlage erzeugt dieses Ribbon selbst.

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

Wenn du nur das Aussehen ändern möchtest, kannst du das vorhandene Ribbon mit CSS überschreiben.
Dafür legst du zum Beispiel in deiner Datei `index.html` im `<style>`-Block eine neue Regel für `.slide::after` an:

```css
.slide::after {
    background: #4b86c2;
    color: white;
}
```

So bleibt das normale Seitenzahl-Ribbon erhalten, aber es bekommt eine andere Farbe.
Du kannst auf dieselbe Weise auch Breite, Höhe oder Form verändern, wenn du dich ein wenig weiter an CSS herantraust.

Wenn du das Ribbon **auf genau einer Folie** ausblenden möchtest, gibst du der Folie eine zusätzliche Klasse:

```html
<section class="slide no-page">
    <h2>Startfolie ohne Seitenzahl</h2>
    <p>Auf dieser Folie soll kein Ribbon erscheinen.</p>
</section>
```

Dazu brauchst du diese CSS-Regel:

```css
.no-page::after {
    display: none;
}
```

Wenn du das Ribbon **überall** ausblenden möchtest, reicht sogar eine einzige Regel:

```css
.slide::after {
    display: none;
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