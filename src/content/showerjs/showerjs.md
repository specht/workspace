<div class='meta'>
image: showerjs.webp:0:80
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3,h4">
</div>

<style>
.shower-mini {
    box-sizing: border-box;
    float: right;
    position: relative;

    width: min(28rem, 45%);
    aspect-ratio: 16 / 9;
    margin: 0.2rem 0 1.2rem 1.5rem;
    padding: 0;

    overflow: hidden;
    border: 0;
    border-radius: 10px;
    box-shadow: 0 0.35rem 1.15rem rgba(0, 0, 0, 0.22);

    background: white;
    color: inherit;
    text-decoration: none;
    cursor: zoom-in;

    transition:
        box-shadow 160ms ease,
        filter 160ms ease,
        transform 160ms ease;
}

.shower-mini img {
    display: block;
    width: 100%;
    height: 100%;
    object-fit: cover;
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
    filter: brightness(1.03);
}

.shower-mini:hover::before,
.shower-mini:focus-visible::before {
    box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.26);
}

.shower-mini:focus-visible {
    outline: 0.18rem solid currentColor;
    outline-offset: 0.3rem;
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

    background: white;

    opacity: 0;
    transform: scale(0.96);
    transition:
        opacity 180ms ease,
        transform 180ms ease;
}

.mini-lightbox-content img {
    display: block;
    width: 100%;
    height: 100%;
    object-fit: contain;
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

.aspect-row {
    display: grid;
    grid-template-columns:
        8fr     /* 2:3  */
        9fr     /* 3:4  */
        12fr    /* 1:1  */
        16fr    /* 4:3  */
        21.33fr;/* 16:9 */
    gap: 18px;
    align-items: start;
    margin: 1em 0;
    max-width: 100%;
    overflow-x: auto;
}

.aspect-box {
    margin: 0;
    display: grid;
    min-height: 200px;
}

.aspect-box > div,
.aspect-box figcaption {
    grid-area: 1 / 1;
}

.aspect-box > div {
    width: 100%;
    aspect-ratio: var(--ratio);

    border: 1px solid #75507b;
    border-radius: 12px;
    background:
        linear-gradient(135deg, rgba(117, 80, 123, 0.15), rgba(117, 80, 123, 0.03));
    box-shadow: 0 4px 10px rgba(0, 0, 0, 0.18);
}

.aspect-box figcaption {
    place-self: center;
}

</style>

<script>
document.addEventListener('DOMContentLoaded', () => {
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

    function prepareMiniSlides() {
        document.querySelectorAll('.shower-mini').forEach((mini) => {
            const img = mini.querySelector('img');

            if (!img) return;

            if (!mini.getAttribute('aria-label')) {
                mini.setAttribute(
                    'aria-label',
                    img.alt ? `${img.alt} vergrößern` : 'Beispiel vergrößern'
                );
            }
        });
    }

    function openMiniSlide(mini) {
        const previewImage = mini.querySelector('img');
        if (!previewImage) return;

        const image = document.createElement('img');
        image.src = mini.dataset.full || previewImage.src;
        image.alt = previewImage.alt || '';

        content.replaceChildren(image);
        lightbox.classList.remove('is-visible');

        if (typeof lightbox.showModal === 'function') {
            lightbox.showModal();
        } else {
            lightbox.setAttribute('open', '');
        }

        requestAnimationFrame(() => {
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

    prepareMiniSlides();
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

<button class='shower-mini' type='button'>
    <img src='screenshots/meine-lieblingstiere.webp'>
</button>

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

Eine Folie braucht meistens zuerst eine klare Überschrift.
In der Shower-Vorlage verwendest du dafür normalerweise `<h2>`.

Längere Texte schreibst du nicht direkt lose in die Folie, sondern in Absätze.
Ein Absatz beginnt mit `<p>` und endet mit `</p>`.

<button class='shower-mini' type='button'>
    <img src='screenshots/warum-html.webp'>
</button>


```html
<section class="slide">
    <h2>Warum HTML?</h2>

    <p>HTML beschreibt den Inhalt einer Webseite oder Präsentation.</p>
    <p>CSS legt fest, wie dieser Inhalt aussehen soll.</p>
</section>
```

Die wichtigsten Tags in diesem Beispiel sind:

- `<h2>` ist die Überschrift der Folie.
- `<p>` beginnt einen Textabsatz.
- `</p>` beendet den Textabsatz.
- Zwischen zwei Absätzen entsteht automatisch etwas Abstand.

<div class='hint'>
Schreibe Text auf Folien lieber in kurze Absätze.
Eine Folie ist keine ganze Buchseite. Wenn du zu viel Text auf eine Folie schreibst, wird sie beim Vortragen schnell unübersichtlich.
</div>

##### Überschriften

In einer normalen Folie reicht meistens eine einzige Überschrift:

```html
<h2>Mein Thema</h2>
```

Die Überschrift sollte kurz sagen, worum es auf der Folie geht.
Besser ist oft eine konkrete Überschrift statt nur ein einzelnes Wort.

<div class='row'>
    <div class='col-md-6'>
        <div class='hint bad'>
            <p>Nicht so gut:</p>
            <pre><code class='html'>&lt;h2&gt;HTML&lt;/h2&gt;</code></pre>
        </div>
    </div>

    <div class='col-md-6'>
        <div class='hint good'>
            <p>Besser:</p>
            <pre><code class='html'>&lt;h2&gt;HTML beschreibt die Struktur&lt;/h2&gt;</code></pre>
        </div>
    </div>
</div>

Die zweite Überschrift hilft dem Publikum mehr, weil sie schon eine kleine Aussage enthält.

##### Absätze

Wenn du mehrere Gedanken auf einer Folie hast, kannst du mehrere Absätze verwenden:

<button class='shower-mini' type='button'>
    <img src='screenshots/erste-webseite.webp'>
</button>


```html
<section class="slide">
    <h2>Die erste Webseite</h2>

    <p>Die erste Webseite bestand nur aus Text und Links.</p>

    <p>
        Heute enthalten Webseiten oft Bilder, Videos,
        Menüs und interaktive Bereiche.
    </p>
</section>
```

Du musst nicht selbst leere Zeilen in die Folie schreiben.
Der Abstand entsteht durch die Gestaltung der Vorlage. Dabei ist es egal, ob du hinter `<p>` direkt Text schreibst oder erst eine Zeile frei lässt.

<div class='hint'>
Verwende keine leeren Absätze wie <code>&lt;p&gt;&lt;/p&gt;</code>, nur um Abstand zu erzeugen.
Wenn du später Abstände verändern möchtest, ist CSS dafür der bessere Ort.
</div>

##### Einen Zeilenumbruch erzwingen

Manchmal möchtest du innerhalb eines Absatzes eine neue Zeile beginnen, ohne einen neuen Absatz zu starten.
Dafür gibt es `<br>`.

<button class='shower-mini' type='button'>
    <img src='screenshots/ueber-mich.webp'>
</button>

```html
<section class="slide">
    <h2>Über mich</h2>
    <p>Wer bin ich?</p>
    <p>
        Max Mustermann<br>
        Klasse 9b<br>
        Informatik
    </p>
</section>
```

`<br>` bedeutet: Hier beginnt eine neue Zeile.

Anders als viele andere HTML-Tags hat `<br>` kein eigenes schließendes Tag.
Du schreibst also nicht `</br>`.

<div class='hint'>
Wie du siehst, erhältst du durch <code>&lt;br&gt;</code> nur einen Zeilenumbruch, aber keinen Abstand zwischen den Zeilen wie bei Absätzen. Verwende <code>&lt;br&gt;</code> also nur, wenn du wirklich nur einen Zeilenumbruch brauchst, aber keinen neuen Absatz.
</div>

##### Zu viel Text vermeiden

Diese Folie ist technisch korrekt, aber als Präsentationsfolie wahrscheinlich zu voll:

<button class='shower-mini' type='button'>
    <img src='screenshots/geschichte-des-internets.webp'>
</button>

```html
<section class="slide">
    <h2>Die Geschichte des Internets</h2>

    <p>
        Das Internet entstand aus mehreren technischen Entwicklungen und wurde
        über viele Jahre hinweg immer weiter ausgebaut. Heute benutzen wir es
        für Webseiten, E-Mails, Chats, Videos, Spiele, Cloud-Dienste und viele
        andere Anwendungen.
    </p>
</section>
```

Oft ist es besser, den Text zu kürzen oder auf mehrere Folien zu verteilen:

<button class='shower-mini' type='button'>
    <img src='screenshots/internet-verbindet-computer.webp'>
</button>

```html
<section class="slide">
    <h2>Das Internet verbindet Computer</h2>

    <p>
        Das Internet ist ein riesiges Netzwerk aus vielen kleineren Netzwerken.
    </p>

    <p>
        Darüber können Computer Daten austauschen: Webseiten, Nachrichten,
        Bilder, Videos und vieles mehr.
    </p>
</section>
```

<div class='hint'>
Wenn du beim Vortragen sowieso erklären möchtest, was auf der Folie steht, muss nicht jeder Satz ausgeschrieben werden.
Die Folie soll deinen Vortrag unterstützen, nicht ihn vollständig ersetzen.
</div>

<div class='shower-mini-clear'></div>

#### Listen und Stichpunkte

<!--
Hier sollten <ul>, <ol> und <li> eingeführt werden.
Sinnvoll wäre ein Vergleich zwischen ungeordneter Liste und nummerierter Liste sowie ein Hinweis, dass jeder Listenpunkt ein eigenes <li> braucht.
-->

Listen sind auf Präsentationsfolien sehr praktisch, weil sie Informationen übersichtlich machen.
In HTML gibt es zwei wichtige Arten von Listen:

- `<ul>` erstellt eine Liste mit Stichpunkten.
- `<ol>` erstellt eine nummerierte Liste.
- Jeder einzelne Listenpunkt steht in einem eigenen `<li>`.

`li` steht für »list item«, also »Listenpunkt«.

<button class='shower-mini' type='button'>
    <img src='screenshots/meine-projektidee.webp'>
</button>

```html
<section class="slide">
    <h2>Meine Projektidee</h2>

    <ul>
        <li>🎮 Ein kleines Spiel programmieren</li>
        <li>🧭 Eine Spielfigur bewegen</li>
        <li>💎 Punkte sammeln</li>
        <li>🏁 Ein Ziel erreichen</li>
    </ul>
</section>
```

Die wichtigsten Tags in diesem Beispiel sind:

- `<ul>` beginnt eine Liste mit Stichpunkten.
- `<li>` beginnt einen Listenpunkt.
- `</li>` beendet einen Listenpunkt.
- `</ul>` beendet die ganze Liste.

<div class='hint'>
Jeder Listenpunkt braucht ein eigenes <code>&lt;li&gt;</code>.
Schreibe also nicht alle Punkte in ein einziges <code>&lt;li&gt;</code>, sondern mache für jeden Punkt eine eigene Zeile.
</div>

##### Nummerierte Liste

Eine nummerierte Liste verwendest du, wenn die Reihenfolge wichtig ist:

<button class='shower-mini' type='button'>
    <img src='screenshots/so-startest-du.webp'>
</button>

```html
<section class="slide">
    <h2>So startest du</h2>

    <ol>
        <li>Repository klonen</li>
        <li>index.html öffnen</li>
        <li>Live Server starten</li>
        <li>Folie bearbeiten</li>
    </ol>
</section>
```

Bei `<ol>` setzt der Browser die Zahlen automatisch davor.
Du musst die Zahlen also nicht selbst schreiben.

<div class='row'>
    <div class='col-md-6'>
        <div class='hint bad'>
            <p>Nicht so gut:</p>
            <pre><code class='html'>&lt;ul&gt;
    &lt;li&gt;1. Repository klonen&lt;/li&gt;
    &lt;li&gt;2. index.html öffnen&lt;/li&gt;
    &lt;li&gt;3. Live Server starten&lt;/li&gt;
&lt;/ul&gt;</code></pre>
        </div>
    </div>

    <div class='col-md-6'>
        <div class='hint good'>
            <p>Besser:</p>
            <pre><code class='html'>&lt;ol&gt;
    &lt;li&gt;Repository klonen&lt;/li&gt;
    &lt;li&gt;index.html öffnen&lt;/li&gt;
    &lt;li&gt;Live Server starten&lt;/li&gt;
&lt;/ol&gt;</code></pre>
        </div>
    </div>
</div>

So kann der Browser die Nummerierung selbst übernehmen.
Wenn du später einen Punkt einfügst oder löschst, stimmen die Zahlen automatisch wieder.

##### Listen nicht zu voll machen


Eine Liste auf einer Folie sollte nicht zu lang sein.
Diese Folie ist zu voll und stößt an den unteren Rand der Folie:

<button class='shower-mini' type='button'>
    <img src='screenshots/vorteile-html-praesentationen-uebersicht.webp'>
</button>

```html
<section class="slide">
    <h2>Vorteile von HTML-Präsentationen</h2>

    <ul>
        <li>Laufen im Browser</li>
        <li>Funktionieren auch offline</li>
        <li>Können mit CSS gestaltet werden</li>
        <li>Kann man auf einen Stick kopieren</li>
        <li>Trainieren den Umgang mit Code</li>
        <li>Sind gut versionierbar</li>
        <li>Können veröffentlicht werden</li>
        <li>Funktionieren auf verschiedenen Geräten</li>
    </ul>
</section>
```

Oft ist es besser, nur die wichtigsten Punkte auf die Folie zu schreiben:

<button class='shower-mini' type='button'>
    <img src='screenshots/vorteile-html-praesentationen-kurz.webp'>
</button>

```html
<section class="slide">
    <h2>Vorteile von HTML-Präsentationen</h2>

    <ul>
        <li>Laufen in jedem modernen Browser</li>
        <li>Lassen sich mit CSS frei gestalten</li>
        <li>Trainieren den sicheren Umgang mit Code</li>
    </ul>
</section>
```

<div class='hint'>
Eine gute Folie zeigt nicht alles, was du sagen möchtest.
Sie zeigt die wichtigsten Stichworte, damit dein Publikum dir leichter folgen kann.
</div>

##### Typische Fehler bei Listen

Achte darauf, dass die `<li>`-Tags wirklich innerhalb der Liste stehen:

```html
<ul>
    <li>Erster Punkt</li>
    <li>Zweiter Punkt</li>
    <li>Dritter Punkt</li>
</ul>
```

Nicht so:

```html
<ul>
</ul>
<li>Erster Punkt</li>
<li>Zweiter Punkt</li>
<li>Dritter Punkt</li>
```

Im zweiten Beispiel stehen die Listenpunkte außerhalb der Liste.
Der Browser versucht zwar oft trotzdem, etwas daraus zu machen, aber der HTML-Code ist nicht sauber.

<div class='hint'>
Wenn eine Liste merkwürdig aussieht, prüfe zuerst, ob jedes <code>&lt;li&gt;</code> zwischen dem öffnenden Listen-Tag und dem schließenden Listen-Tag steht.
Also zwischen <code>&lt;ul&gt;</code> und <code>&lt;/ul&gt;</code> oder zwischen <code>&lt;ol&gt;</code> und <code>&lt;/ol&gt;</code>.
</div>

<div class='shower-mini-clear'></div>

#### Wörter hervorheben

<!--
Hier sollten einfache Inline-Tags wie <strong>, <em>, <code>, <sub> und <sup> gezeigt werden.
Ziel: Einzelne Wörter hervorheben, ohne gleich eigenes CSS schreiben zu müssen.
-->

Manchmal möchtest du nicht einen ganzen Absatz verändern, sondern nur einzelne Wörter hervorheben.
Dafür gibt es in HTML kleine Tags, die mitten im Text stehen können.

Solche Tags nennt man **Inline-Tags**, weil sie in einer Zeile im Text mitlaufen.

<button class='shower-mini' type='button'>
    <img src='screenshots/wichtig-html-css.webp'>
</button>

```html
<section class="slide">
    <h2>Wichtig!</h2>

    <p>Mit <strong>HTML</strong> beschreibst du den Inhalt.</p>
    <p>Mit <em>CSS</em> gestaltest du das Aussehen.</p>
    <p>Der Dateiname ist <code>index.html</code>.</p>
</section>
```

Die wichtigsten Inline-Tags sind:

- `<strong>` hebt etwas stark hervor.
- `<em>` betont etwas.
- `<code>` markiert Code, Dateinamen oder Befehle.
- `<sub>` stellt Text tiefer.
- `<sup>` stellt Text höher.

##### Stark hervorheben mit strong

Mit `<strong>` markierst du Wörter, die besonders wichtig sind:

<button class='shower-mini' type='button'>
    <img src='screenshots/speichern-und-einblendungen.webp'>
</button>

```html
<p>
    Speichere deine Datei mit <strong>Strg + S</strong>,
    bevor du die Vorschau überprüfst.
</p>
```

Im Browser wird dieser Text meistens fett dargestellt.
Wichtiger ist aber die Bedeutung: Dieses Wort oder diese Stelle ist besonders wichtig.

##### Betonen mit em

Mit `<em>` betonst du ein Wort oder eine kurze Wortgruppe:

```html
<p>
    Verwende Einblendungen <em>sparsam</em>,
    damit die Präsentation ruhig bleibt.
</p>
```

Im Browser wird der Text meistens kursiv dargestellt.

<div class='hint'>
Verwende Hervorhebungen gezielt.
Wenn auf einer Folie fast alles fett oder kursiv ist, fällt am Ende gar nichts mehr besonders auf.
</div>

##### Code, Dateinamen und Befehle markieren

Mit `<code>` markierst du kurze Stücke Code, Dateinamen, Tags oder Befehle:

```html
<p>
    Öffne die Datei <code>index.html</code>
    und suche nach <code>&lt;section class="slide"&gt;</code>. Verwende dazu <mark>Strg + F</mark>.
</p>
```

Bei echten HTML-Tags im normalen Text musst du die spitzen Klammern ersetzen:

- Statt `<` schreibst du `&lt;`.
- Statt `>` schreibst du `&gt;`.

Darum steht im Beispiel:

```html
<code>&lt;section class="slide"&gt;</code>
```

und nicht:

```html
<code><section class="slide"></code>
```

Im zweiten Beispiel würde der Browser versuchen, wirklich eine neue Folie zu beginnen, und das ist nicht das, was du hier möchtest.

##### Hochgestellte und tiefgestellte Zeichen

Für manche Themen brauchst du hochgestellte oder tiefgestellte Zeichen.
Dafür gibt es `<sup>` und `<sub>`.

<button class='shower-mini' type='button'>
    <img src='screenshots/formeln-im-text.webp'>
</button>

```html
<section class="slide">
    <h2>Formeln im Text</h2>

    <p>Fläche: A = a<sup>2</sup></p>
    <p>Wasser: H<sub>2</sub>O</p>
    <p>Binärzahl: 1010<sub>2</sub></p>
</section>
```

`<sup>` stellt den Text höher.
Das ist praktisch für Potenzen wie `a²`.

`<sub>` stellt den Text tiefer.
Das ist praktisch für chemische Formeln oder Zahlensysteme.

##### Inline-Tags richtig schließen

Auch Inline-Tags müssen meistens wieder geschlossen werden:

```html
<p>Das ist <strong>wichtig</strong>.</p>
```

Nicht so:

```html
<p>Das ist <strong>wichtig.</p>
```

Wenn du das schließende `</strong>` vergisst, kann es passieren, dass viel mehr Text fett wird, als du eigentlich wolltest.

<div class='hint'>
Wenn plötzlich ein ganzer Abschnitt fett, kursiv oder in Code-Schrift erscheint, fehlt wahrscheinlich irgendwo ein schließendes Tag wie <code>&lt;/strong&gt;</code>, <code>&lt;/em&gt;</code> oder <code>&lt;/code&gt;</code>.
</div>

<div class='shower-mini-clear'></div>


#### Formeln mit KaTeX

Manchmal reichen `<sup>` und `<sub>` nicht aus.
Für richtige mathematische Formeln kannst du in der Vorlage LaTeX-Schreibweise verwenden.
Die Darstellung übernimmt KaTeX.

Es gibt zwei typische Arten von Formeln:

- Eine **Inline-Formel** steht mitten im Satz.
- Eine **abgesetzte Formel** steht groß in einer eigenen Zeile.

<button class='shower-mini' type='button'>
    <img src='screenshots/formeln-mit-katex.webp'>
</button>

```html
<section class="slide">
    <h2>Formeln mit KaTeX</h2>

    <p>Die Fläche eines Kreises ist \(A = \pi r^2\).</p>

    <p>Der Satz des Pythagoras:</p>

    <p>$$a^2 + b^2 = c^2$$</p>
</section>
```

Für kurze Formeln im Text verwendest du `\(` und `\)`:

```html
<p>Die Fläche eines Kreises ist \(A = \pi r^2\).</p>
```

Für größere Formeln in einer eigenen Zeile verwendest du doppelte Dollarzeichen:

```html
<p>$$a^2 + b^2 = c^2$$</p>
```

<div class='hint'>
Dieses Tutorial geht davon aus, dass KaTeX in der Vorlage bereits eingerichtet ist.
Du musst also normalerweise nur die Formel in LaTeX-Schreibweise in deine Folie schreiben.
</div>

##### Häufige LaTeX-Schreibweisen

Einige Schreibweisen brauchst du besonders oft:

```text
x^2              hochgestellte 2
H_2O             tiefgestellte 2
\frac{a}{b}      Bruch
\sqrt{x}         Wurzel
\pi              π
\cdot            Malpunkt
\le              kleiner oder gleich
\ge              größer oder gleich
```

Zum Beispiel:

<button class='shower-mini' type='button'>
    <img src='screenshots/weitere-formeln.webp'>
</button>

```html
<section class="slide">
    <h2>Weitere Formeln</h2>

    <p>Eine Wurzel: \(\sqrt{25} = 5\)</p>
    <p>Ein Bruch: \(\frac{1}{2} + \frac{1}{4} = \frac{3}{4}\)</p>
    <p>Eine Ungleichung: \(x \ge 10\)</p>
</section>
```

##### Wenn eine Formel nicht angezeigt wird

Wenn du statt einer schön gesetzten Formel nur den LaTeX-Code siehst, prüfe zuerst diese Punkte:

- Hast du am Anfang und am Ende der Formel `\(` und `\)` geschrieben?
- Bei abgesetzten Formeln: Hast du am Anfang und am Ende jeweils zwei Dollarzeichen geschrieben?
- Hast du geschweifte Klammern richtig geschlossen, zum Beispiel bei `\frac{1}{2}`?
- Hast du einen Backslash `\` vor LaTeX-Befehlen wie `\sqrt`, `\frac` oder `\pi` geschrieben?


<div class='row'>
    <div class='col-md-6'>
        <div class='hint bad'>
            <p>Falsch:</p>
            <pre><code class='html'>&lt;p&gt;Die Fläche ist \(A = \pi r^2.&lt;/p&gt;</code></pre>
        </div>
    </div>

    <div class='col-md-6'>
        <div class='hint good'>
            <p>Richtig:</p>
            <pre><code class='html'>&lt;p&gt;Die Fläche ist \(A = \pi r^2\).&lt;/p&gt;</code></pre>
        </div>
    </div>
</div>

Im falschen Beispiel fehlt das schließende Klammerzeichen `\)`. Dadurch weiß KaTeX nicht, wo die Formel endet.

<div class='hint'>
LaTeX-Formeln sind sehr praktisch, aber sie können eine Folie auch schnell überladen.
Für Präsentationen sind wenige große und gut erklärte Formeln meistens besser als viele kleine Formeln auf einmal.
</div>

<div class='shower-mini-clear'></div>

### Bilder

<!--
Dieser Block sollte Bilder Schritt für Schritt einführen: erst einfügen, dann Größe und Zuschnitt, dann Platzierung, dann Hintergrundbilder und Lesbarkeit.
Die Beispiele sollten möglichst mit denselben Bilddateien funktionieren, die ohnehin im Tutorial vorkommen.
-->

Bilder können eine Präsentation viel anschaulicher machen.
Dabei musst du dich um zwei Dinge kümmern:

1. Du brauchst zuerst eine Bilddatei in deinem Projekt.
2. Danach musst du diese Bilddatei mit HTML in eine Folie einfügen.

#### Bilddateien hinzufügen

<!--
Hier wird erklärt, wie Schülerinnen und Schüler in der browserbasierten VS-Code-Umgebung an Bilddateien kommen:
per Drag & Drop vom eigenen Computer, per Zwischenablage und per wget im Terminal.
-->

Wenn du den File Explorer in VS Code öffnest, siehst du einen Ordner namens `pictures`, in dem alle Bilder liegen sollten, die du in deiner Präsentation verwenden möchtest. Klicke auf eine Bilddatei, um sie in der Vorschau zu sehen:

<img class='full' src='file-explorer.webp'>

Es gibt verschiedene Bilddateiformate, die dir begegnen werden:

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<tr>
<th>Format</th>
<th>Eigenschaften</th>
<th>Dateiendung</th>
</tr>
<tr>
<td><strong>JPEG</strong></td>
<td>Gut für Fotos, weil sie die Dateigröße klein halten</td>
<td><code>.jpg</code> oder <code>.jpeg</code></td>
</tr>
<tr>
<td><strong>PNG</strong></td>
<td>Gut für Grafiken, Logos oder Bilder mit transparentem Hintergrund</td>
<td><code>.png</code></td>
</tr>
<tr>
<td><strong>GIF</strong></td>
<td>Gut für einfache Animationen, aber nicht so gut für Fotos</td>
<td><code>.gif</code></td>
</tr>
<tr>
<td><strong>WebP</strong></td>
<td>Ein modernes Format, das oft kleinere Dateien als JPEG oder PNG erzeugt</td>
<td><code>.webp</code></td>
</tr>
<tr>
<td><strong>SVG</strong></td>
<td>Ein Vektorformat, das sich gut für Logos und Icons eignet, weil es ohne Qualitätsverlust skaliert werden kann</td>
<td><code>.svg</code></td>
</tr>
</table>
</div>

Verwende für Bilddateien am besten einfache Dateinamen:
kleine Buchstaben, keine Leerzeichen, keine Umlaute und keine Sonderzeichen.
Gut sind zum Beispiel <code>fuji.jpg</code>, <code>berlin.webp</code> oder <code>live-at-msg.png</code>.

<div class='row'>
    <div class='col-md-4'>
        <div class='hint good'>
            <p>Gut:</p>
            <pre>fuji.jpg</pre>
        </div>
    </div>

    <div class='col-md-4'>
        <div class='hint good'>
            <p>Auch gut:</p>
            <pre>mount-fuji.webp</pre>
        </div>
    </div>

    <div class='col-md-4'>
        <div class='hint bad'>
            <p>Eher schlecht:</p>
            <pre>Fuji Bild endgültig NEU!!!.JPG</pre>
        </div>
    </div>
</div>

Im Folgenden siehst du drei Möglichkeiten, wie du eine Bilddatei in den Ordner `pictures` bekommen kannst, damit du sie später in deiner Präsentation verwenden kannst.

##### Möglichkeit 1: Bilddatei per Drag & Drop hochladen

Wenn du eine Bilddatei bereits auf deinem Computer hast, kannst du sie in den File Explorer von VS Code ziehen und dort im Verzeichnis `pictures` ablegen:

<img src='drag-and-drop.webp' class='full'>

<div class='hint'>
Egal, wie du dein Bild in den Ordner bekommst: Achte darauf, deinen Bildern einen sinnvollen Dateinamen zu geben. Das erleichtert dir später die Arbeit, wenn du das Bild in deine Folie einfügen möchtest.
</div>

##### Möglichkeit 2: Bild aus der Zwischenablage einfügen

Du kannst auch ein Bild direkt in die Zwischenablage kopieren und dann in den Ordner `pictures` einfügen. Das ist besonders praktisch, wenn du einen Screenshot gemacht hast und diesen direkt verwenden möchtest, du kannst aber auch Bilder aus dem Internet oder von anderen Quellen auf diese Weise in deinen Workspace bekommen.

Um ein Bild aus dem Internet in die Zwischenablage zu kopieren, kannst du mit der rechten Maustaste auf das Bild klicken und »Bild kopieren« oder »Copy Image« auswählen:

<img src="copy-image.webp" class="full">

<!-- https://de.wikipedia.org/wiki/Japan#/media/Datei:Japanese_classroom.jpg -->

Klicke jetzt im File Explorer auf ein Bild im Ordner `pictures` und drücke <span class='key'>Strg</span><span class='key'>V</span>, um das Bild in denselben Ordner einzufügen:

<img src="confirm-paste.webp" class="full">

Du solltest die neue Datei sehen, die automatisch einen Namen wie `image.png` bekommt:

<img src="image-pasted.webp" class="full">

Am unteren Rand siehst du zwei wichtige Informationen:

1. die Auflösung des Bildes (in diesem Fall 1280×1013 Pixel): Achte darauf, dass deine Bilder nicht zu klein sind, damit sie auf der Folie nicht unscharf werden
2. die Dateigröße (in diesem Fall 1,92 MB): Achte darauf, dass deine Bilder nicht zu groß sind, damit deine Präsentation später nicht zu viel Speicherplatz braucht und schnell lädt

<div class='hint'>
1,92 MB ist für ein Bild ziemlich groß – wir merken uns das und kümmern uns gleich darum, das Bild zu verkleinern, damit es besser für die Präsentation geeignet ist.
</div>

Gib der Bilddatei jetzt einen sinnvollen Namen, zum Beispiel `classroom.png`:

<div class='row' style='margin-bottom: 1em;'>
    <div class='col-md-6'>
        <img src="rename-image.webp" class="full">
    </div>
    <div class='col-md-6'>
        <img src="image-renamed.webp" class="full">
    </div>
</div>

<div class='hint think'>
Achte darauf, dass die Dateiendung erhalten bleibt (in diesem Fall <code>.png</code>), damit die Datei später als Bild erkannt wird.
</div>

Dadurch, dass ein Bild aus der Zwischenablage oft als PNG-Datei gespeichert wird, kann es passieren, dass die Dateigröße sehr groß ist, auch wenn die Auflösung nicht besonders hoch ist. Wir können die Datei aber leicht verkleinern, indem wir sie in ein moderneres Format wie WebP umwandeln.

Installiere dazu die Erweiterung »WebP Converter for VSCode«. Anschließend kannst du mit einem Rechtsklick auf die Bilddatei die Option »Convert to WebP« auswählen:

<div class='row' style='margin-bottom: 1em;'>
    <div class='col-md-6'>
        <img src="install-webp-converter.webp" class="full">
    </div>
    <div class='col-md-6'>
        <img src="convert-to-webp.webp" class="full">
    </div>
</div>

Du wirst nun nach der Qualität gefragt, die du verwenden möchtest. »Lossless« bedeutet, dass die Qualität des Bildes erhalten bleibt, aber die Dateigröße trotzdem deutlich kleiner wird – diese Option eignet sich am besten für Grafiken. »Lossy« bedeutet, dass die Qualität des Bildes etwas schlechter wird, aber die Dateigröße noch weiter reduziert wird – diese Option eignet sich am besten für Fotos. Für unser Foto eines Klassenzimmers wählen wir »Lossy« mit einer Qualität von 85%:

<img src="convert-to-webp-step-1.webp" class="full">

Mit einem Klick auf »Finish« wird die neue WebP-Datei erstellt, die deutlich kleiner ist als die ursprüngliche PNG-Datei:

<img src="convert-to-webp-step-2.webp" class="full">

Die neue Datei `classroom.webp` hat jetzt eine Dateigröße von nur 180 kB und sieht immer noch gut aus:

<img src="webp-converted.webp" class="full">

<div class='hint'>
Bilder, die du mit <span class='key'>Strg</span><span class='key'>V</span> einfügst, werden oft als PNG-Dateien gespeichert.
Das ist für Screenshots gut, aber für Fotos manchmal unnötig groß.
Im Profi-Abschnitt unten siehst du, wie du solche Dateien prüfen und umwandeln kannst.
</div>

#### Bilder einfügen

<!--
Hier sollte <img src="..."> erklärt werden.
Wichtig wären Dateinamen, relative Pfade, Dateiendungen und ein kurzer Hinweis auf alt-Text, ohne den Abschnitt zu überfrachten.
-->

Wenn die Bilddatei im richtigen Ordner liegt, kannst du sie mit dem Tag `<img>` in eine Folie einfügen. Angenommen, dein Bild heißt `fuji.jpg` und liegt im Ordner `pictures`.
Dann fügst du es so ein:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-zu-gross.webp'>
</button>

```html
<section class="slide">
    <h2>Dieses Bild ist zu groß</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji vom Shōji-See aus gesehen"
    >
</section>
```

<div class='shower-mini-clear'></div>

<div class='hint'>
Wir ignorieren erst einmal, dass das eingefügte Bild zu groß ist und nicht auf die Folie passt – wir kümmern uns gleich darum.
</div>

Die wichtigsten Teile sind:

- `<img>` fügt ein Bild ein.
- `src="pictures/fuji.jpg"` sagt dem Browser, wo die Bilddatei liegt.
- `alt="Der Fuji vom Shōji-See aus gesehen"` beschreibt das Bild kurz.

<div class='hint'>
Das <code>&lt;img&gt;</code>-Tag hat kein eigenes schließendes Tag.
Du schreibst also normalerweise nicht <code>&lt;/img&gt;</code>.
</div>

##### Der Pfad muss genau stimmen

Der häufigste Fehler bei Bildern ist ein falscher Pfad. Wenn dein Bild im Ordner `pictures` liegt, muss dieser Ordner auch im `src`-Attribut stehen:

<div class='row'>
    <div class='col-md-6'>
        <div class='hint good'>
            <p>Richtig:</p>
            <pre><code class='html'>&lt;img src="pictures/fuji.jpg" alt="Der Fuji"&gt;</code></pre>
        </div>
    </div>

    <div class='col-md-6'>
        <div class='hint bad'>
            <p>Falsch:</p>
            <pre><code class='html'>&lt;img src="Fuji.jpg" alt="Der Fuji"&gt;</code></pre>
        </div>
    </div>
</div>

<div class='hint'>
Du kannst theoretisch auch eine URL zu einem Bild angeben. Man nennt das »Hotlinking« und es birgt einige Probleme, zum Beispiel, dass das Bild plötzlich nicht mehr da ist, wenn die Webseite, von der du es verlinkt hast, offline geht oder das Bild löscht. Es könnte auch sein, dass der Betreiber der Webseite das Hotlinking nicht erlaubt und das Bild deshalb nicht angezeigt wird. Deshalb ist es meistens besser, Bilder in deinen eigenen Ordner zu legen und von dort aus zu verlinken – dann funktioniert deine Präsentation auch offline und du hast die volle Kontrolle über die Bilder, die du verwendest.
</div>

#### Bildgröße anpassen

<!--
Hier sollten typische Größenprobleme gelöst werden: Bild zu groß, Bild zu klein, falsches Seitenverhältnis.
Mögliche Beispiele: width per style, vorhandene Hilfsklassen der Vorlage und object-fit für zugeschnittene Bilder.
-->

Um die Größe eines Bildes anzupassen, kannst du CSS verwenden. Füge dazu ein `style`-Attribut zum `<img>`-Tag hinzu und schreibe die gewünschten CSS-Eigenschaften hinein.

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-hoehe-350px.webp'>
</button>

```html
<section class="slide">
    <h2>Höhe: 350px</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            height: 350px;
        ">
</section>
```

<div class='shower-mini-clear'></div>

In diesem Beispiel wird die Höhe des Bildes auf 350 Pixel festgelegt. Die Breite wird automatisch angepasst, damit das Seitenverhältnis erhalten bleibt. Du kannst auch die Breite festlegen, zum Beispiel mit `width: 250px;`, wodurch sich die Höhe automatisch anpasst.

Falls du die Breite und die Höhe gleichzeitig festlegst, wird dein Bild verzerrt, wenn die angegebenen Werte nicht zum ursprünglichen Seitenverhältnis des Bildes passen:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-breite-250px-und-hoehe-350px.webp'>
</button>

```html
<section class="slide">
    <h2>Größe: 250px &times; 350px</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            width: 250px;
            height: 350px;
        ">
</section>
```

Das Bild wurde zusammegestaucht und der Fuji ist jetzt spitzer als in Wirklichkeit. Um dieses Problem zu lösen, kannst du die CSS-Eigenschaft `object-fit` verwenden, um zu bestimmen, wie das Bild in den vorgegebenen Rahmen passt. Mit `object-fit: cover;` wird das Bild so skaliert, dass es den gesamten Rahmen ausfüllt, ohne das Seitenverhältnis zu verändern. Dabei wird das Bild aber so zugeschnitten, dass es nicht verzerrt wird:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-breite-250px-und-hoehe-350px-cover.webp'>
</button>

```html
<section class="slide">
    <h2>Größe: 250px &times; 350px + Cover</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            width: 250px;
            height: 350px;
            object-fit: cover;
        ">
</section>
```

Wenn du den Fokus des Bildausschnitts anpassen möchtest, kannst du zusätzlich die Eigenschaft `object-position` verwenden. Zum Beispiel mit `object-position: 70% 0%;` wird der Fokus mehr auf die rechte Seite des Bildes gelegt:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-breite-250px-und-hoehe-350px-cover-position.webp'>
</button>

```html
<section class="slide">
    <h2>Größe: 250px &times; 350px + Cover + Position</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            width: 250px;
            height: 350px;
            object-fit: cover;
            object-position: 70% 0%;
        ">
</section>
```

<div class='hint'>
Die beiden Werte bei <code>object-position</code> geben an, wo der Fokus des Bildes liegen soll. Der erste Wert (in diesem Fall 70%) gibt die horizontale Position an, wobei 0% ganz links und 100% ganz rechts bedeutet. Der zweite Wert (in diesem Fall 0%) gibt die vertikale Position an, wobei 0% ganz oben und 100% ganz unten bedeutet. In diesem Fall spielt der zweite Wert keine Rolle, weil das Bild sowieso schon den kompletten vertikalen Platz ausfüllt.
</div>

#### Bilder verschieben

Um das Bild auf der Folie zu verschieben, kannst du die CSS-Eigenschaft `position: relative;` verwenden und dann mit `left`, und `top` angeben, wie weit das Bild von seiner ursprünglichen Position verschoben werden soll:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-nach-rechts.webp'>
</button>

```html
<section class="slide">
    <h2>Verschiebung nach rechts</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            height: 350px;
            position: relative;
            left: 150px;
        ">
</section>
```

<div class='shower-mini-clear'></div>

#### Abgerundete Ecken und Schattierung

Wenn du dein Bild mit abgerundeten Ecken oder einer Schattierung versehen möchtest, kannst du die CSS-Eigenschaft `border-radius` für abgerundete Ecken und `box-shadow` für Schattierung verwenden:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-abgerundete-ecken-und-schattierung.webp'>
</button>

```html
<section class="slide">
    <h2>Abgerundete Ecken + Schattierung</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        style="
            height: 350px;
            position: relative;
            left: 150px;
            border-radius: 10px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.25);
        ">
</section>
```

Die Eigenschaft `border-radius: 10px;` sorgt dafür, dass die Ecken des Bildes mit einem Radius von 10 Pixeln abgerundet werden. Die Eigenschaft `box-shadow: 0 4px 8px rgba(0, 0, 0, 0.25);` fügt dem Bild einen Schatten hinzu, der:

- um 0 Pixel nach rechts verschoben ist (da es eine 0 ist, können wir hier die Einheit `px` weglassen)
- um 4 Pixel nach unten verschoben ist
- eine Unschärfe von 8 Pixeln hat
- eine Farbe von Schwarz mit einer Transparenz von 25% hat (rgba steht für Rot, Grün, Blau und Alpha, wobei Alpha die Transparenz angibt)

#### Bilder zuschneiden

Manchmal möchte man nur einen Teil eines Bildes zeigen und es wichtiger, dass das Bild auf der Folie ein bestimmtes Seitenverhältnis hat, als dass das ganze Bild zu sehen ist.
Verschiedene Seitenverhältnisse eignen sich für verschiedene Zwecke:

<div class="aspect-row">
    <figure class="aspect-box" style="--ratio: 2 / 3;">
        <div></div>
        <figcaption>2:3</figcaption>
    </figure>

    <figure class="aspect-box" style="--ratio: 3 / 4;">
        <div></div>
        <figcaption>3:4</figcaption>
    </figure>

    <figure class="aspect-box" style="--ratio: 1 / 1;">
        <div></div>
        <figcaption>1:1</figcaption>
    </figure>

    <figure class="aspect-box" style="--ratio: 4 / 3;">
        <div></div>
        <figcaption>4:3</figcaption>
    </figure>

    <figure class="aspect-box" style="--ratio: 16 / 9;">
        <div></div>
        <figcaption>16:9</figcaption>
    </figure>
</div>

Wenn du für dein Bild eine Höhe mit `height` angibst, wird die Breite automatisch berechnet. Du kannst diese Berechnung beeinflussen, indem du dein gewünschtes Seitenverhältnis mit `aspect-ratio` angibst, z. B. so:

```html
<img
    src="pictures/fuji.jpg"
    alt="Der Fuji"
    style="
        height: 170px;
        aspect-ratio: 2/3;
    ">
```

Dabei wird die Höhe auf 170 Pixel festgelegt, und die Breite wird so berechnet, dass das Seitenverhältnis von 2:3 eingehalten wird. Vergiss nicht, `object-fit: cover;` zu verwenden, damit dein Bild nicht verzerrt wird, wenn das Seitenverhältnis nicht zum ursprünglichen Seitenverhältnis des Bildes passt.

<button class='shower-mini' type='button'>
    <img src='screenshots/feste-seitenverhaeltnisse.webp'>
</button>

```html
<section class="slide">
    <h2>Feste Seitenverhältnisse</h2>

    <div style="text-align: center;">

        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                height: 170px;
                aspect-ratio: 2/3;
                object-fit: cover;
                border-radius: 10px;
            ">
        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                height: 160px;
                aspect-ratio: 3/4;
                object-fit: cover;
                border-radius: 10px;
            ">
        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                height: 150px;
                aspect-ratio: 1/1;
                object-fit: cover;
                border-radius: 10px;
            ">
        <br>
        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                height: 150px;
                aspect-ratio: 4/3;
                object-fit: cover;
                border-radius: 10px;
            ">
        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                height: 150px;
                aspect-ratio: 16/9;
                object-fit: cover;
                border-radius: 10px;
            ">
    </div>
</section>
```

#### Bild im Hintergrund

Du kannst ein Bild auch als Hintergrund verwenden, indem du ihm die CSS-Klasse `cover` zuweist:

<button class='shower-mini' type='button'>
    <img src='screenshots/mount-fuji-hintergrund.webp'>
</button>

```html
<section class="slide bright-text">
    <h2>Mount Fuji im Hintergrund</h2>

    <img
        src="pictures/fuji.jpg"
        alt="Der Fuji"
        class="cover"
    >

    <p style="margin-top: 350px;">
        Text auf Bildern ist schwerer lesbar.
        Achte auf ausreichenden Kontrast!
    </p>
</section>
```

In dieser Folie haben wir außerdem der Folie selbst die Klasse `bright-text` zugewiesen, damit der Text auf dem dunklen Bild besser lesbar ist. Im Absatz haben wir außerdem einen großen Abstand nach oben mit `margin-top: 350px;` hinzugefügt, damit der Text das Bild nicht so stört und besser lesbar ist.

#### Text und Bild nebeneinander

Häufig möchte man ein Bild zeigen und daneben noch etwas Text. Um diesen Effekt zu erreichen, kannst du ein `<div>` mit der CSS-Klasse `side-by-side` verwenden, das zwei Kinder hat, die nebeneinander angeordnet werden:

<button class='shower-mini' type='button'>
    <img src='screenshots/text-links-bild-rechts.webp'>
</button>

```html
<section class="slide">
    <h2>Text links, Bild rechts</h2>

    <div class="side-by-side">
        <div>
            <p>
                Lege die Größe deines Bildes mit CSS fest, und der Rest
                des Platzes wird automatisch für den Text genutzt.
            </p>
            <p>
                Tipp: Verschiebe das <code>&lt;img&gt;</code> vor das
                <code>&lt;div&gt;</code>, um das Bild auf der linken
                Seite zu platzieren.
            </p>
        </div>
        <img src="pictures/fuji.jpg" alt="Der Fuji"
            style="
                height: 350px;
                aspect-ratio: 3/4;
                object-fit: cover;
                border-radius: 10px;
            ">
    </div>
    <figcaption class="copyright right">
        Bild: 名古屋太郎, CC BY-SA 3.0, Wikimedia Commons<br>
        https://de.wikipedia.org/wiki/Datei:Kodaki_fuji_frm_shojinko.jpg
    </figcaption>
</section>
```

Im Beispiel ist das erste Kind ein `<div>`, das zwei Absätze mit Text enthält, und das zweite Kind ist ein `<img>`, das das Bild enthält. Das `<div>` und das `<img>` werden automatisch nebeneinander angeordnet.

<div class='shower-mini-clear'></div>

Wenn du die Reihenfolge der Kinder tauschst, wird das Bild auf der linken Seite und der Text auf der rechten Seite angezeigt:

<button class='shower-mini' type='button'>
    <img src='screenshots/bild-links-text-rechts.webp'>
</button>

```html
<section class="slide">
    <h2>Bild links, Text rechts</h2>

    <div class="side-by-side">
        <img src="pictures/fuji.jpg" alt="Der Fuji"
            style="
                height: 350px;
                aspect-ratio: 3/4;
                object-fit: cover;
                border-radius: 10px;
            ">
        <div>
            <p>
                Lege die Größe deines Bildes mit CSS fest, und der Rest
                des Platzes wird automatisch für den Text genutzt.
            </p>
            <p>
                Tipp: Verschiebe das <code>&lt;img&gt;</code> vor das
                <code>&lt;div&gt;</code>, um das Bild auf der linken
                Seite zu platzieren.
            </p>
        </div>
    </div>
    <figcaption class="copyright right">
        Bild: 名古屋太郎, CC BY-SA 3.0, Wikimedia Commons<br>
        https://de.wikipedia.org/wiki/Datei:Kodaki_fuji_frm_shojinko.jpg
    </figcaption>
</section>
```

<div class='shower-mini-clear'></div>

<div class='hint'>
Wozu benötigen wir das <code>&lt;div&gt;</code> um die Absätze? Wenn beide Absätze direkte Kinder des <code>&lt;div class="side-by-side"&gt;</code> wären, würden beide Absätze nebeneinander (»side-by-side«) angeordnet werden, was nicht das gewünschte Ergebnis wäre. Das innere <code>&lt;div&gt;</code> sorgt dafür, dass die beiden Absätze als eine Einheit behandelt werden und innerhalb dieses <code>&lt;div&gt;</code> ganz normal untereinander angeordnet werden.
</div>

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

Mit CSS kannst du die Farben deiner Folien verändern.
Am Anfang sind vor allem zwei Eigenschaften wichtig:

- `color` ändert die Textfarbe.
- `background` ändert den Hintergrund.

Du kannst Farben zum Beispiel mit Farbnamen oder mit Farbcodes angeben:

```css
color: red;
color: white;
color: #75507b;
background: #f3e5f5;
```

Farbcodes mit `#` nennt man **Hex-Farben**.
Sie sind besonders praktisch, weil du damit sehr genaue Farben auswählen kannst.

<div class='hint'>
Achte bei Farben immer auf den Kontrast.
Text sollte sich deutlich vom Hintergrund abheben.
Schwarzer Text auf weißem Hintergrund ist gut lesbar.
Hellgrauer Text auf weißem Hintergrund sieht vielleicht schick aus, ist aber oft schlecht lesbar.
</div>

##### Die Textfarbe eines Elements ändern

Wenn du nur ein einzelnes Element verändern möchtest, kannst du direkt ein `style`-Attribut verwenden.

<button class='shower-mini' type='button'>
    <img src='screenshots/textfarbe-aendern.webp'>
</button>

```html
<section class="slide">
    <h2 style="color: #75507b;">Farben ändern</h2>

    <p>
        Diese Folie hat eine farbige Überschrift.
    </p>

    <p style="color: #4b86c2;">
        Dieser Absatz ist blau.
    </p>
</section>
```

In diesem Beispiel bekommt die Überschrift eine violette Textfarbe:

```css
color: #75507b;
```

Der zweite Absatz bekommt eine blaue Textfarbe:

```css
color: #4b86c2;
```

<div class='hint'>
Das <code>style</code>-Attribut ist praktisch zum Ausprobieren.
Wenn du dieselbe Gestaltung aber auf mehreren Folien verwenden möchtest, ist eine eigene CSS-Klasse meistens besser.
</div>

<div class='shower-mini-clear'></div>

##### Einen Hintergrund färben

Mit `background` kannst du den Hintergrund eines Elements verändern.
Das funktioniert zum Beispiel gut bei einer kleinen Infokarte.

<button class='shower-mini' type='button'>
    <img src='screenshots/farbige-infokarte.webp'>
</button>

```html
<section class="slide">
    <h2>Eine farbige Infokarte</h2>

    <div style="
        background: #f3e5f5;
        color: #3b2540;
        padding: 28px;
        border-radius: 16px;
    ">
        <strong>Merke:</strong><br>
        Mit <code>background</code> färbst du den Hintergrund.
        Mit <code>color</code> färbst du den Text.
    </div>
</section>
```

Die Infokarte bekommt hier einen hellvioletten Hintergrund:

```css
background: #f3e5f5;
```

Die Textfarbe wird dunkelviolett:

```css
color: #3b2540;
```

<div class='hint'>
Wenn du einen farbigen Hintergrund verwendest, solltest du meistens auch die Textfarbe bewusst festlegen.
So verhinderst du, dass der Text schlecht lesbar wird.
</div>

<div class='shower-mini-clear'></div>

##### Die ganze Folie färben

Du kannst auch der ganzen Folie eine Hintergrundfarbe geben.
Dazu setzt du die Farbe direkt auf das `<section>`-Element.

<button class='shower-mini' type='button'>
    <img src='screenshots/dunkle-folie.webp'>
</button>

```html
<section class="slide" style="background: #2e3440; color: white;">
    <h2 style="color: white;">Eine dunkle Folie</h2>

    <p>
        Auf einem dunklen Hintergrund braucht der Text eine helle Farbe.
    </p>
</section>
```

In diesem Beispiel bekommt die gesamte Folie einen dunklen Hintergrund:

```css
background: #2e3440;
```

Gleichzeitig wird die Textfarbe auf Weiß gesetzt:

```css
color: white;
```

Da dies auf die Überschrift nicht automatisch angewendet wird, muss auch hier die Textfarbe explizit mit `color: white;` festgelegt werden.

<div class='hint'>
Wenn du eine ganze Folie dunkel färbst, prüfe auch Links, Code-Stellen und andere besondere Textelemente.
Manche Farben, die auf weißem Hintergrund gut aussehen, sind auf dunklem Hintergrund schwer lesbar.
</div>

<div class='shower-mini-clear'></div>

#### Schriftgrößen ändern

<!--
Hier sollten font-size und eventuell line-height gezeigt werden.
Wichtig wäre ein Hinweis, dass größere Schrift oft besser ist als mehr Text und dass zu viele verschiedene Größen unruhig wirken.
-->

Mit CSS kannst du auch die Größe von Text verändern.
Dafür verwendest du die Eigenschaft `font-size`.

```css
font-size: 36px;
```

<div class='hint'>
Bei Präsentationen ist größere Schrift oft besser als mehr Text.
Wenn du merkst, dass du die Schrift immer kleiner machen musst, steht wahrscheinlich zu viel auf der Folie.
Teile den Inhalt dann lieber auf mehrere Folien auf.
</div>

##### Einen Absatz größer machen

Wenn du nur ein einzelnes Element verändern möchtest, kannst du wieder ein `style`-Attribut verwenden.

<button class='shower-mini' type='button'>
    <img src='screenshots/grosser-absatz.webp'>
</button>

```html
<section class="slide">
    <h2>Schriftgrößen ändern</h2>

    <p>
        Dieser Absatz hat die normale Schriftgröße.
    </p>

    <p style="font-size: 42px;">
        Dieser Absatz ist größer.
    </p>
</section>
```

Die entscheidende Zeile ist:

```css
font-size: 42px;
```

Damit wird nur dieser eine Absatz größer dargestellt.
Die Überschrift und der erste Absatz bleiben unverändert.

<div class='hint'>
Verändere Schriftgrößen gezielt.
Wenn jeder Absatz eine andere Größe hat, wirkt die Folie schnell unruhig.
</div>

<div class='shower-mini-clear'></div>

##### Eine besonders große Aussage

Manchmal soll eine Folie nur eine wichtige Aussage zeigen.
Dann darf der Text ruhig deutlich größer sein.

<button class='shower-mini' type='button'>
    <img src='screenshots/grosse-aussage.webp'>
</button>

```html
<section class="slide">
    <h2>Merksatz</h2>

    <p style="
        font-size: 56px;
        line-height: 1.15;
    ">
        Eine Folie soll deinen Vortrag unterstützen,
        nicht ersetzen.
    </p>
</section>
```

Hier werden zwei Eigenschaften verwendet:

- `font-size: 56px;` macht den Text groß.
- `line-height: 1.15;` legt den Abstand zwischen den Zeilen fest.

`line-height` bedeutet **Zeilenhöhe**.
Bei großem Text ist es oft sinnvoll, die Zeilenhöhe etwas kleiner zu machen, damit die Zeilen optisch zusammengehören.

<div class='hint'>
Wenn großer Text über mehrere Zeilen läuft, prüfe immer den Zeilenabstand.
Zu viel Abstand lässt die Zeilen auseinanderfallen.
Zu wenig Abstand macht den Text schwer lesbar.
</div>

<div class='shower-mini-clear'></div>

##### Kleine Zusatzinformationen

Manchmal brauchst du eine kleine Zusatzinformation, zum Beispiel eine Quelle, einen Hinweis oder eine kurze Bemerkung.
Dann kannst du die Schrift etwas kleiner machen.

<button class='shower-mini' type='button'>
    <img src='screenshots/kleiner-hinweis.webp'>
</button>

```html
<section class="slide">
    <h2>Das Internet verbindet Computer</h2>

    <p>
        Das Internet ist ein riesiges Netzwerk aus vielen
        kleineren Netzwerken.
    </p>

    <p style="
        font-size: 80%;
        color: #666666;
    ">
        Hinweis: Ein Netzwerk verbindet Geräte, damit sie Daten austauschen können.
    </p>
</section>
```

Der Hinweis ist kleiner und etwas heller.
Dadurch erkennt man sofort:
Das ist nicht die Hauptaussage der Folie, sondern eine Zusatzinformation.

<div class='hint'>
Mache wichtige Informationen nicht zu klein.
Kleine Schrift eignet sich für Zusatzinformationen, aber nicht für Inhalte, die alle im Raum sicher lesen können sollen.
</div>

<div class='shower-mini-clear'></div>

<!--
#### Schriftarten ändern
-->

<!--
Hier sollte font-family erklärt werden, aber eher vorsichtig.
Sinnvoll wäre: vorhandene Schriftarten nutzen, nicht zu viele Schriftarten mischen, Überschriften und normalen Text unterscheiden.
-->

<!--
Mit CSS kannst du nicht nur Farben und Schriftgrößen ändern, sondern auch die Schriftart.
Dafür verwendest du die Eigenschaft `font-family`.

```css
font-family: serif;
font-family: sans-serif;
font-family: monospace;
```

Es gibt einige allgemeine Schriftfamilien, die fast jeder Browser kennt:

- `serif` steht für eine Schrift mit kleinen Endstrichen, ähnlich wie in vielen Büchern.
- `sans-serif` steht für eine Schrift ohne Endstriche, oft klar und modern.
- `monospace` steht für eine Schrift, bei der alle Zeichen gleich breit sind. Das ist typisch für Code.

<button class='shower-mini' type='button'>
    <img src='screenshots/schriftfamilien.webp'>
</button>

```html
<section class="slide">
    <h2>Schriftfamilien</h2>

    <p style="font-family: serif;">
        Dieser Text verwendet eine Serifenschrift.
    </p>

    <p style="font-family: sans-serif;">
        Dieser Text verwendet eine serifenlose Schrift.
    </p>

    <p style="font-family: monospace;">
        Dieser Text verwendet eine Schrift mit gleich breiten Zeichen.
    </p>
</section>
```

<div class='hint'>
Verwende nicht zu viele verschiedene Schriftarten in einer Präsentation.
Eine Schrift für Überschriften und eine Schrift für normalen Text reichen meistens völlig aus.
</div>

<div class='shower-mini-clear'></div>

##### Eine Schriftart herunterladen

Wenn du eine besondere Schriftart verwenden möchtest, muss die Schriftdatei in deinem Projekt liegen.
Sonst funktioniert die Präsentation vielleicht nur auf deinem Computer, aber nicht auf einem anderen Gerät.

Eine einfache Möglichkeit ist diese:

1. Suche auf `fonts.google.com` nach einer passenden Schriftart.
2. Merke dir den Namen der Schriftart, zum Beispiel `Atkinson Hyperlegible`.
3. Öffne `gwfh.mranftl.com`.
4. Suche dort nach derselben Schriftart.
5. Wähle die benötigten Schriftschnitte aus, zum Beispiel `regular` und `700`.
6. Lade die ZIP-Datei mit den Schriftdateien herunter.
7. Kopiere die Schriftdateien in einen Ordner `fonts` in deinem Projekt.
8. Kopiere den CSS-Code in deine Datei `styles.css`.

Dein Projekt kann danach zum Beispiel so aussehen:

```text
shower.js/
├── index.html
├── styles.css
├── fonts/
│   ├── atkinson-hyperlegible-v11-latin-regular.woff2
│   └── atkinson-hyperlegible-v11-latin-700.woff2
└── pictures/
```

<div class='hint'>
Du musst dafür nicht unbedingt Google Fonts verwenden.
Wenn du eine Schriftdatei aus einer anderen Quelle hast, kannst du sie genauso einbinden.
Achte aber darauf, dass du die Schrift auch verwenden darfst und dass die Lizenz zu deinem Projekt passt.
</div>

##### Eine Schriftart mit @font-face einbinden

Damit der Browser deine heruntergeladene Schriftdatei kennt, schreibst du in `styles.css` eine `@font-face`-Regel.

Diese Regel gehört in `styles.css`, nicht direkt in eine Folie:

```css
@font-face {
    font-family: 'Atkinson Hyperlegible';
    font-style: normal;
    font-weight: 400;
    font-display: swap;
    src: url('fonts/atkinson-hyperlegible-v11-latin-regular.woff2') format('woff2');
}

@font-face {
    font-family: 'Atkinson Hyperlegible';
    font-style: normal;
    font-weight: 700;
    font-display: swap;
    src: url('fonts/atkinson-hyperlegible-v11-latin-700.woff2') format('woff2');
}
```

Die wichtigsten Teile sind:

- `font-family` legt den Namen fest, den du später im CSS verwendest.
- `font-weight: 400;` steht für normale Schrift.
- `font-weight: 700;` steht für fette Schrift.
- `src` zeigt auf die Schriftdatei im Ordner `fonts`.

<div class='hint'>
Der Pfad in <code>url(...)</code> muss genau stimmen.
Wenn deine Schriftdateien im Ordner <code>fonts</code> liegen, muss der Pfad auch mit <code>fonts/</code> beginnen.
</div>

##### Die Schrift für normalen Text ändern

Wenn die Schrift eingebunden ist, kannst du sie verwenden.
Normaler Text steht auf deinen Folien meistens in Absätzen und Listen.
Darum solltest du die Schrift mindestens für `<p>` und `<li>` festlegen.

Schreibe diese Regel in `styles.css`:

```css
.slide p,
.slide li {
    font-family: 'Atkinson Hyperlegible', sans-serif;
}
```

Damit verwenden Absätze und Listenpunkte auf deinen Folien die neue Schriftart.

<button class='shower-mini' type='button'>
    <img src='screenshots/schrift-fuer-text.webp'>
</button>

```html
<section class="slide">
    <h2>Neue Schrift für Text</h2>

    <p>
        Dieser Absatz verwendet die neue Schriftart.
    </p>

    <ul>
        <li>Auch dieser Listenpunkt verwendet die neue Schriftart.</li>
        <li>Das funktioniert, weil wir auch <code>.slide li</code> gestaltet haben.</li>
    </ul>
</section>
```

Die Regel gilt nicht nur für einen einzelnen Absatz, sondern für alle Absätze und Listenpunkte in deinen Folien.

<div class='hint'>
Wenn du nur <code>.slide p</code> gestaltest, ändern sich Listenpunkte nicht unbedingt mit.
Darum ist <code>.slide li</code> hier wichtig.
</div>

<div class='shower-mini-clear'></div>

##### Die Schrift für Überschriften ändern

Überschriften kannst du getrennt vom normalen Text gestalten.
Dafür verwendest du `h2`.

```css
.slide h2 {
    font-family: 'Atkinson Hyperlegible', sans-serif;
}
```

Dann verwenden auch die Folienüberschriften die neue Schriftart.

<button class='shower-mini' type='button'>
    <img src='screenshots/schrift-fuer-ueberschriften.webp'>
</button>

```html
<section class="slide">
    <h2>Neue Schrift für Überschriften</h2>

    <p>
        Die Überschrift und der Text können dieselbe Schriftart verwenden.
    </p>

    <ul>
        <li>Das wirkt ruhig und einheitlich.</li>
        <li>Für viele Präsentationen ist das eine gute Wahl.</li>
    </ul>
</section>
```

<div class='hint'>
Eine einheitliche Schriftart wirkt meistens ruhiger als viele verschiedene Schriften.
Wenn du gerade erst anfängst, verwende lieber eine Schriftart für alles.
</div>

<div class='shower-mini-clear'></div>

##### Überschrift und Text unterschiedlich gestalten

Du kannst aber auch eine Schrift für Überschriften und eine andere Schrift für normalen Text verwenden.
Dafür brauchst du zwei eingebundene Schriftarten.

Zum Beispiel könntest du in `styles.css` zwei Schriften einbinden:

```css
@font-face {
    font-family: 'Bebas Neue';
    font-style: normal;
    font-weight: 400;
    font-display: swap;
    src: url('fonts/bebas-neue-v14-latin-regular.woff2') format('woff2');
}

@font-face {
    font-family: 'Atkinson Hyperlegible';
    font-style: normal;
    font-weight: 400;
    font-display: swap;
    src: url('fonts/atkinson-hyperlegible-v11-latin-regular.woff2') format('woff2');
}
```

Danach kannst du sie so verwenden:

```css
.slide h2 {
    font-family: 'Bebas Neue', sans-serif;
}

.slide p,
.slide li {
    font-family: 'Atkinson Hyperlegible', sans-serif;
}
```

<button class='shower-mini' type='button'>
    <img src='screenshots/zwei-schriftarten.webp'>
</button>

```html
<section class="slide">
    <h2>Zwei Schriftarten</h2>

    <p>
        Die Überschrift verwendet eine auffälligere Schrift.
        Der normale Text bleibt gut lesbar.
    </p>

    <ul>
        <li>Überschriften dürfen Charakter haben.</li>
        <li>Fließtext und Listen müssen vor allem gut lesbar sein.</li>
    </ul>
</section>
```

<div class='hint'>
Auffällige Schriften eignen sich oft besser für kurze Überschriften als für lange Texte.
Für Absätze und Listen solltest du eine gut lesbare Schrift verwenden.
</div>

<div class='shower-mini-clear'></div>

##### Wenn die Schrift nicht angezeigt wird

Wenn deine neue Schrift nicht erscheint, prüfe zuerst diese Punkte:

- Liegt die Schriftdatei wirklich im Ordner `fonts`?
- Stimmt der Dateiname genau, auch Groß- und Kleinschreibung?
- Stimmt der Pfad in `url(...)`?
- Steht die `@font-face`-Regel in `styles.css`?
- Verwendest du bei `font-family` genau denselben Namen wie in `@font-face`?
- Hast du die Datei gespeichert und die Vorschau neu geladen?

<div class='row'>
    <div class='col-md-6'>
        <div class='hint bad'>
            <p>Nicht so gut:</p>
            <pre><code class='css'>@font-face {
    font-family: 'Atkinson Hyperlegible';
    src: url('atkinson-hyperlegible.woff2') format('woff2');
}</code></pre>
        </div>
    </div>

    <div class='col-md-6'>
        <div class='hint good'>
            <p>Besser:</p>
            <pre><code class='css'>@font-face {
    font-family: 'Atkinson Hyperlegible';
    src: url('fonts/atkinson-hyperlegible.woff2') format('woff2');
}</code></pre>
        </div>
    </div>
</div>

Im ersten Beispiel fehlt der Ordner `fonts/` im Pfad.
Wenn die Datei im Ordner `fonts` liegt, muss dieser Ordner auch in `url(...)` stehen.

<div class='hint'>
Wenn du eine Schriftart testest, ändere zuerst nur eine einzige Stelle, zum Beispiel <code>.slide h2</code>.
Wenn das funktioniert, kannst du danach Absätze und Listen anpassen.
</div>

<div class='shower-mini-clear'></div>
-->

#### Seitenzahl-Ribbon ändern oder ausblenden

<!--
Hier soll erklärt werden, dass die Standardvorlage das rote Seitenzahl-Ribbon schon automatisch mitbringt.
Wichtig ist jetzt besonders: In der Starter-Vorlage steht dafür kein <style>-Block in index.html, sondern die eigenen Regeln gehören in styles.css.
Zuerst sollte man das Default-Verhalten verstehen, dann kleine CSS-Änderungen ausprobieren: Farbe ändern, eine andere Form verwenden, die Seitenzahl an eine andere Stelle setzen oder sie ausblenden.
Dabei sollte klar bleiben, dass man normalerweise kein neues HTML für die Zahl schreibt, sondern das vorhandene ::after-Styling der Vorlage anpasst.
-->

In der Standardvorlage von shower.js erscheint rechts oben automatisch ein rotes Ribbon mit der Foliennummer.
Du musst dafür normalerweise nichts in deine Folie schreiben.
Die Vorlage erzeugt dieses Ribbon selbst mit Hilfe dieser CSS-Regel:

<button class='shower-mini' type='button'>
    <img src='screenshots/ausflug-nach-berlin.webp'>
</button>

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

Wenn du nur die Farbe ändern möchtest, kannst du das vorhandene Ribbon behalten und nur die Hintergrundfarbe überschreiben.
Schreibe diese Regel in `styles.css` unter den Kommentar `Eigene CSS-Regeln`:

<button class='shower-mini' type='button'>
    <img src='screenshots/blaues-ribbon.webp'>
</button>

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

##### Eine runde Seitenzahl verwenden

Das Ribbon muss nicht wie ein Lesezeichen aussehen.
Du kannst daraus auch einen Kreis machen.
Auch diese Regel gehört in `styles.css`:

<button class='shower-mini' type='button'>
    <img src='screenshots/runde-seitenzahl.webp'>
</button>

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
    background-color: #4aa03f;

    line-height: 1;
}
```

Wichtig sind hier vor allem drei Dinge:

- `clip-path: none;` entfernt die Lesezeichenform.
- `border-radius: 50%;` macht aus der Fläche einen Kreis.
- `left: auto; right: 55px;` platziert die Seitenzahl von rechts aus.

<div class='shower-mini-clear'></div>

##### Nur eine kleine Zahl unten rechts anzeigen

Manchmal soll die Seitenzahl sehr unauffällig sein.
Dann kannst du Hintergrund und Form komplett entfernen:

<button class='shower-mini' type='button'>
    <img src='screenshots/unauffaellige-seitenzahl.webp'>
</button>

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

    font-size: 20px;
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

<button class='shower-mini' type='button'>
    <img src='screenshots/keine-seitenzahl.webp'>
</button>

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

<div class='shower-mini-clear'></div>

#### Elemente genau platzieren

<!--
Hier sollten relative und absolute Positionierung erklärt werden.
Wichtig: Nur für gezielte Layouts verwenden, nicht als Ersatz für normalen Textfluss.
-->

Normalerweise ordnet der Browser die Elemente einer Folie automatisch untereinander an:
erst die Überschrift, dann Absätze, Listen, Bilder und so weiter.
Das ist meistens gut, weil die Folie dadurch ordentlich bleibt.

Manchmal möchtest du aber ein Element ganz gezielt an eine bestimmte Stelle setzen:
zum Beispiel eine kleine Infobox unten rechts, ein Logo in eine Ecke oder eine Beschriftung direkt auf ein Bild.

Dafür gibt es in CSS die Eigenschaft `position`.

Es gibt zwei Arten, die du dafür besonders gut gebrauchen kannst:

- `position: relative;` verschiebt ein Element von seiner normalen Position aus.
- `position: absolute;` platziert ein Element frei auf der Folie.

<div class='hint'>
Verwende genaue Positionierung sparsam.
Für normale Inhalte sind Absätze, Listen, Bilder nebeneinander oder Spalten meistens besser.
Genaue Positionierung ist eher für besondere Elemente gedacht: Logos, Hinweise, Pfeile, kleine Infokarten oder Beschriftungen.
</div>

##### Ein Element leicht verschieben

Mit `position: relative;` bleibt ein Element grundsätzlich an seiner normalen Stelle, wird aber optisch verschoben.

<button class='shower-mini' type='button'>
    <img src='screenshots/element-relativ-verschoben.webp'>
</button>

```html
<section class="slide">
    <h2>Relative Positionierung</h2>

    <p>Dieser Absatz steht ganz normal auf der Folie.</p>

    <p style="
        position: relative;
        left: 120px;
        top: 40px;
    ">
        Dieser Absatz wurde verschoben.
    </p>
</section>
```

Die wichtigsten Eigenschaften sind:

- `position: relative;` erlaubt das Verschieben des Elements.
- `left: 120px;` verschiebt das Element 120 Pixel nach rechts.
- `top: 40px;` verschiebt das Element 40 Pixel nach unten.

<div class='hint'>
Bei <code>position: relative;</code> bleibt der ursprüngliche Platz des Elements erhalten.
Andere Elemente tun also so, als wäre das Element noch an seiner alten Position.
Deshalb eignet sich relative Positionierung nur für kleine Korrekturen.
</div>

<div class='shower-mini-clear'></div>

##### Ein Element frei auf der Folie platzieren

Mit `position: absolute;` kannst du ein Element frei auf der Folie platzieren.
Das Element wird dann aus dem normalen Textfluss herausgenommen.
Andere Elemente machen keinen Platz mehr dafür.

<button class='shower-mini' type='button'>
    <img src='screenshots/infokarte-absolute-position.webp'>
</button>

```html
<section class="slide">
    <h2>Absolute Positionierung</h2>

    <p>
        Dieser normale Text bleibt oben auf der Folie.
        Die Infokarte wird unabhängig davon unten rechts platziert.
    </p>

    <div style="
        position: absolute;
        right: 70px;
        bottom: 60px;

        width: 320px;
        padding: 24px;

        border-radius: 16px;
        background: #ffe88b;
        box-shadow: 0 6px 16px rgba(0, 0, 0, 0.2);
    ">
        <strong>Merke:</strong><br>
        Absolute Positionierung ist gut für besondere Elemente.
    </div>
</section>
```

In diesem Beispiel wird die Infokarte unten rechts platziert:

- `position: absolute;` nimmt die Infokarte aus dem normalen Textfluss.
- `right: 70px;` bedeutet: 70 Pixel Abstand vom rechten Rand der Folie.
- `bottom: 60px;` bedeutet: 60 Pixel Abstand vom unteren Rand der Folie.
- `width: 320px;` legt die Breite der Infokarte fest.
- `padding: 24px;` erzeugt Innenabstand innerhalb der Infokarte.

<div class='hint'>
Bei absoluter Positionierung verwendest du meistens zwei Richtungsangaben:
zum Beispiel <code>left</code> und <code>top</code> oder <code>right</code> und <code>bottom</code>.
So ist klar, von welcher Ecke aus das Element platziert wird.
</div>

<div class='shower-mini-clear'></div>

##### Die vier Richtungen

Du kannst ein absolut positioniertes Element von verschiedenen Seiten aus platzieren:

```css
left: 80px;      /* Abstand vom linken Rand */
right: 80px;     /* Abstand vom rechten Rand */
top: 60px;       /* Abstand vom oberen Rand */
bottom: 60px;    /* Abstand vom unteren Rand */
```

Diese Kombination setzt ein Element oben links auf die Folie:

```css
position: absolute;
left: 80px;
top: 80px;
```

Diese Kombination setzt ein Element unten rechts auf die Folie:

```css
position: absolute;
right: 80px;
bottom: 60px;
```

<div class='hint'>
Verwende nicht wahllos alle vier Richtungen gleichzeitig.
Wenn du <code>left</code>, <code>right</code>, <code>top</code> und <code>bottom</code> gleichzeitig angibst, wird das Element gestreckt.
</div>

##### Eine Beschriftung auf ein Bild setzen

Absolute Positionierung ist besonders praktisch, wenn du ein kleines Element über ein Bild legen möchtest.
Dafür packst du Bild und Beschriftung gemeinsam in ein `<div>`.
Dieses äußere `<div>` bekommt `position: relative;`.
Die Beschriftung darin bekommt `position: absolute;`.

<button class='shower-mini' type='button'>
    <img src='screenshots/beschriftung-auf-bild.webp'>
</button>

```html
<section class="slide">
    <h2>Beschriftung auf einem Bild</h2>

    <div style="
        position: relative;
        width: 650px;
        height: 360px;
        border-radius: 15px;
        overflow: hidden;
    ">
        <img
            src="pictures/fuji.jpg"
            alt="Der Fuji"
            style="
                width: 100%;
                height: 100%;
                object-fit: cover;
            ">

        <div style="
            position: absolute;
            background: linear-gradient(to top, rgba(0, 0, 0, 0.7), transparent);
            color: white;
            padding: 0 12px;
            left: 0;
            right: 0;
            bottom: 0;
            text-align: center;
            font-size: 90%;
        ">
            Fuji vom Shōji-See aus, dazwischen der Berg Ōmuro.
        </div>
    </div>
</section>
```

Das äußere `<div>` ist hier der Rahmen für die Positionierung:

```html
<div style="position: relative;">
```

Das Bild und die Beschriftung orientieren sich dadurch nicht an der ganzen Folie, sondern an diesem Rahmen. Die Beschriftung wird mit `position: absolute;` genau innerhalb dieses Rahmens platziert, und zwar so, dass sie unten über dem Bild liegt (`bottom: 0;`) und sich über die gesamte Breite erstreckt (`left: 0; right: 0;`).

<div class='hint'>
Die Kombination aus außen <code>position: relative;</code> und innen <code>position: absolute;</code> ist sehr häufig.
Merke dir:
Der äußere Kasten ist der Bezugspunkt.
Das innere Element wird darin genau platziert.
</div>

In diesem Beispiel tauchen einige interessante CSS-Eigenschaften auf, die du vielleicht noch nicht kennst:

- `overflow: hidden;` sorgt dafür, dass die Inhalte des äußeren `<div>` nicht über dessen Grenzen hinaus sichtbar sind, damit die abgerundeten Ecken auch wirklich rund aussehen
- `background: linear-gradient(to top, rgba(0, 0, 0, 0.7), transparent);` erzeugt einen Farbverlauf von einem halbtransparenten Schwarz zu transparent, damit die weiße Beschriftung besser lesbar ist
- `text-align: center;` zentriert den Text horizontal in der Beschriftung
- `font-size: 90%;` macht die Schrift etwas kleiner als die normale Schriftgröße, damit sie besser zum Bild passt

<div class='shower-mini-clear'></div>

#### Mehrere Spalten verwenden

<!--
Hier sollte ein sehr einfaches Zwei-Spalten-Layout gezeigt werden mit class="columns two" und class="columns three".
-->

Wenn du Inhalte gleichmäßig nebeneinander anordnen möchtest, musst du sie nicht einzeln mit `left`, `right`, `top` oder `bottom` platzieren.
Für normale Layouts sind Spalten meistens viel einfacher.

In der Vorlage gibt es dafür die Klasse `columns`.
Zusätzlich gibst du an, wie viele Spalten du möchtest:

- `columns two` erzeugt zwei Spalten
- `columns three` erzeugt drei Spalten

Das ist besonders praktisch für Vergleiche, kurze Listen, Vor- und Nachteile oder mehrere kleine Textblöcke nebeneinander.

<div class='hint'>
Verwende Spalten für Inhalte, die gleich wichtig sind und nebeneinander verglichen werden sollen.
Wenn du dagegen nur ein einzelnes Logo, eine Beschriftung oder eine kleine Infokarte genau platzieren möchtest, ist <code>position: absolute;</code> oft passender.
</div>

##### Zwei Textspalten

Für zwei Spalten verwendest du ein äußeres `<div>` mit der Klasse `columns two`.
Jedes direkte Kind dieses `<div>` wird zu einer eigenen Spalte.

<button class='shower-mini' type='button'>
    <img src='screenshots/zwei-textspalten.webp'>
</button>

```html
<section class="slide">
    <h2>Zwei Spalten</h2>

    <div class="columns two">
        <div>
            <h3>HTML</h3>
            <p>
                HTML beschreibt die Struktur:
                Überschriften, Absätze, Listen, Bilder und Folien.
            </p>
        </div>

        <div>
            <h3>CSS</h3>
            <p>
                CSS beschreibt die Gestaltung:
                Farben, Größen, Abstände und Positionen.
            </p>
        </div>
    </div>
</section>
```

Die Struktur ist wichtig:

```html
<div class="columns two">
    <div>Erste Spalte</div>
    <div>Zweite Spalte</div>
</div>
```

Das äußere `<div>` erzeugt das Spaltenlayout.
Die beiden inneren `<div>`-Elemente sind die Inhalte der beiden Spalten.

<div class='hint'>
Achte darauf, dass die beiden Spalten ungefähr gleich viel Inhalt haben.
Wenn eine Spalte sehr voll ist und die andere fast leer, wirkt die Folie schnell unausgewogen.
</div>

<div class='shower-mini-clear'></div>

##### Listen in zwei Spalten

Du kannst auch Listen in Spalten setzen.
Das ist praktisch, wenn du zwei Gruppen gegenüberstellen möchtest.

<button class='shower-mini' type='button'>
    <img src='screenshots/zwei-spalten-listen.webp'>
</button>

```html
<section class="slide">
    <h2>Vor- und Nachteile</h2>

    <div class="columns two">
        <div>
            <h3>Vorteile</h3>
            <ul>
                <li>Läuft im Browser</li>
                <li>Funktioniert auch offline</li>
                <li>Lässt sich mit CSS gestalten</li>
            </ul>
        </div>

        <div>
            <h3>Nachteile</h3>
            <ul>
                <li>HTML muss genau stimmen</li>
                <li>Fehler sind anfangs ungewohnt</li>
                <li>Man braucht etwas Übung</li>
            </ul>
        </div>
    </div>
</section>
```

In diesem Beispiel enthält jede Spalte eine eigene Überschrift und eine eigene Liste.
Die Listen gehören also nicht direkt in das äußere Spalten-`<div>`, sondern jeweils in ein eigenes inneres `<div>`.

<div class='hint'>
Wenn du in jeder Spalte mehrere Elemente brauchst, zum Beispiel eine Überschrift und eine Liste, packe sie gemeinsam in ein eigenes <code>&lt;div&gt;</code>.
So bleiben die Inhalte einer Spalte sauber zusammen.
</div>

<div class='shower-mini-clear'></div>

##### Drei Spalten verwenden

Für drei Spalten verwendest du `columns three`.
Das funktioniert genauso wie bei zwei Spalten, nur mit drei direkten Kindern.

<button class='shower-mini' type='button'>
    <img src='screenshots/drei-spalten.webp'>
</button>

```html
<section class="slide">
    <h2>Drei wichtige Dateien</h2>

    <div class="columns three">
        <div>
            <h3>index.html</h3>
            <p>Hier stehen deine Folien und Inhalte.</p>
        </div>

        <div>
            <h3>styles.css</h3>
            <p>Hier stehen deine eigenen Gestaltungsregeln.</p>
        </div>

        <div>
            <h3>pictures</h3>
            <p>In diesem Ordner liegen deine Bilder.</p>
        </div>
    </div>
</section>
```

Bei drei Spalten ist weniger Platz pro Spalte.
Darum sollten die Texte besonders kurz sein.

<div class='hint'>
Drei Spalten eignen sich gut für kurze Begriffe, kleine Erklärungen oder einfache Vergleiche.
Für längere Absätze sind zwei Spalten meistens besser.
</div>

<div class='shower-mini-clear'></div>

<!-- #### Wiederverwendbare CSS-Klassen schreiben -->

<!--
Hier sollte der Schritt von Inline-Style zu eigener Klasse kommen.
Ziel: Wiederholung vermeiden und den Schülerinnen und Schülern zeigen, dass CSS-Klassen eigene kleine Werkzeuge sind.
-->

<!-- #### Häufige Fehler finden -->

<!--
Hier sollten typische Fehler gesammelt werden: vergessenes schließendes Tag, falscher Dateiname, fehlende Anführungszeichen, CSS-Regel an falscher Stelle.
Sinnvoll wäre eine kleine Checkliste für die Fehlersuche in VS Code und Browser-Vorschau.
-->

<!-- ### Profi-Tipps -->

<!--
Dieser Teil sollte Arbeitsweisen behandeln, nicht neue HTML/CSS-Bausteine.
Er ist für die Schülerinnen und Schüler gedacht, die ihre Präsentation sichern, teilen oder veröffentlichen wollen.
-->

<!-- #### Änderungen mit Git sichern -->

<!--
Hier sollte sehr praktisch erklärt werden, wann man einen Commit macht und warum das vor größeren Änderungen hilft.
Ziel: Git als Sicherheitsnetz zeigen, nicht als abstraktes Versionskontrollsystem.
-->

## Präsentation halten

Wenn du deine Präsentation halten möchtest, kannst du sie einfach im Browser öffnen und mit den Pfeiltasten navigieren. Du kannst auch die Maus benutzen, um durch die Präsentation zu scrollen.

Der besondere Vorteil bei dieser Art von Präsentation ist, dass sie in jedem modernen Browser funktioniert und du keine spezielle Software brauchst. Du kannst einfach das ganze Verzeichnis auf einen Stick kopieren und deine Präsentation überall abspielen, wo es einen modernen Webbrowser gibt.

Du kannst deine Präsentation auch online stellen, zum Beispiel direkt in der Hackschule. <a href="/custom-subdomain">Hier erfährst du, wie das geht.</a>