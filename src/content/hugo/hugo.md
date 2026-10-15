<div class='meta'>
image: startseite.webp
kind: projekt
language: Hugo
topic: networks
duration: 2 Doppelstunden
requires: [static-website]
reference: [yaml, html, css]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Eine Website mit Hugo

<p class='abstract'>
Eine einzelne Webseite schreibt man von Hand. Bei zehn Seiten, die alle denselben Kopf, dieselbe Navigation und denselben Fuß haben, wird das mühsam. Mit Hugo schreibst du die Inhalte in Markdown und das Aussehen einmal als Vorlage; Hugo macht daraus eine fertige Website aus lauter HTML-Dateien. Du baust ein kleines Aquarium-Lexikon mit Navigation, Startseite und Schlagwörtern und veröffentlichst es.
</p>

## Warum Hugo?

In [Statische Webseiten mit HTML & CSS](/static-website) hast du eine Seite über den Axolotl gebaut. Stell dir vor, daraus soll ein Lexikon werden: eine Seite für den Axolotl, eine für das Seepferdchen, eine für den Oktopus. Jede Seite braucht denselben Kopf mit derselben Navigation, die zu allen anderen Seiten führt. Kommt ein viertes Tier dazu, musst du die Navigation in allen Seiten ändern, und vergisst du eine, fehlt dort der Link.

Ein **Static Site Generator** wie Hugo trennt zwei Dinge:

- Die **Inhalte**: Für jede Seite eine Markdown-Datei, nur mit dem, was auf dieser Seite steht.
- Das **Layout**: Einmal HTML für alles, was auf jeder Seite gleich ist, mit Lücken für das, was sich ändert.

Hugo setzt beides zusammen und schreibt für jede Seite eine fertige HTML-Datei. Das Ergebnis ist eine ganz normale statische Website wie deine Axolotl-Seite: Dateien, die jeder Webserver ausliefern kann.

## Eine neue Website

Lege im Terminal eine neue Website an:

```bash
cd /workspace
hugo new site aquarium --format yaml
```

Öffne dann den neuen Ordner `/workspace/aquarium` mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>. Hugo hat ein paar Ordner angelegt; vier davon brauchst du:

- `content`: die Inhalte, eine Markdown-Datei pro Seite.
- `layouts`: die Vorlagen aus HTML.
- `static`: Dateien, die Hugo unverändert übernimmt, etwa CSS und Bilder.
- `hugo.yaml`: die Einstellungen der Website.

Die Einstellungen stehen in YAML. Ersetze den Inhalt von `hugo.yaml` durch:

```yaml
baseURL: /
locale: de
title: Mein Aquarium
```

- `baseURL: /` sagt Hugo, dass die Website ganz oben auf ihrem Server liegt. Alle Adressen beginnen dann mit `/`, und die Seite funktioniert in der Vorschau genauso wie später auf deiner Subdomain.
- `locale: de` ist die Sprache der Website.
- `title` ist ihr Name; er erscheint gleich auf jeder Seite.

<!-- tutorial-screenshot
# Neue Website im Terminal, dann den Ordner öffnen
terminal-open
terminal-run: cd /workspace && hugo new site aquarium --format yaml
terminal-wait-for-prompt
write-file: aquarium/hugo.yaml <- file:stufen/aquarium/hugo.yaml
press: Control+j
press: Control+k
press: Control+o
wait-for-text: Open Folder
press: Control+a
type: /workspace/aquarium/
press: Enter
sleep: 5
wait-for-text: hugo.yaml
open-file: hugo.yaml
sleep: 1
-->
<img class='full' src='neue-website.webp' alt='Der Ordner aquarium mit archetypes, assets, content, data, i18n, layouts, static, themes und hugo.yaml; hugo.yaml ist offen mit baseURL, locale und title.'>

_uses(yaml-aufbau)

_task(einstellungen)

## Die erste Seite

Eine Seite ist eine Markdown-Datei in `content`. Lege `content/axolotl.md` an:

```markdown_copy
---
title: Axolotl
date: 2026-10-15
description: Ein Lurch, der nie erwachsen wird.
tags: [Süßwasser, Amphibie]
---

Der **Axolotl** ist ein Schwanzlurch aus Mexiko. In der Natur lebt er nur noch
in den Kanälen von Xochimilco am Rand von Mexiko-Stadt und ist dort vom
Aussterben bedroht.

Anders als andere Lurche bleibt er sein Leben lang im Wasser und behält seine
Kiemen, die wie ein Federkranz am Kopf abstehen.

## Was er kann

- Ein verlorenes Bein wächst nach, mit Knochen, Muskeln und Nerven.
- Er wird bis zu 30 Zentimeter lang.
- Er frisst Würmer, Larven und kleine Fische.
```

Oben steht zwischen zwei Zeilen `---` die **Front Matter**, Angaben über die Seite in YAML: ihr Titel, ein Datum, eine kurze Beschreibung und Schlagwörter (`tags`). Darunter steht der Inhalt in Markdown, wie in einer README-Datei: `**fett**`, `## Überschrift`, `- Liste`.

_uses(front-matter)

Starte Hugos eigenen Webserver:

```bash
hugo server
```

Hugo meldet sich mit einigen Warnungen, darunter diese:

```text
WARN  found no layout file for "html" for kind "page"
```

Hugo hat die Seite gelesen, weiß aber noch nicht, wie daraus HTML werden soll: Es gibt noch kein Layout.

_task(yaml-titel)

## Ein Layout für alle Seiten

Ein Layout ist HTML mit Lücken. Die Lücken stehen in doppelten geschweiften Klammern, `{{ … }}`, und Hugo füllt sie für jede Seite. Lege zuerst das Grundgerüst an, das jede Seite bekommt, `layouts/baseof.html`:

```html_copy
<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{{ .Title }} · {{ site.Title }}</title>
</head>
<body>
  <header>
    <a class="logo" href="{{ "/" | relURL }}">{{ site.Title }}</a>
  </header>
  <main>
    {{ block "main" . }}{{ end }}
  </main>
  <footer>Gebaut mit Hugo im Hackschule Workspace</footer>
</body>
</html>
```

und dann `layouts/page.html`, was eine einzelne Seite in dieses Gerüst schreibt:

```html
{{ define "main" }}
  <h1>{{ .Title }}</h1>
  {{ .Content }}
{{ end }}
```

- `{{ .Title }}` ist der Titel der Seite aus ihrer Front Matter, `{{ site.Title }}` der Titel der Website aus `hugo.yaml`.
- `{{ .Content }}` ist der Inhalt der Markdown-Datei, von Hugo in HTML übersetzt: Aus `## Was er kann` wird `<h2>Was er kann</h2>`.
- `{{ block "main" . }}{{ end }}` in `baseof.html` ist eine Lücke mit dem Namen `main`. `{{ define "main" }} … {{ end }}` in `page.html` füllt sie. So steht der Rahmen nur einmal da, und jede Art von Seite füllt nur ihre Mitte.
- `{{ "/" | relURL }}` ist die Adresse der Startseite. Der senkrechte Strich gibt `"/"` an die Funktion `relURL` weiter, die die passende Adresse daraus macht.

_uses(grundgeruest)

Hugo merkt jede gespeicherte Datei und baut die Website neu. Öffne die Adresse von Port 1313 aus dem Reiter »Ports« (»Forwarded Address«) im Browser und hänge `axolotl/` an:

<!-- tutorial-screenshot
# Die erste Seite mit dem einfachen Layout, Hugo läuft
write-file: aquarium/content/axolotl.md <- file:stufen/aquarium/content/axolotl.md
write-file: aquarium/layouts/baseof.html <- file:stufen/baseof-1.html
write-file: aquarium/layouts/page.html <- file:stufen/page-1.html
open-file: content/axolotl.md
terminal-open
terminal-run: hugo server
sleep: 4
-->
<img class='full' src='erste-seite-code.webp' alt='Die Datei content/axolotl.md mit Front Matter und Markdown; im Terminal meldet hugo server, dass die Website unter localhost:1313 erreichbar ist.'>

<!-- tutorial-screenshot
preview-port: 1313 /axolotl/
tab: preview
wait-for-text: Was er kann
sleep: 1
-->
<img class='full border' src='erste-seite.webp' alt='Die Seite Axolotl im Browser, noch ohne CSS: oben der Link »Mein Aquarium«, darunter Überschrift, Text, eine Liste und der Fuß »Gebaut mit Hugo im Hackschule Workspace«.'>

Noch ohne CSS, aber eine Seite: Der Kopf mit »Mein Aquarium« und der Fuß kommen aus `baseof.html`, die Überschrift aus `page.html`, alles andere aus `axolotl.md`. Lade die Vorschau nach dem Speichern neu, um eine Änderung zu sehen.

_task(content)

## Mehr Seiten und eine Navigation

Jetzt zeigt sich, wofür das gut ist. Lege zwei weitere Seiten an, `content/seepferdchen.md`:

```markdown_copy
---
title: Seepferdchen
date: 2026-10-15
description: Ein Fisch, bei dem die Väter die Jungen austragen.
tags: [Meer, Fisch]
---

Das **Seepferdchen** ist ein Fisch, auch wenn es nicht so aussieht. Es
schwimmt aufrecht und treibt sich mit einer kleinen Rückenflosse an, die bis zu
50-mal in der Sekunde schlägt.

## Was es besonders macht

- Das Männchen trägt die Eier in einer Bauchtasche aus, bis die Jungen
  schlüpfen.
- Mit dem Schwanz hält es sich an Seegras fest, damit die Strömung es nicht
  forttreibt.
- Seine Augen bewegen sich unabhängig voneinander.
```

und `content/oktopus.md`:

```markdown_copy
---
title: Oktopus
date: 2026-10-15
description: Acht Arme, drei Herzen und blaues Blut.
tags: [Meer, Weichtier]
---

Der **Oktopus** ist ein Weichtier, verwandt mit Schnecken und Muscheln, aber
ohne Schale. Er gilt als das klügste wirbellose Tier.

## Was er kann

- Er hat drei Herzen und blaues Blut.
- Er ändert in Sekundenbruchteilen Farbe und Muster seiner Haut.
- Er öffnet Schraubgläser, um an Futter zu kommen.
```

Beide bekommen sofort denselben Kopf und Fuß, ohne dass du dafür HTML schreibst. Fehlt noch die Navigation zu allen Seiten. Ergänze in `layouts/baseof.html` unter dem Link mit `site.Title`:

```html
<nav>
  {{ range site.RegularPages }}
    <a href="{{ .RelPermalink }}">{{ .Title }}</a>
  {{ end }}
</nav>
```

`{{ range site.RegularPages }} … {{ end }}` ist eine Schleife über alle Seiten der Website. Was dazwischen steht, schreibt Hugo für jede Seite einmal hin; der Punkt steht darin für die Seite, um die es gerade geht. `.RelPermalink` ist ihre Adresse, `.Title` ihr Titel.

_task(neue-seite)

_task(navigation)

## Die Startseite

Die Startseite ist eine besondere Seite: `content/_index.md`. Der Unterstrich sagt Hugo, dass sie zum ganzen Ordner gehört, nicht zu einem einzelnen Tier:

```markdown
---
title: Mein Aquarium
---

Tiere, die im Wasser leben, und was sie besonders macht. Jede Seite ist eine
Markdown-Datei im Ordner `content`; Hugo macht daraus eine Webseite.
```

Sie bekommt ein eigenes Layout, `layouts/home.html`, das unter ihrem Text eine Karte für jedes Tier zeigt:

```html
{{ define "main" }}
  <h1>{{ .Title }}</h1>
  {{ .Content }}
  <ul class="cards">
    {{ range site.RegularPages }}
      <li>
        <a href="{{ .RelPermalink }}">
          <strong>{{ .Title }}</strong>
          <span>{{ .Description }}</span>
        </a>
      </li>
    {{ end }}
  </ul>
{{ end }}
```

`{{ .Description }}` ist die Beschreibung aus der Front Matter jeder Seite. Wer ein neues Tier anlegt, bekommt die Karte auf der Startseite und den Link in der Navigation dazu, ohne eine Zeile HTML.

## Aussehen mit CSS

Was in `static` liegt, übernimmt Hugo unverändert in die fertige Website. Lege `static/style.css` an:

```css_copy
body {
  margin: 0;
  font-family: system-ui, sans-serif;
  line-height: 1.6;
  color: #1d3557;
  background: #f1faee;
}

header {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 1em;
  padding: 1em 2em;
  background: #1d3557;
}

header a {
  color: #f1faee;
  text-decoration: none;
}

.logo {
  font-size: 1.4em;
  font-weight: bold;
  margin-right: auto;
}

main {
  max-width: 40em;
  margin: 2em auto;
  padding: 0 1em;
}

h1 {
  color: #e63946;
}

.tags a {
  display: inline-block;
  padding: 0.1em 0.6em;
  border-radius: 1em;
  background: #a8dadc;
  color: #1d3557;
  text-decoration: none;
  font-size: 0.9em;
}

.cards {
  list-style: none;
  padding: 0;
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(12em, 1fr));
  gap: 1em;
}

.cards a {
  display: block;
  height: 100%;
  padding: 1em;
  border-radius: 0.5em;
  background: white;
  color: inherit;
  text-decoration: none;
  box-shadow: 0 0.2em 0.6em rgba(29, 53, 87, 0.15);
}

.cards span {
  display: block;
  font-size: 0.9em;
}

footer {
  text-align: center;
  padding: 2em;
  font-size: 0.8em;
  opacity: 0.7;
}
```

und binde die Datei in `layouts/baseof.html` im `<head>` ein, unter dem `<title>`:

```html
<link rel="stylesheet" href="{{ "style.css" | relURL }}">
```

Eine Zeile in einem Layout, und jede Seite der Website sieht so aus:

<!-- tutorial-screenshot
# Alle Seiten, die Startseite und das CSS
write-file: aquarium/content/seepferdchen.md <- file:stufen/aquarium/content/seepferdchen.md
write-file: aquarium/content/oktopus.md <- file:stufen/aquarium/content/oktopus.md
write-file: aquarium/content/_index.md <- file:stufen/aquarium/content/_index.md
write-file: aquarium/layouts/baseof.html <- file:stufen/aquarium/layouts/baseof.html
write-file: aquarium/layouts/home.html <- file:stufen/aquarium/layouts/home.html
write-file: aquarium/static/style.css <- file:stufen/aquarium/static/style.css
preview-port: 1313
tab: preview
wait-for-text: Ein Fisch, bei dem die Väter
sleep: 1
-->
<img class='full border' src='startseite.webp' alt='Die Startseite »Mein Aquarium« mit dunkelblauem Kopf und der Navigation Axolotl, Oktopus, Seepferdchen; darunter ein Absatz und drei weiße Karten mit Titel und Beschreibung der Tiere.'>

_uses(regeln, selektoren)

## Schlagwörter

In der Front Matter jeder Seite stehen schon Schlagwörter, `tags: [Meer, Fisch]`. Hugo legt dafür von selbst Seiten an: eine Liste aller Schlagwörter unter `/tags/` und für jedes Schlagwort eine Seite mit allen Tieren, die es tragen, zum Beispiel `/tags/meer/`. Damit sie zu sehen sind, brauchen sie ein Layout. `layouts/list.html` gilt für alle Seiten, die andere Seiten auflisten:

```html
{{ define "main" }}
  <h1>{{ .Title }}</h1>
  <ul>
    {{ range .Pages }}
      <li><a href="{{ .RelPermalink }}">{{ .LinkTitle }}</a></li>
    {{ end }}
  </ul>
{{ end }}
```

Dann zeigt jede Tierseite ihre Schlagwörter als Links. Ersetze in `layouts/page.html` die Zeile mit `<h1>` durch:

```html
<h1>{{ .Title }}</h1>
<p class="tags">
  {{ range .GetTerms "tags" }}
    <a href="{{ .RelPermalink }}">{{ .LinkTitle }}</a>
  {{ end }}
</p>
```

`.GetTerms "tags"` sind die Schlagwörter der Seite, jedes mit seiner eigenen Adresse.

<!-- tutorial-screenshot
# Schlagwörter: Tierseite mit Tags, dann die Seite eines Tags
write-file: aquarium/layouts/list.html <- file:stufen/aquarium/layouts/list.html
write-file: aquarium/layouts/page.html <- file:stufen/aquarium/layouts/page.html
preview-port: 1313 /tags/meer/
tab: preview
wait-for-text: Seepferdchen
sleep: 1
-->
<img class='full border' src='tags.webp' alt='Die Seite »Meer« mit den Links Oktopus und Seepferdchen, darüber der Kopf mit der Navigation.'>

_task(tags)

## Die fertige Website

`hugo server` hält die Website für die Vorschau bereit. Die fertige Website, die du veröffentlichst, baut der Befehl `hugo` allein. Beende den Server mit <kbd>Strg</kbd><kbd>C</kbd> und baue die Website neu, nachdem du den alten Stand gelöscht hast:

```bash
rm -rf public
hugo
```

Hugo schreibt alles in den Ordner `public`: `public/index.html` ist die Startseite, `public/axolotl/index.html` die Seite des Axolotls, `public/style.css` dein CSS, und unter `public/tags/` stehen die Seiten der Schlagwörter. Öffne eine der HTML-Dateien: Es ist ganz normales HTML, mit der Navigation darin, als hättest du sie von Hand geschrieben.

<!-- tutorial-screenshot
# hugo baut public, die Startseite ist offen
terminal-open
press: Control+c
sleep: 1
terminal-run: clear
terminal-run: rm -rf public && hugo
terminal-wait-for-prompt
open-file: public/axolotl/index.html
sleep: 2
-->
<img class='full' src='public.webp' alt='Im Explorer der Ordner public mit index.html, style.css und den Ordnern axolotl, oktopus, seepferdchen und tags; public/axolotl/index.html ist offen und zeigt fertiges HTML mit der Navigation; im Terminal die Zusammenfassung von hugo.'>

_task(public)

### Veröffentlichen

Den Inhalt von `public` kopierst du auf deine Hackschule-Subdomain, genau wie eine selbst geschriebene Website. Wie du eine Subdomain bekommst und einrichtest, steht in der [Anleitung zum eigenen Webspace](/custom-subdomain); hochladen geht dann mit `rsync`, von `public` aus. Ersetze `max` und `cobolultras` durch deinen Benutzernamen und deine Subdomain:

```bash
rsync -avz public/ max@cobolultras.hackschule.de:cobolultras
```

Der Schrägstrich hinter `public/` ist wichtig: Er kopiert den Inhalt des Ordners, nicht den Ordner selbst. Nach jeder Änderung baust du mit `hugo` neu und lädst noch einmal hoch; `rsync` überträgt nur, was sich geändert hat.

_task(abgabe)

## Deine Website

Jetzt ist es deine Website. Ein paar Ideen:

- **Ein eigenes Thema:** Ersetze die Tiere durch etwas, das dich interessiert: Bücher, Spiele, Orte, eine Sportart. `title` in `hugo.yaml` und die Seiten in `content` ändern, die Layouts bleiben.
- **Bilder:** Lege Bilder in `static` ab und binde sie in Markdown ein: `![Ein Axolotl](/axolotl.webp)`.
- **Eine Reihenfolge:** Mit `weight: 1`, `weight: 2` … in der Front Matter bestimmst du, in welcher Reihenfolge die Seiten in der Navigation stehen.
- **Teile auslagern:** Lege den Kopf als eigene Datei `layouts/_partials/header.html` an und binde ihn mit `{{ partial "header.html" . }}` ein.
- **Bereiche:** Ein Unterordner `content/tiere/` wird ein eigener Bereich mit eigener Liste unter `/tiere/`.

_task(generator)

## Wie geht es weiter?

Hugo kann noch viel mehr: Bilder verkleinern, mehrere Sprachen, Seiten aus Daten in YAML oder JSON. Alles steht in der [Dokumentation von Hugo](https://gohugo.io/documentation/). Statt eigener Layouts gibt es auch fertige Themes; mit deinen eigenen weißt du aber genau, was auf jeder Seite passiert.

Andere Generatoren wie Jekyll, Eleventy oder Astro funktionieren nach demselben Prinzip: Inhalte und Layout getrennt, am Ende statische Dateien. Viele Websites von Projekten, Vereinen und Dokumentationen entstehen so.
