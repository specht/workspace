<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# YAML

<p class='abstract'>
YAML ist eine Schreibweise für Daten, die Menschen gut lesen und schreiben können: Einstellungen von Programmen, die Kopfzeilen von Seiten in Hugo, Abläufe in GitHub und GitLab, Docker Compose. Es beschreibt dieselben Daten wie JSON, mit Einrückung statt Klammern und mit Kommentaren. Hier stehen Aufbau, Listen und lange Texte, die Fallen, und wie man YAML im Terminal und in Ruby liest.
</p>

## Aufbau

<!-- part: yaml-aufbau -->
<!-- short: `name: wert`, eingerückt mit Leerzeichen (nie Tab) für Verschachtelung; `#` beginnt einen Kommentar. -->
### Namen, Werte und Einrückung

```yaml
# Ein Film, wie in der Filmdatenbank
title: Nosferatu
year: 1922
watched: true
rating: null
director:
  name: F. W. Murnau
  born: 1888
```

- Jede Zeile `name: wert` ist ein Eintrag. Nach dem Doppelpunkt kommt ein Leerzeichen.
- Was zu einem Eintrag gehört, ist **eingerückt**, hier `name` und `born` unter `director`. Die Einrückung ist die Struktur: Wie viele Leerzeichen, ist egal, aber alle Einträge einer Ebene stehen genau untereinander.
- Eingerückt wird nur mit **Leerzeichen**, nie mit Tab.
- `#` beginnt einen Kommentar bis zum Ende der Zeile.
- Texte brauchen meist keine Anführungszeichen. Zahlen, `true`, `false` und `null` (auch `~`) erkennt YAML selbst.
<!-- /part -->

<!-- part: yaml-listen -->
<!-- short: Listen mit `- ` untereinander oder kurz `[a, b]`; ein Objekt kurz `{name: wert}`. -->
### Listen

```yaml
genres:
  - Horror
  - Fantasy
cast:
  - name: Max Schreck
    role: Graf Orlok
  - name: Greta Schröder
    role: Ellen
```

Jeder Eintrag einer Liste beginnt mit `- `. Ein Eintrag kann selbst ein Objekt sein; seine weiteren Zeilen stehen dann unter dem ersten Namen (`role` unter `name`). Kurze Listen und Objekte gehen auch in einer Zeile, wie in JSON:

```yaml
genres: [Horror, Fantasy]
director: {name: F. W. Murnau, born: 1888}
```
<!-- /part -->

<!-- part: yaml-text -->
<!-- short: `"…"` für Texte mit `:` oder `#`, `|` für Text mit Zeilenumbrüchen, `>` für einen langen Absatz. -->
### Texte und lange Texte

Anführungszeichen braucht ein Text, wenn er mit einem Zeichen beginnt, das YAML etwas bedeutet (`[`, `{`, `-`, `*`, `&`, `!`, `%`, `@`), wenn `: ` oder ` #` darin vorkommt, oder wenn er sonst als Zahl oder Wahrheitswert gelesen würde (siehe [Fallen](#fallen)):

```yaml
title: "Mein Film: Teil 2"
tag: "#horror"
version: "1.10"
```

Für Text über mehrere Zeilen gibt es zwei Zeichen:

```yaml
gedicht: |
  Die Zeilenumbrüche
  bleiben, wie sie sind.
beschreibung: >
  Diese Zeilen werden
  zu einem Absatz zusammengefügt.
```

`|` behält jeden Zeilenumbruch, `>` macht aus einfachen Umbrüchen Leerzeichen; eine leere Zeile beginnt einen neuen Absatz.
<!-- /part -->

<!-- part: yaml-json -->
<!-- short: Dieselben Daten wie JSON; jedes JSON ist auch gültiges YAML. -->
### YAML und JSON

YAML und JSON beschreiben dieselben Daten. Derselbe Film als JSON:

```json
{ "title": "Nosferatu", "year": 1922, "genres": ["Horror", "Fantasy"] }
```

Jedes JSON ist auch gültiges YAML. YAML ist für Menschen gedacht, die eine Datei von Hand schreiben, mit Kommentaren und ohne Klammern und Kommas; JSON für Programme, die sich Daten schicken. Mehr dazu im Handbuch [JSON](/reference/json).
<!-- /part -->

## Fallen

<!-- part: yaml-fallen -->
<!-- short: `no`, `1.10`, `12:30` und `01234` bleiben nicht immer Text: in Anführungszeichen setzen. Kein Tab, kein `: ` ohne Anführungszeichen. -->
### Wenn YAML etwas anderes liest

YAML errät, was ein Wert ist. Was du als Text meinst, ist manchmal eine Zahl oder ein Wahrheitswert, und verschiedene Programme raten verschieden. Ruby liest nach der älteren Version YAML 1.1, `yq` nach YAML 1.2:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Geschrieben</th><th>Ruby</th><th><code>yq</code></th><th>Gemeint war</th></tr></thead>
<tbody>
<tr><td><code>country: no</code></td><td><code>false</code></td><td><code>"no"</code></td><td>Norwegen: <code>"no"</code></td></tr>
<tr><td><code>answer: yes</code> · <code>light: on</code></td><td><code>true</code></td><td><code>"yes"</code> · <code>"on"</code></td><td>je nachdem</td></tr>
<tr><td><code>version: 1.10</code></td><td><code>1.1</code></td><td><code>1.10</code></td><td>Version 1.10: <code>"1.10"</code></td></tr>
<tr><td><code>time: 12:30</code></td><td><code>45000</code></td><td><code>"12:30"</code></td><td>eine Uhrzeit: <code>"12:30"</code></td></tr>
<tr><td><code>zip: 01234</code></td><td><code>668</code></td><td><code>1234</code></td><td>eine Postleitzahl: <code>"01234"</code></td></tr>
<tr><td><code>code: 08</code></td><td><code>"08"</code></td><td><code>8.0</code></td><td><code>"08"</code></td></tr>
</tbody>
</table>
</div>

Die Regel ist einfach: **Was Text bleiben soll, steht in Anführungszeichen**, sobald es aussieht wie eine Zahl, eine Uhrzeit oder ein Ja und Nein.

Zwei Fehler, bei denen das Lesen ganz abbricht:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Meldung</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>found a tab character that violates indentation</code></td><td>In der genannten Zeile ist mit Tab eingerückt. In VS Code zeigt die Statusleiste unten »Spaces« oder »Tab Size«; ein Klick darauf stellt um.</td></tr>
<tr><td><code>mapping values are not allowed in this context</code></td><td>In einem Wert steht <code>: </code>, etwa <code>title: Mein Film: Teil 2</code>. Der Wert gehört in Anführungszeichen.</td></tr>
<tr><td><code>did not find expected key</code> · <code>did not find expected '-' indicator</code></td><td>Die Einrückung passt nicht: Ein Eintrag steht ein Leerzeichen zu weit links oder rechts, bei der zweiten Meldung in einer Liste.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Wo YAML vorkommt

<!-- part: front-matter -->
<!-- short: YAML zwischen zwei Zeilen `---` am Anfang einer Datei: Titel, Datum und mehr für Hugo und andere Generatoren. -->
### Front Matter

Am Anfang einer Markdown-Datei steht bei Hugo und anderen Generatoren oft ein Block YAML zwischen zwei Zeilen `---`, die **Front Matter**. Sie beschreibt die Seite; der Text danach ist ihr Inhalt:

```markdown
---
title: Wie lange schwingt ein Pendel?
date: 2026-10-15
draft: false
tags: [physik, versuch]
---

Eine Kugel an einem Faden schwingt hin und her …
```

Ein Datum schreibt man wie oben, Jahr, Monat, Tag, mit Bindestrichen.
<!-- /part -->

Außerdem stehen in YAML: die Einstellungen von Hugo (`hugo.yaml`), Abläufe in GitHub Actions (`.github/workflows/*.yml`) und GitLab CI (`.gitlab-ci.yml`), Docker Compose (`compose.yaml`) und die Pakete eines Flutter-Projekts (`pubspec.yaml`). `.yml` und `.yaml` bedeuten dasselbe.

## Lesen und prüfen

<!-- part: yq -->
<!-- short: `yq . datei.yaml` prüft und zeigt die Datei, `yq .title` holt ein Feld, `yq -o json` macht JSON daraus, `yq -o yaml` YAML. -->
### yq

`yq` ist für YAML, was `jq` für JSON ist: Es liest eine Datei, meldet Fehler mit Zeile und Spalte und holt heraus, was du brauchst.

```bash
yq . film.yaml                    # prüfen und anzeigen
yq .title film.yaml               # ein Feld
yq '.genres[0]' film.yaml         # der erste Eintrag einer Liste
yq '.cast[].name' film.yaml       # ein Feld aus jedem Eintrag
yq -o json film.yaml              # als JSON
yq -o yaml film.json              # JSON als YAML
yq -i '.watched = true' film.yaml # in der Datei ändern
```

`yq --front-matter=extract .title seite.md` liest die Front Matter einer Markdown-Datei.
<!-- /part -->

<!-- part: yaml-ruby -->
<!-- short: `require 'yaml'`; `YAML.load_file(pfad)` liest, `daten.to_yaml` schreibt. -->
### Ruby

```ruby
require 'yaml'

film = YAML.load_file('film.yaml')
puts film['title']                 # Nosferatu
film['watched'] = true
File.write('film.yaml', film.to_yaml)
```

`YAML.load_file` macht aus Objekten Hashes, aus Listen Arrays, wie `JSON.parse`. Ist die Datei kaputt, wirft es einen `Psych::SyntaxError` mit Zeile und Spalte. Ruby liest nach YAML 1.1; die [Fallen](#fallen) oben gelten hier besonders.
<!-- /part -->
