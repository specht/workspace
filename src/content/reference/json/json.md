<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# JSON

<p class='abstract'>
JSON ist eine Schreibweise für Daten, die fast jede Sprache lesen und schreiben kann: Objekte, Listen, Texte, Zahlen. Programme schicken sich JSON über HTTP und WebSockets, und viele Dateien von Werkzeugen sind JSON. Hier stehen die Regeln, die häufigen Fehler, wie Ruby, JavaScript und Python JSON lesen und schreiben, und wie <code>jq</code> es im Terminal zeigt.
</p>

## Aufbau

<!-- part: json-aufbau -->
<!-- short: Objekte `{"name": wert}`, Listen `[1, 2]`, Texte in `"…"`, Zahlen, `true`, `false`, `null`. -->
### Objekte, Listen und Werte

So könnte ein Film aus der Filmdatenbank aussehen:

```json
{
  "title": "Nosferatu",
  "year": 1922,
  "runtime": 94,
  "genres": ["Horror", "Fantasy"],
  "director": { "name": "F. W. Murnau", "born": 1888 },
  "watched": true,
  "rating": null
}
```

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Schreibweise</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>{ "name": wert, … }</code></td><td><strong>Objekt</strong>: Namen und Werte, durch Kommas getrennt. Die Namen stehen immer in doppelten Anführungszeichen.</td></tr>
<tr><td><code>[ wert, wert, … ]</code></td><td><strong>Liste</strong> (Array): Werte in einer Reihenfolge, auch gemischt</td></tr>
<tr><td><code>"Nosferatu"</code></td><td>Text (String), nur in doppelten Anführungszeichen; <code>\"</code> ist ein Anführungszeichen darin, <code>\n</code> ein Zeilenumbruch</td></tr>
<tr><td><code>1922</code> · <code>-3.5</code> · <code>6.02e23</code></td><td>Zahlen, mit Punkt statt Komma</td></tr>
<tr><td><code>true</code> · <code>false</code></td><td>wahr, falsch</td></tr>
<tr><td><code>null</code></td><td>kein Wert</td></tr>
</tbody>
</table>
</div>

Objekte und Listen lassen sich beliebig ineinander setzen: eine Liste von Filmen, jeder mit einer Liste von Genres. Leerzeichen und Zeilenumbrüche zwischen den Teilen sind egal; sie machen JSON nur lesbar.
<!-- /part -->

<!-- part: json-regeln -->
<!-- short: Doppelte Anführungszeichen, kein Komma nach dem letzten Eintrag, keine Kommentare. -->
### Was JSON nicht erlaubt

JSON sieht aus wie ein Objekt in JavaScript, ist aber strenger. Diese Fehler kommen am häufigsten vor:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Falsch</th><th>Richtig</th></tr></thead>
<tbody>
<tr><td><code>{ title: "Nosferatu" }</code></td><td><code>{ "title": "Nosferatu" }</code>: Namen brauchen Anführungszeichen.</td></tr>
<tr><td><code>{ "title": 'Nosferatu' }</code></td><td><code>{ "title": "Nosferatu" }</code>: nur doppelte Anführungszeichen</td></tr>
<tr><td><code>[1, 2, 3,]</code></td><td><code>[1, 2, 3]</code>: kein Komma nach dem letzten Eintrag</td></tr>
<tr><td><code>// ein Kommentar</code></td><td>JSON kennt keine Kommentare. Wer welche braucht, nimmt YAML.</td></tr>
<tr><td><code>{ "year": 01922 }</code></td><td><code>1922</code>: keine führenden Nullen</td></tr>
<tr><td><code>{ "rating": NaN }</code></td><td><code>null</code>: <code>NaN</code> und <code>Infinity</code> gibt es nicht</td></tr>
</tbody>
</table>
</div>

Ist JSON kaputt, nennt die Fehlermeldung die Stelle, an der das Lesen scheiterte. Für ein Komma nach dem letzten Eintrag eines Objekts:

```text
Expected double-quoted property name in JSON at position 42 (line 4 column 1)   ← JavaScript
parse error: Expected another key-value pair at line 4, column 1                ← jq
unexpected token at '{ …' (JSON::ParserError)                                   ← Ruby
```

Der Fehler liegt meist kurz vor der genannten Stelle, hier in der Zeile davor. `jq . datei.json` prüft eine Datei und nennt Zeile und Spalte.
<!-- /part -->

## In Programmen

<!-- part: json-ruby -->
<!-- short: `require 'json'`; `JSON.parse(text)` liest, `JSON.generate(daten)` oder `daten.to_json` schreibt. -->
### Ruby

```ruby
require 'json'

film = JSON.parse('{"title": "Nosferatu", "year": 1922}')
puts film['title']                 # Nosferatu
film['watched'] = true

puts JSON.generate(film)           # {"title":"Nosferatu","year":1922,"watched":true}
puts JSON.pretty_generate(film)    # eingerückt, zum Lesen
File.write('film.json', film.to_json)
films = JSON.parse(File.read('films.json'))
```

`JSON.parse` macht aus Objekten Hashes mit Strings als Schlüsseln (`film['title']`), aus Listen Arrays. Mit `JSON.parse(text, symbolize_names: true)` sind die Schlüssel Symbole (`film[:title]`). Ist der Text kein gültiges JSON, wirft `JSON.parse` einen `JSON::ParserError`; ein Server fängt ihn ab (`rescue`), statt abzustürzen.
<!-- /part -->

<!-- part: json-javascript -->
<!-- short: `JSON.parse(text)` liest, `JSON.stringify(daten, null, 2)` schreibt; `await response.json()` bei `fetch`. -->
### JavaScript

```js
const film = JSON.parse('{"title": "Nosferatu", "year": 1922}');
console.log(film.title);                        // Nosferatu

const text = JSON.stringify(film);              // {"title":"Nosferatu","year":1922}
const pretty = JSON.stringify(film, null, 2);   // eingerückt mit 2 Leerzeichen

const response = await fetch('/api/films');
const films = await response.json();            // die Antwort als JSON lesen
```

Über einen WebSocket kommen Nachrichten als Text an; `JSON.parse(event.data)` macht daraus wieder ein Objekt.
<!-- /part -->

<!-- part: json-python -->
<!-- short: `import json`; `json.loads(text)` liest, `json.dumps(daten, indent=2)` schreibt; `json.load`/`json.dump` für Dateien. -->
### Python

```python
import json

film = json.loads('{"title": "Nosferatu", "year": 1922}')
print(film["title"])                            # Nosferatu

text = json.dumps(film, indent=2, ensure_ascii=False)   # eingerückt, Umlaute bleiben

with open("films.json") as f:
    films = json.load(f)
with open("film.json", "w") as f:
    json.dump(film, f, indent=2)
```

Ohne `ensure_ascii=False` schreibt Python Umlaute als `ü`; das ist gültiges JSON, aber schlecht zu lesen.
<!-- /part -->

## Im Terminal

<!-- part: jq -->
<!-- short: `jq .` zeigt JSON eingerückt und farbig, `jq .title` holt ein Feld, `jq '.[] | .title'` eines aus jedem Eintrag. -->
### jq

`jq` liest JSON aus einer Datei oder aus einer Pipe und zeigt es eingerückt und farbig. Mit einem Filter holt es heraus, was du brauchst:

```bash
jq . film.json                          # alles, lesbar
jq .title film.json                     # ein Feld: "Nosferatu"
jq -r .title film.json                  # ohne Anführungszeichen: Nosferatu
jq '.director.name' film.json           # verschachtelt
jq '.[] | .title' films.json            # aus jedem Eintrag einer Liste
jq '.[] | select(.year < 1930) | .title' films.json   # nur einige
jq 'length' films.json                  # wie viele Einträge
curl -s https://api.github.com/users/octocat | jq .name
```

`jq` meldet kaputtes JSON mit Zeile und Spalte, zum Beispiel `parse error: Expected another key-value pair at line 4, column 1` für ein Komma zu viel.
<!-- /part -->

## Zum Nachschlagen

- [json.org](https://www.json.org/json-de.html): die ganze Schreibweise von JSON auf einer Seite, mit Diagrammen, auf Deutsch.
- [RFC 8259](https://www.rfc-editor.org/rfc/rfc8259.html): die Spezifikation von JSON als Internet-Standard, etwa welche Zeichen in einem String mit `\` geschrieben werden müssen.
- [jq-Handbuch](https://jqlang.org/manual/): alle Filter und Funktionen von `jq`, mit Beispielen.
- [Ruby: JSON](https://docs.ruby-lang.org/en/3.4/JSON.html), [JavaScript: JSON](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/JSON) (MDN, auf Deutsch) und [Python: json](https://docs.python.org/3/library/json.html): alle Optionen zum Lesen und Schreiben in der jeweiligen Sprache.
