<div class='meta'>
image: teaser.webp
image_alt: "Eine Seite mit Suchergebnissen: im Suchfeld »katze«, darunter drei Treffer mit Titel, Adresse und dem Anfang ihres Textes, oben die Hauskatze mit 3 Links auf sie, dann der Hund mit 2, zuletzt orange die Anzeige »Katze Katze Katzenfutter« mit 1 Link, deren Text nur aus dem Wort Katze besteht."
kind: projekt
language: Ruby / Python
languages: [ruby, python]
duration: 2 Doppelstunden
curriculum: "Sek I: Leben in und mit vernetzten Systemen (Suchmaschinen, Beschaffung und Prüfung von Informationen), Algorithmisches Problemlösen (Implementieren von Algorithmen); Oberstufe: Algorithmen und Datenstrukturen (Breitensuche, ein invertierter Index), Vertiefung Informatik und Gesellschaft (wer bestimmt, was gefunden wird)"
requires: [kata-dicts, kata-lists]
reference: [graphen, listen, regex, http]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Eine Suchmaschine

<p class='abstract'>
Wie findet eine Suchmaschine unter Milliarden Seiten die zehn, die sie dir zeigt? Du baust eine für eine kleine Website: einen Crawler, der den Links folgt, einen Index, der zu jedem Wort die Seiten kennt, und eine Rangfolge der Treffer. Dabei siehst du, wer bestimmt, was oben steht, und wie man das ausnutzt.
</p>

## Drei Teile

Eine Suchmaschine liest nicht erst dann im Web nach, wenn du etwas eintippst.
Das würde viel zu lange dauern. Sie hat vorher drei Schritte gemacht, und
jeden davon baust du hier selbst:

- Ein **Crawler** hat das Web abgesucht: Er beginnt bei einer Seite, folgt
  ihren Links, dann den Links der nächsten Seiten, und hebt jede Seite auf.
- Ein **Index** sagt zu jedem Wort, auf welchen Seiten es steht, so wie das
  Stichwortverzeichnis hinten in einem Buch.
- Ein **Ranking** ordnet die Treffer: Was oben steht, wird gelesen.

Deine Suchmaschine durchsucht das »Kleine Tierlexikon«, eine Website mit elf
Seiten.

## Einrichten

Lege einen Ordner an, lade das Tierlexikon herunter und lege zwei leere Dateien
für deine Suchmaschine an:

<!-- language: ruby -->
```bash_copy
mkdir -p ~/suchmaschine
cd ~/suchmaschine
wget -qO- ${web_root}/suchmaschine/tierlexikon.tar.gz | tar xz
touch search.rb main.rb
```

In `search.rb` kommen die Funktionen deiner Suchmaschine, in `main.rb` das
Programm, das sie benutzt.
<!-- /language -->
<!-- language: python -->
```bash_copy
mkdir -p ~/suchmaschine
cd ~/suchmaschine
wget -qO- ${web_root}/suchmaschine/tierlexikon.tar.gz | tar xz
touch search.py main.py
```

In `search.py` kommen die Funktionen deiner Suchmaschine, in `main.py` das
Programm, das sie benutzt.
<!-- /language -->

Das Tierlexikon braucht einen Webserver. Öffne mit
<kbd>Strg</kbd><kbd>Shift</kbd><kbd>5</kbd> ein zweites Terminal und starte
darin Pythons eingebauten Webserver; er liefert die Dateien eines Ordners aus:

```bash
cd ~/suchmaschine
python3 -m http.server 8000 --directory tierlexikon
```

Öffne die »Forwarded Address« für Port 8000 und schau dich um: eine Startseite
mit sieben Tieren, auf jeder Seite ein paar Links zu anderen.

## Der Crawler

Der Crawler holt Seiten über HTTP, so wie dein Browser, und sucht darin nach
Links. Beides gibt die Seite vor.

<!-- language: ruby -->
Schreib in `search.rb`:

```ruby
require 'net/http'

# Wo die Website läuft, die die Suchmaschine durchsucht
SITE = 'http://localhost:8000'

# Eine Seite der Website als String, oder nil, wenn es sie nicht gibt
def fetch_page(path)
  response = Net::HTTP.get_response(URI(SITE + path))
  response.is_a?(Net::HTTPSuccess) ? response.body.force_encoding('UTF-8') : nil
end

# Die Ziele der Links einer Seite, die auf derselben Website bleiben
def links(html)
  html.scan(/<a href="(\/[^"#]*)"/).flatten
end
```

- `fetch_page('/igel.html')` holt die Seite vom Webserver. Antwortet er mit
  einem Fehler wie `404`, gibt sie `nil` zurück.
- `links` findet mit einem [regulären Ausdruck](/reference/regex#in-ruby)
  jedes `<a href="/…">`. Nur Ziele, die mit `/` beginnen, bleiben auf der
  Website; ein Link zur Wikipedia beginnt mit `https:` und zählt nicht.
<!-- /language -->
<!-- language: python -->
Schreib in `search.py`:

```python
import re
import urllib.error
import urllib.request

# Wo die Website läuft, die die Suchmaschine durchsucht
SITE = 'http://localhost:8000'


def fetch_page(path):
    """Eine Seite der Website als String, oder None, wenn es sie nicht gibt"""
    try:
        with urllib.request.urlopen(SITE + path) as response:
            return response.read().decode()
    except urllib.error.HTTPError:
        return None


def links(html):
    """Die Ziele der Links einer Seite, die auf derselben Website bleiben"""
    return re.findall(r'<a href="(/[^"#]*)"', html)
```

- `fetch_page('/igel.html')` holt die Seite vom Webserver. Antwortet er mit
  einem Fehler wie `404`, gibt sie `None` zurück.
- `links` findet mit einem [regulären Ausdruck](/reference/regex#in-python)
  jedes `<a href="/…">`. Nur Ziele, die mit `/` beginnen, bleiben auf der
  Website; ein Link zur Wikipedia beginnt mit `https:` und zählt nicht.
<!-- /language -->

### Von Link zu Link

Der Crawler beginnt bei `/index.html`. Von dort führen Links zu den Tieren, von
den Tieren zu anderen Tieren und zurück. Er geht so vor:

1. In einer **Queue** stehen die Pfade, die er noch holen muss, am Anfang nur
   `/index.html`. In einer zweiten Liste steht alles, was er schon einmal in
   die Queue gestellt hat.
2. Er nimmt den ersten Pfad aus der Queue und holt die Seite. Gibt es sie
   nicht, geht es mit dem nächsten Pfad weiter.
3. Er hebt die Seite auf und stellt jedes Linkziel hinten in die Queue, das er
   noch nie gesehen hat.
4. Das wiederholt er, bis die Queue leer ist.

Ohne die zweite Liste liefe er im Kreis: Von der Katze geht es zum Hund und vom
Hund zurück zur Katze. Dieses Vorgehen heißt **Breitensuche**: zuerst alle
Seiten, die einen Link entfernt sind, dann alle, die zwei Links entfernt sind.

_uses(queue, breitensuche)

Im Ordner `tierlexikon` liegt auch eine Datei `geheim.html`.

_task(geheim)

<!-- language: ruby -->
<div class='hint own' data-check='crawl'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>crawl(start, fetch)</code>. Sie gibt einen Hash zurück, der jeden gefundenen Pfad auf das HTML seiner Seite abbildet. Die Seiten holt sie nicht selbst mit <code>fetch_page</code>, sondern mit <code>fetch.call(path)</code>: Dein Programm gibt ihr nachher <code>method(:fetch_page)</code>, die Prüfung eine kleine ausgedachte Website.
</div>

<!-- hint -->
Du brauchst drei Variablen: den Hash `pages` für das Ergebnis, die Queue als
Array mit `start` darin und ein Array `seen`, ebenfalls mit `start`. `shift`
nimmt das erste Element aus einem Array, `<<` hängt hinten eins an.
<!-- /hint -->

<!-- hint -->
In einer Schleife, solange die Queue nicht leer ist (`until queue.empty?`):
den ersten Pfad nehmen, die Seite holen, bei `nil` mit `next` weitermachen,
sonst die Seite in `pages` eintragen und jedes Linkziel aus `links(html)`, das
nicht in `seen` steht, in `seen` und in die Queue stellen.
<!-- /hint -->

<!-- hint -->
```ruby
def crawl(start, fetch)
  pages = {}
  queue = [start]
  seen = [start]
  until queue.empty?
    path = queue.shift
    html = fetch.call(path)
    next if html.nil?

    pages[path] = html
    # hier: jedes neue Linkziel aus links(html) in seen und in die Queue
  end
  pages
end
```
<!-- /hint -->

<!-- solution -->
```ruby
# Alle Seiten, die von start aus über Links erreichbar sind, als Hash von
# Pfad zu HTML; fetch holt eine Seite
def crawl(start, fetch)
  pages = {}
  queue = [start]
  seen = [start]
  until queue.empty?
    path = queue.shift
    html = fetch.call(path)
    next if html.nil?

    pages[path] = html
    links(html).each do |link|
      next if seen.include?(link)

      seen << link
      queue << link
    end
  end
  pages
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='crawl'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>crawl(start, fetch)</code>. Sie gibt ein <code>dict</code> zurück, das jeden gefundenen Pfad auf das HTML seiner Seite abbildet. Die Seiten holt sie nicht selbst mit <code>fetch_page</code>, sondern mit <code>fetch(path)</code>: Dein Programm gibt ihr nachher die Funktion <code>fetch_page</code>, die Prüfung eine kleine ausgedachte Website.
</div>

<!-- hint -->
Du brauchst drei Variablen: das `dict` `pages` für das Ergebnis, die Queue als
Liste mit `start` darin und eine Liste `seen`, ebenfalls mit `start`.
`pop(0)` nimmt das erste Element aus einer Liste, `append` hängt hinten eins
an.
<!-- /hint -->

<!-- hint -->
In einer Schleife, solange die Queue nicht leer ist (`while queue:`): den
ersten Pfad nehmen, die Seite holen, bei `None` mit `continue` weitermachen,
sonst die Seite in `pages` eintragen und jedes Linkziel aus `links(html)`, das
nicht in `seen` steht, in `seen` und in die Queue stellen.
<!-- /hint -->

<!-- hint -->
```python
def crawl(start, fetch):
    pages = {}
    queue = [start]
    seen = [start]
    while queue:
        path = queue.pop(0)
        html = fetch(path)
        if html is None:
            continue
        pages[path] = html
        # hier: jedes neue Linkziel aus links(html) in seen und in die Queue
    return pages
```
<!-- /hint -->

<!-- solution -->
```python
def crawl(start, fetch):
    """Alle Seiten, die von start aus über Links erreichbar sind, als dict von
    Pfad zu HTML; fetch holt eine Seite"""
    pages = {}
    queue = [start]
    seen = [start]
    while queue:
        path = queue.pop(0)
        html = fetch(path)
        if html is None:
            continue
        pages[path] = html
        for link in links(html):
            if link not in seen:
                seen.append(link)
                queue.append(link)
    return pages
```
<!-- /solution -->
<!-- /language -->

_task(crawl)

<!-- language: ruby -->
<!-- stage: stufen/search-1.rb as search.rb -->

Jetzt lässt du ihn auf das Tierlexikon los. Schreib in `main.rb`:

```ruby
require_relative 'search'

pages = crawl('/index.html', method(:fetch_page))
puts "#{pages.size} Seiten gefunden: #{pages.keys.join(', ')}"
```

`method(:fetch_page)` ist die Funktion selbst, ohne sie aufzurufen; `crawl`
ruft sie dann für jeden Pfad mit `call` auf. Starte das Programm, während der
Webserver im zweiten Terminal läuft:

```bash
ruby main.rb
```

<!-- stage: stufen/main-1.rb as main.rb -->
<!-- /language -->
<!-- language: python -->
<!-- stage: stufen/search-1.py as search.py -->

Jetzt lässt du ihn auf das Tierlexikon los. Schreib in `main.py`:

```python
from search import *

pages = crawl('/index.html', fetch_page)
print(f'{len(pages)} Seiten gefunden:', ', '.join(pages))
```

`fetch_page` ohne Klammern ist die Funktion selbst, ohne sie aufzurufen;
`crawl` ruft sie dann für jeden Pfad auf. Starte das Programm, während der
Webserver im zweiten Terminal läuft:

```bash
python3 main.py
```

<!-- stage: stufen/main-1.py as main.py -->
<!-- /language -->

_task(seiten)

<!-- after: geheim -->
`geheim.html` liegt auf dem Server, aber keine Seite verlinkt sie. Ein Crawler
findet nur, wohin ein Link führt. Was nirgends verlinkt ist, fehlt in jeder
Suchmaschine, auch wenn jeder es mit der genauen Adresse abrufen kann.
<!-- /after -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass den Crawler bei <code>/pinguin.html</code> statt bei <code>/index.html</code> anfangen. Achte darauf, in welcher Reihenfolge er die Seiten findet und welche zuerst kommt.
</div>

## Der Index

Um ein Wort zu suchen, könnte die Suchmaschine jedes Mal alle Seiten
durchlesen. Bei Milliarden Seiten geht das nicht. Darum schreibt sie einmal
auf, wo jedes Wort steht. Für zwei kleine Seiten sieht das so aus:

<!-- language: ruby -->
```ruby
pages = { "/a.html" => "<p>Der Igel schläft.</p>", "/b.html" => "<p>Der Hund schläft nicht.</p>" }
build_index(pages)
# {
#   "der" => ["/a.html", "/b.html"],
#   "igel" => ["/a.html"],
#   "schläft" => ["/a.html", "/b.html"],
#   "hund" => ["/b.html"],
#   "nicht" => ["/b.html"]
# }
```
<!-- /language -->
<!-- language: python -->
```python
pages = {'/a.html': '<p>Der Igel schläft.</p>', '/b.html': '<p>Der Hund schläft nicht.</p>'}
build_index(pages)
# {
#     'der': ['/a.html', '/b.html'],
#     'igel': ['/a.html'],
#     'schläft': ['/a.html', '/b.html'],
#     'hund': ['/b.html'],
#     'nicht': ['/b.html'],
# }
```
<!-- /language -->

Andersherum als eine Seite, die zu jedem Pfad die Wörter kennt, kennt der Index
zu jedem Wort die Pfade. Darum heißt er **invertierter Index**. Die Tags
gehören nicht zu den Wörtern, und groß oder klein geschrieben ist egal: Wer
»Igel« sucht, findet auch »igel«.

<!-- language: ruby -->
Diese Funktion zerlegt eine Seite in ihre Wörter; schreib sie unter `links`:

```ruby
# Die Wörter einer Seite, klein geschrieben, ohne die Tags
def words(html)
  html.gsub(/<[^>]*>/, ' ').downcase.scan(/\p{L}+/)
end
```

`gsub` ersetzt jedes Tag, alles von `<` bis `>`, durch ein Leerzeichen,
`downcase` macht alles klein, und `scan(/\p{L}+/)` findet jedes Wort aus
Buchstaben, auch mit Umlauten.
<!-- /language -->
<!-- language: python -->
Diese Funktion zerlegt eine Seite in ihre Wörter; schreib sie unter `links`:

```python
def words(html):
    """Die Wörter einer Seite, klein geschrieben, ohne die Tags"""
    return re.findall(r'[^\W\d_]+', re.sub(r'<[^>]*>', ' ', html).lower())
```

`re.sub` ersetzt jedes Tag, alles von `<` bis `>`, durch ein Leerzeichen,
`lower` macht alles klein, und `[^\W\d_]+` findet jedes Wort aus Buchstaben,
auch mit Umlauten.
<!-- /language -->

<!-- language: ruby -->
<div class='hint own' data-check='index'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>build_index(pages)</code>. Sie bekommt den Hash, den <code>crawl</code> liefert, und gibt den Index zurück: zu jedem Wort die Pfade der Seiten, auf denen es steht, in der Reihenfolge der Seiten und jeden Pfad nur einmal, auch wenn das Wort auf einer Seite öfter vorkommt.
</div>

<!-- hint -->
Zwei Schleifen ineinander: außen über die Seiten mit `pages.each do |path,
html|`, innen über ihre Wörter. `uniq` macht aus `words(html)` eine Liste, in
der jedes Wort nur einmal steht.
<!-- /hint -->

<!-- hint -->
```ruby
def build_index(pages)
  index = {}
  pages.each do |path, html|
    words(html).uniq.each do |word|
      # hier: die Liste für word anlegen, wenn sie fehlt, und path anhängen
    end
  end
  index
end
```
<!-- /hint -->

<!-- solution -->
```ruby
# Für jedes Wort die Seiten, auf denen es steht
def build_index(pages)
  index = {}
  pages.each do |path, html|
    words(html).uniq.each do |word|
      index[word] ||= []
      index[word] << path
    end
  end
  index
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='index'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>build_index(pages)</code>. Sie bekommt das <code>dict</code>, das <code>crawl</code> liefert, und gibt den Index zurück: zu jedem Wort die Pfade der Seiten, auf denen es steht, in der Reihenfolge der Seiten und jeden Pfad nur einmal, auch wenn das Wort auf einer Seite öfter vorkommt.
</div>

<!-- hint -->
Zwei Schleifen ineinander: außen über die Seiten mit
`for path, html in pages.items():`, innen über ihre Wörter.
`dict.fromkeys(words(html))` behält jedes Wort nur einmal, in seiner
Reihenfolge.
<!-- /hint -->

<!-- hint -->
```python
def build_index(pages):
    index = {}
    for path, html in pages.items():
        for word in dict.fromkeys(words(html)):
            # hier: die Liste für word anlegen, wenn sie fehlt, und path anhängen
    return index
```
<!-- /hint -->

<!-- solution -->
```python
def build_index(pages):
    """Für jedes Wort die Seiten, auf denen es steht"""
    index = {}
    for path, html in pages.items():
        for word in dict.fromkeys(words(html)):
            if word not in index:
                index[word] = []
            index[word].append(path)
    return index
```
<!-- /solution -->
<!-- /language -->

_task(index)

### Suchen

Eine Suche nach »nachts insekten« soll die Seiten finden, auf denen beide
Wörter stehen. Im Index stehen zu jedem Wort seine Seiten; gesucht sind die
Seiten, die in beiden Listen vorkommen, ihre **Schnittmenge**.

<!-- language: ruby -->
<div class='hint own' data-check='suchen'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>search(index, query)</code>. Sie zerlegt die Anfrage mit <code>words</code> in Wörter und gibt die Pfade der Seiten zurück, auf denen jedes Wort steht, nach dem Alphabet sortiert. Steht ein Wort auf keiner Seite, oder ist die Anfrage leer, gibt sie eine leere Liste zurück.
</div>

<!-- hint -->
Fang mit der Liste des ersten Wortes an. Für jedes weitere Wort behältst du
nur die Pfade, die auch in dessen Liste stehen: `a & b` ist in Ruby die
Schnittmenge zweier Arrays. Ein Wort, das nicht im Index steht, hat die leere
Liste `[]`.
<!-- /hint -->

<!-- hint -->
```ruby
def search(index, query)
  results = nil
  words(query).each do |word|
    found = index[word] || []
    # hier: beim ersten Wort results = found, danach die Schnittmenge
  end
  (results || []).sort
end
```
<!-- /hint -->

<!-- solution -->
```ruby
# Die Seiten, auf denen jedes Wort der Anfrage steht, nach dem Alphabet
def search(index, query)
  results = nil
  words(query).each do |word|
    found = index[word] || []
    results = results.nil? ? found : results & found
  end
  (results || []).sort
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='suchen'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>search(index, query)</code>. Sie zerlegt die Anfrage mit <code>words</code> in Wörter und gibt die Pfade der Seiten zurück, auf denen jedes Wort steht, nach dem Alphabet sortiert. Steht ein Wort auf keiner Seite, oder ist die Anfrage leer, gibt sie eine leere Liste zurück.
</div>

<!-- hint -->
Fang mit der Liste des ersten Wortes an. Für jedes weitere Wort behältst du
nur die Pfade, die auch in dessen Liste stehen. Ein Wort, das nicht im Index
steht, hat die leere Liste: `index.get(word, [])`.
<!-- /hint -->

<!-- hint -->
```python
def search(index, query):
    results = None
    for word in words(query):
        found = index.get(word, [])
        # hier: beim ersten Wort results = found, danach nur die Pfade, die auch in found stehen
    return sorted(results or [])
```
<!-- /hint -->

<!-- solution -->
```python
def search(index, query):
    """Die Seiten, auf denen jedes Wort der Anfrage steht, nach dem Alphabet"""
    results = None
    for word in words(query):
        found = index.get(word, [])
        results = found if results is None else [path for path in results if path in found]
    return sorted(results or [])
```
<!-- /solution -->
<!-- /language -->

_task(suchen)

<!-- language: ruby -->
<!-- stage: stufen/search-2.rb as search.rb -->

Jetzt fragt `main.rb` nach Suchwörtern, bis du eine leere Zeile eingibst.
Hänge unten an:

```ruby
~ puts "#{pages.size} Seiten gefunden: #{pages.keys.join(', ')}"
index = build_index(pages)

loop do
  print 'Suche: '
  query = gets
  break if query.nil? || query.strip.empty?

  results = search(index, query)
  puts '  Nichts gefunden.' if results.empty?
  results.each { |path| puts "  #{path}" }
end
```

<!-- stage: stufen/main-2.rb as main.rb -->
<!-- /language -->
<!-- language: python -->
<!-- stage: stufen/search-2.py as search.py -->

Jetzt fragt `main.py` nach Suchwörtern, bis du eine leere Zeile eingibst.
Hänge unten an:

```python
~ print(f'{len(pages)} Seiten gefunden:', ', '.join(pages))
index = build_index(pages)

while True:
    query = input('Suche: ')
    if not query.strip():
        break
    results = search(index, query)
    if not results:
        print('  Nichts gefunden.')
    for path in results:
        print(' ', path)
```

<!-- stage: stufen/main-2.py as main.py -->
<!-- /language -->

_task(treffer)

## Wer steht oben?

Eine echte Suchmaschine findet zu fast jedem Wort Tausende Seiten, und kaum
jemand schaut über die ersten zehn hinaus. Welche oben stehen, entscheidet das
**Ranking**. Eine naheliegende Idee: Je öfter das Suchwort auf einer Seite
steht, desto mehr geht es dort darum.

<!-- language: ruby -->
Schreib diese Funktion unter `search`:

```ruby
# Die Treffer, die Seite mit dem Wort am häufigsten zuerst
def rank_by_count(results, pages, word)
  results.sort_by { |path| [-words(pages[path]).count(word), path] }
end
```

`sort_by` sortiert nach dem, was der Block liefert: zuerst nach der Anzahl,
mit Minus davor, damit die größte vorn steht, bei gleicher Anzahl nach dem
Pfad. In `main.rb` ordnet die Suche ihre Treffer jetzt nach dem ersten Wort der
Anfrage:

```ruby
~   break if query.nil? || query.strip.empty?
~
  results = rank_by_count(search(index, query), pages, words(query).first)
~   puts '  Nichts gefunden.' if results.empty?
```

<!-- stage: stufen/search-3.rb as search.rb -->
<!-- stage: stufen/main-3.rb as main.rb -->
<!-- /language -->
<!-- language: python -->
Schreib diese Funktion unter `search`:

```python
def rank_by_count(results, pages, word):
    """Die Treffer, die Seite mit dem Wort am häufigsten zuerst"""
    return sorted(results, key=lambda path: (-words(pages[path]).count(word), path))
```

`sorted` sortiert nach dem, was `key` liefert: zuerst nach der Anzahl, mit
Minus davor, damit die größte vorn steht, bei gleicher Anzahl nach dem Pfad.
In `main.py` ordnet die Suche ihre Treffer jetzt nach dem ersten Wort der
Anfrage:

```python
~         break
    results = rank_by_count(search(index, query), pages, words(query)[0])
~     if not results:
```

<!-- stage: stufen/search-3.py as search.py -->
<!-- stage: stufen/main-3.py as main.py -->
<!-- /language -->

_task(erster)

<!-- after: erster -->
Ganz oben steht `katzenfutter-billig.html`, eine Anzeige, die kaum etwas
anderes enthält als »Katze«, achtzehnmal. Die Seite über die Hauskatze kommt
erst danach. Wörter so oft zu wiederholen, bis eine Suchmaschine anbeißt,
heißt **Keyword-Stuffing**. Frühe Suchmaschinen der Neunzigerjahre ordneten
ungefähr so wie deine, und Seiten dieser Art standen bei ihnen oft oben.
<!-- /after -->

### Links als Stimmen

Ein Link ist eine Empfehlung: Wer auf eine Seite verlinkt, hält sie für
lesenswert. Auf die Seite über die Hauskatze verlinken die Startseite, der Hund
und die Amsel. Auf die Anzeige verlinkt nur der Hund, und sich selbst zu
verlinken, zählt nicht.

<!-- language: ruby -->
<div class='hint own' data-check='eingehend'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>incoming(pages)</code>. Sie gibt einen Hash zurück, der für jede Seite zählt, wie viele <em>andere</em> Seiten auf sie verlinken. Ein Link einer Seite auf sich selbst zählt nicht, mehrere Links derselben Seite auf dasselbe Ziel zählen einmal, und Ziele, die nicht in <code>pages</code> stehen, kommen nicht vor. Seiten, auf die keine verlinkt, fehlen im Hash.
</div>

<!-- hint -->
Geh die Seiten durch und für jede ihre Linkziele aus `links(html)`, mit `uniq`
jedes nur einmal. Ein Ziel zählst du, wenn es nicht die Seite selbst ist und
in `pages` steht (`pages.key?(link)`).
<!-- /hint -->

<!-- hint -->
```ruby
def incoming(pages)
  counts = {}
  pages.each do |path, html|
    links(html).uniq.each do |link|
      # hier: Selbstlinks und Ziele außerhalb von pages auslassen, die anderen zählen
    end
  end
  counts
end
```
<!-- /hint -->

<!-- solution -->
```ruby
# Für jede Seite, wie viele andere Seiten auf sie verlinken
def incoming(pages)
  counts = {}
  pages.each do |path, html|
    links(html).uniq.each do |link|
      next if link == path || !pages.key?(link)

      counts[link] = (counts[link] || 0) + 1
    end
  end
  counts
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='eingehend'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>incoming(pages)</code>. Sie gibt ein <code>dict</code> zurück, das für jede Seite zählt, wie viele <em>andere</em> Seiten auf sie verlinken. Ein Link einer Seite auf sich selbst zählt nicht, mehrere Links derselben Seite auf dasselbe Ziel zählen einmal, und Ziele, die nicht in <code>pages</code> stehen, kommen nicht vor. Seiten, auf die keine verlinkt, fehlen im <code>dict</code>.
</div>

<!-- hint -->
Geh die Seiten durch und für jede ihre Linkziele aus `links(html)`, mit
`dict.fromkeys` jedes nur einmal. Ein Ziel zählst du, wenn es nicht die Seite
selbst ist und in `pages` steht.
<!-- /hint -->

<!-- hint -->
```python
def incoming(pages):
    counts = {}
    for path, html in pages.items():
        for link in dict.fromkeys(links(html)):
            # hier: Selbstlinks und Ziele außerhalb von pages auslassen, die anderen zählen
    return counts
```
<!-- /hint -->

<!-- solution -->
```python
def incoming(pages):
    """Für jede Seite, wie viele andere Seiten auf sie verlinken"""
    counts = {}
    for path, html in pages.items():
        for link in dict.fromkeys(links(html)):
            if link != path and link in pages:
                counts[link] = counts.get(link, 0) + 1
    return counts
```
<!-- /solution -->
<!-- /language -->

_task(eingehend)

<!-- language: ruby -->
Unter `incoming` kommt die Funktion, die danach ordnet:

```ruby
# Die Treffer, die Seite mit den meisten eingehenden Links zuerst
def rank_by_links(results, counts)
  results.sort_by { |path| [-(counts[path] || 0), path] }
end
```

und `main.rb` zählt die Links einmal nach dem Index und ordnet damit:

```ruby
~ index = build_index(pages)
counts = incoming(pages)
~
~ loop do
~   …
  results = rank_by_links(search(index, query), counts)
~   puts '  Nichts gefunden.' if results.empty?
```

<!-- stage: loesung/search.rb as search.rb -->
<!-- stage: loesung/main.rb as main.rb -->
<!-- /language -->
<!-- language: python -->
Unter `incoming` kommt die Funktion, die danach ordnet:

```python
def rank_by_links(results, counts):
    """Die Treffer, die Seite mit den meisten eingehenden Links zuerst"""
    return sorted(results, key=lambda path: (-counts.get(path, 0), path))
```

und `main.py` zählt die Links einmal nach dem Index und ordnet damit:

```python
~ index = build_index(pages)
counts = incoming(pages)
~
~ while True:
~     …
    results = rank_by_links(search(index, query), counts)
~     if not results:
```

<!-- stage: loesung/search.py as search.py -->
<!-- stage: loesung/main.py as main.py -->
<!-- /language -->

Such noch einmal nach »katze«: Jetzt steht die Hauskatze oben, die Anzeige
unten.

_task(startseite-links)

### PageRank

Genau diese Idee hatten 1998 Sergey Brin und Larry Page, zwei Doktoranden in
Stanford, und gründeten damit Google. Ihr Verfahren heißt **PageRank** und geht
einen Schritt weiter als deins: Ein Link zählt umso mehr, je wichtiger die
Seite ist, von der er kommt, und wichtig ist eine Seite, auf die wichtige
Seiten verlinken. Ein Link von einer großen Zeitung wiegt mehr als einer von
einer Seite, die niemand kennt.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>The Anatomy of a Large-Scale Hypertextual Web Search Engine</strong>, Sergey Brin und Lawrence Page, Computer Networks and ISDN Systems 30, 1998. Der Aufsatz, in dem Google mit Crawler, invertiertem Index und PageRank beschrieben wird: <a href='https://snap.stanford.edu/class/cs224w-readings/Brin98Anatomy.pdf'>PDF</a>, auch bei <a href='https://doi.org/10.1016/S0169-7552(98)00110-X'>Elsevier</a>.</p>
</div>

_task(linkfarm)

## Geschrieben für die Suchmaschine

Die Anzeige für Katzenfutter hat dein erstes Ranking mit einem einfachen Trick
gewonnen: Sie wiederholt das Wort, nach dem gesucht wird. Weil oben stehen Geld
bringt, lebt eine ganze Branche davon, Seiten nach oben zu bringen: die
**Suchmaschinenoptimierung**, englisch Search Engine Optimization (SEO). Seit
es Suchmaschinen gibt, geht es hin und her: Die Optimierer finden einen Trick,
die Suchmaschine lernt, ihn zu erkennen, und die Optimierer suchen den nächsten.

- **Wörter:** Ende der 1990er Jahre füllten Seiten sich mit Suchwörtern, oft
  in weißer Schrift auf weißem Grund, unsichtbar für Menschen, aber nicht für
  den Crawler.
- **Links:** Als Links zählten, kauften und tauschten Seiten Links und bauten
  Linkfarmen. 2012 stufte Google mit einer Änderung namens Penguin Seiten
  herab, die das taten.
- **Masse:** Firmen ließen billig Texte zu allem schreiben, wonach oft gesucht
  wird, sogenannte Inhaltsfarmen; 2011 stufte Google sie mit einer Änderung
  namens Panda herab. Heute schreiben Sprachmodelle solche Texte in Sekunden,
  viel größere als das aus [»Ein winziges Sprachmodell«](/sprachmodell).

Was das aus den Treffern macht, haben Informatiker der Universität Leipzig und
der Bauhaus-Universität Weimar ein Jahr lang gemessen. Sie stellten Google,
Bing und DuckDuckGo 7392 Suchanfragen nach Testberichten, etwa nach den besten
Kopfhörern. Im Web verdienen nur wenige Testberichte an **Affiliate-Links**:
Links zu einem Shop, für die der Shop bei jedem Kauf einen Anteil zahlt. Unter
den Treffern war es die Mehrheit. Wenn eine Suchmaschine solche Seiten
herabstufte, half das nur eine Weile.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Is Google Getting Worse? A Longitudinal Investigation of SEO Spam in Search Engines</strong>, Janek Bevendorff, Matti Wiegmann, Martin Potthast und Benno Stein, ECIR 2024. Ein Jahr Suchanfragen nach Testberichten bei drei Suchmaschinen: <a href='https://doi.org/10.1007/978-3-031-56063-7_4'>Springer</a>.</p>
</div>

Manches, was Suchmaschinen belohnen, hilft auch Menschen: ein Titel, der sagt,
worum es geht, Bilder mit Alt-Text, eine Seite, die schnell lädt. Schwierig
wird es, wenn eine Seite für den Crawler geschrieben ist statt für die, die sie
lesen.

<div class='hint try'>
<strong>Ausprobieren:</strong> Such nach »beste Kopfhörer« oder einem anderen Gerät. Achte darauf, wie viele der ersten zehn Treffer Listen mit Links zum Kaufen sind, und ob eine Seite sagt, wer die Geräte wirklich ausprobiert hat.
</div>

## Wer entscheidet, was gefunden wird

Deine Suchmaschine hat zwei Rankings gesehen, und jedes bevorzugt andere
Seiten. Nach der Anzahl der Wörter gewinnt, wer sein Wort am häufigsten
wiederholt. Nach den Links gewinnt, wer schon bekannt ist; eine neue Seite, auf
die noch niemand verlinkt, steht unten, egal wie gut sie ist.

- Echte Suchmaschinen verbinden Hunderte solcher Signale: Links, Wörter, wie
  neu eine Seite ist, wo du gerade bist, was andere angeklickt haben. Wie genau,
  verraten sie nicht, auch damit niemand das Ranking so leicht ausnutzt wie das
  Katzenfutter das deine.
- In Deutschland läuft der weitaus größte Teil aller Suchanfragen über eine
  einzige Suchmaschine. Was sie unten einordnet, findet kaum jemand.
- Ein Crawler fragt eine Website zuerst nach der Datei `/robots.txt`, in der
  steht, was er nicht lesen soll. Daran hält er sich freiwillig; es ist eine
  Bitte, kein Schloss.

Und manchmal soll eine Seite gar nicht mehr gefunden werden, etwa ein alter
Zeitungsartikel über die Schulden eines Menschen, die er längst bezahlt hat.

_task(vergessen)

<!-- after: vergessen -->
Über so einen Fall hat der Europäische Gerichtshof 2014 entschieden
([Google Spain](https://curia.europa.eu/juris/liste.jsf?num=C-131/12)): Eine
Suchmaschine muss einen Link aus den Treffern zu einem Namen nehmen, wenn das
Recht der Person schwerer wiegt als das Interesse der Öffentlichkeit, ihn zu
finden. Darüber entscheidet zuerst die Suchmaschine selbst, auf einen Antrag
der Person; wer mit ihrer Antwort nicht einverstanden ist, wendet sich an die
Datenschutzbehörde oder ein Gericht. Der Artikel selbst bleibt dabei, wo er
ist; er ist nur über den Namen nicht mehr zu finden. Seit 2018 steht dieses
**Recht auf Vergessenwerden** in Artikel 17 der DSGVO.
<!-- /after -->

_task(reihenfolge)

<!-- extra -->
## Ins echte Web

Dein Crawler kennt bisher nur das Tierlexikon auf deinem eigenen Webserver.
Der Workspace erreicht aber das ganze Internet, und so könnte dein Crawler auch
eine echte Website durchsuchen, etwa die deiner Schule. Draußen fehlt ihm
einiges:

- Echte Websites schreiben ihre Links oft als ganze Adresse
  (`https://www.beispiel-schule.de/kontakt`) oder relativ zu der Seite, auf
  der sie stehen (`hund.html` statt `/tiere/hund.html`). `links` kennt nur
  Pfade, die mit `/` anfangen.
- Er holt Seite um Seite, so schnell er kann, und hört erst auf, wenn er alles
  hat. Ein kleiner Webserver merkt so einen Ansturm, und die Crawler einer
  ganzen Klasse auf einmal erst recht.
- Er fragt nicht, was er lesen soll. Das steht in der Datei `/robots.txt`.

Ein **höflicher Crawler** holt höchstens 30 Seiten, wartet vor jeder Anfrage
eine Sekunde, hält sich an `robots.txt` und bleibt auf der Website, bei der er
angefangen hat. Mit jeder Anfrage schickt er seinen **User-Agent**, einen
Namen, der sagt, wer da fragt, so wie dein Browser seinen.

### Der Crawler für draußen

<!-- language: ruby -->
Lege neben `search.rb` eine neue Datei `webcrawler.rb` an:

```ruby
require 'net/http'
require 'uri'

# Wer der Crawler ist; viele Websites schreiben das in ihr Protokoll
USER_AGENT = 'Hackschule-Crawler (Informatikkurs, Hackschule Workspace)'
# Höchstens so viele Seiten, und so viele Sekunden Pause vor jeder
MAX_PAGES = 30
PAUSE = 1

# Die Ziele aller Links einer Seite, so wie sie im HTML stehen
def href_targets(html)
  html.scan(/<a\s[^>]*?href\s*=\s*["']([^"']*)["']/i).flatten
end

# Was robots.txt allen Crawlern verbietet (User-agent: *), als Liste von
# Anfängen von Pfaden; vereinfacht, RFC 9309 kennt noch mehr
def robots_rules(text)
  rules = []
  applies = false
  text.each_line do |line|
    key, value = line.sub(/#.*/, '').split(':', 2).map(&:strip)
    next if value.nil?

    case key.downcase
    when 'user-agent' then applies = (value == '*')
    when 'disallow' then rules << value if applies && !value.empty?
    end
  end
  rules
end

# Eine Anfrage mit dem User-Agent des Crawlers
def get(url)
  uri = URI(url)
  Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https',
                  open_timeout: 10, read_timeout: 10) do |http|
    http.get(uri.request_uri, 'User-Agent' => USER_AGENT)
  end
end

# robots.txt der Website, oder nichts, wenn es keine gibt
def fetch_robots(start)
  response = get(URI.join(start, '/robots.txt').to_s)
  response.is_a?(Net::HTTPSuccess) ? response.body.force_encoding('UTF-8').scrub : ''
rescue StandardError
  ''
end

# Eine HTML-Seite als [Adresse, HTML], oder nil; folgt Weiterleitungen,
# solange sie auf derselben Website bleiben
def fetch_url(url)
  host = URI(url).host
  5.times do
    response = get(url)
    if response.is_a?(Net::HTTPRedirection)
      url = URI.join(url, response['location']).to_s
      return nil unless URI(url).host == host

      next
    end
    return nil unless response.is_a?(Net::HTTPSuccess) && response['content-type'].to_s.start_with?('text/html')

    return [url, response.body.force_encoding('UTF-8').scrub]
  end
  nil
rescue StandardError
  nil
end

# Höchstens max_pages Seiten der Website von start, mit Pause und nach
# robots.txt, als Hash von Adresse zu HTML
def polite_crawl(start, max_pages = MAX_PAGES)
  rules = robots_rules(fetch_robots(start))
  puts "robots.txt verbietet: #{rules.empty? ? 'nichts' : rules.join(', ')}"
  pages = {}
  queue = [start]
  seen = [start]
  until queue.empty? || pages.size >= max_pages
    url = queue.shift
    unless allowed?(rules, URI(url).request_uri)
      puts "übersprungen: #{url}"
      next
    end
    sleep PAUSE
    found = fetch_url(url)
    next if found.nil?

    address, html = found
    puts "geholt: #{address}"
    pages[address] = html
    absolute_links(html, address).each do |link|
      next if seen.include?(link)

      seen << link
      queue << link
    end
  end
  pages
end
```

<!-- stage: stufen/webcrawler-0.rb as webcrawler.rb -->

- `href_targets` findet das Ziel jedes Links, so wie es im HTML steht, auch
  in einfachen Anführungszeichen.
- `get` schickt eine Anfrage mit dem User-Agent des Crawlers; nach zehn
  Sekunden ohne Antwort gibt sie auf.
- `fetch_url` holt eine Seite. Leitet der Server weiter, etwa von `http` auf
  `https`, folgt sie, solange die Weiterleitung auf derselben Website bleibt.
  Ist die Antwort keine HTML-Seite, sondern ein Bild oder ein PDF, oder lehnt
  der Server ab, gibt sie `nil` zurück, und der Crawler macht mit der nächsten
  Adresse weiter.
- `polite_crawl` ist dein `crawl`, nur mit ganzen Adressen statt Pfaden, mit
  der Pause und der Grenze, und mit den Regeln aus `robots.txt`. Zwei
  Funktionen, die es aufruft, fehlen noch: `absolute_links` und `allowed?`.
<!-- /language -->
<!-- language: python -->
Lege neben `search.py` eine neue Datei `webcrawler.py` an:

```python
import re
import time
import urllib.error
import urllib.request
from urllib.parse import urldefrag, urljoin, urlparse

# Wer der Crawler ist; viele Websites schreiben das in ihr Protokoll
USER_AGENT = 'Hackschule-Crawler (Informatikkurs, Hackschule Workspace)'
# Höchstens so viele Seiten, und so viele Sekunden Pause vor jeder
MAX_PAGES = 30
PAUSE = 1


def href_targets(html):
    """Die Ziele aller Links einer Seite, so wie sie im HTML stehen"""
    return re.findall(r'<a\s[^>]*?href\s*=\s*["\']([^"\']*)["\']', html, re.IGNORECASE)


def robots_rules(text):
    """Was robots.txt allen Crawlern verbietet (User-agent: *), als Liste von
    Anfängen von Pfaden; vereinfacht, RFC 9309 kennt noch mehr"""
    rules = []
    applies = False
    for line in text.splitlines():
        line = line.split('#', 1)[0]
        if ':' not in line:
            continue
        key, value = (part.strip() for part in line.split(':', 1))
        if key.lower() == 'user-agent':
            applies = value == '*'
        elif key.lower() == 'disallow' and applies and value:
            rules.append(value)
    return rules


def get(url):
    """Eine Anfrage mit dem User-Agent des Crawlers"""
    request = urllib.request.Request(url, headers={'User-Agent': USER_AGENT})
    return urllib.request.urlopen(request, timeout=10)


def fetch_robots(start):
    """robots.txt der Website, oder nichts, wenn es keine gibt"""
    try:
        with get(urljoin(start, '/robots.txt')) as response:
            return response.read().decode('utf-8', errors='replace')
    except Exception:
        return ''


def fetch_url(url):
    """Eine HTML-Seite als (Adresse, HTML), oder None; urlopen folgt
    Weiterleitungen, die Seite muss aber auf derselben Website bleiben"""
    try:
        with get(url) as response:
            address = response.geturl()
            if urlparse(address).netloc != urlparse(url).netloc:
                return None
            if response.headers.get_content_type() != 'text/html':
                return None
            return address, response.read().decode('utf-8', errors='replace')
    except Exception:
        return None


def request_path(url):
    """Der Pfad einer Adresse mit dem Teil hinter ?, wie robots.txt ihn meint"""
    parts = urlparse(url)
    return (parts.path or '/') + ('?' + parts.query if parts.query else '')


def polite_crawl(start, max_pages=MAX_PAGES):
    """Höchstens max_pages Seiten der Website von start, mit Pause und nach
    robots.txt, als dict von Adresse zu HTML"""
    rules = robots_rules(fetch_robots(start))
    print('robots.txt verbietet:', ', '.join(rules) if rules else 'nichts')
    pages = {}
    queue = [start]
    seen = [start]
    while queue and len(pages) < max_pages:
        url = queue.pop(0)
        if not allowed(rules, request_path(url)):
            print('übersprungen:', url)
            continue
        time.sleep(PAUSE)
        found = fetch_url(url)
        if found is None:
            continue
        address, html = found
        print('geholt:', address)
        pages[address] = html
        for link in absolute_links(html, address):
            if link not in seen:
                seen.append(link)
                queue.append(link)
    return pages
```

<!-- stage: stufen/webcrawler-0.py as webcrawler.py -->

- `href_targets` findet das Ziel jedes Links, so wie es im HTML steht, auch
  in einfachen Anführungszeichen.
- `get` schickt eine Anfrage mit dem User-Agent des Crawlers; nach zehn
  Sekunden ohne Antwort gibt sie auf.
- `fetch_url` holt eine Seite. Leitet der Server weiter, etwa von `http` auf
  `https`, folgt `urlopen` der Weiterleitung; die Seite muss aber auf derselben
  Website bleiben. Ist die Antwort keine HTML-Seite, sondern ein Bild oder ein
  PDF, oder lehnt der Server ab, gibt sie `None` zurück, und der Crawler macht
  mit der nächsten Adresse weiter.
- `polite_crawl` ist dein `crawl`, nur mit ganzen Adressen statt Pfaden, mit
  der Pause und der Grenze, und mit den Regeln aus `robots.txt`. Zwei
  Funktionen, die es aufruft, fehlen noch: `absolute_links` und `allowed`.
<!-- /language -->

### Ganze Adressen

Steht auf der Seite `https://schule.example/tiere/katze.html` ein Link auf
`hund.html`, meint er die Seite im selben Ordner,
`https://schule.example/tiere/hund.html`. Ein Link auf `/index.html` meint
`https://schule.example/index.html`, und eine ganze Adresse bleibt, wie sie
ist. Was hinter einem `#` steht, ist nur eine Stelle auf einer Seite: Für den
Crawler ist `index.html#oben` dieselbe Seite wie `index.html`.

<!-- language: ruby -->
Das Rechnen nimmt dir `URI.join` ab:

```ruby
URI.join('https://schule.example/tiere/katze.html', 'hund.html').to_s
# => "https://schule.example/tiere/hund.html"
```

Ein `URI` kennt die Teile einer Adresse: `scheme` (`"https"`), `host`
(`"schule.example"`), `port` und `fragment`, das, was hinter `#` steht.

<div class='hint own' data-check='ganze-adressen'>
<strong>Jetzt bist du dran:</strong> Schreib in <code>webcrawler.rb</code> die Funktion <code>absolute_links(html, base)</code>. <code>base</code> ist die Adresse der Seite. Sie gibt die Ziele ihrer Links als ganze Adressen zurück: ohne <code>#…</code>, nur mit <code>http</code> oder <code>https</code>, nur auf demselben Host und Port wie <code>base</code>, und jede Adresse nur einmal.
</div>

<!-- hint -->
Für jedes Ziel aus `href_targets(html)` rechnest du mit `URI.join(base, target)`
die ganze Adresse aus. Manche Ziele kann `URI.join` nicht lesen, etwa mit
Leerzeichen darin; mit `(URI.join(base, target) rescue nil)` bekommst du dann
`nil` und überspringst das Ziel.
<!-- /hint -->

<!-- hint -->
Weiter mit `next`, wenn `uri.scheme` weder `"http"` noch `"https"` ist oder
`uri.host` und `uri.port` andere sind als bei `URI(base)`. Sonst entfernt
`uri.fragment = nil` das `#…`, und `uri.to_s` ist die Adresse, die du
anhängst, wenn sie noch nicht im Ergebnis steht.
<!-- /hint -->

<!-- solution -->
```ruby
# Die Links einer Seite als ganze Adressen, nur auf ihrer eigenen Website,
# ohne #…, jede nur einmal; base ist die Adresse der Seite
def absolute_links(html, base)
  base_uri = URI(base)
  result = []
  href_targets(html).each do |target|
    uri = (URI.join(base, target) rescue nil)
    next if uri.nil?
    next unless %w[http https].include?(uri.scheme)
    next unless uri.host == base_uri.host && uri.port == base_uri.port

    uri.fragment = nil
    url = uri.to_s
    result << url unless result.include?(url)
  end
  result
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
Das Rechnen nimmt dir `urljoin` ab, das Abschneiden von `#…` `urldefrag`:

```python
urljoin('https://schule.example/tiere/katze.html', 'hund.html')
# 'https://schule.example/tiere/hund.html'
urldefrag('https://schule.example/index.html#oben')[0]
# 'https://schule.example/index.html'
```

`urlparse(url)` zerlegt eine Adresse in ihre Teile: `.scheme` ist `'https'`,
`.netloc` der Host, mit Port, wenn einer dasteht.

<div class='hint own' data-check='ganze-adressen'>
<strong>Jetzt bist du dran:</strong> Schreib in <code>webcrawler.py</code> die Funktion <code>absolute_links(html, base)</code>. <code>base</code> ist die Adresse der Seite. Sie gibt die Ziele ihrer Links als ganze Adressen zurück: ohne <code>#…</code>, nur mit <code>http</code> oder <code>https</code>, nur mit demselben <code>netloc</code> wie <code>base</code>, und jede Adresse nur einmal.
</div>

<!-- hint -->
Für jedes Ziel aus `href_targets(html)` rechnest du mit
`urldefrag(urljoin(base, target))[0]` die ganze Adresse ohne `#…` aus.
<!-- /hint -->

<!-- hint -->
Mit `continue` weiter, wenn `urlparse(url).scheme` weder `'http'` noch
`'https'` ist oder `netloc` ein anderes ist als bei `urlparse(base)`. Sonst
hängst du die Adresse an, wenn sie noch nicht im Ergebnis steht.
<!-- /hint -->

<!-- solution -->
```python
def absolute_links(html, base):
    """Die Links einer Seite als ganze Adressen, nur auf ihrer eigenen Website,
    ohne #…, jede nur einmal; base ist die Adresse der Seite"""
    result = []
    for target in href_targets(html):
        url = urldefrag(urljoin(base, target))[0]
        parts = urlparse(url)
        if parts.scheme not in ('http', 'https') or parts.netloc != urlparse(base).netloc:
            continue
        if url not in result:
            result.append(url)
    return result
```
<!-- /solution -->
<!-- /language -->

_task(ganze-adressen)

### Was robots.txt erlaubt

In der Datei `/robots.txt` sagt eine Website Crawlern, was sie nicht holen
sollen. Wie sie aufgebaut ist, steht in [RFC 9309](https://www.rfc-editor.org/rfc/rfc9309),
dem Robots Exclusion Protocol:

```text
User-agent: Googlebot
Disallow: /

User-agent: *
Disallow: /intern/
Disallow: /impressum.html
```

Jede Gruppe beginnt mit `User-agent`, dem Crawler, für den sie gilt; `*` meint
alle. Eine Zeile `Disallow` verbietet jeden Pfad, der so anfängt:
`/intern/` also `/intern/noten.html` und alles andere in diesem Ordner, aber
nicht `/internat.html`. Hier darf der Googlebot gar nichts holen, alle anderen
alles außer `/intern/` und dem Impressum.

`robots_rules` liest aus der Datei die Zeilen `Disallow` der Gruppe für `*`,
im Beispiel `["/intern/", "/impressum.html"]`. Das ist vereinfacht: RFC 9309
kennt auch Zeilen `Allow` und die Zeichen `*` und `$` in einer Regel.
<!-- language: python -->
Pythons Standardbibliothek liest die ganze Datei mit `urllib.robotparser`.
<!-- /language -->

<!-- language: ruby -->
<div class='hint own' data-check='erlaubt'>
<strong>Jetzt bist du dran:</strong> Schreib in <code>webcrawler.rb</code> die Funktion <code>allowed?(rules, path)</code>. Sie gibt <code>true</code> zurück, wenn keine der Regeln der Anfang von <code>path</code> ist.
</div>

<!-- hint -->
`path.start_with?(rule)` sagt, ob der Pfad mit der Regel anfängt, und
`rules.none? { |rule| … }`, ob das für keine Regel gilt.
<!-- /hint -->

<!-- solution -->
```ruby
# Ob der Crawler den Pfad holen darf: Keine Regel ist sein Anfang
def allowed?(rules, path)
  rules.none? { |rule| path.start_with?(rule) }
end
```
<!-- /solution -->

<!-- stage: loesung/webcrawler.rb as webcrawler.rb -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='erlaubt'>
<strong>Jetzt bist du dran:</strong> Schreib in <code>webcrawler.py</code> die Funktion <code>allowed(rules, path)</code>. Sie gibt <code>True</code> zurück, wenn keine der Regeln der Anfang von <code>path</code> ist.
</div>

<!-- hint -->
`path.startswith(rule)` sagt, ob der Pfad mit der Regel anfängt, und
`any(… for rule in rules)`, ob das für mindestens eine Regel gilt; davor ein
`not`.
<!-- /hint -->

<!-- solution -->
```python
def allowed(rules, path):
    """Ob der Crawler den Pfad holen darf: Keine Regel ist sein Anfang"""
    return not any(path.startswith(rule) for rule in rules)
```
<!-- /solution -->

<!-- stage: loesung/webcrawler.py as webcrawler.py -->
<!-- /language -->

_task(erlaubt)

### Erst im Tierlexikon

Gib dem Tierlexikon eine `robots.txt`, die das Impressum verbietet:

```bash
printf 'User-agent: *\nDisallow: /impressum.html\n' > ~/suchmaschine/tierlexikon/robots.txt
```

<!-- language: ruby -->
Das Programm dazu kommt in `web.rb`. Ohne Adresse fängt es beim Tierlexikon an:

```ruby
require_relative 'webcrawler'

start = ARGV[0] || 'http://localhost:8000/index.html'
pages = polite_crawl(start)
puts "#{pages.size} Seiten geholt"
```

Starte es, während der Webserver im zweiten Terminal läuft:

```bash
ruby web.rb
```

<!-- stage: loesung/web.rb as web.rb -->
<!-- /language -->
<!-- language: python -->
Das Programm dazu kommt in `web.py`. Ohne Adresse fängt es beim Tierlexikon an:

```python
import sys

from webcrawler import *

start = sys.argv[1] if len(sys.argv) > 1 else 'http://localhost:8000/index.html'
pages = polite_crawl(start)
print(f'{len(pages)} Seiten geholt')
```

Starte es, während der Webserver im zweiten Terminal läuft:

```bash
python3 web.py
```

<!-- stage: loesung/web.py as web.py -->
<!-- /language -->

_task(hoeflich)

<div class='hint try'>
<strong>Ausprobieren:</strong> Achte darauf, wie lange der Lauf dauert und an welcher Stelle der Crawler eine Seite überspringt. Ändere dann <code>PAUSE</code> auf <code>0</code> und starte noch einmal: So schnell holt ein Crawler, der auf niemanden Rücksicht nimmt.
</div>

### Auf einer echten Website

Bevor dein Crawler eine echte Website besucht:

- **Frag, wer die Website betreut**, an einer Schule oft eine Lehrkraft oder
  die Schul-IT. Jede Anfrage deines Crawlers landet im Protokoll ihres
  Webservers, mit deinem User-Agent.
- **Lasst ihn einmal für die ganze Klasse laufen**, am Beamer, statt dreißigmal
  gleichzeitig.

<!-- language: ruby -->
Dann bekommt `web.rb` die Startseite der Website:

```bash
ruby web.rb https://www.beispiel-schule.de/
```
<!-- /language -->
<!-- language: python -->
Dann bekommt `web.py` die Startseite der Website:

```bash
python3 web.py https://www.beispiel-schule.de/
```
<!-- /language -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Achte darauf, was <code>robots.txt</code> verbietet, wie viele Seiten der Crawler holt, bevor er bei 30 aufhört, und wie lange das dauert. Vergleiche mit dem, was du im Browser siehst: Links, die erst ein Skript im Browser erzeugt, stehen nicht im HTML, und dein Crawler findet sie nicht.
</div>

## Zum Nachschlagen

- [Graphen: Breitensuche](/reference/graphen#breitensuche): der Weg des
  Crawlers durch die Links
- [RFC 9309](https://www.rfc-editor.org/rfc/rfc9309): wie `robots.txt`
  aufgebaut ist, das Robots Exclusion Protocol
- [Katas: Dictionaries](/kata-dicts) und [Katas: Listen](/kata-lists)
