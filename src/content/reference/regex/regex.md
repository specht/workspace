<div class='meta'>
kind: reference
search: Regex, Regexp, regular expressions, Muster
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Reguläre Ausdrücke

<p class='abstract'>
Ein regulärer Ausdruck ist ein Muster für Text: »eine Postleitzahl«, »ein Wort, das auf ing endet«, »eine Zeile mit einem Datum«. Hier stehen die Bausteine, mit denen man solche Muster schreibt, wie man sie in Ruby, Python, mit grep und in VS Code benutzt, und was sie mit endlichen Automaten zu tun haben: Hinter jedem regulären Ausdruck steckt ein Automat.
</p>

<script src='/include/automaton_layout.js'></script>
<script src='/include/automatron.js'></script>

## Die Bausteine

<!-- part: bausteine -->
<!-- short: Zeichen stehen für sich; . ein beliebiges Zeichen, [abc] eins davon, [^abc] keins davon; | oder; * beliebig oft (Kleenesche Hülle), + mindestens einmal (positive Hülle), ? höchstens einmal, {n,m} n bis m mal; ^ und $ Anfang und Ende; Klammern fassen zusammen; \ vor einem Sonderzeichen meint das Zeichen selbst. -->
### Zeichen und Zeichenklassen

Reguläre Ausdrücke heißen auf Englisch *regular expressions*, kurz **Regex** oder Regexp. Ein Buchstabe oder eine Ziffer steht in ihnen für sich selbst: `ing` passt auf die drei Zeichen i, n, g hintereinander. Für »eines von mehreren Zeichen« gibt es **Zeichenklassen**:

<table class='table'>
<tr><th>Muster</th><th>passt auf</th><th>Beispiel</th></tr>
<tr><td><code>.</code></td><td>ein beliebiges Zeichen außer dem Zeilenende</td><td><code>h.t</code> passt auf hat, hut, h7t</td></tr>
<tr><td><code>[aeiou]</code></td><td>eines der Zeichen in den Klammern</td><td><code>h[aeiou]t</code> passt auf hat, hut, nicht auf h7t</td></tr>
<tr><td><code>[a-z]</code>, <code>[0-9]</code></td><td>eines aus dem Bereich</td><td><code>[A-Z][a-z]</code> passt auf Ab, Xy</td></tr>
<tr><td><code>[^0-9]</code></td><td>ein Zeichen, das <em>nicht</em> in den Klammern steht</td><td><code>[^ ]</code> passt auf alles außer einem Leerzeichen</td></tr>
<tr><td><code>\d</code>, <code>\w</code>, <code>\s</code></td><td>eine Ziffer, ein Wortzeichen (Buchstabe, Ziffer, <code>_</code>), ein Leerraum (Leerzeichen, Tab, Zeilenende)</td><td><code>\d\d</code> passt auf 42</td></tr>
<tr><td><code>\D</code>, <code>\W</code>, <code>\S</code></td><td>das Gegenteil davon</td><td><code>\S+</code> passt auf ein ganzes Wort ohne Leerzeichen</td></tr>
<tr><td><code>\.</code>, <code>\*</code>, <code>\(</code></td><td>das Zeichen selbst: Ein <code>\</code> nimmt einem Sonderzeichen seine Bedeutung</td><td><code>3\.14</code> passt nur auf 3.14</td></tr>
</table>

### Wiederholen, oder, zusammenfassen

<table class='table'>
<tr><th>Muster</th><th>bedeutet</th><th>Beispiel</th></tr>
<tr><td><code>A|B</code></td><td>A oder B</td><td><code>Katze|Hund</code></td></tr>
<tr><td><code>(…)</code></td><td>fasst zusammen, wie in der Mathematik</td><td><code>(ab)+</code> passt auf ab, abab, ababab</td></tr>
<tr><td><code>A*</code></td><td>A beliebig oft, auch keinmal: die <strong>Kleenesche Hülle</strong> (der Kleene-Stern)</td><td><code>ab*</code> passt auf a, ab, abbb</td></tr>
<tr><td><code>A+</code></td><td>A mindestens einmal: die <strong>positive Hülle</strong></td><td><code>\d+</code> passt auf jede Zahl</td></tr>
<tr><td><code>A?</code></td><td>A höchstens einmal</td><td><code>Farbe?</code> passt auf Farb und Farbe</td></tr>
<tr><td><code>A{3}</code>, <code>A{2,4}</code></td><td>genau dreimal, zwei- bis viermal</td><td><code>\d{5}</code> passt auf eine Postleitzahl</td></tr>
<tr><td><code>^</code>, <code>$</code></td><td>Anfang und Ende einer Zeile</td><td><code>^#</code> passt auf Zeilen, die mit # beginnen</td></tr>
</table>

`*`, `+` und `?` gelten für das, was direkt davor steht: In `ab*` wiederholt sich nur das b, in `(ab)*` beides. Und sie nehmen so viel, wie sie können: `<.*>` passt in `<b>fett</b>` auf alles von der ersten `<` bis zur letzten `>`. Mit einem `?` dahinter (`<.*?>`) nehmen sie so wenig wie möglich und passen nur auf `<b>`.
<!-- /part -->

## Ausprobieren

<!-- part: ausprobieren -->
<!-- short: In VS Code sucht Alt+R mit regulären Ausdrücken; Automatron zeichnet den Automaten zu einem Ausdruck aus |, *, +, ? und Klammern. -->
Am schnellsten probierst du einen Ausdruck in **VS Code** aus: Strg+F öffnet die Suche, Alt+R (oder der Knopf `.*`) schaltet reguläre Ausdrücke ein, und jede Stelle, auf die er passt, wird markiert. Beim Ersetzen (Strg+H) steht `$1` für das, was die erste Klammer gefunden hat.

Den Automaten hinter einem Ausdruck zeichnet [Automatron](/automatron). Es kennt nur die Bausteine aus der Theorie, Zeichen, `|`, `*`, `+`, `?` und Klammern, keine Zeichenklassen; `[0-9]` schreibst du dort als `(0|1|2|3|4|5|6|7|8|9)`.
<!-- /part -->

## In Ruby

<!-- part: ruby -->
<!-- short: Ruby: /muster/, string.match?(re), string =~ re, string.scan(re), string.gsub(re, ersatz); \A und \z für Anfang und Ende des ganzen Strings. -->
In Ruby steht ein regulärer Ausdruck zwischen Schrägstrichen:

```ruby
plz = /\A\d{5}\z/
puts '10115'.match?(plz)            # true
puts '1011'.match?(plz)             # false

text = 'Am 3.10. und am 24.12. ist frei.'
p text.scan(/\d+\.\d+\./)           # ["3.10.", "24.12."]
puts text.gsub(/\d+/, '#')          # Am #.#. und am #.#. ist frei.

if (m = text.match(/(\d+)\.(\d+)\./))
  puts "Tag #{m[1]}, Monat #{m[2]}" # Tag 3, Monat 10
end
```

`\A` und `\z` sind Anfang und Ende des ganzen Strings. `^` und `$` sind in Ruby Anfang und Ende jeder *Zeile*: `/^\d{5}$/` passt auch auf einen String, in dem irgendwo eine Zeile nur aus fünf Ziffern besteht.
<!-- /part -->

## In Python

<!-- part: python -->
<!-- short: Python: import re; re.fullmatch, re.search, re.findall, re.sub; das Muster als Raw-String r"…". -->
In Python stehen die Funktionen im Modul `re`. Das Muster schreibt man als **Raw-String** `r"…"`, damit Python die Backslashes nicht selbst auswertet:

```python
import re

print(bool(re.fullmatch(r"\d{5}", "10115")))   # True: der ganze String passt
print(bool(re.fullmatch(r"\d{5}", "1011")))    # False

text = "Am 3.10. und am 24.12. ist frei."
print(re.findall(r"\d+\.\d+\.", text))       # ['3.10.', '24.12.']
print(re.sub(r"\d+", "#", text))              # Am #.#. und am #.#. ist frei.

m = re.search(r"(\d+)\.(\d+)\.", text)
if m:
    print(f"Tag {m.group(1)}, Monat {m.group(2)}")   # Tag 3, Monat 10
```

`re.fullmatch` prüft den ganzen String, `re.search` sucht eine Stelle irgendwo darin, `re.match` eine am Anfang.
<!-- /part -->

## Mit grep

<!-- part: grep -->
<!-- short: grep -E "muster" datei: die passenden Zeilen; -o nur die Treffer, -i ohne Groß und Klein, -v die anderen Zeilen, -c zählen, -r in allen Dateien. -->
Im Terminal sucht `grep` Zeilen, auf die ein Ausdruck passt. Mit `-E` versteht es die Bausteine dieser Seite (`+`, `?`, `|`, `{…}`, Klammern) ohne zusätzliche Backslashes; `\d` kennt es nicht, dafür schreibt man `[0-9]`:

```bash
grep -E "[0-9]{5}" adressen.txt          # Zeilen mit einer Postleitzahl
grep -E -o "[0-9]{5}" adressen.txt       # nur die Postleitzahlen
grep -E -c "^#" notizen.md               # wie viele Zeilen mit # beginnen
grep -E -v "^\s*$" notizen.md            # alle Zeilen, die nicht leer sind
```

Die anderen Schalter von grep (`-i`, `-n`, `-r`, …) stehen auf der Handbuch-Seite [Terminal](/reference/terminal#grep).
<!-- /part -->

## Reguläre Ausdrücke und Automaten

<!-- part: automaten -->
<!-- short: Jeder reguläre Ausdruck aus Zeichen, |, * und Klammern beschreibt eine Sprache, die ein endlicher Automat erkennt, und umgekehrt; Rückverweise wie \1 und richtig geschachtelte Klammern gehen darüber hinaus. -->
Die Bausteine Zeichen, Hintereinander, `|` und `*` reichen für alles Reguläre; `+`, `?`, `{…}` und die Zeichenklassen sind Abkürzungen dafür. Aus jedem solchen Ausdruck lässt sich ein **endlicher Automat** bauen, der genau die passenden Wörter annimmt, und aus jedem endlichen Automaten ein Ausdruck: Beide beschreiben dieselben Sprachen, die **regulären Sprachen**. Wie der Automat entsteht (Thompson-Konstruktion, Potenzmengenkonstruktion, Minimierung), steht auf der Handbuch-Seite [Endliche Automaten](/reference/automaten). Für `(a|b)*abb`, »endet auf abb«, ist der kleinste DEA:

<div class='automaton-drawing' role='img' aria-label='Der kleinste DEA zu (a|b)*abb: vier Kreise s0 bis s3 in einer Reihe, s3 akzeptiert. a führt nach s1, dann b nach s2, dann b nach s3; die übrigen Zeichen führen zurück nach s0 oder s1.' data-regex='(a|b)*abb' data-automaton='min'></div>

So arbeiten die Suchfunktionen in Editoren und `grep` im Prinzip: Sie lesen den Text Zeichen für Zeichen und merken sich, in welchen Zuständen des Automaten sie gerade sind.

Was ein endlicher Automat nicht kann, kann auch kein regulärer Ausdruck aus diesen Bausteinen: zählen ohne Grenze. »Gleich viele a wie b« oder »jede öffnende Klammer hat eine schließende« ist keine reguläre Sprache. Ruby und Python können trotzdem etwas mehr, zum Beispiel **Rückverweise**: `(\w+) \1` passt auf ein Wort, das zweimal hintereinander steht. Dafür reicht kein endlicher Automat mehr, und solche Muster können bei langen Texten sehr langsam werden.
<!-- /part -->

## Zum Nachschlagen

- [Ruby: `Regexp`](https://docs.ruby-lang.org/en/3.4/Regexp.html): alle Bausteine, Optionen und Methoden in Ruby.
- [Python: `re`](https://docs.python.org/3/library/re.html) und das [Regular Expression HOWTO](https://docs.python.org/3/howto/regex.html): das Modul und eine Einführung.
- [grep(1)](https://man7.org/linux/man-pages/man1/grep.1.html): die Handbuchseite von grep mit allen Schaltern.
- [VS Code: Suchen und Ersetzen](https://code.visualstudio.com/docs/editing/codebasics#_find-and-replace): reguläre Ausdrücke im Editor.
- [MDN: Cheatsheet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions/Cheatsheet): alle Bausteine auf einer Seite (für JavaScript, fast alles gilt auch in Ruby und Python).
- [Rubular](https://rubular.com/): keine offizielle Referenz; testet einen Ausdruck in Ruby an eigenem Text und zeigt, was jede Klammer findet, mit einer Kurzübersicht der Bausteine darunter, auf Englisch.
- [regex101](https://regex101.com/): keine offizielle Referenz; testet einen Ausdruck an eigenem Text und erklärt jeden Baustein, auf Englisch.
