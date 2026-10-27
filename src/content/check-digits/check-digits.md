<div class='meta'>
image: teaser.webp:25:50
image_alt: "Ein Strichcode mit den Ziffern 4 260111 020434 auf einem weißen Etikett, daneben das Struktogramm der Funktion check_digit mit einer Zählschleife von 0 bis 11."
kind: projekt
duration: 2 Doppelstunden
languages: [ruby, python]
curriculum: "Sek I: Algorithmisches Problemlösen (Eigenschaften und Darstellung von Algorithmen, Implementieren von Algorithmen), Information und Daten (Codierung: Strichcode); Oberstufe: Algorithmen und Softwareentwicklung (Struktogramm, Pseudocode)"
topics: [fehlererkennung, codierung, darstellung]
requires: [kata-lists]
reference: [struktogramme]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Prüfziffern: erst planen, dann programmieren

<p class='abstract'>
Warum piept die Kasse, wenn jemand eine Ziffer eines Strichcodes falsch eintippt? Du rechnest eine Prüfziffer von Hand aus, liest Struktogramme und übersetzt sie in Code. Dann planst du selbst, mit Kommentaren im Code, bevor du programmierst, und findest heraus, welche Tippfehler die Kasse übersieht.
</p>

<script src='/include/struktogramm.js'></script>

## Die Geschichte

Samstagvormittag im Supermarkt. Auf einer Packung Nudeln ist der Strichcode
zerknittert, und der Scanner piept nicht. Die Kassiererin tippt die dreizehn
Ziffern unter den Strichen von Hand ein:

<div class='gallery centered'>
<figure><img src='barcode.svg' alt='Ein Strichcode aus schwarzen Strichen verschiedener Breite, darunter die Ziffern 4 260111 020434.'><figcaption>Der Strichcode der Nudeln, ausgedacht</figcaption></figure>
</div>

Dreizehn Ziffern sind viele, und beim Abtippen passiert schnell ein Fehler.
Dann stünde auf dem Kassenbon eine andere Ware mit einem anderen Preis.

_task(eine-ziffer)

## Der Strichcode

Der Code heißt **EAN-13** (European Article Number, heute GTIN-13); er steht
auf fast jeder Ware im Laden. Die ersten zwölf Ziffern sagen, aus welchem Land
die Nummer kommt, welche Firma sie vergeben hat und welche Ware es ist. Die
dreizehnte ist keine Information über die Ware, sondern eine **Prüfziffer**: Sie
wird aus den anderen zwölf berechnet, nach einer Regel, die die Organisation
GS1 festgelegt hat:

1. Multipliziere die zwölf Ziffern abwechselnd mit 1 und mit 3, mit 1
   beginnend. Die Zahl, mit der eine Ziffer multipliziert wird, heißt ihr
   **Gewicht**.
2. Addiere die Produkte.
3. Die Prüfziffer ist, was bis zur nächsten durch 10 teilbaren Zahl fehlt. Ist
   die Summe schon durch 10 teilbar, ist sie 0.

Für die Ziffern `4 001234 56789` sieht das so aus:

<div class='scroll-x'><table class='table'>
<tr><th>Ziffer</th><td>4</td><td>0</td><td>0</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td></tr>
<tr><th>Gewicht</th><td>1</td><td>3</td><td>1</td><td>3</td><td>1</td><td>3</td><td>1</td><td>3</td><td>1</td><td>3</td><td>1</td><td>3</td></tr>
<tr><th>Produkt</th><td>4</td><td>0</td><td>0</td><td>3</td><td>2</td><td>9</td><td>4</td><td>15</td><td>6</td><td>21</td><td>8</td><td>27</td></tr>
</table>
</div>

Die Summe ist 99; bis 100 fehlt 1, also ist der ganze Code `4 001234 567891`.

_task(von-hand)

<!-- after: von-hand -->
Die Kasse rechnet genauso, mit den zwölf Ziffern, die eingetippt wurden. Passt
das Ergebnis nicht zur dreizehnten Ziffer, piept sie.

Und warum merkt sie **jede** einzelne falsche Ziffer? Ändert sich eine Ziffer
mit dem Gewicht 1 um 1 bis 9, ändert sich die Summe um 1 bis 9. Ändert sich
eine mit dem Gewicht 3, ändert sich die Summe um 3, 6, 9, …, 27, aber nie um 10,
20 oder 30. In beiden Fällen ist danach eine andere Prüfziffer nötig.
<!-- /after -->

## Wie die Striche die Ziffern tragen

Der Scanner liest keine Ziffern, sondern hell und dunkel. Ein Strichcode ist
in 95 gleich breite Streifen geteilt, die **Module**. Jedes Modul ist dunkel
oder hell, also eine 1 oder eine 0; ein breiter Strich sind mehrere dunkle
Module nebeneinander.

<img class='drawing' style='width: 37.5em' src='aufbau.webp' alt='Der Strichcode der Nudeln, 4 260111 020434, auf einem weißen Etikett. Darüber Klammern: am linken und rechten Rand je drei Module »Rand«, in der Mitte fünf Module »Mitte«, dazwischen links und rechts je »sechs Ziffern, je 7 Module«. Die Striche von Rand und Mitte reichen weiter nach unten als die anderen. Darunter eine Klammer über den ganzen Code: 95 Module, 3 + 6 · 7 + 5 + 6 · 7 + 3.'>

Am Rand und in der Mitte stehen feste Muster: `101` links und rechts, `01010`
in der Mitte. Ihre Striche reichen weiter nach unten als die anderen. An ihnen
erkennt der Scanner, wo der Code anfängt, wo seine Mitte ist und wie breit ein
Modul ist, egal wie weit die Packung vom Scanner entfernt ist. Dazwischen
stehen links und rechts je sechs Ziffern mit je sieben Modulen.

Jede Ziffer besteht aus genau zwei dunklen und zwei hellen Abschnitten. Welches
Muster zu welcher Ziffer gehört, legt GS1 in einer Tabelle fest, und zwar in
drei Fassungen, **L**, **G** und **R**. So sieht die 4 in allen dreien aus:

<img class='drawing' style='width: 34em' src='ziffer.webp' alt='Die Ziffer 4 in drei Fassungen, je sieben Kästchen, dunkel für 1 und hell für 0. L, links: 0100011, 3 dunkle Module, ungerade. G, links: 0011101, 4 dunkle Module, gerade. R, rechts: 1011100, 4 dunkle Module, gerade.'>

R ist L mit vertauschtem Hell und Dunkel, G ist R von hinten nach vorne
gelesen. Darum genügt die Tabelle für L:

<div class='scroll-x'><table class='table'>
<tr><th>Ziffer</th><td>0</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td></tr>
<tr><th>L</th><td><code>0001101</code></td><td><code>0011001</code></td><td><code>0010011</code></td><td><code>0111101</code></td><td><code>0100011</code></td><td><code>0110001</code></td><td><code>0101111</code></td><td><code>0111011</code></td><td><code>0110111</code></td><td><code>0001011</code></td></tr>
</table>
</div>

Rechts von der Mitte steht jede Ziffer in R. Links steht sie in L oder in G,
und L hat immer eine ungerade Zahl dunkler Module, G eine gerade.

_task(rechts-lesen)

Zähl die Ziffern unter dem Code: Links und rechts von der Mitte stehen je
sechs, die 4 ganz vorne hat keine eigenen Striche. Sie steckt in der Auswahl
der Fassungen in der linken Hälfte. Bei den Nudeln stehen die Ziffern
2 6 0 1 1 1 in den Fassungen L G L L G G, und dieses Muster bedeutet die 4:

<div class='scroll-x'><table class='table' style='width: auto;'>
<tr><th>Erste Ziffer</th><th>Linke Hälfte</th></tr>
<tr><td>0</td><td><code>L L L L L L</code></td></tr>
<tr><td>1</td><td><code>L L G L G G</code></td></tr>
<tr><td>2</td><td><code>L L G G L G</code></td></tr>
<tr><td>3</td><td><code>L L G G G L</code></td></tr>
<tr><td>4</td><td><code>L G L L G G</code></td></tr>
<tr><td>5</td><td><code>L G G L L G</code></td></tr>
<tr><td>6</td><td><code>L G G G L L</code></td></tr>
<tr><td>7</td><td><code>L G L G L G</code></td></tr>
<tr><td>8</td><td><code>L G L G G L</code></td></tr>
<tr><td>9</td><td><code>L G G L G L</code></td></tr>
</table>
</div>

Die erste Ziffer links steht dabei immer in L. Liest der Scanner einen Code
von rechts nach links, etwa weil die Packung auf dem Kopf liegt, trifft er
zuerst auf eine Ziffer mit einer geraden Zahl dunkler Module. Daran merkt er,
dass er rückwärts liest, und dreht die Ziffern um.

_task(erste-ziffer)

## Ein Algorithmus, drei Schreibweisen

Die Regel in Worten ist kurz, aber ein Computer braucht es genauer: Wo fängt
das Zählen an? Was heißt »bis zur nächsten durch 10 teilbaren Zahl«? Bevor du
programmierst, schreibst du einen Algorithmus darum so auf, dass er genau ist
und trotzdem in keiner Programmiersprache: als **Pseudocode** oder als
**Struktogramm**. Beides steht im Handbuch unter
[Pseudocode und Struktogramme](/reference/struktogramme), mit einem Feld zum
Ausprobieren.

### Ein Struktogramm lesen

Ein Struktogramm liest man von oben nach unten. Jeder Kasten ist eine
Anweisung; eine Schleife hat links einen Balken, und was rechts daneben steht,
wiederholt sie, solange ihre Bedingung oben wahr ist. `n mod 10` ist der Rest
bei der Division durch 10, `n div 10` das ganzzahlige Ergebnis.

```struktogramm
mystery(n):
    s ← 0
    while n > 0:
        s ← s + n mod 10
        n ← n div 10
    return s
```

_task(raetsel)

<!-- after: raetsel -->
`n mod 10` ist die letzte Ziffer von `n`, und `n div 10` streicht sie: Aus 4021
wird 402, dann 40, dann 4, dann 0. `mystery` addiert also alle Ziffern, das ist
die **Quersumme**. Mit `mod` und `div` kommt man an die Ziffern einer Zahl, und
mit `mod` sagt man, was übrig bleibt, wenn eine Zahl nicht aufgeht.
<!-- /after -->

### Die Prüfziffer als Struktogramm

So sieht die Regel von GS1 als Struktogramm aus. `digits` ist eine Liste der
zwölf Ziffern; die erste hat den Index 0. `weight(i)` ist das Gewicht der
Ziffer mit dem Index `i`, also 1 oder 3.

```struktogramm
check_digit(digits):
    total ← 0
    for i from 0 to 11:
        total ← total + weight(i) * digits[i]
    return (10 - total mod 10) mod 10
```

`total mod 10` ist der Rest, und `10 - total mod 10` ist, was bis zur nächsten
durch 10 teilbaren Zahl fehlt.

_task(zweites-mod)

## Vom Struktogramm zum Code

### Einrichten

<!-- language: ruby -->
Lege im Workspace einen Ordner und darin eine Datei für die Funktionen an:

```bash
mkdir -p ~/check-digits
cd ~/check-digits
touch check_digits.rb
```

Öffne `check_digits.rb` im Editor. Ausprobieren kannst du deine Funktionen in
`irb`, das die Datei vorher lädt:

```bash
irb -r ./check_digits.rb
```

Dort tippst du einen Aufruf wie `weight(3)` und siehst, was er zurückgibt. Nach
einer Änderung an der Datei beendest du `irb` mit <kbd>Strg</kbd><kbd>D</kbd>
und startest es neu.
<!-- /language -->
<!-- language: python -->
Lege im Workspace einen Ordner und darin eine Datei für die Funktionen an:

```bash
mkdir -p ~/check-digits
cd ~/check-digits
touch check_digits.py
```

Öffne `check_digits.py` im Editor. Ausprobieren kannst du deine Funktionen in
Python selbst, das die Datei vorher ausführt:

```bash
python3 -i check_digits.py
```

Dort tippst du einen Aufruf wie `weight(3)` und siehst, was er zurückgibt. Nach
einer Änderung an der Datei beendest du Python mit <kbd>Strg</kbd><kbd>D</kbd>
und startest es neu.
<!-- /language -->

### Das Gewicht

Die Ziffern mit geradem Index (0, 2, 4, …) haben das Gewicht 1, die mit
ungeradem Index das Gewicht 3.

<div class='hint own' data-check='gewicht'>
<strong>Jetzt bist du dran:</strong> Schreib die Funktion <code>weight(index)</code>,
die für einen Index das Gewicht zurückgibt.
</div>

<!-- language: ruby -->
<!-- hint -->
Ob eine Zahl gerade ist, sagt der Rest bei der Division durch 2: `index % 2`
ist 0 oder 1. Ruby hat dafür auch `index.even?`.
<!-- /hint -->

<!-- hint -->
Als Struktogramm ist es eine Verzweigung mit zwei Spalten, in jeder ein
`return`:

```struktogramm
weight(index):
    if index mod 2 == 0:
        return 1
    else:
        return 3
```
<!-- /hint -->

<!-- solution -->
```ruby
def weight(index)
  if index.even?
    1
  else
    3
  end
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<!-- hint -->
Ob eine Zahl gerade ist, sagt der Rest bei der Division durch 2: `index % 2`
ist 0 oder 1.
<!-- /hint -->

<!-- hint -->
Als Struktogramm ist es eine Verzweigung mit zwei Spalten, in jeder ein
`return`:

```struktogramm
weight(index):
    if index mod 2 == 0:
        return 1
    else:
        return 3
```
<!-- /hint -->

<!-- solution -->
```python
def weight(index):
    if index % 2 == 0:
        return 1
    else:
        return 3
```
<!-- /solution -->
<!-- /language -->

_task(gewicht)

### Die Prüfziffer

<div class='hint own' data-check='pruefziffer'>
<strong>Jetzt bist du dran:</strong> Übersetze das Struktogramm von
<code>check_digit</code> von oben in Code, Kasten für Kasten, unter
<code>weight</code> in dieselbe Datei.
</div>

<!-- language: ruby -->
<!-- hint -->
Wie die Bausteine in Ruby heißen, steht im Handbuch unter
[Vom Pseudocode zum Code](/reference/struktogramme#vom-pseudocode-zum-code):
`←` ist `=`, `mod` ist `%`, und `for i from 0 to 11` ist `(0..11).each do |i|`.
<!-- /hint -->

<!-- solution -->
```ruby
def check_digit(digits)
  total = 0
  (0..11).each do |i|
    total = total + weight(i) * digits[i]
  end
  (10 - total % 10) % 10
end
```
<!-- /solution -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Ruf in <code>irb</code>
<code>check_digit([4, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9])</code> auf; heraus kommt
die 1 aus der Tabelle oben. Nimm dann das letzte <code>% 10</code> weg, starte
neu und ruf <code>check_digit([4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2])</code> auf. Achte darauf,
ob das Ergebnis noch eine Ziffer ist, und setz das <code>% 10</code> danach wieder ein.
</div>
<!-- /language -->
<!-- language: python -->
<!-- hint -->
Wie die Bausteine in Python heißen, steht im Handbuch unter
[Vom Pseudocode zum Code](/reference/struktogramme#vom-pseudocode-zum-code):
`←` ist `=`, `mod` ist `%`, und `for i from 0 to 11` ist
`for i in range(0, 12)`, denn `range` hört eins vor dem Ende auf.
<!-- /hint -->

<!-- solution -->
```python
def check_digit(digits):
    total = 0
    for i in range(0, 12):
        total = total + weight(i) * digits[i]
    return (10 - total % 10) % 10
```
<!-- /solution -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Ruf in Python
<code>check_digit([4, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9])</code> auf; heraus kommt
die 1 aus der Tabelle oben. Nimm dann das letzte <code>% 10</code> weg, starte
neu und ruf <code>check_digit([4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2])</code> auf. Achte darauf,
ob das Ergebnis noch eine Ziffer ist, und setz das <code>% 10</code> danach wieder ein.
</div>
<!-- /language -->

_task(pruefziffer)

## Erst planen, dann programmieren

Bisher stand der Plan schon da, und du hast ihn übersetzt. Ab jetzt planst du
selbst, und zwar gleich dort, wo der Code hinkommt: Du legst die Funktion an
und schreibst als Kommentare hinein, was sie Schritt für Schritt tun soll, noch
ohne eine Zeile Code. Dabei klärst du drei Fragen: Was bekommt die Funktion,
und was gibt sie zurück? Welche Fälle gibt es, auch solche, die nicht vorkommen
sollten? Und in welcher Reihenfolge geschieht was?

Dann ersetzt du einen Kommentar nach dem anderen durch Code und probierst nach
jedem Schritt aus. Die Kommentare dürfen stehen bleiben: Wer den Code später
liest, sieht so, wofür jeder Teil da ist. Genauso planen viele, die beruflich
programmieren; der Plan ist Pseudocode, nur eben in der Datei.

### Ist der Code gültig?

Die Kasse bekommt, was die Kassiererin getippt hat, als Text, und will wissen:
Ist das ein gültiger Code? Er ist es, wenn er aus genau 13 Ziffern besteht und
die letzte die Prüfziffer der ersten zwölf ist.

<!-- language: ruby -->
<div class='hint own' data-check='ean'>
<strong>Jetzt bist du dran:</strong> Leg die Funktion <code>valid_ean?(code)</code>
an und schreib zuerst deinen Plan als Kommentare hinein, dann den Code dazu.
Sie gibt <code>true</code> oder <code>false</code> zurück und benutzt
<code>check_digit</code>.
</div>

<!-- hint -->
Was macht einen Code sicher ungültig, bevor man rechnet? Wenn er zu kurz oder
zu lang ist, und wenn ein Zeichen keine Ziffer ist. Prüf das zuerst und gib
dann gleich `false` zurück.
<!-- /hint -->

<!-- hint -->
Ein Plan, dem noch der Code fehlt:

```ruby
def valid_ean?(code)
  # nicht genau 13 Zeichen: false
  # ein Zeichen ist keine Ziffer: false
  # die 13 Zeichen als Zahlen
  # gültig, wenn die Prüfziffer der ersten zwölf gleich der dreizehnten ist
end
```
<!-- /hint -->

<!-- hint -->
Für die Einzelheiten in Ruby: `code.length` ist die Zahl der Zeichen,
`code.chars` die Liste der Zeichen, `('0'..'9').include?(c)` sagt, ob `c` eine
Ziffer ist, `c.to_i` macht aus `'7'` die Zahl 7, und `digits[0..11]` sind die
ersten zwölf Elemente einer Liste.
<!-- /hint -->

<!-- solution -->
```ruby
def valid_ean?(code)
  # nicht genau 13 Zeichen: false
  return false unless code.length == 13
  # ein Zeichen ist keine Ziffer: false
  return false unless code.chars.all? { |c| ('0'..'9').include?(c) }

  # die 13 Zeichen als Zahlen
  digits = code.chars.map(&:to_i)
  # gültig, wenn die Prüfziffer der ersten zwölf gleich der dreizehnten ist
  check_digit(digits[0..11]) == digits[12]
end
```
<!-- /solution -->
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='ean'>
<strong>Jetzt bist du dran:</strong> Leg die Funktion <code>is_valid_ean(code)</code>
an und schreib zuerst deinen Plan als Kommentare hinein, dann den Code dazu.
Sie gibt <code>True</code> oder <code>False</code> zurück und benutzt
<code>check_digit</code>.
</div>

<!-- hint -->
Was macht einen Code sicher ungültig, bevor man rechnet? Wenn er zu kurz oder
zu lang ist, und wenn ein Zeichen keine Ziffer ist. Prüf das zuerst und gib
dann gleich `False` zurück.
<!-- /hint -->

<!-- hint -->
Ein Plan, dem noch der Code fehlt. Damit Python die Funktion schon jetzt
annimmt, steht am Ende `pass`, das nichts tut:

```python
def is_valid_ean(code):
    # nicht genau 13 Zeichen: False
    # ein Zeichen ist keine Ziffer: False
    # die 13 Zeichen als Zahlen
    # gültig, wenn die Prüfziffer der ersten zwölf gleich der dreizehnten ist
    pass
```
<!-- /hint -->

<!-- hint -->
Für die Einzelheiten in Python: `len(code)` ist die Zahl der Zeichen,
`c in '0123456789'` sagt, ob `c` eine Ziffer ist, `int(c)` macht aus `'7'` die
Zahl 7, und `digits[0:12]` sind die ersten zwölf Elemente einer Liste.
<!-- /hint -->

<!-- solution -->
```python
def is_valid_ean(code):
    # nicht genau 13 Zeichen: False
    if len(code) != 13:
        return False
    # ein Zeichen ist keine Ziffer: False
    if not all(c in '0123456789' for c in code):
        return False
    # die 13 Zeichen als Zahlen
    digits = [int(c) for c in code]
    # gültig, wenn die Prüfziffer der ersten zwölf gleich der dreizehnten ist
    return check_digit(digits[0:12]) == digits[12]
```
<!-- /solution -->
<!-- /language -->

_task(ean)

### Bücher: die ISBN

Bücher tragen eine **ISBN** (International Standard Book Number). Bis 2007
hatte sie zehn Zeichen, etwa `3-499-12304-5`; die Bindestriche trennen nur
Gruppen und zählen nicht mit. Ihre Prüfung geht anders als beim Strichcode:

1. Multipliziere die zehn Ziffern mit den Gewichten 10, 9, 8, … bis 1: die
   erste mit 10, die letzte mit 1.
2. Addiere die Produkte.
3. Die ISBN ist gültig, wenn die Summe durch 11 teilbar ist.

Die letzte Ziffer ist also so gewählt, dass die Summe aufgeht. Manchmal müsste
sie dafür 10 sein, und das ist keine Ziffer: Dann steht dort ein `X`, wie die
römische Zehn. Ein `X` gibt es nur an dieser Stelle.

Für `3-499-12304-5` ist die Summe
3 · 10 + 4 · 9 + 9 · 8 + 9 · 7 + 1 · 6 + 2 · 5 + 3 · 4 + 0 · 3 + 4 · 2 + 5 · 1
= 242 = 22 · 11; die ISBN ist gültig.

<!-- language: ruby -->
Diesmal steht kein Plan auf der Seite. Leg `valid_isbn?(isbn)` an und schreib
deinen Plan als Kommentare hinein, bevor du Code schreibst: Was geschieht mit
den Bindestrichen, was mit einem `X`, was mit einem Zeichen, das weder Ziffer
noch `X` ist?
<!-- /language -->
<!-- language: python -->
Diesmal steht kein Plan auf der Seite. Leg `is_valid_isbn(isbn)` an und schreib
deinen Plan als Kommentare hinein, bevor du Code schreibst: Was geschieht mit
den Bindestrichen, was mit einem `X`, was mit einem Zeichen, das weder Ziffer
noch `X` ist?
<!-- /language -->

_task(plan-isbn)

_task(isbn)

## Was die Prüfziffer findet

Ein häufiger Fehler beim Abtippen ist nicht die falsche Ziffer, sondern der
**Zahlendreher**: zwei benachbarte Ziffern in der falschen Reihenfolge.

_task(zahlendreher-vorhersage)

<!-- after: zahlendreher-vorhersage -->
Ausrechnen kannst du es mit einem kleinen Programm, das alle Möglichkeiten
ausprobiert. Für zwei verschiedene Ziffern `a` und `b` gibt es 10 · 9 = 90
Paare. Es genügt, jedes Paar an den Anfang eines Codes zu stellen, den Rest mit
Nullen zu füllen und die Prüfziffer anzuhängen; dann ist der Code gültig.
Vertauscht man nun die ersten beiden Ziffern: Ist der Code danach immer noch
gültig, hat die Kasse den Zahlendreher nicht bemerkt.
<!-- /after -->

<!-- language: ruby -->
<div class='hint own' data-check='zahlendreher'>
<strong>Jetzt bist du dran:</strong> Schreib in eine neue Datei
<code>swaps.rb</code> ein Programm, das die 90 Paare durchgeht und zählt, wie
viele Zahlendreher <code>valid_ean?</code> nicht bemerkt. Die erste Zeile
<code>require_relative 'check_digits'</code> holt deine Funktionen dazu.
</div>

<!-- hint -->
Zwei Schleifen ineinander gehen alle Paare durch: außen `a` von 0 bis 9, innen
`b` von 0 bis 9; ist `a == b`, geht es mit dem nächsten weiter (`next`).
<!-- /hint -->

<!-- hint -->
Für ein Paar: `digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]`, dann ist
`code = (digits + [check_digit(digits)]).join` ein gültiger Code.
`code[1] + code[0] + code[2..]` ist derselbe Code mit vertauschten ersten
Ziffern.
<!-- /hint -->

<!-- solution -->
```ruby
require_relative 'check_digits'

missed = 0
(0..9).each do |a|
  (0..9).each do |b|
    next if a == b

    digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    code = (digits + [check_digit(digits)]).join
    swapped = code[1] + code[0] + code[2..]
    missed = missed + 1 if valid_ean?(swapped)
  end
end
puts "Unbemerkt: #{missed} von 90"
```
<!-- /solution -->

```bash
ruby swaps.rb
```
<!-- /language -->
<!-- language: python -->
<div class='hint own' data-check='zahlendreher'>
<strong>Jetzt bist du dran:</strong> Schreib in eine neue Datei
<code>swaps.py</code> ein Programm, das die 90 Paare durchgeht und zählt, wie
viele Zahlendreher <code>is_valid_ean</code> nicht bemerkt. Die erste Zeile
<code>from check_digits import check_digit, is_valid_ean</code> holt deine
Funktionen dazu.
</div>

<!-- hint -->
Zwei Schleifen ineinander gehen alle Paare durch: außen `a` von 0 bis 9, innen
`b` von 0 bis 9; ist `a == b`, geht es mit dem nächsten weiter (`continue`).
<!-- /hint -->

<!-- hint -->
Für ein Paar: `digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]`, dann ist
`code = ''.join(str(d) for d in digits + [check_digit(digits)])` ein gültiger
Code. `code[1] + code[0] + code[2:]` ist derselbe Code mit vertauschten ersten
Ziffern.
<!-- /hint -->

<!-- solution -->
```python
from check_digits import check_digit, is_valid_ean

missed = 0
for a in range(0, 10):
    for b in range(0, 10):
        if a == b:
            continue
        digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        code = ''.join(str(d) for d in digits + [check_digit(digits)])
        swapped = code[1] + code[0] + code[2:]
        if is_valid_ean(swapped):
            missed = missed + 1
print(f"Unbemerkt: {missed} von 90")
```
<!-- /solution -->

```bash
python3 swaps.py
```
<!-- /language -->

_task(zahlendreher)

<!-- after: zahlendreher -->
Unbemerkt bleiben genau die Paare, deren Ziffern sich um 5 unterscheiden, etwa
`05` und `50`. Beim Vertauschen tauschen die beiden Ziffern ihre Gewichte 1 und
3; die Summe ändert sich dadurch um das Doppelte ihres Unterschieds. Ist der
Unterschied 5, ändert sie sich um 10, und die Prüfziffer bleibt dieselbe.
<!-- /after -->

_task(isbn-dreher)

<!-- extra -->
## Wie geht es weiter?

- **Bücher heute:** Seit 2007 hat die ISBN dreizehn Ziffern, beginnt mit 978
  oder 979 und ist selbst ein EAN-13-Code: Auf der Rückseite eines Buchs steht
  sie als Strichcode, mit derselben Prüfziffer wie auf den Nudeln. Probier
  deine Funktion für Strichcodes an einem Buch aus deinem Regal aus.
- **Mehr als Fehler finden:** Ein QR-Code findet Fehler nicht nur, er
  repariert sie. Er trägt so viel zusätzliche Information, dass ein Handy ihn
  auch dann noch liest, wenn ein Teil verdeckt oder verschmutzt ist, je nach
  Stufe der Fehlerkorrektur bis zu 30 Prozent.
  Kleb ein Stück Papier über eine Ecke eines QR-Codes und scann ihn.
- **Kontonummern:** Auch eine IBAN hat Prüfziffern, die dritte und vierte
  Stelle; gerechnet wird mit dem Rest bei der Division durch 97.

## Zum Nachschlagen

- [Pseudocode und Struktogramme](/reference/struktogramme) im Handbuch: alle
  Bausteine, ein Feld zum Ausprobieren, und wie aus Pseudocode Ruby oder Python
  wird.
- [Wie man eine Prüfziffer berechnet](https://www.gs1.org/services/how-calculate-check-digit-manually),
  bei GS1, auf Englisch: die Regel für EAN-13 und die anderen Codes von GS1.
- [ISBN Users' Manual](https://www.isbn-international.org/content/isbn-users-manual/29)
  der Internationalen ISBN-Agentur, auf Englisch: wie eine ISBN aufgebaut ist
  und wie ihre Prüfziffer berechnet wird.
