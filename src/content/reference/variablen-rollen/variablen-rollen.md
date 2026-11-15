<div class='meta'>
kind: reference
search: Rollen, Variablenrollen, Sajaniemi, Fixed Value, Stepper, Gatherer, Most-wanted Holder, Most-recent Holder, One-way Flag, Follower, Temporary, Organizer, Container, Sammler, Zähler, Flag
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Rollen von Variablen

<p class='abstract'>
Fast jede Variable in einem kleinen Programm hat eine von wenigen Aufgaben: Sie zählt die Durchläufe, sammelt eine Summe, merkt sich das Beste bisher oder hält fest, ob etwas schon passiert ist. Diese Aufgaben heißen Rollen. Wer sie kennt, erkennt sie in fremdem Code wieder und weiß beim Planen, welche Variablen ein Programm braucht. Hier steht jede Rolle mit einem Beispiel in Pseudocode.
</p>

<!-- part: rollen -->
<!-- short: Die Rolle einer Variablen: wie sie sich im Lauf des Programms ändert, etwa als Stepper oder Gatherer. -->
## Was eine Rolle ist

Diese Schleife zählt von 1 bis `n` und addiert dabei:

```pseudocode
total ← 0
for i from 1 to n:
    total ← total + i
```

`i` und `total` ändern sich beide in jedem Durchlauf, aber auf ganz verschiedene Art. Die Werte von `i` stehen fest, sobald die Schleife beginnt: 1, 2, 3 und so weiter bis `n`. Was in `total` steht, hängt davon ab, was in jedem Durchlauf dazukommt; es sammelt das Ergebnis. `i` ist ein **Stepper** (Schrittwert), `total` ein **Gatherer** (Sammler). Wie sich eine Variable im Lauf des Programms ändert, heißt ihre **Rolle**.

Der finnische Informatiker Jorma Sajaniemi hat um 2002 viele Programme von Anfängerinnen und Anfängern untersucht und dabei festgestellt, dass zehn Rollen für 99 % ihrer Variablen reichen. Er hat ihnen englische Namen gegeben, und so heißen sie auch hier, wie Stack und Queue: Der deutsche Name in Klammern sagt nur, was gemeint ist.

Rollen helfen zweimal:

- **Beim Lesen.** Wer in fremdem Code `best` vor der Schleife und `if x > best: best ← x` darin sieht, weiß sofort, was `best` am Ende enthält, ohne jeden Durchlauf nachzurechnen.
- **Beim Schreiben.** Wer eine Aufgabe liest, kann zuerst überlegen, welche Rollen das Programm braucht: »Ich brauche einen Stepper für die Zahlen und einen Gatherer für die Summe.« Jede Rolle bringt ihre typischen Zeilen mit.

Keine Programmiersprache hat ein Schlüsselwort für eine Rolle. Die Rolle steckt darin, wie das Programm eine Variable benutzt: wo sie ihren ersten Wert bekommt, wo sie sich ändert und wovon ihr neuer Wert abhängt.
<!-- /part -->

<!-- part: ueberblick -->
<!-- short: Die zehn Rollen mit dem, was sie tun, und ihrer typischen Zeile. -->
## Die Rollen auf einen Blick

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th>Rolle</th><th>Was sie tut</th><th>Typische Zeile</th></tr>
</thead>
<tbody>
<tr><td><a href='#fixed-value'>Fixed Value</a> (Festwert)</td><td>bekommt einmal einen Wert und behält ihn</td><td><code>tax_rate ← 0.19</code></td></tr>
<tr><td><a href='#stepper'>Stepper</a> (Schrittwert)</td><td>geht durch Werte, die feststehen, sobald es losgeht</td><td><code>i ← i + 1</code></td></tr>
<tr><td><a href='#most-recent-holder'>Most-recent Holder</a> (Letzter Wert)</td><td>hält den Wert, der zuletzt hereinkam</td><td><code>price ← read a number</code></td></tr>
<tr><td><a href='#most-wanted-holder'>Most-wanted Holder</a> (Bester bisher)</td><td>hält den besten Wert bisher</td><td><code>if x > best: best ← x</code></td></tr>
<tr><td><a href='#gatherer'>Gatherer</a> (Sammler)</td><td>sammelt, was alle Werte zusammen ergeben</td><td><code>total ← total + x</code></td></tr>
<tr><td><a href='#one-way-flag'>One-way Flag</a> (Einweg-Merker)</td><td>hat zwei Werte und wechselt höchstens einmal</td><td><code>if t < 0: frost ← true</code></td></tr>
<tr><td><a href='#follower'>Follower</a> (Nachläufer)</td><td>hält den vorigen Wert einer anderen Variablen</td><td><code>previous ← current</code></td></tr>
<tr><td><a href='#temporary'>Temporary</a> (Zwischenspeicher)</td><td>hält einen Wert für ganz kurze Zeit</td><td><code>t ← a</code></td></tr>
<tr><td><a href='#organizer'>Organizer</a> (Umordner)</td><td>eine Liste, deren Elemente nur umgeordnet werden</td><td><code>swap list[i] and list[j]</code></td></tr>
<tr><td><a href='#container'>Container</a> (Behälter)</td><td>eine Liste, in die Elemente hineinkommen und aus der sie wieder herausgehen</td><td><code>append x to cart</code></td></tr>
</tbody>
</table>
</div>

Die ersten sechs kommen in fast jeder Schleife vor; mit ihnen fängt man an. Sajaniemis Liste kennt noch den **Walker**, eine Variable, die durch eine verkettete Struktur läuft wie `k ← k.next` in einer [Linked List](/reference/listen#linked-list), und für den seltenen Rest die Rolle **Other**.
<!-- /part -->

## Die Rollen einzeln

<!-- part: fixed-value -->
<!-- short: Fixed Value: bekommt einmal einen Wert und wird danach nur gelesen. -->
### Fixed Value

Ein **Fixed Value** (Festwert) bekommt einmal einen Wert und behält ihn bis zum Ende. Danach wird er nur noch gelesen.

```pseudocode
tax_rate ← 0.19
for each price in prices:
    print price * (1 + tax_rate)
```

Woran du ihn erkennst: Es gibt nur eine Zuweisung, meist ganz oben oder vor der Schleife. Auch eine Eingabe, die das Programm einmal liest und nie ändert, ist ein Fixed Value. Viele Sprachen können das selbst sagen: `const` in JavaScript, `final` in Dart und Java, und Ruby und Python schreiben solche Namen in Großbuchstaben (`TAX_RATE`). Ein Name statt einer Zahl mitten im Code sagt, was die Zahl bedeutet, und lässt sich an einer einzigen Zeile ändern.
<!-- /part -->

<!-- part: stepper -->
<!-- short: Stepper: geht Schritt für Schritt durch Werte, die vorher feststehen, wie die Zählvariable einer Schleife. -->
### Stepper

Ein **Stepper** (Schrittwert) geht Schritt für Schritt durch eine Folge von Werten, die feststeht, sobald sie beginnt. Der neue Wert hängt nur vom alten ab, fast immer mit demselben Schritt. Die Zählvariable einer Schleife ist ein Stepper, ebenso eine Variable, die die Durchläufe zählt:

```pseudocode
for i from 0 to length - 1:          (i: 0, 1, 2 … bis length - 1)
    print list[i]

stake ← 1
steps ← 0
while stake < 1000:
    stake ← stake * 2                 (stake: 1, 2, 4, 8 …)
    steps ← steps + 1                 (steps: 0, 1, 2, 3 …)
```

Woran du ihn erkennst: Wer den ersten Wert und den Schritt kennt, kann alle weiteren aufschreiben, ohne das Programm laufen zu lassen. Ein Stepper zählt auch rückwärts, verdoppelt oder halbiert. Zählt eine Variable dagegen nur die Durchläufe, in denen eine Bedingung gilt, kennt man ihre Werte vorher nicht: Sie ist ein [Gatherer](#gatherer).
<!-- /part -->

<!-- part: most-recent-holder -->
<!-- short: Most-recent Holder: hält den Wert, der zuletzt hereinkam, eine Eingabe oder das Element dieses Durchlaufs. -->
### Most-recent Holder

Ein **Most-recent Holder** (Letzter Wert) hält den Wert, der zuletzt hereingekommen ist: die letzte Eingabe, die letzte Zeile einer Datei, das Element der Liste in diesem Durchlauf. Kommt ein neuer Wert, ist der alte vergessen.

```pseudocode
total ← 0
price ← read a number
while price ≠ 0:
    total ← total + price
    price ← read a number
```

`price` ist hier der Most-recent Holder, `total` ein Gatherer. Woran du ihn erkennst: Der neue Wert kommt von außen, nicht aus dem alten, und niemand kann ihn vorhersagen. Die Variable einer Schleife `for each word in words` ist meist ein Most-recent Holder, denn sie hält in jedem Durchlauf das Element, das gerade dran ist.
<!-- /part -->

<!-- part: most-wanted-holder -->
<!-- short: Most-wanted Holder: hält den besten Wert bisher, den größten, kleinsten oder längsten; er braucht einen guten Anfangswert. -->
### Most-wanted Holder

Ein **Most-wanted Holder** (Bester bisher) hält den besten Wert, den das Programm bisher gesehen hat: den größten, den kleinsten, den längsten, den frühesten. Er ändert sich nur, wenn ein besserer kommt.

```pseudocode
highscore ← 0
repeat for every round:
    score ← play a round
    if score > highscore:
        highscore ← score
```

Woran du ihn erkennst: Die Zuweisung steht in einem `if`, das den neuen Wert mit dem alten vergleicht. Am Ende steht in ihm der beste Wert von allen.

**Der Anfangswert:** Ein Most-wanted Holder braucht einen Wert, den jeder echte Wert schlagen kann. Hier geht 0, weil es keine Punkte unter 0 gibt. Bei Temperaturen ginge es nicht: Sind alle unter null, schlägt keine die 0, und am Ende stünde eine Temperatur da, die nie gemessen wurde. Sicher ist der erste Wert selbst als Anfang. Ob bei gleich guten Werten der erste oder der letzte gewinnt, entscheidet der Vergleich: `>` behält den ersten, `≥` nimmt den letzten.
<!-- /part -->

<!-- part: gatherer -->
<!-- short: Gatherer: sammelt, was alle Werte zusammen ergeben, eine Summe, eine Anzahl, einen Text; er fängt mit dem Wert an, der nichts ändert. -->
### Gatherer

Ein **Gatherer** (Sammler) sammelt, was alle Werte zusammen ergeben: eine Summe, ein Produkt, eine Anzahl, einen Text, der Zeichen für Zeichen wächst. Sein neuer Wert entsteht aus dem alten und dem Wert, der gerade dazukommt.

```pseudocode
letters ← 0
long_words ← 0
for each word in words:
    letters ← letters + length of word
    if length of word > 6:
        long_words ← long_words + 1
```

`letters` und `long_words` sind beide Gatherer. `long_words` zählt zwar in Schritten von 1, aber nur, wenn ein Wort lang genug ist; wie oft, hängt von den Wörtern ab.

Woran du ihn erkennst: Vor der Schleife bekommt er einen Anfangswert, in der Schleife steht er auf beiden Seiten der Zuweisung (`total ← total + x`), nach der Schleife ist er das Ergebnis.

**Der Anfangswert** ist der Wert, der nichts ändert: 0 für eine Summe oder eine Anzahl, 1 für ein Produkt, der leere Text `''` für einen Text, die leere Liste `[]` für eine Liste. Ein Produkt, das bei 0 anfängt, bleibt 0, was auch immer dazukommt.
<!-- /part -->

<!-- part: one-way-flag -->
<!-- short: One-way Flag: fängt mit false an, springt höchstens einmal auf true und nie zurück. -->
### One-way Flag

Ein **One-way Flag** (Einweg-Merker) hat nur zwei Werte, meist `false` und `true`. Es fängt mit dem einen an und springt höchstens einmal auf den anderen, nie zurück. So merkt sich ein Programm, ob etwas schon einmal passiert ist.

```pseudocode
frost ← false
for each t in temperatures:
    if t < 0:
        frost ← true
if frost: print "Es hat gefroren."
```

Woran du ihn erkennst: Die Zuweisung `frost ← true` steht in einem `if`, und nirgends steht `frost ← false`, außer ganz am Anfang. Gelesen wird das Flag erst nach der Schleife, wenn alle Werte durch sind.

**Typischer Fehler:** ein `else: frost ← false` in der Schleife. Dann springt das Flag bei jedem warmen Tag zurück, und am Ende sagt es nur etwas über den letzten Tag. Ob etwas nicht vorkam, steht erst fest, wenn die ganze Liste durch ist.
<!-- /part -->

<!-- part: follower -->
<!-- short: Follower: bekommt den Wert einer anderen Variablen, kurz bevor die sich ändert, und hält so ihren vorigen Wert. -->
### Follower

Ein **Follower** (Nachläufer) läuft einer anderen Variablen hinterher: Er bekommt ihren Wert, kurz bevor sie einen neuen bekommt, und hält so immer ihren vorigen. Damit vergleicht ein Programm zwei Werte, die nacheinander kommen.

```pseudocode
previous ← temperatures[0]
for i from 1 to length - 1:
    print temperatures[i] - previous      (wärmer oder kälter als am Tag davor)
    previous ← temperatures[i]
```

Woran du ihn erkennst: Er bekommt seinen Wert immer aus derselben anderen Variablen, und die Zuweisung steht am Ende des Durchlaufs, nachdem er gelesen wurde.
<!-- /part -->

<!-- part: temporary -->
<!-- short: Temporary: hält einen Wert für ein, zwei Zeilen, etwa beim Tauschen zweier Variablen. -->
### Temporary

Ein **Temporary** (Zwischenspeicher) hält einen Wert nur für ganz kurze Zeit, ein, zwei Zeilen lang. Am häufigsten beim Tauschen: Wer `a ← b` schreibt, hat den alten Wert von `a` verloren, darum kommt er vorher in `t`.

```pseudocode
t ← a
a ← b
b ← t
```

Woran du ihn erkennst: Er wird gesetzt und gleich danach gelesen, und sein Wert zählt im nächsten Durchlauf nicht mehr. Auch ein Zwischenergebnis, das eine lange Zeile in zwei kurze teilt, ist ein Temporary.
<!-- /part -->

<!-- part: organizer -->
<!-- short: Organizer: eine Liste, deren Elemente nur umgeordnet werden, beim Sortieren oder Mischen. -->
### Organizer

Ein **Organizer** (Umordner) ist eine Liste, deren Elemente gleich bleiben und nur ihre Reihenfolge ändern: Es kommt keines hinzu, und keines geht verloren. Dieses Programm mischt eine Liste wie einen Stapel Karten:

```pseudocode
for i from length - 1 down to 1:
    j ← a random number from 0 to i
    swap list[i] and list[j]
```

`list` ist der Organizer, `i` ein Stepper und `j` ein Most-recent Holder, denn in jedem Durchlauf kommt eine neue Zufallszahl. Jedes Sortierverfahren unter [Sortieren](/reference/sortieren) arbeitet mit einem Organizer.
<!-- /part -->

<!-- part: container -->
<!-- short: Container: eine Liste, in die Elemente hineinkommen und aus der sie wieder herausgehen. -->
### Container

Ein **Container** (Behälter) ist eine Liste, in die Elemente hineinkommen und aus der sie wieder herausgehen, während das Programm läuft: ein Warenkorb, eine Liste mit Aufgaben, die noch zu tun sind.

```pseudocode
cart ← []
command ← read a line
while command ≠ "fertig":
    if command starts with "+": append the rest to cart
    if command starts with "-": remove the rest from cart
    command ← read a line
```

Woran du ihn erkennst: Es gibt Zeilen, die anhängen, und Zeilen, die herausnehmen. Ein Container, der nur an einem Ende arbeitet, ist ein Stack oder eine Queue ([Listen, Stack und Queue](/reference/listen)).
<!-- /part -->

<!-- part: erkennen -->
<!-- short: Eine Rolle erkennen: alle Zuweisungen einer Variablen ansehen, oder ihre Spalte in einer Trace-Tabelle. -->
## Eine Rolle erkennen

Um die Rolle einer Variablen zu finden, such alle Zeilen, in denen sie einen Wert bekommt, und frag, wovon der neue Wert abhängt:

- Nur eine Zuweisung, danach nie wieder: **Fixed Value**.
- Nur vom eigenen alten Wert, mit immer demselben Schritt (`+ 1`, `* 2`, `/ 2`): **Stepper**.
- Von außen, und der alte Wert zählt nicht mehr: **Most-recent Holder**.
- In einem `if`, das den neuen Wert mit ihr vergleicht: **Most-wanted Holder**.
- Vom eigenen alten Wert und dem Wert, der gerade dazukommt: **Gatherer**.
- Zwei Werte, nur in einem `if` umgestellt und nie zurück: **One-way Flag**.
- Von einer anderen Variablen, kurz bevor die sich ändert: **Follower**.
- Gesetzt und eine Zeile später gelesen, danach nicht mehr gebraucht: **Temporary**.

Noch deutlicher wird es in einer **Trace-Tabelle**, die nach jedem Durchlauf festhält, was in den Variablen steht. Dieses Programm liest fünf Tage lang, wie viel es geregnet hat:

```pseudocode
limit ← 30
total ← 0
highest ← 0
flood ← false
for day from 1 to 5:
    rain ← read the rain of this day
    total ← total + rain
    if rain > highest: highest ← rain
    if rain > limit: flood ← true
```

Mit den Eingaben 4, 0, 35, 12 und 0 stehen nach jedem Durchlauf diese Werte darin:

<div class='scroll-x'>
<table class='table table-sm reference-table'>
<thead>
<tr><th><code>day</code></th><th><code>rain</code></th><th><code>total</code></th><th><code>highest</code></th><th><code>flood</code></th></tr>
</thead>
<tbody>
<tr><td>1</td><td>4</td><td>4</td><td>4</td><td><code>false</code></td></tr>
<tr><td>2</td><td>0</td><td>4</td><td>4</td><td><code>false</code></td></tr>
<tr><td>3</td><td>35</td><td>39</td><td>35</td><td><code>true</code></td></tr>
<tr><td>4</td><td>12</td><td>51</td><td>35</td><td><code>true</code></td></tr>
<tr><td>5</td><td>0</td><td>51</td><td>35</td><td><code>true</code></td></tr>
</tbody>
</table>
</div>

Jede Spalte hat das Muster ihrer Rolle. `day` zählt gleichmäßig weiter: ein Stepper. `rain` springt, wie es kommt: ein Most-recent Holder. `total` wächst um das, was in `rain` steht: ein Gatherer. `highest` bleibt stehen, bis ein größerer Wert kommt: ein Most-wanted Holder. `flood` springt einmal auf `true` und bleibt dort: ein One-way Flag. `limit` hat keine Spalte, weil es sich nie ändert: Es ist ein Fixed Value.

Dieselbe Variable kann in einem langen Programm nacheinander verschiedene Rollen spielen, etwa erst als Stepper eine Liste füllen und danach als Fixed Value ihre Länge halten. Übersichtlicher ist es meist, ihr dann zwei Namen zu geben.
<!-- /part -->

<!-- part: planen -->
<!-- short: Mit Rollen planen: erst überlegen, welche Rollen die Aufgabe braucht, dann ihre typischen Zeilen schreiben. -->
## Mit Rollen planen

Bevor du eine Schleife schreibst, frag die Aufgabe nach ihren Rollen:

1. **Was kommt herein?** Jeder Wert, der nacheinander kommt, braucht einen Most-recent Holder, oder die Schleife geht mit einem Stepper durch die Indizes.
2. **Was steht am Ende fest?** Eine Summe oder Anzahl ist ein Gatherer, das Größte oder Kleinste ein Most-wanted Holder, ein »ob es vorkam« ein One-way Flag.
3. **Was bleibt gleich?** Eine Grenze oder ein Faktor ist ein Fixed Value und bekommt einen Namen.

Jede Rolle bringt ihre Zeilen mit. Ein Gatherer braucht einen Anfangswert vor der Schleife und eine Zuweisung in ihr. Ein Most-wanted Holder braucht einen Anfangswert, den jeder Wert schlagen kann, und ein `if`. Ein One-way Flag braucht `false` vor der Schleife, `true` in einem `if` und wird erst nach der Schleife gelesen.

In Sajaniemis Kursen steht in den Beispielprogrammen neben jeder Variablen ein Kommentar mit ihrer Rolle. Wer so plant, sieht schon an den Kommentaren, ob etwas fehlt:

```pseudocode
limit ← 30          (Fixed Value: ab hier zählt ein Tag als Hochwasser)
total ← 0           (Gatherer: Regen aller Tage bisher)
flood ← false       (One-way Flag: gab es schon Hochwasser?)
```
<!-- /part -->

## Zum Nachschlagen

Selbst üben kannst du das Erkennen in den [Katas: Schleifen](/kata-loops). Sajaniemis eigene Seiten, alle auf Englisch:

- [The Roles of Variables Home Page](https://saja.kapsi.fi/var_roles/): die Startseite, mit Aufsätzen, Material für den Unterricht und einer Übung, in der man Variablen selbst ihre Rolle gibt.
- [An Introduction to the Roles of Variables](https://saja.kapsi.fi/var_roles/role_intro.html): die Idee an zwei kleinen Programmen.
- [Roles of Data Entities](https://saja.kapsi.fi/var_roles/role_list.html): jede Rolle genau, mit Beispielen in Pascal und Java und mit den Sonderfällen.
- [Using Roles of Variables in Programming Education](https://saja.kapsi.fi/var_roles/teaching.html): wie man die Rollen im Unterricht einführt, mit Listen der Rollen für Lernende in Python, Java und C.
