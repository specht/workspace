<div class='meta'>
icon: bi-toggles
kind: uebung
duration: 2 Doppelstunden
languages: [ruby, python, javascript]
idea: Zahlen in Bits, im Sechzehnersystem und in jedem anderen, und Addieren mit Übertrag
curriculum: "Sek I: Information und Daten (Zahlensysteme, Rechnen mit Dualzahlen, Codierung)"
topics: [zahlensysteme, codierung]
requires: [kata-dicts]
</div>

# Katas: Bits und Zahlensysteme

<p class='abstract'>
Du schreibst Funktionen, die Zahlen in Bits zerlegen und wieder
zusammensetzen, ins Sechzehnersystem und in jedes andere schreiben, negative
Zahlen in einem Byte darstellen, Bits addieren wie von Hand und zum Schluss
römisch.
</p>

## Teilen mit Rest

Der Rechner speichert jede Zahl als Bits, als Nullen und Einsen. Im
Zweiersystem ist jede Stelle doppelt so viel wert wie die rechts von ihr:
`1101` ist 8 + 4 + 0 + 1 = 13. Die letzte Ziffer einer Zahl in einem
Zahlensystem ist der Rest beim Teilen durch seine Basis, im Zehnersystem wie
bei der Quersumme:

<!-- language: ruby -->
```ruby
puts 13 % 2       # 1, das letzte Bit
puts 13 / 2       # 6: das letzte Bit ist weg, 6 ist 110
puts 1234 % 10    # 4, die letzte Ziffer
puts 1234 / 10    # 123
```

Bei ganzen Zahlen rundet `/` ab.
<!-- /language -->
<!-- language: python -->
```python
print(13 % 2)      # 1, das letzte Bit
print(13 // 2)     # 6: das letzte Bit ist weg, 6 ist 110
print(1234 % 10)   # 4, die letzte Ziffer
print(1234 // 10)  # 123
```

`//` teilt und rundet ab; `/` gibt eine Kommazahl.
<!-- /language -->
<!-- language: javascript -->
```javascript
console.log(13 % 2);                  // 1, das letzte Bit
console.log(Math.floor(13 / 2));      // 6: das letzte Bit ist weg, 6 ist 110
console.log(1234 % 10);               // 4, die letzte Ziffer
console.log(Math.floor(1234 / 10));   // 123
```

`/` gibt eine Kommazahl; `Math.floor` rundet ab.
<!-- /language -->

Teilt man immer wieder und sammelt die Reste, hat man alle Ziffern, von
rechts nach links. Was Bits und Bytes im Rechner sind, steht im Handbuch
unter [Bits und Bytes](/reference/bits-und-bytes).

## Einstieg

_task(to-binary)

_task(from-binary)

_task(bit)

_task(binary-length)

## Übung

_task(count-ones)

_task(to-hex)

_task(from-hex)

<div class='hint try'>
<strong>Ausprobieren:</strong> Eine Farbe im Netz besteht aus drei Bytes,
Rot, Grün und Blau, jedes von 0 bis 255: <code>#ff8800</code> ist
<code>ff</code>, <code>88</code>, <code>00</code>. Rechne mit deiner
Funktion aus, wie viel Rot, Grün und Blau in <code>#1e90ff</code> steckt.
</div>

_task(power-of-two)

## Knobelei

_task(twos-complement)

_task(to-base)

_task(convert)

## Addieren

Addiert wird im Zweiersystem wie im Zehnersystem, schriftlich, von rechts
nach links, Stelle für Stelle. Nur ist eine Stelle schon bei 2 voll: 1 + 1
ist `10`. Es gibt vier Fälle:

- 0 + 0 = 0
- 0 + 1 = 1, ebenso 1 + 0
- 1 + 1 = `10`: Schreib 0, die 1 geht als Übertrag zur nächsten Stelle links.
- 1 + 1 + 1 = `11`, mit einem Übertrag von rechts: Schreib 1, Übertrag 1.

So rechnet man 5 + 7. Der Übertrag steht über der Stelle, zu der er kommt:

```text
Übertrag   1 1 1
           0 1 0 1      5
         + 0 1 1 1    + 7
         ---------
           1 1 0 0     12
```

_task(add-by-hand)

<!-- after: add-by-hand -->
```text
Übertrag 1 1 1
           1 0 1 1     11
         + 0 1 1 0   +  6
         ---------
         1 0 0 0 1     17
```

Der letzte Übertrag hat keine Stelle mehr über sich: Er wird eine neue Stelle
vorn, und das Ergebnis ist ein Bit länger als die beiden Zahlen.
<!-- /after -->

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Explication de l'Arithmétique Binaire</strong>, Gottfried Wilhelm Leibniz, 1703. Leibniz schreibt die Zahlen nur mit 0 und 1 und rechnet damit, an Beispielen für Addieren, Subtrahieren, Multiplizieren und Dividieren. Auswendig lernen müsse man dafür nichts, nicht einmal, dass 6 und 7 zusammen 13 sind. Auf Französisch, auf <a href='https://fr.wikisource.org/wiki/Leibniz-en.francais-Gerhardt.Math.1a7.djvu/Num%C3%A9ration.binaire'>Wikisource</a>.</p>
</div>

Für den Code nimmst du dir zuerst eine einzige Stelle vor.

_task(add-digits)

Im Rechner tut das eine kleine Schaltung aus wenigen Gattern, der
**Volladdierer**: drei Bits hinein, das Bit der Stelle und der Übertrag
heraus. Für ein Byte hängen acht davon hintereinander, und jeder gibt seinen
Übertrag an den nächsten links weiter.

_task(add-binary)

### Überlauf

In Ruby und Python bekommt eine ganze Zahl einfach mehr Bits, wenn sie welche
braucht. Der Prozessor darunter rechnet in Registern mit fester Breite, 8, 16,
32 oder 64 Bits, und ein Byte hat acht.

_task(byte-overflow)

<!-- after: byte-overflow -->
```text
Übertrag 1 1
           1 1 0 0 1 0 0 0    200
         + 0 1 1 0 0 1 0 0  + 100
         -----------------
         1 0 0 1 0 1 1 0 0    300
```

300 braucht neun Bits. Das neunte hat im Byte keinen Platz und fällt weg;
im Byte bleibt `00101100`, also 44 = 300 − 256. Das heißt **Überlauf**. Der
Prozessor merkt sich das Bit, das herausfällt, als Flag, und rechnet weiter;
was das Ergebnis bedeutet, muss das Programm wissen. Pac-Man (1980) zählt die
Level in einem Byte: In Level 256 läuft eine Rechnung damit über, die rechte
Hälfte des Bildschirms füllt sich mit Zeichensalat, und das Level ist nicht
zu schaffen ([The Pac-Man Dossier](https://pacman.holenet.info/), englisch). Was in C, Ruby, Python und JavaScript überläuft und was nicht,
steht im Handbuch unter [Negative Zahlen](/reference/bits-und-bytes#zweierkomplement).

_task(add-byte)
<!-- /after -->

### Negative Zahlen

In der Kata »Negative Zahlen in einem Byte« steht −5 als 256 − 5 = 251, also
`11111011`. Der Rechner kommt ohne Subtrahieren dorthin: Er dreht alle Bits
um, aus jeder 0 eine 1 und aus jeder 1 eine 0, und addiert 1.

_task(negate)

<div class='hint try'>
<strong>Ausprobieren:</strong> Damit braucht der Rechner zum Subtrahieren
kein eigenes Rechenwerk: 7 − 5 ist 7 + (−5). Schreib deine Funktion für ein
Byte mit in die Karte und addiere <code>00000111</code> und das negierte
<code>00000101</code>. Achte darauf, welche Zahl im Byte steht und ob vorn ein
Übertrag herausfällt.
</div>

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen: ein Zahlensystem ohne Stellen.

_task(roman)

_task(from-roman)
