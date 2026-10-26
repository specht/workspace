<div class='meta'>
icon: bi-toggles
accent: 340
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Zahlen in Bits, im Sechzehnersystem und in jedem anderen
curriculum: "Sek I: Information und Daten (Zahlensysteme, Codierung)"
requires: [kata-dicts]
</div>

# Katas: Bits und Zahlensysteme

<p class='abstract'>
Du schreibst Funktionen, die Zahlen in Bits zerlegen und wieder
zusammensetzen, ins Sechzehnersystem und in jedes andere schreiben, negative
Zahlen in einem Byte darstellen und zum Schluss römisch.
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

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen: ein Zahlensystem ohne Stellen.

_task(roman)

_task(from-roman)
