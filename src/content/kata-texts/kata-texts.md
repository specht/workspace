<div class='meta'>
icon: bi-quote
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Buchstaben zählen, umdrehen, umwandeln und verschlüsseln
curriculum: "Sek I: Algorithmisches Problemlösen (Zeichenkettenbearbeitung), Information und Daten (Codierung, Verschlüsselung)"
requires: [kata-loops]
</div>

# Katas: Strings

<p class='abstract'>
Du schreibst Funktionen, die mit Strings arbeiten: Buchstaben zählen, einen
String umdrehen, Großbuchstaben aus Nummern rechnen, eine Adresse aus einem
Titel bauen und zum Schluss verschlüsseln wie Caesar.
</p>

## Buchstabe für Buchstabe

Ein Text ist eine Reihe von Zeichen. Man kann sie der Reihe nach durchgehen,
nach ihrem Index fragen und aus ihnen einen neuen Text bauen. Diese Funktion
setzt jedes Zeichen in eckige Klammern:

<!-- language: ruby -->
```ruby
def bracket(text)
  result = ''
  text.each_char do |char|
    result += '[' + char + ']'
  end
  result
end

puts bracket('Hallo')     # [H][a][l][l][o]
puts 'Hallo'.length       # 5
puts 'Hallo'[0]           # H, das Zeichen mit dem Index 0
puts 'a'.ord              # 97, die Nummer des Zeichens
puts 98.chr               # b
```

`text.each_char do |char| … end` geht die Zeichen der Reihe nach durch,
`result += …` hängt hinten an. `text.split` zerlegt einen Text an den
Leerzeichen in eine Liste von Wörtern, `words.join(' ')` verbindet sie wieder.
<!-- /language -->
<!-- language: python -->
```python
def bracket(text):
    result = ''
    for char in text:
        result += '[' + char + ']'
    return result

print(bracket('Hallo'))   # [H][a][l][l][o]
print(len('Hallo'))       # 5
print('Hallo'[0])         # H, das Zeichen mit dem Index 0
print(ord('a'))           # 97, die Nummer des Zeichens
print(chr(98))            # b
```

`for char in text:` geht die Zeichen der Reihe nach durch, `result += …`
hängt hinten an. `text.split()` zerlegt einen Text an den Leerzeichen in eine
Liste von Wörtern, `' '.join(words)` verbindet sie wieder.
<!-- /language -->
<!-- language: javascript -->
```javascript
function bracket(text) {
  let result = '';
  for (const char of text) {
    result += '[' + char + ']';
  }
  return result;
}

console.log(bracket('Hallo'));          // [H][a][l][l][o]
console.log('Hallo'.length);            // 5
console.log('Hallo'[0]);                // H, das Zeichen mit dem Index 0
console.log('a'.charCodeAt(0));         // 97, die Nummer des Zeichens
console.log(String.fromCharCode(98));   // b
```

`for (const char of text) { … }` geht die Zeichen der Reihe nach durch,
`result += …` hängt hinten an. `text.split(' ')` zerlegt einen Text an jedem
Leerzeichen in eine Liste, `words.join(' ')` verbindet sie wieder.
<!-- /language -->

Jedes Zeichen hat eine Nummer: `A` ist 65, `B` 66 und so weiter, `a` ist 97.
Welche Nummer zu welchem Zeichen gehört, legt der Zeichensatz fest; die
ersten 128 sind seit den 1960er Jahren dieselben (ASCII) und heute ein Teil
von Unicode. Mit den Nummern kann man rechnen.

## Einstieg

_task(reverse-text)

_task(count-vowels)

_task(initials)

_task(shout)

<div class='hint try'>
<strong>Ausprobieren:</strong> Ruf deine Funktion <code>shout</code> mit
<code>'grüße'</code> auf. Was passiert mit dem <code>ü</code> und dem
<code>ß</code>, und warum? Die fertigen Funktionen der Sprache machen aus
<code>ü</code> ein <code>Ü</code>: Sie kennen mehr als die 26 Buchstaben.
</div>

## Übung

_task(palindrome)

_task(word-count)

_task(longest-word)

_task(slug)

## Knobelei

_task(caesar)

<div class='hint try'>
<strong>Ausprobieren:</strong> Verschlüssle einen Satz mit 13 und das
Ergebnis noch einmal mit 13. Was kommt heraus, und warum gerade bei 13?
</div>

Caesar lässt sich knacken, indem man alle 25 Verschiebungen ausprobiert. Wie
heute verschlüsselt wird, mit einem Passwort, mit zwei Schlüsseln und mit
Unterschriften, zeigt die Aktivität [Kryptographie im Terminal](/crypto).

_task(anagram)

_task(format-name)

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen.

_task(vigenere)

_task(strong-password)
