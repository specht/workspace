<div class='meta'>
icon: bi-book
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript, dart, java]
idea: Nachschlagen, zählen und gruppieren mit Schlüsseln und Werten
curriculum: "Sek I: Algorithmisches Problemlösen (Implementieren von Algorithmen mit Schlüsseln und Werten); Oberstufe: Algorithmen und Datenstrukturen (Hash Table)"
topics: [datenstrukturen]
age: Sek I
requires: [kata-lists]
</div>

# Katas: Dictionaries

<p class='abstract'>
Du schreibst Funktionen mit einer Struktur, die zu jedem Schlüssel einen Wert
hat: nachschlagen wie im Telefonbuch, Strichlisten führen, Wörter in Gruppen
ordnen und einen kleinen Cache nachbauen.
</p>

## Schlüssel und Wert

Eine Liste findet ihre Elemente über den Index, 0, 1, 2. Oft will man aber
über etwas anderes nachschlagen: die Nummer zu einem Namen, den Preis zu
einer Ware. Dafür ordnet man jedem Schlüssel einen Wert zu.

<!-- language: ruby -->
In Ruby heißt das ein **Hash**:

```ruby
ages = { 'Ada' => 36, 'Alan' => 41 }
puts ages['Ada']             # 36
ages['Grace'] = 85           # ein neues Paar
puts ages.key?('Linus')      # false: diesen Schlüssel gibt es nicht
puts ages.fetch('Linus', 0)  # 0, wenn es ihn nicht gibt
ages.each do |name, age|
  puts "#{name} wurde #{age}"
end
```
<!-- /language -->
<!-- language: python -->
In Python heißt das ein **Dictionary**:

```python
ages = {'Ada': 36, 'Alan': 41}
print(ages['Ada'])          # 36
ages['Grace'] = 85          # ein neues Paar
print('Linus' in ages)      # False: diesen Schlüssel gibt es nicht
print(ages.get('Linus', 0)) # 0, wenn es ihn nicht gibt
for name, age in ages.items():
    print(f'{name} wurde {age}')
```
<!-- /language -->
<!-- language: javascript -->
In JavaScript nimmt man dafür ein **Objekt**:

```javascript
const ages = { Ada: 36, Alan: 41 };
console.log(ages['Ada']);         // 36
ages['Grace'] = 85;               // ein neues Paar
console.log('Linus' in ages);     // false: diesen Schlüssel gibt es nicht
console.log(ages['Linus'] || 0);  // 0, wenn es ihn nicht gibt
for (const name in ages) {
  console.log(`${name} wurde ${ages[name]}`);
}
```

Die Schlüssel eines Objekts sind immer Texte; eine Zahl als Schlüssel wird zu
einem Text. Wer etwas anderes als Schlüssel braucht, nimmt eine `Map`.
<!-- /language -->
<!-- language: dart -->
In Dart heißt das eine **Map**:

```dart
void main() {
  final ages = {'Ada': 36, 'Alan': 41};
  print(ages['Ada']);                // 36
  ages['Grace'] = 85;                // ein neues Paar
  print(ages.containsKey('Linus'));  // false: diesen Schlüssel gibt es nicht
  print(ages['Linus'] ?? 0);         // 0, wenn es ihn nicht gibt
  for (final name in ages.keys) {
    print('$name wurde ${ages[name]}');
  }
}
```

`{'Ada': 36, 'Alan': 41}` ist eine `Map<String, int>`: Texte als
Schlüssel, Zahlen als Werte. `ages['Linus']` ist `null`, wenn es den
Schlüssel nicht gibt; `??` nimmt dann den Wert rechts davon. Weiß man, dass
es ihn gibt, schreibt man `ages['Ada']!`. Eine Map in Dart behält die
Reihenfolge, in der die Paare hineinkamen.
<!-- /language -->
<!-- language: java -->
In Java heißt das eine **Map**:

```java
void main() {
    Map<String, Integer> ages = new HashMap<>();
    ages.put("Ada", 36);
    ages.put("Alan", 41);
    IO.println(ages.get("Ada"));                // 36
    ages.put("Grace", 85);                      // ein neues Paar
    IO.println(ages.containsKey("Linus"));      // false: diesen Schlüssel gibt es nicht
    IO.println(ages.getOrDefault("Linus", 0));  // 0, wenn es ihn nicht gibt
    for (String name : ages.keySet()) {
        IO.println(name + " wurde " + ages.get(name));
    }
}
```

`Map<String, Integer>` hat Texte als Schlüssel und Zahlen als Werte;
`get` gibt `null` zurück, wenn es den Schlüssel nicht gibt. Eine `HashMap`
merkt sich keine Reihenfolge: Die Namen kommen in irgendeiner heraus. Eine
`LinkedHashMap` behält die Reihenfolge, in der die Paare hineinkamen.
`Map.of("Ada", 36, "Alan", 41)` legt eine Map an, die sich nicht mehr
ändern lässt.
<!-- /language -->

Nachschlagen geht in einem Dictionary sofort, ganz gleich, wie viele Paare
es hat; in einer Liste muss man suchen. Wie das gelingt, zeigt die
Handbuchseite [Hash Tables](/reference/hash-tables).

## Einstieg

_task(phone-number)

_task(count-items)

_task(total-price)

_task(keys-with-value)

## Übung

_task(letter-count)

<div class='hint try'>
<strong>Ausprobieren:</strong> Zähl die Buchstaben eines längeren deutschen
Textes, etwa eines Absatzes dieser Seite. Welcher Buchstabe kommt am
häufigsten vor? Mit so einer Zählung knackt man auch Verschlüsselungen wie
die von Caesar.
</div>

_task(most-frequent)

_task(group-by-length)

_task(merge-counts)

## Knobelei

_task(invert)

_task(averages)

_task(first-repeat)

<!-- extra -->
## Extras

Zwei Katas, die nicht mitzählen.

_task(lru-misses)

_task(anagram-groups)
