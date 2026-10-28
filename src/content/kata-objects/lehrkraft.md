Lernende schreiben Klassen und bauen daraus Objekte, direkt in den Karten der Seite, in Ruby, Python oder JavaScript: Attribute und Methoden, ein Objekt, das seine Regeln selbst durchsetzt, Objekte in Objekten, die eigene Textdarstellung, Vererbung mit überschriebenen Methoden und `super`. Die Seite zeigt jede Schreibweise zuerst an einem Beispiel, bevor eine Kata sie braucht.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Listen und Dictionaries, etwa aus den <a href='/kata-lists'>Katas: Listen</a> und den <a href='/kata-dicts'>Katas: Dictionaries</a>. Wie man Klassen aus einer Geschichte findet und als Klassendiagramm zeichnet, zeigt danach <a href='/supermarkt'>Klassen entwerfen: die Expresskasse</a>.</td></tr>
<tr><th>Ablauf</th><td>Zwei Doppelstunden, die erste bis einschließlich der Textdarstellung.<ul>
<li>Eine Klasse, viele Objekte (1 Vorhersage, 1 Kata): ein Zähler als Beispiel, die Vorhersage, dass zwei Zähler getrennt zählen, dann ein Rechteck mit Fläche und Umfang.</li>
<li>Kapselung (1 Kata): ein Konto, das ein Abheben ablehnt, das es nicht decken kann, und dann seinen Stand behält.</li>
<li>Objekte in Objekten (1 Kata): eine Playlist aus Songs rechnet ihre Dauer aus und findet den längsten Song (bei Gleichstand den ersten, in einer leeren Playlist keinen).</li>
<li>Textdarstellung (1 Kata): <code>to_s</code>, <code>__str__</code>, <code>toString</code>, an Geld mit zwei Ziffern für die Cent.</li>
<li>Vererbung (1 Vorhersage, 2 Katas): Tiere als Beispiel, die Vorhersage, welches <code>sound</code> ein geerbtes <code>describe</code> aufruft (Polymorphie), Flächen verschiedener Formen zusammenzählen, eine Managerin mit Bonus über <code>super</code>.</li>
<li>Extras (2): ein Stack als Klasse, ein Bruch, der kürzt und addiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Die Prüfung baut Objekte und ruft ihre Methoden in mehreren Schritten auf, auch mehrere Objekte nebeneinander, so dass ein gemeinsamer Zustand auffällt. Bei den Flächen gibt sie <code>total_area</code> auch eine Form, die keine Klasse der Lernenden ist (in Ruby ein <code>Struct</code>, in Python eine mit <code>type</code> gebaute Klasse, in JavaScript ein Objekt mit <code>area</code>): Wer nach der Klasse fragt statt nach <code>area</code>, fällt auf. Ob <code>super</code> benutzt wird, prüft sie nicht direkt; der Name der Managerin kommt aber nur über das geerbte <code>initialize</code> an.</td></tr>
<tr><th>Gespeichert</th><td>Der Code jeder Kata, für jede Sprache getrennt (Ruby, Python, JavaScript), sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist.</td></tr>
</table>

#### Was schiefgeht

- Die Parameter von <code>initialize</code>, <code>__init__</code> oder <code>constructor</code> werden nicht in Attributen gespeichert; später fehlen sie (in Ruby »undefined local variable«, in Python »has no attribute«, in JavaScript <code>NaN</code>).
- In Python heißt das Attribut wie die Methode, <code>self.balance</code> und <code>balance()</code>: Das Attribut überdeckt die Methode, und der Aufruf scheitert. Die Tipps schlagen einen anderen Namen vor.
- Beim Konto wird erst abgezogen und dann geprüft, oder genau der ganze Kontostand gilt schon als zu viel.
- Bei der Playlist gewinnt bei Gleichstand der letzte Song statt des ersten (<code>&gt;=</code> statt <code>&gt;</code>).
- Beim Geld fehlt die führende 0 der Cent (<code>3,5 €</code>), oder die Methode heißt nicht genau <code>to_s</code>, <code>__str__</code> oder <code>toString</code>, so dass der eingesetzte Text sie nicht findet.
- In JavaScript ruft der <code>constructor</code> von <code>Manager</code> nicht zuerst <code>super(name, monthly)</code> auf und bricht mit einem Fehler ab, sobald er <code>this</code> benutzt.

#### Die Aufgaben

- Eine Klasse, viele Objekte: die Vorhersage »Zwei Zähler«, `Rectangle`.
- Kapselung: `Account`.
- Objekte in Objekten: `Playlist` mit `total_seconds` und `longest_title`.
- Textdarstellung: `Money`.
- Vererbung: die Vorhersage »Wer sagt was?«, `Square`, `Rectangle` und `total_area`, `Manager`.
- Extras: `Stack`, `Fraction`.
