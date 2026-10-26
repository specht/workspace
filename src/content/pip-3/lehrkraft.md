Lernende bringen Pip an einem Monster vorbei, das nach jedem Schritt den kürzesten Weg zu Pip geht: zuerst mit gezählten Schritten und dem Licht der alten Lampen, dann mit einer eigenen Breitensuche auf der Karte, in Ruby, Python oder JavaScript.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/pip-2'>Pip unter Funkelbach: Bellos Spur</a>, Listen und Dictionaries, etwa aus den <a href='/kata-lists'>Katas: Listen</a> und <a href='/kata-dicts'>Katas: Dictionaries</a>. Die Breitensuche steht im Handbuch unter <a href='/reference/graphen'>Graphen</a>.</td></tr>
<tr><th>Ablauf</th><td>Etwa zwei Doppelstunden, vier Kapitel.<ul>
<li>Der Mampfer und Im Licht: eine Vorhersage (er geht den kürzeren Weg um den Felsen, nicht dorthin, wo Pip näher aussieht), dann Pläne, in denen jeder Schritt zählt, auch eine Drehung. Im Licht einer brennenden Lampe ist Pip sicher; zuletzt lockt Pip den Mampfer auf die eine Seite, um die andere zu schaffen.</li>
<li>Was der Mampfer weiß und Der kürzeste Weg: die Breitensuche an einem Widget auf Pips Karte, dann Funktionen auf der Karte als Liste von Zeilen (<code>find</code>, <code>neighbors</code>, <code>distance</code>, <code>way</code>) und zum Schluss ein Plan, der die Karte liest, den kürzesten Weg sucht und ihn abläuft. Extras: wohin der Mampfer als Nächstes geht, was ein Weg mit seinen Drehungen kostet.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Jeder Plan läuft in einem eigenen Container ohne Netz; die Welt auf dem Server entscheidet jeden Schritt, auch den des Mampfers. Die Funktionen prüft sie mit festen Karten, auf denen es genau einen kürzesten Weg gibt. Der letzte Plan läuft auf drei Höhlen, zwei davon aus einem Seed je Person; auf ihnen schafft es jeder kürzeste Weg, auch mit Drehungen nur nach links, und die Hand an der Wand aus Teil 2 nicht.</td></tr>
<tr><th>Gespeichert</th><td>Der Plan und der Code jeder Kata, für jede Sprache getrennt, sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist, und eingereichte Lösungen ohne Namen.</td></tr>
</table>

#### Was schiefgeht

- Drehungen nicht mitzählen: Eine Drehung nach rechts sind drei Schritte, in denen der Mampfer drei Felder näher kommt.
- Im Licht warten wollen, bis der Mampfer weggeht: Er bleibt am Rand des Lichts stehen, solange Pip darin ist, und geht nicht weg.
- In Python Positionen als Listen: Eine Liste kann kein Schlüssel eines Dictionarys sein (»unhashable type: 'list'«), und die Prüfung erwartet Tupel. In JavaScript wird ein Array als Schlüssel eines Objekts zum Text <code>'3,1'</code>; das geht, solange alle Positionen Arrays von Zahlen sind.
- <code>[y, x]</code> statt <code>[x, y]</code>: Die Karte wird als <code>cave[y][x]</code> gelesen, die Position aber zuerst mit der Spalte geschrieben.
- Ein Feld erst beim Herausnehmen aus der Queue als gefunden merken: Dann kommt es mehrmals hinein; auf den kleinen Karten fällt das nicht auf, ist aber eine gute Frage im Gespräch.
- Die Hand an der Wand aus Teil 2 im letzten Plan: In Höhlen mit Rundwegen läuft sie Umwege oder im Kreis, und der Mampfer holt Pip ein.

#### Die Aufgaben

- Der Mampfer: Vorhersage `predict`, `run`.
- Im Licht: `stone-in-the-dark`, `both-sides`.
- Was der Mampfer weiß: `find`, `neighbors`, `distance`, `way`.
- Der kürzeste Weg: `face`, `escape`; Extras `next-step`, `steps`.
