Ein Projekt zur Objektorientierung: Lernende bauen ein Textadventure, das man im Terminal spielt, aus Räumen, Gegenständen und einer Spielerin. Räume zeigen über ein Dictionary auf ihre Nachbarn, Gegenstände wandern vom Raum in die Tasche, und Schlüssel und Apfel erben von `Item` und überschreiben `use`, so dass die Spielerin nur `use` aufruft. Zum Schluss baut jede und jeder eine eigene Welt, und jemand aus der Klasse spielt sie. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Klassen und Objekte, Attribute, Methoden und Vererbung, etwa aus den <a href='/kata-objects'>Katas: Objekte</a>; Dictionaries und Listen. Klassendiagramme kennen die Lernenden aus <a href='/supermarkt'>Klassen entwerfen: die Expresskasse</a> oder aus dem <a href='/reference/uml'>Handbuch</a>; die Seite braucht nur ein kleines.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das Spiel, ein Raum mit Ausgängen und die Vorhersage, ob ein Ausgang auch zurückführt, Gegenstände, die Beschreibung eines Raums.</li>
<li><b>2. Doppelstunde:</b> die Spielerin, die geht und nimmt, das Klassendiagramm mit <code>Key</code> und <code>Food</code>, verschlossene Türen und <code>use</code>.</li>
<li><b>3. Doppelstunde:</b> die eigene Welt mit der fertigen Spielschleife, der Spieltest in Paaren, die Abgabe.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/textadventure</code>: die Klassen und <code>build_world</code> in <code>adventure.rb</code> oder <code>adventure.py</code>, das Diagramm <code>textadventure.uml</code> und die Spielschleife <code>play.rb</code> oder <code>play.py</code>, die die Lernenden herunterladen. Die Klassen schreiben sie selbst; nur die Spielschleife und <code>all_rooms</code> stehen fertig auf der Seite.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sechs Aufgaben rufen den Code der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann: Sie bauen Räume, verbinden sie, legen Gegenstände hinein, schicken eine Spielerin los und vergleichen die Texte genau, die sie zurückbekommen. Der Workspace muss dafür laufen.</li>
<li>Die eigene Welt prüft nur das Mindeste: drei Räume, die man vom ersten aus erreicht (mit <code>all_rooms</code>), eine verschlossene Tür und ein Schlüssel. Ob der Schlüssel zur Tür passt und ob man ihn erreicht, ohne durch sie zu müssen, zeigt erst der Spieltest.</li>
<li>Das Diagramm liest die UML-Werkstatt: <code>Key</code> und <code>Food</code> erben von <code>Item</code> und überschreiben <code>use</code>.</li>
<li>Eine Vorhersage, eine offene Frage zum Spieltest und die Abgabe des Ordners.</li>
</ul></td></tr>
<tr><th>Gespeichert</th><td>Die Abgabe: der Ordner <code>textadventure</code> in dem Stand, in dem er abgegeben wurde (höchstens 1 MB), und die Antwort auf die Frage zum Spieltest.</td></tr>
</table>

#### Die Aufgaben

- »Und zurück?« ist die Vorhersage zu Objekten, die einander kennen: `connect` legt nur einen Ausgang an. Wer »den Flur« wählt, stellt sich die Verbindung als etwas vor, das zwischen den Räumen liegt, statt als Eintrag im Dictionary eines Raums. Einbahnstraßen sind im Spiel erlaubt, eine Rutsche etwa.
- Eine Tür ist kein eigenes Objekt: Der Raum merkt sich, welche Richtung verschlossen ist und welcher Schlüssel passt. Das hält die Klassen klein. Wer eine Klasse `Door` vorschlägt, hat einen guten Einwand: Dann wäre eine Tür von beiden Seiten verschlossen, was hier jede Seite für sich regelt.
- `Food#use` nimmt sich selbst aus der Tasche der Spielerin: ein Objekt, das über sein Ende entscheidet, und ein Anlass zu fragen, wer das sonst tun sollte.
- Die Texte sind genau vorgegeben, weil die Prüfung sie vergleicht. Die Meldungen der Prüfung sagen, welcher Text fehlt oder abweicht.

#### Was schiefgeht

- **<code>take</code> findet den Gegenstand, nimmt ihn aber nicht aus dem Raum:** Er liegt dann im Raum und in der Tasche. Die Prüfung meldet es eigens.
- **Die Spielerin geht durch eine verschlossene Tür:** <code>go</code> fragt <code>locked?</code> oder <code>is_locked</code> nicht, bevor es den Raum wechselt.
- **Python: <code>KeyError</code> in <code>exit_to</code>:** <code>self.exits[direction]</code> statt <code>self.exits.get(direction)</code>.
- **»Hier liegt nichts mit dem Namen …«, obwohl der Gegenstand da ist:** Die Spielschleife schreibt alles klein, was getippt wird; ein Gegenstand namens <code>Schlüssel</code> ist dann nicht zu finden. Die Seite sagt, die Namen klein zu schreiben.
- **<code>play.rb</code> oder <code>play.py</code> findet <code>build_world</code> nicht:** Die Spielschleife muss im selben Ordner liegen wie <code>adventure.rb</code> oder <code>adventure.py</code>, und die Funktion muss genau so heißen.
- **Die UML-Werkstatt zeigt nichts:** Die Datei muss auf <code>.uml</code> enden; das Diagramm öffnet das Symbol oben rechts im Editor.

#### Weiter

Die Ideen am Ende (ein Ziel, eine Lampe für dunkle Räume, ein Spielstand in einer Datei) beginnen jede mit der Frage, welche Klasse was wissen soll. Beim Speichern zeigt sich, dass Räume, die aufeinander zeigen, nicht einfach eine Liste sind: ein Anlass für eindeutige Namen oder Nummern als Verweise, wie in einer Datenbank.
