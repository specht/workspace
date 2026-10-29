Ein Projekt zur Objektorientierung: Lernende spielen zuerst ein kleines Textadventure auf der Seite, »Nachts in der Schule« (nach der letzten Stunde eingeschlafen, die Schule ist abgeschlossen), und bauen es dann nach, aus Räumen, Gegenständen, einer Spielerin und einem Spiel. Sie entwerfen die Klassen zuerst als Diagramm, kapseln die Tasche der Spielerin, lassen Schlüssel, Apfel und Taschenlampe von `Item` und einen dunklen Keller und ein Ziel von `Room` erben, und spielen die Schule dann mit ihren eigenen Klassen durch. Zum Schluss baut jede und jeder eine eigene Welt mit einem Lösungsweg, den die Prüfung durchspielt, und jemand aus der Klasse spielt sie. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Klassen und Objekte, Attribute, Methoden und Vererbung, etwa aus den <a href='/kata-objects'>Katas: Objekte</a>; Dictionaries und Listen. Klassendiagramme zeigt die Seite selbst, mit einem Link ins <a href='/reference/uml'>Handbuch</a>; wer <a href='/supermarkt'>Klassen entwerfen: die Expresskasse</a> kennt, ist schneller.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das Spiel auf der Seite spielen, die Klassen im Spieltext finden und als Diagramm zeichnen, ein Raum mit Ausgängen und die Vorhersage, ob ein Ausgang zurückführt, Gegenstände, die Beschreibung eines Raums.</li>
<li><b>2. Doppelstunde:</b> die Spielerin mit ihrer Tasche, Kapselung (wer den gegessenen Apfel aus der Tasche nimmt), das Diagramm der Unterklassen, Schlüssel, Schloss und Apfel.</li>
<li><b>3. Doppelstunde:</b> Polymorphie im Dunkeln (die Vorhersage, ob ein Schlüssel leuchtet, die Taschenlampe, der dunkle Keller), das Spiel als Objekt mit einem neuen Befehl; danach spielen die Lernenden die Schule mit ihren eigenen Klassen.</li>
<li><b>4. Doppelstunde:</b> die eigene Welt mit Lösungsweg, der Spieltest in Paaren, die Abgabe. Wer früher fertig ist: eine Kiste mit Inhalt (Extra).</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/textadventure</code>: die Klassen, die eigene Welt (<code>build_world</code>) und der Lösungsweg (<code>WALKTHROUGH</code>) in <code>adventure.rb</code> oder <code>adventure.py</code>, das Diagramm <code>textadventure.uml</code>, dazu die Spielschleife <code>play.rb</code> oder <code>play.py</code> und die Schule <code>schule.rb</code> oder <code>schule.py</code>, die die Lernenden herunterladen. Die Klassen schreiben sie selbst; nur die Spielschleife, die Schule und <code>all_rooms</code> sind fertig.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Acht Aufgaben und das Extra rufen den Code der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann: Sie bauen Räume, legen Gegenstände hinein, schicken eine Spielerin los oder geben dem Spiel Zeilen und vergleichen die Texte genau, die zurückkommen. Der Workspace muss dafür laufen.</li>
<li>Die eigene Welt prüft, dass man vom ersten Raum aus mindestens vier erreicht, dass es eine verschlossene Tür gibt, dass der Lösungsweg im Spiel ans Ziel kommt, und dass er es ohne seine <code>benutze</code>-Zeilen nicht tut: Die Tür liegt also auf dem Weg, und der Schlüssel ist vorher zu finden.</li>
<li>Zwei Diagramme liest die UML-Werkstatt: die drei Klassen mit ihren Beziehungen, dann die Unterklassen, die <code>use</code>, das Leuchten und das Dunkelsein überschreiben.</li>
<li>Zwei Vorhersagen, eine Frage zur Kapselung, eine offene Frage zum Spieltest und die Abgabe des Ordners.</li>
</ul></td></tr>
<tr><th>Gespeichert</th><td>Die Abgabe: der Ordner <code>textadventure</code> in dem Stand, in dem er abgegeben wurde (höchstens 1 MB), und die Antwort auf die Frage zum Spieltest.</td></tr>
</table>

#### Die Aufgaben

- »Erst der Plan« sucht die Klassen im Spieltext, bevor jemand programmiert: Hauptwörter werden Klassen, Verben Methoden, »kennt« eine Beziehung. Die Prüfung verlangt nur die drei Klassen, ihre Namen und Beschreibungen und die drei Beziehungen; wer mehr zeichnet, liegt nicht falsch.
- »Und zurück?« ist die Vorhersage zu Objekten, die einander kennen: `connect` legt nur einen Ausgang an. Wer »den Flur« wählt, stellt sich die Verbindung als etwas vor, das zwischen den Räumen liegt, statt als Eintrag im Dictionary eines Raums.
- »Wer räumt den Apfel weg?« ist der Kern der Kapselung: Der Apfel bittet die Spielerin, `remove`, statt sich selbst aus ihrer Liste zu löschen. Die Antwort, dass die Spielschleife aufräumt, ist ein guter Anlass zu fragen, woher sie wüsste, welche Gegenstände verschwinden.
- »Wer leuchtet?« zeigt, dass eine Unterklasse erbt, was sie nicht selbst schreibt: Der Schlüssel leuchtet nicht, ohne dass er es sagen muss. Die Seite begründet, warum die Spielerin jeden Gegenstand fragt, statt nach `Lamp` zu suchen: Eine Kerze, die später dazukommt, leuchtet dann von selbst mit.
- Eine Tür ist kein eigenes Objekt: Der Raum merkt sich, welche Richtung verschlossen ist und welcher Schlüssel passt. Wer eine Klasse `Door` vorschlägt, hat einen guten Einwand: Dann wäre eine Tür von beiden Seiten verschlossen, was hier jede Seite für sich regelt.
- `Game` liest und schreibt nichts selbst; die Spielschleife gibt ihm Zeilen. Darum kann die Prüfung einen ganzen Lösungsweg durchspielen, und darum lässt sich die Schule mit den Klassen der Lernenden spielen.

#### Was schiefgeht

- **<code>take</code> findet den Gegenstand, nimmt ihn aber nicht aus dem Raum:** Er liegt dann im Raum und in der Tasche. Die Prüfung meldet es eigens.
- **Die Spielerin geht durch eine verschlossene Tür:** <code>go</code> fragt <code>locked?</code> oder <code>is_locked</code> nicht, bevor es den Raum wechselt.
- **Der Apfel bleibt nach dem Essen in der Tasche:** <code>Food</code> ruft <code>player.remove(self)</code> nicht auf.
- **Ein Raum ohne Licht und mit Lampe darin:** Liegt die Taschenlampe in einem dunklen Raum, findet man sie nie. In der eigenen Welt fällt das im Spieltest auf, oder im Lösungsweg, der nicht ankommt.
- **Python: <code>KeyError</code> in <code>exit_to</code>:** <code>self._exits[direction]</code> statt <code>self._exits.get(direction)</code>.
- **»Hier liegt nichts mit dem Namen …«, obwohl der Gegenstand da ist:** Das Spiel schreibt alles klein, was getippt wird; ein Gegenstand namens <code>Schlüssel</code> ist dann nicht zu finden. Die Seite sagt, Namen und Richtungen klein zu schreiben.
- **Der Lösungsweg kommt nicht an:** Am schnellsten findet man die Stelle, indem man ihn mit <code>play.rb</code> oder <code>play.py</code> Zeile für Zeile selbst tippt.
- **<code>play.rb schule</code> bricht ab:** Die Schule braucht alle Klassen bis einschließlich <code>Game</code>; vorher fehlt eine, und die Meldung nennt sie.

#### Weiter

Die Ideen am Ende (eine Figur, die tauscht, Punkte, eine Karte, ein Spielstand in einer Datei) beginnen jede mit der Frage, welche Klasse was wissen soll. Beim Speichern zeigt sich, dass Räume, die aufeinander zeigen, nicht einfach eine Liste sind: ein Anlass für eindeutige Namen oder Nummern als Verweise, wie in einer Datenbank.
