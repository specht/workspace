Lernende schreiben der Figur Pip einen Plan, Befehl für Befehl, in Ruby, Python oder JavaScript, und schicken Pip damit durch die Stollen unter Funkelbach: zur Lampe, zu jedem Funkenstein, an Glibbern vorbei. Im ersten Kapitel klicken sie Pip zur Lampe und sehen den Plan dabei entstehen; danach läuft der Plan erst, wenn er fertig ist; die Karte der Stollen über dem Plan spielt den Lauf Schritt für Schritt ab und markiert die Zeile, bei der Pip gerade ist.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine. Die Seite ist der Einstieg für eine Klasse, die noch nicht programmiert hat.</td></tr>
<tr><th>Ablauf</th><td>Etwa zwei Doppelstunden, sechs Kapitel.<ul>
<li>Der Eingang: Pip wird mit zwei Knöpfen zur Lampe geklickt, jeder Klick schreibt eine Zeile in den Plan daneben; so ist ein Plan zuerst die Liste dessen, was Pip getan hat. Pip schaut am Anfang nach unten, die erste Drehung nach links geht also auf dem Bildschirm nach rechts.</li>
<li>Der Schacht: ab hier wird getippt; eine Vorhersage (»Wo steht Pip?«, links ist von Pip aus gesehen links), dann zur Lampe mit <code>move</code> und <code>turn_left</code>; rechts ist dreimal links. Mit <code>take</code> der erste Stein.</li>
<li>Funkensteine: zwei lange Befehlsfolgen. Sie sollen mühsam werden, das ist der Grund für das nächste Kapitel. Extra: ein Plan mit einem Fehler, der in der Wiedergabe zu finden ist.</li>
<li>Die Wendeltreppe: Schleifen, zuletzt eine mit Zählvariable, deren Wert die Länge des nächsten Stücks ist. Extra: dieselbe Treppe in höchstens sieben Zeilen.</li>
<li>Die Pilzwiese: zwei Schleifen ineinander, dann drei Karten mit Steinen an verschiedenen Stellen, die nur ein <code>if</code> mit <code>on_stone</code> schafft.</li>
<li>Glibber!: Warten mit <code>wait</code>, zuerst abgezählt, dann auf drei Karten, auf denen nur noch »warten, wenn ein Glibber vor Pip ist, sonst gehen« hilft.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Der Plan läuft in einem eigenen kleinen Container ohne Netz; jeder Befehl geht an die Welt auf dem Server, die alles entscheidet. Wo mehrere Karten geprüft werden, läuft der Plan auf jeder von vorn, und eine feste Folge von Schritten scheitert an mindestens einer. Die Rückmeldung nennt die Karte und die Zeile (»In Karte 2 in Zeile 6 läuft Pip gegen die Wand.«) und spielt diesen Lauf ab.</td></tr>
<tr><th>Gespeichert</th><td>Der Plan jeder Kata, für jede Sprache getrennt, sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist, und eingereichte Lösungen ohne Namen.</td></tr>
</table>

#### Was schiefgeht

- Links und rechts: Pip dreht sich von sich aus gesehen. Schaut Pip auf der Karte nach unten, ist links für Pip rechts auf dem Bildschirm. Die Vorhersage am Anfang greift das auf; ein Finger auf der Karte, der sich mitdreht, hilft.
- `take` neben dem Stein statt darauf: Pip greift ins Leere, die Rückmeldung nennt die Zeile.
- In Python und JavaScript fehlen die Klammern (`move` statt `move()`): Dann passiert nichts, und am Ende steht Pip noch am Anfang.
- Bei der Wendeltreppe zählt die innere Schleife nicht mit der äußeren mit, sondern immer gleich weit.
- Bei den Glibbern ein Plan, der auf einer Karte passt, weil er abgezählt ist. Die Rückmeldung nennt die Karte, auf der er scheitert.

#### Die Aufgaben

- Der Schacht: Vorhersage `predict`, `shaft`, `first-stone`.
- Funkensteine: `few-stones`, `tunnel`; Extra `broken-plan`.
- Die Wendeltreppe: `room`, `corridor`, `spiral`; Extra `spiral-short`.
- Die Pilzwiese: `meadow`, `meadow-stones`.
- Glibber!: `glibber`, `glibbers`.
