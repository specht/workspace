Lernende steuern Pip mit Plänen, die selbst nachsehen, wo es weitergeht: Schleifen mit `while`, eigene Funktionen mit Parameter, eine Variable, die mitzählt, verknüpfte Bedingungen und die Hand an der Wand als erster Algorithmus, in Ruby, Python oder JavaScript.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/pip-1'>Pip unter Funkelbach: Hinab in die Stollen</a> und Schleifen mit Zählvariable, etwa aus den <a href='/kata-loops'>Katas: Wiederholen</a>.</td></tr>
<tr><th>Ablauf</th><td>Etwa drei Doppelstunden, sechs Kapitel.<ul>
<li>Im Finstern und Eigene Zaubersprüche: <code>while</code> mit <code>front_clear</code>, <code>not</code> und <code>on_lamp</code>; dann Funktionen: <code>turn_right</code>, <code>turn_around</code>, <code>walk(n)</code> und eine Treppe aus ihnen.</li>
<li>Das Labyrinth und Drei Hebel: die Hand an der Wand, auf Labyrinthen, die für jede Person anders gebaut werden; eine Variable, die die Züge an den Hebeln zählt. Extras: ein Labyrinth mit Insel, Steine zählen und die Zahl sagen.</li>
<li>Bellos Spur und Zurück ans Licht: <code>and</code>, <code>or</code>, <code>not</code> mit Pfützen und Pfotenabdrücken; zum Schluss ein Stollen, in dem alles zusammenkommt.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Jeder Plan läuft auf jeder Karte von vorn, in einem eigenen Container ohne Netz; die Welt auf dem Server entscheidet jeden Schritt. Bei den Zaubersprüchen ruft die Prüfung die Funktion selbst auf (»Nach <code>turn_right</code> schaut Pip nach oben statt nach unten.«). Zwei der drei Labyrinthe entstehen aus einem Seed je Person und Aufgabe. Die Treppe hat höchstens 18 Zeilen, so dass sie mit den Funktionen geschrieben sein muss.</td></tr>
<tr><th>Gespeichert</th><td>Der Plan jeder Kata, für jede Sprache getrennt, sichtbar nur für die Person selbst; Lehrkräfte sehen im Klassenboard nur, ob sie gelöst ist, und eingereichte Lösungen ohne Namen.</td></tr>
</table>

#### Woher die Idee kommt

Ein Roboter auf einem Gitter, der sich nur nach links drehen kann und `turn_right` von den Lernenden beigebracht bekommt: Das ist Karel the Robot von Richard E. Pattis, der damit seit 1981 in das Programmieren einführt (*Karel the Robot: A Gentle Introduction to the Art of Programming*). Pip übernimmt die Idee mit den Befehlen der heutigen Sprachen.

#### Was schiefgeht

- `while` ohne Schritt in der Schleife: Die Bedingung ändert sich nie, nach 1000 Schritten oder zehn Sekunden hält die Prüfung an und fragt nach einer Endlosschleife.
- Ein Zauberspruch, der definiert, aber nie aufgerufen wird, oder ein Aufruf über der Definition (in Python und Ruby kennt das Programm den Namen dann noch nicht).
- An der Wand nur nach links drehen: In einem Stollen, der nach rechts abbiegt, läuft Pip zurück.
- Bei den Hebeln an jedem Hebel ziehen: Mit vier Hebeln fahren die Stacheln danach wieder heraus.
- Die Insel (Extra): Mit der linken Hand bleibt Pip an der Außenwand und läuft im Kreis. Wer am Anfang die Wand auf der anderen Seite nimmt, kommt hin. Gut zum Besprechen: Die Hand an der Wand findet nur, was an derselben Wand liegt.

#### Die Aufgaben

- Im Finstern: `dark-tunnel`, `dark-stones`, `winding`.
- Eigene Zaubersprüche: `turn-right`, `turn-around`, `walk`, `stairs`.
- Das Labyrinth: `hand-on-wall`, `maze`; Extra `island`.
- Drei Hebel: `one-lever`, `levers`; Extra `count-stones`.
- Bellos Spur: `dry-feet`, `trail`.
- Zurück ans Licht: `way-out`.
