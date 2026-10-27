Turingmaschinen zum Programmieren, in Ruby oder Python: Lernende lassen eine Maschine 1 zu einer Binärzahl zählen, sagen ihr Ergebnis voraus, bauen daraus eine, die 1 abzieht, und eine, die Strichlisten addiert, zählen die Schritte einer Maschine für a<sup>n</sup>b<sup>n</sup>c<sup>n</sup> und denken das Halteproblem als Gedankenexperiment durch.

<table class='table'>
<tr><th>Voraussetzungen</th><td>»Endliche Automaten«: Automaten als Tabelle in Ruby oder Python. »Kellerautomaten« hilft für die Frage, warum a<sup>n</sup>b<sup>n</sup>c<sup>n</sup> mehr braucht, ist aber keine Bedingung.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Eine Aufgabe, die Lernende in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Band, Kopf und Zustand, die Maschine für +1 mit der Vorhersage, minus 1 und die Addition von Strichlisten.</li>
<li><b>2. Doppelstunde:</b> a<sup>n</sup>b<sup>n</sup>c<sup>n</sup> mit Schritten zählen, universelle Maschine und Church-Turing-These, Maschinen, die nicht anhalten, das Halteproblem.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/turingmaschine</code>: <code>plus_one</code>, <code>minus_one</code>, <code>add</code> und <code>anbncn</code>, mit <code>.rb</code> in Ruby und <code>.py</code> in Python. <code>plus_one</code> und <code>anbncn</code> tippen die Lernenden von der Seite ab; <code>minus_one</code> und <code>add</code> sind Kopien von <code>plus_one</code> mit eigener Tabelle.</td></tr>
<tr><th>Prüfung</th><td>»Eins weniger« und »Strichlisten addieren« laden die Datei der Lernenden in ihrem Workspace und rufen <code>run</code> mit eigenen Eingaben auf, als Gast, der in <code>/workspace</code> nichts verändern kann. Die Rückmeldung nennt den ersten Aufruf, der nicht passt. Der Workspace muss dafür laufen.</td></tr>
</table>

#### Die Aufgaben

- »Was steht danach auf dem Band?« ist eine Vorhersage vor dem ersten Start: `1000`. Die Spur der Maschine steht erst nach der Antwort auf der Seite.
- »Eins weniger« ändert nur die beiden Zeilen von `q1`: Eine 0 wird zu 1 und der Kopf geht weiter, eine 1 wird zu 0 und die Maschine hält. Eine führende 0 darf stehen bleiben (aus `1000` wird `0111`); die Prüfung lässt führende Nullen weg.
- »Strichlisten addieren« ist mit fünf Übergängen gelöst: das `+` wird zu 1, ganz rechts wird eine 1 gelöscht. Die Prüfung ruft auch `+11` und `11+` auf, wo eine der Zahlen 0 ist.
- »Doppelt so lang« ist eine Vorhersage: Für die doppelte Länge braucht die Maschine 77 statt 23 Schritte, gut dreimal so viele; auf lange Sicht wächst die Zahl mit dem Quadrat der Länge. »Schritte zählen« (46 für `aaabbbccc`) lesen die Lernenden aus der Spur ab.
- »Eine Maschine, die nicht aufhört« und »Der Widerspruch« sind Fragen mit Auswahl; das Halteproblem steht als Gedankenexperiment mit einem Programm `paradox`, nicht als Beweis über Turingmaschinen.

#### Was schiefgeht

- **`KeyError`:** Für ein Paar aus Zustand und Zeichen fehlt ein Übergang. Bei `add` ist das oft `+` als erstes Zeichen oder `_` direkt nach dem `+`.
- **»Nach 1000 Schritten hat die Maschine nicht angehalten.«:** Eine Zeile führt in denselben Zustand zurück, ohne dass sich etwas ändert, meist mit der falschen Richtung.
- **Leere Ausgabe statt `0`:** `run` gibt das Band ohne die leeren Felder am Rand zurück; wer bei `minus_one` die 1 zu `_` statt zu `0` macht, bekommt für `run('1')` einen leeren String.

#### Begriffe

Die Seite benutzt deterministische Turingmaschinen mit einem Band, `_` für leere Felder, `L` und `R`. Die Maschine für a<sup>n</sup>b<sup>n</sup>c<sup>n</sup> nimmt in q<sub>J</sub> an und lehnt ab, wenn kein Übergang passt. Universelle Maschine, Church-Turing-These und Halteproblem kommen ohne Beweis vor; Komplexität (P und NP) ist nur Ausblick.

#### Weiter

Turings Aufsatz von 1936 ist auf der Seite verlinkt; seine ersten Abschnitte beschreiben die Maschine in Worten und passen für eine Leseaufgabe im Leistungskurs.
