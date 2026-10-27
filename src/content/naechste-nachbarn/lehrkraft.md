Lernende schreiben ein Programm, das nach den k nächsten Nachbarn entscheidet, ohne Bibliotheken: erst für die Palmer-Pinguine mit allen drei Arten, dann für einen ausgedachten Fahrradverleih, der aus alten Entscheidungen lernen soll, wer eine Kaution zahlt. Dabei zählen sie selbst nach, dass das Programm die Ungerechtigkeit der alten Entscheidungen übernimmt und sogar verschärft, dass das Weglassen des Wohnorts sie verteilt statt beseitigt und dass ein harmloses Merkmal (die Entfernung zum Laden) für den Wohnort stehen kann. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen, Listen mit Index: etwa nach den <a href='/kata-lists'>Katas: Listen</a>. Den Abstand erklärt die Seite mit dem Satz des Pythagoras. »Ein Neuron lernt« vorher ist hilfreich (dieselben Pinguine), aber nicht nötig.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Einstellungen« ein.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Pinguine und die Idee der Nachbarn, Fix und Hodges, Einrichten, <code>distance</code>, <code>nearest</code>, <code>classify</code>, ein unbekannter Pinguin, <code>accuracy</code> an den Pinguinen, die das Programm nicht kennt, und was ein zu großes k anrichtet.</li>
<li><b>2. Doppelstunde:</b> der Fahrradverleih, wie oft das Programm wie der Laden entscheidet, <code>deposit_share</code> und die Pünktlichen je Stadtteil, ohne Wohnort, mit der Entfernung, was der Laden tun könnte, Art. 22 DSGVO, Joy Buolamwini und »Gender Shades«, eine Reflexion zum Schluss.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/nachbarn</code>: <code>penguins.csv</code> und <code>customers.csv</code> (mit <code>wget</code> von dieser Seite), <code>knn.rb</code> oder <code>knn.py</code> mit den Funktionen, <code>penguins.rb</code> und <code>deposit.rb</code> (oder <code>.py</code>) mit den Programmen. Keine Vorlage.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Fünf Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann: <code>distance</code> mit zwei und drei Merkmalen, <code>nearest</code>, <code>classify</code> mit einem Gleichstand, <code>accuracy</code> und <code>deposit_share</code> an kleinen Beispielen.</li>
<li>Dazu fünf Zahlen, drei Vorhersagen, eine Auswahl und eine Reflexion (mindestens 30 Wörter), deren Antworten du unter der Aufgabe siehst. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Zahlen

- Beispiele sind immer die Zeilen mit geradem Index (die erste, dritte, …), geprüft wird an denen mit ungeradem, in der Reihenfolge der Datei.
- Pinguine (171 zum Vergleichen, 171 zum Prüfen, zwei Maße des Schnabels): mit k = 1 und k = 5 96,5 Prozent richtig, mit k = 3 97,1 Prozent (166 von 171), mit k = 51 80,7 Prozent, mit k = 171 43,9 Prozent (immer »Adelie«). Der Pinguin mit 46 mm und 18 mm hat fünf Zügelpinguine als nächste Nachbarn.
- Fahrradverleih, k = 5, je 300 Kundinnen und Kunden: Mit <code>years</code>, <code>late</code>, <code>x</code> und <code>y</code> entscheidet das Programm in 81,3 Prozent der Fälle wie der Laden. Kaution für Pünktliche (<code>on_time</code> 1):

<div class='scroll-x'>
<table class='table'>
<tr><th>Stadtteil</th><th>Pünktliche</th><th>der Laden</th><th>mit Wohnort</th><th>ohne Wohnort</th><th>mit Entfernung</th></tr>
<tr><td>Altstadt</td><td>43</td><td>18,6 %</td><td>14,0 %</td><td>18,6 %</td><td>14,0 %</td></tr>
<tr><td>Lindenhof</td><td>63</td><td>28,6 %</td><td>15,9 %</td><td>27,0 %</td><td>34,9 %</td></tr>
<tr><td>Bahnhofsviertel</td><td>69</td><td>24,6 %</td><td>18,8 %</td><td>27,5 %</td><td>33,3 %</td></tr>
<tr><td>Sandberg</td><td>85</td><td>70,6 %</td><td>84,7 %</td><td>28,2 %</td><td>56,5 %</td></tr>
</table>
</div>

- Die Daten sind ausgedacht (<code>src/scripts/collect-naechste-nachbarn</code>, fester Seed). Zuverlässig sind die Menschen in allen Stadtteilen gleich; der Laden hat Neuen (unter einem halben Jahr) und Leuten mit zwei Verspätungen oder mehr eine Kaution abverlangt, am Sandberg zusätzlich sechs von zehn der übrigen. Die Seite sagt das nach der Aufgabe »Die Pünktlichen am Sandberg«, nicht vorher.

#### Die Aufgaben

- Selbst geschrieben werden <code>distance</code>, <code>nearest</code>, <code>classify</code>, <code>accuracy</code> und <code>deposit_share</code>; das Einlesen (<code>penguins</code>, <code>customers</code>, <code>examples</code>) gibt die Seite vor.
- Der Gleichstand in <code>classify</code> (der nähere Nachbar gewinnt) ergibt sich in Ruby mit <code>max_by</code> und in Python mit <code>max(…, key=…)</code> von selbst, weil beide bei Gleichstand das erste Element nehmen und die Nachbarn nach dem Abstand sortiert sind. Wer mit <code>tally</code> oder <code>Counter</code> zählt, bekommt dasselbe.
- Der Moment der zweiten Stunde ist die Tabelle nach »Die Pünktlichen am Sandberg«: Das Programm ist strenger als der Laden, weil eine Abstimmung aus »meistens« »immer« macht. Danach lohnt sich ein Gespräch, bevor die Klasse den Wohnort weglässt.
- Die Kästen »Nachdenken« und die Reflexion zum Schluss eignen sich für ein Unterrichtsgespräch: Wer ist verantwortlich? Was müsste der Laden einer Kundin sagen können? Ab welcher Höhe ist eine Kaution »erheblich« im Sinn von Art. 22 DSGVO?

#### Was schiefgeht

- **<code>irb</code> oder Python kennen eine geänderte Funktion nicht:** Sie haben die Datei beim Start geladen; nach einer Änderung neu starten.
- **Ruby: <code>accuracy</code> gibt 0:** <code>2 / 4</code> ist in Ruby die ganze Zahl 0; <code>to_f</code> vor dem Teilen.
- **Andere Prozentzahlen:** Meist sind die Rollen von Beispielen und Test vertauscht, k ist nicht 5, oder <code>columns</code> enthält andere Spalten.
- **Python: <code>open</code> meldet einen Fehler mit der Kodierung:** Die Dateien enthalten nur ASCII; das passiert nur, wenn eine andere Datei geöffnet wurde.

#### Woher die Daten kommen

- Pinguine: K. B. Gorman, T. D. Williams, W. R. Fraser, <a href='https://doi.org/10.1371/journal.pone.0090081'>PLOS ONE 2014</a>, über das Paket <a href='https://allisonhorst.github.io/palmerpenguins/'>palmerpenguins</a> unter CC0; dieselbe Datei wie in »Ein Neuron lernt«.
- Fahrradverleih: ausgedacht von <code>src/scripts/collect-naechste-nachbarn/collect-naechste-nachbarn.rb</code>, das beide Dateien neu baut.
