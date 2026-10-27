Lernende bauen ein Perzeptron, ein einzelnes künstliches Neuron, ohne Bibliotheken: Es unterscheidet Adélie- von Eselspinguinen an zwei Maßen des Schnabels und findet die Regel selbst, aus echten Messungen. Dann sehen sie, wo ein Neuron nicht weiterkommt (Zügelpinguine, XOR), und lassen dasselbe Neuron handgeschriebene Ziffern lesen. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen, Listen mit Index: etwa nach den <a href='/kata-lists'>Katas: Listen</a>. Gewichtete Summen erklärt die Seite.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Einstellungen« ein.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Pinguine, ein Neuron von Hand, Rosenblatt, Einrichten, <code>predict</code>, die Lernregel und <code>train</code>, zwölf Epochen bis fehlerfrei.</li>
<li><b>2. Doppelstunde:</b> was das Neuron gelernt hat (die Gewichte als Regel, die Gerade in <code>plot.svg</code>), Pinguine, die es nie gesehen hat, Zügelpinguine und XOR, Ziffern lesen und die Gewichte als Bild.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/perzeptron</code>: <code>penguins.csv</code>, <code>digits-train.csv</code> und <code>digits-test.csv</code> (mit <code>wget</code> von dieser Seite), <code>neuron.rb</code> oder <code>neuron.py</code> mit den Funktionen, <code>learn.rb</code> oder <code>learn.py</code> mit dem Programm, dazu die Bilder <code>plot.svg</code> und <code>weights.svg</code>, die VS Code anzeigt. Keine Vorlage.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Drei Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann: <code>predict</code> mit zwei und drei Merkmalen, <code>train</code> mit »und« und »oder«, <code>accuracy</code> mit vier Beispielen.</li>
<li>Dazu drei Zahlen (von Hand gerechnet, die Epochen, die Genauigkeit bei 3 gegen 8), eine Prozentzahl und vier Vorhersagen. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Zahlen

- Gewichte und Bias beginnen bei 0, die Lernrate ist 1, die Pinguine kommen in der Reihenfolge der Datei. Dann ist das Neuron für Adélie gegen Eselspinguin (274 Pinguine) nach zwölf Epochen fehlerfrei, mit den Gewichten 29.1 und −74.0 und dem Bias −1; die Fehler je Epoche: 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0.
- Aus jedem zweiten Pinguin gelernt, ordnet es die anderen 137 alle richtig ein.
- Adélie gegen Zügelpinguin: auch nach 100 Epochen ein oder zwei Fehler je Epoche.
- Ziffern: 0 gegen 1 nach zehn Epochen fehlerfrei, alle 360 Testziffern richtig; 3 gegen 8 nach fünf Epochen fehlerfrei, 347 von 357 Testziffern richtig (97,2 Prozent).
- Wer eine andere Zahl der Epochen bekommt, hat meist die Gewichte nicht bei 0 begonnen, die Beispiele umsortiert oder den Bias nicht mitgelernt.

#### Die Aufgaben

- Die Seite gibt das Einlesen der Daten und die Bilder vor (<code>penguins</code>, <code>digits</code>, <code>plot</code>, <code>picture</code>); selbst geschrieben werden <code>predict</code>, <code>train</code> und <code>accuracy</code>. <code>predict</code> muss mit jeder Zahl von Merkmalen gehen, sonst scheitern später die Ziffern mit 64 Merkmalen; die Prüfung fragt es mit drei.
- Die Lernregel ist die Stelle, an der man an der Tafel einmal von Hand rechnen sollte: ein falsch eingeordneter Pinguin, die Gewichte vorher und nachher.
- Dass die Gewichte eine lesbare Regel ergeben (langer, flacher Schnabel: Eselspinguin), ist der Moment der Stunde. Der Ausprobieren-Kasten mit einer, dann drei Epochen zeigt in <code>plot.svg</code>, wie die Gerade wandert.
- Zügelpinguine und XOR zeigen dieselbe Grenze: Ein Neuron trennt nur, was eine Gerade trennen kann. Die Seite erzählt, wie Minsky und Papert 1969 das bewiesen und wie mehrere Schichten es lösen, ohne dass die Lernenden ein Netz bauen.

#### Was schiefgeht

- **<code>irb</code> oder Python kennen eine geänderte Funktion nicht:** Sie haben die Datei beim Start geladen; nach einer Änderung neu starten.
- **Ruby: <code>accuracy</code> gibt 0:** <code>3 / 4</code> ist in Ruby die ganze Zahl 0; <code>to_f</code> vor dem Teilen.
- **Das Neuron lernt nie aus:** Meist ändert <code>train</code> den Bias nicht, oder es ändert die Gewichte auch bei richtigen Antworten.

#### Woher die Daten kommen

- Pinguine: K. B. Gorman, T. D. Williams, W. R. Fraser, <a href='https://doi.org/10.1371/journal.pone.0090081'>PLOS ONE 2014</a>, über das Paket <a href='https://allisonhorst.github.io/palmerpenguins/'>palmerpenguins</a> unter CC0; <code>penguins.csv</code> lässt die zwei Pinguine ohne Schnabelmaße weg.
- Ziffern: E. Alpaydin, C. Kaynak, <a href='https://archive.ics.uci.edu/dataset/80/optical+recognition+of+handwritten+digits'>Optical Recognition of Handwritten Digits</a>, UCI Machine Learning Repository, 1998, CC BY 4.0; <code>digits-train.csv</code> und <code>digits-test.csv</code> sind <code>optdigits.tra</code> und <code>optdigits.tes</code> unverändert.
- Beide baut <code>src/scripts/collect-perzeptron/collect-perzeptron.rb</code> neu.
