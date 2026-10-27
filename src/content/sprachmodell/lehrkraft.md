Lernende bauen ein Sprachmodell aus Wortpaaren: Sie zerlegen die Märchen der Brüder Grimm in Wörter, zählen in einem Dictionary von Dictionaries, welches Wort auf welches folgt, und lassen ihr Programm daraus Text schreiben, erst immer mit dem häufigsten Wort, dann gewürfelt nach der Häufigkeit, dann mit zwei Wörtern Kontext. Am Ende sehen sie, wie oft das Modell nur abschreibt und was es aus seinem Text übernimmt. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen, Listen mit Index, Dictionaries: etwa nach den <a href='/kata-dicts'>Katas: Dictionaries</a>. Zufallszahlen erklärt die Seite.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Einstellungen« ein.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das nächste Wort, Einrichten, aus Text werden Wörter, <code>follow_counts</code>, in die Märchen schauen, <code>most_likely</code> und das Programm, das in einer Schleife hängen bleibt.</li>
<li><b>2. Doppelstunde:</b> würfeln mit <code>next_word</code> und <code>generate</code>, Shannon und Markow, zwei Wörter Kontext mit <code>pair_counts</code>, wie viel abgeschrieben ist, was das Modell aus seinem Text lernt, der Vergleich mit großen Sprachmodellen. Für Schnelle: ein eigener Text.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/sprachmodell</code>: <code>maerchen.txt</code> (mit <code>wget</code> von dieser Seite), <code>model.rb</code> oder <code>model.py</code> mit allen Funktionen, <code>story.rb</code> oder <code>story.py</code> mit dem Programm, das schreibt. Keine Vorlage: Die Lernenden legen alles selbst an.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Fünf Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann, und vergleichen die Rückgabe mit kleinen, ausgedachten Dictionaries; die Aufgabe sagt jeweils, womit. <code>next_word</code> wird 4000-mal aufgerufen und muss ungefähr im richtigen Verhältnis ziehen.</li>
<li>Dazu zwei Zahlen aus den Märchen, zwei Fragen und vier Vorhersagen. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Zahlen

- <code>words</code> findet 93 581 Wörter und Satzzeichen (79 979 Wörter, 13 602 Satzzeichen); <code>wc -w</code> zählt 80 122, weil es an Leerzeichen trennt.
- Auf »Es« folgt 41-mal »war«, 21-mal »ist«, 13-mal »trug«; auf »sprach« 207-mal der Doppelpunkt, 149-mal »der«.
- Immer mit dem häufigsten Wort schreibt das Programm »Es war, und sprach: Ich bin ich will ich will …«: Auf »ich« folgt am häufigsten »will« und umgekehrt.
- Von 7 890 verschiedenen Wörtern haben 55 Prozent nur einen Nachfolger, von 41 816 Paaren 78 Prozent (32 477).
- Auf »böse« folgen »Weib« und »Hexe« je dreimal, »Stiefmutter« und »Buben« je zweimal; von 14 Malen ist es achtmal eine Frau.

#### Die Aufgaben

- Die Seite gibt <code>words</code>, <code>top</code>, <code>text</code> und <code>generate_pairs</code> vor; ein regulärer Ausdruck ist nicht das Thema. Selbst geschrieben werden <code>follow_counts</code>, <code>most_likely</code>, <code>next_word</code>, <code>generate</code> und <code>pair_counts</code>, jede mit Tipps in Stufen.
- Das Würfeln nach der Häufigkeit ist die Stelle, an der viele hängen. Die Tabelle auf der Seite (Zufallszahlen 0 bis 9 auf drei Nachfolger verteilt) lohnt sich an der Tafel, bevor jemand Code schreibt. In Python geht es auch mit <code>random.choices</code>; die Seite nennt das nach der Lösung.
- Zum Teil über das, was das Modell aus seinem Text lernt, stehen zwei Fragen zum Nachdenken auf der Seite, ungeprüft: woher die Texte großer Modelle kommen und wem sie gehören, und woran man erkennt, ob ein flüssiger Satz stimmt. Sie eignen sich für ein Gespräch in der Klasse; eine richtige Antwort gibt die Seite nicht vor.
- Die Märchen sind 200 Jahre alt, und ihre Rollen auch: Böse sind vor allem Hexen und Stiefmütter. Die Aufgabe dazu fragt, was das Modell daraus macht, nicht, was die Lernenden davon halten.

#### Was schiefgeht

- **<code>irb</code> oder Python kennen eine geänderte Funktion nicht:** Sie haben <code>model.rb</code> oder <code>model.py</code> beim Start geladen; nach einer Änderung neu starten.
- **Python: <code>from model import *</code> findet nichts:** <code>story.py</code> liegt nicht im selben Ordner wie <code>model.py</code>, oder Python wurde aus einem anderen Ordner gestartet.
- **Umlaute fehlen in den Wörtern:** Die Datei wurde nicht als UTF-8 gelesen; in Python hilft <code>open("maerchen.txt", encoding="utf-8")</code>, in Ruby <code>File.read("maerchen.txt", encoding: "UTF-8")</code>.

#### Woher der Text kommt

<code>maerchen.txt</code> baut <code>src/scripts/collect-maerchen/collect-maerchen.rb</code> aus dem E-Book <a href='https://www.gutenberg.org/ebooks/77905'>Nr. 77905 von Project Gutenberg</a>, »Deutsche Märchen gesammelt durch die Brüder Grimm« (Langewiesche-Brandt 1921, gemeinfrei): nur der Text der Märchen, ein Absatz je Zeile, ohne Titel, Anmerkungen und die Lizenz von Project Gutenberg, ohne die vier Märchen auf Plattdeutsch.
