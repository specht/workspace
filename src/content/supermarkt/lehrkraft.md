Objektorientierter Entwurf an einer Simulation: Lernende finden in einer kurzen Geschichte die Klassen, zeichnen sie als UML-Klassendiagramm in der UML-Werkstatt und programmieren sie dann, mit Kapselung, Zustand und Verhalten, einer Assoziation mit Multiplizitäten und Vererbung mit Polymorphie. Am Ende spielt das Programm einen Samstagvormittag an den Kassen nach und beantwortet die Frage der Geschichte: Die Expresskasse lohnt sich nicht. Ruby, Python, JavaScript, Dart oder Java; das Diagramm schreibt die Namen wie die Sprache.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in einer der fünf Sprachen: Funktionen oder Methoden, Verzweigungen, Listen, eine Schleife. Was eine Klasse ist, erklärt die Seite.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. JavaScript, Dart und Java wählen die Lernenden auf der Seite selbst; die Wahl gilt nur für Seiten, die sie anbieten, und ändert die Einstellung der Klasse nicht. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Geschichte, Klasse oder Attribut, die erste Klasse im Diagramm und im Code (Kapselung, Konstruktor, Getter), Klassendiagramm und ER-Modell.</li>
<li><b>2. Doppelstunde:</b> Zustand und Verhalten an der Kasse, die Assoziation zwischen Supermarkt und Kassen.</li>
<li><b>3. Doppelstunde:</b> Vererbung und Polymorphie mit Expresskasse und Selbstbedienungskasse, die Simulation, was ein Modell weglässt.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/supermarkt</code>: das Diagramm <code>supermarkt.uml</code>, die Klassen in <code>supermarket.rb</code>, <code>.py</code>, <code>.js</code> oder <code>.dart</code>, in Java eine Datei je Klasse (<code>Customer.java</code>, <code>Checkout.java</code>, <code>ExpressCheckout.java</code>, <code>SelfCheckout.java</code>, <code>Supermarket.java</code>), dazu die Simulation in einer eigenen Datei und <code>customers.csv</code>. Keine Vorlage: Die Lernenden legen den Ordner an und schreiben alles selbst; nur die Simulation steht fertig auf der Seite.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Drei Aufgaben lesen das Diagramm mit der UML-Werkstatt und vergleichen Klassen, Attribute, Methoden, Vererbung, überschriebene Methoden und Multiplizitäten mit dem Entwurf. <code>isAvailable</code>, <code>is_available</code> und <code>available?</code> gelten dabei als derselbe Name.</li>
<li>Vier Aufgaben rufen den Code der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann: Sie legen Objekte an, rufen Methoden der Reihe nach auf und vergleichen, was herauskommt. Java und Dart werden dafür übersetzt.</li>
<li>Drei Zahlen aus der Simulation, drei Vorhersagen und eine Frage zur Geschichte. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Zahlen

Mit der Datei <code>customers.csv</code> (171 Kundinnen und Kunden von 9 bis 11 Uhr, ausgedacht, mit festem Zufallswert erzeugt) kommt in allen fünf Sprachen dasselbe heraus:

<table class='table'>
<tr><th>Kassen</th><th>im Mittel</th><th>bis 10 Artikel</th><th>mehr als 10</th></tr>
<tr><td>drei normale</td><td>69,75 s</td><td>57,7 s</td><td>81,9 s</td></tr>
<tr><td>zwei normale, eine Expresskasse</td><td>118,29 s</td><td>40,2 s</td><td>197,3 s</td></tr>
<tr><td>zwei normale, zwei Selbstbedienungskassen</td><td>12,44 s</td><td>8,8 s</td><td>16,1 s</td></tr>
</table>

Weicht eine Zahl ab, liegt es fast immer an der Wahl der Kasse: Die Kundin geht an die, die am frühesten frei ist, bei Gleichstand an die erste in der Liste. Die Prüfung »Der Supermarkt« fragt beides ab.

#### Die Aufgaben

- »Klasse oder Attribut?« trennt Gegenstände mit eigenem Verhalten (Kasse, Kundin, Expresskasse) von Werten (die Zahl der Artikel). Die Kühlregale sind bewusst in der Geschichte: Nicht jedes Nomen kommt ins Programm.
- »Klassendiagramm oder ER-Modell?« greift die [Filmdatenbank](/filmdatenbank) auf: Ein ER-Modell beschreibt, was gespeichert wird, ein Klassendiagramm Objekte eines laufenden Programms. Eine Simulation braucht keine Datenbank; ein echtes Kassensystem hat beides.
- Die Kasse merkt sich nur, ab wann sie frei ist, keine Schlange. Für die Wartezeit genügt das: Wer sich anstellt, kommt dran, wenn alle vor ihr fertig sind. Wer eine echte Queue bauen will, findet die Idee unter »Wie geht es weiter?«.
- Die häufigsten Fehler, die die Prüfungen eigens abfangen: <code>serve</code> fängt bei <code>free_at</code> an, auch wenn die Kundin noch nicht da ist (die Wartezeit wird negativ); <code>&lt;=</code> statt <code>&lt;</code> nimmt bei Gleichstand die letzte Kasse; die mittlere Wartezeit als ganze Zahl; eine Expresskasse, die nichts überschreibt; ein Supermarkt, der nicht fragt, ob eine Kasse die Kundin nimmt.
- »Drei Kassen, ein Aufruf« ist die Vorhersage zur Polymorphie. In Dart und Java ist die Liste eine Liste von <code>Checkout</code>, und doch läuft die Methode der Selbstbedienungskasse: ein guter Moment, über statische und dynamische Typen zu sprechen.
- Die Expresskasse verschlechtert den Mittelwert, weil zwei Kassen für alle und eine für manche weniger bedienen als drei für alle. Wer wenig kauft, gewinnt trotzdem; die Tabelle oben eignet sich für die Frage, was »lohnt sich« heißen soll.

#### Was schiefgeht

- **Die UML-Werkstatt zeigt nichts:** Die Datei muss auf <code>.uml</code> enden; das Diagramm öffnet das Symbol oben rechts im Editor.
- **Die Simulation findet <code>customers.csv</code> nicht:** Sie liest die Datei im Ordner, in dem sie gestartet wird; im Terminal erst <code>cd ~/supermarkt</code>.
- **JavaScript: <code>TypeError: Checkout is not a constructor</code>** in der Simulation: In <code>supermarket.js</code> fehlt die Zeile <code>module.exports = { … }</code>.
- **Java: <code>class Checkout is public, should be declared in a file named Checkout.java</code>:** Jede öffentliche Klasse in ihrer eigenen Datei, die heißt wie sie.
- **Ausgaben aus dem Ausprobieren erscheinen in der Simulation:** Die Zeilen zum Ausprobieren unten in der Datei der Klassen laufen bei jedem <code>require</code> oder <code>import</code> mit; die Seite sagt, sie zu löschen.

#### Eigene Projekte

Die Simulation beantwortet eine Frage; die Ideen hier lassen die Lernenden eigene stellen. Jede beginnt wieder im Diagramm, und bewertet wird, was sie vorher vermutet haben, was die Simulation sagt und ob sie erklären können, warum.

- **Eine eigene Frage an den Samstag** (eine Doppelstunde): die Wartezeit getrennt nach Wagen, die Expresskasse bis fünf oder bis fünfzehn Artikel, die längste statt der mittleren Wartezeit. Erst die Vermutung aufschreiben, dann rechnen lassen; abgegeben wird eine kleine Tabelle mit ein paar Sätzen dazu.
- **Was das Modell weglässt** (eine Doppelstunde): eins von dem, was [»Was die Simulation nicht weiß«](#was-die-simulation-nicht-weiß) aufzählt, ins Modell holen: Kundinnen, die nur die Schlange sehen und die kürzeste wählen (eine [Queue](/reference/listen#queue) je Kasse), Hilfe an der Selbstbedienungskasse, die ab und zu nötig ist. Die Frage dazu: Ändert sich die Antwort der Geschichte?
- **Eine andere Schlange** (ein Projekt): eine Warteschlange aus dem eigenen Alltag, die Mensa, der Skilift, die Ausleihe in der Bibliothek, der Bürgerservice. Von der eigenen kurzen Geschichte über das Klassendiagramm zur Simulation, mit einer Frage, die jemand dort wirklich hat; die Vererbung aus [»Vererbung«](#vererbung) kommt wieder, wo es verschiedene Schalter gibt. Bewertet werden Diagramm, Code und die Antwort im Vergleich zur Vermutung.
- **Derselbe Entwurf in einer zweiten Sprache** (eine Doppelstunde): das Diagramm bleibt, der Code entsteht noch einmal, etwa in Java nach Ruby. Wer beides nebeneinanderlegt, sieht, was am Entwurf liegt und was an der Sprache (Getter, Typen, eine Datei je Klasse); das erklären die Lernenden zum Schluss an ihrem eigenen Code.
