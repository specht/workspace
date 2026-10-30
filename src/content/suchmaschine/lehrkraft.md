Lernende bauen eine Suchmaschine für eine kleine Website, das »Kleine Tierlexikon« mit elf Seiten: einen Crawler als Breitensuche über die Links, einen invertierten Index und zwei Rankings, erst nach der Anzahl der Wörter, dann nach den eingehenden Links. Eine Anzeige, die »Katze« achtzehnmal wiederholt, zeigt, warum das eine Ranking versagt, und führt zu PageRank und zur Frage, wer bestimmt, was gefunden wird. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen, Listen und Dictionaries, etwa nach den <a href='/kata-lists'>Katas: Listen</a> und <a href='/kata-dicts'>Katas: Dictionaries</a>. HTTP und Breitensuche erklärt die Seite, so weit sie gebraucht werden.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Einstellungen« ein. Das Tierlexikon liefert in beiden Fällen Pythons eingebauter Webserver aus.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die drei Teile einer Suchmaschine, Einrichten, <code>fetch_page</code> und <code>links</code>, die Breitensuche mit Queue, <code>crawl</code>, der Lauf über das Tierlexikon, der invertierte Index mit <code>build_index</code>.</li>
<li><b>2. Doppelstunde:</b> <code>search</code> als Schnittmenge, das Ranking nach der Anzahl der Wörter und die Anzeige ganz oben, <code>incoming</code> und das Ranking nach Links, PageRank, wer entscheidet, was gefunden wird, das Recht auf Vergessenwerden.</li>
<li><b>Extra »Ins echte Web«</b> für Schnelle: ein höflicher Crawler in <code>webcrawler.rb</code> oder <code>webcrawler.py</code>, der ganze Adressen auflöst, höchstens 30 Seiten holt, vor jeder Anfrage eine Sekunde wartet, <code>robots.txt</code> beachtet und einen User-Agent schickt. Erst im Tierlexikon, dann, wenn die Betreuung der Website einverstanden ist, einmal gemeinsam am Beamer auf der Website der Schule.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/suchmaschine</code>: der Ordner <code>tierlexikon</code> (mit <code>wget</code> von dieser Seite), <code>search.rb</code> oder <code>search.py</code> mit allen Funktionen, <code>main.rb</code> oder <code>main.py</code> mit dem Programm, das sucht. Keine Vorlage: Die Lernenden legen alles selbst an.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Vier Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann, und vergleichen die Rückgabe mit kleinen, ausgedachten Websites und Indizes; die Aufgabe sagt jeweils, womit. <code>crawl</code> bekommt dafür statt <code>fetch_page</code> eine Funktion, die die ausgedachte Website liefert, und muss jede Seite genau einmal holen.</li>
<li>Dazu drei Zahlen aus dem Tierlexikon, zwei Fragen, zwei Vorhersagen und ein Text über das Ranking.</li>
<li>Im Extra »Ins echte Web« zwei weitere Funktionen, <code>absolute_links</code> und <code>allowed?</code> (Python: <code>allowed</code>), mit ausgedachten Seiten und Regeln geprüft, nie mit einer echten Website, und die Zahl der Seiten, die der höfliche Crawler im Tierlexikon holt. Alle drei sind Extras und zählen nicht mit.</li>
</ul></td></tr>
</table>

#### Die Zahlen

- Der Crawler findet von <code>/index.html</code> aus 10 Seiten; <code>geheim.html</code> verlinkt keine.
- Der höfliche Crawler des Extras holt mit der <code>robots.txt</code> der Seite (<code>Disallow: /impressum.html</code>) 9 Seiten, in etwa 9 Sekunden; den Link des Pinguins zur Wikipedia lässt er aus.
- Der Index hat 187 verschiedene Wörter.
- »nachts« und »insekten« stehen zusammen auf zwei Seiten, Igel und Zwergfledermaus; eins der beiden auf vier.
- »katze« steht auf drei Seiten: 18-mal in der Anzeige, 4-mal bei der Hauskatze, einmal beim Hund. »Hauskatze« ist für <code>words</code> ein anderes Wort.
- Eingehende Links: Startseite 8, Hauskatze, Waldkauz, Amsel und Zwergfledermaus je 3, Hund und Igel je 2, Anzeige, Pinguin und Impressum je 1.

#### Die Aufgaben

- Die Seite gibt <code>fetch_page</code>, <code>links</code>, <code>words</code> und die beiden Funktionen zum Sortieren vor; reguläre Ausdrücke und Sortieren nach einem Schlüssel sind nicht das Thema. Selbst geschrieben werden <code>crawl</code>, <code>build_index</code>, <code>search</code> und <code>incoming</code>, jede mit Tipps in Stufen.
- Dass <code>crawl</code> die Funktion zum Holen als Parameter bekommt, ist für viele neu. Es lohnt sich, an der Tafel zu zeigen, dass die Prüfung so eine ausgedachte Website einsetzen kann, ohne dass ein Server läuft.
- Die Frage zum Recht auf Vergessenwerden fragt, wer zuerst entscheidet, nicht, ob es richtig ist. Der Text über das Ranking am Ende eignet sich für ein Gespräch: Jede Regel bevorzugt jemanden, und jede lässt sich ausnutzen.

#### Was schiefgeht

- **Das Extra auf einer echten Website:** Bitte vorher die Betreuung der Website fragen; jede Anfrage steht dort im Protokoll, mit dem User-Agent »Hackschule-Crawler (Informatikkurs, Hackschule Workspace)«. Manche Server lehnen unbekannte Crawler ab; der Crawler überspringt solche Seiten. Menüs, die erst ein Skript im Browser baut, findet er nicht, und die Ruby-Fassung überspringt Links, die <code>URI.join</code> nicht lesen kann, etwa mit Umlauten oder Leerzeichen.

- **<code>Connection refused</code>:** Der Webserver im zweiten Terminal läuft nicht, oder er läuft auf einem anderen Port als 8000.
- **Der Crawler hört nicht auf:** Er merkt sich nicht, welche Pfade er schon in die Queue gestellt hat, und läuft zwischen Katze und Hund im Kreis. <kbd>Strg</kbd><kbd>C</kbd> beendet ihn.
- **Ruby: <code>wrong number of arguments (given 0, expected 1)</code> in <code>main.rb</code>:** Es gibt <code>fetch_page</code> statt <code>method(:fetch_page)</code> weiter; dann ruft Ruby <code>fetch_page</code> sofort auf, ohne Pfad.
- **Python: <code>input</code> wartet nach dem letzten Treffer weiter:** Eine leere Zeile beendet das Programm.

#### Recht und Quellen

- EuGH, <a href='https://curia.europa.eu/juris/liste.jsf?num=C-131/12'>Google Spain</a> (C-131/12, 13. Mai 2014): Eine Suchmaschine muss auf Antrag Links zu einem Namen entfernen, wenn das Recht der Person überwiegt; seit 2018 Art. 17 DSGVO.
- Sergey Brin, Lawrence Page: <a href='https://doi.org/10.1016/S0169-7552(98)00110-X'>The Anatomy of a Large-Scale Hypertextual Web Search Engine</a>, Computer Networks and ISDN Systems 30 (1998).
- <a href='https://www.rfc-editor.org/rfc/rfc9309'>RFC 9309</a>: das Robots Exclusion Protocol, <code>robots.txt</code>.

#### Eigene Projekte

Nach der Seite haben die Lernenden einen Crawler, einen Index und zwei Rankings. Die Ideen verbessern eins davon; bewertet wird mit Suchanfragen, die die Lernenden vorher aufschreiben, mit dem Ergebnis, das sie erwarten, und daran, ob die Änderung es besser macht.

- **Ein besseres Ranking** (eine Doppelstunde): Wörter, die auf fast jeder Seite stehen, zählen weniger als seltene (TF-IDF), oder ein Wort in der Überschrift zählt mehr als eins im Text. Verglichen wird mit dem Zählen aus [»Suchen«](#suchen) und den Links aus [»Links als Stimmen«](#links-als-stimmen), an denselben Anfragen.
- **Ein Ausschnitt zum Treffer** (eine Doppelstunde): Unter jedem Treffer steht der Satz, in dem das Wort vorkommt, mit dem Wort hervorgehoben, wie bei großen Suchmaschinen. Dafür muss der Index mehr wissen als nur, auf welcher Seite ein Wort steht.
- **Eine Suche, die Tippfehler verzeiht** (ein Projekt): »Elefnat« findet den Elefanten. Die Lernenden messen, wie ähnlich zwei Wörter sind, etwa mit der Zahl der Buchstaben, die man ändern muss (Levenshtein-Abstand), und schlagen das nächste Wort aus dem Index vor.
