Lernende rechnen die Prüfziffer eines Strichcodes (EAN-13) von Hand aus, lesen Struktogramme und übersetzen sie in kleine Funktionen. Dann planen sie selbst, als Pseudocode und Struktogramm, prüfen erst Strichcodes und dann ISBNs und finden mit einem kleinen Programm heraus, welche Zahlendreher die Prüfziffer übersieht. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen mit Parametern und Rückgabewert, Verzweigungen, eine Zählschleife, Listen mit Index; etwa nach den Katas zu Funktionen, Entscheidungen, Schleifen und Listen. Struktogramme und Pseudocode erklärt die Seite, ausführlicher das Handbuch unter <a href='/reference/struktogramme'>Pseudocode und Struktogramme</a>.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. Pseudocode und Struktogramme sind für beide gleich.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Geschichte, die Prüfziffer von Hand, ein Struktogramm lesen, das Struktogramm der Prüfziffer, <code>weight</code> und <code>check_digit</code>.</li>
<li><b>2. Doppelstunde:</b> erst planen, dann programmieren: der gültige Strichcode, die ISBN mit eigenem Plan, die Zahlendreher.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/check-digits</code>: <code>check_digits.rb</code> oder <code>check_digits.py</code> mit allen Funktionen, dazu <code>swaps.rb</code> oder <code>swaps.py</code> für die Zahlendreher. Keine Vorlage: Die Lernenden legen alles selbst an.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Vier Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann, und vergleichen die Rückgabe; die Aufgabe sagt jeweils, womit.</li>
<li>Der Plan für die ISBN ist eine offene Aufgabe: Er geht ans Klassenboard. Unter der Aufgabe sehen nur Lehrkräfte ein Beispiel für einen guten Plan.</li>
<li>Dazu die Prüfziffer von Hand, das Ergebnis eines Struktogramms, die Zahl der übersehenen Zahlendreher und drei Vorhersagen. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Zahlen

- Die Prüfziffer zu `4 260111 02043` ist 4 (Summe 36); der Code auf den Nudeln ist `4 260111 020434`, ausgedacht.
- `mystery(4021)` gibt 7 zurück, die Quersumme.
- Von den 90 Zahlendrehern aus zwei verschiedenen Ziffern übersieht EAN-13 genau 10: die Paare, deren Ziffern sich um 5 unterscheiden (05, 16, 27, 38, 49 und umgekehrt). Die ISBN-10 übersieht keinen, weil 11 eine Primzahl ist und sich die Summe beim Vertauschen um den Unterschied der beiden Ziffern ändert.
- Eine einzelne falsche Ziffer findet EAN-13 immer: Mit dem Gewicht 1 ändert sie die Summe um 1 bis 9, mit dem Gewicht 3 um 3 bis 27, aber nie um ein Vielfaches von 10.

#### Die Aufgaben

- »Erst planen, dann programmieren« beginnt mit übersetzen: Bei <code>check_digit</code> steht das Struktogramm auf der Seite, bei <code>valid_ean</code> ein Plan in Schritten in den Tipps, bei der ISBN nichts. Der Plan für die ISBN ist die Stelle, an der sich zeigt, ob jemand plant oder gleich tippt; ein Vergleich zweier Pläne an der Tafel lohnt sich (Was geschieht mit den Bindestrichen? Wo wird das X behandelt?).
- Die Prüfungen von <code>valid_ean</code> und <code>valid_isbn</code> rufen die Funktionen auch mit zu kurzen, zu langen und falschen Eingaben auf; die Aufgabe kündigt das an. Wer die Fälle im Plan hat, besteht sie beim ersten Versuch.
- Das Struktogramm im Feld zum Ausprobieren entsteht aus dem Pseudocode; »Als SVG speichern« lädt das Bild herunter, etwa für ein Arbeitsblatt. Die Schlüsselwörter des Pseudocodes sind Englisch wie im ganzen Handbuch.
- Die häufigsten Fehler, die die Prüfungen eigens abfangen: die Gewichte vertauscht (mit 3 beginnend), das zweite <code>mod 10</code> vergessen (die Prüfziffer wird 10), ein Code mit 14 Ziffern als gültig, das X der ISBN nicht behandelt.

#### Was schiefgeht

- **<code>irb</code> oder Python kennen eine geänderte Funktion nicht:** Sie haben die Datei beim Start geladen; nach einer Änderung neu starten.
- **Ruby: <code>valid_isbn?("3-12-345603-X")</code> gibt <code>false</code>:** <code>"X".to_i</code> ist 0, ohne Fehlermeldung; das X muss eigens behandelt werden.
- **<code>swaps</code> findet 0:** Meist wird der ursprüngliche Code geprüft statt des vertauschten.

#### Weiter

QR-Codes reparieren Fehler, statt sie nur zu finden (Reed-Solomon); das Experiment mit dem überklebten QR-Code am Ende der Seite dauert zwei Minuten. Die IBAN (Rest bei der Division durch 97) eignet sich als eigene Aufgabe mit derselben Planung: erst der Plan, dann der Code.
