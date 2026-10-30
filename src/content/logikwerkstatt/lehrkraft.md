Lernende bauen Schaltungen selbst in einem Editor auf der Seite: vom Halbaddierer über einen Multiplexer und einen Vergleicher bis zum Addierer für 4 Bit aus eigenen Bausteinen und einem D-Flipflop. Jede Schaltung prüft der Server, indem er sie Zeile für Zeile durchrechnet.

<table class='table'>
<tr><th>Voraussetzungen</th><td>»Vom Gatter zum Rechenwerk«: die Gatter und ihre Schaltzeichen, Halb- und Volladdierer, das RS-Flipflop. Programmieren müssen die Lernenden hier nicht.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der Editor und der freie Editor oben, Halbaddierer (als Baustein <code>HA</code> gespeichert), XOR aus vier NAND, Multiplexer, Volladdierer aus zwei <code>HA</code> (als <code>VA</code> gespeichert).</li>
<li><b>2. Doppelstunde:</b> Addierer für 4 Bit aus vier <code>VA</code>, die Vorhersage zum Übertrag, der Vergleicher für 2 Bit, das D-Flipflop.</li>
<li><b>Für Schnelle:</b> ein Register für 2 Bit aus zwei D-Flipflops im freien Editor; ein Multiplexer für vier Eingänge aus dreien für zwei.</li>
</ul></td></tr>
<tr><th>Geräte</th><td>Ein Computer oder ein Tablet; Maus, Finger und Tastatur gehen. Auf einem Handy lassen sich Schaltungen nur ansehen und schalten.</td></tr>
<tr><th>Dateien</th><td>Keine: Gebaut wird auf der Seite, der Workspace muss nicht laufen.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sieben Aufgaben mit einer Schaltung (<code>circuit</code>): Der Editor schickt sie als Text, der Server ersetzt jeden Baustein durch seine Gatter und rechnet Runde für Runde wie die Seite; ausgeführt wird nichts.</li>
<li>Schaltnetze: alle Zeilen der Wahrheitstabelle, beim Addierer 512. Das D-Flipflop: sieben Schritte nacheinander.</li>
<li>Wo die Aufgabe es sagt: nur bestimmte Gatter (XOR nur aus NAND), höchstens so viele Gatter (auch in Bausteinen gezählt), mindestens so viele Bausteine (Volladdierer, Addierer).</li>
<li>Eine Vorhersage: welche Lampe sich beim Addierer zuletzt ändert.</li>
</ul></td></tr>
<tr><th>Was gespeichert wird</th><td>Die geprüfte Schaltung als Antwort der Aufgabe, wie bei jeder anderen; Lehrkräfte sehen im Klassenboard nur das Ergebnis. Entwürfe und die eigenen Bausteine bleiben im Browser.</td></tr>
<tr><th>Lösung</th><td>Unter jeder Aufgabe nach dem Lösen, in Worten; nach einigen Aufgaben zeigt die Seite eine Schaltung zum Vergleichen.</td></tr>
</table>

#### Die Aufgaben

- **»Ein Halbaddierer«:** XOR und UND, höchstens zwei Gatter, ohne Bausteine. Danach speichern die Lernenden ihn als Baustein `HA`; die Seite sagt, wie.
- **»XOR aus vier NAND«:** nur NAND, höchstens vier. Mit fünf NAND (aus NICHT und ODER zusammengesetzt) stimmt die Tabelle, die Rückmeldung sagt »es geht mit 4«.
- **»Ein Multiplexer«:** `sel` wählt zwischen `a` und `b`, höchstens vier Gatter. Häufig: `a` und `b` vertauscht; die Rückmeldung nennt die erste falsche Zeile.
- **»Ein Volladdierer aus zwei Halbaddierern«:** mindestens zwei Bausteine, fünf Gatter zusammen. Ohne Bausteine gebaut stimmt die Tabelle, die Rückmeldung verlangt die Bausteine. UND statt ODER für die Überträge fällt in der Zeile 011 auf.
- **»Ein Addierer für 4 Bit«:** vier `VA`; der häufigste Fehler, ein Übertrag aus `c_in` statt aus dem Nachbarn, zeigt sich gleich bei 0000 + 0000 + 1.
- **»Welche Lampe zuletzt?«** (Vorhersage): `s3`, der Übertrag läuft durch alle vier Volladdierer.
- **»Zwei Zahlen vergleichen«** (Knobelei): `eq` aus zwei Gleichheiten (XOR mit NICHT), `gt` vom höchsten Bit an. Wer beim niederen Bit vergisst, dass die höheren gleich sein müssen, bekommt die Zeile a = 01, b = 10 genannt.
- **»Ein D-Flipflop«** (Knobelei): RS-Flipflop aus zwei NOR mit zwei UND und einem NICHT davor, oder vier NAND und ein NICHT. Die Rückmeldung nennt den Schritt und sagt, was dort gelten muss.

#### Was schiefgeht

- **»Am UND g2 ist ein Eingang ohne Leitung«:** Jeder Eingang braucht eine Leitung; ein offener Eingang ist im Editor ein leerer Kreis.
- **Das Flipflop kommt nicht zur Ruhe:** Im Editor beginnen alle Gatter mit 0, und die beiden NOR schalten im Gleichtakt. Einmal den Takt auf 1 legen; die Prüfung beginnt genauso und legt im ersten Schritt den Takt auf 1.
- **Ein Baustein fehlt in der Leiste:** Bausteine merkt sich der Browser. An einem anderen Rechner kommen sie mit einer geprüften Schaltung zurück, in der sie stecken; sonst den Halbaddierer noch einmal speichern.
- **Q an der falschen Seite:** Liegt die Lampe an dem NOR, an dem S hängt, zeigt sie Q quer; die Rückmeldung nennt schon den ersten Schritt.

#### Eigene Projekte

Nach der Seite können die Lernenden in Schichten bauen: aus Gattern Bausteine, aus Bausteinen größere. Die Ideen nehmen das wieder auf; bewertet wird an der Schaltung, an ihrer Wahrheitstabelle und daran, dass die Lernenden einen Baustein erklären.

- **Eine Anzeige für Ziffern** (eine Doppelstunde): eine Schaltung, die aus vier Bits die sieben Striche einer Ziffernanzeige ansteuert. Erst die Wahrheitstabelle für jeden Strich, dann die Gatter; wer geschickt ist, findet Striche, die sich Teile teilen.
- **Ein Zähler** (ein Projekt, für Starke): ein Register aus vier Bits und der [Addierer für 4 Bit](#der-addierer-für-4-bit), der bei jedem Takt eins dazuzählt. Das [D-Flipflop](#das-d-flipflop) der Seite allein reicht dafür nicht: Solange der Takt 1 ist, lässt es durch, und die Summe liefe gleich wieder durch den Addierer. Zwei davon hintereinander mit umgekehrtem Takt (Master-Slave) übernehmen nur beim Wechsel; das herauszufinden ist der Kern. Zum Vorführen klickt jemand den Takt, und die Zahl läuft hoch, bis sie bei 15 wieder auf 0 springt.
- **Mit möglichst wenig NAND** (eine Stunde): Wie in [»Alles aus NAND«](#alles-aus-nand), aber als Wettbewerb: der Multiplexer oder der Vergleich zweier Zahlen mit so wenigen NAND wie möglich. Die Klasse vergleicht und erklärt, wo sich etwas sparen ließ.

Wer weiter will: die [CPU Sandbox](/sandbox-cpu), ein Prozessor aus genau solchen Bauteilen, und für größere Schaltungen die Simulatoren [Digital](https://github.com/hneemann/Digital) und [Logisim-evolution](https://github.com/logisim-evolution/logisim-evolution).
