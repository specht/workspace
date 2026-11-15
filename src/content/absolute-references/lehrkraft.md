Lernende rechnen aus, was ein Kind für die Klassenfahrt bezahlt: die Kosten jedes Postens geteilt durch die Zahl der Kinder in B2, dann weniger die Ermäßigung in B3. Die Formel des Elternteils, `=B6/B2`, nach unten ausgefüllt, teilt in C7 durch die Ermäßigung und weiter unten durch eine leere Zelle; mit `$B$2` bleibt der Bezug fest. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>vorhersagen, wie sich die Bezüge einer Formel ändern, wenn man sie nach unten oder nach rechts ausfüllt,</li>
<li>mit dem Dollarzeichen einen Bezug festhalten (<code>$B$2</code>) und begründen, wann das nötig ist,</li>
<li>eine Formel schreiben, die jede Zeile mit einer festen Zelle verrechnet, etwa die Kosten geteilt durch die Zahl der Kinder.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td><a href='/spreadsheet-cells'>Zellen, die rechnen</a> (Ausfüllen, der Weg, den sich eine Formel merkt) und <a href='/number-formats'>Zahlen in Form</a> (das Format Währung).</td></tr>
<tr><th>Ablauf (45 Minuten)</th><td><ul>
<li><b>5 Minuten:</b> Vorführen (siehe unten).</li>
<li><b>10 Minuten:</b> die Vermutung zum Ausfüllen, die Formel in der Vorlage ausfüllen, ablesen, was C7 zeigt, rückgängig machen.</li>
<li><b>15 Minuten:</b> das Dollarzeichen: C6 bis C10 mit <code>$B$2</code>, die SUMME, B2 zum Ausprobieren ändern; die Vermutung zum Ziehen nach rechts.</li>
<li><b>10 Minuten:</b> die Ermäßigung mit <code>$B$3</code>, das Format Währung, prüfen, bis alles erfüllt ist.</li>
<li><b>5 Minuten:</b> der Versuch mit <code>=$B6</code>, nach rechts und unten ausgefüllt.</li>
<li><b>Für Schnelle und zum Üben:</b> die Extras am Ende der Seite (siehe unten).</li>
</ul></td></tr>
<tr><th>Vorführen</th><td>
<p>Bevor die Klasse anfängt, lohnt es sich, die wichtigsten Handgriffe einmal am Beamer zu zeigen, in dem Programm, mit dem die Klasse arbeitet. Hier stehen sie für das Programm, das oben auf der Seite gewählt ist.</p>
<p>Eine Formel mit einem Bezug auf eine Zelle über der Tabelle nach unten ausfüllen und in zwei, drei Zellen nachsehen, was aus dem Bezug geworden ist; dann das Dollarzeichen setzen und noch einmal ausfüllen.</p>
<!-- language: libreoffice -->
<p>Die Dollarzeichen setzt <kbd>Shift</kbd>+<kbd>F4</kbd>, wenn der Cursor in der <em>Eingabezeile</em> in einem Bezug steht; jedes Drücken wechselt zwischen <code>$B$2</code>, <code>B$2</code>, <code>$B2</code> und <code>B2</code>.</p>
<!-- /language -->
<!-- language: word -->
<p>Die Dollarzeichen setzt <kbd>F4</kbd>, wenn der Cursor in der <em>Bearbeitungsleiste</em> in einem Bezug steht; jedes Drücken wechselt zwischen <code>$B$2</code>, <code>B$2</code>, <code>$B2</code> und <code>B2</code>. Auf manchen Laptops braucht <kbd>F4</kbd> die Taste <kbd>Fn</kbd> dazu.</p>
<!-- /language -->
<!-- language: nextcloud -->
<p>Die Dollarzeichen werden getippt, in der Zeile über der Tabelle.</p>
<!-- /language -->
</td></tr>
<tr><th>Dateien</th><td>Die Vorlage <code>03 Anteile.xlsx</code> kommt von der Seite; LibreOffice, Excel und Nextcloud Office öffnen und speichern sie. Speichern als <code>.ods</code> geht auch. Die Kosten sind die der Kinder aus dem ersten Blatt (die beiden Lehrkräfte bezahlen nicht die Familien); jede Familie hat im zweiten Blatt 245 € im Voraus eingezahlt, was übrig bleibt, kommt im letzten Blatt zurück.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Daten der Vorlage (Zahl der Kinder, Ermäßigung, Posten und Kosten) sind unverändert.</li>
<li>C6 bis D11 rechnen mit Formeln, in C6 bis C10 ohne getippte Zahlen (<code>=B6/27</code> sagt die Prüfung mit der Formel); C6 bis C10 mit festem Bezug auf B2, D6 bis D10 auf B3 (<code>B$2</code> genügt, nur die Zeile muss fest sein); C11 und D11 mit SUMME.</li>
<li>Die Summen stimmen (232,30 € und 220,68 €), keine Zelle zeigt einen Fehler, C6 bis D11 haben ein Währungsformat.</li>
<li>Rechnet mit: Die Prüfung setzt in ihrer Kopie die Zahl der Kinder auf 24 und die Ermäßigung auf 10 %; dann müssen C11 261,33 € und D11 235,20 € zeigen. Wer 27 oder 0,95 in die Formeln getippt hat, bleibt bei den alten Zahlen.</li>
</ul></td></tr>
</table>

#### Extras für Schnelle und zum Üben

Am Ende der Seite stehen drei Extras; sie zählen bei niemandem mit. Wer die
Anteile besteht, bekommt auf der Karte einen Vorschlag: nach drei oder mehr
Prüfungen »Noch eine Runde«, nach einer oder zwei die »Fünf Fallen«.

- **Noch eine Runde:** was ein Kind fürs Sportfest bezahlt, gebaut wie die
  Anteile: 28 Kinder, 10 % Ermäßigung; dieselben Punkte wie bei den Anteilen.
- **Zwei Handgriffe:** eine Preistabelle für 25, 27 und 29 Kinder aus einer
  einzigen Formel mit gemischten Bezügen (<code>=$B4*C$3</code>), nach
  rechts und nach unten ausgefüllt, und der Anteil jedes Postens an der
  Summe pro Kind (<code>=B4/$B$8</code>) als Prozent. Die Prüfung ändert einen
  Preis und eine Kinderzahl und rechnet neu.
- **Fünf Fallen:** was ein Kind beim Wandertag bezahlt. Eine Formel teilt
  durch die Ermäßigung (ohne $ ausgefüllt), eine durch eine getippte 26, eine
  nimmt getippte 5 % weg, ein Betrag hat kein Format Währung (vom zweiten
  Blatt), und eine SUMME hört eine Zeile zu früh auf (vom ersten). Die
  Prüfung zählt nur und zeigt jede Falle erst, wenn sie behoben ist; Tipps
  gibt es für jede einzeln.
