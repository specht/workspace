Lernende ersetzen in einer Tabelle der Kosten der Klassenfahrt getippte Ergebnisse durch Formeln: Anzahl mal Preis in jeder Zeile, nach unten ausgefüllt, der Gesamtbetrag mit SUMME. Die Prüfung ändert in ihrer Kopie einen Preis und rechnet neu; so sieht sie, ob die Tabelle wirklich rechnet. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>eine Zelle über ihre Adresse finden und ansprechen,</li>
<li>eine Formel schreiben, die mit den Werten anderer Zellen rechnet, sie nach unten ausfüllen und den Gesamtbetrag mit SUMME bilden,</li>
<li>begründen, warum ein getipptes Ergebnis nach einer Änderung falsch ist und eine Formel nicht.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>Keine; die Reihe zur Textverarbeitung hilft beim Umgang mit Dateien und der Prüfung.</td></tr>
<tr><th>Ablauf (45 Minuten)</th><td><ul>
<li><b>5 Minuten:</b> Vorführen (siehe unten).</li>
<li><b>10 Minuten:</b> die Vermutung zum neuen Preis, Zellen und Adressen, eine Zelle ablesen.</li>
<li><b>15 Minuten:</b> Formeln in D4 und D5, ein Preis zum Ausprobieren, die Vermutung zum Ausfüllen, nach unten ausfüllen.</li>
<li><b>10 Minuten:</b> SUMME, prüfen, bis alles erfüllt ist.</li>
<li><b>5 Minuten:</b> der Versuch mit einer eingefügten Zeile.</li>
<li><b>Für Schnelle und zum Üben:</b> die Extras am Ende der Seite (siehe unten).</li>
</ul></td></tr>
<tr><th>Vorführen</th><td>
<p>Bevor die Klasse anfängt, lohnt es sich, die wichtigsten Handgriffe einmal am Beamer zu zeigen, in dem Programm, mit dem die Klasse arbeitet. Hier stehen sie für das Programm, das oben auf der Seite gewählt ist.</p>
<p>Eine Formel eintippen (sie beginnt mit =) und sehen, wo sie steht, wenn die Zelle das Ergebnis zeigt; eine Zahl ändern, die Formel rechnet neu; das kleine Quadrat unten rechts an der Zelle nach unten ziehen; SUMME über einen Bereich.</p>
<!-- language: libreoffice -->
<p>Die Formel einer Zelle steht in der <em>Eingabezeile</em> über der Tabelle; SUMME über <strong>Σ</strong> (<em>Funktion auswählen</em>) <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Summe</em> oder getippt.</p>
<!-- /language -->
<!-- language: word -->
<p>Die Formel einer Zelle steht in der <em>Bearbeitungsleiste</em> über der Tabelle; SUMME über <em>Start</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <strong>Σ</strong> <em>AutoSumme</em> oder getippt.</p>
<!-- /language -->
<!-- language: nextcloud -->
<p>Die Formel einer Zelle steht in der Zeile über der Tabelle; SUMME über <strong>Σ</strong> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Summe</em> oder getippt.</p>
<!-- /language -->
</td></tr>
<tr><th>Dateien</th><td>Die Vorlage <code>01 Kosten.xlsx</code> kommt von der Seite; LibreOffice, Excel und Nextcloud Office öffnen und speichern sie. Speichern als <code>.ods</code> geht auch. Wie die Prüfung arbeitet (zwei Hälften des Bildschirms, Prüfen beim Speichern): die Notiz zum ersten Blatt der Textverarbeitung, <a href='/formatting-marks'>Was das Auge nicht sieht</a>.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Daten der Vorlage (Posten, Anzahl, Preis) sind unverändert.</li>
<li>In D4 bis D8 und in D9 stehen Formeln, keine getippten Zahlen; auch in den Formeln von D4 bis D8 stehen Adressen, keine Zahlen (<code>=29*148</code> zeigt das richtige Ergebnis und rechnet doch nicht mit).</li>
<li>D9 rechnet mit SUMME, über genau D4 bis D8; eine SUMME bis D7 sagt die Prüfung mit der fehlenden Zelle.</li>
<li>Die Ergebnisse stimmen.</li>
<li>Rechnet mit: Die Prüfung setzt in ihrer Kopie den Preis der Jugendherberge (C5) auf 152 und rechnet neu; dann müssen D5 4.408 und D9 6.711 zeigen. Getippte Ergebnisse bleiben bei den alten Zahlen.</li>
</ul></td></tr>
</table>

#### Was schiefgeht

- **<kbd>Strg</kbd>+<kbd>S</kbd> im Browser statt im Programm:** Wer nach einer Änderung auf die Seite geschaut hat, drückt <kbd>Strg</kbd>+<kbd>S</kbd> oft noch im Browser. Der bietet dann an, die Webseite zu speichern; das Dokument bleibt ungespeichert, und die Karte prüft nicht neu: Die Zeit, zu der die Datei zuletzt gespeichert wurde, bleibt die alte. Den Dialog abbrechen, ins Fenster von Excel oder LibreOffice klicken, dann speichern.

#### Extras für Schnelle und zum Üben

Am Ende der Seite stehen drei Extras; sie zählen bei niemandem mit. Wer die
Kosten besteht, bekommt auf der Karte einen Vorschlag: nach drei oder mehr
Prüfungen »Noch eine Runde«, nach einer oder zwei die »Fünf Fallen«.

- **Noch eine Runde:** der Einkauf fürs Sommerfest, gebaut wie die Kosten
  und ebenso mit dem Taschenrechner ausgerechnet; dieselben Punkte wie bei
  den Kosten.
- **Zwei Handgriffe:** die Kosten, schon rechnend, mit der Zahl der Personen
  darunter. Über »Gesamt« kommt eine Zeile für das Eis; die SUMME muss sie
  mitzählen, und wo sie es nicht tut, sagt die Prüfung, welche Zelle fehlt.
  Dazu ausrechnen, was jede Person bezahlt: eine Formel, die mit dem
  Ergebnis einer anderen rechnet (<code>=D10/B12</code>).
- **Fünf Fallen:** die Kosten des Wandertags. Jede Zelle in Spalte D zeigt
  eine Zahl, aber eine ist getippt, eine rechnet mit getippten Zahlen
  (<code>=2*45</code>), eine nimmt den Preis aus der Zeile darunter, eine
  zählt zusammen statt zu multiplizieren, und die SUMME hört eine Zeile zu
  früh auf. Die Prüfung zählt nur und zeigt jede Falle erst, wenn sie behoben
  ist; Tipps gibt es für jede einzeln.
