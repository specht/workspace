Die Abgabe der Reihe: Lernende bauen die Abrechnung der Klassenfahrt selbst, aus einer Vorlage, in der noch nichts rechnet. Einnahmen und Ausgaben mit SUMME, der Rest, die Zahl der Kinder mit ANZAHL, die Rückzahlung pro Kind mit ABRUNDEN, eine Spalte mit festem Bezug darauf, WENN für Rück- oder Nachzahlung, Formate, die Liste nach Familien sortiert, ein Kreisdiagramm; zuletzt das Ganze als PDF. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>eine Tabelle, in der noch nichts rechnet, selbst mit Formeln, festen Bezügen und Funktionen (SUMME, ANZAHL, ABRUNDEN, WENN) aufbauen,</li>
<li>begründen, warum ein Betrag pro Kind abgerundet und nicht gerundet wird, wenn das Geld reichen muss,</li>
<li>eine fertige Tabelle mit Formaten, Sortierung und einem Diagramm so aufbereiten, dass andere sie lesen können, und als PDF weitergeben.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>Die sechs Blätter davor, von <a href='/spreadsheet-cells'>Zellen, die rechnen</a> bis <a href='/spreadsheet-charts'>Diagramme</a>. Die Seite verlinkt zu jedem Punkt das Blatt, auf dem er erklärt ist.</td></tr>
<tr><th>Ablauf (45 Minuten)</th><td><ul>
<li><b>5 Minuten:</b> die Aufgabe und die Liste der Punkte; die Vermutung zum Runden.</li>
<li><b>25 Minuten:</b> die Abrechnung bauen, zwischendurch prüfen.</li>
<li><b>10 Minuten:</b> die Vermutung zum PDF, Querformat auf einer Seite, exportieren und hochladen.</li>
<li><b>5 Minuten:</b> Puffer; wer fertig ist, vergleicht mit einer Nachbarin oder einem Nachbarn, ob beide auf 11,79 € kommen.</li>
<li><b>Für Schnelle und zum Üben:</b> die Extras am Ende der Seite (siehe unten).</li>
</ul>Reicht die Stunde nicht, geht die Abgabe zu Hause weiter: Die Seite hält die zuletzt geprüfte Fassung fest.</td></tr>
<tr><th>Dateien</th><td>Die Vorlage <code>07 Abrechnung.xlsx</code> kommt von der Seite; LibreOffice, Excel und Nextcloud Office öffnen und speichern sie. Speichern als <code>.ods</code> geht auch. Hochgeladen werden zwei Dateien: die Tabelle und das PDF.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Einnahmen sind nach der Familie sortiert, jede Zeile ist dieselbe wie in der Vorlage; die Ausgaben sind unverändert.</li>
<li>G13 bis G19 und D5 bis D31 rechnen mit Formeln: SUMME, ANZAHL, ABRUNDEN, WENN; D5 bis D31 mit festem Bezug auf G17.</li>
<li>Die Ergebnisse: Einnahmen 6.590,50 €, Ausgaben 6.272,00 €, Rest 318,50 €, 27 Kinder, zurück 11,79 € pro Kind, 0,17 € bleiben, »Rückzahlung«.</li>
<li>Formate: Währung für Beträge, Datum für die Tage. Ein Kreisdiagramm der Ausgaben mit Titel.</li>
<li>Rechnet mit: Die Prüfung setzt in ihrer Kopie den Bus auf 1.400 € (dann 168,50 € Rest, 6,24 € pro Kind, 0,02 € bleiben) und auf 1.700 € (dann −131,50 € und »Nachzahlung«). Formeln mit getippten Zahlen (<code>=SUMME(6590,5)</code>, <code>=318,5-0,17</code>) zählen nicht.</li>
<li>Das PDF: mindestens eine Seite, alle Schriften eingebettet, mindestens 60 Wörter.</li>
</ul></td></tr>
</table>

#### Extras für Schnelle und zum Üben

Am Ende der Seite stehen drei Extras; sie zählen bei niemandem mit. Wer die
Abrechnung besteht, bekommt auf der Karte einen Vorschlag: nach drei oder
mehr Prüfungen »Noch eine Runde«, nach einer oder zwei die »Fünf Fallen«.

- **Noch eine Runde:** die Abrechnung der Theaterfahrt, gebaut wie die
  Abgabe: 27 Kinder zu 22 €, drei Rechnungen, 61 € Rest, 2,25 € pro Kind
  (gerundet 2,26 €, dann fehlten 2 Cent), 0,25 € bleiben.
- **Zwei Handgriffe:** die fertige Abrechnung mit einer späten Rechnung des
  Busunternehmens (450 €), 131,50 € fehlen. Abgerundet zahlt jedes Kind
  4,87 € nach, und der Kasse fehlt ein Cent; G17 rundet deshalb mit WENN
  auf, wenn Geld fehlt (4,88 €, 0,26 € bleiben), und ab, wenn etwas übrig
  ist. Die Prüfung setzt die späte Rechnung auf 0 und erwartet wieder
  11,79 €; wer nur aufrundet, fällt dort auf. Dazu färbt eine bedingte
  Formatierung G19 bei »Nachzahlung«; die Prüfung liest Bedingungen auf
  Text (gleich, enthält, eine Formel wie <code>G19="Nachzahlung"</code>).
- **Fünf Fallen:** die Abrechnung des Wandertags, in Eile gebaut. Die Summe
  der Einnahmen lässt die letzte Familie aus (vom ersten Blatt), ein Betrag
  steht als Text und fehlt in SUMME und ANZAHL (vom zweiten), Spalte D
  rechnet ohne $ mit G17 (vom dritten), »Rückzahlung« ist getippt statt mit
  WENN berechnet (vom vierten), und G17 rundet mit RUNDEN statt ABRUNDEN.
  Mit den Fallen sieht die Abrechnung fertig aus: 9 Kinder, 1,92 € zurück.
  Die Prüfung zählt nur und zeigt jede Falle erst, wenn sie behoben ist;
  Tipps gibt es für jede einzeln.
