Lernende bringen die Liste der Zahlungen aller 27 Familien in Ordnung: nach Familien sortiert, ohne dass eine Zeile auseinanderfällt, mit einem Filter für die Familien, die noch etwas zahlen müssen, und einer bedingten Formatierung, die offene Beträge rot hinterlegt. Die Prüfung vergleicht jede Zeile mit der Vorlage und erkennt so eine Spalte, die allein sortiert wurde. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die ersten vier Blätter der Reihe, vor allem <a href='/spreadsheet-functions'>Auswerten</a> (leere Zellen in Funktionen).</td></tr>
<tr><th>Ablauf (45 Minuten)</th><td><ul>
<li><b>5 Minuten:</b> Vorführen (siehe unten).</li>
<li><b>10 Minuten:</b> die Vermutung zur leeren Zelle, die Vorlage öffnen, die Vermutung zur allein sortierten Spalte, sortieren.</li>
<li><b>15 Minuten:</b> filtern, ablesen, wie viele noch zahlen müssen, die Vermutung zur SUMME unter dem Filter, den Filter zurücknehmen.</li>
<li><b>10 Minuten:</b> bedingte Formatierung, prüfen, bis alles erfüllt ist.</li>
<li><b>5 Minuten:</b> der Versuch mit Kayas Zahlung; Gespräch über die Person Hollerith: Was zählt eine Maschine, die Menschen zählt?</li>
<li><b>Für Schnelle und zum Üben:</b> die Extras am Ende der Seite (siehe unten).</li>
</ul></td></tr>
<tr><th>Vorführen</th><td>
<p>Bevor die Klasse anfängt, lohnt es sich, die wichtigsten Handgriffe einmal am Beamer zu zeigen, in dem Programm, mit dem die Klasse arbeitet. Hier stehen sie für das Programm, das oben auf der Seite gewählt ist.</p>
<p>Nur eine Spalte markieren und sortieren lassen, die Nachfrage des Programms zeigen und mit »nur die Auswahl« bestätigen: Die Namen stehen neben fremden Beträgen. Rückgängig machen, dann in die Liste klicken und richtig sortieren. Einen Filter setzen und zurücknehmen.</p>
<!-- language: libreoffice -->
<p>Sortieren: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Sortieren…</em>; die Nachfrage bietet <em>Erweiterte Auswahl</em> und <em>Aktuelle Auswahl</em>. Filter: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>AutoFilter</em>. Bedingte Formatierung: <em>Format</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Bedingte Formatierung</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Bedingung…</em>, die Vorlage <em>Schlecht</em>.</p>
<!-- /language -->
<!-- language: word -->
<p>Sortieren: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Sortieren</em>; die Nachfrage bietet <em>Markierung erweitern</em> und <em>Mit bestehender Markierung fortfahren</em>. Filter: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Filtern</em>. Bedingte Formatierung: <em>Start</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Bedingte Formatierung</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Regeln zum Hervorheben von Zellen</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Größer als…</em></p>
<!-- /language -->
<!-- language: nextcloud -->
<p>Sortieren: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Sortieren…</em> Filter: <em>Daten</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>AutoFilter</em>. Bedingte Formatierung: <em>Format</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Bedingte Formatierung</em> <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Bedingung…</em> Die Namen sind die von LibreOffice; in Nextcloud Office vor dem Unterricht nachsehen.</p>
<!-- /language -->
</td></tr>
<tr><th>Dateien</th><td>Die Vorlage <code>05 Zahlungen.xlsx</code> kommt von der Seite; LibreOffice, Excel und Nextcloud Office öffnen und speichern sie. Speichern als <code>.ods</code> geht auch. Die Familiennamen sind ausgedacht; offen sind am Ende 1.220 € bei sechs Familien (vier ohne Zahlung, zwei mit einem Teil).</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Liste ist nach der Familie sortiert, von A bis Z (Ç wie C, ą wie a, wie die Programme sortieren), und jede Zeile ist dieselbe wie in der Vorlage, nur an anderer Stelle.</li>
<li>Spalte E und die Summe darunter rechnen weiter mit Formeln; keine Zelle zeigt einen Fehler.</li>
<li>Die Liste hat einen Filter; ob gerade etwas ausgeblendet ist, zählt nicht.</li>
<li>E4 bis E30 haben eine bedingte Formatierung. Welche Farbe sie gibt, prüft der Workspace nicht; auf dem gezeichneten Blatt ist sie zu sehen.</li>
</ul></td></tr>
</table>

#### Extras für Schnelle und zum Üben

Am Ende der Seite stehen drei Extras; sie zählen bei niemandem mit. Wer die
Zahlungen besteht, bekommt auf der Karte einen Vorschlag: nach drei oder
mehr Prüfungen »Noch eine Runde«, nach einer oder zwei die »Fünf Fallen«.
Bei der bedingten Formatierung sieht die Prüfung nach, ob sie genau die
Beträge über 0 färbt: »größer oder gleich 0« (alles rot) oder »größer als
100« fallen auf.

- **Noch eine Runde:** die Ausleihe der Klassenbücherei, nach dem Tag; nach
  dem Namen sortieren, filtern, wem noch Bücher fehlen färben.
- **Zwei Handgriffe:** nach zwei Spalten sortieren (Offen absteigend, dann
  Familie) und `=TEILERGEBNIS(9;E4:E30)`, die Summe nur der Zeilen, die der
  Filter zeigt.
- **Fünf Fallen:** die Zahlungen für den Wandertag. Kaya steht vor Jansen,
  die Liste hat keinen Filter, die Farbe kommt erst über 9 € (Çelik fehlen 8 €),
  bei Kaya ist der offene Betrag getippt (vom ersten Blatt), und ein Soll hat
  kein Format Währung (vom zweiten). Die Prüfung zählt nur und zeigt jede
  Falle erst, wenn sie behoben ist; Tipps gibt es für jede einzeln.
