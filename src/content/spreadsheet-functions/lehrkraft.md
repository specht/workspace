Lernende werten die Umfrage nach der Klassenfahrt aus: 27 Antworten mit ein bis fünf Sternen zu vier Programmpunkten, manche Fragen ausgelassen. Unter jede Spalte kommen ANZAHL, MITTELWERT, MAX und MIN, nach rechts ausgefüllt, und WENN entscheidet, ob ein Programmpunkt mindestens vier Sterne bekam. Zwei Stellen, an denen Lernende stolpern sollen: Eine leere Zelle ist keine 0, und der Mittelwert der Jugendherberge zeigt 4,0, ist aber 3,96, sodass WENN »nein« sagt. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/spreadsheet-cells'>Zellen, die rechnen</a> (SUMME, Ausfüllen), <a href='/number-formats'>Zahlen in Form</a> (das Format ändert nicht die Zahl) und <a href='/absolute-references'>Feste Bezüge</a> (Bezüge wandern beim Ausfüllen mit).</td></tr>
<tr><th>Ablauf (45 Minuten)</th><td><ul>
<li><b>5 Minuten:</b> Vorführen (siehe unten).</li>
<li><b>10 Minuten:</b> die Funktionen, die Vermutung zur leeren Zelle, ANZAHL bis MIN unter dem Kletterwald.</li>
<li><b>10 Minuten:</b> nach rechts ausfüllen, ANZAHL fürs Moormuseum ablesen, eine Nachkommastelle.</li>
<li><b>15 Minuten:</b> WENN, die Vermutung zur Jugendherberge, prüfen, bis alles erfüllt ist.</li>
<li><b>5 Minuten:</b> der Versuch mit der nachgetragenen Antwort; Gespräch: Ist eine Umfrage ohne Namen wirklich anonym, wenn nur 27 Kinder mitmachen?</li>
<li><b>Für Schnelle:</b> in Zeile 37 mit <code>=ZÄHLENWENN(B4:B30;5)</code> zählen, wie viele fünf Sterne gaben.</li>
</ul></td></tr>
<tr><th>Vorführen</th><td>
<p>Bevor die Klasse anfängt, lohnt es sich, die wichtigsten Handgriffe einmal am Beamer zu zeigen, in dem Programm, mit dem die Klasse arbeitet. Hier stehen sie für das Programm, das oben auf der Seite gewählt ist.</p>
<p>Eine Funktion tippen, den Bereich mit der Maus markieren, während die Formel offen ist; die Funktion finden, wenn man ihren Namen nicht weiß; WENN mit Text in Anführungszeichen.</p>
<!-- language: libreoffice -->
<p>Der <em>Funktionsassistent</em> öffnet sich über das Zeichen <em>f</em>(x) über der Tabelle oder <kbd>Strg</kbd>+<kbd>F2</kbd>. Nachkommastellen: Rechtsklick <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Zellen formatieren…</em>, Kategorie <em>Zahl</em>, <em>Nachkommastellen</em>, oder die Knöpfe <strong>Dezimalstelle hinzufügen</strong> und <strong>Dezimalstelle entfernen</strong>.</p>
<!-- /language -->
<!-- language: word -->
<p><em>Funktion einfügen</em> öffnet sich über das Zeichen <em>fx</em> neben der Bearbeitungsleiste. Nachkommastellen: <kbd>Strg</kbd>+<kbd>1</kbd>, Kategorie <em>Zahl</em>, <em>Dezimalstellen</em>, oder im Register <em>Start</em> die Knöpfe <em>Dezimalstelle hinzufügen</em> und <em>Dezimalstelle entfernen</em>.</p>
<!-- /language -->
<!-- language: nextcloud -->
<p>Die Funktionen finden sich über das Zeichen <em>f</em>(x) neben der Zeile über der Tabelle. Nachkommastellen: Rechtsklick <i class='bi bi-chevron-right' aria-hidden='true'></i> <em>Zellen formatieren…</em>, Kategorie <em>Zahl</em>, <em>Nachkommastellen</em>.</p>
<!-- /language -->
</td></tr>
<tr><th>Dateien</th><td>Die Vorlage <code>04 Umfrage.xlsx</code> kommt von der Seite; LibreOffice, Excel und Nextcloud Office öffnen und speichern sie. Speichern als <code>.ods</code> geht auch. Die Antworten sind ausgedacht; ausgelassene Fragen stehen als leere Zellen darin (Kletterwald eine, Moormuseum drei, Jugendherberge zwei).</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Die Antworten sind unverändert; B32 bis E36 rechnen mit Formeln: Zeile 32 mit ANZAHL, 33 mit MITTELWERT, 34 mit MAX, 35 mit MIN, 36 mit WENN.</li>
<li>Die Ergebnisse stimmen (Antworten 26, 27, 24, 25; Mittelwerte 4,4, 3,6, 2,6 und 4,0; »ja« nur beim Kletterwald); die Mittelwerte haben eine Nachkommastelle; keine Zelle zeigt einen Fehler.</li>
<li>Rechnet mit: Die Prüfung trägt in ihrer Kopie in E15 fünf Sterne nach und setzt B4 auf einen Stern; dann muss die Jugendherberge 26 Antworten, den Mittelwert 4 und »ja« zeigen, der Kletterwald den schlechtesten Wert 1. Ein Bereich, der vor Zeile 30 endet, oder getippte Mittelwerte fallen dabei auf.</li>
</ul></td></tr>
</table>
