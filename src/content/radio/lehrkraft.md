Lernende funken zu zweit Wörter von einem Raum in den anderen, nur mit bis zu fünf Lampen, die jemand mit gehaltenen Knöpfen ein- und ausschaltet, auf einer Frequenz, die alle hören können. Vorher einigen sie sich auf ein Protokoll: wie Buchstaben zu Lichtzeichen werden, woran man Anfang und Ende erkennt, was bei Fehlern passiert. Während der Übertragungen nehmen Sie Lampen weg, und die Paare müssen ihr Protokoll anpassen, ohne sich zu treffen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine; die Aktivität passt als Einstieg vor <a href='/tcpip'>TCP/IP</a>. Sie geht auf das Arbeitsblatt »Datenübertragung per Kabel« und den Cable Simulator der Hackschule zurück; aus dem Kabel ist ein Funkgerät geworden, weil eine Frequenz, die alle hören, das Mithören und Stören erklärt.</td></tr>
<tr><th>Vorbereitung</th><td><ul>
<li>Zwei Räume (oder ein Raum und der Flur).</li>
<li>Für jede Person ein <a href='${web_root}/radio/blatt.pdf'>Blatt</a> (A4, eine Seite): oben das Protokoll, unten acht Runden mit den Spalten »Gesendet«, in die Sie das Wort schreiben, und »Empfangen« für die Empfängerseite.</li>
<li>Heften Sie die Aktivität auf der <a href='/klasse'>Klassenseite</a> im Reiter »Startseite« für die Gruppe an: Dann steht sie bei den Lernenden oben auf der Startseite unter »Von deiner Lehrkraft«.</li>
<li>Für das Funkgerät selbst braucht niemand ein Konto: <code>${web_root}/ether</code> öffnet sie direkt, auch auf dem Handy. Im Kasten »Für Lehrkräfte« unter den Lampen zeigt der Knopf »Für den Beamer« diese Adresse groß mit QR-Code.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>10 Minuten:</b> Paare bilden, das Funkgerät ausprobieren (Frequenz, Knöpfe, Tasten 1 bis 5).</li>
<li><b>15 Minuten:</b> Protokoll ausdenken und oben auf das Blatt schreiben, die Frequenz eintragen.</li>
<li><b>Runden:</b> Die Paare trennen sich; die Senderseite holt sich bei Ihnen ein Wort, Sie sagen es leise und schreiben es auf ihr Blatt. Die Empfängerseite bringt ihr Blatt zu Ihnen; erst dann vergleichen die beiden. Neues Wort, die Rollen dürfen wechseln.</li>
<li><b>Weniger Lampen:</b> Nach zwei, drei Runden setzen Sie die Lampen auf 4, später auf 3 und so weiter.</li>
<li><b>Am Ende:</b> die Besprechung (unten) und die beiden Reflexionen.</li>
</ul></td></tr>
<tr><th>Lampen wegnehmen</th><td><ul>
<li>Im Kasten »Für Lehrkräfte« unter den Lampen: Wartezeit wählen (0 bis 5 Minuten, voreingestellt 3) und auf die neue Zahl klicken.</li>
<li>Alle sehen dann einen Countdown, und die Lampen, die wegfallen, sind gestrichelt umrandet; »Abbrechen« nimmt die Änderung zurück, solange sie noch nicht gilt. Mit Wartezeit können die Paare eine laufende Übertragung noch beenden.</li>
<li><strong>Die Zahl der Lampen gilt für alle, die das Funkgerät auf dieser Installation gerade benutzen</strong>, auch für Klassen anderer Lehrkräfte: Es gibt nur einen gemeinsamen Äther, wie im Original. Ändern können sie alle angemeldeten Lehrkräfte und die Administration.</li>
</ul></td></tr>
<tr><th>Wörter</th><td><ul>
<li>Kommt eine Senderseite zu Ihnen, nennt sie ihre Frequenz. In der Liste »Frequenzen« gibt der Knopf <i class='bi bi-chat-quote'></i> in dieser Zeile das nächste Wort aus einer gemischten Liste von 92 ausgedachten Wörtern (5 bis 11 Buchstaben, nur A bis Z), ohne Wiederholung. Es steht groß unter »Wörter« und in der Zeile der Frequenz. Sagen Sie es leise und schreiben Sie es auf das Blatt der Senderseite, unter »Gesendet«.</li>
<li>Bringt die Empfängerseite ihr Blatt, steht das Wort ihrer Frequenz noch in deren Zeile, auch wenn gerade niemand mehr auf ihr ist; »Vergeben« unter den Wörtern listet alle Runden mit Uhrzeit, Frequenz und Zahl der Lampen.</li>
<li>»Nächstes Wort« gibt ein Wort ohne Frequenz, »Neu mischen« fängt von vorn an und leert die Liste. Wörter und Liste merkt sich nur Ihr Browser, nicht der Server.</li>
<li>Einige Wörter haben doppelte Buchstaben (FROZZLEBOP, KWIBBLE): Gut für die Besprechung, denn wer Buchstaben nur durch einen Wechsel der Lampen trennt, sieht dort einen statt zwei.</li>
</ul></td></tr>
<tr><th>Frequenzen</th><td>Die Liste im Kasten zeigt jede Frequenz, auf der gerade jemand ist, mit der Zahl der Personen und den Lampen, die leuchten. Mehr als zwei Personen auf einer Frequenz sind hervorgehoben: Dort hört jemand mit oder zwei Paare haben dieselbe gewählt. Die Lernenden sehen die Liste nicht.</td></tr>
<tr><th>Gespeichert</th><td>Auf dem Server nichts. Frequenzen, Lampen und die Zahl der Lampen liegen nur im Arbeitsspeicher des Servers und sind nach einem Neustart weg (dann wieder 5 Lampen). Welches Wort an welche Frequenz ging, steht nur im Browser der Lehrkraft. Die Reflexionen speichert der Workspace wie jede Aufgabe, nur für Angemeldete.</td></tr>
</table>

#### Besprechung

Gute Fragen danach, an den Blättern entlang:

- *Codierung:* Wie habt ihr Buchstaben dargestellt? Mit fünf Lampen gleichzeitig gibt es 32 Muster, genug für 26 Buchstaben; mit vier nur 16, dann braucht ein Buchstabe zwei Schritte. Manche Paare benutzen Lampen nacheinander, wie Morsezeichen: langsamer, aber robuster.
- *Anfang und Ende:* Woran erkennt die Empfängerseite, dass es losgeht, dass ein Buchstabe fertig ist und dass das Wort zu Ende ist? Ein Muster für »Start« und »Ende«, oder die Länge vorab.
- *Takt:* Wie lange gilt ein Muster? Eine Lampe als Takt (»jetzt gilt es«) löst das Problem, dass beide verschieden schnell sind. Doppelte Buchstaben (FROZZLEBOP) zeigen, ob das Protokoll Zeichen wirklich trennt.
- *Bestätigung:* Hat die Empfängerseite zurückgesendet, dass ein Buchstabe angekommen ist? Das kostet Zeit, verhindert aber, dass die Senderseite zu schnell wird; hier wird aus einer Leitung eine Verbindung in beide Richtungen wie bei TCP.
- *Fehler:* Was passiert bei einem falschen Buchstaben? Nachfragen, Wiederholen, eine Prüfsumme (etwa die Zahl der Buchstaben am Ende).
- *Mithören und Stören:* Alle auf derselben Frequenz sehen alles und können senden. Niemand merkt, wer mitliest. Wer stört, fällt nur auf, wenn das Protokoll Unsinn erkennt. Das ist die Eigenschaft von Funk, die ein Kabel nicht hat, und führt zu Verschlüsselung und zu Prüfsummen.
- *Bandbreite:* Weniger Lampen heißt weniger Information pro Schritt und damit mehr Schritte pro Buchstabe. Wer die Dauer der Runden in die Tabelle schreibt, sieht es. Am Ende der Seite steht Baudots Telegraf mit fünf Tasten, nach dem die Einheit Baud heißt.
- *Sender-Empfänger-Modell:* Die Seite schließt mit Shannons Bild von Quelle, Sender, Kanal mit Störungen, Empfänger und Ziel; die Lernenden ordnen ihre eigene Übertragung darin ein.

#### Was schiefgeht

- Eine Lampe leuchtet nur, solange jemand ihren Knopf hält; verliert ein Fenster den Fokus oder wird der Tab gewechselt, gehen die Lampen dieser Person aus.
- Auf dem Handy dürfen mehrere Finger gleichzeitig halten.
- Bricht die Verbindung ab (WLAN), sagt das Funkgerät es unter den Lampen und verbindet sich nach einigen Sekunden neu, auf derselben Frequenz.
- Die Tasten 1 bis 5 wirken auf der Seite der Aktivität nur, solange die Lampen zu sehen sind.
