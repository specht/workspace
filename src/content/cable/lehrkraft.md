Lernende übertragen zu zweit Wörter von einem Raum in den anderen, nur mit bis zu fünf Lampen, die jemand mit gehaltenen Knöpfen ein- und ausschaltet. Vorher einigen sie sich auf ein Protokoll: wie Buchstaben zu Lichtzeichen werden, woran man Anfang und Ende erkennt, was bei Fehlern passiert. Während der Übertragungen nehmen Sie Lampen weg, und die Paare müssen ihr Protokoll anpassen, ohne sich zu treffen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Keine; die Aktivität passt als Einstieg vor <a href='/tcpip'>TCP/IP</a>. Sie geht auf das Arbeitsblatt »Datenübertragung per Kabel« und den Cable Simulator der Hackschule zurück.</td></tr>
<tr><th>Vorbereitung</th><td><ul>
<li>Zwei Räume (oder ein Raum und der Flur).</li>
<li>Heften Sie die Aktivität auf der <a href='/klasse'>Klassenseite</a> im Reiter »Startseite« für die Gruppe an: Dann steht sie bei den Lernenden oben auf der Startseite unter »Von deiner Lehrkraft«.</li>
<li>Für die Lampen selbst braucht niemand ein Konto: <code>${web_root}/ether</code> öffnet sie direkt, auch auf dem Handy. Im Kasten »Für Lehrkräfte« unter den Lampen zeigt der Knopf »Für den Beamer« diese Adresse groß mit QR-Code.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>10 Minuten:</b> Paare bilden, das Kabel ausprobieren (Frequenz, Knöpfe, Tasten 1 bis 5).</li>
<li><b>15 Minuten:</b> Protokoll ausdenken und aufschreiben, die Tabelle auf Papier vorbereiten.</li>
<li><b>Runden:</b> Die Paare trennen sich; die Senderseite holt sich bei Ihnen ein Wort und überträgt es, die Empfängerseite bringt es auf dem Zettel mit. Nach jeder Runde vergleichen, neues Wort.</li>
<li><b>Weniger Lampen:</b> Nach zwei, drei Runden setzen Sie die Lampen auf 4, später auf 3 und so weiter.</li>
<li><b>Am Ende:</b> die Besprechung (unten) und die beiden Reflexionen.</li>
</ul></td></tr>
<tr><th>Lampen wegnehmen</th><td><ul>
<li>Im Kasten »Für Lehrkräfte« unter den Lampen: Wartezeit wählen (0 bis 5 Minuten, voreingestellt 3) und auf die neue Zahl klicken.</li>
<li>Alle sehen dann einen Countdown, und die Lampen, die wegfallen, sind gestrichelt umrandet; »Abbrechen« nimmt die Änderung zurück, solange sie noch nicht gilt. Mit Wartezeit können die Paare eine laufende Übertragung noch beenden.</li>
<li><strong>Die Zahl der Lampen gilt für alle, die das Kabel auf dieser Installation gerade benutzen</strong>, auch für Klassen anderer Lehrkräfte: Es gibt nur einen gemeinsamen Äther, wie im Original. Ändern können sie alle angemeldeten Lehrkräfte und die Administration.</li>
</ul></td></tr>
<tr><th>Wörter</th><td><ul>
<li>»Nächstes Wort« im selben Kasten zeigt ein Wort aus einer gemischten Liste von 92 ausgedachten Wörtern (5 bis 11 Buchstaben, nur A bis Z), ohne Wiederholung, bis alle verteilt sind; »Neu mischen« fängt von vorn an. Die Reihenfolge merkt sich nur Ihr Browser, nicht der Server.</li>
<li>Zeigen Sie das Wort nur der Senderseite.</li>
<li>Einige Wörter haben doppelte Buchstaben (FROZZLEBOP, KWIBBLE): Gut für die Besprechung, denn wer Buchstaben nur durch einen Wechsel der Lampen trennt, sieht dort einen statt zwei.</li>
</ul></td></tr>
<tr><th>Frequenzen</th><td>Die Liste im Kasten zeigt jede Frequenz, auf der gerade jemand ist, mit der Zahl der Personen und den Lampen, die leuchten. Mehr als zwei Personen auf einer Frequenz sind hervorgehoben: Dort hört jemand mit oder zwei Paare haben dieselbe gewählt. Die Lernenden sehen die Liste nicht.</td></tr>
<tr><th>Gespeichert</th><td>Nichts. Frequenzen, Lampen und die Zahl der Lampen liegen nur im Arbeitsspeicher des Servers und sind nach einem Neustart weg (dann wieder 5 Lampen). Die Reflexionen speichert der Workspace wie jede Aufgabe, nur für Angemeldete.</td></tr>
</table>

#### Besprechung

Gute Fragen danach, an den Zetteln entlang:

- *Codierung:* Wie habt ihr Buchstaben dargestellt? Mit fünf Lampen gleichzeitig gibt es 32 Muster, genug für 26 Buchstaben; mit vier nur 16, dann braucht ein Buchstabe zwei Schritte. Mancher hat Lampen nacheinander benutzt (wie Morsezeichen): langsamer, aber robuster.
- *Anfang und Ende:* Woran erkennt die Empfängerseite, dass es losgeht, dass ein Buchstabe fertig ist und dass das Wort zu Ende ist? Ein Muster für »Start« und »Ende«, oder die Länge vorab.
- *Takt:* Wie lange gilt ein Muster? Eine Lampe als Takt (»jetzt gilt es«) löst das Problem, dass beide verschieden schnell sind. Doppelte Buchstaben (FROZZLEBOP) zeigen, ob das Protokoll Zeichen wirklich trennt.
- *Bestätigung:* Hat die Empfängerseite zurückgesendet, dass ein Buchstabe angekommen ist? Das kostet Zeit, verhindert aber, dass die Senderseite zu schnell wird; hier wird aus einer Leitung eine Verbindung in beide Richtungen wie bei TCP.
- *Fehler:* Was passiert bei einem falschen Buchstaben? Nachfragen, Wiederholen, eine Prüfsumme (etwa die Zahl der Buchstaben am Ende).
- *Mithören und Stören:* Alle auf derselben Frequenz sehen alles und können senden. Niemand merkt, wer mitliest. Wer stört, fällt nur auf, wenn das Protokoll Unsinn erkennt. Das führt zu Verschlüsselung und zu Prüfsummen.
- *Bandbreite:* Weniger Lampen heißt weniger Information pro Schritt und damit mehr Schritte pro Buchstabe. Wer die Dauer der Runden in die Tabelle schreibt, sieht es. Am Ende der Seite steht Baudots Telegraf mit fünf Tasten, nach dem die Einheit Baud heißt.

#### Was schiefgeht

- Eine Lampe leuchtet nur, solange jemand ihren Knopf hält; verliert ein Fenster den Fokus oder wird der Tab gewechselt, gehen die Lampen dieser Person aus.
- Auf dem Handy dürfen mehrere Finger gleichzeitig halten.
- Bricht die Verbindung ab (WLAN), sagt das Kabel es unter den Lampen und verbindet sich nach einigen Sekunden neu, auf derselben Frequenz.
- Die Tasten 1 bis 5 wirken auf der Seite der Aktivität nur, solange die Lampen zu sehen sind.
