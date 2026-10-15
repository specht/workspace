Die ganze Klasse malt an einem Bild, live: ein Ruby-Server mit WebSockets, den die Lernenden in drei Schritten bauen (Handshake und Echo, Nachrichten an alle, die Pixelwand), dazu eine kleine Webseite mit `<canvas>`. Am Ende gibt eine Person (oder die Lehrkraft) Port 8000 frei, und alle malen gemeinsam. Vorausgesetzt wird »Ein eigener Webserver«.

<table class='table'>
<tr><th>Ablauf</th><td>Erste Doppelstunde: warum WebSockets, die Datei <code>websocket.rb</code>, der Handshake mit SHA-1 und Base64, das Echo, Nachrichten an alle in zwei Tabs. Zweite: die Pixelwand mit JSON und Prüfung der Nachrichten, das gemeinsame Malen über Shared Live Apps, eigene Erweiterungen.</td></tr>
<tr><th>Dateien</th><td>In <code>/workspace/pixelwand</code>: <code>server.rb</code>, <code>websocket.rb</code> (die Frames, zum Abschreiben) und <code>index.html</code>. Die Stufen liegen unter <code>src/content/pixelwand/stufen/</code> (<code>server1.rb</code> bis <code>server3.rb</code>, <code>index1.html</code> für die ersten beiden, <code>index3.html</code> für die Wand).</td></tr>
<tr><th>Gemeinsam malen</th><td>Am besten startet die Lehrkraft den Server in ihrem eigenen Workspace und gibt Port 8000 frei; alle öffnen die Adresse. Die Freigabe gilt für die eigene Gruppe. Der Zustand des Bildes lebt im Server; ein Neustart löscht es, solange niemand die Erweiterung »Das Bild speichern« gebaut hat.</td></tr>
</table>

**Was schiefgeht.** Ein Tippfehler im festen String `MAGIC` oder ein fehlendes `\r\n` im Handshake: Der Browser schließt die Verbindung sofort, die Seite meldet »Die Verbindung ist zu«. Die Browserkonsole (<kbd>F12</kbd>) zeigt dann »WebSocket connection failed«. Nach jeder Änderung den Server neu starten und die Seite neu laden. Wer `broadcast` ohne Lock baut, bekommt bei vielen Malenden gelegentlich zerrissene Nachrichten; das ist ein guter Anlass, über Threads zu sprechen.

**Die Aufgaben.** »Die Pixelwand« liest `server.rb` und sucht `JSON.parse`, den Handshake und eine Bereichsprüfung. Die übrigen fragen nach dem Statuscode 101, der Länge eines Frames, wer eine Nachricht bekommt (Vorhersage), der Größe des Bildes und warum ein Server fremden Nachrichten nicht trauen darf.

**Weiter.** Die Ideen am Ende der Seite, vor allem die Wartezeit pro Verbindung (wie »r/place«), entstehen meist von selbst, sobald die ersten das Bild übermalen.
