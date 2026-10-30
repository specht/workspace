Die ganze Klasse malt an einem Bild, live: Lernende schreiben einen Server in Ruby oder Python mit WebSockets in drei Schritten (Echo, Nachrichten an alle, die Pixelwand), dazu eine kleine Webseite mit `<canvas>`. Am Ende gibt eine Person (oder die Lehrkraft) Port 8000 frei, und alle malen gemeinsam. Ein letzter Abschnitt zeigt an einem Zähler, was Threads ohne Lock anrichten.

<table class='table'>
<tr><th>Voraussetzungen</th><td>»Ein eigener Webserver«.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> warum WebSockets, die Gems (in Python das Paket), das Echo, der Handshake mit <code>curl</code>, Nachrichten an alle in zwei Tabs.</li>
<li><b>2. Doppelstunde:</b> die Pixelwand mit JSON und Prüfung der Nachrichten, das gemeinsame Malen über Shared Live Apps, dann »Viele Threads« mit dem Lock.</li>
</ul></td></tr>
<tr><th>Bibliotheken</th><td>Handshake und Frames übernimmt eine Bibliothek, in Ruby faye-websocket, in Python das Paket websockets; die Lernenden kümmern sich um Verbindungen, Verteilen, JSON und das Prüfen fremder Nachrichten. <code>gem install faye-websocket puma rackup</code> und <code>pip install websockets</code> installieren ins eigene Verzeichnis (<code>/workspace/.local</code> in Python), ohne Administratorrechte.</td></tr>
<tr><th>Dateien</th><td><ul>
<li>In <code>/workspace/pixelwand</code>: <code>server.rb</code> oder <code>server.py</code>, <code>index.html</code> und für den letzten Abschnitt <code>flood.js</code>.</li>
<li>Die Stufen liegen unter <code>src/content/pixelwand/stufen/</code>: <code>server1.rb</code> bis <code>server5.rb</code> und <code>server1.py</code> bis <code>server5.py</code>, <code>index1.html</code> für die ersten beiden, <code>index3.html</code> für die Wand.</li>
</ul></td></tr>
<tr><th>Gemeinsam malen</th><td>Am besten startet die Lehrkraft den Server in ihrem eigenen Workspace und gibt Port 8000 frei; alle öffnen die Adresse. Hinein kommt nur, wer angemeldet ist. Der Zustand des Bildes lebt im Server; ein Neustart löscht es, bis jemand »Das Bild laden« gebaut hat.</td></tr>
</table>

#### Was schiefgeht

- **Die Gems fehlen.** Dann bricht `ruby server.rb` mit `cannot load such file -- faye/websocket` ab; in Python heißt das `ModuleNotFoundError: No module named 'websockets'`, und es fehlt `pip install websockets`.
- **`ConnectionClosedError` im Python-Server.** Schließt ein Programm die Verbindung ohne Abschied, wie `curl` beim Handshake, zeigt der Server diese Meldung; sie ist harmlos, der Server läuft weiter.
- **Nichts ändert sich.** Nach jeder Änderung den Server neu starten und die Seite neu laden.
- **»WebSocket connection failed«** in der Browserkonsole (<kbd>F12</kbd>): Es läuft kein Server.

#### Viele Threads

- `flood.js` schickt 200 Pixel an den eigenen Server auf `localhost`. Vier Threads arbeiten eine Queue ab (eine feste Zahl, denn Workspaces begrenzen die Zahl der Threads; ein Thread pro Nachricht scheitert dort bei etwa 160).
- Ohne Lock steht der Zähler danach weit unter 200, im Test meist zwischen 50 und 70, weil die Threads denselben alten Wert lesen, während einer speichert; mit `$lock.synchronize` bei 200.
- Faye selbst ruft die `on`-Blöcke nacheinander in einem Thread auf, darum brauchen die ersten Stufen keinen Lock.
- In Python hat jede Verbindung ihren eigenen Thread; die Menge `clients` kommt trotzdem ohne Lock aus, weil `add` und `discard` einzelne Schritte sind und `broadcast` über eine Kopie geht. Ohne Lock kommt der Python-Server im Test auf etwa 90.
- Das Skript erreicht nur den eigenen Server: Shared Live Apps lassen nur angemeldete Browser herein, und Workspaces erreichen einander nicht direkt. Die Seite sagt deutlich, dass das Überschütten fremder Server ein Angriff ist (§ 303b StGB); wer eine geteilte Wand schützen will, baut die Wartezeit pro Verbindung aus den Ideen.

#### Die Aufgaben

- »Die Pixelwand« liest `server.rb` und sucht `JSON.parse`, `Faye::WebSocket` und eine Bereichsprüfung, »Ein Lock« `Thread.new`, `Mutex.new` und `synchronize`; in Python lesen sie `server.py` und suchen `json.loads`, `websockets` und eine Bereichsprüfung, `threading.Thread`, `Lock()` und `with lock`.
- »An alle, und wieder weg« und »Nur glauben, was passt« laden `server.rb` und rufen `App` mit einer Verbindung zum Ausprobieren auf, ohne dass der Server läuft (in Python `handle` aus `server.py`). Die erste schickt »Hallo«, während eine zweite Verbindung (Bo) offen ist, und schließt: Bo muss das »Hallo« bekommen, die Verbindung aus der Liste verschwinden und Bo danach hören, dass noch 1 da ist. Die zweite schickt einen gültigen Pixel und zehn ungültige Nachrichten (kein JSON, kein Hash, Text statt Zahl, `x` 64 und −1, `y` 40, Farbe 16 und −1 …); nur der gültige darf ins Bild und an alle. Beide gelten für den Stand, an dem sie stehen: Ab »Viele Threads« verarbeitet eine Queue die Nachrichten, dann kommt bei diesem Aufruf nichts mehr an.
- Die übrigen fragen nach dem Statuscode 101, wer eine Nachricht bekommt und was der Zähler nach 200 Pixeln zeigt (Vorhersagen), der Größe des Bildes und warum ein Server fremden Nachrichten nicht trauen darf.

#### Jetzt bist du dran

- Nach dem Echo zeigt die Seite den Server nicht mehr jedes Mal ganz, sondern nur, was sich ändert; nur für die Pixelwand mit JSON kommt die Datei noch einmal ganz, weil sich fast jede Zeile ändert.
- An drei Stellen schreiben die Lernenden den Code zuerst selbst: Nachrichten an alle und das Aufräumen beim Schließen einer Verbindung (in Python die beiden Lücken mit `...` in `handle`), dann die Prüfung jeder Nachricht, bevor sie ins Bild kommt, und zuletzt den Lock um das Ändern von Bild und Zähler (`$lock.synchronize`, `with lock:`). »An alle, und wieder weg«, »Nur glauben, was passt« und »Ein Lock« prüfen sie. Die Seite geht danach mit ihrem eigenen Code weiter.
- Für die Threads zeigt die Seite die Schleife mit einer Lücke: Die Lernenden schneiden ihre eigene Prüfung aus `ws.on(:message)` (in Python aus `handle`) aus und setzen sie dort ein, statt sie von der Seite neu zu kopieren.
- Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.
- Der Kasten »Ausprobieren« nach der Pixelwand schickt aus der Browserkonsole ungültige Nachrichten und zeigt so, ob die eigene Prüfung hält; »Die Pixelwand« sucht nur nach einer Bereichsprüfung, »Nur glauben, was passt« probiert alle aus.

#### Weiter

Die Ideen am Ende der Seite, vor allem die Wartezeit pro Verbindung (wie »r/place«), entstehen meist von selbst, sobald die ersten das Bild übermalen.

#### Eigene Projekte

Nach der Seite hat die Klasse einen gemeinsamen Zustand, den jede Änderung an alle verteilt. Die Ideen bauen darauf; bewertet wird beim gemeinsamen Ausprobieren und am Erklären, was der Server bei einer Nachricht tut.

- **Ein Programm, das malt** (eine Doppelstunde): ein Client ohne Browser, der ein Bild aus einer Datei Pixel für Pixel an die Wand schickt. Sobald zwei solche Programme gegeneinander malen, wird die Wartezeit pro Verbindung zur Frage der Fairness; das Lock aus [»Ein Lock«](#ein-lock) zeigt, was der Server dabei aushalten muss.
- **Ein Bild nach Plan** (eine Stunde): Die Klasse einigt sich vorher auf ein Motiv und teilt die Wand in Bereiche auf. Was beim Malen schiefgeht, wer in fremde Bereiche malt und wie man sich abspricht, ist dasselbe Problem wie bei Software, an der viele zugleich arbeiten.
- **Ein Spiel auf der Wand** (ein Projekt): ein Spiel, das alle zugleich spielen, etwa Felder erobern in Teamfarben oder ein Bild, das man gemeinsam erraten muss. Neu ist, dass der Server Regeln prüft, bevor er ein Pixel annimmt, und nicht mehr alles weitergibt.
