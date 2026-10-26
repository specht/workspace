Lernende schreiben einen Webserver in Ruby oder Python, ohne Bibliothek, in sechs Schritten: zuhören, antworten, Pfade mit 404, Dateien ausliefern, Parameter und ein Zähler, ein Formular mit `POST`. Die Aktivität schließt an TCP/IP an und macht sichtbar, was Live Server, Sinatra oder nginx tun.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python (Variablen, Schleifen, Funktionen, Verzweigungen) und die Seite TCP/IP.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen. Die ausgelieferten Seiten in <code>public</code> sind in beiden Sprachen dieselben.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der zuhörende Server mit <code>curl</code> im zweiten Terminal, die erste Antwort, Status-Line und Header, der Browser über den Reiter »Ports«, Pfade und 404.</li>
<li><b>2. Doppelstunde:</b> Dateien aus <code>public</code> mit Content-Type und der Prüfung gegen <code>..</code>, Parameter, der Zähler mit Mutex oder Lock, die Umfrage mit <code>POST</code>, <code>Content-Length</code> und <code>303 See Other</code>, Teilen über Shared Live Apps; wer schnell ist, lässt die Klasse abstimmen.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles liegt in <code>/workspace/webserver</code>: <code>server.rb</code> oder <code>server.py</code> und der Ordner <code>public</code>. Jede Stufe liegt in diesem Repository unter <code>src/content/webserver/stufen/</code> (<code>1.rb</code> bis <code>6.rb</code> und <code>1.py</code> bis <code>6.py</code>, dazu <code>public/poll.html</code>).</td></tr>
<tr><th>Ports</th><td>Der Server hört auf Port 8000. Der Browser erreicht ihn über die Adresse im Reiter »Ports« (»Forwarded Address«); für die Klasse gibt man den Port im Profil unter »Shared Live Apps« frei. Port 1234 ist der einzige Port für direkte Verbindungen zwischen Workspaces und wird hier nicht gebraucht.</td></tr>
</table>

#### Was schiefgeht

- **»Address already in use«** heißt: Ein alter Server läuft noch, oft im anderen Terminal. <kbd>Strg</kbd><kbd>C</kbd> dort beendet ihn.
- **Es ändert sich nichts.** Nach jeder Änderung muss der Server neu gestartet werden; ein Server, der nicht neu gestartet wurde, ist die häufigste Ursache.
- **Der Browser wartet oder schneidet ab.** Es fehlt die leere Zeile `"\r\n"` nach den Headern, oder `Content-Length` stimmt nicht (Zeichen statt Bytes bei Umlauten).
- **Keine Ausgabe im Browser.** Ausgaben mit `puts` oder `print` erscheinen im Terminal des Servers, nicht im Browser.
- **Server und Browser hängen beim Formular.** Der Server liest den Body zeilenweise (`client.gets` in Ruby, `reader.readline()` in Python) statt mit `read`; hinter dem Body steht aber kein Zeilenumbruch.
- **`TypeError` in Python.** Antworten sind Bytes: Ein `str` an `sendall` ergibt diesen Fehler, darum gehört `encode()` davor.

#### Die Aufgaben

- »Die erste Antwort« liest `server.rb` und sucht Status-Line, `Content-Length` und `Thread.new` (in Python `server.py` mit `threading.Thread`); »Eine Seite aus public« prüft `public/index.html`.
- »Dein Server grüßt« führt den `curl`-Befehl der Lernenden in ihrem Workspace aus (als Gast, ohne Schreibrechte) und prüft, was ihr laufender Server auf `/hello?name=Ada%20Lovelace` antwortet; läuft der Server nicht, kommt keine Ausgabe.
- Die übrigen fragen nach Request-Line, Bytes, Statuscodes, der Sicherheitsprüfung, dem Mutex und dem Body; »Eine Seite, die es nicht gibt«, »Ein Name in der Adresse« und »Neu laden« sind Vorhersagen.
- »Eine Umfrage« sucht in `server.rb` nach `POST`, `Content-Length`, `client.read` und `303`, in `server.py` statt `client.read` nach `reader.read`.

#### Jetzt bist du dran

- Ab »Pfade und 404« zeigt die Seite nur noch, was sich am Server ändert, mit grauen Zeilen drumherum, statt die ganze Datei noch einmal. Wer den Überblick über die eigene Datei verliert, findet jede Stufe vollständig unter `stufen/`.
- An vier Stellen schreiben die Lernenden den Code zuerst selbst: die Funktion `respond`, in die sie die Zeilen der festen Antwort umziehen, die Auswahl, die alles außer `/time` an `serve_file` gibt, den Fall `/hello` und die Funktion `redirect`. Die Seite geht danach mit ihrem eigenen Code weiter.
- Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.
- Den Fall `/hello` prüft »Dein Server grüßt« am laufenden Server; wer sie löst, kann den Code der Seite sofort öffnen und mit dem eigenen vergleichen.

#### Weiter

Die Pixelwand baut auf diesem Server auf und ergänzt WebSockets. Für ein Gespräch über Sicherheit eignet sich die Prüfung mit `start_with?` (Python: `startswith`): Die Seite lässt den Angriff mit `curl --path-as-is localhost:8000/../server.rb` (oder `server.py`) ausprobieren, einmal mit und einmal ohne die Prüfung; ohne sie liefert der Server seinen Quelltext aus. Browser und normales `curl` entfernen `..` selbst, darum fällt die Lücke beim bloßen Klicken nicht auf.
