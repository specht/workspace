Lernende erkunden das Netz mit Kommandozeilenwerkzeugen: die eigene Adresse, Entfernungen mit `ping` und `traceroute`, eine Schicht tiefer MAC-Adressen, ARP, Switches und Router im eigenen Netz, dann Schichten, Ports und der TCP-Handshake, eine eigene Verbindung mit `netcat` und HTTP von Hand. Die Seite ist die erste Hälfte von TCP → HTTP → Webserver: »Ein eigener Webserver« und »Daten aus dem Netz« setzen sie voraus. Programmierkenntnisse braucht sie nicht, nur das Terminal.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> <code>ifconfig</code>, private Adressen, <code>ping</code> zu Google, zu sich selbst und zu <code>rubiomonocoat.pe</code> (das Rätsel um den Standort), Canberra, <code>traceroute</code> und TTL.</li>
<li><b>2. Doppelstunde:</b> »Im eigenen Netz«: die MAC-Adresse mit <code>ip link</code>, der Frame, ARP, zu zweit <code>ping</code> und <code>ip neigh</code>, die Routing-Tabelle mit <code>ip route</code>, Switches, WLAN und was ein Router mit Frame und Paket tut. Danach die Schichten, Ports und der Handshake mit <code>curl -w</code> gemessen.</li>
<li><b>3. Doppelstunde:</b> <code>netcat</code> allein und zu zweit, HTTP von Hand bei <code>info.cern.ch</code>, dann <code>wget</code>, <code>curl</code> und <code>jq</code>. Das ASCII-Kino passt ans Ende oder in eine Pause.</li>
</ul></td></tr>
<tr><th>Ports</th><td>Zwischen zwei Workspaces ist nur Port 1234 frei, für <code>netcat</code> zu zweit, dazu <code>ping</code>. Allein, mit <code>localhost</code>, geht jeder Port. Nach außen gehen Verbindungen ins Internet, auch an Port 80 (<code>info.cern.ch</code>).</td></tr>
<tr><th>Im eigenen Netz</th><td><ul>
<li>Alle Workspaces hängen an einer Linux-Bridge auf dem Server, einem Switch aus Software; ARP geht zwischen ihnen ungehindert, darum sehen Lernende nach einem <code>ping</code> die MAC-Adresse der anderen Person in <code>ip neigh</code>.</li>
<li>Mitschneiden (<code>tcpdump</code>, <code>arping</code>) geht im Workspace nicht: Die Werkzeuge sind nicht installiert, und dem Container fehlt das Recht für rohe Sockets. Was ARP schickt, erklärt die Seite; zu sehen ist das Ergebnis in <code>ip neigh</code>.</li>
<li><code>ip neigh flush</code> ist nicht erlaubt. Gelernte Einträge blieben im Test über 20 Minuten stehen und waren nach einem Neustart des Workspace fort; wer die Vorhersage »Bevor ihr euch etwas schickt« noch einmal machen will, nimmt eine Person, mit der er noch nichts getauscht hat.</li>
</ul></td></tr>
</table>

#### Das Rätsel um Peru

- Die Seite stellt `rubiomonocoat.pe` als Server mit peruanischer Domain vor und fragt dann, ob die gemessenen 25 ms dazu passen.
- Die Aufgabe »Mindestens wie lange?« rechnet die Lichtlaufzeit nach Lima aus (111 ms), »Wo steht der Server wirklich?« lässt den Namen in der `ping`-Ausgabe lesen: ein Hoster in Belgien.
- Der Punkt: Eine Domain-Endung sagt nichts über den Standort.
- Canberra danach zeigt, wie ein wirklich weit entfernter Server aussieht.
- Steht der Server einmal woanders, prüft die Aufgabe weiter gegen eine Messung im Workspace der Person. Die Musterlösung nennt aber Combell, und einen Hoster, dessen Name nicht auf ein Länderkürzel endet, erkennt die Prüfung nur, wenn er in `src/ruby/include/measure_check.rb` eingetragen ist.

#### Was schiefgeht

- `ping` und `nc -l` laufen, bis man <kbd>Strg</kbd><kbd>C</kbd> drückt.
- Nach der Antwort von `info.cern.ch` wartet `nc` weiter auf Eingaben; auch hier hilft <kbd>Strg</kbd><kbd>C</kbd>.
- Wer die leere Zeile nach den Headern vergisst, bekommt keine Antwort, bis der Server nach einer Weile aufgibt.
- Ohne `-C` schickt `nc` Zeilenenden ohne `\r`, und `info.cern.ch` antwortet mit `400 Bad Request`.
- Die erste Messung mit `curl -w` enthält das Nachschlagen des Namens, die zweite nicht.
- `telnet` lässt sich am einfachsten mit der Mülltonne über dem Terminal beenden.
- Für `ip neigh` zu zweit muss A die IP-Adresse von B anpingen, nicht die eigene; wer sich vertippt und eine Adresse trifft, die niemand hat, bekommt `Destination Host Unreachable` und in `ip neigh` ein `FAILED`.
- Haben sich zwei schon vorher etwas geschickt, steht die andere Person schon vor dem ersten `ping` in `ip neigh`; dann mit einer dritten Person versuchen.

#### Die Aufgaben

- »Deine Adresse« vergleicht mit der Adresse des Workspace.
- »Drei Entfernungen«, »Wo steht der Server wirklich?«, »Router zählen« und »Welcher Server?« messen im Workspace der Person selbst und vergleichen mit ihrer Eingabe.
- »Die erste Webseite« fragt nach dem Titel von `TheProject.html` (»The World Wide Web project«), »Ein Spiel aus dem Netz« sucht das übersetzte Programm `2048`.
- »Deine MAC-Adresse« vergleicht mit der MAC-Adresse, die Docker dem Workspace beim Start gegeben hat. Nach einem Neustart hatte der Workspace im Test dieselbe IP-Adresse, aber eine andere MAC-Adresse; eine Antwort von vor dem Neustart gilt dann nicht mehr.
- »Ein Paket mit TTL 3«, »Wie lange dauert der Handshake?« und »Ohne leere Zeile« sind Vorhersagen, ebenso »Wer kennt die MAC-Adresse?«, »Bevor ihr euch etwas schickt«, »Wer kennt jetzt wen?«, »Und 8.8.8.8?« und »Ein Switch ohne Tabelle«.
- »Beim zweiten Router« fragt, was ein Router am Frame und am Paket ändert, mit Tipps in Stufen.

#### Weiter

»Ein eigener Webserver« schreibt die Gegenseite von `nc -l` in Ruby, »Daten aus dem Netz« benutzt eine API aus einem Programm und einer Webseite. Für ein Gespräch eignen sich der Handshake (warum fühlt sich Australien so langsam an?) und `telnet`: Was unverschlüsselt durchs Netz geht, kann jeder Router unterwegs mitlesen.
