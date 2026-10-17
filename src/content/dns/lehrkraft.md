Ein Labor zu DNS, nach [TCP/IP-Verbindungen](/tcpip): Lernende fragen mit `dig` nach der Adresse von hackschule.de, lesen die Antwort (Frage, Antwort, TTL, fragender Server), lernen die Typen `A`, `AAAA`, `MX`, `NS` und `TXT` kennen und gehen den Weg einer Frage selbst ab, mit `dig @a.root-servers.net` und dann mit `dig +trace`: von den Root-Servern über die DENIC für `de` zu Cloudflare, das die Adresse kennt. Danach der Zwischenspeicher und die TTL, die Rückwärtssuche mit `dig -x` und was eine gefälschte Antwort anrichten kann. Nur Terminal, kein Programmieren.

<table class='table'>
<tr><th>Ablauf</th><td>Eine Doppelstunde: <code>dig</code> und die Typen (25 Minuten), die Hierarchie mit den Root-Servern und <code>+trace</code> (30 Minuten), TTL und Zwischenspeicher (15 Minuten), rückwärts und die gefälschte Antwort (20 Minuten).</td></tr>
<tr><th>Netz</th><td>Alles läuft im Workspace, der dafür ins Netz muss; im Prüfungsmodus geht es nicht. Der DNS-Server, den ein Workspace fragt, ist <code>127.0.0.11</code>, der eingebaute von Docker; er fragt weiter beim Server der Installation. <code>dig @a.root-servers.net</code> und <code>dig +trace</code> fragen die Server im Netz direkt, über UDP Port 53; ein Schulnetz, das das sperrt, lässt nur <code>dig</code> ohne <code>@</code> zu.</td></tr>
<tr><th>Prüfung</th><td>Wo die Antwort aus dem Netz kommt (Adresse, Mailserver, Nameserver, Rückwärtssuche), fragt der Server im Workspace der Person selbst mit <code>dig +short</code> nach und nimmt jede der Antworten an, ohne Punkt am Ende und ohne Groß- und Kleinschreibung. Ändert hackschule.de seinen Anbieter, bleiben die Aufgaben richtig. »Nur die Adresse« führt den Befehl der Person aus und will genau eine Zeile mit einer IPv4-Adresse. Dazu kommen Auswahl, eine Vorhersage und eine Reflexion.</td></tr>
</table>

**Die Ausgabe lesen.** `dig` gibt viel aus. Die Seite nennt drei Stellen: QUESTION, ANSWER mit der TTL und SERVER. Beim ersten Mal hilft es, die Ausgabe gemeinsam an der Tafel durchzugehen; dass der Punkt am Ende von `hackschule.de.` die Wurzel ist, überrascht fast alle.

**Die Aufgaben.** »Was der Root-Server sagt« lässt erkennen, dass ein Root-Server nicht weiterfragt, sondern verweist; die Rückmeldung zur letzten Option trennt das von dem, was der DNS-Server des Workspace tut. »Zweimal dieselbe Frage« ist eine Vorhersage vor dem Ausprobieren: Die TTL zählt beim Merken weiter herunter. »Eine falsche Antwort« zeigt, dass bei `http://` niemand Name und Adresse vergleicht, und leitet zu den Zertifikaten in [Kryptographie im Terminal](/crypto) über.

**Was schiefgeht.**

- **Die TTL ist nicht 300.** Hat kurz vorher jemand anderes in der Klasse gefragt, kommt die Antwort aus dem Zwischenspeicher, mit kleinerer TTL und `Query time: 0 msec`. Das ist kein Fehler, sondern das Thema des Abschnitts über die TTL.
- **`dig @a.root-servers.net` hängt.** Dann sperrt das Netz DNS zu fremden Servern. `dig +trace` geht dann auch nicht; die Lernenden können den Abschnitt mit den Bildern auf der Seite lesen.
- **Der Punkt am Ende.** In die Karten kann man Namen mit oder ohne den Punkt eintragen; beides zählt.
- **Bei `MX` die Zahl mit eintragen.** `5 gmail-smtp-in.l.google.com` wird nicht angenommen; die Aufgabe sagt, ohne die Zahl davor.

**Weiter.** [Ein eigener Webserver](/webserver) baut auf Namen und Ports auf; wer mag, sieht mit `dig +short TXT` nach, wie Domains mit SPF festlegen, wer in ihrem Namen E-Mails schicken darf.
