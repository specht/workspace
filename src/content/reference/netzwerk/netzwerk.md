<div class='meta'>
kind: reference
search: ping, traceroute, ifconfig, ss, Port, netcat, nc, dig, DNS, wget, curl, ssh, scp, Connection refused, localhost, 127.0.0.1
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Netzwerk im Terminal

<p class='abstract'>
Die Befehle aus <a href='/tcpip'>TCP/IP</a>, <a href='/dns'>DNS</a> und dem <a href='/terminal-mystery'>Krimi im Terminal</a> zum Nachschlagen: die eigene Adresse, ob ein Rechner erreichbar ist, eine eigene Verbindung, Namen nachschlagen, Dateien holen, auf einem anderen Rechner arbeiten, und was die Fehlermeldungen sagen. Wie Browser und Server miteinander reden, steht unter <a href='/reference/http'>HTTP</a>.
</p>

## Adressen und Ports

<!-- part: netz-adressen -->
<!-- short: ifconfig zeigt die eigene Adresse, ss -tln die Ports, an denen ein Programm wartet; localhost ist der eigene Rechner. -->
<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er zeigt</th></tr>
<tr><td><code>ifconfig</code></td><td>die Adressen dieses Rechners: hinter <code>inet</code> bei <code>eth0</code> die im Netz, bei <code>lo</code> immer <code>127.0.0.1</code></td></tr>
<tr><td><code>ss -tln</code></td><td>die Ports, an denen ein Programm auf Verbindungen wartet (<code>-t</code> TCP, <code>-l</code> nur wartende, <code>-n</code> Zahlen statt Namen)</td></tr>
</table>

`localhost` und `127.0.0.1` sind immer der eigene Rechner: Ein Server, den du im Workspace startest, ist dort zu erreichen, etwa unter `localhost:8000`.

Die **IP-Adresse** bringt ein Paket zum richtigen Rechner, der **Port** zum richtigen Programm. Ports, die man kennt:

<table class='table table-sm reference-table'>
<tr><th>Port</th><th>Wofür</th></tr>
<tr><td>22</td><td>SSH: auf einem anderen Rechner arbeiten, Git über SSH</td></tr>
<tr><td>53</td><td>DNS: Namen nachschlagen</td></tr>
<tr><td>80</td><td>HTTP, unverschlüsselt</td></tr>
<tr><td>443</td><td>HTTPS</td></tr>
<tr><td>8000, 8080, 1234 …</td><td>frei für eigene Programme, etwa den eigenen Webserver</td></tr>
</table>
<!-- /part -->

## Ist ein Rechner erreichbar?

<!-- part: netz-ping -->
<!-- short: ping misst, wie lange ein Paket hin und zurück braucht; traceroute zeigt die Stationen dazwischen. -->
<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er tut</th></tr>
<tr><td><code>ping 8.8.8.8</code></td><td>schickt jede Sekunde ein kleines Paket und zeigt, wie viele Millisekunden die Antwort braucht; <kbd>Strg</kbd><kbd>C</kbd> hört auf und zeigt eine Zusammenfassung</td></tr>
<tr><td><code>ping -c 4 www.anu.edu.au</code></td><td>nur vier Pakete; ein Name wird vorher nachgeschlagen</td></tr>
<tr><td><code>traceroute -q 1 8.8.8.8</code></td><td>die Router auf dem Weg dorthin, einer pro Zeile, mit der Zeit bis zu jedem; ein <code>*</code> heißt, dieser Router antwortet nicht, nicht, dass der Weg dort endet</td></tr>
</table>

Antwortet `ping` nicht, kann der Rechner trotzdem da sein: Viele Server und Firewalls lassen diese Pakete einfach fallen.
<!-- /part -->

## Eine eigene Verbindung: `nc`

<!-- part: netz-nc -->
<!-- short: nc -l -p 1234 wartet als Server, nc ADRESSE 1234 verbindet sich; was man tippt, geht hinüber. -->
`nc` (netcat) baut eine TCP-Verbindung auf und tut sonst nichts: Was du tippst, geht hinüber, was ankommt, steht im Terminal.

<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er tut</th></tr>
<tr><td><code>nc -l -p 1234</code></td><td>wartet als Server an Port 1234, bis sich jemand verbindet</td></tr>
<tr><td><code>nc localhost 1234</code></td><td>verbindet sich als Client mit Port 1234 auf dem eigenen Rechner; mit einer Adresse wie <code>172.20.0.12</code> mit einem anderen Workspace</td></tr>
<tr><td><code>nc -C info.cern.ch 80</code></td><td>eine Verbindung zu einem Webserver, jede Zeile endet so, wie HTTP es verlangt (<code>-C</code>); dann eine Anfrage von Hand tippen</td></tr>
<tr><td><code>nc -l -p 1234 &gt; datei</code></td><td>speichert, was ankommt, in einer Datei</td></tr>
<tr><td><code>nc -N ADRESSE 1234 &lt; datei</code></td><td>schickt eine Datei und schließt danach die Verbindung (<code>-N</code>)</td></tr>
</table>

<kbd>Strg</kbd><kbd>C</kbd> beendet die Verbindung. Eine Anfrage über HTTPS lässt sich so nicht tippen, weil alles verschlüsselt sein muss; dafür gibt es die [TCP-Konsole](/reference/http#tcp-konsole).
<!-- /part -->

## Namen nachschlagen: `dig`

<!-- part: netz-dig -->
<!-- short: dig name fragt das DNS nach der Adresse; +short nur die Antwort, Typen wie AAAA, MX, NS; -x rückwärts; +trace den ganzen Weg. -->
<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er zeigt</th></tr>
<tr><td><code>dig hackschule.de</code></td><td>die ganze Antwort: Frage, Antwort mit TTL in Sekunden, welcher Server geantwortet hat und wie schnell</td></tr>
<tr><td><code>dig +short hackschule.de</code></td><td>nur die IPv4-Adresse</td></tr>
<tr><td><code>dig +short AAAA google.com</code></td><td>die IPv6-Adresse; <code>MX</code> die Mailserver, <code>NS</code> die Nameserver, <code>TXT</code> Text-Einträge</td></tr>
<tr><td><code>dig +short -x 8.8.8.8</code></td><td>rückwärts: den Namen zu einer Adresse</td></tr>
<tr><td><code>dig @a.root-servers.net hackschule.de</code></td><td>fragt einen bestimmten Server statt des eigenen</td></tr>
<tr><td><code>dig +trace +nodnssec hackschule.de</code></td><td>den ganzen Weg: von der Wurzel über <code>de.</code> bis zum Server, der den Namen kennt</td></tr>
</table>
<!-- /part -->

## Dateien und Seiten holen

<!-- part: netz-holen -->
<!-- short: wget ADRESSE speichert eine Datei, curl ADRESSE zeigt sie im Terminal. -->
<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er tut</th></tr>
<tr><td><code>wget ADRESSE</code></td><td>lädt eine Datei in das aktuelle Verzeichnis, unter ihrem Namen aus der Adresse</td></tr>
<tr><td><code>curl ADRESSE</code></td><td>zeigt die Antwort im Terminal; <code>curl -s ADRESSE | less</code> seitenweise, <code>curl -o datei ADRESSE</code> in eine Datei</td></tr>
<tr><td><code>curl -s ADRESSE | jq</code></td><td>macht eine Antwort in JSON lesbar (Handbuch <a href='/reference/json'>JSON</a>)</td></tr>
</table>

Was `curl` sonst noch kann (Header ansehen, Formulare schicken, JSON schicken), steht unter [curl](/reference/http#curl). Steht `&` oder `?` in einer Adresse, setz sie in Anführungszeichen, sonst versteht die Shell sie falsch.
<!-- /part -->

## Auf einem anderen Rechner: `ssh` und `scp`

<!-- part: netz-ssh -->
<!-- short: ssh name@rechner öffnet ein Terminal dort, exit kehrt zurück; scp kopiert Dateien, -J über einen Rechner dazwischen. -->
<table class='table table-sm reference-table'>
<tr><th>Befehl</th><th>Was er tut</th></tr>
<tr><td><code>ssh name@rechner</code></td><td>ein Terminal auf dem anderen Rechner, angemeldet als <code>name</code>; <code>exit</code> kehrt zurück</td></tr>
<tr><td><code>scp name@rechner:pfad ziel</code></td><td>kopiert eine Datei von dort hierher; umgekehrt <code>scp datei name@rechner:pfad</code></td></tr>
<tr><td><code>scp 'name@rechner:ordner/a*' ziel</code></td><td>alle Dateien, deren Name mit <code>a</code> beginnt; die Anführungszeichen lassen das <code>*</code> erst auf dem anderen Rechner wirken</td></tr>
<tr><td><code>ssh -J name@a name@b</code></td><td>auf Rechner <code>b</code>, der nur von <code>a</code> aus zu erreichen ist (<code>-J</code> wie »jump«); bei <code>scp</code> genauso</td></tr>
</table>

Beim ersten Mal zeigt `ssh` den **Fingerabdruck** des Rechners und fragt, ob du ihm traust; mit `yes` merkt es sich ihn in `~/.ssh/known_hosts`. Ein Passwort erscheint beim Tippen nicht, auch nicht als Sternchen. Achte darauf, auf welchem Rechner du gerade bist: `scp` rufst du dort auf, wo die Datei ankommen soll, meist im eigenen Workspace. Wie du dich mit einem Schlüssel statt eines Passworts anmeldest, steht unter [SSH-Schlüssel](/reference/gitlab#ssh-schluessel).
<!-- /part -->

## Wenn etwas nicht klappt

<!-- part: netz-fehler -->
<!-- short: Connection refused, Could not resolve host, timed out, Address already in use, Permission denied (publickey): was sie bedeuten. -->
<table class='table table-sm reference-table'>
<tr><th>Meldung</th><th>Was meistens los ist</th></tr>
<tr><td><code>Connection refused</code>, <code>Couldn't connect to server</code></td><td>der Rechner ist da, aber an diesem Port wartet kein Programm: Läuft dein Server? Stimmt der Port?</td></tr>
<tr><td><code>Could not resolve host</code>, <code>Name or service not known</code></td><td>der Name ist falsch geschrieben, oder kein DNS-Server kennt ihn</td></tr>
<tr><td><code>Connection timed out</code></td><td>keine Antwort: falsche Adresse, der Rechner ist aus, oder eine Firewall lässt die Pakete fallen</td></tr>
<tr><td><code>Address already in use</code></td><td>beim Start eines Servers: Ein anderes Programm wartet schon an diesem Port, oft dein eigener Server von vorhin. Beende ihn mit <kbd>Strg</kbd><kbd>C</kbd> in seinem Terminal, oder nimm einen anderen Port.</td></tr>
<tr><td><code>Permission denied (publickey)</code></td><td>der Server will einen SSH-Schlüssel, den er nicht kennt, oder der Name vor dem <code>@</code> stimmt nicht</td></tr>
<tr><td><code>Permission denied, please try again.</code></td><td>bei <code>ssh</code> mit Passwort: falsches Passwort, oder das Passwort eines anderen Rechners</td></tr>
</table>
<!-- /part -->

## Zum Nachschlagen

Zu jedem Befehl zeigt `man befehl` alle Optionen, etwa `man dig` oder `man ssh`.

- [Julia Evans: How to use dig](https://jvns.ca/blog/2021/12/04/how-to-use-dig/): was in der Ausgabe von `dig` steht und welche Optionen man wirklich braucht, auf Englisch.
- [OpenSSH Manual Pages](https://www.openssh.org/manual.html): `ssh`, `scp` und `ssh-keygen` mit allen Optionen, auf Englisch.
