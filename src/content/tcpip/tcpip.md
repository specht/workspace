<div class='meta'>
image: teaser.webp:0:50
image_alt: "Ein Terminal, in dem nc eine HTTP-Anfrage an info.cern.ch schickt, darunter die Antwort des Servers"
kind: labor
duration: 3 Doppelstunden
curriculum: "Sek I: Leben in und mit vernetzten Systemen (Datenaustausch im Netzwerk); Oberstufe: Computernetze (LAN, WLAN, Funktionsweise des Internet, Schichtenmodell, Protokolle), Rechner und Netze (Protokolle, Schichtenarchitektur), Vertiefung Computer-Netze (Ethernet, MAC-Adressen, ARP, Switches, Routing)"
topics: [netze]
age: Sek I
reference: [netzwerk, http, json, terminal]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# TCP/IP-Verbindungen

<p class='abstract'>
Wie findet ein Paket seinen Weg durchs Internet, und wie wird aus einzelnen Paketen eine Verbindung, über die eine ganze Webseite kommt? Du findest die Adresse deines Workspace heraus, misst mit <code>ping</code>, wie weit Server entfernt sind, und verfolgst mit <code>traceroute</code> den Weg der Pakete. Eine Schicht tiefer siehst du, wie ein Paket im eigenen Netz mit MAC-Adressen und ARP das richtige Gerät findet und was Switches und Router damit tun. Dann siehst du, wie TCP eine Verbindung aufbaut, chattest mit <code>netcat</code> und holst eine Webseite von 1992, indem du HTTP von Hand tippst. Zum Schluss lädst du mit <code>wget</code> und <code>curl</code> Dateien und Daten aus dem Netz.
</p>

## Vorbereitung

_include(workspace#terminal-oeffnen)

## Die eigene Adresse

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>ifconfig</code> kennen.
</div>

Um die IP-Adresse deines Workspace herauszufinden, gib den Befehl `ifconfig` ein und drücke <kbd>Enter</kbd>:

```bash
ifconfig
```

Du solltest nun folgende Ausgabe sehen:

<!-- tutorial-screenshot
# Terminal öffnen: Handbuch »Workspace«, eingebunden oben
hide-left-sidebar
terminal-open
terminal-maximize
terminal-run: clear
terminal-run: ifconfig
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ifconfig.webp' alt='Die Ausgabe von ifconfig: der Adapter eth0 mit inet 172.20.0.6, netmask, broadcast und ether, darunter der Adapter lo mit inet 127.0.0.1, jeweils mit den Zählern RX und TX.'>

Zu sehen sind zwei Netzwerkadapter:

- `eth0` (»Ethernet«), über den dein Workspace mit dem Internet verbunden ist, mit einer IP-Adresse, die mit `172.` beginnt
- `lo` (»Loopback«) mit der IP-Adresse `127.0.0.1`, der nur für Verbindungen innerhalb des Rechners da ist

<div class='hint'>
Deine IP-Adresse für <code>eth0</code> ist wahrscheinlich eine andere als die hier gezeigte. Das ist in Ordnung, es ist nur ein Beispiel. Schreib dir deine Adresse auf, du brauchst sie später noch.
</div>

Du siehst außerdem die MAC-Adresse des Adapters (`ether`), die Broadcast-Adresse (`broadcast`) und die Subnetzmaske (`netmask`) sowie die Datenmenge, die bisher angekommen (`RX`, »received«) und hinausgegangen ist (`TX`, »transmitted«).

Adressen, die mit `10.`, `172.16.` bis `172.31.` oder `192.168.` beginnen, sind **private Adressen**: Sie gelten nur innerhalb eines Netzes, und viele Netze auf der Welt benutzen dieselben. Im Internet erscheinen alle Workspaces deiner Schule unter einer gemeinsamen öffentlichen Adresse; ein Router übersetzt dazwischen. Auch dein Rechner zu Hause hat meist eine Adresse mit `192.168.`, und der Adapter heißt dort oft anders, zum Beispiel `enp0s3` oder, im WLAN, `wlp2s0`.

_task(adresse)

_task(loopback)

## Wie weit ist es?

### Andere Rechner anpingen

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>ping</code> kennen.
</div>

Ob du einen anderen Rechner erreichst, prüfst du mit `ping`, gefolgt von seiner IP-Adresse. Wir nehmen die Adresse von Googles DNS-Server:

```bash
ping 8.8.8.8
```

Anschließend solltest du eine Ausgabe wie diese sehen:

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ping 8.8.8.8
sleep: 4.5
press: Control+C
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ping-8-8-8-8.webp' alt='ping 8.8.8.8: fünf Antworten mit ttl=116 und Zeiten zwischen 12 und 18 ms, danach die Zusammenfassung mit 0 % Verlust und einem Mittelwert von knapp 14 ms.'>

`ping` schickt kleine Pakete an die Adresse (ICMP, ein Hilfsprotokoll von IP) und wartet auf die Antwort. Jede Sekunde geht ein Paket hinaus, bis du mit <kbd>Strg</kbd><kbd>C</kbd> abbrichst. Danach fasst `ping` zusammen, wie viele Pakete angekommen sind und wie lange sie im Schnitt gebraucht haben.

Die Angabe hinter `time` ist die **Roundtrip-Zeit**: wie viele Millisekunden ein Paket zum Ziel und zurück gebraucht hat. Zu Google sind es nur wenige Millisekunden. Google beantwortet die Adresse `8.8.8.8` von vielen Rechenzentren auf der Welt aus, und dir antwortet ein nahes.

_person(muuss)

Pingst du deinen eigenen Rechner an, verlassen die Pakete ihn gar nicht:

```bash
ping 127.0.0.1
```

Die Roundtrip-Zeit liegt jetzt deutlich unter einer Millisekunde:

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ping 127.0.0.1
sleep: 4.5
press: Control+C
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ping-127-0-0-1.webp' alt='ping 127.0.0.1: fünf Antworten mit ttl=64 und Zeiten um 0,06 ms.'>

Statt einer IP-Adresse kannst du auch einen Namen angeben, zum Beispiel `google.com`. `ping` fragt dann einen DNS-Server, welche IP-Adresse zu dem Namen gehört. Probier einen Namen mit der Endung `.pe`, die zu Peru gehört, gut 10 000 km von hier:

```bash
ping rubiomonocoat.pe
```

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ping rubiomonocoat.pe
sleep: 4.5
press: Control+C
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ping-rubiomonocoat.webp' alt='ping rubiomonocoat.pe: Es antwortet 188.208.37.117.static.hosted.by.combell-ops.net, fünf Antworten mit ttl=51 und Zeiten um 26 ms.'>

Die Antworten brauchen kaum länger als die von Google. Kann das sein, wenn der Server in Peru steht?

_task(entfernungen)

_task(lichtgeschwindigkeit)

_task(standort)

Die Endung eines Namens sagt nur, wo der Name registriert ist, nicht, wo der Rechner steht. Ein Name ist ein Schild; welcher Rechner dahinter antwortet, entscheidet die IP-Adresse, die DNS dazu nennt.

### Ein Server am anderen Ende der Welt

Ganz anders sieht es aus, wenn ein Server wirklich weit weg steht. Die Australian National University in Canberra betreibt ihre Webseite selbst, in Australien:

```bash
ping www.anu.edu.au
```

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ping www.anu.edu.au
sleep: 4.5
press: Control+C
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ping-anu.webp' alt='ping www.anu.edu.au: Es antwortet policyforum.net mit der Adresse 130.56.67.33, drei Antworten mit ttl=233 und Zeiten um 345 ms.'>

Jetzt braucht jedes Paket für Hin und Zurück mehr als eine Drittelsekunde. Berlin und Canberra liegen gut 16 000 km auseinander; selbst das Licht in einer Glasfaser bräuchte für Hin und Zurück mindestens 160 ms. Den Rest kosten Umwege der Kabel und die Router unterwegs. Dass sich der Rechner als `policyforum.net` meldet, ist übrigens nur ein zweiter Name desselben Servers: Die Adresse `130.56.67.33` gehört der Universität.

### Den Weg der Pakete verfolgen

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>traceroute</code> kennen.
</div>

Ein Paket springt auf seinem Weg von Router zu Router. Welche das sind, zeigt `traceroute`, gefolgt von der IP-Adresse oder dem Namen des Ziels:

```bash
traceroute -q 1 8.8.8.8
```

<div class='hint'>
Mit <code>-q 1</code> schickt <code>traceroute</code> nur ein Paket pro Schritt, das macht die Ausgabe übersichtlicher.
</div>

Anschließend solltest du eine Ausgabe wie diese sehen:

<!-- traceroute zeigt den Weg aus dem Netz des Rechners, auf dem die Bilder
     entstehen. Deshalb gibt das Bild eine Aufnahme aus einem Workspace auf
     code.hackschule.de wieder (14.10.2026, traceroute-8-8-8-8.txt): Das
     Rezept lässt traceroute diese Aufnahme ausgeben. -->
<!-- tutorial-screenshot
write-file: .tcpip/traceroute.txt <- file:traceroute-8-8-8-8.txt
terminal-run: traceroute() { cat ~/.tcpip/traceroute.txt; }
terminal-wait-for-prompt
terminal-run: clear
terminal-run: traceroute -q 1 8.8.8.8
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='traceroute-q-1-8-8-8-8.webp' alt='traceroute -q 1 8.8.8.8 mit zwölf Schritten: zwei private Adressen, ein Rechner von your-cloud.host, zwei Sterne, drei Router von hetzner.com in Falkenstein und Frankfurt, drei Adressen ohne Namen und zuletzt dns.google nach gut 11 ms.'>

Um `traceroute` zu verstehen, musst du wissen, dass jedes IP-Paket eine »Time To Live« (TTL) mitbringt. Sie sagt, über wie viele Router das Paket höchstens weitergereicht werden darf. Jeder Router zieht eins ab; wer bei null ankommt, verwirft das Paket. So kreist kein Paket ewig im Netz, wenn irgendwo ein Weg falsch eingetragen ist.

_task(ttl-drei)

<!-- after: ttl-drei -->
`traceroute` schickt zuerst ein Paket mit TTL 1 los. Der erste Router verwirft es und schickt eine Fehlermeldung zurück, und damit kennt `traceroute` seine Adresse. Dann schickt es ein Paket mit TTL 2, das beim zweiten Router hängen bleibt, und so weiter, bis ein Paket das Ziel erreicht.
<!-- /after -->

Ein Stern `*` bedeutet, dass ein Router das Paket verworfen hat, ohne sich zu melden; seine Adresse bleibt darum unbekannt. Eine ausführlichere Erklärung findest du in diesem [Blogpost](https://blog.davidvarghese.dev/posts/traceroute-explained/) von David Varghese.

_task(traceroute-erklaeren)

_task(router-zaehlen)

<div class='hint try'>
<strong>Ausprobieren:</strong> Gib einem Paket selbst eine kleine TTL mit: <code>ping -c 3 -t 3 8.8.8.8</code> schickt drei Pakete mit TTL 3. Wer antwortet jetzt, und was meldet er? Erhöhe die TTL Schritt für Schritt: Ab welcher TTL antwortet Google selbst? Kommt bei einer TTL gar nichts zurück, hat sich der Router nicht gemeldet, wie bei einem Stern in <code>traceroute</code>.
</div>

## Im eigenen Netz: Ethernet, ARP und Switches

<div class='hint books'>
In diesem Abschnitt lernst du die Befehle <code>ip link</code>, <code>ip neigh</code> und <code>ip route</code> kennen.
</div>

`traceroute` hat gezeigt, dass ein Paket von Router zu Router springt. Aber wie kommt es über ein Kabel oder durch die Luft vom einen Gerät zum nächsten? Dafür gibt es unter IP eine eigene Schicht mit eigenen Adressen.

### Die MAC-Adresse

`ifconfig` hat dir hinter `ether` schon eine zweite Adresse gezeigt. Der neuere Befehl `ip` zeigt sie auch:

```bash
ip link
```

<!-- tutorial-scope -->
<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ip link
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ip-link.webp' alt='Die Ausgabe von ip link: der Adapter lo mit link/loopback und lauter Nullen, darunter eth0 mit link/ether und einer MAC-Adresse aus sechs Hexadezimalzahlen, dahinter brd ff:ff:ff:ff:ff:ff.'>

<!-- /tutorial-scope -->

Hinter `link/ether` steht die **MAC-Adresse** von `eth0` (»Media Access Control«): sechs Bytes, als Hexadezimalzahlen mit Doppelpunkten dazwischen. Ein Netzwerkadapter in einem Laptop oder Handy bekommt sie schon in der Fabrik. Die ersten drei Bytes stehen für den Hersteller, die letzten drei vergibt der Hersteller selbst, jede Kombination nur einmal. Dein Workspace hat keine Netzwerkkarte, die man anfassen kann; seine MAC-Adresse hat sich Docker ausgedacht, als es deinen Workspace gestartet hat, und nach einem Neustart kann es eine andere sein. Selbst vergebene Adressen erkennt man an der zweiten Ziffer: 2, 6, A oder E.

`brd ff:ff:ff:ff:ff:ff` ist die **Broadcast-Adresse**: Was an sie geht, bekommen alle im Netz. Bei `lo` stehen nur Nullen, denn was über Loopback geht, verlässt den Rechner nicht.

_task(mac-adresse)

Wozu zwei Adressen? Stell dir ein Paket vor, das quer durchs Land geschickt wird. Die **IP-Adresse** ist die Anschrift auf dem Paket: Sie bleibt von der Absenderin bis zum Ziel dieselbe. Die **MAC-Adresse** ist der Zettel für die nächste Etappe, von welchem Lager zu welchem Lager der nächste Lkw fährt. In jedem Lager kommt ein neuer Zettel darauf, die Anschrift bleibt. Eine MAC-Adresse gilt darum nur im eigenen Netz, bis zum nächsten Router.

### Der Frame

Auf dem Kabel reist ein IP-Paket nicht allein. **Ethernet** packt es in einen **Frame**: vorn die MAC-Adresse des Ziels, die des Absenders und eine Zahl für das, was drinsteckt (`0x0800` für ein IPv4-Paket, `0x0806` für ARP), hinten eine Prüfsumme, an der der Empfänger erkennt, ob unterwegs ein Bit gekippt ist.

<svg viewBox="0 0 400 210" role="img" aria-label="Ein Ethernet-Frame als Reihe von Feldern: Ziel-MAC, 6 Bytes; Absender-MAC, 6 Bytes; Typ, 2 Bytes; das IP-Paket, 46 bis 1500 Bytes; Prüfsumme, 4 Bytes. Darunter ist das IP-Paket vergrößert: vorn der IP-Header mit Absender-IP, Ziel-IP und TTL, dahinter die Daten für ICMP oder TCP." style="width: 100%; max-width: 34em; display: block; margin: 1em auto; font-family: inherit; font-size: 12px;">
    <text x="2" y="22" fill="currentColor" font-weight="bold">Ethernet-Frame</text>
    <rect x="2" y="32" width="70" height="50" fill="currentColor" fill-opacity="0.12" stroke="currentColor" stroke-width="1.5" />
    <rect x="72" y="32" width="100" height="50" fill="currentColor" fill-opacity="0.12" stroke="currentColor" stroke-width="1.5" />
    <rect x="172" y="32" width="34" height="50" fill="currentColor" fill-opacity="0.12" stroke="currentColor" stroke-width="1.5" />
    <rect x="206" y="32" width="116" height="50" fill="none" stroke="currentColor" stroke-width="1.5" />
    <rect x="322" y="32" width="76" height="50" fill="currentColor" fill-opacity="0.12" stroke="currentColor" stroke-width="1.5" />
    <text x="37" y="53" text-anchor="middle" fill="currentColor">Ziel-MAC</text>
    <text x="37" y="71" text-anchor="middle" fill="currentColor" opacity="0.75">6 Bytes</text>
    <text x="122" y="53" text-anchor="middle" fill="currentColor">Absender-MAC</text>
    <text x="122" y="71" text-anchor="middle" fill="currentColor" opacity="0.75">6 Bytes</text>
    <text x="189" y="53" text-anchor="middle" fill="currentColor">Typ</text>
    <text x="189" y="71" text-anchor="middle" fill="currentColor" opacity="0.75">2</text>
    <text x="264" y="53" text-anchor="middle" fill="currentColor" font-weight="bold">IP-Paket</text>
    <text x="264" y="71" text-anchor="middle" fill="currentColor" opacity="0.75">46–1500 Bytes</text>
    <text x="360" y="53" text-anchor="middle" fill="currentColor">Prüfsumme</text>
    <text x="360" y="71" text-anchor="middle" fill="currentColor" opacity="0.75">4 Bytes</text>
    <line x1="206" y1="82" x2="90" y2="132" stroke="currentColor" stroke-width="1" stroke-dasharray="4 3" />
    <line x1="322" y1="82" x2="398" y2="132" stroke="currentColor" stroke-width="1" stroke-dasharray="4 3" />
    <rect x="90" y="132" width="160" height="72" fill="currentColor" fill-opacity="0.12" stroke="currentColor" stroke-width="1.5" />
    <rect x="250" y="132" width="148" height="72" fill="none" stroke="currentColor" stroke-width="1.5" />
    <text x="170" y="153" text-anchor="middle" fill="currentColor">IP-Header:</text>
    <text x="170" y="171" text-anchor="middle" fill="currentColor">Absender-IP, Ziel-IP,</text>
    <text x="170" y="189" text-anchor="middle" fill="currentColor">TTL und mehr</text>
    <text x="324" y="162" text-anchor="middle" fill="currentColor">Daten für</text>
    <text x="324" y="180" text-anchor="middle" fill="currentColor">ICMP oder TCP</text>
</svg>

Ein Adapter sieht sich von jedem Frame zuerst die Ziel-MAC an. Ist es nicht seine eigene und nicht die Broadcast-Adresse, wirft er den Frame weg, ohne hineinzusehen.

_person(metcalfe)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Ethernet: Distributed Packet Switching for Local Computer Networks</strong>, Robert M. Metcalfe und David R. Boggs, Communications of the ACM, 1976. Der Aufsatz, der Ethernet beschreibt: ein Kabel, an dem alle hängen, Pakete, die jeder bekommt und nur der Adressat behält, und was geschieht, wenn zwei gleichzeitig senden: <a href='https://www.cs.cornell.edu/courses/cs414/2004fa/ethernet.pdf'>als PDF</a>.</p>
</div>

### ARP: Wer hat diese Adresse?

Für einen Frame braucht dein Workspace die MAC-Adresse des Ziels. Meist kennt er aber nur dessen IP-Adresse.

_task(arp-wie)

<!-- after: arp-wie -->
Er fragt alle. Das Protokoll dafür heißt **ARP**, Address Resolution Protocol ([RFC 826](https://www.rfc-editor.org/rfc/rfc826), 1982). Der Workspace schickt einen Frame an die Broadcast-Adresse `ff:ff:ff:ff:ff:ff`, darin sinngemäß: »Wer hat `172.20.0.9`? Antwort bitte an `172.20.0.7`, MAC-Adresse `f2:fd:53:d5:5f:65`.« Alle im Netz bekommen die Frage, aber nur der Rechner mit dieser IP-Adresse antwortet, direkt an den Fragenden: »`172.20.0.9` hat `62:d2:c5:7a:c2:d7`.« Die Antwort merkt sich der Workspace in einer Tabelle, damit er nicht vor jedem Paket neu fragen muss.
<!-- /after -->

### Die Nachbarn: `ip neigh`

<div class='hint handshake'>
Hierfür brauchst du eine zweite Person mit einem eigenen Workspace.
</div>

Die Tabelle mit den MAC-Adressen, die dein Workspace schon kennt, zeigt `ip neigh` (»neighbours«, die Nachbarn). Findet mit `ifconfig` eure beiden IP-Adressen heraus und schreibt sie auf, aber schickt euch noch nichts.

_task(neigh-vorher)

<!-- after: neigh-vorher -->
Sieh nach:

```bash
ip neigh
```

Die Ausgabe sieht etwa so aus, mit anderen Adressen:

```text
172.20.0.6 dev eth0 lladdr 16:57:cd:6f:d3:96 REACHABLE
172.20.0.1 dev eth0 lladdr 72:51:dc:81:a6:37 STALE
```

Die Adresse der anderen Person steht nicht darin: Dein Workspace hatte noch keinen Grund, nach ihr zu fragen. Jede Zeile nennt eine IP-Adresse, den Adapter (`dev eth0`) und hinter `lladdr` (»link layer address«) die MAC-Adresse. `REACHABLE` heißt, der Nachbar hat sich vor Kurzem gemeldet; `STALE`, dass das schon eine Weile her ist und dein Workspace beim nächsten Paket an ihn nachprüft. Welche Zeilen bei dir stehen, hängt davon ab, mit wem dein Workspace zuletzt geredet hat.
<!-- /after -->

Jetzt schickt eine von euch, Person A, drei Pakete an Person B, mit der IP-Adresse von B, zum Beispiel:

```bash
ping -c 3 172.20.0.9
```

`-c 3` heißt: drei Pakete, dann hört `ping` von selbst auf. Danach sieht A wieder mit `ip neigh` nach:

```text
172.20.0.9 dev eth0 lladdr 62:d2:c5:7a:c2:d7 REACHABLE
172.20.0.6 dev eth0 lladdr 16:57:cd:6f:d3:96 REACHABLE
172.20.0.1 dev eth0 lladdr 72:51:dc:81:a6:37 STALE
```

Oben steht eine neue Zeile: die IP-Adresse von B mit einer MAC-Adresse. B prüft mit `ip link`, ob es die eigene ist.

_task(neigh-gegenseite)

<!-- after: neigh-gegenseite -->
A steht jetzt auch bei B. Die Frage von A kam mit der IP-Adresse und der MAC-Adresse von A, und B hat sich beide gleich gemerkt; ohne sie hätte B gar nicht antworten können. So steht es auch in RFC 826. Bei B zeigt `ip neigh` jetzt zum Beispiel:

```text
172.20.0.7 dev eth0 lladdr f2:fd:53:d5:5f:65 REACHABLE
```
<!-- /after -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Was geschieht, wenn niemand antwortet? Ändere die letzte Zahl deiner IP-Adresse in <code>250</code> und schick drei Pakete dorthin, etwa <code>ping -c 3 172.20.0.250</code>; antwortet doch jemand, nimm eine andere Zahl. Von welcher Adresse kommt die Meldung <code>Destination Host Unreachable</code>? Und was zeigt <code>ip neigh</code> danach für diese Adresse?
</div>

### Und Google?

Bisher ging es um Nachbarn im eigenen Netz. Was aber ist mit Servern im Internet?

_task(neigh-google)

<!-- after: neigh-google -->
Für `8.8.8.8` gibt es keine Zeile, auch nicht für `rubiomonocoat.pe` oder Canberra. Wohin dein Workspace ein Paket schickt, steht in seiner **Routing-Tabelle**:

```bash
ip route
```

<!-- tutorial-scope -->
<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ip route
terminal-wait-for-prompt
terminal-run: ip neigh show 8.8.8.8
terminal-wait-for-prompt
terminal-run: ip neigh show 172.20.0.1
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ip-route.webp' alt='ip route zeigt zwei Zeilen: default via 172.20.0.1 dev eth0 und 172.20.0.0/16 dev eth0 proto kernel scope link src mit der eigenen Adresse. Danach gibt ip neigh show 8.8.8.8 nichts aus, und ip neigh show 172.20.0.1 zeigt eine Zeile mit der MAC-Adresse des Routers.'>

<!-- /tutorial-scope -->

Die zweite Zeile im Bild sagt: Alle Adressen von `172.20.0.0` bis `172.20.255.255` liegen im eigenen Netz, hinter `eth0` (`/16` heißt: Die ersten 16 Bits sind bei allen gleich). Für sie fragt dein Workspace mit ARP direkt nach der MAC-Adresse. Die erste Zeile gilt für alles andere: `default via 172.20.0.1`, ab zum Router. Für ein Paket an `8.8.8.8` fragt dein Workspace darum nach der MAC-Adresse des Routers und schickt den Frame an ihn; im IP-Paket darin steht weiter `8.8.8.8` als Ziel. Mit einer Adresse dahinter zeigt `ip neigh show` nur die Zeile zu dieser Adresse: Für `8.8.8.8` gibt es keine, für den Router schon. Bei dir können die Zahlen andere sein. Der Router hinter `default via` ist der Server, auf dem die Workspaces laufen.
<!-- /after -->

### Switches

In einem Klassenraum hängen die Rechner nicht an einem gemeinsamen Kabel wie bei Metcalfe, sondern jeder mit einem eigenen Kabel an einem **Switch**, einem Kasten mit vielen Buchsen, den **Ports**. Der Switch liest in jedem Frame die MAC-Adressen und schickt ihn nur an den Port, an dem das Ziel hängt. Woher weiß er das? Er führt eine Tabelle aus MAC-Adressen und Ports, und wenn er eingeschaltet wird, ist sie leer.

_task(switch-leer)

<!-- after: switch-leer -->
Drei Rechner hängen an einem Switch, der gerade eingeschaltet wurde: A an Port 1, B an Port 2 und C an Port 3. A will B anpingen und kennt die MAC-Adresse von B noch nicht.

<table class='table'>
<tr><th>Frame</th><th>Was der Switch tut</th><th>Seine Tabelle danach</th></tr>
<tr><td>A an alle (<code>ff:ff:ff:ff:ff:ff</code>): »Wer hat die IP-Adresse von B?«</td><td>Ein Broadcast geht an alle anderen Ports, an 2 und 3. Er merkt sich: A hängt an Port 1.</td><td>A: Port 1</td></tr>
<tr><td>B an A: »Die habe ich.«</td><td>A kennt er: nur an Port 1. Er merkt sich: B hängt an Port 2.</td><td>A: Port 1<br>B: Port 2</td></tr>
<tr><td>A an B: das erste <code>ping</code></td><td>B kennt er: nur an Port 2. C bekommt davon nichts mit.</td><td>A: Port 1<br>B: Port 2</td></tr>
<tr><td>A an C</td><td>C kennt er noch nicht: an alle anderen Ports, an 2 und 3, wie einen Broadcast. B wirft den Frame weg.</td><td>A: Port 1<br>B: Port 2</td></tr>
</table>

Der Switch lernt nur aus den Absender-Adressen der Frames, die bei ihm ankommen; was er nicht kennt, schickt er an alle. Kommt von einer Adresse ein paar Minuten lang nichts, vergisst er sie wieder, und steckt jemand seinen Rechner um, lernt er den neuen Port mit dem nächsten Frame.

Auch dein Workspace hängt an einem Switch. Docker hat auf dem Server einen Switch aus Software gebaut, eine Linux-Bridge, und jeder Workspace steckt mit einem virtuellen Kabel darin. Ihre Tabelle siehst du von deinem Workspace aus nicht; auf dem Server zeigt sie der Befehl `bridge fdb`, mit den MAC-Adressen der Container, die in den letzten fünf Minuten etwas geschickt haben.
<!-- /after -->

### WLAN: Alle hören alles

Im WLAN gibt es kein Kabel und keinen Port, an den ein Frame allein geht. Jeder Adapter in Reichweite empfängt jeden Frame auf seinem Kanal, so wie die Rechner an Metcalfes Kabel. Auch hier trägt jeder Frame MAC-Adressen, und ein Adapter wirft weg, was nicht an ihn geht. Ein Laptop kann das aber auch lassen und alles mitlesen, was in der Luft ist. Darum verschlüsseln WPA2 und WPA3 den Inhalt jedes Frames. Wer mithört, sieht dann noch, welche MAC-Adressen miteinander reden und wie viel, aber nicht mehr, was sie sich schicken. Der Access Point reicht die Frames zwischen Funk und Kabel weiter, ähnlich wie ein Switch.

### Router: neue MAC-Adressen bei jedem Sprung

Zurück zu `traceroute`: Ein Paket an `8.8.8.8` kam über elf Router. Jeder davon packt den Frame aus, liest im IP-Paket die Ziel-IP, zieht eins von der TTL ab und sucht in seiner Routing-Tabelle den nächsten Router. Dessen MAC-Adresse kennt er schon oder fragt mit ARP nach. Dann packt er das Paket in einen neuen Frame, mit seiner eigenen MAC-Adresse als Absender und der des nächsten Routers als Ziel. Zwischen zwei großen Routern läuft vielleicht kein Ethernet, sondern ein anderes Verfahren über Glasfaser, aber immer gilt: neuer Frame, dasselbe Paket. Die IP-Adressen darin bleiben, nur die TTL wird kleiner.

Eine Ausnahme kennst du schon: Der Router, der die privaten Adressen übersetzt, tauscht auch die Absender-IP gegen eine öffentliche Adresse aus, und auf dem Rückweg wieder zurück.

_task(router-tauscht)

_include(netzwerk#netz-lan)

## Pakete, Ports und Verbindungen

### Schichten

Bisher ging es um Pakete, die von einer Adresse zu einer anderen reisen. Darum kümmert sich **IP**, das Internet Protocol: Es reicht ein Paket von Router zu Router bis zum Ziel, aber ohne Garantie. Ein Paket kann verloren gehen, doppelt ankommen oder ein späteres überholen. Für eine Webseite, eine Datei oder einen Chat reicht das nicht. Darum arbeiten mehrere Protokolle übereinander, und jedes hat eine Aufgabe:

<table class='table'>
<tr><th class='text-nowrap'>Schicht</th><th>Aufgabe</th><th>Wer ist gemeint?</th><th>Beispiele</th></tr>
<tr><td class='text-nowrap'>Anwendung</td><td>Was die Programme einander sagen</td><td>ein Pfad, ein Name: <code>/index.html</code></td><td>HTTP, SSH, DNS</td></tr>
<tr><td class='text-nowrap'>Transport</td><td>Daten vollständig und in der richtigen Reihenfolge zum richtigen Programm bringen</td><td>ein Port: <code>443</code></td><td>TCP, UDP</td></tr>
<tr><td class='text-nowrap'>Internet</td><td>Pakete über viele Router zum richtigen Rechner bringen</td><td>eine IP-Adresse: <code>172.20.0.6</code></td><td>IP, ICMP</td></tr>
<tr><td class='text-nowrap'>Netzzugang</td><td>Ein Stück Weg bis zum nächsten Gerät, über Kabel oder Funk</td><td>eine MAC-Adresse: <code>d2:db:7c:26:20:29</code></td><td>Ethernet, WLAN</td>
</tr>
</table>

Jede Schicht benutzt die darunter und weiß nichts von denen darüber: IP trägt ein Paket zum Ziel, ohne zu wissen, dass ein Stück einer Webseite darin steckt. Zwei dieser Adressen kennst du schon: die MAC-Adresse aus `ip link` und die IP-Adresse aus `ifconfig`. Die beiden mittleren Schichten geben dem Ganzen seinen Namen: **TCP/IP**.

_person(cerf-kahn)

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>A Protocol for Packet Network Intercommunication</strong>, Vinton G. Cerf und Robert E. Kahn, IEEE Transactions on Communications, 1974. Der Aufsatz, mit dem TCP/IP anfing: wie ein Gateway Pakete zwischen ganz verschiedenen Netzen weiterreicht; TCP und IP sind darin noch ein einziges Protokoll: <a href='https://www.cs.princeton.edu/courses/archive/fall06/cos561/papers/cerf74.pdf'>als PDF</a>.</p>
</div>

_task(schichten)

### Ports

Auf einem Rechner laufen viele Programme, die das Netz benutzen, und sie alle teilen sich seine IP-Adresse. Überlege, bevor du weiterliest:

_task(port)

<!-- after: port -->
Die IP-Adresse bringt ein Paket zum richtigen Rechner, der **Port** zum richtigen Programm: eine Zahl von 1 bis 65 535. Server warten an Ports, die alle kennen: 22 für SSH, 80 für HTTP, 443 für HTTPS. Die Seite, die du gerade liest, kam über Port 443. Der Client bekommt für jede Verbindung irgendeinen freien Port; an ihn gehen die Antworten zurück.
<!-- /after -->

### Der Handshake

Bevor über TCP Daten fließen, bauen Client und Server eine **Verbindung** auf, mit drei Paketen, dem **Handshake**:

<svg viewBox="0 0 440 255" role="img" aria-label="Zwei senkrechte Linien, links der Client, rechts der Server. Drei schräge Pfeile: SYN vom Client zum Server, SYN-ACK zurück, ACK wieder zum Server, dicht gefolgt von den ersten Daten, GET. Eine Klammer links markiert SYN und SYN-ACK als ein Hin und Zurück." style="width: 100%; max-width: 34em; display: block; margin: 1em auto; font-family: inherit; font-size: 14px;">
    <defs>
        <marker id="handshake-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerUnits="userSpaceOnUse" markerWidth="11" markerHeight="11" orient="auto">
            <path d="M0,0 L10,5 L0,10 z" fill="currentColor" />
        </marker>
    </defs>
    <text x="120" y="20" text-anchor="middle" fill="currentColor" font-weight="bold">Client</text>
    <text x="380" y="20" text-anchor="middle" fill="currentColor" font-weight="bold">Server</text>
    <line x1="120" y1="30" x2="120" y2="250" stroke="currentColor" stroke-width="2" />
    <line x1="380" y1="30" x2="380" y2="250" stroke="currentColor" stroke-width="2" />
    <line x1="120" y1="45" x2="380" y2="95" stroke="currentColor" stroke-width="1.5" marker-end="url(#handshake-arrow)" />
    <text x="250" y="63" transform="rotate(10.89 250 70)" text-anchor="middle" fill="currentColor">SYN: »Hallo?«</text>
    <line x1="380" y1="100" x2="120" y2="150" stroke="currentColor" stroke-width="1.5" marker-end="url(#handshake-arrow)" />
    <text x="250" y="118" transform="rotate(-10.89 250 125)" text-anchor="middle" fill="currentColor">SYN-ACK: »Hallo, ich höre dich.«</text>
    <line x1="120" y1="155" x2="380" y2="205" stroke="currentColor" stroke-width="1.5" marker-end="url(#handshake-arrow)" />
    <text x="250" y="173" transform="rotate(10.89 250 180)" text-anchor="middle" fill="currentColor">ACK: »Ich dich auch.«</text>
    <line x1="120" y1="175" x2="380" y2="225" stroke="currentColor" stroke-width="3" marker-end="url(#handshake-arrow)" />
    <text x="250" y="218" transform="rotate(10.89 250 200)" text-anchor="middle" fill="currentColor" font-weight="bold">Daten: GET / …</text>
    <path d="M105,45 L95,45 L95,150 L105,150" fill="none" stroke="currentColor" stroke-width="1.5" />
    <text x="85" y="94" text-anchor="end" fill="currentColor">ein Hin</text>
    <text x="85" y="112" text-anchor="end" fill="currentColor">und Zurück</text>
</svg>

Danach nummeriert TCP jedes Byte, das es schickt, und die Gegenseite bestätigt, was angekommen ist. Was nicht bestätigt wird, schickt TCP noch einmal, und was in falscher Reihenfolge ankommt, sortiert es. So kommt alles vollständig und in der richtigen Reihenfolge an, auch wenn IP unterwegs Pakete verliert.

Der Handshake kostet Zeit: Bevor das erste Byte Daten unterwegs ist, muss ein Paket einmal hin und zurück. Das kannst du messen. Mit `-w` sagt `curl`, wie lange die einzelnen Schritte gedauert haben:

```bash
curl -so /dev/null -w 'Verbindung: %{time_connect} s\n' https://www.anu.edu.au
```

`-s` macht `curl` still, und `-o /dev/null` wirft die Seite weg; uns interessiert nur die Zeit. Überleg vorher, was herauskommen müsste, und führe den Befehl dann zweimal aus. Beim ersten Mal kommt noch die Zeit dazu, in der der Name nachgeschlagen wird.

_task(handshake)

<!-- after: handshake -->
<!-- tutorial-screenshot
terminal-run: clear
terminal-run: curl -so /dev/null -w 'Verbindung: %{time_connect} s\n' https://www.anu.edu.au
terminal-wait-for-prompt
terminal-run: curl -so /dev/null -w 'Verbindung: %{time_connect} s\n' https://www.anu.edu.au
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='curl-handshake.webp' alt='Zweimal curl mit -w: Die Verbindung zu www.anu.edu.au steht jeweils nach etwa 0,34 Sekunden, so lange wie ein ping.'>

Die Verbindung steht nach etwa 0,34 Sekunden, so lange wie ein `ping` nach Canberra. Bei `https://` folgt danach noch die Verschlüsselung, mit weiteren Paketen hin und zurück. Darum fühlt sich ein Server am anderen Ende der Welt so viel langsamer an, als 0,3 Sekunden vermuten lassen.
<!-- /after -->

## Eine eigene Verbindung: `netcat`

<div class='hint books'>
In diesem Abschnitt lernst du die Befehle <code>netcat</code> und <code>ss</code> kennen.
</div>

`netcat` (kurz `nc`) baut eine TCP-Verbindung auf und tut sonst nichts: Was du tippst, geht hinüber, was ankommt, steht im Terminal. Eine Seite wartet als **Server** an einem Port auf eine Verbindung, die andere verbindet sich als **Client**.

### Allein, mit zwei Terminals

Starte im Terminal einen Server, der an Port 1234 wartet. `-l` steht für »listen«, `-p 1234` für den Port:

```bash
nc -l -p 1234
```

Öffne mit <kbd>Strg</kbd><kbd>Shift</kbd><kbd>5</kbd> ein zweites Terminal daneben. Dort zeigt `ss`, welche Ports auf deinem Rechner auf Verbindungen warten (`-t` TCP, `-l` nur wartende, `-n` Zahlen statt Namen):

```bash
ss -tln
```

```text
State   Recv-Q  Send-Q  Local Address:Port   Peer Address:Port
LISTEN  0       511           0.0.0.0:8443        0.0.0.0:*
LISTEN  0       1             0.0.0.0:1234        0.0.0.0:*
LISTEN  0       4096       127.0.0.11:46137       0.0.0.0:*
```

Port 1234 steht jetzt in der Liste, das ist dein `nc`. Port 8443 ist VS Code, das du gerade im Browser benutzt; die Zeile mit `127.0.0.11` gehört zum Namensdienst des Workspace, ihr Port ist bei dir ein anderer. `0.0.0.0` heißt: Verbindungen über jeden Adapter sind willkommen. Verbinde dich nun als Client mit deinem eigenen Rechner:

```bash
nc localhost 1234
```

`localhost` ist der Name für `127.0.0.1`. Tippe eine Nachricht und drücke <kbd>Enter</kbd>: Sie erscheint links. Klick ins linke Terminal und antworte:

<!-- tutorial-screenshot
# netcat allein: links der Server an Port 1234, rechts ss und der Client
terminal-run: clear
terminal-run: nc -l -p 1234
sleep: 1
press: Control+Shift+5
sleep: 1
terminal-run: nc localhost 1234
sleep: 1
type: Hallo, ist da jemand?
press: Enter
sleep: 1
press: Alt+ArrowLeft
sleep: 1
type: Ja, hier links!
press: Enter
sleep: 2
-->

<img class='full' src='nc-allein.webp' alt='Zwei Terminals nebeneinander. Links wartet nc -l -p 1234 und zeigt »Hallo, ist da jemand?« und die Antwort »Ja, hier links!«. Rechts verbindet sich nc localhost 1234 und zeigt dieselben zwei Zeilen.'>

Mit <kbd>Strg</kbd><kbd>C</kbd> in einem der beiden Terminals endet die Verbindung, und beide `nc` hören auf.

### Zu zweit

<div class='hint handshake'>
Hierfür brauchst du eine zweite Person mit einem eigenen Workspace.
</div>

Findet mit `ifconfig` eure beiden IP-Adressen heraus. Eine Person startet den Server:

```bash
nc -l -p 1234
```

Die andere verbindet sich mit der Adresse des Servers, zum Beispiel:

```bash
nc 172.20.0.12 1234
```

Jetzt könnt ihr chatten. Kommt keine Verbindung zustande, prüft die Adresse und ob der Server schon wartet, bevor sich der Client verbindet.

<div class='hint'>
Port <code>1234</code> hat im Workspace eine besondere Rolle: Er ist der einzige TCP-Port, der für direkte Verbindungen zwischen zwei Workspaces freigeschaltet ist. Andere Ports sowie UDP zwischen Workspaces sind nicht erreichbar. Browser-Anwendungen teilt ihr stattdessen über Shared Live Apps im <a href='/profil' target='_blank'>Profil</a>.
</div>

Eigentlich ist `netcat` nicht zum Chatten gedacht. Es schickt Daten ohne weiteres Protokoll und ohne Verschlüsselung von einem Rechner zum anderen, und genau das macht es so nützlich, um auszuprobieren, was andere Programme über eine Verbindung schicken.

## HTTP von Hand

Wenn dein Browser eine Webseite holt, macht er nichts anderes: Er baut eine TCP-Verbindung zu Port 80 oder 443 auf und schreibt Text hinein, nach den Regeln von **HTTP**. Das kannst du auch. Am CERN in Genf, wo das World Wide Web erfunden wurde, liegt die allererste Webseite noch heute auf dem Server `info.cern.ch`, ohne Verschlüsselung an Port 80. Verbinde dich:

```bash
nc -C info.cern.ch 80
```

`-C` beendet jede Zeile so, wie HTTP es verlangt, mit `\r\n`; ohne `-C` antwortet der Server mit `400 Bad Request`. Tippe dann diese zwei Zeilen und danach eine leere Zeile, also noch einmal <kbd>Enter</kbd>:

```text
HEAD /hypertext/WWW/TheProject.html HTTP/1.0
Host: info.cern.ch
```

`HEAD` fragt nur nach den Headern der Seite, `Host` sagt, welche Website auf dem Server gemeint ist, und die leere Zeile sagt: »Fertig, du bist dran.«

<!-- tutorial-screenshot
press: Control+Shift+P
type: Terminal: Kill All Terminals
press: Enter
sleep: 2
press: Control+Shift+Backquote
sleep: 3
terminal-maximize
terminal-wait-for-prompt
terminal-run: clear
terminal-run: nc -C info.cern.ch 80
sleep: 1
type: HEAD /hypertext/WWW/TheProject.html HTTP/1.0
press: Enter
type: Host: info.cern.ch
press: Enter
press: Enter
wait-for-text: Content-Type
sleep: 1
crop-terminal-lines: auto
-->

<img class='full' src='http-von-hand.webp' alt='Nach nc -C info.cern.ch 80 und den getippten Zeilen HEAD /hypertext/WWW/TheProject.html HTTP/1.0 und Host: info.cern.ch antwortet der Server mit HTTP/1.1 200 OK, Server: Apache, Last-Modified: Thu, 03 Dec 1992, Content-Length: 2217 und Content-Type: text/html.'>

Die Antwort beginnt mit der **Status-Line** `HTTP/1.1 200 OK`: Die Seite gibt es. Danach kommen Header, zum Beispiel `Last-Modified`: Diese Seite wurde zuletzt im Dezember 1992 geändert. `nc` wartet danach weiter auf deine Eingabe; beende es mit <kbd>Strg</kbd><kbd>C</kbd>.

_person(berners-lee)

Hol jetzt die Seite selbst: dieselben Zeilen, nur mit `GET` statt `HEAD`. Nach der leeren Zeile unter den Headern kommt diesmal das HTML der ersten Webseite der Welt.

_uses(http-anfrage, http-antwort)

_task(erste-webseite)

_task(leere-zeile)

<div class='hint try'>
<strong>Ausprobieren:</strong> Schick <code>info.cern.ch</code> Anfragen, die nicht ganz stimmen, jede über eine neue Verbindung mit <code>nc -C info.cern.ch 80</code> und mit einer leeren Zeile am Ende:
<ul>
<li>einen Pfad, den es nicht gibt: <code>HEAD /missing.html HTTP/1.0</code></li>
<li>eine Methode, die es nicht gibt: <code>HAED /hypertext/WWW/TheProject.html HTTP/1.0</code></li>
<li><code>HTTP/1.1</code> statt <code>HTTP/1.0</code>, aber ohne die Zeile <code>Host:</code></li>
</ul>
Welche Status-Line kommt jeweils zurück? Warum besteht ein Server bei <code>HTTP/1.1</code> auf dem <code>Host</code>-Header?
</div>

### Die TCP-Konsole

Bei `https://` liegt zwischen TCP und HTTP noch eine Schicht, **TLS**, die alles verschlüsselt. Mit `nc` kommst du da nicht mehr hinein. Die [TCP-Konsole](/tcp-console) kann es: Sie macht dasselbe wie `nc -C`, nur im Browser, mit einem Schalter für TLS, und sie zeigt, was du geschickt hast, was zurückkam und wann. Die Verbindung beginnt dabei in deinem Workspace, so wie bei `nc`.

_tool(tcp-console)

Öffne die Konsole, klicke auf »example.com, Port 443 mit TLS« und dann auf »Verbinden«. Tippe danach wieder eine Anfrage:

```text
HEAD / HTTP/1.1
Host: example.com
```

und eine leere Zeile. Die Konsole verschlüsselt jede Zeile, bevor sie zum Server geht, und entschlüsselt die Antwort.

<!-- tutorial-screenshot
# TCP-Konsole: HEAD an example.com über TLS
open-site: /tcp-console
tab: preview
viewport: 1029x1300
click: example.com, Port 443 mit TLS
click: selector:#tcp-connect-button
wait-for-text: verschlüsselt mit
click: selector:#tcp-line
type: HEAD / HTTP/1.1
press: Enter
type: Host: example.com
press: Enter
press: Enter
wait-for-text: HTTP/1.1 200
sleep: 2
crop-element: #tcp-log
-->
<img class='full' src='tcp-konsole.webp' alt='Die TCP-Konsole wie ein Chat: oben die Uhrzeit und »Verbunden mit example.com, Port 443, verschlüsselt mit TLSv1.3«. Rechts in einer blauen Sprechblase die Anfrage, HEAD / HTTP/1.1, Host: example.com und eine leere Zeile, jede Zeile mit einem Pfeil für CR LF. Links in einer grauen Sprechblase die Antwort: HTTP/1.1 200 OK und eine Reihe Header. Unten in jeder Sprechblase die Uhrzeit.'>

Deine Anfrage steht in einer Sprechblase, jede Zeile mit einem Pfeil für das CR LF an ihrem Ende, die leere Zeile als Pfeil allein. Die Antwort des Servers hat dieselbe Form: Status-Line, Header, leere Zeile. Diesmal schließt der Server die Verbindung nicht gleich: Bei `HTTP/1.1` bleibt sie offen, für die nächste Anfrage. Schick noch eine, oder warte und lies an den Uhrzeiten ab, wie lange der Server wartet, bevor er auflegt.

### Wer zu langsam tippt

Verbinde dich jetzt mit `www.wikipedia.org`, Port 80, ohne TLS, und lass dir Zeit mit der ersten Zeile.

_task(zu-langsam)

<!-- after: zu-langsam -->
Ein Server hält nicht jede Verbindung ewig offen. Jede offene Verbindung kostet ihn Speicher, und wer tausend Verbindungen öffnet und nie etwas schickt, könnte ihn sonst lahmlegen. Darum schließt er Verbindungen, über die eine Weile nichts kommt. Wie lange er wartet, legt jeder Server selbst fest: `www.wikipedia.org` nur wenige Sekunden, `example.com` etwa 15 Sekunden, `info.cern.ch` fast eine Minute. Wer trotzdem in Ruhe tippen möchte, schreibt die Anfrage vorher hin und fügt sie auf einmal ein; die Konsole schickt mehrere Zeilen nacheinander ab.
<!-- /after -->

## Daten aus dem Netz holen

### Dateien herunterladen: `wget`

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>wget</code> kennen.
</div>

Um Dateien aus dem Internet herunterzuladen, kannst du den Befehl `wget` verwenden, gefolgt von der Adresse der Datei:

```bash_copy
wget https://github.com/begilbert-sys/2048-in-C/raw/main/main.c
```

Du solltest sehen, wie die Datei `main.c` heruntergeladen wird:

<!-- tutorial-screenshot
press: Control+C
terminal-wait-for-prompt
terminal-run: clear
terminal-run: wget https://github.com/begilbert-sys/2048-in-C/raw/main/main.c
terminal-wait-for-prompt
wait-for-file: main.c
crop-terminal-lines: auto
-->

<img class='full' src='wget-2048.webp' alt='wget löst github.com auf, verbindet sich mit Port 443, bekommt 302 Found mit einer neuen Adresse bei raw.githubusercontent.com, folgt ihr, bekommt 200 OK und speichert main.c mit 10096 Bytes.'>

Auch hier siehst du die Schritte von eben: Den Namen nachschlagen (»Resolving«), die Verbindung zu Port 443 aufbauen (»Connecting … connected«), die Anfrage schicken. GitHub antwortet zuerst mit `302 Found`: Die Datei liegt unter einer anderen Adresse, und `wget` folgt ihr.

Du solltest jetzt eine Datei `main.c` in deinem aktuellen Verzeichnis haben (`ls -l`):

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls-2048.webp' alt='ls -l zeigt die Datei main.c mit 10096 Bytes.'>

Es handelt sich um ein Mini-Spiel, das in der [Programmiersprache C](/c) programmiert worden ist. Du kannst es mit dem Befehl `gcc` kompilieren und anschließend ausführen.

```bash
gcc main.c -o 2048 -lncurses -lm
```

<div class='hint'>
Das Spiel benötigt die Bibliotheken <code>ncurses</code> und <code>math</code>, weshalb wir dem Compiler mit den Optionen <code>-lncurses</code> und <code>-lm</code> mitteilen, dass er diese Bibliotheken einbinden soll.
</div>

Danach liegt neben `main.c` das Programm `2048`:

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: gcc main.c -o 2048 -lncurses -lm
terminal-wait-for-prompt
wait-for-file: 2048
terminal-run: ls -l
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='ls-2048-bin.webp' alt='Nach gcc main.c -o 2048 -lncurses -lm zeigt ls -l das ausführbare Programm 2048 neben main.c.'>

Das Spiel ist jetzt kompiliert und kann mit dem Befehl `./2048` ausgeführt werden:

```bash
./2048
```

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: ./2048
sleep: 1.5
press: ArrowLeft
sleep: 0.3
press: ArrowUp
sleep: 0.3
press: ArrowRight
sleep: 1
crop-terminal-lines: 99
-->

<img class='full' src='2048.webp' alt='Das Spiel 2048 läuft als vier mal vier großes Zahlenraster im Terminal.'>

Du kannst das Spiel mit den Pfeiltasten steuern und mit <kbd>Strg</kbd><kbd>C</kbd> beenden.

_task(spiel-2048)

### Webseiten und APIs: `curl`

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>curl</code> kennen.
</div>

Auch `curl` lädt Dateien herunter, gibt sie aber im Terminal aus, statt sie zu speichern. Es spricht HTTP für dich, auch verschlüsselt, und eignet sich darum gut, um mit Webseiten und APIs zu reden. Hol eine Webseite, wie es auch ein Browser tun würde:

```bash
curl -s https://gymnasiumsteglitz.de | less
```

`-s` schaltet die Fortschrittsanzeige ab. Mit den Pfeiltasten scrollst du durch die Seite, <kbd>Q</kbd> beendet `less`.

<!-- tutorial-screenshot
press: Control+C
terminal-wait-for-prompt
terminal-run: clear
terminal-run: curl -s https://gymnasiumsteglitz.de | less
wait-for-text: html
sleep: 1
crop-terminal-lines: 99
-->

<img class='full' src='curl-gyst.webp' alt='less zeigt den Anfang des HTML der Website des Gymnasiums Steglitz: doctype, head, meta-Angaben, der Titel und die eingebundenen Stylesheets und Skripte.'>

Mit `-I` fragt `curl` wie eben `HEAD` und zeigt nur die Header der Antwort:

```bash
curl -I https://gymnasiumsteglitz.de
```

<!-- tutorial-screenshot
press: q
terminal-wait-for-prompt
terminal-run: clear
terminal-run: curl -I https://gymnasiumsteglitz.de
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='curl-i-gyst.webp' alt='curl -I zeigt HTTP/2 200 und die Header der Antwort, darunter content-length, cache-control, content-type text/html und server Apache.'>

`curl -v` zeigt außerdem alles, was `curl` selbst schickt: dieselben Zeilen, die du eben von Hand getippt hast.

_uses(http-header, curl)

_task(webserver)

<div class='hint try'>
<strong>Ausprobieren:</strong> Dreh es um und sieh dir an, was <code>curl</code> schickt. Starte in einem Terminal <code>nc -C -l -p 1234</code> und im zweiten <code>curl http://localhost:1234/hallo</code>. Was steht im ersten Terminal? <code>curl</code> wartet jetzt auf eine Antwort: Tippe im ersten Terminal <code>HTTP/1.0 200 OK</code>, eine leere Zeile und <code>Hallo zurück!</code> und beende <code>nc</code> dann mit <kbd>Strg</kbd><kbd>C</kbd>. Was gibt <code>curl</code> aus? Gerade warst du ein Webserver.
</div>

_person(stenberg)

Eine API ist eine Adresse, die keine Seite für Menschen liefert, sondern Daten für Programme. Die »Cat Facts«-API zum Beispiel gibt zufällige Fakten über Katzen zurück:

```bash
curl https://catfact.ninja/fact
```

Die Ausgabe ist etwas schwer zu lesen:

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: curl https://catfact.ninja/fact
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='curl-catfact.webp' alt='curl gibt den Katzenfakt als JSON in einer einzigen Zeile aus, mit den Feldern fact und length.'>

Die Antwort ist im JSON-Format. Dieses Format eignet sich hervorragend, um es in einem Programm oder Skript zu verarbeiten. `jq` gibt JSON übersichtlich und farbig aus:

```bash
curl -s https://catfact.ninja/fact | jq
```

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: curl -s https://catfact.ninja/fact | jq
terminal-wait-for-prompt
crop-terminal-lines: auto
-->

<img class='full' src='curl-catfact-jq.webp' alt='Eine Antwort derselben API, diesmal mit einem anderen Fakt, von jq eingerückt und farbig: ein Objekt mit den Feldern fact und length.'>

_uses(json-aufbau, jq)

Du kannst die Abfrage beliebig oft wiederholen und solltest jedesmal einen zufälligen, neuen Fakt über Katzen erhalten.

_task(katzenfakt)

Es gibt viele APIs, die du mit `curl` abfragen kannst. Manche sind frei zugänglich, andere verlangen einen API-Schlüssel. Eine lange Liste hat jemand unter [Public APIs](https://github.com/public-apis/public-apis) gesammelt. Wie man eine API aus einem Programm und einer Webseite benutzt, zeigt [Daten aus dem Netz](/wetter) am Beispiel eines Wetterdienstes.

## Zum Ausprobieren: ASCII-Kino

<div class='hint books'>
In diesem Abschnitt lernst du den Befehl <code>telnet</code> kennen, den du aber auf keinen Fall verwenden solltest, um dich bei anderen Servern anzumelden. Wir verwenden ihn hier nur, um die ASCII-Version von Star Wars zu schauen.
</div>

Früher meldete man sich mit `telnet` bei anderen Rechnern an. Wie `netcat` öffnet `telnet` einfach eine TCP-Verbindung, unverschlüsselt: Jeder Router unterwegs könnte Passwörter mitlesen. Heute nimmt man dafür SSH. Einen Zweck hat `telnet` aber noch, die ASCII-Version von Star Wars:

```bash
telnet towel.blinkenlights.nl
```

<!-- tutorial-screenshot
terminal-run: clear
terminal-run: telnet towel.blinkenlights.nl
sleep: 25
crop-terminal-lines: 99
-->

<img class='full' src='starwars.webp' alt='Eine Star-Wars-Szene aus Textzeichen im Terminal.'>

<!-- tutorial-screenshot
# Die Mülltonne über dem Terminal, mit Mauszeiger und Tooltip; nur die
# Titelleiste des Terminals und der Tooltip, der Rest transparent
move-mouse: selector:.part.panel [aria-label*="Kill Terminal"] >> nth=0
sleep: 1.5
show-mouse
# the terminal is maximised here: its title bar is at the top
crop-left: 68%
crop-bottom: 76%
keep-only: .part.panel > .composite.title
keep-only: .workbench-hover
keep-only: .workbench-hover-pointer
-->

<img src='terminal-kill.webp' class='r' style='width: 25em;' alt='Der Knopf mit der Mülltonne über dem Terminal, der Mauszeiger darauf, darunter der Hinweis »Kill Terminal«.'>

Da `telnet` etwas umständlich zu beenden ist, drücke einfach auf die Mülltonne in der rechten oberen Ecke des Terminals, um das Terminal zu schließen, und anschließend auf das Pluszeichen, um ein neues Terminal zu öffnen.

<div style='clear: both;'></div>

## Zusammenfassung

<table class='table table-sm'>
<tr><th>Adresse</th><td><code>ifconfig</code></td></tr>
<tr><th>Entfernung und Weg</th><td><code>ping</code>, <code>traceroute -q 1</code></td></tr>
<tr><th>Im eigenen Netz</th><td><code>ip link</code> (die MAC-Adresse), <code>ip neigh</code> (die Nachbarn, die ARP gefunden hat), <code>ip route</code> (der Router)</td></tr>
<tr><th>Ports und Verbindungen</th><td><code>ss -tln</code> (wartende Ports), <code>curl -w '%{time_connect}'</code> (Dauer des Handshakes)</td></tr>
<tr><th>Eigene Verbindungen</th><td><code>nc -l -p 1234</code> und <code>nc ADRESSE 1234</code>, <code>nc -C info.cern.ch 80</code> (HTTP von Hand), <code>telnet</code> (unverschlüsselt, nur noch fürs Kino)</td></tr>
<tr><th>Herunterladen</th><td><code>wget</code>, <code>curl</code>, <code>curl -I</code> (nur die Header), <code>jq</code> (JSON lesbar machen)</td></tr>
</table>

## Wie geht es weiter?

In [Ein eigener Webserver](/webserver) schreibst du die andere Seite selbst: ein Programm, das wie `nc -l` an einem Port wartet, die Zeilen liest, die du hier getippt hast, und mit HTTP antwortet. In der [Pixelwand](/pixelwand) bleibt die Verbindung offen, und die ganze Klasse malt gleichzeitig an einem Bild. Wie du eine API aus Ruby und aus einer Webseite benutzt, zeigt [Daten aus dem Netz](/wetter).

<!-- tutorial-screenshot
# Bild für die Karte der Aktivität: HTTP von Hand, groß; 700 hoch, weil
# VS Code 1.140 dem Terminal weniger Platz lässt und sonst der Befehl oben
# herausscrollt
zoom: 1.5
viewport: 1029x700
press: Control+Shift+P
type: Terminal: Kill All Terminals
press: Enter
sleep: 2
press: Control+Shift+Backquote
sleep: 3
terminal-maximize
terminal-wait-for-prompt
terminal-run: clear
terminal-run: nc -C info.cern.ch 80
sleep: 1
type: HEAD /hypertext/WWW/TheProject.html HTTP/1.0
press: Enter
type: Host: info.cern.ch
press: Enter
press: Enter
wait-for-text: Content-Type
sleep: 1
crop-terminal-lines: auto
-->
<img class='teaser' hidden src='teaser.webp' alt='Ein Terminal in großer Schrift: nc -C info.cern.ch 80, die getippte Anfrage HEAD /hypertext/WWW/TheProject.html und die Antwort des Servers mit HTTP/1.1 200 OK und Last-Modified: Thu, 03 Dec 1992.'>
