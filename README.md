# Hackschule Workspace

Der Hackschule Workspace ist eine browserbasierte Entwicklungsumgebung für den Informatikunterricht. Schülerinnen und Schüler arbeiten mit einem echten Linux-System, Visual Studio Code, Terminal, Git, Compilern, Datenbanken und weiteren Werkzeugen – ohne auf dem eigenen Rechner etwas installieren zu müssen. Dateien und Einstellungen bleiben erhalten, sodass in der Schule und zu Hause dieselbe Arbeitsumgebung zur Verfügung steht.

Zum Workspace gehören außerdem zahlreiche Tutorials und Projekte, die direkt mit den vorhandenen Werkzeugen arbeiten: von den ersten Schritten mit Dateien und Git über Webentwicklung und Datenbanken bis zu Computergrafik, interaktiven Geschichten und systemnaher Programmierung.

Der Workspace am Gymnasium Steglitz ist hier zu finden: <a href='https://workspace.hackschule.de'>https://workspace.hackschule.de</a>.

Die veröffentlichten Tutorials findest du direkt auf der Startseite des Workspace.

## Funktionen

Der Workspace bündelt bewusst unterschiedliche Bereiche der Informatik in einer gemeinsamen Umgebung. Dazu gehören unter anderem:

- Programmierung in zahlreichen Sprachen – von BASIC, Pascal und C bis Python, JavaScript, Rust und weiteren
- Webentwicklung mit HTML, CSS, JavaScript, modernen Frameworks und Shared Live Apps zum Teilen laufender Projekte
- Computergrafik mit Pixelflow Canvas, PixelRAM und weiteren Werkzeugen
- interaktive Geschichten mit BIF
- relationale und Graphdatenbanken mit MySQL, Neo4j, SQL und Cypher
- App-Entwicklung mit Flutter für Web und Android, einschließlich APK-Builds
- Kommandozeile, Dateien, TCP/IP und Git
- wissenschaftliche Dokumente mit LaTeX
- HTML-Präsentationen mit shower.js

Direkte Netzwerkverbindungen zwischen studentischen Workspaces sind bewusst eingeschränkt. Für Netzwerkübungen steht TCP-Port 1234 zur Verfügung; laufende Webanwendungen lassen sich kontrolliert über Shared Live Apps teilen.

Die vollständige und aktuelle Übersicht der Tutorials wird aus den Inhalten des Workspace erzeugt; das README soll deshalb nur einen repräsentativen Überblick geben.

## Installation (lokal)

Für die lokale Installation benötigst du Docker mit Docker Compose, Git und Ruby. Die Installation wurde unter Linux getestet, sollte aber auch unter Windows funktionieren.

**Klonen des Repositories**

```bash
git clone https://github.com/specht/workspace.git
```
**Anpassung der Konfiguration**

Kopiere die Datei `src/ruby/credentials.template.rb` nach `src/ruby/credentials.rb` und nimm ein paar Anpassungen vor:

- `DEVELOPMENT` sollte `true` sein
- `PATH_TO_HOST_DATA` sollte einen absoluten Pfad zum Datenverzeichnis beinhalten (leg einfach ein Unterverzeichnis `data` an und gib den absoluten Pfad an)
- `WEBSITE_HOST` wird erst wichtig, wenn die Seite tatsächlich auf einem Server gehostet wird
- die E-Mail-Zugangsdaten sind im Development-Modus nicht relevant
- trag deine E-Mail-Adresse bei `ADMIN_USERS` ein

**Webserver-Image bauen**

```bash
./config.rb build
```

**Webserver starten**

```bash
./config.rb up
```

**Domain »workspace.test« einrichten**

Da der Workspace viel mit Subdomains arbeitet, müssen wir uns darum kümmern, dass die Domain `workspace.test` und alle Subdomains auf unseren lokalen Rechner zeigen.

**Einrichtung mit NetworkManager**

```
echo "address=/workspace.test/127.0.0.1" > \
    /etc/NetworkManager/dnsmasq.d/workspace.conf
systemctl restart NetworkManager
```

Wenn der Workspace gestartet ist, kannst du ihn im Browser unter <a href='http://workspace.test:8025'>http://workspace.test:8025</a> erreichen. Du solltest dich mit deiner E-Mail-Adresse (oder einem eindeutigen Präfix) und dem Code 123456 (fester Code in der Development-Umgebung) anmelden können. Um den eigentlichen Workspace (Visual Studio Code) zu starten, ist ein weiterer Schritt notwendig:

**Workspace-Image bauen**

Das Bauen des Workspace-Images dauert beim ersten Mal relativ lange und benötigt mehrere Gigabyte Speicherplatz, da zahlreiche Compiler, SDKs und Werkzeuge heruntergeladen und installiert werden müssen.

```bash
./build-image.sh
```

Falls der Platz einmal knapp werden sollte, lohnt es sich, zwischendurch mal den eisernen Besen zu schwingen:

```bash
docker system prune
```

**TIC-80 kompilieren**

Wenn du TIC-80 nutzen möchtest, musst du es einmalig kompilieren. Dazu musst du das folgende Skript ausführen:

```bash
./build-tic80.sh
```

## Entwicklung und Tests

Für zentrale Workspace-Abläufe gibt es End-to-End- und Toolchain-Tests mit Playwright. Sie prüfen unter anderem Anmeldung und Sessions, Workspace-Start, Profil, Shared Live Apps, die Netzwerkisolation sowie ausgewählte Tutorials und Werkzeugketten bis hin zum Flutter-Android-APK-Build. Nach Möglichkeit verwenden die Tests dieselben Beispiele, die auch in den Tutorials dokumentiert sind.

Die Einrichtung und Ausführung der Tests ist in [`e2e/README.md`](e2e/README.md) beschrieben.

## Betrieb auf einem Server

Der Workspace ist für den Betrieb auf einem Server für eine Schule konzipiert. Die Installation funktioniert im Wesentlichen genau wie die lokale Installation, aber es gibt einen wichtigen Punkt zu beachten: Der Webserver hat vollen Zugriff auf Docker, da er Container starten und stoppen können muss. Das bedeutet, dass der Workspace isoliert auf einem eigenen Server laufen sollte.

Ich betreibe dafür einen Cloud-Server bei Hetzner mit den folgenden Eckdaten:

- Shared vCPU x86 (Intel/AMD)
- Standort Falkenstein
- 80 GB Festplatte
- 100 GB externes Volume

Je nach Bedarf kann man flexibel zwischen verschiedenen Servern wechseln. Wenn viel los ist und viel gearbeitet wird, verwende ich einen Server mit 16 Kernen und 32 GB RAM für ca. 65 € / Monat. Wenn weniger los ist, z. B. in den Ferien, reicht auch ein Server mit 4 Kernen und 8 GB RAM für ca. 8 € / Monat. Dafür wird der Server einfach über die Cloud Console heruntergefahren, skaliert und neu gestartet. Der ganze Vorgang dauert eine bis zwei Minuten und die Abrechnung erfolgt stundengenau, so dass man die Kosten gut im Griff hat.

Falls du mit dem Gedanken spielst, einen eigenen Workspace für deine Schule aufzusetzen und es einfach mal ausprobieren möchtest, findest du [hier eine Schnellstart-Anleitung](bootstrap/README.hetzner.centos.md).