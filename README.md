# Hackschule Workspace

Der Workspace der Hackschule ist eine Entwicklungsumgebung, die speziell für den Einsatz in der Schule konzipiert wurde. Sie läuft vollständig im Browser und benötigt keine Installation. Der Workspace enthält alle notwendigen Werkzeuge, um Programme in verschiedenen Programmiersprachen zu schreiben, zu kompilieren und auszuführen. Der wesentliche Vorteil ist, dass Schülerinnen und Schüler in der Schule und zu Hause dieselbe Umgebung vorfinden und ihre Dateien an einem zentralen Ort speichern können.

Der Workspace am Gymnasium Steglitz ist hier zu finden: <a href='https://workspace.hackschule.de'>https://workspace.hackschule.de</a>.

## Funktionen

- Programmieren: Fortran, COBOL, BASIC, Pascal, C, Smalltalk, C++, Common Lisp, Python, Lua, Ruby, Java, JavaScript, Netwide Assembler, C#, Go, Dart, Rust
- wissenschaftliche Dokumente setzen mit LaTeX
- moderne und portable HTML-Präsentationen erstellen mit shower.js
- den Umgang mit der Kommandozeile lernen (Bash)
- Versionsverwaltung mit Git üben

## Installation (lokal)

Für die lokale Installation benötigst du Docker (mit docker-compose), Git und Ruby. Die Installation wurde unter Linux getestet, sollte aber auch unter Windows funktionieren.

**Klonen des Repositories**

```bash
git clone https://github.com/specht/workspace.git
```
**Anpassung der Konfiguration**

Kopiere die Datei `src/ruby/credentials.template.rb` nach `src/rubyt/credentials.rb` und nimm ein paar Anpassungen vor:

- `DEVELOPMENT` sollte `true` sein
- `PATH_TO_HOST_DATA` sollte einen absoluten Pfad zum Datenverzeichnis beinhalten (leg einfach ein Unterverzeichnis `data` an und gib den absoluten Pfad an)
- `WEBSITE_HOST` wird erst wichtig, wenn die Seite tatsächlich auf einem Server gehostet wird
- die E-Mail-Zugangsdaten sind im Development-Modus nicht relevant
- trag deine E-Mail-Adresse bei `ADMIN_USERS` ein

**Einladung hinzufügen**

Lege die Datei `src/invitations/dev.txt` an (der Dateiname ist egal) und trage die folgenden Zeilen ein (mit deinen Daten):

```
> Developer
Max Mustermann <max@example.com>
```

Wir haben damit eine Gruppe »Developer« mit einem Mitglied »Max Mustermann« angelegt. Die E-Mail-Adresse wird später für die Anmeldung benötigt – später könnte man hier Klassen oder Lerngruppen anlegen.

**Webserver-Image bauen**

```bash
./config.rb build
```

**Webserver starten**

```bash
./config.rb up
```

Wenn der Workspace gestartet ist, kannst du ihn im Browser unter <a href='http://localhost:8025'>http://localhost:8025</a> erreichen. Du solltest dich mit deiner E-Mail-Adresse (oder einem eindeutigen Präfix) und dem Code 123456 (fester Code in der Development-Umgebung) anmelden können. Um den eigentlichen Workspace (Visual Studio Code) zu starten, ist ein weiterer Schritt notwendig:

**Workspace-Image bauen**

Das Bauen des Workspace-Images dauert relativ lange (bei mir ca. 30 Minuten), da alle notwendigen Pakete heruntergeladen und installiert werden müssen. Das Image ist ca. 13.5 GB groß.

```bash
./build-image.sh
```

Falls der Platz einmal knapp werden sollte, lohnt es sich, zwischendurch mal den eisernen Besen zu schwingen:

```bash
docker system prune
```

**TIC-80 compilieren**

Wenn du TIC-80 nutzen möchtest, musst du es einmalig kompilieren. Dazu musst du folgendes Skript ausführen:

```bash
./build-tic80.sh
```

## Betrieb auf einem Server

Der Workspace ist für den Betrieb auf einem Server für eine Schule konzipiert. Die Installation funktioniert im wesentlichen genau wie die lokale Installation, aber es gibt einen wichtigen Punkt zu beachten: Der Webserver hat vollen Zugriff auf Docker, da er Container starten und stoppen können muss. Das bedeutet, dass der Workspace isoliert auf einem eigenen Server laufen sollte.

Ich betreibe dafür einen Cloud-Server bei Hetzner mit den folgenden Eckdaten:

- Shared vCPU x86 (Intel/AMD)
- Standort Falkenstein
- 80 GB Festplatte
- 100 GB externes Volume

Je nach Bedarf kann man flexibel zwischen verschiedenen Servern wechseln. Wenn viel los ist und viel gearbeitet wird, verwende ich einen Server mit 16 Kernen und 32 GB RAM für ca. 65 € / Monat. Wenn weniger los ist, z. B. in den Ferien, reicht auch ein Server mit 4 Kernen und 8 GB RAM für ca. 8 € / Monat. Dafür wird der Server einfach über die Cloud Console heruntergefahren, skaliert und neu gestartet. Der ganze Vorgang dauert eine bis zwei Minuten und die Abrechnung erfolg minutengenau, so dass man die Kosten gut im Griff hat.

Für die Installation auf dem Cloud Server gibt es die Datei `cloud-config.yaml`, die man beim Erstellen des Servers mit angeben kann (vorher bitte den Nutzernamen `micha` ggfs. anpassen und den eigenen Public Key eintragen).

Das TLS-Frontend wird ebenfalls in einem Container betrieben, der über Docker Compose gestartet wird. Dazu einfach die Datei `frontend-docker-compose.yaml` in ein Verzeichnis `frontend` als `docker-compose.yaml` kopieren und dort starten:

```bash
docker compose up -d
```

In der `config.rb` sollten die Werte für `VIRTUAL_HOST`, `LETSENCRYPT_HOST` und `LETSENCRYPT_EMAIL` angepasst werden, damit das Frontend automatisch Letsencrypt-Zertifikate anfordern kann.
