# Entwicklung: Handgriffe zum Nachschlagen

Was man beim Entwickeln am Workspace immer wieder braucht und nach ein paar
Wochen vergessen hat. Alle Befehle laufen im Wurzelverzeichnis des
Repositorys, sofern nicht anders angegeben. Die Einrichtung selbst steht im
[README](../README.md).

## Dienste

`./config.rb` ist ein dünner Aufsatz auf `docker compose`: Alles hinter
`./config.rb` geht unverändert an `docker compose --project-name workspace`.

```bash
./config.rb build                 # Images der Dienste bauen (nicht das Workspace-Image)
./config.rb up -d                 # starten
./config.rb down                  # stoppen
./config.rb logs -f ruby          # Log eines Dienstes verfolgen
./config.rb exec ruby bash        # Shell im Ruby-Container
./config.rb restart ruby          # Ruby-Dienst neu starten
```

Dienste: `nginx`, `ruby`, `mysql`, `phpmyadmin`, `peer_firewall`, `neo4japp`,
`docker_guard`, in der Entwicklung `tutorial_screenshots` und mit
`CASE_SERVER = true` `case_museum` und `case_alarm` (siehe »Der Fallserver«).

### Der Docker-Wächter

Der Ruby-Dienst hat keinen Docker-Socket. Er spricht mit `docker_guard`
(`DOCKER_HOST=unix:///run/docker-guard/docker.sock`), der als einziger neben
der Peer-Firewall den echten Socket hat und jede Anfrage prüft
(`docker/docker-guard/guard.rb`, Regeln in `src/ruby/include/docker_policy.rb`,
getestet in `test/docker_policy_test.rb`). Durch geht nur, was der Workspace
braucht: Container `hs_code_*` aus dem Workspace-Image mit Ordnern unter
`PATH_TO_HOST_DATA/user/`, ohne Privilegien, Host-Modi, zusätzliche
Fähigkeiten oder Ports; `exec` in Workspaces und in die beiden Datenbanken;
Lesen; `kill` für Workspaces und `HUP` für nginx. Alles andere bekommt eine
403, und der Wächter schreibt den Grund in sein Log
(`./config.rb logs docker_guard`). Braucht der Code eine neue Docker-Aktion,
gehört sie in `docker_policy.rb` (mit Test) und danach
`./config.rb restart docker_guard`.

### Kapazität messen

`./config.rb exec ruby ruby /src/scripts/measure-capacity.rb [ANZAHL]`
startet ANZAHL (vorgegeben 30) Test-Workspaces gleichzeitig, misst die Zeit
bis jeder antwortet, Last und Speicher, räumt danach auf und legt den Bericht
in `/internal/capacity-*.txt`. Auf dem Server außerhalb des Unterrichts.

### Betrieb überwachen

`/admin` zeigt unter »Betrieb«, beim Laden der Seite geprüft
(`src/ruby/include/server_health.rb`, bewertet in `include/health.rb`, getestet
in `test/health_test.rb`):

- **Version:** der ausgecheckte Commit mit Link (der Ruby-Dienst sieht
  `.git` nur lesend) und wann das Workspace-Image gebaut wurde; fehlt ihm
  das Label `de.hackschule.file-capabilities`, ist es älter als die Härtung
  und muss neu gebaut werden.
- **Sicherung:** wie alt die letzte ist. Der Workspace sieht die Sicherungen
  nicht; sie melden sich selbst, indem sie nach jedem erfolgreichen Lauf die
  Datei `internal/backup-done` im Datenordner anfassen. Ohne die Datei steht
  dort »nicht eingerichtet«, älter als `BACKUP_MAX_AGE_HOURS` (in
  `credentials.rb`, vorgegeben 26) gilt als gestört. Für borgmatic:

  ```yaml
  # borgmatic 1.x
  after_backup:
      - touch /pfad/zum/datenordner/internal/backup-done

  # borgmatic 2
  commands:
      - after: action
        when: [create]
        run:
            - touch /pfad/zum/datenordner/internal/backup-done
  ```

- **Workspace-Starts:** wie viele in 24 Stunden (seit dem Neustart des
  Ruby-Dienstes, die Zahlen liegen nur im Speicher), wie viele
  fehlgeschlagen sind, wie lange einer dauert. Gestört ab drei Fehlschlägen,
  wenn sie ein Fünftel der Starts sind; die Gründe stehen im Log
  (`Server start worker failed`).
- **Warteschlange:** wartende Starts; gestört, wenn einer länger als fünf
  Minuten wartet.
- **Docker** (über den Wächter), **MySQL**, **Neo4j** der Lernenden und die
  Neo4j des Workspace: ob sie innerhalb von drei Sekunden antworten.
- **Speicherplatz** unter `/user` und die **stündlichen Aufgaben**
  (Löschungen, Links, Erinnerungen, die Speichermessung von
  `housekeeping.rb`): gestört, wenn sie länger als zwei Stunden nicht
  gelaufen sind.

Ist etwas davon gestört, bekommt die Administration einmal am Tag eine
E-Mail je Punkt (vermerkt in `/internal/health-alerts.json`; für den
Speicherplatz gibt es die eigene Mail). Die Warteschlange bekommt keine Mail,
sie leert sich von selbst. Was das alles nicht sieht: einen Server, der
ganz steht. Dafür braucht es einen Dienst von außen, der die Startseite
aufruft.

### Große Workspaces

»Betrieb« zeigt die laufenden Workspaces nach Last (Rechenzeit, dann
Arbeitsspeicher), die zehn größten zuerst. »Stoppen« beendet alle Programme
eines Workspace; die Dateien bleiben. Für den Speicherplatz gibt es in
`/personen` »Platz schaffen« in der Zeile einer Person. Beides fragt nach
einem Grund, den die Person als Hinweis liest (`include/look_in.rb`, Arten
`stop` und `cleanup`), und schickt ihn, wenn angehakt, per E-Mail
(`MailTexts.workspace_action`).

Welche Ordner sich neu erzeugen lassen, steht an einer Stelle:
`src/ruby/tools/rebuildable.py`. Immer: `node_modules`, `__pycache__`,
`.gradle`, `.dart_tool`, `.pub-cache`. `build` nur neben `pubspec.yaml`,
`build.gradle(.kts)` oder `CMakeLists.txt`, `target` nur neben `Cargo.toml`
oder `pom.xml`, Python-Umgebungen nur mit `pyvenv.cfg`. Vorab angehakt ist
ein Ordner nur, wenn der Befehl ihn auch wieder anlegen kann (`package.json`
oder `pubspec.yaml` daneben, bei Python-Umgebungen `requirements.txt` oder
`pyproject.toml`) und, bei `node_modules`, keine Datei darin jünger ist als
der feste Zeitstempel, den npm setzt (26.10.1985): Eine jüngere hat jemand
von Hand geändert. Das Skript läuft als
Workspace-Benutzer (`setpriv`), folgt keinen Links und löscht nur, was ein
neuer Scan im Moment des Löschens noch findet.

Der Download von »Meine Daten« (`tools/zip_stream.py`) lässt diese Ordner
weg, außer mit `?alle=1`, und hält einen Puma-Thread, solange er läuft.
Deshalb laufen höchstens `EXPORT_LIMIT` (2) gleichzeitig
(`include/my_data.rb`) und Puma hat 12 Threads (`config.rb`).

## Was muss ich nach einer Änderung neu bauen?

| Geändert | Dann |
| --- | --- |
| Inhalte in `src/content/` | nichts; in der Entwicklung liest jeder Seitenaufruf sie neu |
| Ruby (`src/ruby/`) | in der Entwicklung nichts: `rerun` startet den Dienst bei jeder Änderung neu; auf dem Server `./config.rb restart ruby` |
| CSS, JS in `src/static/` | Seite neu laden. Hält der Browser die alte Datei, hilft <kbd>Strg</kbd>+<kbd>Umschalt</kbd>+<kbd>R</kbd>; jeder Neustart von `ruby` setzt außerdem einen neuen `CACHE_BUSTER` |
| Screenshot-Generator (`src/tutorial-screenshots/`, `docker/tutorial-screenshots/`) | `./config.rb build tutorial_screenshots && ./config.rb up -d tutorial_screenshots` |
| eine VS-Code-Erweiterung, eine Toolchain, `docker/code/` | Workspace-Image neu bauen (`./build-image.sh`, siehe unten); laufende Workspaces nutzen das neue Image erst nach einem Neustart |
| `config.rb`, `credentials.rb` | `./config.rb up -d` (legt geänderte Container neu an) |

## Tests

```bash
cd src/ruby
ruby test/task_spec_test.rb                       # ein Test
for f in test/*_test.rb; do ruby "$f" | tail -1; done   # alle
```

`reference_index_test.rb` braucht nokogiri und läuft deshalb nur im
Ruby-Container (`./config.rb exec ruby ruby test/reference_index_test.rb`).
`task_spec_test.rb` prüft alle `tasks.yaml` im Inhaltsordner, ein Test in
`content_parts_test.rb` alle Kataloge und `_include(…, done: …)`.
`task_examples_test.rb` führt die `ruby`- und `python`-Prüfungen mit den
Interpretern auf dem Rechner aus; für »Geheime Botschaften in Bildern« braucht
es dort die Bibliotheken, die auch die Lernenden installieren:
`gem install chunky_png` und `pip install pillow`.

JavaScript-Tests laufen mit Node, ohne npm-Pakete:

```bash
node --test src/tutorial-screenshots/test/*.test.js
node --test test/static/*.test.cjs        # JavaScript der Website, etwa der Query Stepper
cd src/vscode-extensions/er && node --test test/*.test.cjs core/*.test.cjs
```

Der Query Stepper (`src/static/include/query_stepper.js`, Seite
`/query-stepper`, eingebettet in »Einführung in SQL« und »Filmdatenbank«)
rechnet im Browser mit den Tabellen in `src/static/query-stepper/`.
`src/scripts/query-stepper-data.rb` baut sie aus den Dateien, die Lernende
herunterladen:

```bash
src/scripts/query-stepper-data.rb countries countries.sql
src/scripts/query-stepper-data.rb countries-basic countries-basic.sql
src/scripts/query-stepper-data.rb movies movies-schema.sql movies-data-films.sql movies-data-crew.sql
src/scripts/query-stepper-data.rb music music-schema.sql
```

`countries`: die Länder der Welt mit allen vier Tabellen ganz. `movies`:
Filme, Genres und ihre Zuordnung ganz, von den Personen nur Regie und
Schauspiel ab fünf Filmen (522 kB, komprimiert gut 170 kB). `music`: nur die
Tabellen und Schlüssel, ohne Zeilen, für `_schema(music)`. Die Datensätze
selbst baut `src/scripts/collect-countries`, `collect-movies` und
`collect-music`; beschrieben sind sie im Handbuch unter »Datensätze«.

Die End-to-End-Tests mit Playwright (Anmeldung, Workspace-Start,
Toolchains bis zum Android-APK) stehen in [e2e/README.md](../e2e/README.md).

## Screenshots und Videos der Tutorials

Steht vor einem `<img>` oder `<video>` ein `<!-- tutorial-screenshot … -->`,
erzeugt der Generator die Datei selbst. In der Entwicklung reicht es, die
Tutorial-Seite neu zu laden: Veraltetes wird im Hintergrund neu gerendert,
die Fußzeile zeigt den Fortschritt. Rezeptsprache, Hooks, Videos und wann
etwas als veraltet gilt: [src/tutorial-screenshots/README.md](../src/tutorial-screenshots/README.md).

```bash
src/scripts/recreate-screenshots.rb bif           # alles in einem Tutorial, auch Aktuelles
src/scripts/recreate-screenshots.rb --all         # alles in allen Tutorials
src/scripts/recreate-screenshots.rb --stale       # nur Veraltetes, alle Tutorials, und warten
src/scripts/recreate-screenshots.rb --stale bif   # nur Veraltetes in einem Tutorial
src/scripts/recreate-screenshots.rb --status      # nichts rendern: Stand jedes erzeugten Bildes (✓/✗),
                                                  # erzeugte und von Hand gemachte Bilder je Seite, Summe
src/scripts/recreate-screenshots.rb --help
```

Jeder Lauf endet mit derselben Übersicht für die gerenderten Tutorials.

Rendert der Generator gerade etwas (zum Beispiel mit dem Skript), und ein
Seitenaufruf würde weiteres Rendern anstoßen, pausiert das automatische
Rendern bei Seitenaufrufen von selbst. Die Fußzeile zeigt dann
»Automatisches Rendern pausiert« und den Knopf »Fortsetzen«; solange
gerendert wird, steht dort »Pausieren«. Der Zustand bleibt über Neustarts
erhalten. Das Skript rendert immer, auch wenn pausiert ist.

Ein neu gebautes Workspace-Image erzeugt **nichts** neu, solange die
code-server-Version gleich bleibt. Sieht man eine Änderung am Image in
Screenshots (neue Erweiterung in der Seitenleiste), rendert man die
betroffenen Tutorials mit dem Skript neu. Die erzeugten Dateien und
`.tutorial-screenshots.json` gehören in den Commit.

Für Screenshots von Hand: `src/scripts/fetch-screenshot.rb` holt den letzten
Screenshot vom eigenen Rechner (Pfade stehen im Skript, je nach Hostname),
`src/scripts/replace-screenshot.rb` ersetzt eine Bilddatei in allen
Tutorials, die dieselbe Datei verwenden.

## VS-Code-Erweiterungen

Die eigenen Erweiterungen liegen in `src/vscode-extensions/`. Jede baut ihre
`.vsix` selbst:

| Erweiterung | Bauen | Ergebnis |
| --- | --- | --- |
| `er` (ER-Werkstatt) | `npm run package` oder ohne npm: `node --test test/*.test.cjs core/*.test.cjs && node pack.cjs` | `hackschule-er-<version>.vsix` |
| `checkpoints` | `npm run package` | `hackschule-checkpoints-<version>.vsix` |
| `bif-watcher` | `npm run package` | `bif-authoring-tools-<version>.vsix` |
| `keyboard-tutorial` | `npm run package` | `hackschule-keyboard-tutorial-<version>.vsix` |
| `sidebar-init` | `npm run package` | `hackschule-sidebar-init-<version>.vsix` |
| `live-server` | fremde Erweiterung, liegt fertig bei | `ritwickdey.LiveServer-5.7.10.vsix` |

Ins Workspace-Image kommt eine neue `.vsix` so:

1. `version` in der `package.json` der Erweiterung erhöhen und bauen.
2. In `build-image.sh` die `cp`-Zeile auf den neuen Dateinamen setzen.
3. In `docker/code/Dockerfile` die passende `ARG …_VERSION` oben anpassen
   (z. B. `HACKSCHULE_ER_VERSION`).
4. In `docker/code/svc-code-server-run` den Dateinamen in `EXTENSIONS`
   anpassen. Dieses Skript installiert die Erweiterung beim Start des
   Workspaces; fehlt die Datei, überspringt es sie nur mit einer Warnung.
5. `./build-image.sh`. Das Skript baut zuerst einen Kandidaten, prüft, ob jede
   `.vsix` aus `svc-code-server-run` darin liegt, prüft das LaTeX-Tutorial und
   taggt erst dann `hs_code_server:latest`.

Zum schnellen Ausprobieren ohne neues Image: in einem laufenden Workspace
»Extensions« → »…« → »Install from VSIX…« und die Datei auswählen.

## Workspace-Image

```bash
./build-image.sh      # Executor-Images, Erweiterungen, Toolchains, code-server-Image
./build-tic80.sh      # TIC-80 für den Browser (Version in TIC80_REF), nach jedem Wechsel der Version
BRIDGE_ONLY=1 ./build-tic80.sh   # nur docker/tic80/hackschule-persistence.js neu davorsetzen
docker system prune   # wenn der Platz knapp wird
```

Das erste Bauen dauert lange und braucht mehrere Gigabyte. Die
code-server-Version steht in `docker/code/Dockerfile` (`CODE_SERVER_IMAGE`).

Ein Workspace startet als root (die Init von linuxserver, dann der Nutzer
der Lernenden) und behält von Dockers Fähigkeiten nur CHOWN, DAC_OVERRIDE,
FOWNER, KILL, SETGID und SETUID, mit `no-new-privileges`
(`HARDENING` in `src/ruby/include/workspace_runtime.rb`). Ohne NET_RAW kann
niemand im Netz der Lernenden Pakete fälschen. `ping` kommt ohne aus
(ICMP-Sockets ohne Rechte); das Image entfernt dafür alle Datei-Fähigkeiten,
denn ein Programm mit einer Fähigkeit, die der Container nicht hat, startet
gar nicht erst. `build-image.sh` prüft `ping` so, wie Lernende es aufrufen.
Braucht ein neues Werkzeug eine Fähigkeit, steht sie in `HARDENING`, nicht
im Image.

## Namen und Home-Verzeichnis im Workspace

Alle Namen kommen aus dem Teil der E-Mail-Adresse vor dem `@`; der Name
einer Person (Einladung, Profil) spielt keine Rolle. Umlaute werden so
geschrieben, wie man es ohne sie tut: ä → ae, ö → oe, ü → ue, ß → ss;
andere Buchstaben verlieren ihre Akzente (é → e, ø → o, ł → l); was dann
noch kein Buchstabe, keine Ziffer, kein Punkt oder Bindestrich ist, wird
zu `-` (`src/ruby/include/login_names.rb`, getestet in
`test/login_names_test.rb`).

- **Linux-Benutzer** im Workspace (`LoginNames.linux_login`): beginnt mit
  einem Buchstaben (sonst `user-` davor, leer: `student`), höchstens
  32 Zeichen. Er muss nicht eindeutig sein: Jeder Workspace ist ein eigener
  Container, der Name steht nur im Prompt und bei `whoami`. Heißt er wie ein
  Systemkonto des Images (`root@…`), benennt der Container ihn um
  (`student-root-2`, `docker/code/init-code-server-run`). Er wird bei jedem
  Start neu gebildet; die Dateien gehören der Benutzernummer 1000, eine
  andere Regel ändert also nur den Prompt.
- **MySQL- und Neo4j-Login** (`DatabaseIdentity`): dieselbe Umschrift,
  dazu 3 bis 32 Zeichen aus a–z, 0–9, `.` und `-`, nie ein reservierter Name
  (`root`, `mysql`, `neo4j`, `system…`). Er ist eindeutig: Haben zwei
  Adressen denselben Teil vor dem `@`, bekommt die zweite einen Zusatz aus
  der ganzen Adresse (`max-3pnw`). Gespeichert wird er einmal am Konto
  (`User.db_login`) und ändert sich danach nicht; ältere Konten behalten
  ihren Login aus der Zeit vor der Umschrift (`jurgen.muller` statt
  `juergen.mueller`).

| Adresse | Linux | MySQL / Neo4j |
| --- | --- | --- |
| `jürgen.müller@…` | `juergen.mueller` | `juergen.mueller` |
| `anna+informatik@…` | `anna-informatik` | `anna-informatik` |
| `2a.lea@…` | `user-2a.lea` | `2a.lea` |
| `max@schule-b.de`, wenn es `max@schule-a.de` schon gibt | `max` | `max-3pnw` |

Das Home-Verzeichnis ist `/workspace`, für alle gleich, und bleibt es
(Entscheidung vom 19.10.2026). `/home/<name>` wäre näher an einem
Linux-Lehrbuch, aber Pfade wären dann je Person verschieden, sodass die
Seiten keinen Pfad mehr nennen könnten, den alle abtippen; `/workspace`
steht in rund 100 Seiten, 35 Aufgabendateien, den Rezepten der Screenshots,
dem Backend, dem Image und den E2E-Tests; und vorhandene Workspaces
speichern absolute Pfade (Python-Umgebungen, installierte Werkzeuge,
zuletzt geöffnete Ordner in VS Code). Befehle auf den Seiten schreiben das
Home trotzdem als `~` (`git clone … ~/gif`), wo das natürlich ist.

## Datenbanken

Neo4j (Konten, Aufgabenstände, Gruppen) über `neo4j_bolt` im Ruby-Container:

```bash
cd src/scripts
./dump-database.rb …        # neo4j_bolt dump im Ruby-Container, Argumente gehen durch
./load-dump.rb DATEI …      # spielt DATEI mit neo4j_bolt load ein
./clear-database.rb …       # neo4j_bolt clear: leert die Datenbank (Vorsicht)
```

Die Skripte wechseln selbst ins Wurzelverzeichnis und rufen `./config.rb exec ruby
neo4j_bolt …` auf; welche Optionen `neo4j_bolt` kennt, zeigt
`./config.rb exec ruby neo4j_bolt --help`.

Die MySQL-Datenbanken der Lernenden verwaltet der Ruby-Dienst selbst; phpMyAdmin
läuft als eigener Dienst.

## Der Fallserver

Für das letzte Kapitel von »Ein Krimi im Terminal« gibt es zwei kleine
SSH-Server: `museum.bitstein` und `alarmanlage.bitstein`. Lernende melden sich
mit `ssh` an, springen mit `ssh -J` über das Museum zur Alarmanlage und
kopieren mit `scp` Beweise in ihren Workspace. Ohne den Server zeigt die Seite
das Kapitel nicht, und seine Aufgaben stehen auch nicht in der
Klassenübersicht (`<!-- feature: case_server -->`, docs/aufgaben.md).

Einschalten in `credentials.rb`:

```ruby
CASE_SERVER = true
```

```bash
./config.rb build case_museum case_alarm peer_firewall
./config.rb up -d
```

- **Zugänge:** Jeder Server hat 1000 Konten `e000` bis `e999`. Die Website
  wählt für jede Person eins aus ihrer Adresse. Auf `museum.bitstein` hat
  jedes Konto ein eigenes Passwort aus `LOGIN_CODE_SALT`
  (`src/ruby/include/case_server.rb`), das der Server beim Start mit `openssl`
  nachrechnet (`docker/case-server/entrypoint.sh`). Auf `alarmanlage.bitstein`
  gilt für alle Konten das Passwort der Wartung, das Lernende im
  Übergabeprotokoll auf dem Museumsserver finden (`make_case.rb`,
  `ALARM_PASSWORD`). Teilen sich zwei Personen ein Konto, schadet das nicht:
  Schreiben kann dort niemand etwas.
- **Der Zettel:** Die Akten liefert die Website selbst. Wer angemeldet ist,
  bekommt auf der Seite eine eigene Adresse,
  `/terminal-mystery/akten/SCHLÜSSEL/bitstein.tar.gz` (`CaseServer.archive_key`);
  im Archiv liegt dann zusätzlich `bitstein/zugang-museum.txt` mit dem Zugang
  dieser Person, egal woher die Anfrage kommt. Unter
  `/terminal-mystery/bitstein.tar.gz` und ohne gültigen Schlüssel sucht die
  Website den Workspace an der Adresse der Anfrage. Das klappt nur, wenn der
  Workspace sie direkt erreicht: Über die öffentliche Adresse kommt die
  Anfrage vom Server selbst. Workspaces
  bekommen außerdem `CASE_USER` und `CASE_PASSWORD` mit, für die
  Screenshot-Rezepte; die Seite nennt sie nicht mehr.
- **Nur lesen:** Alles gehört `root`. Beschreibbar ist nur `/tmp`, 64 MB, und
  dort sieht niemand, was andere ablegen. Jedes Konto hat höchstens 40
  Prozesse, 4 Anmeldungen, Dateien bis 1 MB und 5 Minuten Rechenzeit pro
  Prozess; der ganze Container 512 MB und eine CPU.
- **Netz:** `museum.bitstein` hängt im Netz der Workspaces, mit eigenem Label
  für die `peer_firewall`: Workspaces erreichen dort nur Port 22, und vom
  Museum aus erreicht man keinen anderen Workspace, so wie von Workspace zu
  Workspace. Es ist ausdrücklich keine Infrastruktur, denn die darf alle
  Workspaces erreichen. `alarmanlage.bitstein` hängt nur in einem eigenen
  Netz ohne Verbindung nach außen. Der einzige Weg dorthin führt über das
  Museum, das nur diese eine Weiterleitung erlaubt (`PermitOpen`).
- **Schlüssel:** Die Schlüssel der Server liegen in `data/case-server/`, damit
  ihr Fingerabdruck nach einem neuen Build gleich bleibt.
- **Inhalt:** `src/content/terminal-mystery/beispiele/make_case.rb` schreibt
  die Dateien beider Server nach `docker/case-server/files/`; danach die
  Images neu bauen. Die Bilder der Kameras in `kameras/` baut es dabei mit
  `beispiele/cctv/build.py` aus den Ausgangsbildern daneben; dafür braucht es
  ein Python mit OpenCV und Pillow, in `CCTV_PYTHON` (`beispiele/cctv/README.md`).
- **Prüfung:** Im Prüfungsmodus hat ein Workspace kein Netz und bekommt keinen
  Zugang.

## Daten für Tutorials

Große Dateien, die Lernende mit `wget` holen, liegen im Repository
`specht/workspace-files`, nicht hier. Die drei Datensätze baut je ein Skript,
mit Anleitung im README daneben: `src/scripts/collect-countries` die Länder
der Welt (`country-archive/`), `src/scripts/collect-movies` die Filme
(`movie-archive/`) und `src/scripts/collect-music` die Musik
(`music-archive/`). Veröffentlicht werden sie mit einem Befehl:

```bash
src/scripts/publish-datasets.rb --repo ~/workspace-files countries          # kopieren, dann ansehen
src/scripts/publish-datasets.rb --repo ~/workspace-files countries --push   # und veröffentlichen
```

Das Skript kopiert die Ausgabe des Sammlers (ohne die Caches) in einen Klon
von `specht/workspace-files`, schreibt dort die `README.md` mit allen
Datensätzen, ihren Tabellen und Zeilen, baut hier die Tabellen des
Query Steppers neu (die dann hier committet werden) und committet dort, mit
`--push` auch gleich hinauf. `--repo` kann `WORKSPACE_FILES` ersetzen,
`--from countries=DIR` nimmt einen Bau aus einem anderen Ordner. Ohne
Datensatz schreibt es nur die Übersicht neu. Erwartet eine Aufgabe in einer
`tasks.yaml` eine andere Zeilenzahl (`tables: { city: 4748 }`), verweigert
es den Bau, bis die Aufgaben, ihre Lösungen und die Screenshots nachgezogen
sind, oder mit `--force`. Agenten veröffentlichen nicht selbst: `--patch
DATEI` arbeitet in einem frischen Klon und schreibt einen Patch für
`git am`.

## Icons

Die Tabler-Icons sind ein gebündeltes Sprite. Neues Icon: Zeile in
`src/static/include/tabler/icons.txt` ergänzen, dann

```bash
git clone --depth 1 https://github.com/tabler/tabler-icons.git /tmp/tabler-icons
ruby src/ruby/tools/build_tabler_sprite.rb /tmp/tabler-icons
```

## Änderungen festhalten

Jede nennenswerte Änderung bekommt einen Eintrag in
[CHANGELOG.md](../CHANGELOG.md) unter »Unreleased«, getrennt nach
»Hinzugefügt«, »Geändert« und »Für Betreibende«. Patches, die Claude liefert,
spielt man mit `git am <datei>.patch` auf `main` ein.
