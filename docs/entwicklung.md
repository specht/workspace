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

Dienste: `nginx`, `ruby`, `mysql`, `phpmyadmin`, `peer_firewall`, `neo4japp`
und in der Entwicklung `tutorial_screenshots`.

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

JavaScript-Tests laufen mit Node, ohne npm-Pakete:

```bash
node --test src/tutorial-screenshots/test/*.test.js
cd src/vscode-extensions/er && node --test test/*.test.cjs core/*.test.cjs
```

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
```

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
./build-tic80.sh      # einmalig, wenn TIC-80 gebraucht wird
docker system prune   # wenn der Platz knapp wird
```

Das erste Bauen dauert lange und braucht mehrere Gigabyte. Die
code-server-Version steht in `docker/code/Dockerfile` (`CODE_SERVER_IMAGE`).

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

## Daten für Tutorials

Große Dateien, die Lernende mit `wget` holen, liegen im Repository
`specht/workspace-files`, nicht hier. Für die Filmdatenbank baut
`src/scripts/collect-movies/collect-movies.rb` die Dateien in
`movie-archive/` (Anleitung im README daneben); die Ausgabe kommt nach
`specht/workspace-files/movie-archive/`.

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
