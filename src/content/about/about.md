<div class='meta'>
image: start-page.webp:50:0
image_alt: "Die Startseite des Workspace mit einem kurzen Text, daneben VS Code mit einem kleinen Python-Programm im Terminal"
kind: grundlagen
</div>

# Was ist der Hackschule Workspace?

<img class='r' style='width: 8em;' src='vscode.png' alt='Das blaue Logo von Visual Studio Code'>

<p class='abstract'>
Der Hackschule Workspace ist eine Programmierumgebung, die direkt im Browser läuft. Schülerinnen und Schüler können damit programmieren, Webseiten bauen und neue Technologien ausprobieren, ohne vorher Software installieren oder einrichten zu müssen.
</p>

Im Mittelpunkt steht **Visual Studio Code**, ein Editor, der auch in der professionellen Softwareentwicklung weit verbreitet ist. Dazu kommen ein Terminal und viele bereits eingerichtete Programmiersprachen, Werkzeuge und Erweiterungen.

Jeder Workspace ist persönlich: Deine Dateien bleiben erhalten und du kannst sowohl in der Schule als auch zu Hause daran weiterarbeiten.

## Für Schülerinnen und Schüler

Im Workspace kannst du direkt loslegen. Du kannst zum Beispiel Programme schreiben und ausführen, Webseiten entwickeln, mit Datenbanken arbeiten oder ganz neue Programmiersprachen und Technologien kennenlernen.

Auf dieser Website findest du dazu Tutorials, Beispiele und kleine Projekte. Sie sollen dir einen Einstieg geben – nicht jede einzelne Funktion erklären.

Programmieren lernt man vor allem durchs Ausprobieren. Wenn etwas nicht funktioniert, gehört das dazu: Fehlermeldungen lesen, nachforschen, verändern und noch einmal ausprobieren ist ein wichtiger Teil des Programmierens.

Eine kurze Einführung in die wichtigsten Funktionen des Workspaces findest du unter [»Basics«](/basics).


## Für Lehrkräfte

Der Hackschule Workspace ist für den Einsatz im Informatikunterricht, in Workshops und in Arbeitsgemeinschaften gedacht. Die notwendige Software ist bereits vorbereitet, sodass Unterricht nicht damit beginnen muss, Entwicklungsumgebungen auf unterschiedlichen Rechnern zu installieren und einzurichten.

Der Workspace ist dabei bewusst keine einzelne Lernumgebung für eine bestimmte Programmiersprache. Er stellt eine gemeinsame technische Grundlage bereit, auf der ganz unterschiedliche Unterrichtsprojekte laufen können – von ersten HTML-Seiten und kleinen Programmen bis zu Datenbanken, Grafik, Spielen und weiteren Themen.

Die Tutorials auf dieser Website können direkt verwendet, als Ausgangspunkt für eigene Aufgaben genutzt oder einfach zum Entdecken neuer Themen eingesetzt werden.

## Los geht's

Melde dich an und klicke oben auf »Workspace öffnen«. Der Workspace wird bei Bedarf gestartet und öffnet sich anschließend in einem neuen Browser-Tab.

Danach kannst du entweder einem Tutorial folgen oder direkt ein eigenes Projekt beginnen.

<!-- tutorial-screenshot
# Die Startseite, nicht angemeldet
open-site: /
tab: preview
viewport: 1029x640
hook: guest
sleep: 1
variants: dark
-->
<img class='teaser' hidden src='start-page.webp' alt='Die Startseite des Workspaces: oben »Anmelden«, darunter, was man im Workspace machen kann.'>

<!-- tutorial-screenshot
# Für die Startseite (index.html, front_picture_html): ein aufgeräumter Workspace mit einem kleinen Programm und einem Lauf im Terminal.
# zoom: Das Bild ist auf der Startseite nur etwa halb so breit wie die anderen Screenshots; etwas größer gezoomt bleibt es lesbar.
# viewport: 16:9, damit es neben dem Text der Startseite nicht höher ist als der Text.
viewport: 1029x579
zoom: 1.6
write-file: hallo/hallo.py <- file:hallo.py
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: hallo
press: Enter
wait-for-input-value: /workspace/hallo/
press: Enter
wait-for-text: hallo.py
open-file: hallo.py
hide-left-sidebar
terminal-open
terminal-run: clear
terminal-run: python3 hallo.py
sleep: 1
type: Ada
press: Enter
terminal-wait-for-prompt
move-mouse: selector:.terminal
-->
<img hidden src='workspace.webp' alt='Der Workspace: oben im Editor hallo.py, ein Programm, das nach dem Namen fragt, unten im Terminal sein Lauf: Wie heißt du? Ada, Hallo, Ada!'>
