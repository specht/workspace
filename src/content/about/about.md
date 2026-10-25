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

Die Tutorials auf dieser Website können direkt verwendet, als Ausgangspunkt für eigene Aufgaben genutzt oder einfach zum Entdecken neuer Themen eingesetzt werden. Wie eine Aktivität aufgebaut ist und warum, steht unten unter [»Was eine Aktivität ausmacht«](#was-eine-aktivität-ausmacht).

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

## Was eine Aktivität ausmacht

Eine Aktivität ist kein Text mit ein paar Beispielen, sondern ein Projekt, das Lernende Schritt für Schritt in ihrem eigenen Workspace bauen: eine Webseite, einen Webserver, einen Raytracer. Die Seite erklärt, was neu ist, aber vor allem lässt sie die Lernenden etwas tun und sagt ihnen, ob es stimmt. Das Muster dahinter heißt in der Informatikdidaktik PRIMM: erst vermuten, dann ausführen, untersuchen, ändern und am Ende selbst schreiben (Predict, Run, Investigate, Modify, Make; [Sentance, Waite und Kallia 2019](https://doi.org/10.1080/08993408.2019.1608781)). Die Bilder in diesem Kapitel stammen aus [»Raytracing in C«](/raytracing).

### Echte Dateien, echte Werkzeuge

Gearbeitet wird in Visual Studio Code, mit Dateien, Terminal und Git, so wie in der Softwareentwicklung. Was entsteht, gehört den Lernenden: Sie arbeiten zu Hause daran weiter, und eine Abgabe ist eine Kopie, über die sie selbst bestimmen. Nichts wird versteckt: Pfade, Befehle und Adressen stehen im Text, wie sie sind. Was beim ersten Mal ausführlich erklärt wird, steht danach in einer Zeile mit einem Link ins Handbuch.

### Erst vermuten, dann ausprobieren

Bevor ein Programm läuft, fragt die Seite oft, was gleich passieren wird. Was die Antwort verraten würde, ein Bild vom Ergebnis oder der Absatz, der es erklärt, erscheint erst, wenn die Vermutung abgegeben ist, ob richtig oder falsch. Wer sich erst festgelegt hat, schaut beim Ausprobieren genau hin. Lehrkräfte und Gäste sehen alles sofort; so wie unten sehen es nur Lernende.

<div class='gallery columns-2'>
<figure><img src='tour-vorhersage.webp' alt='Die Aufgabe »Ein Strahl, der auf der Oberfläche beginnt« mit vier Antworten zur Wahl und dem Knopf »Vermutung abgeben«. Darunter statt des Ergebnisses nur die Zeile: Hier geht es weiter, sobald du deine Antwort oben abgegeben hast.'><figcaption>Vor der Antwort wartet das Ergebnis.</figcaption></figure>
<figure><img src='tour-vorhersage-danach.webp' alt='Dieselbe Aufgabe nach der Antwort, mit der Erklärung, warum Stellen in der Sonne gesprenkelt werden. Darunter ist jetzt das Bild erschienen: Kugeln auf einem Schachbrett, auf den hellen Feldern dunkle Sprenkel.'><figcaption>Danach erscheinen Erklärung und Bild.</figcaption></figure>
</div>

### Selbst schreiben statt abtippen

Hat die Seite erklärt, was eine kleine Funktion tun soll, schreiben die Lernenden sie zuerst selbst (»Jetzt bist du dran«). Wer hängen bleibt, holt sich Tipps in Stufen: erst zum Verstehen, dann zum Weg, dann ein Gerüst mit Lücken, nie die ganze Lösung. Der nächste Tipp erscheint erst eine Minute, nachdem der vorige aufgeklappt wurde, damit man mit jedem arbeitet, statt bis zum letzten durchzuklicken. Der Code der Seite kommt als letzte Stufe. Welche Tipps jemand gebraucht hat, merkt sich nur der eigene Browser; keine Lehrkraft sieht es.

<img class='full full-shadow' src='tour-tipp.webp' alt='Der Kasten »Jetzt bist du dran«: Schreib unter hit_sphere die Funktion hit_floor. Darunter ist der erste Tipp aufgeklappt; der nächste Tipp und die Lösung sind noch nicht zu sehen.'>

Oft prüft eine Aufgabe darunter den eigenen Code: Beim Raytracer übersetzt der Workspace die Datei der Lernenden und ruft ihre Funktion `hit_floor` mit ein paar Strahlen auf, auch bevor das Programm sie selbst benutzt. Wer sie so gelöst hat, kann den Code der Seite sofort aufklappen und mit dem eigenen vergleichen.

### Aufgaben, die nachsehen

Aufgaben stehen dort, wo die Lernenden gerade etwas getan haben, nicht gesammelt am Ende. Viele vergleichen nicht nur eine Antwort, sondern sehen im Workspace nach: ob eine Datei richtig liegt, ob das HTML einer Seite stimmt, ob alles committet ist, was eine selbst geschriebene Funktion zurückgibt oder ob der eigene Server antwortet. Dabei lesen sie nur und ändern nichts. Die Rückmeldung nennt den typischen Fehler, nicht bloß »falsch«, und die Erklärung zur Lösung erscheint erst nach einer richtigen Antwort.

<img class='full full-shadow' src='tour-rueckmeldung.webp' alt='Die Aufgabe »Wann kommt der Boden?« mit der Antwort −4. Die Rückmeldung sagt, dass das Minus in der Formel dazugehört und ein Strahl, der nach unten zeigt, den Boden bei positivem t trifft.'>

### Die Menschen hinter einer Sache

Hinter jeder Erfindung stehen Menschen. Wer ein Verfahren, eine Sprache oder ein Format erfunden hat, um das es in einem Abschnitt geht, bekommt dort einen kurzen Kasten: ein freies Foto, wenn es eines gibt, und zwei, drei Sätze, die an das anknüpfen, was die Lernenden gerade tun. Wo ein Abschnitt mit einem berühmten Aufsatz anfängt, verweist die Seite auf ihn, und wo ein Buch genau dort weitermacht, wo die Seite aufhört, auch auf das Buch. Alle Personen stehen gesammelt im Handbuch unter [»Personen«](/reference/personen).

<img class='full full-shadow' src='tour-menschen.webp' alt='Ein Kasten mit einem Dokument-Symbol: The Rendering Equation, James T. Kajiya, SIGGRAPH 1986, mit Links zum Aufsatz. Darunter ein Kasten mit einem Foto von Jim Kajiya und zwei Sätzen dazu, wer er ist.'>

### Was die Lehrkraft davon hat

Auf der Klassenseite sieht die Lehrkraft, wer welche Aufgabe gelöst hat, wie viele Versuche es waren und bei welcher Aufgabe gerade viele falsch liegen: ein guter Moment, kurz für alle zu erklären. Zu jeder Aktivität gibt es eine Notiz für Lehrkräfte mit dem Ablauf je Doppelstunde und dem, was oft schiefgeht. Baut eine Aktivität ein Programm Schritt für Schritt auf, kopiert ein Knopf unter den Bildern das ganze Programm bis zu dieser Stelle. Lernende sehen weder Notiz noch Knopf.

### Was eine Aktivität nicht ist

Sie ersetzt keinen Unterricht. Die Aktivitäten sind für eine Klasse mit einer Lehrkraft gedacht, die erklärt, hilft und mit den Lernenden spricht; sie nehmen ihr ab, jeden Handgriff einzeln vorzumachen. Schwierige Teile bleiben ausgearbeitete Beispiele, die die Seite erklärt. Und die Tipps sind eine Einladung, kein Schloss: Wer die Lösung unbedingt sucht, findet sie.

Wer selbst eine Aktivität schreiben möchte, findet die Anleitung im Repository: [»Eine Aktivität mit Aufgaben schreiben«](https://github.com/specht/workspace/blob/main/docs/aktivitaet-schreiben.md).
