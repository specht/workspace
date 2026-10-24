# Selbst denken, nicht abschreiben

Was Lernende in einer Aktivität tun, entscheidet, was sie lernen. Eine Seite,
die ein schönes Programm Block für Block vorgibt, führt zu einem schönen Bild,
aber nicht unbedingt zu jemandem, der das Programm schreiben könnte. Diese
Seite hält fest, wie die Aktivitäten Lernende selbst denken lassen, warum, und
welche Entscheidungen dazu gefallen sind. Die Regeln selbst stehen dort, wo
sie gebraucht werden: [aktivitaet-schreiben.md](aktivitaet-schreiben.md) (wie
man es macht), [aufgaben.md](aufgaben.md) (die Felder und Prüfarten) und
[checkliste.md](checkliste.md) (was jede Aktivität erfüllen muss).

Das Muster dahinter ist PRIMM (Predict, Run, Investigate, Modify, Make;
Sentance und Waite): erst vermuten, dann ausführen, untersuchen, ändern und am
Ende selbst schreiben. Die geführten Projekte (Raytracer, Raycaster,
Webserver …) waren stark beim Ausführen und Untersuchen und schwach beim
Selbstschreiben. Die Werkzeuge unten gleichen das aus, ohne das Geführte
aufzugeben: Schwierige Teile (Brechung, Fresnel, Path Tracing) bleiben
ausgearbeitete Beispiele, die die Seite erklärt.

## Erst selbst schreiben

Wo die Seite gerade erklärt hat, was eine kleine Funktion tun soll, oft mit
ihrer Formel, schreiben die Lernenden sie zuerst selbst: ein Kasten
`hint own` (»Erst selbst:«) sagt, was und wo, der Code der Seite folgt
zugeklappt unter »Lösung anzeigen«, und die Seite geht mit diesem Code weiter.

- Nur kleine, abgeschlossene Schritte, deren Idee schon erklärt ist; nichts,
  was erst eingeführt wird, und nichts, nach dem der Rest der Seite nicht mehr
  passt, wenn es anders aussieht.
- Wer hängen bleibt, klappt auf und macht weiter. Niemand bleibt stecken,
  nur weil ein Schritt nicht gelingt.
- Wo es geht, steht unter der Lösung eine Aufgabe, die den eigenen Code prüft
  (`ruby`, `python`, `c`), auch bevor das Programm ihn aufruft.
- Eine Lücke im Gerüst (`// hier: forward, right und up`, in Python `...`) ist
  oft besser als ein leeres Blatt: Die Lernenden sehen, wohin ihr Code gehört.

## Nicht die ganze Datei noch einmal

Wird ein Programm über mehrere Schritte größer, zeigt die Seite, was sich
ändert, mit Kontextzeilen (`~ `) drumherum, statt die ganze Datei jedes Mal
neu zum Kopieren. Eine ganze Datei nur, wenn sie neu ist oder sich fast jede
Zeile ändert, und dann wo möglich mit einer Lücke für das, was die Lernenden
selbst schreiben. Der Grund: In einer neu kopierten Datei geht die eine neue
Idee unter, und eine Prüfung, die danach `Thread.new` in der Datei findet,
findet nur, was eingefügt wurde.

## Prüfen, was der Code tut

Eine Prüfung sieht besser nach, was ein Programm tut, als ob ein Wort darin
steht: `ruby`, `python` und `c` rufen die Funktionen der Lernenden auf,
`command` lässt einen Befehl der Lernenden gegen ihren laufenden Server
laufen (»Dein Server grüßt« im Webserver). Rückmeldungen nennen den typischen
Fehler, nicht nur, dass etwas falsch ist (`say`, `feedback`). Diese
Rückmeldungen sind die beste Hilfe, die der Workspace geben kann, weil sie auf
das antworten, was jemand wirklich geschrieben hat.

## Vermuten vor dem Ausprobieren

Vor einem Ergebnis, das man vorhersagen kann, steht eine Vorhersage
(`prediction: true`): Was zeigt die Spiegelkugel mit `MAX_DEPTH` 0? Werfen
die Kugeln schon Schatten? Wer erst vermutet, schaut beim Ausprobieren genau
hin. Nur der erste Versuch zählt fürs Klassenboard.

## Ausprobieren

Wo etwas gerade läuft, schlägt ein Kasten `hint try` eine kleine Änderung vor
und sagt, worauf man achten soll. Nichts wird geprüft, und die Seite geht mit
dem unveränderten Code weiter.

## Tipps in Stufen

Eine Aufgabe, an der man hängen bleiben kann, hat Tipps in Stufen statt eines
einzigen Tipps, und ein Kasten »Erst selbst« kann vor seiner Lösung Tipps
haben. Entschieden ist:

- **Die Stufen.** Erst verstehen, was verlangt ist (ein kleines Beispiel von
  Hand), dann der Weg in Worten oder Pseudocode, dann ein Gerüst mit Lücken.
  Zwei bis fünf Stufen.
- **Nie die ganze Lösung.** Auch die letzte Stufe ist ein Gerüst oder die
  eine fehlende Idee, nicht der fertige Code. Die Lösung einer Aufgabe
  (`solution`) erscheint weiterhin erst nach dem Lösen. Der Code eines
  Kastens »Erst selbst« ist davon ausgenommen: Die Seite baut auf ihm auf.
- **Eine Pause zwischen den Stufen.** Die nächste Stufe erscheint erst eine
  Minute, nachdem die vorige aufgeklappt wurde (`HintSteps::DELAY_SECONDS`),
  damit man über jeden Tipp nachdenkt, statt bis zum letzten durchzuklicken.
  Die Wartezeit steht da, als Uhr.
- **Niemand sieht, wer welchen Tipp gebraucht hat.** Was aufgeklappt wurde,
  merkt sich nur der Browser (lokaler Speicher), damit ein Neuladen die Pause
  nicht neu beginnt; zum Server geht nichts, und Lehrkräfte sehen es nicht.
  Lernende sollen Hilfe nehmen, ohne sich zu fragen, wie das aussieht.
- **Hängen alle, hilft die Lehrkraft der Gruppe.** Die Tipps ersetzen nicht
  den Unterricht; wo viele an derselben Stelle hängen, ist das ein Thema für
  alle.

Mit Tipps in Stufen fängt man dort an, wo eine fehlende Funktion alles danach
blockiert: in den Labs (Huffman, LZW, Routenplaner, Rechner) und bei den
Kästen »Erst selbst«. Bei SQL leisten die Rückmeldungen schon viel davon.

## Kopierknöpfe

Ein Kopierknopf nur für Code, den man nicht tippen soll (Befehle zum
Einrichten, Adressen, Datentabellen), und für große Blöcke, die in eine Datei
gehören. Abtippen hilft beim Lesen; gelernt wird beim Selbstschreiben.

## Widgets

Ein Widget zeigt, wie ein Verfahren läuft, und löst keine Aufgabe seiner
Aktivität ([widgets.md](widgets.md)).

## Wo es schon umgesetzt ist

- »Erst selbst« mit Prüfungen darunter: Raytracer, Raycaster, Webserver,
  Pixelwand, Wetter; eine Prüfung `c` auch im GIF-Decoder.
- Änderungen statt ganzer Dateien: Webserver, Pixelwand.
- Tipps in Stufen: Huffman (Baum, Decodieren, kanonische Codes) und die
  Kästen »Erst selbst« im Raytracer.
- Noch nicht durchgesehen: glsl, raylib, tic80 und die übrigen Labs.
