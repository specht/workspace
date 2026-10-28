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

## Jetzt bist du dran

Wo die Seite gerade erklärt hat, was eine kleine Funktion tun soll, oft mit
ihrer Formel, schreiben die Lernenden sie zuerst selbst: ein Kasten
`hint own` (»Jetzt bist du dran:«) sagt, was und wo, und die Seite geht
danach mit ihrem eigenen Code weiter. Diesen Code (»Lösung anzeigen«) gibt es
erst als letzte Stufe nach den Tipps (»Tipps in Stufen« unten).
Der Kasten hieß zuerst »Erst selbst«; »Jetzt bist du dran« spricht die
Lernenden an und klingt nach einer Einladung, nicht nach einer Bedingung.

- Nur kleine, abgeschlossene Schritte, deren Idee schon erklärt ist; nichts,
  was erst eingeführt wird, und nichts, nach dem der Rest der Seite nicht mehr
  passt, wenn es anders aussieht.
- Wer hängen bleibt, nimmt die Tipps, einen nach dem anderen, und nach dem
  letzten den Code der Seite. So bleibt niemand für immer stecken, aber
  niemand kommt um das eigene Nachdenken herum.
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

Was die Antwort verrät, steht nicht gleich darunter: Ein Bild vom Ergebnis,
der Absatz, der es erklärt, oder ein Kasten, der es vorführt, wartet zwischen
`<!-- after: id -->` und `<!-- /after -->` (`AfterTask`): nach einer Vorhersage,
bis sie eine Antwort hat, richtig oder falsch (dann kommt das Warum); nach jeder
anderen Aufgabe, bis sie gelöst ist, sonst zeigte eine falsche Antwort die
richtige. Bis dahin steht dort »Hier geht es weiter, sobald du deine Antwort
oben abgegeben hast« oder »… sobald du die Aufgabe oben gelöst hast«. Das gilt
für alle, die antworten können, auch für Lehrkräfte: Sie sehen die Seite wie
ihre Klasse und öffnen die Stelle mit »Für Lehrkräfte: jetzt zeigen« (der
Maintainer, der sonst meinte, die Antwort stünde gleich darunter). Besucher
ohne Anmeldung sehen die Stelle nicht, sondern »Hier geht es weiter, sobald du
angemeldet bist und die Aufgabe oben beantwortet hast« (`tasks.js`): Sonst
läse eine Klasse die Antworten in einem privaten Fenster nach (der Maintainer,
Oktober 2026). Klassen ohne Aufgaben sehen alles.
Zuerst stand das Bild vom Ergebnis direkt unter der Vorhersage (»Licht mit
256 Farben« im Raycaster), und die Vermutung war geschenkt.

## Ausprobieren

Wo etwas gerade läuft, schlägt ein Kasten `hint try` eine kleine Änderung vor
und sagt, worauf man achten soll. Nichts wird geprüft, und die Seite geht mit
dem unveränderten Code weiter.

## Tipps in Stufen

Eine Aufgabe, an der man hängen bleiben kann, hat Tipps in Stufen statt eines
einzigen Tipps, und ein Kasten »Jetzt bist du dran« kann vor seiner Lösung Tipps
haben. Entschieden ist:

- **Die Stufen.** Erst verstehen, was verlangt ist (ein kleines Beispiel von
  Hand), dann der Weg in Worten oder Pseudocode, dann ein Gerüst mit Lücken.
  Zwei bis fünf Stufen.
- **Nie die ganze Lösung vorweg.** Auch der letzte Tipp ist ein Gerüst oder
  die eine fehlende Idee, nicht der fertige Code. Die Lösung einer Aufgabe
  (`solution`) erscheint erst nach dem Lösen. Den Code eines Kastens »Jetzt
  bist du dran« braucht die Seite, um weiterzugehen; er ist deshalb die
  letzte Stufe nach den Tipps und wartet wie sie. Prüft eine Aufgabe unter
  dem Kasten den eigenen Code (`data-check`), bringt das Lösen ihn sofort:
  Wer es selbst geschafft hat, darf vergleichen. Zuerst ließ sich dieser
  Code jederzeit aufklappen; das hat das eigene Schreiben übersprungen.
- **Eine Pause zwischen den Stufen, ohne Uhr.** Die nächste Stufe erscheint
  erst eine Minute, nachdem die vorige aufgeklappt wurde
  (`HintSteps::DELAY_SECONDS`), damit man mit jedem Tipp arbeitet, statt bis
  zum letzten durchzuklicken. Bis dahin ist sie nicht zu sehen, und keine Uhr
  zählt herunter: Wer eine Uhr sieht, schaut auf die Uhr statt auf den Tipp
  und den eigenen Code. Ist die Stufe da, steht ihr Titel fett, ohne
  Bewegung und ohne Ton.
- **Nicht verraten, wie viele Tipps es gibt.** Der erste heißt »Tipp«, jeder
  weitere »Nächster Tipp«, ohne Nummer und ohne »von 2«. Wer weiß, dass der
  letzte Tipp oder die Lösung gleich kommt, wartet darauf, statt mit dem Tipp
  zu arbeiten, den er hat.
- **Eine Einladung, kein Schloss.** Wer die Lösung unbedingt sucht, findet
  sie im Quelltext der Seite, in `stufen/` oder im Git einer Vorlage. Das ist
  in Ordnung: Die Pausen sollen das Nachdenken leicht machen, nicht das
  Abschreiben unmöglich. Mehr Aufwand, die Lösung zu verstecken, lohnt nicht.
- **Niemand sieht, wer welchen Tipp gebraucht hat.** Was aufgeklappt wurde,
  merkt sich nur der Browser (lokaler Speicher), damit ein Neuladen die Pause
  nicht neu beginnt; zum Server geht nichts, und Lehrkräfte sehen es nicht,
  auch nicht beim Nachahmen. Der Browser merkt es sich für jedes Konto
  getrennt: An einem Schulrechner, an dem nacheinander mehrere arbeiten,
  findet niemand die Tipps der Person davor schon aufgeklappt. Lernende
  sollen Hilfe nehmen, ohne sich zu fragen, wie das aussieht.
- **Hängen alle, hilft die Lehrkraft der Gruppe.** Die Tipps ersetzen nicht
  den Unterricht; wo viele an derselben Stelle hängen, ist das ein Thema für
  alle.

Mit Tipps in Stufen fängt man dort an, wo eine fehlende Funktion alles danach
blockiert: in den Labs (Huffman, LZW, Routenplaner, Rechner) und bei den
Kästen »Jetzt bist du dran«. Bei SQL leisten die Rückmeldungen schon viel davon.

## Kopierknöpfe

Ein Kopierknopf nur für Code, den man nicht tippen soll (Befehle zum
Einrichten, Adressen, Datentabellen), und für große Blöcke, die in eine Datei
gehören. Abtippen hilft beim Lesen; gelernt wird beim Selbstschreiben.

## Widgets

Ein Widget zeigt, wie ein Verfahren läuft, und löst keine Aufgabe seiner
Aktivität ([widgets.md](widgets.md)).

## Vorbilder

Je eine Seite, an der man sieht, wie es gemeint ist:

- »Jetzt bist du dran« mit einer Prüfung darunter: der Raytracer; in C mit den
  Headern von raylib und Ersatzfunktionen für Fenster und Tasten
  (`CCheck::RAYLIB_INCLUDE`, `setup`): die Klötzchenwelt. Ohne Prüfart (Shader,
  Lua) zeigt die Vorschau, ob es stimmt: GLSL, TIC-80.
- Änderungen statt ganzer Dateien: der Webserver.
- Tipps in Stufen: Huffman (Baum, Decodieren, kanonische Codes) und der
  Routenplaner; eine Aufgabe, die nur einen Namen ändert, braucht keine.
- Was eine Vorhersage verrät, wartet auf die Antwort (`<!-- after: … -->`):
  die Neo4j-Filmdatenbank, wo zwei Vorhersagen aneinanderhängen und die
  zweite Karte auf die erste wartet.
- Katas: die Sets mit Pip (`pip-1` bis `pip-3`) und Tests schreiben, wo die
  Lernenden die Prüfung selbst schreiben.
