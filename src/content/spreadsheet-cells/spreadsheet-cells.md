<div class='meta'>
image: karte.webp
image_alt: "Drei Blätter, aufgefächert, auf Weiß: eine Tabelle mit einer orange markierten Zelle und der grünen Formel =B5*C5 darunter, ein Kreisdiagramm »Wofür ein Kind bezahlt« und ein Säulendiagramm »Wie hat es gefallen?« mit grünen Säulen"
card_title: "Tabellenkalkulation Schritt für Schritt"
kind: labor
duration: 1 Stunde (45 Minuten)
curriculum: "Sek I: Standardsoftware (Tabellenkalkulation)"
topics: [standardsoftware]
language: LibreOffice / Excel / Nextcloud Office
languages: [libreoffice, word, nextcloud]
office: sheet
reference: []
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Zellen, die rechnen

<p class='abstract'>
In sieben Blättern rechnest du mit einer Tabellenkalkulation: von den Kosten der Klassenfahrt, die ein Taschenrechner ausgerechnet hat, bis zur Abrechnung, die selbst rechnet, mit Diagrammen. Du arbeitest in LibreOffice, Excel oder Nextcloud Office, und die Prüfung zeigt dir auf deiner eigenen Seite, was noch fehlt.
</p>

Für die Klassenfahrt der 7b hat ein Elternteil die Kosten zusammengestellt,
in einer Tabelle: jeder Posten, wie oft er bezahlt wird, sein Preis und die
Summe, ganz unten der Gesamtbetrag.

<img class='paper' src='bilder/kosten.svg' alt='Eine Tabelle »Kosten der Klassenfahrt« mit den Spalten Posten, Anzahl, Preis und Summe: Bus 1 mal 1.250,00, Jugendherberge 29 mal 148,00, Kletterwald 27 mal 24,50, Schifffahrt 29 mal 9,00, Moormuseum 29 mal 4,50, ganz unten Gesamt 6.595,00'>

Die Summen und den Gesamtbetrag hat das Elternteil mit dem Taschenrechner
ausgerechnet und in die Tabelle getippt.

_task(vorher)

<!-- after: vorher -->
Nichts ändert sich: In den Summen stehen getippte Zahlen. Die Tabelle weiß
nicht, dass 4.292,00 einmal 29 mal 148,00 war. Wer den Preis ändert, muss
jede Summe, die davon abhängt, selbst neu ausrechnen und tippen, und vergisst
leicht eine.
<!-- /after -->

<div class='history' id='history-visicalc'>
<p><strong>Die Tafel, die selbst rechnet</strong>, 1979. Bevor es Tabellenkalkulationen gab, rechnete man solche Tabellen auf Papier, mit dem Taschenrechner, und änderte sich eine Zahl, radierte man jede Zahl aus, die von ihr abhing. Dan Bricklin sah einem Professor zu, der das an der Tafel tat, und schrieb mit Bob Frankston ein Programm, das nach jeder Änderung selbst neu rechnet: VisiCalc.</p>
</div>

_person(bricklin-frankston)

## Zellen und Adressen

Eine Tabelle besteht aus **Zellen**. Die Spalten heißen A, B, C …, die Zeilen
1, 2, 3 …, und jede Zelle hat eine **Adresse** aus beidem: erst die Spalte,
dann die Zeile. In C6 steht der Preis des Kletterwalds.

<img class='drawing d-none d-lg-block' style='width: 32em' src='adresse-wide.svg' alt='Ein Stück einer Tabelle mit den Spalten A bis D und den Zeilen 1 bis 6. Spalte C und Zeile 4 sind blau hinterlegt; wo sie sich kreuzen, ist die Zelle C4 orange eingerahmt: Zelle C4, Spalte C, Zeile 4'>
<img class='drawing d-lg-none' style='width: 22.5em' src='adresse.svg' alt='Ein Stück einer Tabelle mit den Spalten A bis D und den Zeilen 1 bis 6. Spalte C und Zeile 4 sind blau hinterlegt; wo sie sich kreuzen, ist die Zelle C4 orange eingerahmt: Zelle C4, Spalte C, Zeile 4'>

Hol dir die Tabelle und öffne sie:

<p><a class='document-starter btn btn-primary' href='/spreadsheet-cells/01%20Kosten.xlsx' download><i class='bi bi-file-earmark-spreadsheet' aria-hidden='true'></i>Vorlage herunterladen:<span class='document-starter-name'>01 Kosten.xlsx</span></a></p>

_task(ablesen)

## Formeln

Eine **Formel** rechnet mit den Zahlen in anderen Zellen. Sie beginnt mit
einem Gleichheitszeichen; für die Summe der Jugendherberge schreibst du in D5:
`=B5*C5`, Anzahl mal Preis. Das Sternchen `*` ist das Malzeichen, `/` das
Geteiltzeichen. Nach <kbd>Enter</kbd> zeigt die Zelle das Ergebnis, die
Formel steht weiter in ihr. Ändert sich B5 oder C5, rechnet die Zelle neu.

<img class='drawing d-none d-lg-block' style='width: 35em' src='formel-wide.svg' alt='Zweimal die Zeile 5 mit 29, 148,00 und 4.292,00. Oben »getippt«: In D5 steht die Zahl 4292, die nichts von B5 und C5 weiß. Unten »eine Formel«: In D5 steht =B5*C5, zwei blaue Bögen führen von B5 und C5 in D5'>
<img class='drawing d-lg-none' style='width: 22.5em' src='formel.svg' alt='Zweimal die Zeile 5 mit 29, 148,00 und 4.292,00. Oben »getippt«: In D5 steht die Zahl 4292, die nichts von B5 und C5 weiß. Unten »eine Formel«: In D5 steht =B5*C5, zwei blaue Bögen führen von B5 und C5 in D5'>

<!-- language: libreoffice -->
Welche Formel in einer Zelle steht, zeigt LibreOffice in der **Eingabezeile**
über der Tabelle, sobald du die Zelle anklickst. Dort kannst du sie auch
ändern.
<!-- /language -->
<!-- language: word -->
Welche Formel in einer Zelle steht, zeigt Excel in der **Bearbeitungsleiste**
über der Tabelle, sobald du die Zelle anklickst. Dort kannst du sie auch
ändern. Fehlt die Leiste, schaltest du sie im Register *Ansicht*, Gruppe
*Anzeigen*, mit dem Haken bei *Bearbeitungsleiste* ein.
<!-- /language -->
<!-- language: nextcloud -->
Welche Formel in einer Zelle steht, zeigt Nextcloud Office in der Zeile über
der Tabelle, sobald du die Zelle anklickst. Dort kannst du sie auch ändern.
<!-- /language -->

**Jetzt bist du dran:** Schreib in D4 die Formel für den Bus, `=B4*C4`, und
drück <kbd>Enter</kbd>: Erst dann rechnet die Zelle, und der Cursor springt
eine Zeile tiefer. Schreib in D5 die Formel für die Jugendherberge. Ändere dann den Preis der Jugendherberge in
C5 auf 150 und sieh nach, was in D5 passiert. Setz ihn danach wieder auf 148.

## Nach unten ausfüllen

Für jede Zeile eine Formel zu tippen, wäre mühsam. Unten rechts an der
markierten Zelle sitzt ein kleines Quadrat. Ziehst du es nach unten, schreibt
das Programm die Formel in die Zellen darunter und passt sie an.

<img class='drawing d-none d-lg-block' style='width: 40em' src='quadrat-wide.svg' alt='Links, vergrößert: die Zellen C4 bis D5, D4 orange eingerahmt, an seiner Ecke unten rechts ein kleines orangefarbenes Quadrat, darauf der Mauszeiger als dünnes Kreuz. Rechts: die Spalten C und D, Zeilen 3 bis 8. D4 ist markiert, ein gestrichelter Rahmen reicht von D4 bis D8, der Mauszeiger an seiner Ecke unten, ein Pfeil nach unten; D5 bis D8 sind noch leer'>
<img class='drawing d-lg-none' style='width: 21.75em' src='quadrat.svg' alt='Oben, vergrößert: die Zellen C4 bis D5, D4 orange eingerahmt, an seiner Ecke unten rechts ein kleines orangefarbenes Quadrat, darauf der Mauszeiger als dünnes Kreuz. Unten: die Spalten C und D, Zeilen 3 bis 8. D4 ist markiert, ein gestrichelter Rahmen reicht von D4 bis D8, der Mauszeiger an seiner Ecke unten, ein Pfeil nach unten; D5 bis D8 sind noch leer'>

_task(kopiert)

<!-- after: kopiert -->
<img class='drawing d-none d-lg-block' style='width: 33em' src='ausfuellen-wide.svg' alt='Die Spalten B, C und D, Zeilen 4 bis 7. In D4 steht =B4*C4, orange eingerahmt, das kleine Quadrat unten rechts daran, ein Pfeil nach unten. Darunter in D5 =B5*C5, in D6 =B6*C6, in D7 =B7*C7: jede Zeile rechnet mit ihrer eigenen Zeile'>
<img class='drawing d-lg-none' style='width: 22.5em' src='ausfuellen.svg' alt='Die Spalten B, C und D, Zeilen 4 bis 7. In D4 steht =B4*C4, orange eingerahmt, das kleine Quadrat unten rechts daran, ein Pfeil nach unten. Darunter in D5 =B5*C5, in D6 =B6*C6, in D7 =B7*C7: jede Zeile rechnet mit ihrer eigenen Zeile'>

Die Formel merkt sich nicht die Adressen B4 und C4, sondern den Weg dorthin:
»zwei Spalten links und eine links von mir, in meiner Zeile«. In jeder Zeile
führt dieser Weg zu den Zahlen dieser Zeile.
<!-- /after -->

**Jetzt bist du dran:** Füll die Formel aus D4 nach unten aus, bis D8:

1. Klick auf D4; nach <kbd>Enter</kbd> steht der Cursor eine Zeile tiefer.
2. Zeig mit der Maus auf das kleine Quadrat unten rechts an D4, bis der Zeiger
   ein dünnes schwarzes **+** ist. Ein Pfeil mit vier Spitzen heißt dagegen:
   Du würdest die Zelle verschieben, statt die Formel auszufüllen.
3. Zieh mit gedrückter Maustaste bis D8 und lass los, nicht weiter.

Ist etwas verrutscht, macht <kbd>Strg</kbd>+<kbd>Z</kbd> es rückgängig.

## Die Summe

Den Gesamtbetrag könntest du als `=D4+D5+D6+D7+D8` schreiben. Kürzer und
sicherer ist die Funktion **SUMME**: `=SUMME(D4:D8)` zählt alles von D4 bis
D8 zusammen. Der Doppelpunkt heißt »bis«. Kommt eine Zeile dazwischen dazu,
wächst der Bereich mit.

<!-- language: libreoffice -->
In LibreOffice tippst du die Formel selbst, oder du stellst dich in D9 und
klickst über der Tabelle auf **Σ** (*Funktion auswählen*) und dann *Summe*:
LibreOffice schlägt den Bereich darüber vor. Sieh nach, ob er D4 bis D8 ist,
und drück <kbd>Enter</kbd>.
<!-- /language -->
<!-- language: word -->
In Excel tippst du die Formel selbst, oder du stellst dich in D9 und klickst im
Register *Start* auf **Σ** *AutoSumme*: Excel schlägt den Bereich darüber vor.
Sieh nach, ob er D4 bis D8 ist, und drück <kbd>Enter</kbd>.
<!-- /language -->
<!-- language: nextcloud -->
In Nextcloud Office tippst du die Formel selbst, oder du stellst dich in D9
und klickst über der Tabelle auf **Σ** und dann *Summe*: Das Programm schlägt
den Bereich darüber vor. Sieh nach, ob er D4 bis D8 ist, und drück
<kbd>Enter</kbd>.
<!-- /language -->

**Jetzt bist du dran:** Schreib in D9 den Gesamtbetrag mit SUMME.

## Prüfen

Speichere die Tabelle und lass sie prüfen. Die Prüfung ändert in ihrer Kopie
eine Zahl und rechnet neu: Rechnet deine Tabelle mit, stimmt alles.

_task(kosten)

<!-- after: kosten -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Füg zwischen Zeile 7 und 8 eine neue Zeile ein (Rechtsklick auf die Zeilennummer 8, <em>Zeilen oberhalb einfügen</em> oder <em>Einfügen</em>) und trag einen Posten ein, etwa »Eis« für 29 Personen zu 2,00. Füll die Formel in D aus und achte auf den Bereich in der SUMME.
</div>
<!-- /after -->

<!-- extra -->
## Üben und knobeln

Drei Tabellen für alle, die mehr wollen: eine zum Üben, eine mit zwei neuen
Handgriffen und eine zum Suchen.

### Noch eine Runde

Den Einkauf fürs Sommerfest der 7b hat jemand mit dem Taschenrechner
ausgerechnet, gebaut wie die Kosten der Klassenfahrt. Lass die Tabelle selbst
rechnen, diesmal ohne oben nachzusehen.

_task(runde)

### Zwei Handgriffe

In die Kosten der Klassenfahrt kommt noch ein Eis für alle: 29 Personen zu
2,00 €. Die Vorlage dazu rechnet schon und hat unter dem Gesamtbetrag eine
Zeile mit der Zahl der Personen.

Eine neue Zeile fügst du über einer anderen ein: Klick mit der rechten
Maustaste auf deren Zeilennummer.

<!-- language: libreoffice -->
In LibreOffice wählst du dann *Zeilen oberhalb einfügen*.
<!-- /language -->
<!-- language: word -->
In Excel wählst du dann *Einfügen*.
<!-- /language -->
<!-- language: nextcloud -->
In Nextcloud Office wählst du dann *Zeilen oberhalb einfügen*.
<!-- /language -->

Die Formel der neuen Zeile holst du am kleinen Quadrat aus der Zeile darüber.
Sieh dir danach den Gesamtbetrag an: Rechnet seine SUMME die neue Zeile mit?

Was jede Person bezahlt, ist der Gesamtbetrag geteilt durch die Zahl der
Personen. Eine Formel darf mit dem Ergebnis einer anderen Formel rechnen;
ändert sich ein Preis, rechnen beide neu.

_task(handgriffe)

### Fünf Fallen

Die Kosten des Wandertags sehen fertig aus: Jede Zelle in Spalte D zeigt eine
Zahl. Fünf Zellen sind trotzdem falsch gebaut, alle von der Art wie auf
diesem Blatt. Die Prüfung sagt dir nur, wie viele du gefunden hast, und jede
Falle hat ihre eigenen Tipps.

_task(fallen)
