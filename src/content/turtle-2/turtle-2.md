<div class='meta'>
icon: /images/icons/hilbert.svg
kind: uebung
duration: 1 Doppelstunde
languages: [ruby, python, javascript]
idea: Bäume, ein Farnblatt und Kurven, die aus kleineren Kopien ihrer selbst bestehen
curriculum: "Sek I: Digitale Bilder und Visualisierung (Turtle-Grafik, rekursive Algorithmen für Grafiken), Geschichte der Informatik (Lebensläufe und Leistungen bedeutender Persönlichkeiten); Oberstufe: Algorithmen und Datenstrukturen (rekursive Verfahren)"
topics: [rekursion, computergrafik, geschichte]
requires: [turtle-1, kata-recursion]
age: Oberstufe
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Turtle: Bäume und Kurven

<p class='abstract'>
Ein Baum besteht aus einem Stamm und zwei kleineren Bäumen, ein Farnblatt aus
einem Stück Stiel und drei kleineren Farnblättern. Mit einer Funktion, die
sich selbst aufruft, zeichnet die Schildkröte solche Bilder in wenigen
Zeilen: Bäume, die schief wachsen oder bis zum Boden hängen, ein Farnblatt,
das Sierpiński-Dreieck, die Koch-Kurve und die Hilbert-Kurve.
</p>

Die Befehle der Schildkröte kennst du aus [Turtle: Bilder aus
Befehlen](/turtle-1), Funktionen, die sich selbst aufrufen, aus den [Katas:
Rekursion](/kata-recursion). Hier kommt beides zusammen: Jede Figur auf
dieser Seite besteht aus kleineren Kopien ihrer selbst. Wie, zeigt vor jeder
Aufgabe ein Bild, das du Stufe für Stufe durchklickst; was aus der Stufe
davor stammt, ist farbig.

## Ein Baum

Ein Baum ist ein Stamm, an dem links und rechts ein kleinerer Baum wächst.
Jeder davon ist wieder ein Stamm mit zwei kleineren Bäumen, und so weiter,
bis die Bäume so klein sind, dass die Funktion aufhört. Klick dich durch
die Stufen: Jede Stufe besteht aus einem Stamm und zwei Bäumen der Stufe
davor, in Farbe.

<div class='turtle-levels' data-figure='tree'></div>

Wer die Funktion schreibt, muss also nur zwei Fragen beantworten: Was
zeichnet die kleinste Stufe? Und wie setzt sich eine Stufe aus der davor
zusammen? Den Rest erledigen die Aufrufe von selbst.

_task(tree)

_task(beautiful-tree)

Ein Baum, in dem links und rechts dasselbe passiert, sieht aus wie gezeichnet.
Andere Winkel und andere Längen für jede Seite, und er wächst wie ein
echter:

_task(asymmetric-tree)

_task(weeping-tree)

## Ein Farnblatt

Ein Farnblatt wächst wie ein Baum, nur anders: Sein Stiel biegt sich bei
jedem Schritt ein wenig, und an jedem Schritt wächst links und rechts ein
kleines Farnblatt. Was nach diesem Schritt kommt, ist wieder ein Farnblatt,
nur ein bisschen kleiner.

<div class='turtle-levels' data-figure='fern'></div>

_task(fern)

## Das Sierpiński-Dreieck

Wacław Sierpiński hat ein Dreieck beschrieben, das aus drei halb so großen
Dreiecken besteht, von denen jedes wieder aus dreien besteht:

<div class='turtle-levels' data-figure='sierpinski'></div>

_person(sierpinski)

_task(sierpinski)

## Die Koch-Kurve

Helge von Koch hat 1904 eine Kurve beschrieben, die aus vier Kurven mit
einem Drittel der Länge besteht. Je höher die Stufe, desto feiner die
Zacken; ein Stück von ihr sieht aus wie die ganze Kurve.

<div class='turtle-levels' data-figure='koch'></div>

_task(koch)

<!-- extra -->
## Die Hilbert-Kurve

David Hilbert hat 1891 eine Kurve beschrieben, die durch ein Quadrat läuft,
ohne sich je zu kreuzen, und mit jeder Stufe jeder Stelle des Quadrats
näherkommt. Ihre Stufe 1 ist ein U aus drei Linien. Jede höhere Stufe setzt
vier Kurven der Stufe davor an die Ecken dieses U, verbunden durch seine
drei Linien. Damit sie sich zu einer Linie verbinden, sind die erste und
die letzte gespiegelt.

<div class='turtle-levels' data-figure='hilbert'></div>

_task(hilbert)

## Dein eigener Baum

Zum Schluss ein Baum, den sich niemand vorher ausgedacht hat. Geprüft wird
hier nichts: Jeder Baum ist richtig.

_task(own-tree)

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass die Winkel und die Längen bei jedem
Ast ein wenig vom Zufall abhängen: <code>random(15, 35)</code> ist eine zufällige Zahl
von 15 bis 35, <code>random(0.6, 0.8)</code> eine Kommazahl dazwischen. Lass den Baum
dann mehrmals wachsen. Achte darauf, ob er so eher wie ein echter Baum
aussieht und welche Tiefe dafür reicht.
</div>
