<div class='meta'>
image: karte.webp
image_alt: "Drei Blätter, aufgefächert: eine Einladung mit blauen Punkten vor der Überschrift und einer orangen 1 daneben, ein Reisebericht mit Inhaltsverzeichnis und eine Zeitungsseite »Kraniche« in zwei Spalten mit einem Foto und einem grünen Haken"
card_title: "Textverarbeitung Schritt für Schritt"
kind: labor
duration: 1 Stunde (45 Minuten)
curriculum: "Sek I: Standardsoftware (Umgang mit Textverarbeitungssoftware); Medienbildung: Produzieren (Herstellung von Medienprodukten)"
topics: [standardsoftware]
language: LibreOffice / Word / Nextcloud Office
languages: [libreoffice, word, nextcloud]
reference: []
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Was das Auge nicht sieht

<p class='abstract'>
In zehn Blättern baust du Dokumente so, dass sie halten: von einer Einladung, die nur ausgedruckt ordentlich aussieht, bis zu deinem eigenen Artikel für die Zeitung der Klassenfahrt. Du arbeitest in LibreOffice, Word oder Nextcloud Office, und die Prüfung zeigt dir auf deiner eigenen Seite, was noch fehlt.
</p>

Die Klassenleitung der 7b hat eine Einladung zum Elternabend getippt, in Eile,
kurz vor der Pause. Ausgedruckt sieht sie ordentlich aus: die Überschrift in
der Mitte, das Datum rechts, darunter ein kurzer Brief.

<img class='paper' src='bilder/einladung.svg' alt='Die Einladung, wie sie ausgedruckt aussieht: »Einladung zum Elternabend« fett in der Mitte, darunter »Klassenfahrt der 7b an den Kranichsee«, rechts »2. März«, dann »Liebe Eltern,« und ein Brief aus drei Zeilen, zwei Sätze zu Ort und Zeit, Gruß und Unterschrift'>

Wie ein Dokument gebaut ist, sieht man auf dem Papier nicht. Man merkt es erst,
wenn sich etwas ändert.

Wie groß eine Schrift ist, misst die Textverarbeitung in **Punkt** (pt). Ein
Punkt ist ungefähr 0,35 mm; die 12 pt des Briefs sind gut 4 mm, das Feld, in
das ein Buchstabe mit allem passt, was nach oben und unten herausragt. Auch
Abstände misst sie in pt, Seitenränder und Bilder dagegen in cm.

<img class='drawing d-none d-lg-block' style='width: 35em' src='punkt-wide.svg' alt='Oben ein Zentimeter, vergrößert, mit Millimeterstrichen. Vom Anfang aus ist ein winziges blaues Stück markiert: 1 pt ≈ 0,35 mm; darüber eine orange Klammer bis gut 4 mm: 12 pt ≈ 4,2 mm. Daneben: 72 pt = 2,54 cm (ein Zoll), 28 pt ≈ 1 cm. Darunter groß »Hg« in einem blauen Feld, so hoch wie die orange Klammer lang ist: 12 pt ≈ 4,2 mm, das Feld, in das die Buchstaben passen; ein H ist etwas kleiner'>
<img class='drawing d-lg-none' style='width: 22.5em' src='punkt.svg' alt='Oben ein Zentimeter, vergrößert, mit Millimeterstrichen. Vom Anfang aus ist ein winziges blaues Stück markiert: 1 pt ≈ 0,35 mm; darüber eine orange Klammer bis gut 4 mm: 12 pt ≈ 4,2 mm. Darunter: 72 pt = 2,54 cm (ein Zoll), 28 pt ≈ 1 cm. Darunter groß »Hg« in einem blauen Feld, so hoch wie die orange Klammer lang ist: 12 pt ≈ 4,2 mm, das Feld, in das die Buchstaben passen; ein H ist etwas kleiner'>

_task(groesser)

<!-- after: groesser -->
So sieht die Einladung aus, wenn die Schrift überall eine Stufe größer wird:

<img class='paper' src='bilder/einladung-groesser.svg' alt='Dieselbe Einladung mit größerer Schrift: Die Überschrift steht rechts von der Mitte, das Datum am Anfang einer eigenen Zeile links, und im Brief folgt auf jede lange Zeile eine kurze mit ein, zwei Wörtern'>

Die Überschrift stand nie in der Mitte. Vor ihr stehen 38 Leerzeichen, so
viele, bis sie bei 16 pt ungefähr in der Mitte aussah. Werden die Leerzeichen
breiter, schieben sie die Überschrift weiter nach rechts. Vor dem Datum stehen
148 Leerzeichen; mit größerer Schrift passen sie nicht mehr in die Zeile, und
das Datum rutscht in die nächste. Und der Brief bricht so seltsam um, weil am
Ende jeder Zeile ein Zeilenumbruch von Hand steht: Das Programm bricht die zu
lange Zeile um, und gleich danach kommt der Umbruch, den jemand getippt hat.
<!-- /after -->

## Unsichtbares sichtbar machen

Hol dir die Einladung und öffne sie:

<p><a class='document-starter btn btn-primary' href='/api/task/document_starter/formatting-marks/einladung' download><i class='bi bi-file-earmark-word' aria-hidden='true'></i>Vorlage herunterladen:<span class='document-starter-name'>01 Einladung.docx</span></a></p>

Jede Textverarbeitung kann die Zeichen zeigen, die nicht gedruckt werden. Sie
heißen **Formatierungszeichen**.

<!-- language: word -->
In Word schaltest du sie im Register *Start* mit dem Knopf **¶** ein
(»Alle anzeigen«).
<!-- /language -->
<!-- language: libreoffice -->
In LibreOffice schaltest du sie unter *Ansicht* <i class='bi bi-chevron-right' aria-hidden='true'></i> *Formatierungszeichen*
ein, oder mit <kbd>Strg</kbd>+<kbd>F10</kbd>.
<!-- /language -->
<!-- language: nextcloud -->
In Nextcloud Office schaltest du sie unter *Ansicht* <i class='bi bi-chevron-right' aria-hidden='true'></i>
*Formatierungszeichen* ein.
<!-- /language -->

Dann siehst du drei Zeichen, die vorher nicht da waren:

<table class='table'>
<tr><th>Zeichen</th><th>Bedeutung</th><th>Taste</th></tr>
<tr><td><strong>·</strong></td><td>ein Leerzeichen</td><td><kbd>Leertaste</kbd></td></tr>
<tr><td><strong>¶</strong></td><td>das Ende eines Absatzes</td><td><kbd>Enter</kbd></td></tr>
<tr><td><strong>↵</strong></td><td>ein Zeilenumbruch von Hand: neue Zeile, aber derselbe Absatz</td><td><kbd>Shift</kbd>+<kbd>Enter</kbd></td></tr>
</table>

So sieht der Anfang der Einladung mit Formatierungszeichen aus:

<img class='paper' src='bilder/zeichen.svg' alt='Der Anfang der Einladung mit Formatierungszeichen: vor der Überschrift und vor der Zeile darunter eine lange Reihe blauer Punkte, eine noch längere vor »2. März«, zwischen den Wörtern je ein Punkt, am Ende jedes Absatzes ein blaues ¶, dazwischen zwei Zeilen, in denen nur ein ¶ steht'>

Ein **Absatz** ist alles bis zum nächsten ¶, auch wenn er über mehrere Zeilen
geht. Innerhalb eines Absatzes bricht das Programm die Zeilen selbst um, dort,
wo die Zeile voll ist.

_task(absaetze)

_task(zeilenende)

<div class='history' id='history-schreibmaschine'>
<p><strong>Von der Schreibmaschine zur Textverarbeitung.</strong> An einer Schreibmaschine schob ein Hebel am Ende jeder Zeile die Walze mit dem Papier zurück an den Anfang und eine Zeile weiter, der Wagenrücklauf. Daher kommt das Zeichen ↵, und daher tippten viele zwei Leerzeichen nach dem Punkt: Auf der Schreibmaschine war jeder Buchstabe gleich breit, und ein Leerzeichen allein fiel kaum auf. 1955 schlug Ulrich Steinhilper, Verkäufer von Schreibmaschinen bei IBM in Deutschland, vor, Texte so mit Maschinen zu bearbeiten wie Zahlen, eine <em>Textverarbeitung</em> neben der Datenverarbeitung; IBM ehrte ihn 1972 für das Wort. Die Amerikanerin Evelyn Berezin baute 1968 die »Data Secretary«, ein Gerät so groß wie ein kleiner Kühlschrank, das getippten Text auf einem Band speicherte: Wer sich vertippt hatte, musste nicht mehr die ganze Seite neu schreiben.</p>
</div>

## Mitte und Rand

Ob ein Absatz links, in der Mitte oder rechts steht, ist seine
**Ausrichtung**. Sie gehört zum Absatz, nicht zu den Leerzeichen davor: Ein
zentrierter Absatz bleibt in der Mitte, wie groß die Schrift auch wird.

<div class='gallery centered small'>
<figure><img class='drawing' src='ausrichtung-links.svg' alt='Ein Absatz aus vier Zeilen, linksbündig: Alle Zeilen beginnen am linken Rand.'><figcaption>linksbündig<br><kbd>Strg</kbd>+<kbd>L</kbd></figcaption></figure>
<figure><img class='drawing' src='ausrichtung-mitte.svg' alt='Derselbe Absatz zentriert: Jede Zeile steht in der Mitte.'><figcaption>zentriert<br><kbd>Strg</kbd>+<kbd>E</kbd></figcaption></figure>
<figure><img class='drawing' src='ausrichtung-rechts.svg' alt='Derselbe Absatz rechtsbündig: Alle Zeilen enden am rechten Rand.'><figcaption>rechtsbündig<br><kbd>Strg</kbd>+<kbd>R</kbd></figcaption></figure>
</div>

<!-- language: word -->
Die Knöpfe dafür stehen in Word im Register *Start*, in der Gruppe *Absatz*.
<!-- /language -->
<!-- language: libreoffice -->
Die Knöpfe dafür stehen in LibreOffice in der Leiste oben, neben fett und
kursiv.
<!-- /language -->
<!-- language: nextcloud -->
Die Knöpfe dafür stehen in Nextcloud Office im Register *Start*, neben fett
und kursiv.
<!-- /language -->

**Jetzt bist du dran:** Lösch die Leerzeichen vor der Überschrift und vor der
Zeile darunter und zentriere beide Absätze. Lösch die Leerzeichen vor dem
Datum und richte es rechtsbündig aus. Der Cursor muss dafür nur irgendwo im
Absatz stehen; markieren musst du nichts.

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell den Cursor in die Überschrift und drück abwechselnd <kbd>Strg</kbd>+<kbd>L</kbd>, <kbd>Strg</kbd>+<kbd>E</kbd> und <kbd>Strg</kbd>+<kbd>R</kbd>. Achte darauf, was mit den Punkten (·) vor dem Text passiert, solange sie noch da sind.
</div>

## Abstand statt leerer Absätze

Die Lücken in der Einladung sind leere Absätze: ein ¶ allein in einer Zeile.
Ein leerer Absatz ist so hoch wie eine Zeile seiner Schrift und wächst mit
ihr. Abstand zwischen zwei Absätzen gibst du besser dem Absatz selbst: Jeder
Absatz hat einen **Abstand davor** und einen **Abstand danach**, in Punkt (pt).
12 pt sind ungefähr eine Zeile.

<img class='drawing d-none d-lg-block' style='width: 35em' src='abstand-wide.svg' alt='Zwei Blätter nebeneinander. Links »mit einem leeren Absatz«: unter »Liebe Eltern,¶« eine Zeile, in der nur ein ¶ steht, markiert als »leerer Absatz«, dann der Brief. Rechts »mit Abstand danach«: »Liebe Eltern,¶« in einem blauen Feld, das unter der Zeile weitergeht, mit einer Klammer »12 pt« daneben, dann der Brief, gleich weit entfernt wie links'>
<img class='drawing d-lg-none' style='width: 16.5em' src='abstand.svg' alt='Zwei Blätter untereinander. Oben »mit einem leeren Absatz«: unter »Liebe Eltern,¶« eine Zeile, in der nur ein ¶ steht, markiert als »leerer Absatz«, dann der Brief. Unten »mit Abstand danach«: »Liebe Eltern,¶« in einem blauen Feld, das unter der Zeile weitergeht, mit einer Klammer »12 pt« daneben, dann der Brief, gleich weit entfernt wie links'>

<!-- language: word -->
In Word stellst du den Abstand im Register *Layout* ein, im Feld *Abstand:
Nach*.
<!-- /language -->
<!-- language: libreoffice -->
In LibreOffice stellst du den Abstand unter *Format* <i class='bi bi-chevron-right' aria-hidden='true'></i> *Absatz* <i class='bi bi-chevron-right' aria-hidden='true'></i>
*Einzüge und Abstände* ein, im Feld *Unter dem Absatz*.
<!-- /language -->
<!-- language: nextcloud -->
In Nextcloud Office stellst du den Abstand unter *Format* <i class='bi bi-chevron-right' aria-hidden='true'></i> *Absatz* ein,
im Feld *Unter dem Absatz*.
<!-- /language -->

**Jetzt bist du dran:** Lösch die leeren Absätze. Gib dann jedem Absatz, nach
dem eine Lücke war, Abstand danach: 12 pt nach »Liebe Eltern,«, nach dem Brief
und nach dem Satz zum Elternabend, mehr vor dem Gruß, zum Beispiel 24 pt nach
»Bitte sagen Sie uns …«. Zwischen »Mit freundlichen Grüßen« und der
Unterschrift bleibt kein Abstand.

<div class='hint'>
<strong>Tipp:</strong> Markierst du mehrere Absätze, bekommen alle denselben Abstand auf einmal.
</div>

## Zeilen, die das Programm umbricht

Im Brief steht am Ende jeder Zeile ein ↵. Lösch beide und setz stattdessen ein
Leerzeichen, wo vorher keins war; dann bricht das Programm die Zeilen selbst
um.

Bleiben noch Leerzeichen, die doppelt stehen: nach einem Punkt hat jemand zwei
getippt, wie früher auf der Schreibmaschine. Die findest du nicht mit den
Augen, sondern mit **Suchen und Ersetzen** (<kbd>Strg</kbd>+<kbd>H</kbd>):
Such nach zwei Leerzeichen und ersetze sie durch eins, so lange, bis das
Programm keine mehr findet.

## Prüfen

Speichere die Einladung und lass sie prüfen. Die Prüfung liest, wie dein
Dokument gebaut ist, und markiert auf deiner Seite jede Stelle, an der noch
etwas fehlt. Prüfen kannst du, so oft du willst.

_task(einladung)

<!-- after: einladung -->
<div class='hint try'>
<strong>Ausprobieren:</strong> Mach die Schrift jetzt noch einmal überall eine Stufe größer, wie in der Vermutung oben. Achte darauf, wo die Überschrift und das Datum stehen und wo die Zeilen des Briefs umbrechen. Danach mach es mit <kbd>Strg</kbd>+<kbd>Z</kbd> rückgängig.
</div>

So sieht die gebaute Einladung mit größerer Schrift aus:

<img class='paper' src='bilder/fertig-groesser.svg' alt='Die reparierte Einladung mit größerer Schrift: Überschrift und Zeile darunter genau in der Mitte, das Datum am rechten Rand, der Brief bricht gleichmäßig um, die Abstände zwischen den Absätzen sind gleich geblieben'>

_task(ausdruck)
<!-- /after -->

<div class='history' id='history-libreoffice'>
<p><strong>Woher LibreOffice kommt.</strong> 1985 gründete Marco Börries mit 16 Jahren, noch als Schüler, in Lüneburg die Firma Star Division. Ihr Textprogramm StarWriter lief bald auch auf den Heimcomputern von Schneider; daraus wurde das Paket StarOffice. 1999 kaufte die Firma Sun StarOffice und gab seinen Quelltext frei, als OpenOffice.org. 2010 machten Entwicklerinnen und Entwickler daraus LibreOffice, mit der Stiftung The Document Foundation, die seit 2012 ihren Sitz in Berlin hat. LibreOffice ist freie Software: Jede und jeder darf es benutzen, weitergeben und verändern, kostenlos.</p>
</div>

