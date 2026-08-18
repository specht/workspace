<div class='meta'>
image: latex-logo.png
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3,h4">
</div>


# Dokumente schreiben mit LaTeX

<p class='abstract'>
In diesem Tutorial lernst du LaTeX kennen – ein Werkzeug, mit dem man Texte nicht „zusammenschiebt“, sondern strukturiert beschreibt.
Anstatt Schriftgrößen, Abstände und Formatierungen per Hand einzustellen, sagst du LaTeX, was etwas ist (Überschrift, Absatz, Tabelle, Formel) – und LaTeX kümmert sich darum, wie es aussieht.
</p>

LaTeX wird häufig an Universitäten, in der Wissenschaft und bei technischen Berufen verwendet, zum Beispiel für Protokolle, Facharbeiten, Präsentationen oder Bewerbungen. Aber auch für die Schule ist es sehr praktisch: Texte sehen automatisch ordentlich aus, Formeln sind klar lesbar, und Änderungen wirken sich sofort auf das ganze Dokument aus.

In diesem Tutorial lernst du die Grundlagen von LaTeX anhand einer **Bewerbung**, eines **Lebenslaufs** und einer **schriftlichen Ausarbeitung** kennen.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp' alt=''>

## Repository klonen

Für diese Anleitung brauchst du ein Repository, das du klonen kannst, indem du auf den blauen Button »Clone Repository« klickst. Gib die folgende URL ein und bestätige mit <kbd>Enter</kbd>:

```bash
https://github.com/specht/latex-tutorial.git
```

<img class='full' src='git-clone.webp' alt=''>

Als nächstes musst du angeben, in welches Verzeichnis du das Repository klonen möchtest. Bestätige den Standardpfad `/workspace/` mit <kbd>Enter</kbd>.

<img class='full' src='confirm-clone-path.webp' alt=''>

Beantworte die Frage »Would you like to open the repository?« mit »Open«.

<img class='full' src='open-yes-no.webp' alt=''>

## LaTeX Workshop installieren

Um uns die Arbeit mit LaTeX zu erleichtern, verwenden wir die Erweiterung »LaTeX Workshop«. Diese Erweiterung bietet viele nützliche Funktionen, wie z. B. das automatische Compilieren von LaTeX-Dokumenten, Syntaxhervorhebung und Vorschauen. Klicke dazu auf das Extensions-Symbol <img src='../basics/extensions.webp' style='border-radius: 4px; height: 1.5em;' alt=''> in der Seitenleiste und suche nach »LaTeX Workshop«. Klicke auf »Install«, um die Erweiterung zu installieren.

<img class='full' src='latex-workshop-ext.webp' alt=''>

Wechsle nun wieder zurück zum Explorer, indem du auf das Explorer-Symbol <img src='../basics/explorer.webp' style='border-radius: 4px; height: 1.5em;' alt=''> in der Seitenleiste klickst. Öffne als erstes die Datei `hello.tex`. Diese Datei enthält ein einfaches LaTeX-Dokument, das du als Ausgangspunkt verwenden kannst. Schau dir den Inhalt der Datei an und versuche zu verstehen, was die verschiedenen Teile bedeuten:

_include_file(hello.tex, tex)

Rechts oben findest du einen kleinen Button: »Build LaTeX project« 
<img src='build-latex-project.webp' style='border-radius: 4px; height: 1.5em;' alt=''>. Drücke diesen Button, um dein LaTeX-Dokument zu compilieren (oder drücke <kbd>Strg</kbd><kbd>Alt</kbd><kbd>B</kbd>). Währenddessen siehst du unten in der Zeile den Fortschritt der Compilierung (<i class='bi bi-arrow-repeat bi-spin'></i> Build), und wenn die compilierung abgeschlossen ist, siehst du ein grünes Häkchen (<i class='bi bi-check-lg'></i>):

<img class='full' src='latex-build.webp' alt=''>

Wenn die compilierung erfolgreich war, solltest du eine
PDF-Datei namens `hello.pdf` im selben Verzeichnis sehen. Klicke anschließend auf den Button rechts oben: »View LaTeX PDF file«
<img src='view-latex-pdf-file.webp' style='border-radius: 4px; height: 1.5em;' alt=''> oder verwende den Shortcut <kbd>Strg</kbd><kbd>Alt</kbd><kbd>V</kbd>, um die PDF-Datei neben deinem LaTeX-Quelltext anzuzeigen:

<img class='full' src='side-by-side.webp' alt='LaTeX-Quelltext und die daraus erzeugte PDF-Vorschau stehen nebeneinander.'>

Das Dokument sieht noch nicht besonders spektakulär aus, deswegen schauen wir uns jetzt das nächste Dokument an: die Bewerbung.

## Bewerbung

Öffne als nächstes die Datei `Bewerbung.tex`. Diese Datei enthält eine Vorlage für eine Bewerbung, die du anpassen kannst. Wenn du die Datei compilierst und die PDF-Datei anschaust, sollte sie ungefähr so aussehen:

<img class='full' src='side-by-side-bewerbung.webp' alt='LaTeX-Quelltext und die daraus gesetzte einseitige Bewerbung stehen nebeneinander.'>

Schau dir den Inhalt der LaTeX-Datei an und versuche zu verstehen, wie die verschiedenen Teile funktionieren. Du kannst die Vorlage an deine eigenen Bedürfnisse anpassen, indem du die entsprechenden Informationen änderst.

Versuche als nächstes die Schriftart in der Bewerbung zu ändern, indem du in der Zeile `\setmainfont{XCharter}` die Schriftart `XCharter` durch eine andere Schriftart ersetzt. Welche Schriftarten du verwenden kannst, erfährst du im nächsten Abschnitt.

## Schriftarten

In LaTeX kannst du verschiedene Schriftarten verwenden, um dein Dokument ansprechender zu gestalten. Es gibt viele verschiedene Schriftarten, die du verwenden kannst, und jede hat ihren eigenen Stil und Charakter. In diesem Abschnitt findest du eine kleine Auswahl an Schriftarten, die besonders gut für LaTeX-Dokumente geeignet sind.

### Klassiker (typisches LaTeX-Aussehen)

Diese Schriftarten sind die klassischen LaTeX-Schriftarten, die oft in wissenschaftlichen Arbeiten verwendet werden. Sie sind gut lesbar und haben ein traditionelles Aussehen.

Achte auf den Bezeichner rechts oben im Bild - um z. B. `Latin Modern Roman` zu verwenden, musst du folgende Befehle in deinem LaTeX-Dokument verwenden:

```tex
\setmainfont{Latin Modern Roman}
```

<img class='full' src='fonts/latin-modern-roman.webp' alt='Schriftprobe in Latin Modern Roman mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/latin-modern-sans.webp' alt='Schriftprobe in Latin Modern Sans mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/cmu-typewriter-text.webp' alt='Schriftprobe in CMU Typewriter Text mit Überschrift, Fließtext und Beispielzeichen.'>

### Buch- und Textschriften (für längere Texte)

Diese Schriftarten sind gut geeignet für längere Texte, wie z. B. Bücher oder Berichte. Sie sind angenehm zu lesen und haben ein professionelles Aussehen.

<img class='full' src='fonts/nimbus-roman.webp' alt='Schriftprobe in Nimbus Roman mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/xcharter.webp' alt='Schriftprobe in XCharter mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/vollkorn.webp' alt='Schriftprobe in Vollkorn mit Überschrift, Fließtext und Beispielzeichen.'>

### Moderne serifenlose Schriften (Arbeitsblätter, Präsentationen)

Diese Schriftarten sind modern und serifenlos, was sie gut geeignet macht für Arbeitsblätter, Präsentationen oder andere Dokumente, die ein zeitgemäßes Aussehen erfordern.

<img class='full' src='fonts/inter.webp' alt='Schriftprobe in Inter mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/ibm-plex-sans.webp' alt='Schriftprobe in IBM Plex Sans mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/ubuntu.webp' alt='Schriftprobe in Ubuntu mit Überschrift, Fließtext und Beispielzeichen.'>

### Monospace-Schriften

Monospace-Schriften sind Schriftarten, bei denen jedes Zeichen die gleiche Breite hat. Sie werden oft für Code, Terminal-Ausgaben und Informatik-Dokumente verwendet.

<img class='full' src='fonts/jetbrains-mono.webp' alt='Schriftprobe in JetBrains Mono mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/anonymous-pro.webp' alt='Schriftprobe in Anonymous Pro mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/ibm-plex-mono.webp' alt='Schriftprobe in IBM Plex Mono mit Überschrift, Fließtext und Beispielzeichen.'>

### Kreative Schriftarten (für Überschriften, Plakate)

Diese Schriftarten sind kreativ und gut lesbar, was sie ideal macht für Überschriften, Plakate oder andere Dokumente, die Aufmerksamkeit erregen sollen.

<img class='full' src='fonts/atkinson-hyperlegible.webp' alt='Schriftprobe in Atkinson Hyperlegible mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/comic-neue.webp' alt='Schriftprobe in Comic Neue mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/montserrat.webp' alt='Schriftprobe in Montserrat mit Überschrift, Fließtext und Beispielzeichen.'>
<img class='full' src='fonts/comfortaa.webp' alt='Schriftprobe in Comfortaa mit Überschrift, Fließtext und Beispielzeichen.'>

### Schriftarten mit mathematischer Unterstützung

In LaTeX wird unterschieden zwischen Text- und Mathe-Schriftarten. Für mathematische Formeln werden spezielle Schriftarten benötigt, die mathematische Symbole und Zeichen unterstützen. Hier sind einige Schriftarten, die sowohl für den Text als auch für mathematische Formeln geeignet sind.

Achte auf die Bezeichner rechts oben im Bild - um z. B. `Latin Modern Roman` und `Latin Modern Math` zu verwenden, musst du folgende Befehle in deinem LaTeX-Dokument verwenden:

```tex
\setmainfont{Latin Modern Roman}
\setmathfont{Latin Modern Math}
```

<img class='full' src='fonts/latin-modern-math.webp' alt='Schriftprobe in Latin Modern Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/xcharter-math.webp' alt='Schriftprobe in XCharter Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/stix-two-math.webp' alt='Schriftprobe in STIX Two Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/tex-gyre-termes-math.webp' alt='Schriftprobe in TeX Gyre Termes Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/tex-gyre-pagella-math.webp' alt='Schriftprobe in TeX Gyre Pagella Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/tex-gyre-schola-math.webp' alt='Schriftprobe in TeX Gyre Schola Math mit Fließtext und mathematischen Formeln.'>
<img class='full' src='fonts/tex-gyre-bonum-math.webp' alt='Schriftprobe in TeX Gyre Bonum Math mit Fließtext und mathematischen Formeln.'>

<div class='hint'>
Du kannst auch eigene Schriftarten verwenden, wenn du die entsprechenden Schriftdateien (z. B. .ttf oder .otf) hast.
</div>

## Lebenslauf

In der Datei `CV.tex` findest du eine Vorlage für einen Lebenslauf, die du anpassen kannst. Wenn du die Datei compilierst und die PDF-Datei anschaust, sollte sie ungefähr so aussehen:

<img class='full' src='cv.webp' alt='Ein einseitiger Lebenslauf mit Profilbild, Kontaktdaten, Ausbildung, Erfahrung und Kenntnissen.'>

Wenn du eine Datei `photo.jpg` im selben Verzeichnis hast, wird sie automatisch in den Lebenslauf eingebunden.

## Schriftliche Ausarbeitung

Falls du eine schriftliche Ausarbeitung oder einen Bericht schreiben musst, kannst du die Vorlage in der Datei `Ausarbeitung.tex` verwenden. Diese Vorlage enthält bereits eine Struktur für eine wissenschaftliche Arbeit, einschließlich Titelblatt, Inhaltsverzeichnis, Abschnitte und Literaturverzeichnis. Wenn du die Datei compilierst und die PDF-Datei anschaust, sollte sie ungefähr so aussehen:

<img class='full' src='ausarbeitung.webp' alt='Eine schriftliche Ausarbeitung mit Titelblatt, Inhaltsverzeichnis und gegliederten Kapiteln.'>

Der Befehl `\setstretch{1.2}` sorgt dafür, dass der Zeilenabstand auf 1,2-fach eingestellt ist, was die Lesbarkeit verbessert. Du kannst diesen Wert anpassen, um den Zeilenabstand nach deinen Bedürfnissen zu ändern.

Zur Ausarbeitung gehört auch ein Literaturverzeichnis. In der Vorlage wird das Literaturverzeichnis mit dem Befehl `\printbibliography` erstellt, und die Literaturangaben werden in der Datei `litatur.bib` gespeichert, die im BibTeX-Format geschrieben ist:

_include_file(literatur.bib, bibtex)

In dieser Datei kannst du deine Literaturangaben hinzufügen, indem du die entsprechenden Einträge im BibTeX-Format erstellst. Anschließend kannst du die Literaturangaben in deinem LaTeX-Dokument mit dem Befehl `\cite{}` zitieren, wobei du den Schlüssel des Eintrags in der `literatur.bib`-Datei angibst:

- `\textcite{mueller2022}`: Zitiert die Quelle mit dem Schlüssel `mueller2022` und zeigt die Zitatangabe im Fließtext an, z. B. Müller (2022).
- `\parencite{schmidt2023}`: Zitiert die Quelle mit dem Schlüssel `schmidt2023` und zeigt die Zitatangabe in Klammern an, z. B. (Schmidt, 2023).

## Mathematische Formeln

Ein Bereicht, in dem LaTeX besonders glänzt, ist die Darstellung von mathematischen Formeln. LaTeX bietet eine Vielzahl von Möglichkeiten, um komplexe mathematische Ausdrücke klar und professionell darzustellen. In der Datei `Mathe.tex` findest du bereits einige Beispiele für mathematische Formeln, die du anpassen und erweitern kannst:

<img class='full' src='mathe.webp' alt='Eine PDF-Seite mit Beispielen für Brüche, Wurzeln, Summen, Integrale, Matrizen und Fallunterscheidungen.'>

## Ein ganzes Buch

Zum Abschluss dieses Tutorials findest du in der Datei `wpgtr.tex` eine LaTeX-Datei für ein ganzes Buch: »Why's (Poignant) Guide to Ruby« von [*\_why the lucky stiff*](https://tmewett.com/whytheluckystiff/). Diese Datei enthält die gesamte Struktur eines Buches, einschließlich Inhaltsverzeichnis, Kapiteln, Bildern und Formatierungen. Wenn du die Datei compilierst und die PDF-Datei anschaust, sollte sie ungefähr so aussehen:

<img class='full' src='wpgtr.webp' alt='Ein gesetztes Buch mit Inhaltsverzeichnis, Kapiteln, Fließtext und Illustrationen.'>