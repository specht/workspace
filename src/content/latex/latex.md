<div class='meta'>
image: latex-logo.png
</div>

# Dokumente schreiben mit LaTeX

<p class='abstract'>
In diesem Tutorial lernst du LaTeX kennen – ein Werkzeug, mit dem man Texte nicht „zusammenschiebt“, sondern strukturiert beschreibt.
Anstatt Schriftgrößen, Abstände und Formatierungen per Hand einzustellen, sagst du LaTeX, was etwas ist (Überschrift, Absatz, Tabelle, Formel) – und LaTeX kümmert sich darum, wie es aussieht.
</p>

LaTeX wird häufig an Universitäten, in der Wissenschaft und bei technischen Berufen verwendet, zum Beispiel für Protokolle, Facharbeiten, Präsentationen oder Bewerbungen. Aber auch für die Schule ist es sehr praktisch: Texte sehen automatisch ordentlich aus, Formeln sind klar lesbar, und Änderungen wirken sich sofort auf das ganze Dokument aus.

In diesem Tutorial lernst du die Grundlagen von LaTeX anhand einer **Bewerbung**, eines **Lebenslaufs** und einer **schriftlichen Ausarbeitung** kennen.

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<img class='full' src='fresh-start.webp'>

## Repository klonen

Für diese Anleitung brauchst du ein Repository, das du klonen kannst, indem du auf den blauen Button »Clone Repository« klickst. Gib die folgende URL ein und bestätige mit <span class='key'>Enter</span>:

```bash
https://github.com/specht/latex-tutorial.git
```

<img class='full' src='git-clone.webp'>

Als nächstes musst du angeben, in welches Verzeichnis du das Repository klonen möchtest. Bestätige den Standardpfad `/workspace/` mit <span class='key'>Enter</span>.

<img class='full' src='confirm-clone-path.webp'>

Beantworte die Frage »Would you like to open the cloned repository?« mit »Open«.

<img class='full' src='open-yes-no.webp'>

## LaTeX Workshop installieren

Um uns die Arbeit mit LaTeX zu erleichtern, verwenden wir die Erweiterung »LaTeX Workshop«. Diese Erweiterung bietet viele nützliche Funktionen, wie z. B. das automatische Kompilieren von LaTeX-Dokumenten, Syntaxhervorhebung und Vorschauen. Klicke dazu auf das Extensions-Symbol <img src='../basics/extensions.webp' style='border-radius: 4px; height: 1.5em;'> in der Seitenleiste und suche nach »LaTeX Workshop«. Klicke auf »Install«, um die Erweiterung zu installieren.

<img class='full' src='latex-workshop-ext.webp'>

Wechsle nun wieder zurück zum Explorer, indem du auf das Explorer-Symbol <img src='../basics/explorer.webp' style='border-radius: 4px; height: 1.5em;'> in der Seitenleiste klickst. Öffne als erstes die Datei `hello.tex`. Diese Datei enthält ein einfaches LaTeX-Dokument, das du als Ausgangspunkt verwenden kannst. Schau dir den Inhalt der Datei an und versuche zu verstehen, was die verschiedenen Teile bedeuten:

_include_file(hello.tex, tex)

Rechts oben findest du einen kleinen Button: »Build LaTeX project« 
<img src='build-latex-project.webp' style='border-radius: 4px; height: 1.5em;'>. Drücke diesen Button, um dein LaTeX-Dokument zu kompilieren (oder drücke <span class='key'>Strg</span><span class='key'>Alt</span><span class='key'>B</span>). Währenddessen siehst du unten in der Zeile den Fortschritt der Kompilierung (<i class='bi bi-arrow-repeat bi-spin'></i> Build), und wenn die Kompilierung abgeschlossen ist, siehst du ein grünes Häkchen (<i class='bi bi-check-lg'></i>):

<img class='full' src='latex-build.webp'>

Wenn die Kompilierung erfolgreich war, solltest du eine
PDF-Datei namens `hello.pdf` im selben Verzeichnis sehen. Klicke anschließend auf den Button rechts oben: »View LaTeX PDF file«
<img src='view-latex-pdf-file.webp' style='border-radius: 4px; height: 1.5em;'> oder verwende den Shortcut <span class='key'>Strg</span><span class='key'>Alt</span><span class='key'>V</span>, um die PDF-Datei neben deinem LaTeX-Quelltext anzuzeigen:

<img class='full' src='side-by-side.webp'>

Das Dokument sieht noch nicht besonders spektakulär aus, deswegen schauen wir uns jetzt das nächste Dokument an: die Bewerbung.

## Bewerbung

Öffne als nächstes die Datei `Bewerbung.tex`. Diese Datei enthält eine Vorlage für eine Bewerbung, die du anpassen kannst. Schau dir den Inhalt der Datei an und versuche zu verstehen, wie die verschiedenen Teile funktionieren. Du kannst die Vorlage an deine eigenen Bedürfnisse anpassen, indem du die entsprechenden Informationen änderst.

Versuche als nächstes die Schriftart in der Bewerbung zu ändern, indem du in der Zeile `\setmainfont{XCharter}` die Schriftart `XCharter` durch eine andere Schriftart ersetzt.

## Schriftarten

In LaTeX kannst du verschiedene Schriftarten verwenden, um dein Dokument ansprechender zu gestalten. Es gibt viele verschiedene Schriftarten, die du verwenden kannst, und jede hat ihren eigenen Stil und Charakter. In diesem Abschnitt findest du eine kleine Auswahl an Schriftarten, die besonders gut für LaTeX-Dokumente geeignet sind.

### Klassiker (typisches LaTeX-Aussehen)

Diese Schriftarten sind die klassischen LaTeX-Schriftarten, die oft in wissenschaftlichen Arbeiten verwendet werden. Sie sind gut lesbar und haben ein traditionelles Aussehen.

Achte auf den Bezeichner rechts oben im Bild - um z. B. `Latin Modern Roman` zu verwenden, musst du folgende Befehle in deinem LaTeX-Dokument verwenden:

```tex
\setmainfont{Latin Modern Roman}
```

<img class='full' src='fonts/latin-modern-roman.webp'>
<img class='full' src='fonts/latin-modern-sans.webp'>
<img class='full' src='fonts/cmu-typewriter-text.webp'>

### Buch- und Textschriften (für längere Texte)

Diese Schriftarten sind gut geeignet für längere Texte, wie z. B. Bücher oder Berichte. Sie sind angenehm zu lesen und haben ein professionelles Aussehen.

<img class='full' src='fonts/nimbus-roman.webp'>
<img class='full' src='fonts/xcharter.webp'>
<img class='full' src='fonts/vollkorn.webp'>

### Moderne serifenlose Schriften (Arbeitsblätter, Präsentationen)

Diese Schriftarten sind modern und serifenlos, was sie gut geeignet macht für Arbeitsblätter, Präsentationen oder andere Dokumente, die ein zeitgemäßes Aussehen erfordern.

<img class='full' src='fonts/inter.webp'>
<img class='full' src='fonts/ibm-plex-sans.webp'>
<img class='full' src='fonts/ubuntu.webp'>

### Monospace-Schriften

Monospace-Schriften sind Schriftarten, bei denen jedes Zeichen die gleiche Breite hat. Sie werden oft für Code, Terminal-Ausgaben und Informatik-Dokumente verwendet.

<img class='full' src='fonts/jetbrains-mono.webp'>
<img class='full' src='fonts/anonymous-pro.webp'>
<img class='full' src='fonts/ibm-plex-mono.webp'>

### Kreative Schriftarten (für Überschriften, Plakate)

Diese Schriftarten sind kreativ und gut lesbar, was sie ideal macht für Überschriften, Plakate oder andere Dokumente, die Aufmerksamkeit erregen sollen.

<img class='full' src='fonts/atkinson-hyperlegible.webp'>
<img class='full' src='fonts/comic-neue.webp'>
<img class='full' src='fonts/montserrat.webp'>
<img class='full' src='fonts/comfortaa.webp'>

### Schriftarten mit mathematischer Unterstützung

In LaTeX wird unterschieden zwischen Text- und Mathe-Schriftarten. Für mathematische Formeln werden spezielle Schriftarten benötigt, die mathematische Symbole und Zeichen unterstützen. Hier sind einige Schriftarten, die sowohl für den Text als auch für mathematische Formeln geeignet sind.

Achte auf die Bezeichner rechts oben im Bild - um z. B. `Latin Modern Roman` und `Latin Modern Math` zu verwenden, musst du folgende Befehle in deinem LaTeX-Dokument verwenden:

```tex
\setmainfont{Latin Modern Roman}
\setmathfont{Latin Modern Math}
```

<img class='full' src='fonts/latin-modern-math.webp'>
<img class='full' src='fonts/xcharter-math.webp'>
<img class='full' src='fonts/stix-two-math.webp'>
<img class='full' src='fonts/tex-gyre-termes-math.webp'>
<img class='full' src='fonts/tex-gyre-pagella-math.webp'>
<img class='full' src='fonts/tex-gyre-schola-math.webp'>
<img class='full' src='fonts/tex-gyre-bonum-math.webp'>

<div class='hint'>
Du kannst auch eigene Schriftarten verwenden, wenn du die entsprechenden Schriftdateien (z. B. .ttf oder .otf) hast.
</div>

## Lebenslauf

## Schriftliche Ausarbeitung
