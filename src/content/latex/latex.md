<div class='meta'>
image: latex-logo.png
</div>

# Dokumente schreiben mit LaTeX

<p class='abstract'>
In diesem Tutorial lernst du LaTeX kennen – ein Werkzeug, mit dem man Texte nicht „zusammenschiebt“, sondern strukturiert beschreibt.
Anstatt Schriftgrößen, Abstände und Formatierungen per Hand einzustellen, sagst du LaTeX, was etwas ist (Überschrift, Absatz, Tabelle, Formel) – und LaTeX kümmert sich darum, wie es aussieht.
</p>

LaTeX wird häufig an Universitäten, in der Wissenschaft und bei technischen Berufen verwendet, zum Beispiel für Protokolle, Facharbeiten, Präsentationen oder Bewerbungen. Aber auch für die Schule ist es sehr praktisch: Texte sehen automatisch ordentlich aus, Formeln sind klar lesbar, und Änderungen wirken sich sofort auf das ganze Dokument aus.

In diesem Tutorial lernst du die Grundlagen von LaTeX anhand einer Bewerbung, eines Lebenslaufs und einer schriftlichen Ausarbeitung kennen.

_include_file(hello.tex, tex)

## Schriftarten

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
<img class='full' src='fonts/tex-gyre-termes-math.webp'><img class='full' src='fonts/tex-gyre-pagella-math.webp'>
<img class='full' src='fonts/tex-gyre-schola-math.webp'>
<img class='full' src='fonts/tex-gyre-bonum-math.webp'>

## Bewerbungsschreiben

## Lebenslauf

## Schriftliche Ausarbeitung
