Lernende bauen ihre erste eigene Webseite mit HTML und CSS: eine Seite mit Überschrift, Liste, Link und einem eigenen Bild, dann Farben, Schriften und ein Layout mit Flexbox, das auf dem Handy untereinander rückt. Sie sehen jede Änderung sofort im Live Server und geben am Ende den Ordner ab.

Die Aufgaben lesen nur Dateien in `/workspace/website-test` und entscheiden selbst, ob etwas da ist: ob `index.html` sauber verschachtelt ist und eine Liste mit drei Punkten und einen Link hat, ob Bilder einen Alt-Text haben und unter ihrem Pfad wirklich existieren (mit Groß- und Kleinschreibung, wie der Live Server sie sieht), ob die Bilddatei zu ihrer Endung passt, ob `index.html` die `styles.css` wirklich einbindet und ob darin `main` mit Flexbox angeordnet ist und in einer Media Query untereinander rückt.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das Projekt mit Live Server anlegen, Inhalt mit HTML.</li>
<li><b>2. Doppelstunde:</b> Aussehen mit CSS, die Seite zur eigenen machen, veröffentlichen und abgeben; »Weiter ausprobieren« für Schnelle.</li>
</ul></td></tr>
<tr><th>Handbuch</th><td>Was HTML-Tags, CSS-Regeln, Flexbox und Media Queries bedeuten, steht im Handbuch (<a href='/reference/html'>HTML</a>, <a href='/reference/css'>CSS</a>, <a href='/reference/layout'>Layout</a>) und erscheint hier als Kasten. Dieselben Abschnitte nutzt <a href='/showerjs'>»Eine Präsentation in HTML erstellen«</a>; wer eine der beiden Aktivitäten schon gemacht hat, sieht sie in der anderen nur noch als eine Zeile. Die Beispiele im Handbuch zeigen sich hier als Webseite, dort als Folie.</td></tr>
<tr><th>Adresse</th><td>»Die Adresse deiner Seite« vergleicht mit der Live-Server-Adresse des eigenen Workspaces. Wer eine Adresse vom Nachbarn kopiert, liegt daneben.</td></tr>
<tr><th>Abgabe</th><td>Kopiert den ganzen Ordner <code>website-test</code> (höchstens 20 MB), wenn sich <code>index.html</code> lesen lässt. Die Abgabe ist eine Kopie; spätere Änderungen ändern sie nicht.</td></tr>
</table>

»Dein eigenes Bild« ist die Stelle, an der die meisten hängen: Bild nicht neben `index.html`, `.JPG` statt `.jpg`, oder ein `.png`, das eigentlich ein JPEG ist. Die Karte nennt jeweils, was sie gefunden hat. Die zweite häufige Stelle ist »Gestaltet mit CSS«: Die Regeln stehen in `styles.css`, aber der `<link>` im `<head>` fehlt oder nennt einen anderen Dateinamen; die Karte sagt das jetzt ausdrücklich.

#### Eigene Projekte

Die Seite macht aus der Vorlage eine eigene Seite. Die Ideen hier geben ihr einen Zweck außerhalb des Unterrichts; bewertet wird an der Seite selbst, auf dem Rechner und auf dem Handy, und am Erklären einer CSS-Regel, die die Lernenden selbst geschrieben haben.

- **Eine Seite für jemand anderen** (ein Projekt): für einen Verein, eine AG, eine Klassenfahrt. Am Anfang steht ein kurzes Gespräch mit der Person, die die Seite haben will, und eine Liste, was hineinsoll; am Ende sagt diese Person, ob sie bekommen hat, was sie wollte. Fotos und Namen anderer kommen nur mit deren Erlaubnis auf eine Seite, die veröffentlicht wird.
- **Von der Skizze zur Seite** (eine Doppelstunde): Jemand zeichnet ein Layout auf Papier, jemand anderes baut es mit [»Layout mit Flexbox«](#layout-mit-flexbox) nach, und beide vergleichen, auch auf einem schmalen Bildschirm wie in [»Auf kleinen Bildschirmen«](#auf-kleinen-bildschirmen).
- **Eine Seite für alle** (eine Doppelstunde): die eigene Seite auf Barrierefreiheit durchgehen, mit den Werkzeugen des Browsers: Haben alle Bilder einen Alternativtext, reicht der Kontrast, lässt sich alles mit der Tastatur erreichen? Abgegeben wird die Liste der Funde und die verbesserte Seite.
