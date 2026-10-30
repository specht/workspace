Lernende bauen mit Hugo eine kleine Website ohne fertiges Theme, ein Lexikon über die Planeten: Inhalte in Markdown mit Front Matter, ein eigenes Layout mit Navigation, eine Startseite, CSS, Schlagwörter und am Ende die fertigen HTML-Dateien in `public` zum Veröffentlichen. Die Aktivität schließt an »Statische Webseiten mit HTML & CSS« an und braucht sonst nur Markdown, das sie nebenbei zeigt.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das Problem vieler gleicher Seiten, <code>hugo new site</code>, <code>hugo.yaml</code>, die erste Seite, das Layout aus <code>baseof.html</code> und <code>page.html</code>, mehr Seiten und die Navigation.</li>
<li><b>2. Doppelstunde:</b> Startseite, CSS, Schlagwörter, <code>hugo</code> und <code>public</code>, Veröffentlichen, eigenes Thema.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles liegt in <code>/workspace/solar-system</code>. Die fertige Website steht in diesem Repository unter <code>src/content/hugo/stufen/solar-system/</code>, die Zwischenstände der Layouts als <code>baseof-1.html</code> und <code>page-1.html</code> daneben. Hugo ist im Workspace installiert.</td></tr>
<tr><th>Vorschau</th><td><code>hugo server</code> läuft auf Port 1313; die Adresse steht im Reiter »Ports«. Das automatische Neuladen von Hugo erreicht den Browser durch die Weiterleitung nicht; nach dem Speichern lädt man die Vorschau selbst neu. <code>baseURL: /</code> in <code>hugo.yaml</code> sorgt dafür, dass alle Links in der Vorschau und auf der Subdomain stimmen. Die Aufgabe »Die Seite ist da« fragt <code>localhost:1313/mars/</code> im Workspace der Lernenden selbst ab: Sie unterscheidet, ob gar kein Server läuft, ob Hugo die Seite nicht hat oder ob die Überschrift aus <code>page.html</code> fehlt.</td></tr>
</table>

#### Was schiefgeht

- Ein Doppelpunkt in einem Titel ohne Anführungszeichen (`title: Pluto: Planet`) lässt Hugo die ganze Website nicht mehr bauen; die Meldung nennt Datei und Zeile.
- Ein Tippfehler in `{{ … }}` (etwa `{{ .title }}` statt `{{ .Title }}`) bricht ebenfalls ab, mit Datei und Zeile im Terminal.
- Ohne `layouts/page.html` meldet Hugo `found no layout file`, und die Seite bleibt leer.
- Wer in `hugo.yaml` die `baseURL` von `https://example.org/` stehen lässt, bekommt in der Vorschau keine CSS-Datei.
- Neue Seiten erscheinen erst nach dem Speichern und Neuladen.

#### Die Aufgaben

»Die Einstellungen«, »Die Navigation« und »Schlagwörter« lesen `hugo.yaml`, `layouts/baseof.html` und `layouts/page.html`; »Die fertige Website« sieht nach, ob `public/index.html` mit den Karten der Startseite entstanden ist. »Ein vierter Planet« ist eine Vorhersage, »Ein Doppelpunkt zu viel« eine Falle aus YAML. Die Abgabe ist der Ordner `solar-system`, die Reflexion fragt, wann sich ein Generator lohnt.

#### Veröffentlichen

Die Seite verweist auf die Anleitung zum eigenen Webspace (`/reference/webspace`) und lädt `public/` mit `rsync` hoch. Minderjährige brauchen dafür die unterschriebene Einverständniserklärung.

#### Eigene Projekte

Nach der Seite haben die Lernenden ein eigenes Layout, Inhalte in Markdown und Schlagwörter. Die Ideen geben dem einen eigenen Inhalt; bewertet wird an der fertigen Website in `public` und daran, dass die Lernenden erklären, welche Datei welche Seite erzeugt.

- **Ein Lexikon zu einem eigenen Thema** (eine Doppelstunde bis ein Projekt): Vögel im Schulgarten, die Bauwerke einer Stadt, die Figuren eines Romans aus dem Deutschunterricht. Die Schlagwörter ordnen die Einträge auf eine zweite Art, etwa nach Lebensraum oder Epoche; was in das Front Matter gehört und was in den Text, entscheiden die Lernenden selbst.
- **Ein Projekttagebuch** (über ein anderes Projekt hinweg): ein Eintrag je Doppelstunde zu einem größeren Vorhaben, etwa der [Klötzchenwelt](/raylib), mit Datum, Bildschirmfoto und einem Satz, was schiefging. Hugo sortiert nach Datum; die Lehrkraft sieht den Weg und nicht nur das Ergebnis.
- **Eine Seite in zwei Sprachen** (ein Projekt, für Starke): Hugo kann mehrsprachige Websites; jeder Eintrag gibt es dann auf Deutsch und auf Englisch, mit einem Umschalter im Layout. Gut zusammen mit dem Englischunterricht.
