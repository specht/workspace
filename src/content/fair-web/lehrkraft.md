Lernende finden auf einer ausgedachten Website der Hacker-AG Barrieren und Tricks und bauen sie aus: erst für alle benutzbar, dann fair, dann im dritten Teil Phishing-Mails erkennen und einen eigenen Link-Prüfer schreiben. Am Ende steht eine Seite, die jede Prüfung besteht, und eine kurze Erklärung zur Barrierefreiheit. Lernende sehen die Notiz nicht.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/static-website'>Statische Webseiten</a> (HTML, CSS, »Go Live«); für die Funktionen erste Schritte in Ruby oder Python (Strings, <code>if</code>, Listen). Der Teil Phishing kommt ohne die Seite der AG aus.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Eine Seite für alle: die Seite holen, mit Tastatur und Zoom benutzen, Bilder und Sprache, Formulare, Knöpfe und Links.</li>
<li><b>2. Doppelstunde:</b> Farbe und Kontrast mit den beiden Funktionen <code>luminance</code> und <code>contrast_ratio</code>, Struktur, Fokus und Bewegung, Prüfen mit Lighthouse und die Erklärung zur Barrierefreiheit.</li>
<li><b>3. Doppelstunde:</b> Fair statt trickreich: Banner, Häkchen, Countdown, je mit einer Vorhersage und einer Prüfung des HTML.</li>
<li><b>4. Doppelstunde:</b> Phishing: Hinweise im Postfach finden, <code>host</code>, <code>real_domain</code> und <code>looks_like</code>, was nach einem Klick zu tun ist.</li>
<li><b>Für Schnelle:</b> die eigene Seite aus »Statische Webseiten« genauso prüfen und reparieren.</li>
</ul></td></tr>
<tr><th>Zwei Ebenen</th><td>Jeder Teil fängt mit dem Erkennen an: Demos, Vorhersagen und Auswahlaufgaben ohne Code, auch für jüngere Klassen (ab etwa Klasse 5) oder eine Vertretungsstunde. Das Bauen danach (HTML reparieren, die Funktionen) ist Informatik. Wer nur das Erkennen will, lässt die Aufgaben mit Prüfung des HTML und die Funktionen weg; der Fortschritt zählt dann nicht bis zum Ende.</td></tr>
<tr><th>Dateien</th><td><code>fair-web/index.html</code>, <code>style.css</code>, <code>script.js</code> und zwei Bilder in <code>fair-web/bilder</code>, mit <code>wget</code> geholt. Die Funktionen stehen in den Karten, nicht im Workspace.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>HTML: <code>lang</code>, Alt-Texte ohne Dateinamen, ein <code>label</code> für jedes Feld, kein <code>div</code> mit <code>onclick</code>, Überschriften ohne Sprung, <code>nav</code> und <code>main</code>, zwei Knöpfe im Banner und kein Link, kein gesetztes Häkchen, kein Countdown.</li>
<li>CSS: jede Schrift mindestens 4,5 : 1 auf ihrem Hintergrund, eine Regel für <code>:focus-visible</code>, eine für <code>prefers-reduced-motion</code>.</li>
<li>Die Funktionen in Ruby oder Python mit festen Fällen, darunter der Port, das <code>@</code> und die Domain hinter der Tarnung.</li>
</ul></td></tr>
</table>

#### Was schiefgeht

- **Der Knopf ist ein `<button>`, aber nichts passiert:** `onclick="send()"` steht nicht mehr da. `script.js` muss am Formular auf `submit` hören (der Codeblock unter »Knöpfe und Links«).
- **Die Überschriften sind nach dem Umbau klein:** `h4 {` in `style.css` gilt nicht mehr; es wird `h2 {`.
- **Die Konsole meldet jede Sekunde einen Fehler:** Die Zeile mit dem Countdown ist weg, das `setInterval` in `script.js` noch nicht.
- **Kontrast reicht nicht, obwohl die Schrift dunkel ist:** Die Meldung nennt die Regel. Oft ist es weiße Schrift auf einem hellen Knopf; dann wird der Hintergrund dunkler.
- **`real_domain` liefert die Schule:** die ersten beiden Teile statt der letzten. Genau darauf zählt der Phishing-Link; die Rückmeldung sagt es.

#### Die Aufgaben

- Vorhersagen vor dem Ausprobieren: Tab bis zum Knopf, das Grau `#777777`, die Klicks im Banner, das doppelt verneinte Häkchen, der Countdown nach dem Neuladen, wem ein Link gehört, was ein Passwort wert ist.
- Die Erklärung zur Barrierefreiheit ist eine Reflexion: Gute Antworten nennen, was geändert ist, was offen ist und was kein Werkzeug prüfen kann (ob ein Alt-Text passt).
- Die Mails im Postfach sind ausgedacht; alle Adressen enden auf `.example`, eine Endung, die für Beispiele reserviert ist. Echte Dienste und Personen kommen nicht vor.

#### Eigene Projekte

- **Die eigene Seite für alle** (eine Doppelstunde): die Seite aus »Statische Webseiten« mit Tastatur, Zoom und Lighthouse prüfen, reparieren und eine Erklärung zur Barrierefreiheit schreiben. Bewertet: Was die Prüfungen hier verlangen, und wie ehrlich die Erklärung ist.
- **Ein Link-Prüfer als Programm** (eine Doppelstunde): die drei Funktionen zu einem Programm verbinden, das eine Liste bekannter Domains kennt und zu jedem Link sagt, wem er gehört und ob er einer bekannten zum Verwechseln ähnlich sieht. Bewertet: eigene Testfälle, auch solche, die der Prüfer nicht erkennt.
- **Ein Plakat für die Schule** (eine Stunde, auch für jüngere Klassen): die Hinweise auf Phishing in eigenen Worten, mit einem selbst ausgedachten Beispiel. Bewertet: ob jemand, der die Seite nicht kennt, danach eine Phishing-Mail erkennt.
