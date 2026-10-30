Lernende bauen ihr erstes Projekt mit einem Frontend-Framework: Aus HTML, CSS und etwas JavaScript in einer Svelte-Komponente entsteht eine Stoppuhr für Zauberwürfel, die als Zustandsautomat modelliert ist. Am Ende steht eine statische Webseite, die ohne Svelte auf jedem Webserver läuft.

<table class='table'>
<tr><th>Voraussetzungen</th><td>HTML und CSS wie in <a href='/static-website'>Statische Webseiten mit HTML & CSS</a>. JavaScript wird unterwegs erklärt: Variablen, Funktionen, <code>if</code>, Event-Listener.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Projekt anlegen, Entwicklungsserver, Gerüst und Zustand.</li>
<li><b>2. Doppelstunde:</b> Tasten, Zeitmessung, die fertige Stoppuhr.</li>
<li><b>3. Doppelstunde:</b> Schriften, Reset-Button, Icons, Build und Abgabe; wer schneller ist, gestaltet weiter oder veröffentlicht auf einer eigenen Subdomain.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>Nach <code>handleKeyDown</code> als Beispiel schreiben die Lernenden <code>handleKeyUp</code> selbst, aus dem Ablaufdiagramm.</li>
<li>Für den Reset-Button schreiben sie <code>resetTimer</code>: <code>state</code>, <code>t0</code> und <code>timerString</code> zurück auf ihre Anfangswerte, die Klasse <code>ready</code> weg.</li>
<li>Davor stehen Tipps, eine Stufe nach der anderen mit einer Minute Pause und ohne Uhr; der Code der Seite (bei <code>handleKeyUp</code> mit der ganzen Datei) ist die letzte Stufe.</li>
<li>Unter jedem Kasten liest eine Aufgabe den <code>&lt;script&gt;</code>-Teil von <code>+page.svelte</code> als Text, ohne ihn auszuführen. »Dein handleKeyUp« sucht <code>clearTimeout(timeoutId)</code>, den Fall für Zustand 2 (<code>state === 2</code> oder <code>case 2:</code>), <code>state = 3</code> und <code>t0 = Date.now()</code>; »Dein resetTimer« die Funktion, die Bedingung für Zustand 3 oder 4, <code>t0 = 0</code>, den Anfangstext von <code>timerString</code> und <code>classList.remove("ready")</code>. Nicht prüfen lassen sich so <code>state = 0</code> und <code>requestAnimationFrame(updateTimer)</code>, weil beide Zeilen schon an anderer Stelle der Datei stehen; ob die Stoppuhr richtig läuft und zurückspringt, zeigt das Ausprobieren. Wer eine Aufgabe löst, kann die Lösung des Kastens sofort öffnen.</li>
</ul></td></tr>
<tr><th>Abgabe</th><td>»Dein Timer« kopiert den Ordner <code>rubik-timer/build</code> (höchstens 5 MB). In der Klassenübersicht startet ein Klick die gebaute Seite im Browser, ohne dass der Workspace der Person laufen muss; nach einem Klick in die Seite reagiert sie auf die Leertaste wie das Original.</td></tr>
</table>

#### Was schiefgeht

- Zeigt die Vorschau »Blocked request. This host is not allowed«, fehlt in `vite.config.js` `allowedHosts: true`; meldet sie, dass keine Verbindung zustande kommt, fehlt `host: '127.0.0.1'` (ohne lauscht Vite nur unter IPv6, die Weiterleitung des Workspaces fragt über IPv4).
- In beiden Fällen danach den Server neu starten.
- Die meisten anderen Fragen sind Klammern und Kommas in `vite.config.js` oder im `<script>`-Teil; die Fehlermeldung im Terminal und in der Vorschau nennt Datei und Zeile.
- Nach `npm install @iconify/svelte` vergessen viele, den Server wieder zu starten: Dann ist die Vorschau nicht erreichbar.

#### Die Aufgaben

- »Die Überschrift in der Mitte« liest nur den `<style>`-Teil von `src/routes/+page.svelte` und sucht die Flexbox-Angaben für `.main`.
- »Dein handleKeyUp« und »Dein resetTimer« lesen den `<script>`-Teil derselben Datei als Text (oben, »Jetzt bist du dran«).
- »Die Anzeige steht« und »Der Reset-Button« lesen das HTML derselben Datei.
- »Die statische Webseite« liest `build/index.html`, also das, was `npm run build` wirklich erzeugt hat: Steht dort keine Anzeige, ist der Build älter als die letzte Änderung.
- »Zu kurz gedrückt« und »Warum :global?« fragen nach dem Ablaufdiagramm und dem CSS; »Ohne $state« ist eine Vorhersage, die die Lernenden danach ausprobieren.

#### Versionen

Die Seite folgt SvelteKit 3: Die Einstellungen, auch der Adapter, stehen in `vite.config.js`; eine `svelte.config.js` gibt es nicht mehr. Ältere Anleitungen im Netz nennen noch diese Datei.

#### Eigene Projekte

Die Stoppuhr ist ein Zustandsautomat mit einer Oberfläche. Die Ideen nehmen beides wieder auf; jede beginnt mit dem Diagramm der Zustände wie in [»Modellierung«](#modellierung), und bewertet wird an Diagramm und Vorführung.

- **Ein eigener Automat mit Oberfläche** (eine Doppelstunde bis ein Projekt): eine Uhr für Referate, die nach der Zeit rot wird, ein Pomodoro-Timer mit Arbeit und Pause, ein Quiz mit Frage, Antwort und Auswertung. Erst die Zustände und Übergänge, dann die Komponente; wer etwas vergessen hat, findet es beim Vorführen, wenn jemand die Knöpfe in einer anderen Reihenfolge drückt.
- **Bestzeiten** (eine Doppelstunde): Die Stoppuhr merkt sich ihre Zeiten im Browser (`localStorage`), zeigt die beste und den Durchschnitt der letzten fünf, wie bei Wettkämpfen mit dem Zauberwürfel üblich, ohne die beste und die schlechteste. Neu ist Zustand, der das Neuladen der Seite übersteht.
- **In Komponenten zerlegen** (eine Doppelstunde): Anzeige, Knöpfe und Liste der Zeiten werden eigene Komponenten, die Werte aneinander weitergeben. Zu erklären ist, welche Komponente welchen Zustand besitzt und warum nicht jede alles weiß.
