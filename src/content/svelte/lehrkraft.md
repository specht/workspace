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
<li>Ob die Stoppuhr richtig läuft und zurückspringt, zeigt das Ausprobieren; keine Aufgabe prüft das JavaScript, »Der Reset-Button« nur das HTML des Buttons.</li>
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
- »Die Anzeige steht« und »Der Reset-Button« lesen das HTML derselben Datei.
- »Die statische Webseite« liest `build/index.html`, also das, was `npm run build` wirklich erzeugt hat: Steht dort keine Anzeige, ist der Build älter als die letzte Änderung.
- »Zu kurz gedrückt« und »Warum :global?« fragen nach dem Ablaufdiagramm und dem CSS; »Ohne $state« ist eine Vorhersage, die die Lernenden danach ausprobieren.

#### Versionen

Die Seite folgt SvelteKit 3: Die Einstellungen, auch der Adapter, stehen in `vite.config.js`; eine `svelte.config.js` gibt es nicht mehr. Ältere Anleitungen im Netz nennen noch diese Datei.
