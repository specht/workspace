Die Lernenden schreiben zuerst gemeinsam die kleine Geschichte »Nach Schulschluss« (fünf Seiten, ein Schlüssel, ein Ende) und dann eine eigene. Für den ersten Teil reicht eine Doppelstunde; die eigene Geschichte braucht eine bis zwei weitere.

Die Aufgaben lesen, was BIF selbst über die Geschichte herausgefunden hat: Die Erweiterung »BIF Authoring Tools« im Workspace analysiert sie bei jedem Speichern (das ist auch der Graph und die Liste unter »Problems«). Der Workspace führt dabei nichts aus. Er zählt Seiten, Entscheidungen, lokale Entscheidungen, Bedingungen, Skripte und Enden, prüft, ob jede Seite erreichbar ist und BIF keine Fehler meldet, und bei der eigenen Geschichte, ob `config.js` einen eigenen Ordner wählt.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Loslegen und Seiten mit Entscheidungen.</li>
<li><b>2. Doppelstunde:</b> Variablen und Bedingungen, Fehler finden.</li>
<li><b>3. Doppelstunde:</b> die eigene Geschichte und die Abgabe.</li>
</ul></td></tr>
<tr><th>Workspace</th><td>Die Prüfungen brauchen eine aktuelle Analyse. Wer Seiten ändert, während der Workspace nicht läuft, bekommt »Öffne deinen Workspace und speichere eine Seite«. Auf der Klassenseite lassen sich die Geschichten-Aufgaben für alle auf einmal prüfen.</td></tr>
<tr><th>Handbuch</th><td>Die Schreibweisen stehen zum Nachschlagen unter <a href='/reference/bif'>Interaktive Geschichten mit BIF</a>, mit einer Tabelle, was die englischen Meldungen unter »Problems« bedeuten. Für die eigene Geschichte ist das der Spickzettel.</td></tr>
<tr><th>Abgabe</th><td>Kopiert den Ordner <code>bif</code> ohne <code>node_modules</code>, <code>.git</code> und die Tests von BIF (wenige MB). Auf der Klassenseite öffnet ein Klick die Abgabe: »Starten« spielt die Geschichte im Browser, mit Graph, so wie die Person sie abgegeben hat; »Quelltext« zeigt jede Seite.</td></tr>
</table>

Wo es meistens hakt:

- **Einrückung.** Was nach einer lokalen Entscheidung erscheint, muss mit vier Leerzeichen eingerückt sein. Ohne Einrückung meldet BIF »A local choice has no result«, und der Text steht für alle sichtbar auf der Seite.
- **`(2.md)` statt `(2)`.** Mit `.md` ist es ein gewöhnlicher Link, keine Entscheidung; die Seite 2 gilt dann als unerreichbar.
- **`(.)` und `(2)`.** Der Punkt bleibt auf der Seite, eine Zahl wechselt sie. Die Vermutung »Was bedeutet der Punkt?« fragt das vor der Erklärung ab; auf der Klassenseite sieht man, wie viele richtig vermutet haben.
- **Anfangswerte.** `has_key = false;` gehört nach `1.md`. Steht es auf einer Seite, die man mehrmals besucht, verliert man den Schlüssel bei jedem Besuch wieder. Die Frage »Warum in 1.md?« greift das auf.
- **`config.js`.** Der Ordnername muss genau so dastehen wie im Explorer, in Anführungszeichen. Sonst meldet BIF »Configured story directory does not exist«, und die Vorschau bleibt leer.
- **Unerreichbare Seiten.** Neue Seiten erscheinen im Graphen zuerst ohne Pfeil. »Deine Geschichte« verlangt, dass jede Seite erreichbar ist, und nennt die Datei, zu der nichts führt.

Für die eigene Geschichte verlangen die Aufgaben mindestens fünf Seiten, vier Entscheidungen zwischen ihnen und ein Ende, ohne Fehler; »Eine Entscheidung mit Folgen« zusätzlich eine Variable und eine Bedingung. Ob die Geschichte gut ist, entscheidet keine Aufgabe; dafür sind die Fragen unter »Eine Idee entwickeln« gedacht, und das Durchspielen der Abgaben.
