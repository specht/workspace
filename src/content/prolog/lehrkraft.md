Ein Projekt zur logischen Programmierung: Lernende schreiben in SWI-Prolog Fakten über die Musikerfamilie Bach und Regeln für Vater, Mutter, Großeltern, Geschwister und Vorfahren, stellen Anfragen und sehen in einem Schritt-für-Schritt-Bild, wie Prolog mit Unifikation und Backtracking sucht. Dann Listen als Kopf und Rest, mit einem <code>joined</code>, das auch rückwärts rechnet, und zum Schluss die zwölf Bezirke Berlins in vier Farben, bei denen die Reihenfolge der Bedingungen über die Laufzeit entscheidet. Extra: ein Sudoku mit <code>library(clpfd)</code>.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Rekursion (<a href='/kata-recursion'>Katas: Rekursion</a>). Die Sprache selbst ist für alle neu; wer <a href='/grammatiken'>Grammatiken</a> kennt, findet am Ende einen Ausblick auf Grammatiken in Prolog.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> SWI-Prolog starten, Fakten und Anfragen, die fehlenden Kinder eintragen, Regeln für Vater, Mutter und Großeltern, die Vorhersage zu den Geschwistern und die Regel dazu.</li>
<li><b>2. Doppelstunde:</b> wie Prolog sucht (das Schritt-für-Schritt-Bild), die Vorhersage zur linken Rekursion, Vorfahren, dann Listen: <code>is_in</code>, <code>size</code>, <code>joined</code> und die Vorhersage, was <code>joined</code> rückwärts liefert.</li>
<li><b>3. Doppelstunde:</b> Berlin in vier Farben, mit dem Versuch, erst zu färben und dann zu prüfen. Extra: das Sudoku.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/prolog</code>: <code>familie.pl</code>, <code>listen.pl</code>, <code>bezirke.pl</code> und <code>sudoku.pl</code>, die die Lernenden herunterladen.</td></tr>
<tr><th>Prüfung</th><td>Acht Aufgaben laden die Datei der Lernenden mit SWI-Prolog in ihrem Workspace und stellen Anfragen, jede mit einer Zeitgrenze; verglichen wird die Menge der Antworten, bei den Listen auch ihre Reihenfolge. Eine Regel, die sich endlos selbst aufruft, bekommt eine eigene Rückmeldung. Bei den Bezirken prüft eine Anfrage die erste Färbung und eine zählt alle 3408; eine Regel, die erst alle zwölf Bezirke färbt und dann prüft, ist richtig und besteht, braucht aber etwa sechzigmal so lange. Dazu vier Vorhersagen.</td></tr>
<tr><th>Im Workspace</th><td>SWI-Prolog (<code>swipl</code>) ist im Workspace-Image enthalten.</td></tr>
</table>

#### Was schiefgeht

- **Der Punkt fehlt:** Jeder Fakt, jede Regel und jede Anfrage endet mit einem Punkt. Ohne ihn wartet <code>swipl</code> auf mehr; die Prüfung meldet die Zeile, die Prolog nicht lesen kann.
- **Groß und klein:** <code>Johann_sebastian</code> ist eine Variable, kein Name. Eine Anfrage mit großem Anfangsbuchstaben gibt dann jeden Elternteil als Antwort.
- **Geändert, aber nicht neu geladen:** Wer <code>familie.pl</code> im Editor ändert, muss in Prolog <code>make.</code> eingeben; die Prüfung lädt die Datei immer frisch.
- **Jemand ist sein eigenes Geschwister:** Ohne <code>X \= Y</code>; die Vorhersage bereitet das vor.
- **Linke Rekursion:** <code>ancestor(A, D) :- ancestor(A, P), parent(P, D).</code> als erste Klausel hört nicht auf. Logisch ist sie richtig; der Anlass zu besprechen, dass Prolog in fester Reihenfolge sucht.
- **<code>=</code> statt <code>is</code>:** <code>N = M + 1</code> rechnet nicht, sondern ergibt den Term <code>2+1</code>.
- **Ein Nachbarpaar fehlt:** Die Zählung ergibt dann mehr als 3408 Färbungen.

#### Für die Besprechung

- Was Prolog unter »false« versteht: nicht »falsch«, sondern »nicht beweisbar aus dem, was in der Datei steht« (die Annahme einer geschlossenen Welt).
- Warum <code>joined</code> rückwärts rechnet, eine Funktion in Ruby oder Python aber nicht: eine Beziehung zwischen drei Listen statt einer Rechnung von zwei Listen zu einer.
- Wo logische Programmierung heute steckt: in Typprüfern, in Datenbankabfragen (Datalog), in Planungs- und Konfigurationssystemen, und in Bedingungslösern wie beim Sudoku.
