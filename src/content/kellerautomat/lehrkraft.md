Kellerautomaten zum Programmieren, in Ruby oder Python: Lernende prüfen Klammern erst mit einem Zähler, dann mit einem Stack, bauen einen Kellerautomaten als Tabelle (wie die endlichen Automaten in »Endliche Automaten«), erweitern ihn um eckige Klammern, schreiben einen für a<sup>n</sup>b<sup>n</sup> und überlegen mit dem Schubfachprinzip, warum kein endlicher Automat a<sup>n</sup>b<sup>n</sup> prüft.

<table class='table'>
<tr><th>Voraussetzungen</th><td>»Endliche Automaten«: Automaten als Tabelle, Start- und akzeptierende Zustände, die Grenze am Ende. Listen oder Arrays in Ruby oder Python; den Stack erklärt die Seite und das Handbuch.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Eine Aufgabe, die Lernende in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Zähler, die Vorhersage zu <code>([)]</code>, <code>balanced</code> mit einem Stack, der Kellerautomat für runde Klammern und die eckigen Klammern dazu.</li>
<li><b>2. Doppelstunde:</b> a<sup>n</sup>b<sup>n</sup> als Kellerautomat, das Schubfachprinzip und die Überlegung dahinter, welche Fragen ein endlicher Automat beantwortet, die Reflexion über Compiler.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/kellerautomat</code>: <code>count</code>, <code>brackets</code>, <code>pda</code> und <code>anbn</code>, mit <code>.rb</code> in Ruby und <code>.py</code> in Python. <code>count</code> und <code>pda</code> tippen die Lernenden von der Seite ab, <code>brackets</code> schreiben sie selbst, <code>anbn</code> ist eine Kopie von <code>pda</code> mit leerer Tabelle.</td></tr>
<tr><th>Prüfung</th><td>Drei Aufgaben laden die Datei der Lernenden in ihrem Workspace und rufen <code>balanced</code> oder <code>accepts</code> mit eigenen Wörtern auf, als Gast, der in <code>/workspace</code> nichts verändern kann. Die Rückmeldung nennt den ersten Aufruf, der nicht passt. Der Workspace muss dafür laufen.</td></tr>
</table>

#### Die Aufgaben

- »Reicht ein Zähler?« ist eine Vorhersage: Zwei Zähler halten <code>([)]</code> für richtig. Die Seite erklärt das erst nach der Antwort.
- »Klammern mit einem Stack«: Die Prüfung ruft `balanced` mit `([)]`, `)(`, `(]` und einem Ausdruck mit Buchstaben und Leerzeichen auf. Wer nur zählt, scheitert an `([)]`; wer am Ende nicht prüft, ob der Stack leer ist, an `((`.
- »Auch eckige Klammern« braucht fünf weitere Übergänge: `(` über `[`, `[` über `#`, `(` und `[`, und `]` über `[`. Wer `(` über `[` vergisst, scheitert an `[([])]`.
- »Erst a, dann gleich viele b«: Mit einem einzigen Zustand nimmt der Automat auch `abab` an; die Rückmeldung verweist dann auf einen zweiten Zustand. `ACCEPTING` muss beide Zustände enthalten, sonst fällt `ab` durch.
- »Vier Zustände, viele a« (4) führt zum Schubfachprinzip; die Überlegung für a<sup>n</sup>b<sup>n</sup> steht erst nach der Antwort auf der Seite. Das Pumping-Lemma wird nur genannt, nicht bewiesen.
- »Wo reicht ein endlicher Automat?«: Nur »gerade viele a« ist regulär; die Rückmeldungen zu den anderen nennen, was sich der Automat ohne Grenze merken müsste.
- »Wo steckt ein Stack?« ist eine Reflexion in ein paar Sätzen, ohne automatische Bewertung.

#### Was schiefgeht

- **Alles wird abgelehnt:** Der Push-String ist von unten nach oben geschrieben. `'#('` legt `(` auf `#`; wer `'(#'` schreibt, hat `#` oben und den Stack verdreht.
- **Das leere Wort wird abgelehnt:** `q0` fehlt in `ACCEPTING`.
- **`ValueError` (Python) oder `NoMethodError` (Ruby) in `accepts`:** In `TRANSITIONS` steht ein Ziel mit nur einem Wert statt eines Paars aus Zustand und dem, was auf den Stack kommt.

#### Begriffe

Die Seite benutzt deterministische Kellerautomaten mit einem Zeichen `#` am Boden und Annahme durch akzeptierenden Zustand und leeren Stack, ohne ε-Übergänge. Nichtdeterministische Kellerautomaten (für Palindrome) und kontextfreie Grammatiken werden nur erwähnt; die Grammatiken übt »Ein Rechner für Ausdrücke«.

#### Weiter

»Turingmaschinen« schließt an: a<sup>n</sup>b<sup>n</sup>c<sup>n</sup> prüft kein Kellerautomat, eine Turingmaschine schon.
