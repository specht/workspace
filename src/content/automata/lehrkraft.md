Endliche Automaten zum Programmieren, in Ruby oder Python: Lernende bauen einen DEA als Tabelle (in Ruby ein Hash mit Arrays als Schlüsseln, in Python ein `dict` mit Tupeln), einen Akzeptor für »durch 3 teilbar«, ein Zahlenschloss, einen NEA und die Potenzmengenkonstruktion. Automatron (`/automatron`) dient zum Vergleichen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Grundlagen in Ruby oder Python: Methoden oder Funktionen, Schleifen, Listen; Hash oder <code>dict</code> und Mengen erklärt die Seite, soweit sie gebraucht werden.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Drehkreuz, Teilbarkeit, Grenzen und das Zahlenschloss bis zum Vergleich mit Automatron.</li>
<li><b>2. Doppelstunde:</b> NEA, Potenzmengenkonstruktion, Generalschlüssel und die Explosion, dann Zusammenfassung und Reflexion.</li>
<li><b>Für Schnelle:</b> »Teilbar durch 5« mit Schleifen, die Beispiele in Automatron.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/automata</code>: <code>turnstile</code>, <code>divisible</code>, <code>divisible5</code>, <code>lock</code>, <code>lock_nfa</code>, <code>convert</code> und <code>last</code>, mit <code>.rb</code> in Ruby und <code>.py</code> in Python. Die Lernenden tippen sie von der Seite ab; <code>lock_nfa</code> und <code>convert</code> haben einen Kopierknopf.</td></tr>
<tr><th>Prüfung</th><td>Vier Aufgaben laden die Datei der Lernenden in ihrem Workspace und rufen Funktionen daraus auf, als Gast, der in <code>/workspace</code> nichts verändern kann, höchstens zehn Sekunden. Die Rückmeldung nennt den ersten Aufruf, der nicht passt, und was er ergibt, darunter alle Aufrufe mit Haken oder Kreuz. Der Workspace muss dafür laufen.</td></tr>
</table>

#### Die Aufgaben

- Die Zustände heißen überall q<sub>0</sub>, q<sub>1</sub> und so weiter, in den Programmen `'q0'`, `'q1'`; fehlende Übergänge führen in den Fehlerzustand q<sub>F</sub>, der nicht gezeichnet wird. Automatron gibt seinen drei Automaten die Buchstaben q, r und s, die Seite sagt das dazu.
- »Eintritt zwei Euro« prüft das Verhalten: Ob der dritte Zustand wirklich `q2` heißt, ist egal. Fehlt ein Übergang, etwa `('q2', 'drücken')`, bricht `run` mit einem `KeyError` ab, und die Rückmeldung zeigt genau dieses Paar; das ist ein guter Moment, über vollständige Tabellen zu sprechen.
- »Das Schloss reparieren« braucht vier zusätzliche Übergänge: `('q1', '1'): 'q1'`, `('q3', '1'): 'q1'`, `('q3', '2'): 'q2'` und `('q4', '1'): 'q1'`. Die Frage dahinter ist die nach dem längsten Ende des Getippten, das ein Anfang des Codes ist (die Idee hinter dem Suchverfahren von Knuth, Morris und Pratt).
- »Teilbar durch 5« gelingt mit zehn Zeilen von Hand oder mit zwei Schleifen und `(2 * r + d) % 5`.
- Für den Generalschlüssel kommen vier Übergänge aus `q0` über `q5`, `q6`, `q7` nach `q8` dazu und `'q8'` in `ACCEPTING`; der DEA hat dann 9 Zustände, so viele wie Automatrons minimierter DEA für `(0|1|2|3|4|5|6|7|8|9)*(1213|4711)`.

#### Was schiefgeht

- **`ModuleNotFoundError: No module named 'lock_nfa'`** heißt in Python, dass `convert.py` nicht im selben Ordner liegt wie `lock_nfa.py`; in Ruby sagt `require_relative` dasselbe mit `cannot load such file`.
- **`lock_nfa` gibt beim Laden Zeilen aus:** Es fehlt `if __name__ == '__main__':` (Python) oder `if __FILE__ == $0` (Ruby).
- **`convert` arbeitet nicht mehr mit dem Schloss:** Nach dem Umstellen auf `last` muss die Zeile für die Aufgabe zum Generalschlüssel wieder `lock_nfa` laden.
- **Kein Fehler für ein fehlendes Paar:** In Ruby meldet `TRANSITIONS.fetch` einen `KeyError`, wie Python bei `TRANSITIONS[…]`; mit eckigen Klammern gäbe Ruby still `nil` zurück.

#### Begriffe

Die Seite sagt DEA und NEA und erwähnt ε-Übergänge, Thompson-Konstruktion und Minimierung nur als Ausblick; Automatron zeigt alle Schritte mit Erklärungen. Endliche Automaten mit Ausgabe (Mealy, Moore) kommen nicht vor; das Drehkreuz ist als Einstieg gedacht, nicht als Akzeptor.

#### Tipps in Stufen

»Eintritt zwei Euro«, »Teilbar durch 5«, »Das Schloss reparieren« und »Ein Generalschlüssel« haben zwei oder drei Tipps: ein Beispiel von Hand, den Weg in Worten, meist ein Gerüst mit Lücken, nie die ganze Lösung. Die nächste Stufe erscheint eine Minute nach der vorigen, ohne Uhr, und wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst. Hängen viele an derselben Stelle, ist das ein Thema für die ganze Gruppe.

#### Weiter

Kellerautomaten und kontextfreie Grammatiken (Klammern), reguläre Ausdrücke mit `re`, und die Frage, warum `a^n b^n` nicht regulär ist (Pumping-Lemma, wenn es zum Lehrplan passt).
