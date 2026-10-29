Ein Projekt zu Aufwand und Laufzeit: Lernende zählen die verschiedenen Straßennamen Berlins auf drei Arten (eine Liste, Sortieren, eine Menge), messen, wie lange jede braucht, und zählen Schritte statt Sekunden. Sie sehen an einem Regler, wie schnell O(n²), O(2ⁿ) und O(n!) unbrauchbar werden, machen zwei langsame Programme schnell (Fibonacci mit Gedächtnis, ein Paar mit gegebener Summe mit einer Menge) und planen eine Rundfahrt zu Berlins Sehenswürdigkeiten, einmal mit allen Reihenfolgen und einmal immer zum nächsten Ort. Zum Schluss P und NP. Ruby oder Python, mit echten Daten aus dem Datensatz des Routenplaners.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Listen, Dictionaries und Mengen (<a href='/kata-dicts'>Katas: Dictionaries</a>), Rekursion (<a href='/kata-recursion'>Katas: Rekursion</a>, dort kommt Fibonacci vor). Der <a href='/routenplaner'>Routenplaner</a> ist keine Voraussetzung; wer ihn kennt, hat die Rundfahrt schon auf echten Straßen gesehen.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Daten holen, die drei Arten zu zählen, die Vorhersage zum Verdoppeln, messen und was die Messung zeigt.</li>
<li><b>2. Doppelstunde:</b> Schritte zählen und O-Notation, der Regler zum Wachstum, Hochrechnen, Fibonacci mit Gedächtnis und das Paar mit gegebener Summe.</li>
<li><b>3. Doppelstunde:</b> die Rundfahrt (Länge, alle Reihenfolgen, die Vorhersage für drei Orte mehr, der nächste Ort), wo es aufhört, P und NP. Extra: warum Sortieren durch Vergleichen O(n log n) braucht.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/laufzeit</code>: die Straßen (<code>berlin.tsv</code>, etwa 70 MB ausgepackt, und daraus <code>strassen.txt</code>) und die Orte (<code>berlin-places.tsv</code>) aus dem Datensatz auf GitHub, die Funktionen der Lernenden in <code>laufzeit.rb</code> oder <code>laufzeit.py</code>, dazu <code>messen</code> und <code>rundfahrt</code>, die sie herunterladen.</td></tr>
<tr><th>Prüfung</th><td>Acht Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf. Vier davon geben große Eingaben und eine knappe Zeit: Eine Menge, die eine Liste ist, Fibonacci ohne Gedächtnis oder zwei Schleifen ineinander sind richtig, aber zu langsam, und die Rückmeldung sagt, was sie stattdessen tun sollen. Vier Vorhersagen, eine Frage zur eigenen Messung und eine offene Frage am Ende.</td></tr>
<tr><th>Im Workspace</th><td><code>hyperfine</code> misst ein Programm mehrmals und gibt Mittelwert und Streuung an; es ist im Workspace-Image enthalten.</td></tr>
</table>

#### Was die Messung zeigt

Mit den echten Straßennamen wächst die Zeit von <code>distinct_list</code> zuerst etwa um das Vierfache je Verdopplung, später nur noch um das Doppelte: Berlin hat nur 11 772 verschiedene Straßennamen, und sind die meisten in der Liste, wächst sie nicht mehr. Das ist kein Messfehler, sondern der Anlass zu sagen, dass der Aufwand auch von den Daten abhängt (O(n · d) mit d verschiedenen Namen). Die Seite erklärt es nach der Frage zur Messung. Die Zeiten selbst hängen vom Rechner ab; das Verhältnis zur Menge (etwa hundertmal so lange bei 64 000 Namen) bleibt.

#### Was schiefgeht

- **Die Menge ist eine Liste:** <code>distinct_set</code> mit <code>include?</code> oder <code>in</code> auf einer Liste ist richtig, aber für 300 000 Namen zu langsam; die Rückmeldung nennt die Menge.
- **Sortiert, aber der erste Name fehlt:** Wer nur zählt, wo ein Name anders ist als der davor, zählt einen zu wenig.
- **Python: <code>memo={}</code> als Vorgabe:** funktioniert sogar, weil das Dictionary zwischen Aufrufen erhalten bleibt, ist aber eine bekannte Falle; der Tipp erklärt <code>memo=None</code>.
- **Ein Paar aus einer Zahl:** Wer die Zahl zuerst in die Menge legt und dann fragt, findet bei 5 und dem Ziel 10 ein Paar, das es nicht gibt.
- **Die Rundfahrt ohne Rückweg:** Ohne den Schritt vom letzten Ort zurück zum ersten ist sie zu kurz.
- **<code>wget</code> scheitert:** Die Daten liegen auf GitHub; ohne Netz, etwa im Prüfungsmodus, geht die Aktivität nicht.

#### Für die Besprechung

- Warum ein doppelt so schneller Rechner bei O(n²) nur gut 40 Prozent mehr Eingabe schafft und bei O(2ⁿ) nur eine einzige mehr.
- Was »gut genug« bei der Rundfahrt heißt: Bei den sechs Orten der Aufgabe ist die Rundfahrt zum nächsten Ort etwa 7 Prozent länger als die kürzeste und in Millisekunden fertig.
