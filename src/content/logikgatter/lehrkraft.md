Lernende schalten Gatter, Addierer, Flipflops und ein kleines Rechenwerk auf der Seite durch, sagen ihr Verhalten vorher und schreiben jede Schaltung danach als Funktion in Ruby oder Python, die ihre Eingänge nur durch Gatter schicken darf: von NICHT und UND aus NAND bis zu einem Rechenwerk mit `ADD`, `SUB`, `AND`, `OR`, `XOR` und den Flags der CPU Sandbox.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Das Zweiersystem und Funktionen mit Listen, etwa aus den Katas »Bits und Zahlensysteme«; für das Subtrahieren hilft die Kata »Negative Zahlen in einem Byte« (Zweierkomplement). Die CPU Sandbox ist keine Voraussetzung: Die Seite führt zu ihr hin.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python, wie auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« eingestellt, ohne Einstellung Ruby. Eine Aufgabe, die in einer Sprache gelöst ist, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Gatter und ihre Schaltzeichen, die Regel von De Morgan als Vorhersage, NICHT, UND, ODER und XOR aus NAND, der Halbaddierer.</li>
<li><b>2. Doppelstunde:</b> Volladdierer, Addierer für 4 Bit mit der Vorhersage zum Überlauf, RS- und D-Flipflop.</li>
<li><b>3. Doppelstunde oder für Schnelle:</b> das Rechenwerk: Subtrahieren mit dem Addierer, die Flags, die Kata <code>alu</code> und der Vergleich mit der CPU Sandbox.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Keine: Der Code steht in den Karten der Seite (Katas), der Workspace muss nicht laufen.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sieben Katas (<code>function</code>): Die Prüfung ruft die Funktionen mit allen Kombinationen der Eingänge auf, beim Addierer und beim Rechenwerk mit 10 und 15 Paaren.</li>
<li>Jeder Aufruf läuft zweimal: einmal mit 0 und 1, dann mit Signalen, die nur die Gatter der Aufgabe lesen können (<code>gates:</code>). Wer selbst rechnet oder vergleicht (<code>a &amp; b</code>, <code>if a == 1</code>, eine Tabelle), besteht den ersten Lauf und nicht den zweiten, mit einer Rückmeldung, die das sagt.</li>
<li>Sieben Fragen mit Auswahl oder Zahl, vier davon Vorhersagen vor der Schaltung, die sie beantwortet.</li>
</ul></td></tr>
<tr><th>Lösung</th><td>Unter jeder Kata nach dem Lösen; als Lehrkraft mit »Lösung einsetzen«.</td></tr>
</table>

#### Die Aufgaben

- **»Zwei Schaltungen«** (Vorhersage): nicht (a oder b) und (nicht a) und (nicht b) liefern in allen vier Zeilen dasselbe, De Morgan. Die Schaltung zum Durchklicken steht erst nach der Antwort da.
- **»NICHT und UND aus NAND«** und **»ODER und XOR aus NAND«**: `nand` steht im Startcode. Typische Fehler mit eigener Rückmeldung: `nand(a, 0)` als NICHT (immer 1), das NAND selbst als UND, beim ODER `nand(a, b)` ohne umgedrehte Eingänge.
- **»Summe und Übertrag«**: XOR und UND, aus der Tabelle abzulesen. **»Ein Halbaddierer«** übersetzt das; in Python fängt die Rückmeldung `return s, c` ab (ein Tupel statt einer Liste).
- **»Wann gibt es einen Übertrag?«**: 4 der 8 Zeilen. **»Ein Volladdierer«** aus zwei Halbaddierern; die Rückmeldungen erkennen UND statt ODER und einen vergessenen Übertrag.
- **»Wenn die vier Bits nicht reichen«** (Vorhersage): 1111 + 0001 ergibt 0000 mit Übertrag 1, wie 255 + 1 in der CPU Sandbox. **»Ein Addierer für 4 Bit«**: Listen mit dem höchsten Bit vorn, die Schleife läuft vom Index 3 abwärts; wer andersherum läuft, bekommt das gesagt.
- **»S kurz auf 1«** und **»S und R zugleich«** (Vorhersagen): Das Flipflop hält die 1; der verbotene Zustand lässt sich nicht vorhersagen, in der Simulation schwingt die Schaltung. **»Was das D-Flipflop speichert«**: `d_latch(d, clock, q)` als Multiplexer aus Gattern.
- **»Subtrahieren mit einem Addierer«**: b umdrehen und 1 dazu. **»Ein Rechenwerk für 4 Bit«** (Knobelei): Mit `if op == …` darf gewählt werden, die Bits gehen nur durch Gatter. CF ist bei `sub` das Gegenteil des Übertrags; wer das vergisst, bekommt eine eigene Rückmeldung.

#### Was schiefgeht

- **»Mit 0 und 1 stimmt …, kommen die Eingänge aber als Signale …«:** Der Code rechnet selbst mit den Eingängen. Die Gatter im Startcode sind mit `==` geschrieben; das dürfen nur sie.
- **Eine Funktion aus einer früheren Karte fehlt:** Jede Karte ist ein eigenes Programm. `half_adder`, `full_adder` und `add4` werden in die späteren Karten kopiert; der Startcode sagt, wohin.
- **Der Übertrag läuft falsch herum:** Index 0 ist das höchste Bit, wie die Zahl geschrieben wird.

#### Begriffe

Schaltzeichen nach DIN EN 60617 (Rechtecke mit `&`, `≥1`, `=1`, `1`); die Seite nennt die amerikanischen Formen nach ANSI/IEEE 91 nur. Schaltnetz und Schaltwerk, RS-Flipflop aus zwei NOR, D-Flipflop taktzustandsgesteuert (D-Latch); flankengesteuerte Flipflops kommen nur als Ausblick vor. Die Flags ZF und CF sind die der CPU Sandbox: CF nach `SUB` ist das Borgen, nicht der Übertrag des Addierers.

#### Weiter

Die [CPU Sandbox](/sandbox-cpu) mit ihren Flags. Wer Schaltungen frei bauen will, findet in Simulatoren wie [Digital](https://github.com/hneemann/Digital) dieselben Schaltzeichen; das Buch und der Kurs »From Nand to Tetris« am Ende der Seite gehen von hier bis zum ganzen Computer.
