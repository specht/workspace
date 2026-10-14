<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# MiniCPU

<p class='abstract'>
Der Prozessor der CPU Sandbox in drei Stufen: MiniCPU 8, 8S und 8X. Hier stehen seine Register, Flags und alle Befehle mit Opcode, Länge und Takten, dazu der Stack, die Register X und Y, der Maschinencode und der Assembler. Die Tabellen stammen aus denselben Tabellen, mit denen die Sandbox selbst rechnet.
</p>

<!-- part: stufen -->
<!-- short: Drei Stufen: 8 (Register, Flags, Sprünge), 8S (+ Stack), 8X (+ 64 KiB und X, Y). -->
## Die drei Stufen

Jede Stufe behält alles von der vorigen und fügt eine Idee hinzu. Welche Stufe ein Programm braucht, steht in seiner ersten Zeile: `.cpu 8`, `.cpu 8s` oder `.cpu 8x` (in C `#pragma cpu 8x`). Ohne diese Zeile läuft ein Assembler-Programm auf der MiniCPU 8, ein C-Programm auf der 8X. Das Menü »CPU« in der Sandbox ändert die Zeile.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th></th><th>MiniCPU 8</th><th>MiniCPU 8S</th><th>MiniCPU 8X</th></tr></thead>
<tbody>
<tr><th>Idee</th><td>Register, Flags, Sprünge</td><td>+ ein Stack</td><td>+ viel Speicher und Zeiger</td></tr>
<tr><th>RAM</th><td>256 Bytes</td><td>256 Bytes</td><td>65.536 Bytes (64 KiB)</td></tr>
<tr><th>Adressen</th><td>1 Byte</td><td>1 Byte</td><td>2 Bytes</td></tr>
<tr><th>Register</th><td><code>A</code> <code>B</code> <code>C</code> <code>D</code>, <code>PC</code></td><td>+ <code>SP</code></td><td>+ <code>X</code> <code>Y</code> (16 Bit) mit <code>XL</code> <code>XH</code> <code>YL</code> <code>YH</code></td></tr>
<tr><th>Flags</th><td><code>ZF</code> <code>CF</code></td><td><code>ZF</code> <code>CF</code></td><td><code>ZF</code> <code>CF</code></td></tr>
<tr><th>Neue Befehle</th><td>–</td><td><code>PUSH</code> <code>POP</code> <code>CALL</code> <code>RET</code>, <code>[SP+n]</code>, <code>ADD</code>/<code>SUB SP, n</code></td><td>keine: die bekannten Befehle können X und Y</td></tr>
<tr><th>Geräte</th><td colspan='3'>auf allen Stufen dieselben, über <code>IN</code> und <code>OUT</code> (<a href='/reference/minicpu-io#ports'>Geräte</a>)</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: regeln -->
<!-- short: Ziel zuerst, `[…]` heißt »der Speicher an«, zweiter Wert Register oder Zahl. -->
## Vier Regeln für alle Befehle

Auf allen drei Stufen gelten dieselben Regeln. Wer eine Stufe weitergeht, muss nichts umlernen.

- **Das Ziel steht vorn**: `LDI A, 5` lädt 5 in A, `ADD A, B` ändert A, `STA [0x70], A` schreibt nach 0x70.
- **Eckige Klammern heißen »der Speicher an«.** `LDI A, 0x70` lädt die Zahl 0x70, `LDA A, [0x70]` das Byte, das an Adresse 0x70 steht, und `LDA A, [B]` das Byte an der Adresse, die in B steht.
- **Der zweite Wert ist ein Register oder eine Zahl**, bei allen Rechen-, Logik- und Vergleichsbefehlen: `ADD A, B` und `ADD A, 16`, `AND A, 0x0F`, `CMP A, 10`.
- **Höhere Stufen bringen Register, keine neuen Schreibweisen.** Auf der 8X funktionieren `LDI`, `LDA`, `STA`, `MOV`, `ADD`, `SUB`, `CMP`, `INC`, `DEC`, `PUSH` und `POP` auch mit X und Y.

Ob eine Zahl dezimal (`112`), hexadezimal (`0x70`) oder binär (`0b0111_0000`) dasteht, ist gleich; mehr dazu unter [Assembler](#assembler).
<!-- /part -->

## MiniCPU 8

<!-- part: register-8 -->
<!-- short: 256 Bytes RAM, A–D mit je 8 Bit, PC; Start bei 0x00, Ende bei HLT. -->
### Speicher und Register

- **Ein RAM mit 256 Bytes**, Adressen `0x00` bis `0xFF`. Programm, Daten und Bildschirm teilen sich diesen Speicher; nichts hindert ein Programm daran, seine eigenen Befehle zu überschreiben.
- **Vier Register A, B, C, D** mit je 8 Bit, also Werten von 0 bis 255. Mehr passt nicht hinein: 255 + 1 ergibt 0, und das Flag CF merkt sich den Übertrag.
- **Der Programmzähler PC** enthält die Adresse des nächsten Befehls. Die CPU beginnt bei `0x00` und läuft, bis sie auf `HLT` trifft.
- **Zahlen über 255** belegen zwei Bytes. `ADC` und `SBC` rechnen den Übertrag des Befehls davor mit, so addiert `ADD B, D` und danach `ADC C, 0` den Wert D zur 16-Bit-Zahl in C (oben) und B (unten).

Beim Start stehen A bis D und die Flags auf 0.
<!-- /part -->

<!-- part: flags -->
<!-- short: ZF = Ergebnis 0; CF = Übertrag oder Borgen; nach CMP: JZ gleich, JC kleiner. -->
### Flags

Zwei Flags halten fest, wie der letzte Rechenbefehl ausgegangen ist. Die Sprungbefehle entscheiden danach.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Flag</th><th>ist 1, wenn …</th></tr></thead>
<tbody>
<tr><td><code>ZF</code> (Zero)</td><td>das Ergebnis 0 ist. Nach <code>CMP</code>: beide Werte gleich waren.</td></tr>
<tr><td><code>CF</code> (Carry)</td><td>es einen Übertrag aus Bit 7 gab (<code>ADD</code>, <code>ADC</code>, <code>INC</code>, <code>SHL</code>), etwas geborgt werden musste, weil der erste Wert kleiner war (<code>SUB</code>, <code>SBC</code>, <code>DEC</code>, <code>CMP</code>), oder <code>SHR</code> eine 1 hinausgeschoben hat.</td></tr>
</tbody>
</table>
</div>

`AND`, `OR` und `XOR` setzen ZF nach ihrem Ergebnis und löschen CF immer; `CLF` löscht beide. Laden, Speichern, `MOV`, Sprünge, der Stack und die Geräte lassen die Flags, wie sie sind. In der Befehlstabelle steht <i class='bi bi-check-lg minicpu-set' role='img' aria-label='ein Haken'></i>, wo ein Befehl das Flag nach seinem Ergebnis setzt, 0, wo er es löscht, und –, wo er es nicht verändert.

**Vergleichen.** `CMP A, B` rechnet A − B, behält aber nur die Flags. Danach gilt (für Zahlen von 0 bis 255):

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Bedingung</th><th>Flags</th><th>Sprung</th></tr></thead>
<tbody>
<tr><td>A = B</td><td>ZF = 1</td><td><code>JZ</code></td></tr>
<tr><td>A ≠ B</td><td>ZF = 0</td><td><code>JNZ</code></td></tr>
<tr><td>A &lt; B</td><td>CF = 1</td><td><code>JC</code></td></tr>
<tr><td>A ≥ B</td><td>CF = 0</td><td><code>JNC</code></td></tr>
<tr><td>A &gt; B</td><td>CF = 0 und ZF = 0</td><td>andersherum vergleichen: <code>CMP B, A</code>, dann <code>JC</code></td></tr>
</tbody>
</table>
</div>

In der Sandbox erklärt die Maus über ZF und CF, warum der letzte Befehl das Flag gesetzt oder gelöscht hat.
<!-- /part -->

<!-- part: befehle-8 -->
<!-- short: Alle Befehle der MiniCPU 8 mit Opcode, Flags, Bytes und Takten. -->
### Befehle

Alle Befehle der MiniCPU 8. Die Beispiele stehen für alle Register: Statt `ADD A, B` geht auch `ADD C, D`, statt 5 jede Zahl von 0 bis 255. Bei bedingten Sprüngen stehen zwei Taktzahlen: ohne Sprung / mit Sprung.

<!-- minicpu-table: befehle-8 -->

<div class='scroll-x'><table class='table table-sm reference-table minicpu-table'>
<thead><tr><th>Befehl</th><th>Wirkung</th><th>ZF</th><th>CF</th><th>Bytes</th><th>Takte</th><th>Opcode</th></tr></thead>
<tbody>
<tr class='minicpu-family'><th colspan='7'>Steuerung</th></tr>
<tr><td><code>NOP</code></td><td>Tut nichts.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>1</td><td class='minicpu-num'>1</td><td><code>0x00</code></td></tr>
<tr><td><code>HLT</code></td><td>Hält die CPU an.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>1</td><td class='minicpu-num'>1</td><td><code>0x01</code></td></tr>
<tr><td><code>CLF</code></td><td>Löscht ZF und CF.</td><td class='minicpu-flag'>0</td><td class='minicpu-flag'>0</td><td class='minicpu-num'>1</td><td class='minicpu-num'>1</td><td><code>0x02</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Daten bewegen</th></tr>
<tr><td><code>LDI A, 42</code></td><td>Lädt die Zahl 42 in A.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x10</code></td></tr>
<tr><td><code>LDA A, [0x70]</code></td><td>Lädt das Byte an Adresse 0x70 in A.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x11</code></td></tr>
<tr><td><code>LDA A, [B]</code></td><td>Lädt das Byte an der Adresse, die in B steht.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x12</code></td></tr>
<tr><td><code>STA [0x70], A</code></td><td>Speichert A an Adresse 0x70.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x13</code></td></tr>
<tr><td><code>STA [B], A</code></td><td>Speichert A an der Adresse, die in B steht.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x14</code></td></tr>
<tr><td><code>MOV A, B</code></td><td>Kopiert B nach A.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x15</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Rechnen</th></tr>
<tr><td><code>ADD A, B</code></td><td>A = A + B. CF = Übertrag aus dem obersten Bit.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x20</code></td></tr>
<tr><td><code>ADD A, 5</code></td><td>A = A + 5.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x21</code></td></tr>
<tr><td><code>SUB A, B</code></td><td>A = A − B. CF = geborgt (A war kleiner).</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x22</code></td></tr>
<tr><td><code>SUB A, 5</code></td><td>A = A − 5.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x23</code></td></tr>
<tr><td><code>ADC A, B</code></td><td>A = A + B + CF: addiert den Übertrag eines ADD davor.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x24</code></td></tr>
<tr><td><code>ADC A, 0</code></td><td>A = A + 0 + CF.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x25</code></td></tr>
<tr><td><code>SBC A, B</code></td><td>A = A − B − CF: zieht das Geborgte eines SUB davor ab.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x26</code></td></tr>
<tr><td><code>SBC A, 0</code></td><td>A = A − 0 − CF.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x27</code></td></tr>
<tr><td><code>INC A</code></td><td>A = A + 1.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x28</code></td></tr>
<tr><td><code>DEC A</code></td><td>A = A − 1.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x29</code></td></tr>
<tr><td><code>SHL A</code></td><td>Schiebt nach links; das oberste Bit kommt nach CF.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x2A</code></td></tr>
<tr><td><code>SHR A</code></td><td>Schiebt nach rechts; Bit 0 kommt nach CF.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x2B</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Logik</th></tr>
<tr><td><code>AND A, B</code></td><td>Bitweises UND.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x30</code></td></tr>
<tr><td><code>AND A, 0x0F</code></td><td>Bitweises UND mit einer Zahl.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x31</code></td></tr>
<tr><td><code>OR A, B</code></td><td>Bitweises ODER.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x32</code></td></tr>
<tr><td><code>OR A, 0x80</code></td><td>Bitweises ODER mit einer Zahl.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x33</code></td></tr>
<tr><td><code>XOR A, B</code></td><td>Bitweises exklusives ODER.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x34</code></td></tr>
<tr><td><code>XOR A, 0xFF</code></td><td>Bitweises exklusives ODER mit einer Zahl.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'>0</td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x35</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Vergleichen</th></tr>
<tr><td><code>CMP A, B</code></td><td>Rechnet A − B, nur für die Flags.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>2</td><td class='minicpu-num'>2</td><td><code>0x40</code></td></tr>
<tr><td><code>CMP A, 10</code></td><td>Rechnet A − 10, nur für die Flags.</td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-flag'><i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i></td><td class='minicpu-num'>3</td><td class='minicpu-num'>3</td><td><code>0x41</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Springen</th></tr>
<tr><td><code>JMP label</code></td><td>Springt immer.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x60</code></td></tr>
<tr><td><code>JZ label</code></td><td>Springt, wenn ZF = 1 (null, nach CMP: gleich).</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2 / 3</td><td><code>0x61</code></td></tr>
<tr><td><code>JNZ label</code></td><td>Springt, wenn ZF = 0.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2 / 3</td><td><code>0x62</code></td></tr>
<tr><td><code>JC label</code></td><td>Springt, wenn CF = 1 (Übertrag, nach CMP: kleiner).</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2 / 3</td><td><code>0x63</code></td></tr>
<tr><td><code>JNC label</code></td><td>Springt, wenn CF = 0.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>2 / 3</td><td><code>0x64</code></td></tr>
<tr class='minicpu-family'><th colspan='7'>Ein- und Ausgabe</th></tr>
<tr><td><code>IN A, 0x02</code></td><td>Liest ein Byte vom Gerät an Port 2.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x70</code></td></tr>
<tr><td><code>OUT 0x11, A</code></td><td>Schickt A an das Gerät an Port 0x11.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x71</code></td></tr>
</tbody>
</table>
</div>

<!-- /minicpu-table -->
<!-- /part -->

## MiniCPU 8S

<!-- part: stack -->
<!-- short: SP beginnt bei 0x100; PUSH legt ab, POP holt zurück, der Stack wächst nach unten. -->
### Der Stack

- **SP**, der Stackzeiger, beginnt bei `0x100`, eins hinter dem Ende des RAMs. `PUSH A` zieht 1 von SP ab und speichert A dort; `POP A` liest das Byte und zählt SP wieder hoch. Der Stack wächst also durch das gewöhnliche RAM nach unten.
- **`CALL label`** legt die Adresse des nächsten Befehls auf den Stack und springt; **`RET`** holt sie wieder herunter und macht dort weiter. Aufrufe dürfen verschachtelt sein.
- **`LDA A, [SP+n]`** und **`STA [SP+n], A`** lesen und schreiben `n` Bytes über SP, ohne etwas vom Stack zu nehmen. **`SUB SP, n`** macht Platz für `n` Bytes, **`ADD SP, n`** gibt sie wieder frei.
- Stack, Programm und Bildschirm teilen sich weiterhin 256 Bytes: Zu viele PUSHes überschreiben, was unter dem Stack liegt. Die RAM-Ansicht der Sandbox markiert den Stack.
<!-- /part -->

<!-- part: befehle-8s -->
<!-- short: Neu auf der 8S: PUSH, POP, CALL, RET, [SP+n], ADD/SUB SP. -->
### Befehle für den Stack

Diese Befehle kommen auf der 8S zu allen Befehlen der MiniCPU 8 hinzu.

<!-- minicpu-table: befehle-8s -->

<div class='scroll-x'><table class='table table-sm reference-table minicpu-table'>
<thead><tr><th>Befehl</th><th>Wirkung</th><th>ZF</th><th>CF</th><th>Bytes</th><th>Takte</th><th>Opcode</th></tr></thead>
<tbody>
<tr class='minicpu-family'><th colspan='7'>Stack</th></tr>
<tr><td><code>PUSH A</code></td><td>SP = SP − 1, dann A nach [SP].</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x50</code></td></tr>
<tr><td><code>POP A</code></td><td>A aus [SP], dann SP = SP + 1.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x51</code></td></tr>
<tr><td><code>CALL label</code></td><td>Legt die Rücksprungadresse auf den Stack und springt.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>4</td><td><code>0x52</code></td></tr>
<tr><td><code>RET</code></td><td>Nimmt die Rücksprungadresse vom Stack und springt dorthin.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>1</td><td class='minicpu-num'>3</td><td><code>0x53</code></td></tr>
<tr><td><code>LDA A, [SP+2]</code></td><td>Lädt das Byte 2 Plätze über SP: ein Argument oder eine lokale Variable.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x54</code></td></tr>
<tr><td><code>STA [SP+2], A</code></td><td>Speichert A 2 Plätze über SP.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>3</td><td class='minicpu-num'>4</td><td><code>0x55</code></td></tr>
<tr><td><code>ADD SP, 3</code></td><td>Gibt 3 Bytes auf dem Stack frei (lokale Variablen, oder Argumente nach dem Aufruf).</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x56</code></td></tr>
<tr><td><code>SUB SP, 3</code></td><td>Macht auf dem Stack Platz für 3 Bytes lokale Variablen.</td><td class='minicpu-flag'>–</td><td class='minicpu-flag'>–</td><td class='minicpu-num'>2</td><td class='minicpu-num'>3</td><td><code>0x57</code></td></tr>
</tbody>
</table>
</div>

<!-- /minicpu-table -->
<!-- /part -->

<!-- part: funktionen -->
<!-- short: Argumente PUSHen, CALL; in der Funktion SUB SP, n und [SP+n]; Ergebnis in A. -->
### Funktionen mit Argumenten

So rufen sich Funktionen gegenseitig auf; der C-Compiler macht es bei jeder Funktion genauso:

1. Der Aufrufer legt die Argumente mit `PUSH` auf den Stack und ruft die Funktion mit `CALL` auf.
2. Die Funktion macht mit `SUB SP, n` Platz für ihre lokalen Variablen.
3. Sie liest und schreibt lokale Variablen und Argumente mit `[SP+n]`: zuerst kommen die lokalen Variablen, dann die Rücksprungadresse, dann die Argumente.
4. Sie gibt ihre lokalen Variablen mit `ADD SP, n` frei und kehrt mit `RET` zurück, das Ergebnis in A.
5. Der Aufrufer gibt die Argumente mit `ADD SP, n` frei.

```asm
.cpu 8s
        LDI A, 21
        PUSH A              ; das Argument
        CALL double
        ADD SP, 1           ; Argument wieder freigeben
        OUT 0x11, A         ; zeigt 42
        HLT

double: LDA A, [SP+1]       ; [SP+0] ist die Rücksprungadresse
        ADD A, A
        RET
```

Jeder Aufruf hat seine eigenen Argumente und lokalen Variablen, deshalb darf eine Funktion sich selbst aufrufen. Einen eigenen Rahmenzeiger gibt es nicht, alle Abstände zählen ab SP. Jedes `PUSH` mitten in einer Funktion verschiebt sie deshalb um eins. Am einfachsten reserviert eine Funktion alle lokalen Variablen gleich am Anfang. Auf der 8X ist die Rücksprungadresse zwei Bytes lang, die Argumente beginnen dort ein Byte höher.
<!-- /part -->

## MiniCPU 8X

<!-- part: speicher-8x -->
<!-- short: 64 KiB RAM, Adressen mit zwei Bytes (unteres zuerst), SP beginnt bei 0x10000. -->
### 64 KiB und Adressen mit zwei Bytes

- **65.536 Bytes RAM**, Adressen `0x0000` bis `0xFFFF`: alles, was eine 16-Bit-Adresse erreicht, so wie 256 Bytes alles sind, was 8 Bit erreichen. Platz für Tabellen, große Bildschirme mit zwei Seiten und längere Programme.
- **Jede Adresse hat zwei Bytes**, das untere zuerst. `LDA A, [0x0812]`, `STA [0x0812], A`, Sprünge und `CALL` werden dadurch ein Byte länger und einen Takt langsamer.
- **PC und SP haben 16 Bit.** SP beginnt bei `0x10000`, ganz oben. X und Y stehen beim Start auf 0. `CALL` legt eine Rücksprungadresse mit zwei Bytes ab.
- `LDA A, [B]` gibt es weiter, aber B erreicht nur die ersten 256 Bytes. Für alles darüber sind X und Y da.
<!-- /part -->

<!-- part: x-und-y -->
<!-- short: X und Y: 16-Bit-Register für Adressen, mit LDI, LDA, STA, ADD, CMP, INC, PUSH …; XL/XH für MOV. -->
### X und Y

X und Y sind zwei Register mit 16 Bit, gedacht als Zeiger. Sie brauchen keine neuen Befehle:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Befehl</th><th>Wirkung</th></tr></thead>
<tbody>
<tr><td><code>LDI X, 0x0800</code></td><td>lädt eine 16-Bit-Zahl</td></tr>
<tr><td><code>LDA X, [0x0700]</code> · <code>STA [0x0700], X</code></td><td>lädt und speichert zwei Bytes, das untere zuerst</td></tr>
<tr><td><code>LDA X, [SP+2]</code> · <code>STA [SP+2], X</code></td><td>dasselbe auf dem Stack</td></tr>
<tr><td><code>MOV X, Y</code></td><td>kopiert</td></tr>
<tr><td><code>ADD X, 32</code> · <code>ADD X, A</code> · <code>ADD X, Y</code></td><td>addiert eine Zahl, ein Byte (0–255) oder ein 16-Bit-Register; <code>SUB</code> ebenso</td></tr>
<tr><td><code>CMP X, 0x0900</code> · <code>CMP X, Y</code></td><td>vergleicht alle 16 Bit</td></tr>
<tr><td><code>INC X</code> · <code>DEC X</code></td><td>zum nächsten oder vorigen Byte</td></tr>
<tr><td><code>PUSH X</code> · <code>POP X</code></td><td>zwei Bytes auf den Stack und zurück</td></tr>
<tr><td><code>LDA A, [X]</code> · <code>STA [X], A</code></td><td>X als Zeiger: das Byte an der Adresse, die in X steht</td></tr>
</tbody>
</table>
</div>

Bei X und Y beziehen sich ZF und CF auf alle 16 Bit. Schieben, Logik, `ADC` und `SBC` gibt es nur für A bis D, und ein 8-Bit-Register kann kein 16-Bit-Register aufnehmen: `ADD A, X` geht nicht.

**XL, XH, YL, YH** sind das untere und das obere Byte von X und Y. Sie funktionieren nur mit `MOV`, in beide Richtungen zwischen ihnen und A bis D: `MOV XH, A`, `MOV B, YL`.
<!-- /part -->

<!-- part: befehle-8x -->
<!-- short: Auf der 8X sind Adressen, X und Y teurer: ein Byte und ein Takt mehr. -->
### Was die 8X kostet

Breiter ist langsamer: Eine Adresse hat zwei Bytes, eine Zahl für X oder Y auch, Laden und Speichern bewegen bei X und Y zwei Bytes, und die 8-Bit-Recheneinheit braucht für 16-Bit-Rechnungen einen Takt mehr. Die Tabelle nennt Bytes · Takte für jeden Befehl, der auf der 8X anders kostet oder X und Y kann; alle anderen kosten so viel wie auf der 8.

<!-- minicpu-table: kosten-8x -->

<div class='scroll-x'><table class='table table-sm reference-table minicpu-table'>
<thead><tr><th>Befehl</th><th>8 / 8S</th><th>8X</th><th>8X mit X oder Y</th></tr></thead>
<tbody>
<tr><td><code>LDI A, 42</code></td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>4 · 5</td></tr>
<tr><td><code>LDA A, [0x70]</code></td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'><strong>4 · 5</strong></td><td class='minicpu-num'>4 · 6</td></tr>
<tr><td><code>STA [0x70], A</code></td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'><strong>4 · 5</strong></td><td class='minicpu-num'>4 · 6</td></tr>
<tr><td><code>MOV A, B</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>ADD A, B</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>ADD A, 5</code></td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>4 · 5</td></tr>
<tr><td><code>SUB A, B</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>SUB A, 5</code></td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>4 · 5</td></tr>
<tr><td><code>INC A</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>DEC A</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>CMP A, B</code></td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 2</td><td class='minicpu-num'>2 · 3</td></tr>
<tr><td><code>CMP A, 10</code></td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>3 · 3</td><td class='minicpu-num'>4 · 5</td></tr>
<tr><td><code>PUSH A</code></td><td class='minicpu-num'>2 · 3</td><td class='minicpu-num'>2 · 3</td><td class='minicpu-num'>2 · 4</td></tr>
<tr><td><code>POP A</code></td><td class='minicpu-num'>2 · 3</td><td class='minicpu-num'>2 · 3</td><td class='minicpu-num'>2 · 4</td></tr>
<tr><td><code>CALL label</code></td><td class='minicpu-num'>2 · 4</td><td class='minicpu-num'><strong>3 · 6</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>RET</code></td><td class='minicpu-num'>1 · 3</td><td class='minicpu-num'><strong>1 · 4</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>LDA A, [SP+2]</code></td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'>3 · 5</td></tr>
<tr><td><code>STA [SP+2], A</code></td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'>3 · 4</td><td class='minicpu-num'>3 · 5</td></tr>
<tr><td><code>JMP label</code></td><td class='minicpu-num'>2 · 3</td><td class='minicpu-num'><strong>3 · 4</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>JZ label</code></td><td class='minicpu-num'>2 · 2 / 3</td><td class='minicpu-num'><strong>3 · 3 / 4</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>JNZ label</code></td><td class='minicpu-num'>2 · 2 / 3</td><td class='minicpu-num'><strong>3 · 3 / 4</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>JC label</code></td><td class='minicpu-num'>2 · 2 / 3</td><td class='minicpu-num'><strong>3 · 3 / 4</strong></td><td class='minicpu-num'>–</td></tr>
<tr><td><code>JNC label</code></td><td class='minicpu-num'>2 · 2 / 3</td><td class='minicpu-num'><strong>3 · 3 / 4</strong></td><td class='minicpu-num'>–</td></tr>
</tbody>
</table>
</div>

<!-- /minicpu-table -->
<!-- /part -->

<!-- part: takte -->
<!-- short: Ein Takt pro Befehlsbyte, pro Datenbyte, pro Sprung, pro 16-Bit-Rechnung. -->
## Takte

Der Regler »Clock« stellt den Takt von 1 Hz bis 64 MHz ein, und die CPU läuft in diesem Takt in Echtzeit. Wie viele Takte ein Befehl braucht, folgt aus einem einfachen Modell einer 8-Bit-CPU, deren Tempo der Speicherbus begrenzt:

- 1 Takt für jedes Byte des Befehls, das geholt wird,
- 1 Takt für jedes Datenbyte, das gelesen oder geschrieben wird (RAM, Stack, Rücksprungadresse, Port), 2 bei X und Y,
- 1 Takt, wenn PC einen neuen Wert bekommt (`JMP`, ein bedingter Sprung, der springt, `CALL`, `RET`),
- 1 Takt für eine 16-Bit-Rechnung (mit X, Y oder SP), weil die Recheneinheit nur 8 Bit breit ist.

So braucht `ADD A, B` 2 Takte, `LDI A, 5` 3, `LDA A, [0x70]` 4 (auf der 8X 5) und `JNZ loop` 2, wenn es nicht springt, und 3, wenn es springt. Die Statuszeile der Sandbox zeigt die verbrauchten Takte und wie lange sie beim gewählten Takt dauern; »Cycles« zeigt einen Befehl Takt für Takt.
<!-- /part -->

<!-- part: maschinencode -->
<!-- short: Erste Hex-Ziffer = Befehlsfamilie; Registerbytes A=0 … Y=9; Zahlen und Adressen unteres Byte zuerst. -->
## Maschinencode

Neben dem Code zeigt die Sandbox die Bytes jeder Zeile. So entstehen sie:

- **Der Opcode** ist das erste Byte. Seine erste Hex-Ziffer nennt die Familie: `0x` Steuerung, `1x` Daten bewegen, `2x` Rechnen, `3x` Logik, `4x` Vergleichen, `5x` Stack, `6x` Springen, `7x` Ein- und Ausgabe. Höhere Stufen fügen Opcodes hinzu, nummerieren aber keine um.
- **Ein Register** ist ein Byte: A = 0, B = 1, C = 2, D = 3; auf der 8X auch XL = 4, XH = 5, YL = 6, YH = 7, X = 8, Y = 9.
- **Zwei Register** teilen sich ein Byte: erstes × 16 + zweites. `ADD A, B` ist `20 01`, `MOV B, A` ist `15 10`.
- **Eine Zahl** folgt dem Registerbyte: ein Byte, bei X und Y zwei (das untere zuerst). `LDI A, 5` ist `10 00 05`, `LDI X, 0x0800` ist `10 08 00 08`.
- **Eine Adresse** hat ein Byte (8, 8S) oder zwei, das untere zuerst (8X): `JMP 0x23` ist auf der 8 `60 23`, auf der 8X `60 23 00`.

Ein Byte, das kein Befehl ist, führt zu einer Fehlermeldung, ebenso ein Befehl, den die Stufe noch nicht kennt.
<!-- /part -->

<!-- part: assembler -->
<!-- short: `label:`, `.cpu`, `.org`, `.db`, `.equ`, `.display`; Zahlen dezimal, 0x, 0b, 'A'; Kommentare mit ;. -->
## Assembler

Eine Zeile enthält höchstens einen Befehl. Groß- und Kleinschreibung der Befehle ist egal, Kommentare beginnen mit `;`.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Schreibweise</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>loop:</code></td><td>Eine Marke ist die Adresse des Bytes, das danach kommt. Sie beginnt mit einem Buchstaben oder <code>_</code>; ein Befehl darf in derselben Zeile folgen.</td></tr>
<tr><td><code>.cpu 8x</code></td><td>wählt die Stufe: <code>8</code>, <code>8s</code> oder <code>8x</code>. Ohne diese Zeile die MiniCPU 8.</td></tr>
<tr><td><code>.org 0x60</code></td><td>legt die folgenden Bytes ab dieser Adresse ab.</td></tr>
<tr><td><code>.db 1, 2, "Hi", 0</code></td><td>schreibt Bytes ins RAM: Zahlen, Zeichen oder Texte.</td></tr>
<tr><td><code>WIDTH .equ 16</code></td><td>eine Konstante mit Namen.</td></tr>
<tr><td><code>.display 0x80, 16, 8, gray8</code></td><td>schlägt vor, wie der Bildschirm das RAM zeigt (<a href='/reference/minicpu-io#display'>Bildschirm</a>).</td></tr>
</tbody>
</table>
</div>

**Werte** schreibst du dezimal `42`, hexadezimal `0x2A`, binär `0b0010_1010` oder als Zeichen `'A'` (65). Marken und Konstanten lassen sich mit `+ - * / & | << >>` und Klammern verrechnen, etwa `screen + 16 * 3` oder `table >> 8`. Negative Bytes laufen über: `-1` ist 255.

Die Sandbox übersetzt schon beim Tippen. Fehler erscheinen unter dem Editor, ein Klick springt in die Zeile. Die Meldungen erklären mit diesen Regeln, was gemeint war: Auf `LDA A, 0x70` antwortet sie, dass `LDA` den Speicher liest, und schlägt `LDA A, [0x70]` oder `LDI A, 0x70` vor.
<!-- /part -->
