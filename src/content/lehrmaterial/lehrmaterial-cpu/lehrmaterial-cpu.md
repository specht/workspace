<div class='meta'>
image: cpu-sandbox.webp
</div>

# Rechnerarchitektur mit der MiniCPU

<p class='abstract'>
In der CPU Sandbox programmieren Lernende eine kleine 8-Bit-CPU in Assembler
oder C. Register, Flags, Maschinenbytes, RAM, Bildschirm und Geräte lassen
sich während der Ausführung unmittelbar beobachten.
</p>

**Werkzeug:** [CPU Sandbox](https://github.com/specht/cpu-sandbox) ·
**Dokumentierter Stand:** [`730060800243aaa5fa1622b6b94567c9857fa1a0`](https://github.com/specht/cpu-sandbox/commit/730060800243aaa5fa1622b6b94567c9857fa1a0)
(12.10.2026). Die Screenshot-Rezepte verwenden die **fest eingebauten
Beispielprogramme** dieses Commits und keine leeren Arbeitsdateien.

## 1. Was die MiniCPU alles kann

Der Einstieg benötigt nur Laden, Addieren, Speichern und Springen. Ein
simulierter Computer wird aber erst spannend, wenn Programme seine Hardware
wirklich nutzen: Zur CPU Sandbox gehören **26 Beispielprogramme** von einfachen
Rechenaufgaben über Spiele bis zu aufwendigen Grafikeffekten. Die Grafik
entsteht vollständig durch Programme auf der simulierten Maschine.

<!-- tutorial-screenshot
terminal-open
terminal-run: cd /workspace && git clone -q https://github.com/specht/cpu-sandbox.git && cd cpu-sandbox && git checkout -q 730060800243aaa5fa1622b6b94567c9857fa1a0
terminal-wait-for-prompt
terminal-run: cd /workspace/cpu-sandbox && mkdir -p programs && (nohup ./run --port 18765 > /tmp/cpu-sandbox.log 2>&1 < /dev/null &)
terminal-wait-for-prompt
preview-port: 18765
tab: preview
viewport: 1600x900
wait-for-text: Class progress
click: selector:#program-picker .picker-button
click: selector:#program-picker .picker-filter
type: 25-raycaster.c
click: selector:#program-picker .picker-option
sleep: 2
click: selector:#clock
press: End
click: selector:#run
sleep: 8
-->
<img class='full' src='cpu-sandbox.webp' alt='CPU Sandbox mit laufendem 3D-Raycaster, C-Programm, Registern und RAM.'>

Das Beispiel `25-raycaster.c` zeigt einen texturierten 3D-Irrgarten,
`18-cube.c` einen rotierenden 3D-Würfel; weitere Beispiele zeigen Feuer,
Mandelbrot, Breakout, Snake, Plasma, Game of Life und eine Landschaft mit
Wasserspiegelung. Daraus ergeben sich Fragen zu Festkommaarithmetik,
Speichermodellen, Optimierung und grafischer Darstellung.

## 2. Drei Ausbaustufen, dieselbe Befehlssprache

| Modell | Neu im Unterricht | Speicher und Register |
| --- | --- | --- |
| **MiniCPU 8** | Daten bewegen, rechnen, vergleichen, Schleifen | 256 Byte; A–D, PC, ZF, CF |
| **MiniCPU 8S** | Unterprogramme, Stack, `CALL`/`RET`, Rekursion | zusätzlich Stackpointer SP |
| **MiniCPU 8X** | Zeiger, größere Programme, grafische Demos | 64 KiB, 16-Bit-Adressen, X und Y |

Die Assembler-Syntax bleibt über alle drei Ausbaustufen einheitlich. Der
Unterschied zwischen `LDI A, 0x70` (Wert) und `LDA A, [0x70]` (Speicherinhalt)
kann ebenso untersucht werden wie Überläufe und die Bedeutung der Flags.

## 3. Cycles: Fetch–Decode–Execute beobachten

**Step** führt einen Befehl aus, **Step back** macht ihn rückgängig.
Im Modus **Cycles** verfolgt man stattdessen einzelne Takte: Opcode holen,
dekodieren, Operanden lesen, Speicherzugriffe und Sprünge. Dabei zeigt die
Anwendung auch Adress- und Datenbus. Die Bytes jedes Assemblerbefehls stehen
direkt neben dem Quelltext.

<!-- tutorial-screenshot
tab: preview
viewport: 1600x900
click: selector:#run
click: selector:#program-picker .picker-button
click: selector:#program-picker .picker-filter
type: 02-loop.asm
click: selector:#program-picker .picker-option
sleep: 1
click: selector:#cycle-mode
click: selector:#step-next
click: selector:#step-next
click: selector:#step-next
click: selector:#step-next
click: selector:#step-next
-->
<img class='full' src='cpu-cycles.webp' alt='MiniCPU im Taktmodus mit Assemblerquelle, Registern und einzelnen Buszyklen.'>

**Unterrichtsimpuls:** Vorhersagen, was der Programmzähler nach `JNZ`
macht, wann das Zero-Flag gesetzt wird und warum ein Speicherzugriff
mehr Takte braucht als eine Registeroperation.

## 4. Von Assembler zu C — und zur 3D-Grafik

Auf MiniCPU 8S und 8X können Programme auch in einer kleinen C-Variante
entstehen. Die Register- und RAM-Anzeige bleibt dieselbe; der Reiter
**Assembly** zeigt den von C erzeugten Assemblercode. Schülerinnen und Schüler
können Aufgaben zweimal lösen und den Preis der höheren Abstraktion in
**Bytes und Takten** vergleichen.

<!-- tutorial-screenshot
tab: preview
viewport: 1600x900
click: selector:#program-picker .picker-button
click: selector:#program-picker .picker-filter
type: 18-cube.c
click: selector:#program-picker .picker-option
sleep: 2
click: selector:#clock
press: End
click: selector:#run
sleep: 8
-->
<img class='full' src='cpu-cube.webp' alt='MiniCPU 8X rendert einen rotierenden dreidimensionalen Würfel aus einem C-Programm.'>

Neben den Beispielen bietet die Anwendung **Aufgaben mit Testfällen**.
**Check** prüft Ergebnisse automatisch; ein Testfall lässt sich mit
**Try it** laden und schrittweise verfolgen. **Class progress** zeigt
bearbeitete Aufgaben und vergleicht korrekte Lösungen nach Programmgröße und
Zyklenzahl. Die Klasse arbeitet im eigenen Unterrepository `programs/`.

## Vorschlag für eine Unterrichtsreihe

1. `01-add` und `02-loop`: Register, Maschinenbytes, Flags, Schleifen.
2. `04-memory` und `06-rectangle`: Speicher, Adressen, Bildschirmausgabe.
3. `10-subroutines` und `11-hanoi`: Stack, Aufrufe und Rekursion.
4. `17-first-c.c`: C mit dem erzeugten Assembler vergleichen.
5. Mit `18-cube.c` oder `25-raycaster.c` grafische Projekte untersuchen.

[Leitfaden mit passenden Aufgaben und Fragen](https://github.com/specht/cpu-sandbox/blob/730060800243aaa5fa1622b6b94567c9857fa1a0/docs/teaching.md) ·
[Einrichtung](https://github.com/specht/cpu-sandbox/blob/730060800243aaa5fa1622b6b94567c9857fa1a0/README.md).
