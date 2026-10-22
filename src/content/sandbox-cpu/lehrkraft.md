Lernende programmieren in der CPU Sandbox einen kleinen 8-Bit-Computer, erst in Assembler, später in C, und sehen dabei Register, Flags, Speicher und Bildschirm. Die Sandbox ist ein eigenes Programm im Browser; die Seite hier zeigt, wie sie ihre Programme im Workspace mit Git ablegen und veröffentlichen.

Diese Seite begleitet die CPU Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in einem Kurs.

<table class='table'>
<tr><th>Klassen-Repository</th><td>steht im Meta-Block als <code>class_repo</code> und gehört jedes Schuljahr angepasst, wie in der Anleitung der Sandbox</td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>programs</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

**Aufgabe 6** (»Committet und gepusht«) ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen Aufgabe 6. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

**Im Unterricht.** Eine Stunde mit einem der Beispielprogramme zu beginnen, wirkt Wunder: Erst sehen, was die Maschine kann, dann verstehen, wie. Der Kasten »Takte vorhersagen« eignet sich als Impuls für die ganze Klasse, die Tabelle Assembler gegen C als Gesprächsanlass über den Preis der Abstraktion. **Class progress** in der App zeigt alle Lösungen, nach Bytes und Takten verglichen.

Die Beispiele bilden einen durchgehenden Weg in sieben Kapiteln; jedes endet mit den Aufgaben, die es vorbereitet.

1. **Befehle und Schleifen** (`01-add`, `02-loop`, `03-division`): Register, Maschinenbytes, Flags, Schleifeninvarianten.
2. **Speicher und Bildschirm** (`04-memory` bis `07-pixel-formats`): Adressen, ein Pixel als Byte, selbstveränderlicher Code; danach die ersten Aufgaben.
3. **Geräte** (`08-input`, `09-fire`): Tastatur, Timer, Zufall, und warum ein Spiel bei jedem Prozessortakt gleich schnell laufen kann.
4. **Stack** (`10-subroutines`, `11-hanoi`, `12-scroller`): Unterprogramme, Rücksprungadressen, Rekursion mit den Türmen von Hanoi.
5. **Zeiger** (`13-pointers` bis `16-breakout`): großer Speicher, Festkommazahlen, ein vollständiges Spiel.
6. **C** (`17-first-c.c`): dieselben Aufgaben noch einmal, der Preis der Abstraktion.
7. **Projekte** (`18-cube.c` bis `26-landscape.c`): untersuchen, verändern, eigene Ideen umsetzen, vom Pong-Klon bis zur eigenen Landschaft.

<table class='table'>
<tr><th>Voraussetzungen</th><td>keine für den Einstieg; Binär- und Hexadezimalzahlen helfen, werden aber auch nebenbei gelernt</td></tr>
<tr><th>Sprachen</th><td>Assembler für die MiniCPU, später MiniC (eine kleine Variante von C)</td></tr>
<tr><th>Stand der Bilder</th><td><a href='https://github.com/specht/cpu-sandbox/commit/976c731d03cf0331898befab7bffd7afb94c81d4'><code>976c731</code></a> (14.10.2026); <a href='https://github.com/specht/cpu-sandbox/blob/976c731d03cf0331898befab7bffd7afb94c81d4/docs/teaching.md'>Leitfaden mit Fragen zu jedem Beispiel</a>, <a href='https://github.com/specht/cpu-sandbox/blob/976c731d03cf0331898befab7bffd7afb94c81d4/README.md'>README</a></td></tr>
</table>
