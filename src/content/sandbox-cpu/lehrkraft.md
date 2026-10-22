Lernende programmieren in der CPU Sandbox einen kleinen 8-Bit-Computer, erst in Assembler, später in C, und sehen dabei Register, Flags, Speicher und Bildschirm. Die Sandbox ist ein eigenes Programm im Browser; die Seite hier zeigt, wie sie die Sandbox im Workspace einrichten und, wenn ihre Gruppe ein Klassen-Repository hat, ihre Programme mit Git veröffentlichen, und fragt dazwischen ab, ob sie verstanden haben, was sie in der Sandbox sehen.

Diese Seite begleitet die CPU Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in einem Kurs.

<table class='table'>
<tr><th>Klassen-Repository</th><td>trägst du je Gruppe auf der <a href='/klasse'>Klassenseite</a> ein: der Knopf »Git« neben »Sprache«, ein Feld je Sandbox (<code>git@server:pfad.git</code>, <code>ssh://server/pfad.git</code> oder <code>https://server/pfad.git</code>). Über dieser Seite steht für dich, welche Adresse deine Gruppen haben, mit einem Link zum Eintragen. Steht jemand in mehreren Gruppen, gilt die erste mit einer Adresse</td></tr>
<tr><th>Git-Server</th><td>Die Lernenden müssen das Repository erreichen können: Lade sie auf den Git-Server ein und gib ihnen Schreibrechte im Klassen-Repository, bevor die Klasse mit dem Einrichten beginnt. Der Workspace legt dort keine Konten an. Die Seite nennt den Server aus der Adresse und sagt: »Deine Lehrkraft lädt dich dorthin ein«</td></tr>
<tr><th>SSH oder HTTPS</th><td>Beginnt die Adresse mit <code>git@</code> oder <code>ssh://</code>, richten die Lernenden einen SSH-Schlüssel ein (Aufgabe »Dein Schlüssel«). Beginnt sie mit <code>https://</code>, fällt der SSH-Teil samt Aufgabe weg; stattdessen erzeugen sie auf dem Server ein Zugangstoken, das in Repositories schreiben darf, und geben es bei der ersten Frage von Git als Passwort ein. Du siehst die Seite in der Fassung mit SSH</td></tr>
<tr><th>Ohne Klassen-Repository</th><td>Lernende einer Gruppe ohne Adresse sehen die Seite ohne Git: Sie klonen nur die App, legen den Ordner <code>programs</code> selbst an und arbeiten allein in ihrem Workspace. Die 26 Beispiele und die 16 Aufgaben der App gehen trotzdem; »Class progress« zeigt nur den eigenen Ordner. Die Aufgaben zu SSH-Schlüssel, Git und Veröffentlichen fallen für sie weg, auch in der Klassenübersicht dieser Gruppe und auf ihrer Startseite; die Fragen zum Thema bleiben</td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>programs</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Fragen zum Thema</th><td>»Was steht in B?« (eine Schleife, die verdoppelt: 16), »Zu groß für ein Byte« (200 + 100: A = 44, CF = 1) und die Vermutung »Wie viele Takte?« (die Schleife von der Seite: 41 Takte, in der Statuszeile der Sandbox nachzuprüfen). Sie lesen nichts im Workspace; bei der Vermutung zählt fürs Klassenboard der erste Versuch</td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

**»Committet und gepusht«**, die letzte Aufgabe der Seite mit Klassen-Repository, ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen »Committet und gepusht«. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

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
