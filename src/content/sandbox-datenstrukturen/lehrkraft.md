Lernende programmieren in der Data Structures Sandbox Listen, Stacks, Queues, Bäume, Heaps und Hash Tables selbst (in Dart, Python, JavaScript oder Java) und sehen bei jedem Aufruf, was im Speicher passiert. Die Sandbox ist ein eigenes Programm im Browser; die Seite hier zeigt, wie sie die Sandbox im Workspace einrichten und, wenn ihre Gruppe ein Klassen-Repository hat, ihre Implementierungen mit Git veröffentlichen, und fragt dazwischen ab, ob sie verstanden haben, was sie in der Sandbox sehen.

Diese Seite begleitet die Data Structures Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in Klasse 11.

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten (Schlüssel und Name, wenn es ein Klassen-Repository gibt), Vorlage klonen, App starten, ein Datentyp in mehreren Speicherformen.</li>
<li><b>Danach</b> je Doppelstunde ein Schritt der Reihe unter »Im Unterricht«. Am Ende jeder Stunde committen und pushen alle (»Committet und gepusht«).</li>
</ul></td></tr>
<tr><th>Klassen-Repository</th><td>trägst du je Gruppe auf der <a href='/klasse'>Klassenseite</a> ein: der Knopf »Git« neben »Sprache«, ein Feld je Sandbox (<code>git@server:pfad.git</code>, <code>ssh://server/pfad.git</code> oder <code>https://server/pfad.git</code>). Über dieser Seite steht für dich, welche Adresse deine Gruppen haben, mit einem Link zum Eintragen. Steht jemand in mehreren Gruppen, gilt die erste mit einer Adresse</td></tr>
<tr><th>Git-Server</th><td>Die Lernenden müssen das Repository erreichen können: Lade sie auf den Git-Server ein und gib ihnen Schreibrechte im Klassen-Repository, bevor die Klasse mit dem Einrichten beginnt. Der Workspace legt dort keine Konten an. Die Seite nennt den Server aus der Adresse und sagt: »Deine Lehrkraft lädt dich dorthin ein«</td></tr>
<tr><th>SSH oder HTTPS</th><td><ul>
<li>Beginnt die Adresse mit <code>git@</code> oder <code>ssh://</code>, richten die Lernenden einen SSH-Schlüssel ein (Aufgabe »Dein Schlüssel«).</li>
<li>Beginnt sie mit <code>https://</code>, fällt der SSH-Teil samt Aufgabe weg; stattdessen erzeugen sie auf dem Server ein Zugangstoken, das in Repositories schreiben darf, und geben es bei der ersten Frage von Git als Passwort ein.</li>
<li>Du siehst die Seite in der Fassung mit SSH.</li>
</ul></td></tr>
<tr><th>Ohne Klassen-Repository</th><td><ul>
<li>Lernende einer Gruppe ohne Adresse sehen die Seite ohne Git: Sie klonen nur die App, legen den Ordner <code>structures</code> selbst an und arbeiten allein in ihrem Workspace.</li>
<li>Die App zeigt dann nur ihre eigenen Implementierungen, »Class progress« eine Zeile.</li>
<li>Die Aufgaben zu SSH-Schlüssel, Git und Veröffentlichen fallen für sie weg, auch in der Klassenübersicht dieser Gruppe und auf ihrer Startseite; die Fragen zum Thema bleiben.</li>
</ul></td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>structures</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Fragen zum Thema</th><td>»Wer kommt zuletzt heraus?« (Stack und Queue mit derselben Folge), »Platz machen im Array« (wie viele Werte <code>insert(2, 6)</code> verschiebt: 3), die Vermutung »Welche Rotation rettet ihn?« (AVL nach 30, 20, 10) und »Verschenkte Buckets« (Vielfache von 3 bei 12 Buckets: 4). Sie lesen nichts im Workspace; bei der Vermutung zählt fürs Klassenboard der erste Versuch</td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

#### »Committet und gepusht«

, die letzte Aufgabe der Seite mit Klassen-Repository, ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen »Committet und gepusht«. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

#### Im Unterricht

Es gibt kein fertiges `add()` und keinen doppelten Boden: Die Darstellung zeigt den Speicher, den die Lernenden selbst verwalten. Die Kästen »Dieselben Aufrufe, zwei Welten«, »Erst skizzieren, dann rotieren« und »Kollisionen provozieren« auf der Seite eignen sich als Impulse für die ganze Klasse. **Class progress** in der App zeigt eine Tabelle aller Lernenden und Strukturen mit ihrem Teststand.

Eine mögliche Unterrichtsreihe:

1. **Indizierter Speicher:** unsortierte Array-Liste, danach die unsortierte Linked List mit denselben Aufrufen. Einfügen, Entfernen, Zugriff und ihre Kosten.
2. **Abstrakte Datentypen:** Stack und Queue als Verhalten formulieren und je zwei Implementierungen schreiben und testen; der Ring Buffer als Höhepunkt.
3. **Sortierte Listen:** Was ändert sich am Einfügen, was an der Suche?
4. **Bäume:** Suchbaum, Einfluss der Einfügereihenfolge auf die Höhe, AVL-Rotationen (die vier Fälle LL, RR, LR, RL) als Erweiterung.
5. **Heap und Hashing:** Priority Queue und Hash Table als weitere Antworten auf die Frage, wie man schnell findet, was man sucht.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Klassen und Objekte, Referenzen, Rekursion für die Bäume</td></tr>
<tr><th>Sprachen</th><td>Dart, Python, JavaScript oder Java; Darstellung und Tests sind in allen Sprachen gleich. Für Python, JavaScript und Java braucht der Rechner den jeweiligen Interpreter bzw. Compiler.</td></tr>
<tr><th>Stand der Bilder</th><td><a href='https://github.com/specht/data-structures-sandbox/commit/e4ea4fc5e126574ed27e671d20e66d43462909c0'><code>e4ea4fc</code></a> (12.10.2026); <a href='https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/README.md'>README</a>, <a href='https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/docs/list-exercises.md'>Unterrichtsreihe zu den Listen</a></td></tr>
</table>
