Diese Seite begleitet die Data Structures Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in Klasse 11.

<table class='table'>
<tr><th>Klassen-Repository</th><td>steht im Meta-Block als <code>class_repo</code> und gehört jedes Schuljahr angepasst, wie in der Anleitung der Sandbox</td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>structures</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

**Aufgabe 5** ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen Aufgabe 5. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

**Im Unterricht.** Es gibt kein fertiges `add()` und keinen doppelten Boden: Die Darstellung zeigt den Speicher, den die Lernenden selbst verwalten. Die Kästen »Dieselben Aufrufe, zwei Welten«, »Erst skizzieren, dann rotieren« und »Kollisionen provozieren« auf der Seite eignen sich als Impulse für die ganze Klasse. **Class progress** in der App zeigt eine Tabelle aller Lernenden und Strukturen mit ihrem Teststand.

Eine mögliche Unterrichtsreihe:

1. **Indizierter Speicher:** unsortierte Array-Liste, danach die unsortierte verkettete Liste mit denselben Aufrufen. Einfügen, Entfernen, Zugriff und ihre Kosten.
2. **Abstrakte Datentypen:** Stack und Queue als Verhalten formulieren und je zwei Implementierungen schreiben und testen; der Ringpuffer als Höhepunkt.
3. **Sortierte Listen:** Was ändert sich am Einfügen, was an der Suche?
4. **Bäume:** Suchbaum, Einfluss der Einfügereihenfolge auf die Höhe, AVL-Rotationen (die vier Fälle LL, RR, LR, RL) als Erweiterung.
5. **Heap und Hashing:** Prioritätswarteschlange und Hash-Tabelle als weitere Antworten auf die Frage, wie man schnell findet, was man sucht.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Klassen und Objekte, Referenzen, Rekursion für die Bäume</td></tr>
<tr><th>Sprachen</th><td>Dart, Python, JavaScript oder Java; Darstellung und Tests sind in allen Sprachen gleich. Für Python, JavaScript und Java braucht der Rechner den jeweiligen Interpreter bzw. Compiler.</td></tr>
<tr><th>Stand der Bilder</th><td><a href='https://github.com/specht/data-structures-sandbox/commit/e4ea4fc5e126574ed27e671d20e66d43462909c0'><code>e4ea4fc</code></a> (12.10.2026); <a href='https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/README.md'>README</a>, <a href='https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/docs/list-exercises.md'>Unterrichtsreihe zu den Listen</a></td></tr>
</table>
