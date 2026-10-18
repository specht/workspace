Lernende üben Git im Terminal an zwei ausgedachten Personen: Erst arbeitet Alice allein (Commits, `git add`, `.gitignore`, ein Blick zurück mit `HEAD~2`, ein Branch und ein Merge), dann arbeiten Alice und Bob über ein gemeinsames Remote zusammen, mit Push, Pull, einem Konflikt, den sie auflösen, und Tags. Vorkenntnisse braucht es keine außer dem Terminal aus »Dateien und Verzeichnisse«.

Die Aufgaben folgen dem Weg durch das Tutorial: erst Alice allein, dann Alice und Bob mit einem gemeinsamen Remote im Ordner `git-tutorial/remote-repo`.

<table class='table'>
<tr><th>Was gelesen wird</th><td><code>git status</code> und die Commits in <code>/workspace/git-tutorial/alice</code> und <code>/workspace/git-tutorial/bob/remote-repo</code>, im laufenden Workspace der Person. Die Prüfung committet, pusht und zieht nichts.</td></tr>
<tr><th>Commits zählen</th><td>Gezählt werden Commits mit der E-Mail-Adresse, die Git im Workspace kennt. Fehlt sie, sagt die Karte, wie man sie mit <code>git config --global</code> setzt.</td></tr>
<tr><th>Zeitpunkt</th><td>Die Prüfungen lesen den Stand beim Klick. Wer bei »Zeitreise« noch auf <code>HEAD~2</code> steht, bekommt bei den späteren Aufgaben den Hinweis auf »detached HEAD«.</td></tr>
</table>

**Wo es hakt.** Die Staging Area (Aufgabe »Was macht git add?«) und die Konfliktmarkierungen. Bei »Konfliktmarkierungen« lohnt es, die Antwort zu besprechen, bevor Bob den Konflikt auflöst: Oben steht der eigene Stand (`HEAD`), unten der geholte.

»Feature gemergt« meldet die Datei `hello`, wenn `.gitignore` fehlt oder nicht committet ist. Das ist die Stelle, an der man sieht, wofür `.gitignore` gut ist.
