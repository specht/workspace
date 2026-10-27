Lernende bauen als Klasse ein Quiz in einem gemeinsamen Git-Repository: Jede und jeder schreibt eine Runde Fragen und trägt sie in eine gemeinsame Liste ein. Dabei erleben sie einen abgelehnten Push und einen echten Merge-Konflikt, verteilen danach Aufgaben am Programm über eine Liste im Repository, erledigen eine davon in einem eigenen Branch und lesen einen Commit von jemand anderem. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die Grundlagen von Git aus <a href='/git'>Versionsverwaltung mit Git</a>: Repository, Commit, Branch. Programmieren nur für die Aufgaben aus <code>TODO.md</code>: kleine Änderungen an einem fertigen Programm.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein. Im Repository liegt das Quiz in beiden Sprachen, <code>quiz.rb</code> und <code>quiz.py</code>; die Runden sind für beide dieselben.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> einrichten, die eigene Runde, abgeben, der abgelehnte Push, der Konflikt in <code>rounds.txt</code>.</li>
<li><b>2. Doppelstunde:</b> Aufgaben aus <code>TODO.md</code> verteilen, in Branches erledigen und zusammenführen, ein Review, die Regeln fürs Team.</li>
</ul></td></tr>
<tr><th>Das Repository</th><td><ul>
<li><b>Im Workspace</b> (wenn die Installation den Git-Server des Workspace eingeschaltet hat): Auf der Klassenseite im Reiter »Einstellungen« unter »Git-Repositorys« neben »Im Team mit Git: das Klassenquiz« auf »Im Workspace anlegen, mit den Dateien der Seite«. Das Repository enthält dann schon das Quiz. Dort wählst du auch, ob die Lernenden selbst einen Schlüssel erzeugen und abgeben oder ob der Workspace ihn für alle einrichtet. Unter »Ansehen« siehst du Dateien, Verlauf und jeden Commit im Browser; die Lernenden der Gruppe auch.</li>
<li><b>Auf einem Git-Server im Netz:</b> Lege ein leeres Repository an, lade die Lernenden ein und fülle es einmal mit dem Quiz, etwa im eigenen Workspace: klonen, dann <code>mkdir questions</code> und für jede der Dateien <code>quiz.rb</code>, <code>quiz.py</code>, <code>rounds.txt</code>, <code>TODO.md</code>, <code>README.md</code> und <code>questions/beispiel.txt</code> ein <code>wget -O DATEI ${web_root}/class-quiz/starter/DATEI</code>, dann <code>git add -A</code>, <code>git commit</code>, <code>git push</code>. Trag die Adresse auf der Klassenseite bei »Git« ein.</li>
<li><b>Ohne Repository</b> spielen die Lernenden das Team allein nach: ein Repository in einem Ordner als »Server«, ihr eigener Klon und der einer ausgedachten Mitschülerin, Ida. Den Konflikt erleben sie so auch, die Arbeit verteilen nur mit Ida.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>»Deine Runde« ruft das Prüfprogramm des Quiz selbst auf (<code>problems_in</code> aus <code>quiz.rb</code> oder <code>quiz.py</code>) und zählt die Runden: Es findet Fragen ohne Antwort, Dateien, die nicht in <code>rounds.txt</code> stehen, und übrig gebliebene Konfliktmarkierungen.</li>
<li>Drei Git-Aufgaben lesen den Stand des Repositorys im Workspace: ein Remote, alles committet und gepusht, zuletzt mindestens drei eigene Commits (erkannt an der E-Mail-Adresse aus <code>git config</code>).</li>
<li>Dazu zwei Vorhersagen, eine Frage zu <code>--force</code> und das Review als offene Aufgabe, mit einem Beispiel, das nur Lehrkräfte sehen.</li>
</ul></td></tr>
</table>

#### Wie der Konflikt entsteht

Alle hängen ihre Runde ans Ende von <code>rounds.txt</code> an. Wer als zweites pusht, wird abgelehnt; <code>git pull</code> führt dann zusammen, und weil beide an derselben Stelle eine Zeile angefügt haben, meldet Git einen Konflikt. In einer Klasse passiert das fast allen, nur der oder die Erste kommt ohne durch. Gelöst ist er, wenn beide Zeilen bleiben und die drei Markierungszeilen weg sind; <code>--check</code> zeigt übrig gebliebene Markierungen an. Die neuen Dateien in <code>questions/</code> machen keinen Konflikt, auch das ist eine gute Frage an die Klasse.

#### Was schiefgeht

- **<code>fatal: Need to specify how to reconcile divergent branches</code>** bei <code>git pull</code>: Es fehlt <code>git config --global pull.rebase false</code> aus dem Einrichten.
- **Ein Editor öffnet sich** bei <code>git pull</code> oder <code>git merge</code> ohne <code>--no-edit</code>: Im Workspace ist es <code>vi</code>. <kbd>Esc</kbd>, dann <code>:wq</code> und Enter speichert die vorgeschlagene Nachricht und schließt ihn.
- **Der Push wird immer wieder abgelehnt:** Zwischen <code>git pull</code> und <code>git push</code> war jemand schneller; noch einmal holen, dann pushen. In einer großen Klasse hilft es, in zwei Gruppen nacheinander abzugeben.
- **<code>git push --force</code>** lehnt der Git-Server des Workspace ab (<code>non-fast-forward</code>); ein Git-Server im Netz nimmt ihn je nach Einstellung an, dann sind fremde Commits weg.
- **Jemand hat die Markierungen mitcommittet:** <code>--check</code> meldet sie, die Prüfung »Deine Runde« auch; die Zeilen löschen, committen, pushen.

#### Weiter

Die Aufgaben in <code>TODO.md</code> lassen sich erweitern; jede, die <code>quiz.rb</code> oder <code>quiz.py</code> an derselben Stelle ändert wie eine andere, gibt einen Konflikt im Code, den die beiden Beteiligten gemeinsam lösen. Wer mehr will: Pull Requests auf einem Git-Server im Netz, oder Tests, die vor jedem Merge laufen (<a href='/kata-tests'>Katas: Testen</a>).
