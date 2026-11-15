Lernende schreiben in der Sorting Sandbox Sortieralgorithmen (in Dart, Python, JavaScript oder Java), sehen ihrem eigenen Code beim Sortieren zu, lassen Verfahren gegeneinander antreten und messen ihren Aufwand. Die Sandbox ist ein eigenes Programm im Browser; die Seite hier zeigt, wie sie die Sandbox im Workspace einrichten und, wenn ihre Gruppe ein Klassen-Repository hat, ihre Algorithmen mit Git veröffentlichen, und fragt dazwischen ab, ob sie verstanden haben, was sie in der Sandbox sehen.

Diese Seite begleitet die Sorting Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in einem Kurs.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>einen Sortieralgorithmus schreiben und Schritt für Schritt an seinen Lese-, Schreib- und Vergleichsoperationen verfolgen, von Bubble Sort über Insertion Sort und Selection Sort bis zu Merge Sort mit einer zweiten Liste,</li>
<li>vorhersagen, welcher Algorithmus auf welcher Eingabe (zufällig, sortiert, umgekehrt, fast sortiert, wenige verschiedene Werte) weniger Arbeit hat, und es im Race prüfen,</li>
<li>den Aufwand für wachsende Listen messen und mit <i>n</i>, <i>n</i> log <i>n</i> und <i>n</i>² vergleichen, um eine Vermutung über sein Wachstum zu belegen,</li>
<li>Korrektheit, Stabilität und Aufwand als drei Gütekriterien unterscheiden und an einem Gegenbeispiel zeigen, warum ein Verfahren nicht stabil ist.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten (Schlüssel und Name, wenn es ein Klassen-Repository gibt), Vorlage klonen, App starten, Explore.</li>
<li><b>Danach</b> je Doppelstunde ein Schritt der Reihe unter »Im Unterricht«. Am Ende jeder Stunde committen und pushen alle (»Committet und gepusht«).</li>
</ul></td></tr>
<tr><th>Ohne Rechner</th><td><ul>
<li><b>Vor Explore, 15 Minuten, zu dritt:</b> acht Spielkarten verdeckt in einer Reihe, oder acht gleiche undurchsichtige Becher mit verschieden viel Sand und eine Balkenwaage. Eine Person sortiert und sieht nie einen Wert: Sie zeigt auf zwei Karten, die zweite Person schaut nach und sagt nur, welche größer ist, die dritte macht für jeden Vergleich einen Strich. Zwei Runden mit zwei Wegen, die sich die Gruppe selbst ausdenkt; zum Schluss vergleichen die Gruppen ihre Striche.</li>
<li><b>Was sichtbar wird:</b> Ohne die Werte zu sehen, bleibt nur Vergleichen und Umlegen, und wie viele Vergleiche es braucht, hängt vom Weg ab. Oft entstehen dabei schon »immer die kleinste suchen« (Selection Sort) und »jede Karte an ihren Platz schieben« (Insertion Sort).</li>
<li><b>Zurück zur Seite:</b> <a href='#worum-es-geht'>»Worum es geht«</a> sagt, dass auch die Algorithmen der Sandbox die Zahlen nicht sehen; die Striche sind die Vergleiche, die <a href='#race-gleicher-auftrag-ungleiche-arbeit'>»Race: gleicher Auftrag, ungleiche Arbeit«</a> unter jeder Bahn zählt. Schritt 2 unter »Im Unterricht« nimmt die Karten wieder auf. Eine schon sortierte Reihe noch nicht legen: Das ist die Wette »Deine erste Wette«.</li>
<li><b>Vorbild:</b> »Sorting Algorithms« bei <a href='https://classic.csunplugged.org/activities/sorting-algorithms/'>Classic CS Unplugged</a>, mit Gewichten und Waage, dort auch auf Deutsch.</li>
</ul></td></tr>
<tr><th>Klassen-Repository</th><td>trägst du je Gruppe auf der <a href='/klasse'>Klassenseite</a> ein: der Knopf »Git« neben »Sprache«, ein Feld je Sandbox (<code>git@server:pfad.git</code>, <code>ssh://server/pfad.git</code> oder <code>https://server/pfad.git</code>). Über dieser Seite steht für dich, welche Adresse deine Gruppen haben, mit einem Link zum Eintragen. Steht jemand in mehreren Gruppen, gilt die erste mit einer Adresse</td></tr>
<tr><th>Git-Server</th><td>Die Lernenden müssen das Repository erreichen können: Lade sie auf den Git-Server ein und gib ihnen Schreibrechte im Klassen-Repository, bevor die Klasse mit dem Einrichten beginnt. Der Workspace legt dort keine Konten an. Die Seite nennt den Server aus der Adresse und sagt: »Deine Lehrkraft lädt dich dorthin ein«</td></tr>
<tr><th>SSH oder HTTPS</th><td><ul>
<li>Beginnt die Adresse mit <code>git@</code> oder <code>ssh://</code>, richten die Lernenden einen SSH-Schlüssel ein (Aufgabe »Dein Schlüssel«).</li>
<li>Beginnt sie mit <code>https://</code>, fällt der SSH-Teil samt Aufgabe weg; stattdessen erzeugen sie auf dem Server ein Zugangstoken, das in Repositories schreiben darf, und geben es bei der ersten Frage von Git als Passwort ein.</li>
<li>Du siehst die Seite in der Fassung mit SSH.</li>
</ul></td></tr>
<tr><th>Ohne Klassen-Repository</th><td><ul>
<li>Lernende einer Gruppe ohne Adresse sehen die Seite ohne Git: Sie klonen nur die App, legen den Ordner <code>algorithms</code> selbst an und arbeiten allein in ihrem Workspace.</li>
<li>Die App zeigt dann nur ihre eigenen Algorithmen; für ein Rennen legen sie einen zweiten an (die Seite sagt, wie).</li>
<li>Die Aufgaben zu SSH-Schlüssel, Git und Veröffentlichen fallen für sie weg, auch in der Klassenübersicht dieser Gruppe und auf ihrer Startseite; die Fragen zum Thema bleiben.</li>
</ul></td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>algorithms</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Fragen zum Thema</th><td><ul>
<li>»Wie oft wird getauscht?« (Vertauschungen von Bubble Sort auf <code>[4, 2, 5, 1, 3]</code>: 6), die Vermutungen »Deine erste Wette« (Bubble Sort gegen Insertion Sort auf einer sortierten Liste) und »Doppelt so viele Elemente« (der Aufwand vervierfacht sich) vor Race und Analyze, »Wer springt vorbei?« (welche Zeile Selection Sort instabil macht).</li>
<li>Sie lesen nichts im Workspace; bei den Vermutungen zählt fürs Klassenboard der erste Versuch.</li>
</ul></td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

#### »Committet und gepusht«

, die letzte Aufgabe der Seite mit Klassen-Repository, ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen »Committet und gepusht«. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

#### Im Unterricht

Sortieren ist oft schnell »erledigt«: Bubble Sort an der Tafel, ein Arbeitsblatt, fertig. Die Sandbox macht daraus ein kleines Forschungsfeld. Weil die Zahlen für die Algorithmen unsichtbar sind, ist jedes Verfahren vergleichsbasiert: genau das Modell, in dem man die untere Schranke von *n* log *n* Vergleichen beweist. Die Kästen »Erst vorhersagen«, »Wetten abschließen« und »Eine Wachstumsvermutung belegen« auf der Seite eignen sich als Impulse für die ganze Klasse.

Eine mögliche Unterrichtsreihe:

1. **Lesen und verbessern:** Bubble Sort schrittweise erklären, dann beide Schleifen so weit wie möglich verkürzen und den Gewinn im Race sichtbar machen.
2. **Selbst entwickeln:** Insertion Sort oder Selection Sort zunächst mit Spielkarten von Hand durchspielen, dann programmieren und mit **Check** prüfen.
3. **Vergleichen:** Im Race Hypothesen für verschiedene Eingabeformen aufstellen und überprüfen. Insertion Sort auf einer sortierten Liste ist dabei jedes Mal ein kleines Erlebnis.
4. **Messen:** Mit **Analyze** Wachstumsvermutungen belegen; Korrektheit, Stabilität und Aufwand als drei verschiedene Gütekriterien festhalten.
5. **Ausbauen:** Merge Sort mit `scratch`, Quick Sort, Heap Sort: Die Klasse sammelt ihre Verfahren, und Analyze wird zur gemeinsamen Rangliste.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Schleifen, Bedingungen, Listen mit Index</td></tr>
<tr><th>Sprachen</th><td>Dart, Python, JavaScript oder Java; jede und jeder wählt selbst, alle treten in derselben App gegeneinander an</td></tr>
<tr><th>Stand der Bilder</th><td><a href='https://github.com/specht/sorting-sandbox/commit/153a572734bbb9be57e09fa75980479532324189'><code>153a572</code></a> (12.10.2026); Einrichtung und Arbeitsweise im <a href='https://github.com/specht/sorting-sandbox/blob/153a572734bbb9be57e09fa75980479532324189/README.md'>README</a></td></tr>
</table>
