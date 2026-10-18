Lernende schreiben in der Sorting Sandbox Sortieralgorithmen (in Dart, Python, JavaScript oder Java), sehen ihrem eigenen Code beim Sortieren zu, lassen Verfahren gegeneinander antreten und messen ihren Aufwand. Die Sandbox ist ein eigenes Programm im Browser; die Seite hier zeigt, wie sie ihre Programme im Workspace mit Git ablegen und veröffentlichen.

Diese Seite begleitet die Sorting Sandbox, ersetzt sie aber nicht. Sie ist für alle gedacht, die Git noch nicht kennen, zum Beispiel neue Lernende in einem Kurs.

<table class='table'>
<tr><th>Klassen-Repository</th><td>steht im Meta-Block als <code>class_repo</code> und gehört jedes Schuljahr angepasst, wie in der Anleitung der Sandbox</td></tr>
<tr><th>Was gelesen wird</th><td><code>~/.ssh/id_ed25519.pub</code> (nur ob es ihn gibt), der Ordner <code>algorithms</code> und <code>git status</code> darin, im laufenden Workspace der Person</td></tr>
<tr><th>Was nie passiert</th><td>Der Workspace committet, pusht oder zieht nichts. Der private Schlüssel wird nicht geöffnet.</td></tr>
</table>

**Aufgabe 5** ist die wichtige. Sie meldet, was fehlt, mit den echten Dateinamen und den Befehlen dafür: nicht committete Dateien, kein Push, ein Branch ohne Verbindung zum Server, oder dass Git den Namen der Person noch nicht kennt. »Gepusht« heißt: so weit das Repository es weiß; die Prüfung holt nichts vom Server.

Am Ende jeder Stunde lohnt eine Minute: alle prüfen Aufgabe 5. Wer dabei hängen bleibt, ist genau die Person, die sonst nach zwei Wochen ohne einen einzigen Push auffällt.

**Im Unterricht.** Sortieren ist oft schnell »erledigt«: Bubble Sort an der Tafel, ein Arbeitsblatt, fertig. Die Sandbox macht daraus ein kleines Forschungsfeld. Weil die Zahlen für die Algorithmen unsichtbar sind, ist jedes Verfahren vergleichsbasiert: genau das Modell, in dem man die untere Schranke von *n* log *n* Vergleichen beweist. Die Kästen »Erst vorhersagen«, »Wetten abschließen« und »Eine Wachstumsvermutung belegen« auf der Seite eignen sich als Impulse für die ganze Klasse.

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
