Lernende bringen zuerst ohne Code die Schritte zweier Threads selbst in eine Reihenfolge und sehen, wo eine Erhöhung verloren geht, wie ein Lock das verhindert und wie zwei Locks in verschiedener Reihenfolge einander blockieren. Danach dasselbe im Code, in Java oder in Dart (»Sprachen der Klasse«).

<table class='table'>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> das Widget, ohne Code: die verlorene Erhöhung, wie viele der 20 Reihenfolgen gut gehen, drei Threads, wie viele Reihenfolgen mit einem Lock bleiben, ein Deadlock mit zwei Locks, die speisenden Philosophen und wie eine feste Reihenfolge der Gabeln sie rettet.</li>
<li><b>2. Doppelstunde:</b> im Code: die Vorhersage »Was kommt heraus?«, der Zähler, der nichts verliert, die Vorhersage »Was passiert?« zum Deadlock, Locks in fester Reihenfolge (Java) oder ein Lock aus <code>Future</code>s (Dart); als Extra <code>AtomicInteger</code> (Java) oder der Besuchszähler in einer Datei (Dart).</li>
<li><b>3. Doppelstunde:</b> Arbeit weitergeben: ein Monitor mit <code>wait</code> und <code>notifyAll</code> und <code>ArrayBlockingQueue</code> (Java) oder Isolates und ein <code>Stream</code> (Dart); dann mehrere Kerne: ein Thread-Pool (Java) oder eine gerechte Verteilung auf Isolates (Dart), jeweils mit einer Messung.</li>
</ul></td></tr>
<tr><th>Java und Dart</th><td>Java hat Threads, die sich Speicher teilen: Der Fehler ist echt und zeigt sich nicht bei jedem Lauf (im Workspace-Image mit vier Kernen: zehn Läufe, neun unter 200000, einer genau 200000). Dart hat keine geteilten Threads; dort entsteht dieselbe verlorene Erhöhung an einem <code>await</code>, und zwar bei jedem Lauf gleich. Isolates sind Darts Weg zu echter Parallelität: Sie teilen nichts und schicken nur Ergebnisse.</td></tr>
<tr><th>Was geprüft wird</th><td>Die Aufgaben mit dem Widget schicken die gewählte Reihenfolge; der Server lässt sie mit denselben Regeln laufen (Prüfart <code>interleaving</code>). Die Code-Aufgaben rufen die Klassen und Funktionen der Lernenden im Workspace auf, mehrmals und mit mehreren Threads; ein Programm, das hängt, scheitert an der Zeitgrenze und bekommt einen Satz, woran es liegen kann.</td></tr>
</table>

#### Wo es hakt

- Dass ein falsches Programm manchmal richtig rechnet. Ein einzelner richtiger Lauf beweist nichts; die Prüfung von »Ein Zähler, der nichts verliert« ruft den Zähler darum mit mehreren Threads und mehrmals auf.
- Beim Deadlock im Code: Wer <kbd>Strg</kbd><kbd>C</kbd> nicht kennt, sitzt vor einem Programm, das nicht endet. Die Seite sagt es im Kasten »Ausprobieren«.
- Beim Monitor: <code>wait()</code> gehört in eine <code>while</code>-Schleife, nicht in ein <code>if</code>; mit einem Erzeuger und einem Verbraucher fällt der Unterschied nicht auf, mit mehreren schon. Ein Thema für ein Unterrichtsgespräch.
- Der Deadlock in Dart hängt nicht: Ein Programm, in dem jede Aktion auf eine andere wartet und sonst nichts geplant ist, endet einfach, ohne Fehlermeldung (im Image ausprobiert). Das überrascht, und die Vorhersage »Was passiert?« zielt darauf.
- Die Zahlen hinter den Widget-Aufgaben sind ausgezählt, mit den Regeln der Prüfung: drei Threads haben 1680 Reihenfolgen, in 1404 davon steht am Ende 1; mit dem Lock bleiben 2; bei den Philosophen sind Deadlocks erreichbar, mit der festen Reihenfolge der Gabeln in keiner der 236 Reihenfolgen.
- In Dart: Vier Isolates sind nicht viermal so schnell, weil die großen Zahlen länger brauchen als die kleinen (im Image etwa 2,4 s mit einem, 1,0 s mit vier). Die Messung im Kasten »Ausprobieren« führt darauf, und die Aufgabe »Mehrere Kerne« verteilt die Zahlen gerechter. Der Thread-Pool in Java misst im Image etwa 2,4 s mit einem und 0,8 s mit vier Threads.
