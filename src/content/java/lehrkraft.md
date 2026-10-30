Lernende schreiben ihr erstes Java-Programm im Workspace: »Hello, World!« in einer Klasse `HelloWorld`, übersetzt mit `javac` und gestartet mit `java`, lesen eine Fehlermeldung des Compilers und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; die Seite zeigt den Weg über den Bytecode, der Java auf jedem Rechner laufen lässt.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>HelloWorld.java</code> die Klasse <code>HelloWorld</code> mit <code>System.out.println("Hello, World!")</code> enthält, und ob daneben <code>HelloWorld.class</code> liegt.</li>
<li>»Was javac nicht findet« verlangt das Symbol hinter <code>symbol:</code>: die Methode <code>printn</code>.</li>
<li>»Eine zu große Zahl« verlangt die Exception, mit der <code>java Factor</code> bei 3000000000 abbricht: <code>InputMismatchException</code>. Anders als in C überläuft nichts still; der <code>Scanner</code> weigert sich, die Zahl als <code>int</code> zu lesen.</li>
<li>»Absteigend sortieren« übersetzt <code>BubbleSort.java</code> im Workspace, als Gast, der nichts verändern kann, und ruft <code>bubbleSort</code> mit kleinen Feldern auf; <code>main</code> läuft dabei nicht.</li>
</ul></td></tr>
</table>
