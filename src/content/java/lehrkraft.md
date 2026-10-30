Lernende schreiben ihr erstes Java-Programm im Workspace: »Hello, World!« in `HelloWorld.java`, übersetzt mit `javac` und gestartet mit `java` oder direkt mit `java HelloWorld.java`, lesen eine Fehlermeldung des Compilers und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine. Die Seite schreibt Java so, wie es seit Java 25 geht: ein Programm ohne Klasse mit `void main()` und `IO.println`; die Form mit `public class` und `public static void main(String[] args)` aus älteren Büchern zeigt sie daneben. Bubblesort steht wieder in einer Klasse, weil andere Programme seine Methode aufrufen sollen; dort lernen die Lernenden `class`, `static` und `import` kennen.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>HelloWorld.java</code> <code>void main(</code> und <code>IO.println("Hello, World!")</code> enthält (die ältere Form mit Klasse und <code>System.out.println</code> gilt auch), und ob daneben <code>HelloWorld.class</code> liegt.</li>
<li>»Was javac nicht findet« verlangt das Symbol hinter <code>symbol:</code>: die Methode <code>printn</code>.</li>
<li>»Eine zu große Zahl« verlangt die Exception, mit der <code>java Factor</code> bei 3000000000 abbricht: <code>NumberFormatException</code>. Anders als in C überläuft nichts still; <code>Integer.parseInt</code> weigert sich, den Text als <code>int</code> zu lesen.</li>
<li>»Absteigend sortieren« übersetzt <code>BubbleSort.java</code> im Workspace, als Gast, der nichts verändern kann, und ruft <code>bubbleSort</code> mit kleinen Feldern auf; <code>main</code> läuft dabei nicht.</li>
</ul></td></tr>
</table>
