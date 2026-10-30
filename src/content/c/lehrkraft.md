Lernende schreiben ihr erstes C-Programm im Workspace: »Hello, World!« in `hello.c`, übersetzt mit `gcc`, lesen eine Fehlermeldung, die nicht vom Compiler, sondern vom Linker kommt, und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.c</code> <code>#include &lt;stdio.h&gt;</code> und <code>printf("Hello, World!")</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Wer gibt auf?« fragt nach dem Programm hinter <code>collect2: error:</code>: <code>ld</code>, der Linker. Ein Anlass zu zeigen, dass <code>gcc</code> mehrere Schritte nacheinander ausführt.</li>
<li>»Eine zu große Zahl«: 3000000000 passt nicht in ein <code>int</code>, <code>factor</code> nennt −1294967296. Der Überlauf geschieht still.</li>
<li>»Absteigend sortieren« übersetzt <code>bubblesort.c</code> im Workspace, als Gast, der nichts verändern kann, und ruft <code>bubbleSort</code> mit kleinen Feldern auf; das Programm darum herum läuft dabei nicht.</li>
</ul></td></tr>
</table>
