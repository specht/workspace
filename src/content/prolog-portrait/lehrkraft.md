Lernende schreiben ihr erstes Prolog-Programm im Workspace: »Hello, World!« in `prolog/hello.pl`, ausgeführt mit SWI-Prolog (`swipl`), lesen eine Fehlermeldung, sehen, dass Prolog mit `=` nicht rechnet, und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; wer weitermachen will, findet in [»Logisch programmieren mit Prolog«](/prolog) eine ganze Aktivität.

<table class='table'>
<tr><th>Prüfung</th><td>Vier Aufgaben:<ul>
<li>»Hello, World!« liest <code>/workspace/prolog/hello.pl</code>: ob dort <code>writeln('Hello, World!')</code> steht.</li>
<li>»Was es nicht gibt« verlangt, was SWI-Prolog hinter <code>Unknown procedure:</code> meldet: <code>writln/1</code>, der Name mit der Zahl der Argumente.</li>
<li>»Eins plus zwei« ist eine Vorhersage: <code>X = 1 + 2</code> ergibt <code>X = 1+2</code>, denn <code>=</code> vergleicht Muster und rechnet nicht; das tut erst <code>is</code>. Das überrascht fast alle und ist der Kern dessen, was Prolog anders macht.</li>
<li>»Die größte zuerst« ruft <code>bubble_sort</code> aus <code>prolog/bubblesort.pl</code> im Workspace auf, als Gast, der nichts verändern kann. Wer mit <code>=&lt;</code> vergleicht, vertauscht gleiche Zahlen immer wieder; die Prüfung sagt dann, dass <code>bubble_sort</code> nicht aufhört.</li>
</ul></td></tr>
</table>
