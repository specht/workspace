Lernende schreiben ihr erstes C++-Programm im Workspace: »Hello, World!« in `hello.cpp` mit `cout`, übersetzt mit `g++`, lesen in einer Fehlermeldung Zeile und Spalte und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; wer die Seite zu C kennt, sieht, was C++ dazubringt.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.cpp</code> <code>#include &lt;iostream&gt;</code> und <code>cout &lt;&lt; "Hello, World!"</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Wo der Fehler steht« fragt, was die 10 in <code>hello.cpp:4:10:</code> sagt: das Zeichen in Zeile 4, an dem der Fehler beginnt.</li>
<li>»Eine zu große Zahl«: <code>cin</code> kann 3000000000 nicht in ein <code>int</code> lesen und setzt den größten Wert, 2147483647; <code>factor</code> nennt diese Zahl. Anders als in C ist das kein stiller Überlauf, sondern ein gescheitertes Lesen.</li>
<li>»Absteigend sortieren« liest <code>bubblesort.cpp</code>: ob der Vergleich in <code>bubbleSort</code> umgedreht ist.</li>
</ul></td></tr>
</table>
