Lernende schreiben ihr erstes Go-Programm im Workspace: »Hello, World!« in `hello.go`, übersetzt mit `go build`, lesen eine Fehlermeldung, erleben, dass Go eine unbenutzte Variable nicht übersetzt, und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.go</code> mit <code>package main</code> beginnt und <code>fmt.Println("Hello, World!")</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Was Go über Printn sagt« verlangt das Wort vor <code>fmt.Printn</code>: <code>undefined</code>.</li>
<li>»Eine Variable, die niemand braucht« ist eine Vorhersage: Mit <code>x := 42</code>, das nie benutzt wird, übersetzt Go das Programm nicht. Ein guter Anlass, über Sprachen zu sprechen, die streng sind, damit Fehler früh auffallen.</li>
<li>»Absteigend sortieren« liest <code>bubblesort.go</code>: ob der Vergleich in <code>bubbleSort</code> umgedreht ist.</li>
</ul></td></tr>
</table>
