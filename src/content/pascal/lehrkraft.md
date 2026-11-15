Lernende schreiben ihr erstes Pascal-Programm im Workspace: »Hello, World!« in `hello.pas`, übersetzt mit Free Pascal (`fpc`), lesen eine Fehlermeldung des Compilers, probieren eine Primfaktorzerlegung und Bubblesort aus und sehen zum Schluss eine Entwicklungsumgebung im Stil der 90er-Jahre. Vorkenntnisse brauchen sie keine.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>ein Programm in Pascal schreiben, mit <code>fpc</code> übersetzen und starten,</li>
<li>eine Fehlermeldung des Compilers lesen und den Fehler beheben,</li>
<li>vorhersagen, was mit einer Zahl passiert, die größer ist als der Typ <code>integer</code> erlaubt,</li>
<li>Bubblesort in Pascal lesen, mit einer Sprache vergleichen, die sie kennen, und so ändern, dass es absteigend sortiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.pas</code> mit <code>program …;</code> beginnt und <code>writeln('Hello, World!')</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Die Fehlermeldung« verlangt den Text, den <code>fpc</code> für <code>WritLn</code> meldet (<code>Identifier not found</code>).</li>
<li>»Hunderttausend« ist eine Vorhersage: <code>integer</code> hat in Free Pascal 16 Bit, aus 100000 wird −31072, und <code>factor</code> findet keine Faktoren. Der Überlauf geschieht still, ohne Fehlermeldung; ein guter Vergleich mit Fortran, das abbricht.</li>
<li>»Die größte zuerst« liest <code>bubblesort.pas</code>: ob der Vergleich umgedreht ist.</li>
</ul></td></tr>
</table>
