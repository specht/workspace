Lernende schreiben ihr erstes Lua-Programm im Workspace: »Hello, World!« in `hello.lua`, ausgeführt mit `lua`, machen das Skript mit Shebang und `chmod +x` ausführbar und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; Lua steckt in vielen Spielen, und wer [TIC-80](/tic80) kennt, hat es schon gesehen.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>ein Programm in Lua schreiben und mit <code>lua</code> ausführen,</li>
<li>ein Skript mit Shebang und <code>chmod +x</code> ausführbar machen,</li>
<li>vorhersagen, was Lua für einen Index vor dem ersten Element liefert, und erklären, warum Arrays in Lua bei 1 anfangen,</li>
<li>Bubblesort in Lua lesen, mit einer Sprache vergleichen, die sie kennen, und so ändern, dass es absteigend sortiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Vier Aufgaben:<ul>
<li>Zwei lesen <code>/workspace/hello.lua</code>: ob dort <code>print("Hello, World!")</code> steht und, nach dem Shebang, ob die Datei ausführbar ist und mit <code>#!…lua</code> anfängt.</li>
<li>»Vor der ersten Zahl« ist eine Vorhersage: <code>array[0]</code> ist <code>nil</code>, denn Lua zählt ab 1, und ein Index, unter dem nichts steht, ist kein Fehler. Ein guter Vergleich mit Sprachen, die ab 0 zählen.</li>
<li>»Die größte zuerst« liest <code>bubblesort.lua</code>: ob der Vergleich umgedreht ist.</li>
</ul></td></tr>
</table>
