Lernende schreiben ihr erstes JavaScript-Programm außerhalb des Browsers: »Hello, World!« in `hello.js`, ausgeführt mit Node.js, lesen eine Fehlermeldung, machen das Skript mit Shebang und `chmod +x` ausführbar und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>ein Programm in JavaScript außerhalb des Browsers mit Node.js ausführen,</li>
<li>eine Fehlermeldung lesen und an ihrer Art erkennen, was falsch ist,</li>
<li>ein Skript mit Shebang und <code>chmod +x</code> ausführbar machen,</li>
<li>vorhersagen, was JavaScript mit einer sehr großen ganzen Zahl macht, und Bubblesort so ändern, dass es absteigend sortiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace/hello.js</code>: ob dort <code>console.log("Hello, World!")</code> steht und, nach dem Shebang, ob die Datei ausführbar ist und mit <code>#!…node</code> anfängt.</li>
<li>»Die Art des Fehlers« verlangt die Art, die Node.js für <code>console.print</code> meldet: <code>TypeError</code>, denn <code>console.print</code> gibt es, aber es ist keine Funktion.</li>
<li>»Eine große Zahl« ist eine Vorhersage: <code>123456789123456789</code> wird zu <code>123456789123456780</code>, denn JavaScript speichert jede Zahl als Kommazahl mit begrenzter Genauigkeit. Ab 2<sup>53</sup> gehen hinten Stellen verloren; ein guter Anlass, über <code>BigInt</code> zu sprechen.</li>
<li>»Die größte zuerst« liest <code>bubblesort.js</code>: ob der Vergleich umgedreht ist.</li>
</ul></td></tr>
</table>
