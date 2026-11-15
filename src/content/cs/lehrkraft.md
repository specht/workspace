Lernende schreiben ihr erstes C#-Programm im Workspace: »Hello, World!« in `HelloWorld.cs`, gestartet mit `dotnet run`, lesen eine Fehlermeldung mit ihrer Nummer und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; wer Java kennt, sieht viel Vertrautes.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>ein Programm in C# schreiben und mit <code>dotnet run</code> starten,</li>
<li>eine Fehlermeldung mit ihrer Nummer lesen und den Fehler beheben,</li>
<li>vorhersagen, wie ein Programm bei einer zu großen Zahl oder einem Index hinter dem Ende eines Arrays abbricht,</li>
<li>Bubblesort in C# lesen, mit Java oder einer anderen Sprache vergleichen, die sie kennen, und so ändern, dass es absteigend sortiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>»Hello, World!« liest <code>/workspace/HelloWorld.cs</code>: ob dort <code>static void Main(</code> und <code>Console.WriteLine("Hello, World!")</code> stehen.</li>
<li>»Die Nummer des Fehlers« verlangt die Nummer, die der Compiler <code>WriteLin</code> gibt: <code>CS0117</code>. Mit ihr findet man die Erklärung in der Dokumentation.</li>
<li>»Eine zu große Zahl« verlangt die Exception, mit der <code>Factor.cs</code> bei 3000000000 abbricht: <code>OverflowException</code>.</li>
<li>»Über das Ende hinaus« ist eine Vorhersage: Mit <code>i &lt;= numbers.Length</code> bricht das Programm mit einer <code>IndexOutOfRangeException</code> ab. C# prüft jeden Index, anders als C.</li>
<li>»Absteigend sortieren« liest <code>BubbleSort.cs</code>: ob der Vergleich in <code>Sort</code> umgedreht ist.</li>
</ul></td></tr>
</table>
