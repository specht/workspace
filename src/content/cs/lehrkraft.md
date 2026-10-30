Lernende schreiben ihr erstes C#-Programm im Workspace: »Hello, World!« in `HelloWorld.cs`, gestartet mit `dotnet run`, lesen eine Fehlermeldung mit ihrer Nummer und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; wer Java kennt, sieht viel Vertrautes.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>»Hello, World!« liest <code>/workspace/HelloWorld.cs</code>: ob dort <code>static void Main(</code> und <code>Console.WriteLine("Hello, World!")</code> stehen.</li>
<li>»Die Nummer des Fehlers« verlangt die Nummer, die der Compiler <code>WriteLin</code> gibt: <code>CS0117</code>. Mit ihr findet man die Erklärung in der Dokumentation.</li>
<li>»Eine zu große Zahl« verlangt die Exception, mit der <code>Factor.cs</code> bei 3000000000 abbricht: <code>OverflowException</code>.</li>
<li>»Über das Ende hinaus« ist eine Vorhersage: Mit <code>i &lt;= numbers.Length</code> bricht das Programm mit einer <code>IndexOutOfRangeException</code> ab. C# prüft jeden Index, anders als C.</li>
<li>»Absteigend sortieren« liest <code>BubbleSort.cs</code>: ob der Vergleich in <code>Sort</code> umgedreht ist.</li>
</ul></td></tr>
</table>
