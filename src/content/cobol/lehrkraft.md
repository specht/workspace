Lernende schreiben ihr erstes COBOL-Programm im Workspace: »Hello, World!« in `hello.cbl` mit seinen Divisions, übersetzt mit `cobc`, finden einen Fehler über die Zeilennummer in der Meldung und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; die Seite zeigt, wie anders eine Sprache für Geschäftsdaten von 1959 aussieht.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>ein Programm in COBOL mit seinen Divisions schreiben, mit <code>cobc</code> übersetzen und starten,</li>
<li>einen Fehler über die Zeilennummer in der Meldung finden,</li>
<li>vorhersagen, was eine Beschreibung wie <code>PIC 99</code> mit einer Zahl aus drei Ziffern macht,</li>
<li>Bubblesort in COBOL lesen, mit einer Sprache vergleichen, die sie kennen, und so ändern, dass es absteigend sortiert.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.cbl</code> eine <code>PROCEDURE DIVISION</code> und <code>DISPLAY "Hello, World!"</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Wo ist der Fehler?« fragt nach der Zeile, die <code>cobc</code> nennt (5).</li>
<li>»Eine Zahl mit drei Ziffern« ist eine Vorhersage zu <code>PIC 99</code>: Aus 123 wird <code>23</code>, COBOL schneidet vorn ab, ohne Fehlermeldung. Ein guter Anlass, über feste Feldbreiten zu sprechen, wie bei Beträgen in Banken.</li>
<li>»Die größte zuerst« liest <code>bubblesort.cbl</code>: ob der Vergleich in <code>BUBBLE-SORT</code> umgedreht ist.</li>
</ul></td></tr>
</table>
