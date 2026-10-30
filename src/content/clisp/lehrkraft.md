Lernende schreiben ihr erstes Lisp-Programm im Workspace: »Hello, World!« in `hello.lisp`, ausgeführt mit SBCL (`sbcl --script`), lesen eine Fehlermeldung, rechnen mit Brüchen und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; die Seite zeigt die Schreibweise mit dem Rechenzeichen vorn und Funktionen, die man anderen Funktionen übergibt.

<table class='table'>
<tr><th>Prüfung</th><td>Vier Aufgaben:<ul>
<li>»Hello, World!« liest <code>/workspace/hello.lisp</code>: ob dort <code>(format t "Hello, World!")</code> steht.</li>
<li>»Die Art des Fehlers« verlangt, was SBCL hinter <code>Unhandled</code> meldet: <code>UNDEFINED-FUNCTION</code>.</li>
<li>»Ein Drittel und ein Sechstel« ist eine Vorhersage: <code>(+ 1/3 1/6)</code> ergibt genau <code>1/2</code>, nicht 0.5. Lisp rechnet mit Brüchen, ohne zu runden; ein guter Vergleich mit den Kommazahlen anderer Sprachen.</li>
<li>»Die größte zuerst« liest <code>bubblesort.lisp</code>: Geändert wird nicht der Sortierer, sondern der Aufruf, der ihm <code>#'&gt;</code> als Vergleich mitgibt. Ein Anlass, über Funktionen als Werte zu sprechen.</li>
</ul></td></tr>
</table>
