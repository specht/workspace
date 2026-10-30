Lernende schreiben ihr erstes Dart-Programm im Workspace: »Hello, World!« in `hello.dart`, gestartet mit `dart run`, lesen eine Fehlermeldung, sehen, wie Dart mit `null` umgeht, und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine; Dart ist die Sprache von Flutter und wird auf vielen Seiten des Workspace angeboten.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>»Hello, World!« liest <code>/workspace/hello.dart</code>: ob dort <code>void main(</code> und <code>print('Hello, World!')</code> stehen.</li>
<li>»Was Dart nicht findet« verlangt das Wort in <code>Method not found: 'prin'</code>: <code>Method</code>.</li>
<li>»Eine zu große Zahl« verlangt die Exception, mit der <code>factor.dart</code> bei 9223372036854775808 abbricht, eins über dem größten <code>int</code>: <code>FormatException</code>.</li>
<li>»Ohne Ausrufezeichen« ist eine Vorhersage: Ohne das <code>!</code> übersetzt Dart das Programm nicht, weil <code>readLineSync()</code> <code>null</code> liefern könnte. Ein guter Anlass, über Null-Sicherheit zu sprechen.</li>
<li>»Absteigend sortieren« ruft <code>bubbleSort</code> aus <code>bubblesort.dart</code> im Workspace auf, als Gast, der nichts verändern kann, mit kleinen Listen; <code>main</code> läuft dabei nicht.</li>
</ul></td></tr>
</table>
