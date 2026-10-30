Lernende schreiben ihr erstes Rust-Programm im Workspace: »Hello, world!« in `hello.rs`, übersetzt mit `rustc`, lesen eine Fehlermeldung, erleben, dass eine Variable ohne `mut` sich nicht ändern darf, und probieren eine Primfaktorzerlegung und Bubblesort aus. Vorkenntnisse brauchen sie keine.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.rs</code> <code>fn main()</code> und <code>println!("Hello, world!")</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Wofür rustc printn hält« verlangt das Wort in <code>cannot find … printn</code>: <code>macro</code>, wegen des Ausrufezeichens.</li>
<li>»Ohne mut« ist eine Vorhersage: Ohne <code>mut</code> übersetzt <code>rustc</code> das Programm nicht, weil die Schleife in <code>arr</code> schreibt. In Rust ist jede Variable unveränderlich, bis man es anders sagt.</li>
<li>»Absteigend sortieren« liest <code>bubblesort.rs</code>: ob der Vergleich in <code>bubble_sort</code> umgedreht ist und das <code>mut</code> wieder dasteht.</li>
</ul></td></tr>
</table>
