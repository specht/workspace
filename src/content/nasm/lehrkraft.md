Lernende schreiben ihr erstes Assembler-Programm für x86-64 im Workspace: »Hello, World!« in `hello.asm` mit einem Systemaufruf, übersetzt mit `nasm` und gelinkt mit `ld`, lesen eine Fehlermeldung und ändern den Text, den das Programm ausgibt. Vorkenntnisse brauchen sie keine, aber wer schon in C geschrieben hat, sieht, was dort alles verborgen bleibt.

<table class='table'>
<tr><th>Prüfung</th><td>Fünf Aufgaben:<ul>
<li>Zwei lesen <code>/workspace</code>: ob <code>hello.asm</code> den Text <code>hello db 'Hello, World!'</code> und ein <code>syscall</code> enthält, und ob daneben das übersetzte, ausführbare <code>hello</code> liegt.</li>
<li>»Die Fehlermeldung« verlangt, was <code>nasm</code> für <code>ray</code> meldet: <code>symbol `ray' not defined</code>.</li>
<li>»Zu kurz« ist eine Vorhersage: Mit <code>mov rdx, 5</code> gibt das Programm nur <code>Hello</code> aus, ohne Zeilenumbruch. <code>rdx</code> sagt, wie viele Bytes ausgegeben werden; niemand zählt sie für einen.</li>
<li>»Ein anderer Text« liest <code>hello.asm</code>: ob dort <code>'Hello, Workspace!', 10</code> steht und <code>rdx</code> die neue Länge hat, 18 Bytes mit dem Zeilenumbruch.</li>
</ul></td></tr>
</table>
