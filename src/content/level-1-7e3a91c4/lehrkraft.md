Lernende bauen ihr Level aus Level 0 weiter, nach oben und nach unten: mit Rampen, Treppen, einem Dachboden und einer Tür hoch über einem Schacht. Im »Maschinenraum« sehen sie, wie das Programm Kollisionen erkennt. Das ist Level 1 einer Reihe aus vier Seiten.

<table class='table'>
<tr><th>Versteckt</th><td>Wie <a href='/level-0-c1d8a2f7'>Level 0</a>: in keinem Katalog. Hierher führt nur der Link am Ende von Level 0.</td></tr>
<tr><th>Voraussetzung</th><td>Der Klon der Vorlage mit den Räumen aus Level 0 (Kammer, Saal mit seinen Türen, Salon). Wer Level 0 nicht gemacht hat, kann die Räume von dort abschreiben; schneller ist es, Level 0 bis »Ein Salon« durchzugehen.</td></tr>
<tr><th>Ablauf</th><td>Eine Doppelstunde:<ul>
<li>Rampen: ein Gang, dessen Boden ansteigt, Böden und Decken, die gegeneinander kippen, und der krumme Gang</li>
<li>das Treppenhaus, in dem eine Treppe über einen Absatz um die Ecke zu einer hohen Tür führt, und der Dachboden über anderen Räumen</li>
<li>das Loch im Boden, die Tür ins Leere über einem Schacht</li>
<li>zum Erkunden am Schluss der Maschinenraum: Mit der Taste <kbd>K</kbd> sieht man die Kollision, einen Kreis um die Füße, acht Punkte, an denen nach einem Boden gesucht wird, und Kästen für Säulen, Modelle und Geländer.</li>
</ul></td></tr>
<tr><th>Aufgaben</th><td>Eine Frage zur Dicke des Bodens zwischen zwei Stockwerken (<code>slab</code>).</td></tr>
<tr><th>Stolpersteine</th><td>Räume übereinander gehen, solange der obere Boden nicht unter der Decke des unteren liegt; ein Loch verlangt, dass die Decke darunter genau <code>slab</code> unter dem Boden darüber liegt. Eine Treppe braucht ein Rechteck aus Feldern und davor Platz zum Antreten; liegt ihr unteres Ende an einer Wand, kommt man nicht hinauf. In einem Raum mit <code>ramp</code> gibt es keine Treppen.</td></tr>
</table>
