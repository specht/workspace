<div class='meta'>
image: data-structures-sandbox.webp
</div>

# Datenstrukturen von innen sehen

<p class='abstract'>
In der Data Structures Sandbox implementieren Lernende Listen, Stacks, Queues, Bäume,
Heaps und Hash-Tabellen selbst – in Dart, Python, JavaScript oder Java – und sehen
bei jedem Methodenaufruf, was in ihrem Speicher passiert: welche Zelle beschrieben
wird, welche Referenz umgebogen wird, welcher Knoten rotiert.
</p>

**Werkzeug:** [Data Structures Sandbox](https://github.com/specht/data-structures-sandbox) ·
**Stand der Bilder:** [`e4ea4fc`](https://github.com/specht/data-structures-sandbox/commit/e4ea4fc5e126574ed27e671d20e66d43462909c0) (12.10.2026)

<table class='table'>
<tr><th>Voraussetzungen</th><td>Klassen und Objekte, Referenzen, Rekursion für die Bäume</td></tr>
<tr><th>Sprachen</th><td>Dart, Python, JavaScript oder Java – Darstellung und Tests sind in allen Sprachen gleich</td></tr>
<tr><th>Die Lernenden …</th><td>implementieren Datenstrukturen mit selbst verwaltetem Speicher und prüfen sie mit Tests</td></tr>
<tr><th>Die Lehrkraft …</th><td>sieht in <strong>Class progress</strong>, wer welche Struktur implementiert hat und welche Tests bestehen</td></tr>
</table>

## Die Idee: Kein `add()`, kein Netz, kein doppelter Boden

Die meisten Programmiersprachen bringen fertige Listen, Mengen und Wörterbücher mit.
Gerade deshalb bleibt oft unklar, was darin eigentlich passiert. In der Sandbox gibt es
diese Abkürzung nicht: Eine Array-Liste besteht aus **acht festen Speicherzellen**,
eine verkettete Liste aus **Knoten mit `next`-Referenz**, und die Größe ist ein Feld
`size`, das die Lernenden selbst pflegen müssen. Die Visualisierung zeigt genau diesen
Speicher – sie räumt nichts auf und rechnet nichts nach.

Jede Struktur gibt es als unfertigen **Starter** und als vollständiges
**Referenzbeispiel**. Die Lernenden schreiben nur die Logik, keinen Zeichencode. Ins
Eingabefeld oben tippen sie Methodenaufrufe wie `insert(30); insert(20)`, drücken
Enter und können den Ablauf dann Schritt für Schritt vor- und zurückspulen.

## Ein abstrakter Datentyp, mehrere Speicherformen

Ein **Stack** arbeitet nach dem LIFO-, eine **Queue** nach dem FIFO-Prinzip – aber
dasselbe Verhalten lässt sich mit einem festen Array, einem Ringpuffer oder
verketteten Knoten erreichen. Besonders ergiebig sind die **vier Listen**: unsortiert
oder sortiert, jeweils als Array oder als verkettete Knoten. Alle vier haben dieselbe
Schnittstelle, und man kann dieselbe Aufruffolge auf alle vier loslassen.

Hier dieselbe Folge von Einfügungen auf einer einfach verketteten Liste, diesmal in
Java implementiert:

<!-- tutorial-screenshot
terminal-open
terminal-run: cd /workspace && git clone -q https://github.com/specht/data-structures-sandbox.git && cd data-structures-sandbox && git checkout -q e4ea4fc5e126574ed27e671d20e66d43462909c0
terminal-wait-for-prompt
terminal-run: cd /workspace/data-structures-sandbox && mkdir -p structures && ./new-structure demo list unsorted-nodes --example --lang java && ./new-structure demo tree avl --example --lang python && ./new-structure demo hash chains --example --lang dart
terminal-wait-for-prompt
terminal-run: cd /workspace/data-structures-sandbox && (nohup ./run --port 18081 --no-open > /tmp/data-structures-sandbox.log 2>&1 < /dev/null &)
terminal-wait-for-prompt
preview-port: 18081
tab: preview
viewport: 1600x900
wait-for-text: demo/my_unsorted_linked_list.java
click: selector:#call-input
press: Control+A
type: insert(0,7); insert(1,3); insert(1,5); insert(3,9)
press: Enter
wait-for-text: steps ready
sleep: 1
click: selector:#last
sleep: 1
-->
<img class='full' src='data-structures-sandbox.webp' alt='Einfach verkettete Liste in Java nach vier Einfügungen: Knoten und next-Referenzen neben dem Quelltext.'>

<div class='hint task'>
<strong>Unterrichtsimpuls: Dieselben Aufrufe, zwei Welten.</strong> Die Klasse lässt
<code>insert(0,7); insert(1,3); insert(1,5); insert(3,9)</code> zuerst auf der
Array-Liste und dann auf der verketteten Liste laufen. Welche Operation verschiebt
Werte, welche biegt nur Referenzen um? Warum ist <code>get(i)</code> beim Array in
konstanter Zeit möglich, während die verkettete Liste dafür bis zu <em>n</em> Knoten
ablaufen muss? Und was kostet umgekehrt das Einfügen ganz vorne?
</div>

Die Tests sind streng, und das ist gewollt: Eine Liste gilt erst dann als korrekt,
wenn sie sich nach außen richtig verhält **und** ihr Inneres stimmt – etwa wenn
`size` wirklich der Zahl der gespeicherten Werte entspricht oder die Knotenkette keinen
Zyklus enthält. Ungültige Indizes, volle Arrays und doppelte Werte sind fester
Bestandteil der Prüfung.

## Bäume, Rekursion und AVL-Rotationen

Bei **binären Suchbäumen** wird die Suchinvariante sichtbar: links das Kleinere, rechts
das Größere. Rekursive Aufrufe lassen sich Schritt für Schritt verfolgen; man sieht, an
welchem Knoten gerade gearbeitet wird. **AVL-Bäume** speichern zusätzlich die Höhe ihrer
Teilbäume und stellen das Gleichgewicht nach dem Einfügen durch Rotationen wieder her.

Derselbe Baum, diesmal in Python implementiert, nach sechs Einfügungen:

<!-- tutorial-screenshot
tab: preview
viewport: 1600x900
select-option: #structure = avl
wait-for-text: demo/my_avl.py
click: selector:#call-input
press: Control+A
type: insert(30); insert(20); insert(10); insert(25); insert(40); insert(35)
press: Enter
wait-for-text: steps ready · avl
sleep: 1
click: selector:#last
sleep: 1
-->
<img class='full' src='data-structures-avl.webp' alt='AVL-Baum in Python nach sechs Einfügungen und zwei Rotationen.'>

<div class='hint task'>
<strong>Unterrichtsimpuls: Erst skizzieren, dann rotieren.</strong> Nach
<code>insert(30); insert(20); insert(10)</code> skizziert die Klasse zunächst den
unbalancierten Baum und begründet, welche Rotation ihn rettet. Dann lässt sie die
Sandbox nachrechnen. Später werden die vier Fälle LL, RR, LR und RL verglichen. Zum
Kontrast: Was passiert mit einem gewöhnlichen Suchbaum, wenn man 1, 2, 3, … 10 der Reihe
nach einfügt?
</div>

## Hash-Tabellen: Die Lernenden entscheiden

Die Hash-Tabelle löst Kollisionen durch **Verkettung** (separate chaining). Anzahl der
Buckets und Hashfunktion legen die Lernenden in ihrer eigenen Klasse fest – die Tests
akzeptieren jede gültige Wahl. So werden Kollisionen und Lastfaktor zu etwas, das man
gezielt herbeiführen und vermeiden kann.

<!-- tutorial-screenshot
tab: preview
viewport: 1600x900
select-option: #structure = hash
wait-for-text: demo/my_hash_table.dart
click: selector:#call-input
press: Control+A
type: insert(-7); insert(4); insert(12); insert(19); insert(27)
press: Enter
wait-for-text: steps ready · hash
sleep: 1
click: selector:#last
sleep: 1
-->
<img class='full' src='data-structures-hash.webp' alt='Hash-Tabelle mit acht Buckets, in denen sich kollidierende Schlüssel zu Ketten verbinden.'>

<div class='hint task'>
<strong>Unterrichtsimpuls: Kollisionen provozieren.</strong> Findet fünf Schlüssel, die
alle im selben Bucket landen. Was passiert mit der Suche? Ändert dann die Bucketzahl von
8 auf 7 oder 11 und wiederholt die Folge. Welche Bucketzahlen sind eine schlechte Idee,
wenn die Schlüssel zum Beispiel alle gerade sind?
</div>

<div class='hint think'>
<strong>Zum Weiterdenken:</strong> Wohin gehört der Schlüssel −7 bei acht Buckets?
Die Antwort hängt von der Sprache ab: In Dart und Python ergibt <code>-7 % 8</code>
den Wert 1, in Java und JavaScript −7 – also einen ungültigen Index. Die
Referenzbeispiele der Sandbox lösen das in jeder Sprache anders
(<code>Math.floorMod</code> in Java, eine kleine Formel in JavaScript). Ein schöner
Anlass, über die Definition von „Rest“ zu sprechen.
</div>

## Arbeiten wie in echten Projekten

Einige Werkzeuge machen die Sandbox im Alltag besonders brauchbar:

- **Repeat** spielt die eingegebene Aufruffolge nach jedem Speichern automatisch
  erneut ab – die Lernenden sehen sofort, was ihre Änderung bewirkt.
- **Run tests** prüft Methoden und interne Darstellung gegen unabhängige
  Referenzmodelle und nennt bei einem Fehler den konkreten Fall.
- Ausgaben mit `print` erscheinen in der Konsole genau an dem Schritt, an dem sie
  entstanden sind; Laufzeitfehler springen auf Wunsch zur passenden Zeile.
- **Class progress** zeigt der Lehrkraft eine Tabelle aller Lernenden und Strukturen
  mit dem jeweiligen Teststand.

Neben den gezeigten Strukturen gibt es Stacks und Queues jeweils als Array und als
Knotenkette sowie einen Min-Heap im Array.

## Eine mögliche Unterrichtsreihe

1. **Indizierter Speicher:** unsortierte Array-Liste, danach die unsortierte
   verkettete Liste mit denselben Aufrufen. Einfügen, Entfernen, Zugriff und ihre Kosten.
2. **Abstrakte Datentypen:** Stack und Queue als Verhalten formulieren und je zwei
   Implementierungen schreiben und testen; der Ringpuffer als Höhepunkt.
3. **Sortierte Listen:** Was ändert sich am Einfügen, was an der Suche?
4. **Bäume:** Suchbaum, Einfluss der Einfügereihenfolge auf die Höhe, AVL-Rotationen
   als Erweiterung.
5. **Heap und Hashing:** Prioritätswarteschlange und Hash-Tabelle als weitere
   Antworten auf die Frage, wie man schnell findet, was man sucht.

## Organisation

Die Lernenden arbeiten ausschließlich in einem **eigenen Git-Repository** im
Unterordner `structures/`, jede und jeder im eigenen Ordner. Neue Dateien legen sie mit
`./new-structure` an; vorhandene Dateien werden dabei nie überschrieben. Für Python,
JavaScript und Java braucht der Rechner den jeweiligen Interpreter bzw. Compiler.

[Einrichtung (README)](https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/README.md) ·
[Verträge aller Strukturen](https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/docs/contracts.md) ·
[Unterrichtsreihe zu den Listen](https://github.com/specht/data-structures-sandbox/blob/e4ea4fc5e126574ed27e671d20e66d43462909c0/docs/list-exercises.md)
