<div class='meta'>
image: data-structures-sandbox.webp
</div>

# Datenstrukturen unterrichten

<p class='abstract'>
In der Data Structures Sandbox implementieren Lernende abstrakte Datentypen mit
Arrays oder verketteten Knoten, führen Operationen schrittweise aus und testen
ihre Implementierungen automatisch.
</p>

## Data Structures Sandbox

Unterstützt werden unter anderem Listen, Stacks, Queues, binäre Suchbäume,
AVL-Bäume, Heaps und Hash-Tabellen. Der Quelltext wird direkt im Browser
bearbeitet. Einzelschritte, Laufzeitfehler, `print`-Ausgaben und die zugehörige
Visualisierung lassen sich gemeinsam untersuchen.

<!-- tutorial-screenshot
terminal-open
terminal-run: cd ~ && git clone -q https://github.com/specht/data-structures-sandbox.git && cd data-structures-sandbox && git checkout -q 5c99ad20922f1d7c61ddd87501e8602042f15f23
terminal-wait-for-prompt
terminal-run: cd ~/data-structures-sandbox && mkdir -p structures && ./new-structure demo stack array
terminal-wait-for-prompt
terminal-run: cd ~/data-structures-sandbox && (nohup ./run --port 18081 --no-open > /tmp/data-structures-sandbox.log 2>&1 < /dev/null &)
terminal-wait-for-prompt
preview-port: 18081
tab: preview
wait-for-text: Structure
viewport: 1600x900
-->
<img class='full' src='data-structures-sandbox.webp' alt='Data Structures Sandbox mit Codeeditor und Datenstrukturansicht.'>

## Einsatz im Unterricht

Ein guter Einstieg ist die unsortierte Liste, zunächst mit Array und dann
mit verketteten Knoten. Danach bieten sich **Stack (LIFO)** und **Queue
(FIFO)** zum Vergleich der Implementierungen an. Später können rekursive
Baumoperationen, Balanceverfahren und Hashing untersucht werden.

Automatische Tests prüfen sowohl das Verhalten der öffentlichen Operationen
als auch die Speicherstruktur. Für die gemeinsame Arbeit verwendet die Klasse
ein separates Repository im Unterordner `structures`.

[Quelltext, Installation und Anleitung](https://github.com/specht/data-structures-sandbox)
