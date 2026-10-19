<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Graphen und Wege

<p class='abstract'>
Ein Graph besteht aus Knoten und Kanten zwischen ihnen: Kreuzungen und Straßen, Zustände und Übergänge, Menschen und Freundschaften. Hier steht, wie man einen Graphen im Programm aufschreibt, wie Breitensuche, Tiefensuche, Dijkstra und A* darin einen Weg finden, und was man tun kann, wenn eine Rundreise durch viele Orte gesucht ist. Jedes Verfahren gibt es zum Durchklicken an demselben kleinen Beispiel und als Pseudocode, den du in deine Sprache übersetzt.
</p>

<script src='/include/algo_stepper.js'></script>

## Graphen

<!-- part: graph -->
<!-- short: Knoten und Kanten, eine Kante kann eine Länge haben; im Programm als Adjazenzliste: zu jedem Knoten seine Nachbarn. -->
### Knoten und Kanten

Ein **Graph** besteht aus **Knoten** und **Kanten**. Eine Kante verbindet zwei Knoten. Sie kann eine Richtung haben (eine Einbahnstraße, ein Pfeil in einem Automaten) oder in beide Richtungen gelten (eine Straße, eine Freundschaft). Oft hat sie eine Zahl, ihr **Gewicht**: die Länge einer Straße in Metern, die Zeit, die man für sie braucht, die Kosten einer Verbindung.

Das Beispiel auf dieser Seite hat acht Knoten, A bis H, und zwölf Kanten mit ihren Längen. Von A nach H gibt es einen Weg über wenige Kanten, über C und G, und einen kürzeren über mehr Kanten. Welche Suche welche findet, zeigen die Abschnitte unten.

### Die Adjazenzliste

Im Programm schreibt man einen Graphen meistens als **Adjazenzliste**: zu jedem Knoten die Liste seiner Nachbarn, jeweils mit dem Gewicht der Kante. In Ruby ist das ein Hash, in Python ein Dictionary, dessen Schlüssel die Knoten sind:

<!-- page-code: own -->
```text
A: [(B, 29), (C, 29), (D, 80)]
B: [(A, 29), (D, 33), (E, 26)]
C: [(A, 29), (D, 47), (G, 70)]
…
```

Gilt eine Kante in beide Richtungen, steht sie zweimal da: B bei A und A bei B. Eine Einbahnstraße steht nur bei dem Knoten, von dem aus man sie fahren darf. Die Alternative, eine **Adjazenzmatrix** mit einer Zeile und einer Spalte für jeden Knoten, lohnt sich nur für kleine Graphen: Ein Stadtplan mit hunderttausend Kreuzungen hätte zehn Milliarden Felder, fast alle leer.
<!-- /part -->

<!-- part: weg-zurueck -->
<!-- short: Jede Suche merkt sich zu jedem gefundenen Knoten seinen Vorgänger; der Weg ist die Kette der Vorgänger vom Ziel zurück zum Start. -->
### Der Weg zurück

Jede Suche auf dieser Seite merkt sich zu jedem Knoten, den sie findet, von welchem Knoten aus sie ihn gefunden hat: seinen **Vorgänger**. Der Start hat keinen. Ist das Ziel gefunden, liest man den Weg rückwärts ab: vom Ziel zu seinem Vorgänger, zu dessen Vorgänger und so weiter, bis zum Start.

```text
weg_zurück(vorgänger, ziel):
    wenn ziel nicht in vorgänger: es gibt keinen Weg
    weg ← leere Liste
    knoten ← ziel
    solange knoten nicht »keiner« ist:
        knoten vorne an weg anfügen
        knoten ← vorgänger[knoten]
    gib weg zurück
```

Weil man rückwärts geht, kommt jeder Knoten **vorne** dazu. Wer hinten anfügt, bekommt den Weg vom Ziel zum Start und muss ihn am Ende umdrehen.
<!-- /part -->

<!-- part: breitensuche -->
<!-- short: Breitensuche: eine Queue, wer zuerst gefunden wird, kommt zuerst dran; findet den Weg über die wenigsten Kanten. -->
## Breitensuche

Die **Breitensuche** geht vom Start aus wie eine Welle: erst zu allen Nachbarn des Starts, dann zu deren Nachbarn und so weiter. Die Knoten, die sie gefunden, aber noch nicht weiterverfolgt hat, stehen in einer **Queue**: Was zuerst hineinkommt, kommt zuerst wieder heraus (FIFO, *first in, first out*).

```text
breitensuche(graph, start, ziel):
    vorgänger ← { start: keiner }
    queue ← [start]
    solange queue nicht leer:
        knoten ← vorne aus der queue nehmen
        wenn knoten = ziel: gib weg_zurück(vorgänger, ziel) zurück
        für jeden nachbar von knoten:
            wenn nachbar nicht in vorgänger:
                vorgänger[nachbar] ← knoten
                nachbar hinten an die queue anfügen
    es gibt keinen Weg
```

Probier es aus, von A nach H. Gelb sind die Knoten in der Queue, lila die schon besuchten, der schwarze Ring zeigt, wer gerade dran ist; die dicken Linien sind die Vorgänger:

<div class='algo-stepper' data-algo='bfs' data-graph='stadt' data-start='A' data-goal='H'></div>

Die Breitensuche findet A–C–G–H: drei Kanten, die wenigsten, die es gibt, aber 128 lang. Auf die Längen achtet sie nicht. Für einen Stadtplan ist das selten der kürzeste Weg, für ein Labyrinth, in dem jeder Schritt gleich lang ist, genau richtig.

**Typische Fehler**

- Einen Knoten erst beim Herausnehmen als gefunden merken statt beim Hineinlegen. Dann kann er mehrmals in die Queue kommen, und die Suche wird bei großen Graphen sehr langsam.
- Die Queue als Liste, aus der man vorne herausnimmt, indem man alle anderen Einträge nachrückt (`shift` in Ruby ist schnell, `pop(0)` in Python nicht). Für viele Knoten nimmt man eine echte Queue (`collections.deque` in Python) oder merkt sich in einer Zählvariable, wo vorne ist.
- Die Nachbarn eines Knotens schon prüfen, bevor er dran ist. Das ist nicht falsch, aber dann gilt »ist es das Ziel?« für den Nachbarn, nicht für den Knoten.

**Aufwand:** Jeder Knoten kommt einmal in die Queue, jede Kante wird von beiden Seiten einmal angesehen: \\(O(V + E)\\) für V Knoten und E Kanten.
<!-- /part -->

<!-- part: tiefensuche -->
<!-- short: Tiefensuche: ein Stack, wer zuletzt gefunden wird, kommt zuerst dran; geht erst einen Weg zu Ende. -->
## Tiefensuche

Die **Tiefensuche** nimmt statt einer Queue einen **Stack**: Was zuletzt hineinkommt, kommt zuerst wieder heraus (LIFO, *last in, first out*). Sie geht deshalb einen Weg erst ganz zu Ende, bevor sie umkehrt, wie jemand, der in einem Labyrinth immer der rechten Wand folgt. Der Pseudocode ist der der Breitensuche, nur mit einem Stack:

```text
tiefensuche(graph, start, ziel):
    vorgänger ← { start: keiner }
    stack ← [start]
    solange stack nicht leer:
        knoten ← oben vom stack nehmen
        wenn knoten = ziel: gib weg_zurück(vorgänger, ziel) zurück
        für jeden nachbar von knoten:
            wenn nachbar nicht in vorgänger:
                vorgänger[nachbar] ← knoten
                nachbar oben auf den stack legen
    es gibt keinen Weg
```

<div class='algo-stepper' data-algo='dfs' data-graph='stadt' data-start='A' data-goal='H'></div>

Hier findet sie A–D–G–H, weil sie D als Letztes auf den Stack gelegt hat und dort weitermacht. Den kürzesten Weg findet die Tiefensuche im Allgemeinen nicht, und auch nicht den mit den wenigsten Kanten. Sie ist aber die richtige Suche für Fragen wie »Gibt es überhaupt einen Weg?«, »Welche Knoten erreicht man?« oder »Hat der Graph einen Kreis?«, und sie lässt sich ganz kurz rekursiv schreiben: Die Funktion ruft sich für jeden noch nicht gefundenen Nachbarn selbst auf, und der Stack ist der der Aufrufe.
<!-- /part -->

<!-- part: dijkstra -->
<!-- short: Dijkstra: eine Priority Queue nach dem Weg bis hier; der kleinste kommt zuerst dran und ist dann fertig; findet den kürzesten Weg, wenn keine Kante negativ ist. -->
## Dijkstra

Den kürzesten Weg nach Länge findet das Verfahren von **Edsger W. Dijkstra** (1959). Es nimmt als Nächstes immer den Knoten, der vom Start aus am nächsten liegt, auf dem besten Weg, den es bis jetzt kennt. Dafür hält es die gefundenen Knoten in einer **Priority Queue**: Man legt Einträge mit einer Priorität hinein und nimmt immer den mit der kleinsten heraus. Wie das schnell geht, steht unter [Heap](/reference/heap).

```text
dijkstra(graph, start, ziel):
    weg ← { start: 0 }
    vorgänger ← { start: keiner }
    fertig ← leere Menge
    pq ← Priority Queue mit (0, start)
    solange pq nicht leer:
        (abstand, knoten) ← kleinsten Eintrag aus pq nehmen
        wenn knoten in fertig: weiter        (ein alter Eintrag)
        knoten zu fertig hinzufügen
        wenn knoten = ziel: gib weg_zurück(vorgänger, ziel) zurück
        für jeden (nachbar, länge) von knoten:
            neu ← abstand + länge
            wenn nachbar nicht in weg oder neu < weg[nachbar]:
                weg[nachbar] ← neu
                vorgänger[nachbar] ← knoten
                (neu, nachbar) in pq legen
    es gibt keinen Weg
```

Über jedem Knoten steht der beste bekannte Weg vom Start. Die Tabelle daneben zeigt, was sich in diesem Schritt geändert hat:

<div class='algo-stepper' data-algo='dijkstra' data-graph='stadt' data-start='A' data-goal='H'></div>

Dijkstra findet A–B–E–F–H mit 107, vier Kanten, aber kürzer als die 128 der Breitensuche. Ein Knoten kann mehrmals in der Priority Queue stehen: Findet die Suche einen kürzeren Weg zu ihm, legt sie ihn noch einmal hinein, statt den alten Eintrag zu suchen. Kommt der alte später heraus, ist der Knoten schon fertig, und die Suche überspringt ihn (durchgestrichen).

**Warum stimmt das?** Kommt ein Knoten heraus, ist alles andere in der Priority Queue mindestens so weit weg. Jeder andere Weg zu ihm müsste über einen dieser Knoten gehen und wäre damit nicht kürzer, solange keine Kante eine negative Länge hat. Deshalb darf die Suche aufhören, wenn das **Ziel herauskommt**, nicht schon, wenn es hineingelegt wird.

**Typische Fehler**

- Aufhören, sobald das Ziel in die Priority Queue kommt. Dann ist sein Weg vielleicht noch nicht der kürzeste.
- Einen Knoten schon beim Hineinlegen als fertig markieren. Dann bekommt er keinen besseren Weg mehr.
- Alte Einträge nicht überspringen. Das Ergebnis stimmt meist trotzdem, aber die Suche arbeitet Knoten mehrmals ab.
- Den kleinsten Eintrag jedes Mal in der ganzen Liste suchen. Das ist richtig, aber für große Graphen langsam; dafür gibt es den [Heap](/reference/heap).

**Aufwand:** Mit einem binären Heap \\(O((V + E) \log V)\\). Wer den kleinsten Eintrag jedes Mal in einer Liste sucht, braucht \\(O(V^2)\\): Bei hunderttausend Knoten ist das zehntausendmal so viel.
<!-- /part -->

<!-- part: a-stern -->
<!-- short: A*: wie Dijkstra, die Priorität ist der Weg bis hier plus eine Schätzung für den Rest, die nie zu groß ist (die Luftlinie). -->
## A-Stern

Dijkstra sucht in alle Richtungen gleich, auch weg vom Ziel. **A*** (»A-Stern«, 1968) weiß, wo das Ziel liegt, und sucht zuerst in seiner Richtung. Die Priorität eines Knotens ist der Weg vom Start bis zu ihm **plus eine Schätzung** für den Rest:

$$f(\text{knoten}) = \text{weg}(\text{knoten}) + h(\text{knoten})$$

Auf einer Karte ist die Schätzung \\(h\\) die Luftlinie zum Ziel. Der Pseudocode ist der von Dijkstra, mit zwei Änderungen: In die Priority Queue kommt \\(\text{neu} + h(\text{nachbar})\\) statt nur \\(\text{neu}\\), und der Weg eines Knotens steht in `weg`, nicht in der Priorität:

```text
        (_, knoten) ← kleinsten Eintrag aus pq nehmen
        …
            neu ← weg[knoten] + länge
            …
                (neu + h(nachbar), nachbar) in pq legen
```

<div class='algo-stepper' data-algo='astar' data-graph='stadt' data-start='A' data-goal='H'></div>

A* findet denselben Weg wie Dijkstra, ist aber nach sechs statt acht Knoten fertig: C und G liegen weg vom Ziel, ihr Weg bis hier plus die Luftlinie ist größer als die 107, und sie kommen gar nicht erst dran. Auf einem Stadtplan mit hunderttausend Knoten macht das aus einem Kreis um den Start ein schmales Band zum Ziel.

**Wann stimmt der Weg?** Wenn die Schätzung **nie größer ist** als der wirkliche Rest. Die Luftlinie erfüllt das, kein Weg auf der Straße ist kürzer. Je näher die Schätzung am wirklichen Rest liegt, desto weniger Knoten besucht A*; ist sie 0, ist A* genau Dijkstra. Zählt der Graph Sekunden statt Meter, teilt man die Luftlinie durch das größte Tempo, das es gibt, damit sie auch in Sekunden nie zu groß ist.
<!-- /part -->

<!-- part: rundreise -->
<!-- short: Die beste Reihenfolge für eine Rundreise: alle ausprobieren ((n − 1)! viele), der nächste Nachbar (schnell, oft schlecht), 2-opt (umdrehen, solange es kürzer wird). -->
## Eine Rundreise

Soll eine Reise durch mehrere Orte gehen und am Ende zurück zum Anfang, ist die beste Reihenfolge gesucht: das **Problem des Handlungsreisenden** (*Traveling Salesperson Problem*). Man rechnet zuerst mit einer der Suchen oben aus, wie weit es von jedem Ort zu jedem anderen ist, und schreibt das in eine Tabelle `abstand[i][j]`. Danach braucht man den Graphen nicht mehr.

**Alle ausprobieren.** Der erste Ort steht fest, die übrigen \\(n - 1\\) kommen in jeder möglichen Reihenfolge dahinter. Das sind \\((n-1)!\\) Reihenfolgen: bei 5 Orten 24, bei 10 schon 362 880, bei 20 mehr als \\(10^{17}\\). Für ein paar Orte ist das die sichere Wahl.

**Der nächste Nachbar.** Vom Anfang aus immer zum nächsten Ort, an dem man noch nicht war:

```text
nächster_nachbar(abstand):
    reihenfolge ← [0]
    übrig ← alle Orte außer 0
    solange übrig nicht leer:
        letzter ← letzter Ort in reihenfolge
        nächster ← der Ort in übrig mit dem kleinsten abstand[letzter][ort]
        nächster an reihenfolge anfügen, aus übrig entfernen
    gib reihenfolge zurück
```

Das ist schnell, aber am Ende bleiben die weiten Wege übrig, und die Rundreise kreuzt sich oft selbst.

**2-opt.** Man nimmt ein Stück der Reihenfolge und dreht es um. Wird die Rundreise dadurch kürzer, bleibt es dabei. Das probiert man für jedes Stück, und so lange, bis kein Umdrehen mehr hilft:

```text
[0, 3, 2, 1, 4]   das Stück 2, 1, 4 umdrehen
[0, 3, 4, 1, 2]
```

Eine Kreuzung verschwindet, wenn man das Stück zwischen ihren beiden Kanten umdreht. 2-opt findet nicht immer die beste Rundreise, aber meist eine, die nur wenig länger ist, in einem Bruchteil der Zeit. Für viele Orte kennt niemand ein schnelles Verfahren, das sicher die beste findet; Paketdienste und Routenplaner arbeiten deshalb mit Verfahren wie diesem.
<!-- /part -->

## Zum Nachschlagen

Die Adjazenzliste ist in Ruby ein `Hash`, in Python ein `dict`. Die Queue der Breitensuche ist in Python `collections.deque`, in Ruby ein `Array` mit `push` und `shift`; den Stack der Tiefensuche gibt jedes Array mit `push` (Python: `append`) und `pop` her. Die Priority Queue für Dijkstra und A* ist in Python `heapq`; Ruby hat keine, dort programmierst du den [Heap](/reference/heap) selbst.

- [Ruby: `Hash`](https://docs.ruby-lang.org/en/3.4/Hash.html): Schlüssel und Werte, für die Adjazenzliste und für `vorgänger` und `weg`.
- [Python: `dict`](https://docs.python.org/3/library/stdtypes.html#typesmapping): dasselbe in Python.
- [Python: `collections.deque`](https://docs.python.org/3/library/collections.html#collections.deque): die Queue für die Breitensuche.
- [Python: `heapq`](https://docs.python.org/3/library/heapq.html): die Priority Queue für Dijkstra und A*.
- [VisuAlgo: Breiten- und Tiefensuche](https://visualgo.net/en/dfsbfs) und [kürzeste Wege](https://visualgo.net/en/sssp): keine offizielle Referenz; beide Suchen und Dijkstra als Animation, auch an selbst gezeichneten Graphen, auf Englisch.
