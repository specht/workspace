<div class='meta'>
kind: reference
search: DEA, NEA, DFA, NFA, Automat, Zustandsautomat, Potenzmengenkonstruktion, Minimierung
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Endliche Automaten

<p class='abstract'>
Ein endlicher Automat liest ein Wort Zeichen für Zeichen und weiß am Ende nur eins: ob das Wort dazugehört. Hier steht, wie ein Automat aufgeschrieben wird, wie ein deterministischer (DEA) und ein nichtdeterministischer (NEA) ein Wort lesen, wie aus einem regulären Ausdruck ein NEA wird, wie die Potenzmengenkonstruktion daraus einen DEA macht und wie die Minimierung ihn so klein wie möglich macht. Jeder Schritt hat ein Beispiel zum Durchklicken und Pseudocode, den du in deine Sprache übersetzt.
</p>

<script src='/include/automaton_layout.js'></script>
<script src='/include/automatron.js'></script>
<script src='/include/automaton_stepper.js'></script>
<script src='/include/algo_stepper.js'></script>

## Ein endlicher Automat

<!-- part: definition -->
<!-- short: Zustände, ein Alphabet, Übergänge, ein Startzustand und akzeptierende Zustände; im Bild Kreise, Pfeile, ein Pfeil ohne Anfang zum Start und ein doppelter Rand fürs Akzeptieren. -->
### Was dazugehört

Ein **endlicher Automat** hat:

- endlich viele **Zustände**, meist q<sub>0</sub>, q<sub>1</sub>, … genannt;
- ein **Alphabet**: die Zeichen, die er lesen kann, zum Beispiel a und b oder die Ziffern 0 bis 9;
- **Übergänge**: zu einem Zustand und einem Zeichen der Zustand, in den er wechselt;
- einen **Startzustand**, in dem er jedes Wort zu lesen beginnt;
- **akzeptierende Zustände**: Endet er nach dem letzten Zeichen in einem davon, **nimmt er das Wort an**, sonst lehnt er es ab.

Mathematisch ist ein Automat das Tupel \\((Q, \Sigma, \delta, q\_0, F)\\): Q die Zustände, Σ (Sigma) das Alphabet, δ (delta) die Übergänge, q<sub>0</sub> der Start, F die akzeptierenden Zustände. Die Wörter, die er annimmt, heißen seine **Sprache**.

### Als Bild und als Tabelle

Im Bild ist jeder Zustand ein Kreis und jeder Übergang ein Pfeil mit seinem Zeichen. Ein Pfeil ohne Anfang zeigt auf den Start, akzeptierende Zustände haben einen doppelten Rand. Dieser Automat nimmt die Wörter aus a und b an, in denen **a gerade oft** vorkommt: q<sub>0</sub> heißt »bisher gerade oft«, q<sub>1</sub> »bisher ungerade oft«.

<div class='automaton-drawing' role='img' aria-label='Zwei Kreise q0 und q1, q0 mit doppeltem Rand ist der Start. a führt von q0 nach q1 und von q1 zurück nach q0; b führt bei beiden im Bogen zum selben Zustand zurück.' data-states='q0 q1' data-start='q0' data-accept='q0' data-transitions='q0 a q1, q0 b q0, q1 a q0, q1 b q1'></div>

Im Programm schreibt man die Übergänge als **Tabelle**: eine Zeile je Zustand, eine Spalte je Zeichen. In Ruby ist das ein Hash, in Python ein Dictionary mit dem Paar aus Zustand und Zeichen als Schlüssel:

<table class='table' style='width: auto;'>
<tr><th>Zustand</th><th>a</th><th>b</th></tr>
<tr><td>→ q<sub>0</sub> (akzeptiert)</td><td>q<sub>1</sub></td><td>q<sub>0</sub></td></tr>
<tr><td>q<sub>1</sub></td><td>q<sub>0</sub></td><td>q<sub>1</sub></td></tr>
</table>

Fehlt zu einem Zustand und einem Zeichen ein Übergang, geht der Automat in einen **Fehlerzustand**, aus dem er nicht mehr herauskommt und der nichts akzeptiert. Meist zeichnet man ihn nicht, sondern sagt dazu, dass alle fehlenden Pfeile dorthin führen.
<!-- /part -->

## Ein DEA liest ein Wort

<!-- part: dea -->
<!-- short: DEA: zu jedem Zustand und Zeichen genau ein nächster Zustand; das Wort Zeichen für Zeichen lesen, am Ende in einem akzeptierenden Zustand heißt angenommen. -->
Bei einem **deterministischen endlichen Automaten** (DEA) steht jeder Schritt fest: Zu jedem Zustand und jedem Zeichen gibt es höchstens einen Übergang. Er braucht sich darum nur einen Zustand zu merken.

```pseudocode
accepts(word):
    state ← start
    for each symbol in word:
        if (state, symbol) not in delta: return false    # Fehlerzustand
        state ← delta[(state, symbol)]
    return state in accepting
```

Probier es aus: Der Ring ist der Zustand, in dem der DEA gerade ist, der Pfeil, den er nimmt, leuchtet auf. Das Wort darfst du ändern.

<div class='automaton-stepper' data-stepper data-title='DEA: a kommt gerade oft vor' data-states='q0 q1' data-start='q0' data-accept='q0' data-alphabet='ab' data-transitions='q0 a q1, q0 b q0, q1 a q0, q1 b q1' data-input='abbab'></div>

**Typische Fehler**

- Nach jedem Zeichen prüfen, ob der Zustand akzeptiert. Angenommen oder abgelehnt wird erst, wenn das Wort zu Ende ist.
- Ein fehlender Übergang bricht mit einem Fehler ab (`fetch` in Ruby, ein `KeyError` in Python). Gemeint ist der Fehlerzustand: Das Wort wird abgelehnt.
<!-- /part -->

## Ein NEA rät

<!-- part: nea -->
<!-- short: NEA: zu einem Zustand und Zeichen mehrere Übergänge oder keiner; er ist in einer Menge von Zuständen und nimmt an, wenn einer davon akzeptiert. -->
### Mehrere Wege zugleich

Ein **nichtdeterministischer endlicher Automat** (NEA) darf zu einem Zustand und einem Zeichen mehrere Übergänge haben, oder keinen. Man stellt sich vor, er **rät** den richtigen Weg; nachrechnen lässt sich das, indem man alle Wege zugleich verfolgt. Der NEA ist dann nicht in einem Zustand, sondern in einer **Menge von Zuständen**, und er nimmt ein Wort an, wenn am Ende **wenigstens einer** davon akzeptiert.

Dieser NEA nimmt die Wörter an, in denen **aa** vorkommt. In q<sub>0</sub> liest er, was vor dem aa steht; bei jedem a kann das aa anfangen:

<div class='automaton-stepper' data-stepper data-title='NEA: enthält aa' data-states='q0 q1 q2' data-start='q0' data-accept='q2' data-alphabet='ab' data-transitions='q0 a-b q0, q0 a q1, q1 a q2, q2 a-b q2' data-input='abaab'></div>

```pseudocode
accepts(word):
    states ← closure({start})
    for each symbol in word:
        next ← {}
        for each state in states:
            add every target of (state, symbol) to next
        states ← closure(next)
    return states contains an accepting state
```

### ε-Übergänge

Ein **ε-Übergang** (Epsilon) kostet kein Zeichen: Der NEA darf ihn jederzeit nehmen, auch mehrere hintereinander. Die Zustände, die er von einem Zustand aus über ε-Übergänge erreicht, samt diesem selbst, heißen seine **ε-Hülle**. Die Funktion `closure` oben bildet sie für eine ganze Menge; ein NEA ohne ε-Übergänge gibt seine Menge unverändert zurück.

```pseudocode
closure(states):
    result ← copy of states
    todo ← list of states
    while todo not empty:
        state ← take one from todo
        for each target of (state, ε):
            if target not in result:
                add target to result and to todo
    return result
```

Ein NEA ist oft kleiner und leichter aufzuschreiben als ein DEA für dieselbe Sprache: Er beschreibt nur das Muster. Ein DEA ist leichter auszuführen: Er merkt sich einen Zustand statt einer Menge.
<!-- /part -->

## Vom regulären Ausdruck zum NEA

<!-- part: thompson -->
<!-- short: Thompson-Konstruktion: für jedes Zeichen ein Pfeil, für Hintereinander, |, * (Kleenesche Hülle) und + (positive Hülle) je ein Baustein aus ε-Übergängen; daraus ein NEA mit einem Start und einem akzeptierenden Zustand. -->
Ein [regulärer Ausdruck](/reference/regex) beschreibt eine Sprache, und die **Thompson-Konstruktion** baut daraus einen NEA, Stück für Stück aus wenigen **Bausteinen**. Jeder Baustein hat einen Start und einen akzeptierenden Zustand, und die Bausteine werden mit ε-Übergängen verbunden. Die Bilder in diesem Abschnitt zeichnet Automatron, und genau so sehen die Automaten dort aus. Es baut nach dieser Konstruktion, lässt aber ε-Übergänge weg, die nur ein Stück an das nächste hängen.

<div class='gallery columns-3'>
<figure>
<div class='automaton-drawing' role='img' aria-label='Zwei Kreise: a führt vom Start q0 zum akzeptierenden q1.' data-regex='a' data-automaton='nfa'></div>
<figcaption><strong>Symbol</strong> <code>a</code>: ein Zeichen ist ein Pfeil.</figcaption>
</figure>
<figure>
<div class='automaton-drawing' role='img' aria-label='Drei Kreise in einer Reihe: a von q0 nach q1, b von q1 nach q2, q2 akzeptiert.' data-regex='ab' data-automaton='nfa'></div>
<figcaption><strong>Verkettung</strong> (Konkatenation) <code>ab</code>: Der akzeptierende Zustand des ersten Stücks ist der Start des zweiten.</figcaption>
</figure>
<figure>
<div class='automaton-drawing' role='img' aria-label='Vom Start q0 führen zwei ε-Pfeile zu zwei Wegen, einem mit a und einem mit b; beide führen mit ε zum akzeptierenden q5.' data-regex='a|b' data-automaton='nfa'></div>
<figcaption><strong>Alternative</strong> <code>a|b</code>: ein neuer Start mit ε zu beiden Stücken, von beiden mit ε zu einem neuen Ende. Die Sprache ist die <strong>Vereinigung</strong> der beiden Sprachen.</figcaption>
</figure>
<figure>
<div class='automaton-drawing' role='img' aria-label='Vom Start q0 führt ε nach q1 und, im Bogen über alles hinweg, nach dem akzeptierenden q3. Von q1 führt a nach q2, von q2 führt ε zurück nach q1 und weiter nach q3.' data-regex='a*' data-automaton='nfa'></div>
<figcaption><strong>Kleenesche Hülle</strong> (Kleene-Stern) <code>a*</code>: ein ε am Stück vorbei (keinmal) und eins vom Ende zurück an seinen Anfang (noch einmal).</figcaption>
</figure>
<figure>
<div class='automaton-drawing' role='img' aria-label='Wie bei a*, aber ohne den ε-Bogen vom Start zum Ende: q0 führt mit ε nach q1, a nach q2, von q2 ε zurück nach q1 und weiter zum akzeptierenden q3.' data-regex='a+' data-automaton='nfa'></div>
<figcaption><strong>Positive Hülle</strong> <code>a+</code>: derselbe Baustein ohne den Pfeil vorbei, das Stück kommt mindestens einmal.</figcaption>
</figure>
<figure>
<div class='automaton-drawing' role='img' aria-label='Wie bei a*, aber ohne den ε-Pfeil zurück: q0 führt mit ε nach q1 und im Bogen nach q3, q1 mit a nach q2, q2 mit ε nach q3.' data-regex='a?' data-automaton='nfa'></div>
<figcaption><strong>Option</strong> <code>a?</code>: derselbe Baustein ohne den Pfeil zurück, das Stück kommt höchstens einmal.</figcaption>
</figure>
</div>

Symbol, Verkettung, Alternative und Kleenesche Hülle reichen für jeden regulären Ausdruck; positive Hülle und Option sind Abkürzungen: `a+` ist `aa*`, `a?` ist »a oder nichts«. Die Kleenesche Hülle einer Sprache L, geschrieben L<sup>*</sup>, sind alle Wörter, die sich aus beliebig vielen Wörtern von L zusammensetzen lassen, auch aus keinem: Das leere Wort ε gehört immer dazu. Die positive Hülle L<sup>+</sup> lässt das leere Zusammensetzen weg: mindestens ein Wort von L; L<sup>+</sup> ist dasselbe wie L L<sup>*</sup>. Größere Ausdrücke werden aus diesen Bausteinen zusammengesetzt, von innen nach außen; für `(a|b)*aa` steckt der Baustein für `a|b` im Baustein für `*`, und danach kommen zwei a:

<div class='automaton-drawing' role='img' aria-label='Der NEA zu (a|b)*aa: in einer Reihe der Baustein für a oder b, umschlossen von einem ε-Bogen zurück und einem ε-Bogen vorbei, danach a und noch einmal a bis zum akzeptierenden q9.' data-regex='(a|b)*aa' data-automaton='nfa'></div>
<!-- /part -->

## Potenzmengenkonstruktion

<!-- part: potenzmenge -->
<!-- short: Potenzmengenkonstruktion: jeder Zustand des DEA ist eine Menge von Zuständen des NEA; beim Start die ε-Hülle des Starts, je Zeichen die ε-Hülle aller Ziele; akzeptierend ist eine Menge mit einem akzeptierenden Zustand des NEA. -->
Ein NEA lässt sich immer in einen DEA verwandeln, der dieselben Wörter annimmt. Die Idee steckt schon im Pseudocode des NEA: Er ist in einer Menge von Zuständen, und welche Menge nach dem nächsten Zeichen kommt, steht fest. Die **Potenzmengenkonstruktion** macht darum aus jeder Menge, die vorkommen kann, einen Zustand des DEA. (Die Potenzmenge ist die Menge aller Teilmengen; gebraucht werden meist nur wenige davon.)

```pseudocode
to_dea(nea):
    first ← closure({nea.start})
    states ← [first]
    todo ← [first]
    while todo not empty:
        set ← take one from todo
        for each symbol a:
            next ← closure(every target of (state, a) for each state in set)
            if next is empty: continue                 # Fehlerzustand
            if next not in states: add next to states and to todo
            delta[(set, a)] ← next
    accepting: every set that contains an accepting state of the nea
```

Schritt für Schritt für den NEA, den Automatron aus `a*b` baut: oben der NEA, die durchgezogenen Ringe sind `set`, die gestrichelten `next`; unten wächst der DEA, jeder Zustand mit seiner Menge darunter, an der Stelle, die er am Ende hat; daneben die Tabelle. Bei `a*` arbeitet die ε-Hülle mit.

<div class='algo-stepper' data-title='Potenzmengenkonstruktion für a*b' data-algo='subset' data-regex='a*b'></div>

Ein Beispiel ohne ε zum Nachrechnen: Dieser NEA nimmt die Wörter an, die auf **aa** enden.

<div class='automaton-drawing' role='img' aria-label='NEA mit drei Kreisen: q0 ist der Start und führt mit a und b zu sich selbst und mit a nach q1; q1 führt mit a nach q2, q2 akzeptiert.' data-states='q0 q1 q2' data-start='q0' data-accept='q2' data-transitions='q0 a-b q0, q0 a q1, q1 a q2'></div>

Die Konstruktion beginnt mit {q<sub>0</sub>} und geht jede neue Menge mit jedem Zeichen durch:

<table class='table' style='width: auto;'>
<tr><th>Zustand des DEA</th><th>Menge im NEA</th><th>a</th><th>b</th></tr>
<tr><td>→ r<sub>0</sub></td><td>{q<sub>0</sub>}</td><td>{q<sub>0</sub>, q<sub>1</sub>} = r<sub>1</sub>, neu</td><td>{q<sub>0</sub>} = r<sub>0</sub></td></tr>
<tr><td>r<sub>1</sub></td><td>{q<sub>0</sub>, q<sub>1</sub>}</td><td>{q<sub>0</sub>, q<sub>1</sub>, q<sub>2</sub>} = r<sub>2</sub>, neu</td><td>{q<sub>0</sub>} = r<sub>0</sub></td></tr>
<tr><td>r<sub>2</sub> (akzeptiert, enthält q<sub>2</sub>)</td><td>{q<sub>0</sub>, q<sub>1</sub>, q<sub>2</sub>}</td><td>{q<sub>0</sub>, q<sub>1</sub>, q<sub>2</sub>} = r<sub>2</sub></td><td>{q<sub>0</sub>} = r<sub>0</sub></td></tr>
</table>

Keine neue Menge mehr: Der DEA hat drei Zustände. r<sub>1</sub> heißt »zuletzt ein a gelesen«, r<sub>2</sub> »zuletzt zwei a«.

<div class='automaton-drawing' role='img' aria-label='DEA mit drei Kreisen r0, r1, r2 in einer Reihe, r2 akzeptiert. a führt von r0 nach r1 und von r1 nach r2, in r2 bleibt er mit a. b führt von jedem Zustand zurück nach r0.' data-states='r0 r1 r2' data-start='r0' data-accept='r2' data-transitions='r0 a r1, r0 b r0, r1 a r2, r1 b r0, r2 a r2, r2 b r0'></div>

**Typische Fehler**

- Die ε-Hülle vergessen, beim Start oder nach einem Zeichen. Dann fehlen dem DEA Wege, und er lehnt Wörter ab, die dazugehören.
- Mengen als Listen vergleichen: {q<sub>0</sub>, q<sub>1</sub>} und {q<sub>1</sub>, q<sub>0</sub>} sind dieselbe Menge. In Ruby und Python nimmt man ein `Set` (Python: ein `frozenset`, damit es als Schlüssel taugt) oder sortiert die Liste.
- Ein DEA kann viel mehr Zustände haben als sein NEA. Bei manchen Sprachen wächst er so schnell, dass die Konstruktion an ihre Grenzen kommt; Automatron hört darum bei 60 Zuständen auf.
<!-- /part -->

## Minimierung

<!-- part: minimierung -->
<!-- short: Minimierung: Zustände, die kein Wort voneinander unterscheidet, werden einer; Paare markieren, erst akzeptierend gegen nicht akzeptierend, dann jedes Paar, das mit einem Zeichen zu einem markierten Paar führt; was unmarkiert bleibt, wird zusammengelegt. -->
### Welche Zustände dasselbe tun

Der DEA aus der Potenzmengenkonstruktion hat oft mehr Zustände als nötig. Zwei Zustände dürfen **einer werden**, wenn kein Wort sie unterscheidet: Von beiden aus nimmt der DEA genau dieselben Wörter an. Die **Minimierung** findet diese Zustände und legt sie zusammen. Heraus kommt der **minimierte DEA**, der DEA mit den wenigsten Zuständen für die Sprache; es gibt nur einen, bis auf die Namen der Zustände.

### Das Markierungsverfahren

Man schreibt alle Paare von Zuständen in eine Tabelle und **markiert** die, die sicher verschieden sind:

```pseudocode
minimize(dea):
    mark every pair (p, q) where exactly one of p and q is accepting
    repeat until nothing changes:
        for each unmarked pair (p, q):
            for each symbol a:
                if the pair (delta[(p, a)], delta[(q, a)]) is marked:
                    mark (p, q)
    the states of every unmarked pair become one state
```

Schritt für Schritt für den DEA, den Automatron aus `(a|b)*abb` baut, »endet auf abb«: Die beiden Ringe sind das Paar, das gerade dran ist, die gestrichelten das Paar, zu dem ein Zeichen führt; daneben die Tabelle der Paare.

<div class='algo-stepper' data-title='Minimierung für (a|b)*abb' data-algo='minimize' data-regex='(a|b)*abb'></div>

Ein Paar wird markiert, wenn ein Zeichen es zu einem schon markierten Paar führt: Dann gibt es ein Wort, das die beiden unterscheidet, dieses Zeichen und dahinter das Wort für das andere Paar. Automatron baut für `(a|b)*bb`, »endet auf bb«, diesen DEA:

<div class='automaton-drawing' role='img' aria-label='DEA mit vier Kreisen r0 bis r3, r3 akzeptiert. a führt von jedem Zustand nach r1, b führt von r0 und r1 nach r2 und von r2 und r3 nach r3.' data-regex='(a|b)*bb' data-automaton='dfa'></div>

- Erst sind alle Paare mit r<sub>3</sub> verschieden: r<sub>3</sub> akzeptiert, die anderen nicht.
- In der ersten Runde führt b das Paar (r<sub>0</sub>, r<sub>2</sub>) zu (r<sub>2</sub>, r<sub>3</sub>), das markiert ist; ebenso (r<sub>1</sub>, r<sub>2</sub>). Beide werden markiert, das Zeichen steht in Klammern dabei.
- (r<sub>0</sub>, r<sub>1</sub>) führt mit a zu (r<sub>1</sub>, r<sub>1</sub>) und mit b zu (r<sub>2</sub>, r<sub>2</sub>), also nirgends zu einem markierten Paar. In der zweiten Runde ändert sich nichts mehr.

<table class='table' style='width: auto;'>
<tr><th></th><th>r<sub>1</sub></th><th>r<sub>2</sub></th><th>r<sub>3</sub></th></tr>
<tr><th>r<sub>0</sub></th><td><i class='bi bi-check-lg text-success'></i> gleich</td><td><i class='bi bi-x-lg text-danger'></i> (b)</td><td><i class='bi bi-x-lg text-danger'></i></td></tr>
<tr><th>r<sub>1</sub></th><td></td><td><i class='bi bi-x-lg text-danger'></i> (b)</td><td><i class='bi bi-x-lg text-danger'></i></td></tr>
<tr><th>r<sub>2</sub></th><td></td><td></td><td><i class='bi bi-x-lg text-danger'></i></td></tr>
</table>

r<sub>0</sub> und r<sub>1</sub> werden ein Zustand s<sub>0</sub>, r<sub>2</sub> wird s<sub>1</sub>, r<sub>3</sub> wird s<sub>2</sub>. Der minimierte DEA hat drei Zustände:

<div class='automaton-drawing' role='img' aria-label='Minimierter DEA mit drei Kreisen s0, s1, s2 in einer Reihe, s2 akzeptiert. b führt von s0 nach s1 und von s1 nach s2, in s2 bleibt er mit b; a führt von jedem Zustand nach s0.' data-regex='(a|b)*bb' data-automaton='min'></div>
<!-- /part -->

## Zum Nachschlagen

Wie man solche Automaten selbst programmiert, zeigt die Aktivität [Endliche Automaten](/automata); [Automatron](/automatron) rechnet alle Schritte dieser Seite für einen eigenen regulären Ausdruck durch. Automaten stehen in jedem Lehrbuch der theoretischen Informatik. Zum Weiterlesen:

- [Wikipedia: Endlicher Automat](https://de.wikipedia.org/wiki/Endlicher_Automat), mit [deterministischem](https://de.wikipedia.org/wiki/Deterministischer_endlicher_Automat) und [nichtdeterministischem](https://de.wikipedia.org/wiki/Nichtdeterministischer_endlicher_Automat) Automaten: die Definitionen, Beispiele und Beweise.
- [Wikipedia: Thompson-Konstruktion](https://de.wikipedia.org/wiki/Thompson-Konstruktion), [Potenzmengenkonstruktion](https://de.wikipedia.org/wiki/Potenzmengenkonstruktion) und [Minimalautomat](https://de.wikipedia.org/wiki/Minimalautomat): die drei Verfahren dieser Seite ausführlich.
- [inf-schule: Automaten und Sprachen](https://inf-schule.de/automaten-sprachen): dasselbe Thema für die Schule, mit Aufgaben, auf Deutsch.
