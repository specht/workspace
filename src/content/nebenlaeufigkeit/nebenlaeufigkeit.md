<div class='meta'>
kind: projekt
language: Java / Dart
languages: [java, dart]
duration: 3 Doppelstunden
curriculum: "Oberstufe: Rechner und Netze (Client-Server-Struktur)"
topics: [client-server]
requires: [webserver]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Nebenläufigkeit

<p class='abstract'>
Ein Webserver beantwortet viele Anfragen zugleich, ein Spiel rechnet, während es zeichnet: Teile eines Programms laufen nebeneinander. Das geht schief, sobald sie dieselben Daten ändern. In dieser Aktivität probierst du zuerst ohne Code aus, wo es schiefgeht, mit zwei Threads, deren Schritte du selbst in eine Reihenfolge bringst. Dann schützt du gemeinsame Daten mit einem Lock und siehst, wie zwei Locks einander blockieren können.
</p>

## Zwei Threads, ein Zähler

Im Webserver zählte ein Zähler die Besuche, und jede Verbindung lief in einem eigenen Thread. Zählen sieht aus wie ein einziger Schritt, `counter ← counter + 1`. Der Rechner macht daraus drei: den Wert lesen, eins dazuzählen, den neuen Wert zurückschreiben. Jeder Thread hat dafür seine eigene Variable, `a` und `b`; `counter` teilen sich beide.

Zwei Threads können ihre Schritte in jeder Reihenfolge machen. Welcher als Nächstes dran ist, entscheidest hier du: Jeder Knopf führt die nächste Zeile eines Threads aus.

<div class='interleaving' data-title='Zwei Threads zählen' data-program='{"shared": {"counter": 0}, "locks": [], "threads": {"A": ["a ← counter", "a ← a + 1", "counter ← a"], "B": ["b ← counter", "b ← b + 1", "counter ← b"]}}'></div>

<script src='/include/interleaving.js'></script>

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass erst A alle drei Schritte machen, dann B. Achte darauf, welchen Wert <code>b</code> beim Lesen bekommt und was am Ende in <code>counter</code> steht. Dann von vorn, diesmal abwechselnd: A, B, A, B, A, B.
</div>

_task(verlorene-erhoehung)

<!-- after: verlorene-erhoehung -->
Liest B den Zähler, bevor A seinen neuen Wert zurückgeschrieben hat, rechnen beide mit 0 und schreiben beide 1. Eine der beiden Erhöhungen ist verloren, ohne Fehlermeldung. Das heißt **lost update**; dass das Ergebnis von der Reihenfolge abhängt, in der die Threads drankommen, heißt **Race Condition**.
<!-- /after -->

_task(reihenfolgen-ohne-verlust)

## Ein Lock

Ein **Lock** (auch Mutex) sorgt dafür, dass immer nur ein Thread einen Abschnitt ausführt. `lock m` nimmt den Lock `m`; hält ihn schon ein anderer Thread, wartet der Thread an dieser Zeile, bis `unlock m` ihn freigibt.

<div class='interleaving' data-title='Zählen mit einem Lock' data-program='{"shared": {"counter": 0}, "locks": ["m"], "threads": {"A": ["lock m", "a ← counter", "a ← a + 1", "counter ← a", "unlock m"], "B": ["lock m", "b ← counter", "b ← b + 1", "counter ← b", "unlock m"]}}'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Versuch wieder, eine Erhöhung zu verlieren. Achte darauf, wann der Knopf eines Threads grau wird und was dann in seiner Spalte steht.
</div>

## Zwei Locks

Eine Bank überweist zwischen zwei Konten `x` und `y`. Damit niemand dazwischenfunkt, nimmt jede Überweisung die Locks beider Konten. A überweist von `x` nach `y` und nimmt erst `m` (für `x`), dann `n`. B überweist in die andere Richtung und nimmt erst `n`, dann `m`.

<div class='interleaving' data-title='Zwei Überweisungen' data-program='{"shared": {"x": 10, "y": 10}, "locks": ["m", "n"], "threads": {"A": ["lock m", "lock n", "x ← x - 5", "y ← y + 5", "unlock n", "unlock m"], "B": ["lock n", "lock m", "y ← y - 3", "x ← x + 3", "unlock m", "unlock n"]}}'></div>

_task(deadlock)

<!-- after: deadlock -->
Hält jeder Thread einen Lock und wartet auf den des anderen, kommt keiner mehr weiter: ein **Deadlock**. Es hilft, wenn alle Threads ihre Locks in derselben Reihenfolge nehmen, hier immer erst `m`, dann `n`. Dann kann der zweite gar nicht erst einen Lock halten, auf den der erste wartet.
<!-- /after -->
