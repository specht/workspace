# Plan: concurrency in Java and Dart

An activity on what happens when two parts of a program run at the same
time: lost updates, locks, deadlock, threads that hand work to each other,
and Dart's different model. Planned on 29 October 2026; nothing is built.
»Decided« marks what the maintainer said; the rest is a proposal to show the
maintainer before building.

## Decided

- **Java and Dart, not Ruby or Python.** Schools teach this topic in Java
  (and object orientation too); the maintainer's school uses Dart. Ruby and
  Python have a global interpreter lock: a lost update shows up rarely and
  unpredictably there, so a »broken« program often looks right, which
  teaches the wrong lesson.
- **The order of the work:** Textadventure, Prüfziffern and Laufzeit in Dart
  and Java first (done), then this activity.

## What exists

- »Ein eigener Webserver« (`webserver`, Ruby and Python) gives each
  connection a thread and protects the visit counter with a Mutex or Lock
  (`webserver.md`, »Unter `PUBLIC_DIR` kommt der Zähler, mit einem Lock«),
  with the prediction »Zwei auf einmal« (`mutex` in its `tasks.yaml`): two
  threads read 10, both write 11.
- »Eine Pixelwand für alle« (`pixelwand`) has a chapter on threads and a
  lock.
- Students meet the idea in passing there; nothing makes them work it out.
- The checks `java` and `dart` (docs/aufgaben.md, »java, dart«) call the
  student's code in their Workspace, which has 4 CPUs
  (`workspace_runtime.rb`), so Java threads really run side by side there.
- Berlin-Brandenburg does not name the topic. The nearest names are
  »Betriebssysteme« (Einführungsphase, »Grundlagen informatischen
  Arbeitens«, docs/lehrplan.md) and client and server in »Rechner und
  Netze«. Bavaria's Oberstufe had »Kommunikation und Synchronisation von
  Prozessen« (producer and consumer, monitors, deadlock); check the current
  plan before writing a `topics:` line.

## Proposed: the activity

About the size of »Wie lange dauert das?« (three Doppelstunden). A teacher
picks Java or Dart under »Sprachen der Klasse«; the parts below say what
each language shows.

### 1. A step-through widget, without code

Two threads side by side, each with the steps `lesen`, `+1`, `schreiben`
on a shared counter. The student picks which thread takes its next step and
watches the counter and each thread's own copy. The widget is deterministic,
so it can carry real tasks:

- »Find an order in which the counter ends at 1 instead of 2.«
- »How many orders of the six steps are there, and in how many is an update
  lost?« (20 orders, 2 of them correct: a `number` card.)
- With a lock: the steps `sperren` and `freigeben` added, and the widget
  refuses the order that lost an update. It shows what a mutex forbids.
- Two locks taken in opposite order: an order where neither thread can go
  on (deadlock), found by hand.

Built like the other step-through widgets (docs/widgets.md, the AVL tree is
the model). Open: how the widget hands in an order. Either a small check
type that receives the widget's state, as `circuit` receives the
Logik-Werkstatt's, or an `answer` card where the order is typed (»A1 B1 A2
B2 A3 B3«). The check type is more work and nicer to use.

### 2. Java: threads, locks, monitor

- **A counter that loses updates:** two threads, each `count++` a million
  times; the program prints less than two million, and a different number on
  each run. A prediction first (»Was gibt das Programm aus?«).
- **`synchronized`:** a method `increment` in a class `Counter`; the check
  calls a method of the student's that starts the threads, joins them and
  returns the count, several times, and the count must be exact every time.
- **Deadlock:** two accounts, two threads transferring in opposite
  directions, each taking the two locks in its own order. A prediction (»Wie
  viele Läufe bleiben hängen?«), then the fix: always take the locks in the
  same order (by the account's number). A hang shows up as running out of
  time; `slow:` words the message (»Nehmen beide Threads die Locks in
  derselben Reihenfolge?«).
- **The monitor:** producer and consumer through a bounded buffer with
  `synchronized`, `wait` and `notifyAll`; then the same with
  `ArrayBlockingQueue`, to show that the standard library already has it.
- **Extra:** `AtomicInteger`; why `volatile` is not enough for `count++`.

### 3. Dart: one thread, races at `await`

Dart has no threads that share memory, no mutex and no monitor in the
standard library, and a lost update between two threads cannot happen. The
race happens at `await` instead:

```dart
var balance = 0;

Future<void> deposit(int amount) async {
  final before = balance;
  await save();
  balance = before + amount;
}
```

Two calls of `deposit` that run together interleave exactly at the `await`,
and one deposit is lost. This is deterministic, so it can be shown, predicted
and checked reliably, unlike threads.

- **The race at `await`:** a prediction, then the fix (no `await` between
  reading and writing, or one `Future` after the other with `await` in a
  loop instead of `Future.wait`).
- **A lock for `async` code:** a small class `Lock` with a `Future` chain
  (`synchronized(() async {...})`), written by the student; the widget's
  lock in Dart.
- **Isolates:** separate workers that share nothing and only exchange
  messages: `Isolate.run` for a long calculation (the prime numbers up to
  ten million in four parts), the time on 1 and on 4 isolates measured, and
  why nothing can be lost there. The same idea as Go's channels.

### Checks

- Java: the existing `java` check. A broken version (no `synchronized`)
  loses updates only usually, never certainly, so it cannot be an example
  that must fail (`task-examples.yaml`); the examples that must fail are
  versions that are wrong every time (a missing `join`, a count that is not
  returned), and the lost update is a prediction and a measurement on the
  page. Deadlock examples must hang every time: a `CountDownLatch` or a short
  `sleep` between taking the two locks makes that certain.
- Dart: **the `dart` check does not await yet.** `DartCheck.harness` runs
  each case synchronously, so a call that returns a `Future` would come
  back as »Instance of 'Future<…>'«. Needed first: cases as `async`
  functions that `await` a result that is a `Future`, with a test in
  `test/java_dart_check_test.rb`.
- Time: a check that starts threads a million times needs `timeout:` a bit
  higher than the default; measure in a Workspace.

### Open, for the maintainer

- Where it sits: after the web server and Pixelwand in »Netze & Sicherheit«
  (both use threads), or in »Algorithmen und Datenstrukturen«, or a section
  of its own beside »Software entwerfen«. Then `curriculum-graph.rb`.
- The name: »Zwei auf einmal« (the web server's prediction), »Wer zuerst
  kommt«, or plainer, »Nebenläufigkeit: Threads, Locks und Isolates«.
- Whether a Ruby version is wanted after all (the widget, deadlock, `Queue`
  and `ConditionVariable` work in Ruby; the lost update only with a `sleep 0`
  between reading and writing, which is artificial).
- Person boxes: Edsger W. Dijkstra (semaphores, the dining philosophers),
  C. A. R. Hoare and Per Brinch Hansen (monitors), Tony Hoare's CSP as the
  idea behind channels; a `.book` box, for example Goetz, »Java Concurrency
  in Practice«.
