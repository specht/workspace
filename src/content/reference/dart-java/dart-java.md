<div class='meta'>
kind: reference
search: Dart, Java, print, IO.println, System.out.println, List, ArrayList, List.of, Map, HashMap, LinkedHashMap, Set, String, equals, int, Integer, double, null, Null Safety, record, class, extends, implements, interface, abstract, toString, Lambda, stream, javac, jshell, dart run, Exception, NullPointerException, cannot find symbol, Fehlermeldung
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Dart und Java nebeneinander

<p class='abstract'>
Manche Aktivitäten lassen dir die Wahl zwischen Dart und Java, etwa das <a href='/textadventure'>Textadventure</a>, die <a href='/check-digits'>Prüfziffern</a> und die Katas zu <a href='/kata-lists'>Listen</a>, <a href='/kata-dicts'>Dictionaries</a>, <a href='/kata-recursion'>Rekursion</a> und <a href='/kata-objects'>Objekten</a>. Hier steht dasselbe in beiden Sprachen nebeneinander: zum Nachschlagen in der eigenen, und um zu sehen, wie es in der anderen aussieht. Wie man ein Programm überhaupt schreibt und startet, zeigen die Aktivitäten <a href='/dart'>Dart</a> und <a href='/java'>Java</a>.
</p>

Java kam 1995 heraus, Dart 2011, und Dart sieht Java mit Absicht ähnlich: geschweifte Klammern um jeden Block, ein Semikolon am Ende jeder Anweisung, Klassen mit `extends`. Anders als in Ruby und Python hat in beiden jede Variable einen **Typ**, und ein Compiler prüft ihn, bevor das Programm läuft: Wer einer Zahl einen Text zuweist, bekommt eine Fehlermeldung, ohne dass eine Zeile ausgeführt wird. Am meisten unterscheiden sich die beiden darin, wie sie mit `null` umgehen: In Dart darf eine Variable nur dann `null` sein, wenn ihr Typ es mit einem `?` erlaubt.

_person(gosling)

_person(bak-lund)

## Ein Programm

<!-- part: dj-programm -->
<!-- short: dart run prog.dart und java Prog.java; main; print und IO.println; Blöcke mit { } und ; in beiden. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>starten</th><td><code>dart run prog.dart</code></td><td><code>java Prog.java</code> übersetzt und startet in einem Schritt; oder erst <code>javac Prog.java</code>, dann <code>java Prog</code></td></tr>
<tr><th>wo das Programm beginnt</th><td><code>void main() {</code> … <code>}</code></td><td><code>void main() {</code> … <code>}</code>; vor Java 25 nur in einer Klasse: <code>public static void main(String[] args)</code></td></tr>
<tr><th>ausprobieren, Zeile für Zeile</th><td>gibt es nicht; ein kleines Programm mit <code>main</code></td><td><code>jshell</code>, beenden mit <code>/exit</code>; <code>/open Prog.java</code> lädt eine Datei</td></tr>
<tr><th>ausgeben</th><td><code>print(x)</code></td><td><code>IO.println(x)</code>; vor Java 25 <code>System.out.println(x)</code></td></tr>
<tr><th>ohne Zeilenende ausgeben</th><td><code>import 'dart:io';</code>, dann <code>stdout.write(x)</code></td><td><code>IO.print(x)</code>, <code>System.out.print(x)</code></td></tr>
<tr><th>einlesen</th><td><code>stdin.readLineSync()</code> aus <code>dart:io</code>, ein <code>String?</code></td><td><code>IO.readln()</code>; vor Java 25 <code>new Scanner(System.in).nextLine()</code></td></tr>
<tr><th>Kommentar</th><td colspan='2'><code>// bis zum Zeilenende</code>, <code>/* über mehrere Zeilen */</code>, in beiden gleich</td></tr>
<tr><th>Argumente beim Start</th><td><code>void main(List&lt;String&gt; args)</code>, dann ist <code>args[0]</code> das erste</td><td><code>void main(String[] args)</code>, dann ist <code>args[0]</code> das erste</td></tr>
<tr><th>eine andere Datei benutzen</th><td><code>import 'graph.dart';</code></td><td>Klassen in anderen Dateien desselben Ordners findet Java selbst; aus der Bibliothek: <code>import java.util.List;</code></td></tr>
<tr><th>mit einer Meldung abbrechen</th><td><code>stderr.writeln('Datei fehlt');</code> und <code>exit(1);</code> aus <code>dart:io</code></td><td><code>System.err.println("Datei fehlt");</code> und <code>System.exit(1);</code></td></tr>
</tbody>
</table>
</div>

Einen Block fassen beide in geschweifte Klammern; die Einrückung ist nur für Menschen. Nach `if`, `while` und `for` steht die Bedingung in runden Klammern, und jede Anweisung endet mit einem Semikolon.

<!-- language: dart -->
```dart
void main() {
  final count = 5;
  if (count > 3) {
    print('viele');
  } else {
    print('wenige');
  }
}
```
<!-- /language -->
<!-- language: java -->
```java
void main() {
    int count = 5;
    if (count > 3) {
        IO.println("viele");
    } else {
        IO.println("wenige");
    }
}
```
<!-- /language -->

Dart rückt üblicherweise um zwei Leerzeichen ein, Java um vier. `&&`, `||` und `!` heißen in beiden gleich, ebenso `else if`.
<!-- /part -->

## Typen und Variablen

<!-- part: dj-typen -->
<!-- short: var und final in Dart, int und var in Java; int, double, bool und boolean, String; int? in Dart; int und Integer in Java. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>eine Variable mit Typ</th><td><code>int count = 0;</code></td><td><code>int count = 0;</code></td></tr>
<tr><th>den Typ selbst finden lassen</th><td><code>var count = 0;</code></td><td><code>var count = 0;</code>, nur in Methoden</td></tr>
<tr><th>einmal setzen, dann nicht mehr</th><td><code>final name = 'Ada';</code>; <code>const</code> für Werte, die schon beim Übersetzen feststehen</td><td><code>final String name = "Ada";</code></td></tr>
<tr><th>ganze Zahlen, Kommazahlen</th><td><code>int</code> (64 Bit), <code>double</code></td><td><code>int</code> (32 Bit, bis 2 147 483 647), <code>long</code> (64 Bit, mit <code>L</code>: <code>20L</code>), <code>double</code></td></tr>
<tr><th>wahr, falsch</th><td><code>bool</code>: <code>true</code>, <code>false</code></td><td><code>boolean</code>: <code>true</code>, <code>false</code></td></tr>
<tr><th>Text, ein Zeichen</th><td><code>String</code> in <code>'…'</code> oder <code>"…"</code>; ein Zeichen ist ein String der Länge 1</td><td><code>String</code> in <code>"…"</code>; ein Zeichen ist ein <code>char</code> in <code>'…'</code></td></tr>
<tr><th>nichts</th><td><code>null</code>, nur bei einem Typ mit <code>?</code>: <code>int? found = null;</code></td><td><code>null</code>, bei jedem Objekt, aber nicht bei <code>int</code>, <code>double</code>, <code>boolean</code>, <code>char</code></td></tr>
<tr><th>eine Zahl als Objekt, etwa in einer Liste</th><td><code>int</code> ist schon ein Objekt: <code>List&lt;int&gt;</code></td><td><code>Integer</code>, <code>Double</code>, <code>Boolean</code>, <code>Character</code>: <code>List&lt;Integer&gt;</code></td></tr>
</tbody>
</table>
</div>

**Was als falsch gilt**: In beiden nur `false`. `if (count)` mit einer Zahl oder `if (list)` mit einer Liste übersetzt der Compiler gar nicht; man schreibt `if (count != 0)` und `if (list.isNotEmpty)` bzw. `if (!list.isEmpty())`.

**`null` in Dart**: Ein `String` ist nie `null`, ein `String?` vielleicht. Bevor man ein `String?` benutzt, fragt man nach (`if (s != null)`), nimmt mit `s ?? 'leer'` einen Ersatz, oder sagt mit `s!`, dass es sicher nicht `null` ist; stimmt das nicht, bricht das Programm mit `Null check operator used on a null value` ab. In Java kann jedes Objekt `null` sein, und der Fehler zeigt sich erst beim Ausführen, als `NullPointerException`.
<!-- /part -->

## Zahlen

<!-- part: dj-zahlen -->
<!-- short: 7 / 2 ist in Dart 3.5, in Java 3; ~/ in Dart; -7 % 3 ist 2 und -1; int.parse und Integer.parseInt; Text mit $ und +. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th><code>7 / 2</code></th><td><strong>3.5</strong>: <code>/</code> gibt immer ein <code>double</code></td><td><strong>3</strong>: zwei <code>int</code> ergeben ein <code>int</code>, der Rest fällt weg</td></tr>
<tr><th>ganzzahlig teilen</th><td><code>7 ~/ 2</code> → 3</td><td><code>7 / 2</code> → 3</td></tr>
<tr><th>mit Komma teilen</th><td><code>7 / 2</code> → 3.5</td><td><code>7.0 / 2</code> oder <code>(double) 7 / 2</code> → 3.5</td></tr>
<tr><th>Rest</th><td><code>7 % 3</code> → 1; <code>-7 % 3</code> → <strong>2</strong>, nie negativ</td><td><code>7 % 3</code> → 1; <code>-7 % 3</code> → <strong>−1</strong>, mit dem Vorzeichen der linken Zahl</td></tr>
<tr><th>Potenz, Wurzel</th><td><code>import 'dart:math';</code>, dann <code>pow(2, 10)</code>, <code>sqrt(2)</code></td><td><code>Math.pow(2, 10)</code> → 1024.0, ein <code>double</code>; <code>Math.sqrt(2)</code></td></tr>
<tr><th>zu groß</th><td>über 9 223 372 036 854 775 807 läuft ein <code>int</code> über</td><td>über 2 147 483 647 läuft ein <code>int</code> über: <code>Integer.MAX_VALUE + 1</code> ist −2 147 483 648</td></tr>
<tr><th>Text zu Zahl</th><td><code>int.parse('42')</code>, <code>double.parse('3.5')</code>; <code>int.parse('x')</code> ist eine <code>FormatException</code>, <code>int.tryParse('x')</code> ist <code>null</code></td><td><code>Integer.parseInt("42")</code>, <code>Double.parseDouble("3.5")</code>; bei <code>"x"</code> eine <code>NumberFormatException</code></td></tr>
<tr><th>Zahl zu Text</th><td><code>42.toString()</code></td><td><code>String.valueOf(42)</code>, oder <code>"" + 42</code></td></tr>
<tr><th>Werte in Text einsetzen</th><td><code>'$name hat $count'</code>, Rechnungen in <code>${…}</code>: <code>'${a + b}'</code></td><td><code>name + " hat " + count</code>, oder <code>"%s hat %d".formatted(name, count)</code></td></tr>
<tr><th>auf zwei Stellen</th><td><code>x.toStringAsFixed(2)</code></td><td><code>String.format("%.2f", x)</code>, mit Komma oder Punkt je nach Spracheinstellung des Rechners</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Text

<!-- part: dj-text -->
<!-- short: length und length(), s[0] und charAt(0), substring in beiden, == und equals, split und join, ' * 3' und repeat. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>Länge</th><td><code>s.length</code></td><td><code>s.length()</code></td></tr>
<tr><th>ein Zeichen, das letzte</th><td><code>s[0]</code>, <code>s[s.length - 1]</code>, jeweils ein String</td><td><code>s.charAt(0)</code>, <code>s.charAt(s.length() - 1)</code>, jeweils ein <code>char</code></td></tr>
<tr><th>ein Stück</th><td colspan='2'><code>s.substring(1, 4)</code>: ab 1 bis vor 4; <code>s.substring(1)</code>: ab 1 bis zum Ende; in beiden gleich</td></tr>
<tr><th>gleich?</th><td><code>a == b</code></td><td><code>a.equals(b)</code>; <code>a == b</code> fragt, ob es <em>dasselbe Objekt</em> ist</td></tr>
<tr><th>enthält, beginnt, endet</th><td><code>s.contains('ll')</code>, <code>s.startsWith('Ha')</code>, <code>s.endsWith('o')</code></td><td><code>s.contains("ll")</code>, <code>s.startsWith("Ha")</code>, <code>s.endsWith("o")</code></td></tr>
<tr><th>groß, klein</th><td colspan='2'><code>s.toUpperCase()</code>, <code>s.toLowerCase()</code>, in beiden gleich</td></tr>
<tr><th>Leerraum weg</th><td colspan='2'><code>s.trim()</code>, in beiden gleich</td></tr>
<tr><th>teilen, zusammenfügen</th><td><code>'a,b'.split(',')</code> gibt eine Liste, <code>list.join(',')</code></td><td><code>"a,b".split(",")</code> gibt ein Array, <code>String.join(",", list)</code></td></tr>
<tr><th>Zeichen für Zeichen</th><td><code>for (final c in s.split('')) {</code> … <code>}</code></td><td><code>for (char c : s.toCharArray()) {</code> … <code>}</code></td></tr>
<tr><th>wiederholen</th><td><code>'ab' * 3</code> → <code>'ababab'</code></td><td><code>"ab".repeat(3)</code> → <code>"ababab"</code></td></tr>
<tr><th>vorn auffüllen</th><td><code>'7'.padLeft(3, '0')</code> → <code>'007'</code></td><td><code>String.format("%03d", 7)</code> → <code>"007"</code></td></tr>
<tr><th>Nummer eines Zeichens und zurück</th><td><code>'A'.codeUnitAt(0)</code> → 65, <code>String.fromCharCode(65)</code></td><td><code>(int) 'A'</code> → 65, <code>(char) 65</code></td></tr>
<tr><th>mit einem regulären Ausdruck</th><td><code>RegExp(r'\d+').allMatches(s)</code>, <code>s.contains(RegExp(r'\d'))</code></td><td><code>Pattern.compile("\\d+").matcher(s)</code>, <code>s.matches("\\d+")</code> für den ganzen Text</td></tr>
</tbody>
</table>
</div>

In beiden lässt sich ein String nicht verändern: `s.toUpperCase()` gibt einen neuen zurück und ändert `s` nicht. Wer einen langen Text Stück für Stück baut, nimmt in Dart einen `StringBuffer`, in Java einen `StringBuilder`.
<!-- /part -->

## Listen

<!-- part: dj-listen -->
<!-- short: `List<int>` und `List<Integer>`; [3, 1, 2] und List.of; add, removeLast, a[i] und get(i); map und where, stream().map und filter; sort. -->
Dart schreibt eine Liste wie Ruby und Python, `[3, 1, 2]`, eine `List<int>`. Java legt mit `List.of(3, 1, 2)` eine Liste an, die sich nicht mehr ändern lässt, und mit `new ArrayList<>(List.of(3, 1, 2))` eine, die man ändern kann. Daneben hat Java **Arrays** fester Länge, `int[] a = {3, 1, 2};`, mit `a[0]` und `a.length`.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>eine leere</th><td><code>final a = &lt;int&gt;[];</code></td><td><code>List&lt;Integer&gt; a = new ArrayList&lt;&gt;();</code></td></tr>
<tr><th>Länge, enthält</th><td><code>a.length</code>, <code>a.contains(2)</code></td><td><code>a.size()</code>, <code>a.contains(2)</code></td></tr>
<tr><th>ein Element lesen, ändern</th><td><code>a[i]</code>, <code>a[i] = x</code></td><td><code>a.get(i)</code>, <code>a.set(i, x)</code></td></tr>
<tr><th>hinten anhängen, wegnehmen</th><td><code>a.add(4)</code>, <code>a.removeLast()</code></td><td><code>a.add(4)</code>, <code>a.removeLast()</code></td></tr>
<tr><th>vorn wegnehmen</th><td><code>a.removeAt(0)</code></td><td><code>a.removeFirst()</code></td></tr>
<tr><th>das erste, das letzte, ab dem zweiten</th><td><code>a.first</code>, <code>a.last</code>, <code>a.sublist(1)</code></td><td><code>a.getFirst()</code>, <code>a.getLast()</code>, <code>a.subList(1, a.size())</code></td></tr>
<tr><th>der Reihe nach</th><td><code>for (final x in a) {</code> … <code>}</code></td><td><code>for (int x : a) {</code> … <code>}</code></td></tr>
<tr><th>mit Index</th><td><code>for (var i = 0; i &lt; a.length; i++)</code></td><td><code>for (int i = 0; i &lt; a.size(); i++)</code></td></tr>
<tr><th>jedes umrechnen</th><td><code>a.map((x) =&gt; x * 2).toList()</code></td><td><code>a.stream().map(x -&gt; x * 2).toList()</code></td></tr>
<tr><th>nur manche</th><td><code>a.where((x) =&gt; x &gt; 1).toList()</code></td><td><code>a.stream().filter(x -&gt; x &gt; 1).toList()</code></td></tr>
<tr><th>Summe, Minimum, Maximum</th><td><code>a.fold(0, (s, x) =&gt; s + x)</code>; mit <code>dart:math</code> <code>a.reduce(min)</code>, <code>a.reduce(max)</code></td><td><code>a.stream().mapToInt(x -&gt; x).sum()</code>, <code>Collections.min(a)</code>, <code>Collections.max(a)</code></td></tr>
<tr><th>sortieren</th><td><code>a.sort()</code> sortiert <code>a</code> selbst und gibt nichts zurück; absteigend <code>a.sort((x, y) =&gt; y.compareTo(x))</code></td><td><code>Collections.sort(a)</code> sortiert <code>a</code> selbst; absteigend <code>a.sort(Comparator.reverseOrder())</code>; eine sortierte Kopie: <code>a.stream().sorted().toList()</code></td></tr>
<tr><th>gleich?</th><td><code>[1, 2] == [1, 2]</code> ist <strong>false</strong>: <code>==</code> fragt, ob es dieselbe Liste ist</td><td><code>a.equals(b)</code> vergleicht die Elemente</td></tr>
</tbody>
</table>
</div>

Für Paare und feste Gruppen hat Dart **Records**: `(3, 'B')` vom Typ `(int, String)`, ausgepackt mit `final (count, letter) = pair;`. In Java schreibt man dafür eine kleine Klasse in einer Zeile, `record Pair(int count, String letter) {}`, oder nimmt für zwei Werte `Map.entry(3, "B")`.
<!-- /part -->

## Maps und Mengen

<!-- part: dj-maps -->
<!-- short: Map in beiden: {'car': 50} und Map.of / HashMap; m[k] ?? 0 und getOrDefault; containsKey; Reihenfolge in Dart ja, in HashMap nein; Set. -->
Was zu einem Schlüssel gehört, steht in beiden in einer **Map**.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>anlegen</th><td><code>{'car': 50, 'bike': 15}</code>, eine <code>Map&lt;String, int&gt;</code>; leer: <code>&lt;String, int&gt;{}</code></td><td><code>Map.of("car", 50, "bike", 15)</code>, unveränderlich; zum Ändern <code>new HashMap&lt;&gt;()</code></td></tr>
<tr><th>nachschlagen</th><td><code>m['car']</code>; fehlt der Schlüssel, <code>null</code></td><td><code>m.get("car")</code>; fehlt der Schlüssel, <code>null</code></td></tr>
<tr><th>mit einer Vorgabe, wenn er fehlt</th><td><code>m['bus'] ?? 0</code></td><td><code>m.getOrDefault("bus", 0)</code></td></tr>
<tr><th>gibt es den Schlüssel?</th><td><code>m.containsKey('car')</code></td><td><code>m.containsKey("car")</code></td></tr>
<tr><th>eintragen, entfernen</th><td><code>m['bus'] = 30</code>, <code>m.remove('bus')</code></td><td><code>m.put("bus", 30)</code>, <code>m.remove("bus")</code></td></tr>
<tr><th>alle durchgehen</th><td><code>for (final key in m.keys)</code>, oder <code>m.forEach((key, value) {</code> … <code>});</code></td><td><code>for (String key : m.keySet())</code>, oder <code>for (var e : m.entrySet())</code> mit <code>e.getKey()</code>, <code>e.getValue()</code></td></tr>
<tr><th>nur Schlüssel, nur Werte</th><td><code>m.keys</code>, <code>m.values</code></td><td><code>m.keySet()</code>, <code>m.values()</code></td></tr>
<tr><th>zählen</th><td><code>counts[x] = (counts[x] ?? 0) + 1;</code></td><td><code>counts.put(x, counts.getOrDefault(x, 0) + 1);</code>, oder <code>counts.merge(x, 1, Integer::sum);</code></td></tr>
<tr><th>zu jedem Schlüssel eine Liste</th><td><code>m.putIfAbsent(id, () =&gt; []).add(x);</code></td><td><code>m.computeIfAbsent(id, k -&gt; new ArrayList&lt;&gt;()).add(x);</code></td></tr>
<tr><th>Reihenfolge</th><td>eine Map behält die Reihenfolge, in der die Schlüssel hineinkamen</td><td>eine <code>HashMap</code> nicht; eine <code>LinkedHashMap</code> behält sie, eine <code>TreeMap</code> sortiert die Schlüssel</td></tr>
</tbody>
</table>
</div>

Eine **Menge** enthält jeden Wert höchstens einmal: in Dart `{'q0', 'q1'}`, ein `Set<String>` (eine leere ist `<String>{}`, denn `{}` allein ist eine leere Map), in Java `Set.of("q0", "q1")` oder `new HashSet<>()`. Dart rechnet mit `a.union(b)` und `a.intersection(b)` neue Mengen aus; Java ändert mit `a.addAll(b)` und `a.retainAll(b)` die Menge `a` selbst. `contains(x)` fragt in beiden.
<!-- /part -->

## Funktionen

<!-- part: dj-funktionen -->
<!-- short: Typ vor dem Namen in beiden; => in Dart; optionale Parameter in Dart, Überladen in Java; (x) => und x ->. -->
<!-- language: dart -->
```dart
import 'dart:math';

double distance(List<double> a, List<double> b, [double scale = 1]) {
  return sqrt(pow(a[0] - b[0], 2) + pow(a[1] - b[1], 2)) * scale;
}
```
<!-- /language -->
<!-- language: java -->
```java
double distance(double[] a, double[] b, double scale) {
    return Math.hypot(a[0] - b[0], a[1] - b[1]) * scale;
}

double distance(double[] a, double[] b) {
    return distance(a, b, 1);
}
```
<!-- /language -->

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>Rückgabe</th><td colspan='2'>nur mit <code>return</code>; der Typ vor dem Namen sagt, was zurückkommt, <code>void</code> heißt nichts; in beiden gleich</td></tr>
<tr><th>kurz, in einer Zeile</th><td><code>int twice(int n) =&gt; 2 * n;</code></td><td>gibt es nicht</td></tr>
<tr><th>ein Parameter, den man weglassen darf</th><td>in eckigen Klammern mit Vorgabe: <code>[double scale = 1]</code>; mit Namen: <code>{int scale = 1}</code>, aufgerufen mit <code>scale: 2</code></td><td>gibt es nicht; eine zweite Methode mit weniger Parametern, die die erste aufruft (<strong>Überladen</strong>)</td></tr>
<tr><th>zwei Werte zurückgeben</th><td>ein Record: <code>return (a, b);</code>, ausgepackt mit <code>final (x, y) = f();</code></td><td>ein <code>record</code>, ein Array oder ein Objekt</td></tr>
<tr><th>eine kleine Funktion als Wert</th><td><code>final square = (int x) =&gt; x * x;</code>, aufrufen mit <code>square(4)</code></td><td><code>Function&lt;Integer, Integer&gt; square = x -&gt; x * x;</code>, aufrufen mit <code>square.apply(4)</code></td></tr>
<tr><th>ein Stück Code mitgeben</th><td><code>a.forEach((x) {</code> … <code>});</code></td><td><code>a.forEach(x -&gt; {</code> … <code>});</code></td></tr>
<tr><th>wo Funktionen stehen</th><td>überall in der Datei, auch außerhalb von Klassen</td><td>als Methoden in einer Klasse; in einem Programm mit <code>void main()</code> ohne Klasse drumherum auch direkt in der Datei</td></tr>
</tbody>
</table>
</div>

In Java darf ein Lambda (`x -> …`) eine Variable der Methode, in der es steht, lesen, aber nicht ändern; der Compiler sagt dann `local variables referenced from a lambda expression must be final or effectively final`. Dart erlaubt es.
<!-- /part -->

## Klassen

<!-- part: dj-klassen -->
<!-- short: Konstruktor Point(this.x, this.y) in Dart; private mit _ und private; toString mit @override und public; extends und super in beiden; abstract, interface und implements. -->
<!-- language: dart -->
```dart
class Point {
  final int x;
  final int y;

  Point(this.x, this.y);

  @override
  String toString() => '($x, $y)';
}
```
<!-- /language -->
<!-- language: java -->
```java
class Point {
    private final int x;
    private final int y;

    Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }
}
```
<!-- /language -->

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>ein Objekt bauen</th><td><code>Point(3, 4)</code>; ein <code>new</code> davor geht, ist aber unüblich</td><td><code>new Point(3, 4)</code></td></tr>
<tr><th>Attribute im Konstruktor setzen</th><td><code>Point(this.x, this.y);</code></td><td><code>this.x = x;</code> im Rumpf des Konstruktors</td></tr>
<tr><th>privat</th><td>ein Unterstrich vorn: <code>_money</code>, privat in dieser Datei</td><td><code>private int money;</code>, privat in dieser Klasse</td></tr>
<tr><th>wie es sich als Text zeigt</th><td><code>@override String toString()</code>; sonst <code>Instance of 'Point'</code></td><td><code>@Override public String toString()</code>; sonst etwa <code>Point@1b6d3586</code></td></tr>
<tr><th>erben</th><td colspan='2'><code>class Dog extends Animal</code>, in beiden gleich</td></tr>
<tr><th>den Konstruktor der Oberklasse aufrufen</th><td><code>Dog(String name) : super(name);</code>, kurz <code>Dog(super.name);</code></td><td><code>super(name);</code> in der ersten Zeile des Konstruktors</td></tr>
<tr><th>die Methode der Oberklasse aufrufen</th><td colspan='2'><code>super.sound()</code>, in beiden gleich</td></tr>
<tr><th>eine Methode, die erst Unterklassen ausfüllen</th><td><code>abstract class Shape { int area(); }</code></td><td><code>abstract class Shape { abstract int area(); }</code></td></tr>
<tr><th>Schnittstelle</th><td>jede Klasse ist eine; <code>class Square implements Shape</code></td><td><code>interface Shape { int area(); }</code>, dann <code>class Square implements Shape</code>, und <code>area</code> muss <code>public</code> sein</td></tr>
<tr><th>ist es ein …?</th><td><code>if (shape is Square)</code></td><td><code>if (shape instanceof Square square)</code></td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Dateien und Bytes

<!-- part: dj-dateien -->
<!-- short: File('a.txt').readAsStringSync() und Files.readString(Path.of(…)); throws IOException in Java; Bytes in Dart 0 bis 255, in Java −128 bis 127. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart, mit <code>import 'dart:io';</code></th><th>Java, mit <code>import java.nio.file.*;</code></th></tr></thead>
<tbody>
<tr><th>eine Textdatei lesen</th><td><code>File('a.txt').readAsStringSync()</code></td><td><code>Files.readString(Path.of("a.txt"))</code></td></tr>
<tr><th>Zeile für Zeile</th><td><code>File('a.txt').readAsLinesSync()</code></td><td><code>Files.readAllLines(Path.of("a.txt"))</code></td></tr>
<tr><th>schreiben</th><td><code>File('a.txt').writeAsStringSync(text)</code></td><td><code>Files.writeString(Path.of("a.txt"), text)</code></td></tr>
<tr><th>gibt es die Datei?</th><td><code>File('a.txt').existsSync()</code></td><td><code>Files.exists(Path.of("a.txt"))</code></td></tr>
<tr><th>eine Datei als Bytes lesen</th><td><code>File('bild.gif').readAsBytesSync()</code>, Zahlen von 0 bis 255</td><td><code>Files.readAllBytes(Path.of("bild.gif"))</code>, ein <code>byte[]</code> mit Zahlen von −128 bis 127; <code>b &amp; 0xff</code> macht daraus 0 bis 255</td></tr>
<tr><th>Text zu Bytes und zurück</th><td><code>import 'dart:convert';</code>, dann <code>utf8.encode(text)</code>, zurück <code>utf8.decode(bytes)</code></td><td><code>text.getBytes(StandardCharsets.UTF_8)</code>, zurück <code>new String(bytes, StandardCharsets.UTF_8)</code></td></tr>
</tbody>
</table>
</div>

Java verlangt, dass eine Methode, die eine Datei liest, das sagt: `void main() throws IOException`. Sonst übersetzt der Compiler sie nicht und meldet `unreported exception IOException; must be caught or declared to be thrown`. Dart kennt diese Pflicht nicht.
<!-- /part -->

## Fehler

<!-- part: dj-fehler -->
<!-- short: throw und throw new; try/on/catch und try/catch; Meldungen des Compilers: Expected ';' und ';' expected, Undefined name und cannot find symbol; zur Laufzeit Null check und NullPointerException. -->
<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Was</th><th>Dart</th><th>Java</th></tr></thead>
<tbody>
<tr><th>einen Fehler melden</th><td><code>throw ArgumentError('leer');</code></td><td><code>throw new IllegalArgumentException("leer");</code></td></tr>
<tr><th>einen Fehler abfangen</th><td><code>try {</code> … <code>} on FormatException catch (e) {</code> … <code>} finally {</code> … <code>}</code></td><td><code>try {</code> … <code>} catch (NumberFormatException e) {</code> … <code>} finally {</code> … <code>}</code></td></tr>
</tbody>
</table>
</div>

Die meisten Fehler meldet in beiden schon der Compiler, mit Datei, Zeile und einem Zeiger `^` unter der Stelle; das Programm läuft dann gar nicht erst. Häufige Meldungen:

- **Ein Semikolon fehlt**: Dart `Error: Expected ';' after this.`, Java `error: ';' expected`. Der Zeiger steht hinter der Zeile, der es fehlt.
- **Ein Name, den es nicht gibt**: Dart `Error: Undefined name 'x'.`, Java `error: cannot find symbol` mit `symbol: variable x` darunter. Vertippt, oder die Variable steht in einer anderen Methode. Bei einer Methode heißt es in Dart `The method 'x' isn't defined for the type …`.
- **Der falsche Typ**: Dart `Error: A value of type 'String' can't be assigned to a variable of type 'int'.`, Java `error: incompatible types: String cannot be converted to int`. Erst umwandeln, mit `int.parse(…)` bzw. `Integer.parseInt(…)`.
- **Es fehlt ein `return`**: Dart `Error: A non-null value must be returned since the return type 'int' doesn't allow null.`, Java `error: missing return statement`. Oft gibt die Methode nur in einem Zweig eines `if` etwas zurück.
- **Vielleicht `null`**: Dart prüft es beim Übersetzen: `Property 'length' cannot be accessed on 'String?' because it is potentially null.` Java merkt es erst beim Ausführen: `NullPointerException: Cannot invoke "String.length()" because … is null`.

Beim Ausführen:

- **Ein Index, den es nicht gibt**: Dart `RangeError (length): Invalid value: Not in inclusive range 0..2: 3`, Java `Index 3 out of bounds for length 3`. Die Indizes einer Liste mit drei Elementen sind 0, 1 und 2.
- **Durch 0 geteilt**: Dart `IntegerDivisionByZeroException` bei `~/`, Java `ArithmeticException: / by zero`. Mit Kommazahlen gibt es keinen Fehler, sondern `Infinity`.
- **Etwas ist `null`**: Dart `Null check operator used on a null value`, nach einem `!`. Java `NullPointerException`.
- **Kein Ende**: Eine Funktion ruft sich selbst ohne Ende auf: Dart `Stack Overflow`, Java `StackOverflowError`.
<!-- /part -->

<!-- part: dj-fallen -->
<!-- short: 7 / 2 ist in Java 3; Strings in Java mit equals; Integer über 127 nicht mit ==; [1] == [1] ist in Dart false; List.of ist unveränderlich; remove(1) nimmt den Index. -->
### Fallen beim Wechseln

- **Teilen**: In Java ergibt `7 / 2` eine 3, in Dart 3.5. Ganzzahlig teilt Dart mit `~/`.
- **Texte vergleichen**: In Java fragt `a == b` bei zwei Strings, ob es dasselbe Objekt ist. Zwei gleiche Texte, die verschieden entstanden sind, sind dann nicht gleich; `a.equals(b)` vergleicht die Zeichen. In Dart vergleicht `==` die Zeichen.
- **Zahlen in Listen**: Auch zwei `Integer` vergleicht Java mit `==` als Objekte. Bis 127 geht es zufällig gut, `list.get(0) == list.get(1)` ist bei zweimal 1000 aber `false`. Sicher sind `equals` oder ein `int` auf einer Seite.
- **Listen vergleichen**: In Dart ist `[1, 2] == [1, 2]` `false`, denn `==` fragt bei Listen, ob es dieselbe ist. Java vergleicht mit `equals` die Elemente.
- **Unveränderliche Listen**: `List.of(…)` und `Map.of(…)` lassen sich in Java nicht ändern; `add` und `put` werfen dann eine `UnsupportedOperationException`. Zum Ändern: `new ArrayList<>(List.of(…))`.
- **Index oder Wert**: `list.remove(1)` nimmt in einer `List<Integer>` das Element mit dem Index 1 heraus, nicht die Zahl 1. Die Zahl entfernt `list.remove(Integer.valueOf(1))`.
- **Zeichen und Text**: In Java ist `'a'` ein `char` und `"a"` ein String; `'a' + 'b'` ist die Zahl 195, `"a" + "b"` der Text `"ab"`. In Dart sind `'a'` und `"a"` dasselbe.
- **split**: In Java ist das Argument von `split` ein regulärer Ausdruck. `"a.b".split(".")` gibt ein leeres Array, denn `.` passt auf jedes Zeichen; richtig ist `split("\\.")`.
- **sort**: In Dart gibt `a.sort()` nichts zurück, sondern sortiert `a` selbst. Nach `final sorted = a.sort();` ist `sorted` vom Typ `void`, und wer es benutzt, liest `This expression has type 'void' and can't be used.` Eine sortierte Kopie gibt `[...a]..sort()`.
<!-- /part -->

## Zum Nachschlagen

- [Dart-Dokumentation](https://dart.dev/language): die Sprache Schritt für Schritt, auf Englisch; die [Bibliothek](https://api.dart.dev/) mit allen Klassen, `String`, `List`, `Map` und so weiter.
- [Java-Dokumentation](https://docs.oracle.com/en/java/javase/25/docs/api/): alle Klassen der Bibliothek, `String`, `List`, `Map` und so weiter, auf Englisch; das [Tutorial](https://dev.java/learn/) erklärt die Sprache.
- [Dart für Java-Programmierer](https://dart.dev/resources/coming-from/java-to-dart): was in Dart anders ist, für alle, die Java können, auf Englisch.
