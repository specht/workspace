<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Pseudocode und Struktogramme

<p class='abstract'>
Wie du einen Algorithmus aufschreibst, bevor du ihn programmierst: als Pseudocode, der in keiner Programmiersprache geschrieben ist, und als Struktogramm, das ihn als Bild zeigt. Dazu, wie aus beidem Code in Ruby oder Python wird.
</p>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Flowchart Techniques for Structured Programming</strong>, Isaac Nassi und Ben Shneiderman, ACM SIGPLAN Notices, 1973. Der Aufsatz, in dem die Struktogramme zum ersten Mal vorkommen, als Ersatz für Flussdiagramme mit ihren Pfeilen kreuz und quer: <a href='https://doi.org/10.1145/953349.953350'>bei der ACM</a>. Darum heißen sie auch Nassi-Shneiderman-Diagramme.</p>
</div>

<script src='/include/struktogramm.js'></script>

<!-- part: ausprobieren -->
## Ausprobieren

Schreib Pseudocode ins Feld; daneben oder darunter erscheint das Struktogramm, während du tippst. Hier wird nichts gespeichert; »Als SVG speichern« legt das Bild als Datei ab, für ein Arbeitsblatt oder eine Abgabe.

<div class='struktogramm-playground'>
<textarea class='form-control struktogramm-playground-input' wrap='off' spellcheck='false' autocapitalize='off' aria-label='Pseudocode'>count_vowels(word):
    count ← 0
    for each letter in word:
        if letter in "aeiou":
            count ← count + 1
    return count</textarea>
<div class='struktogramm-playground-output'>
<div class='struktogramm-playground-problems' aria-live='polite'></div>
<div class='struktogramm-playground-diagram'></div>
<button type='button' class='btn btn-sm btn-outline-secondary struktogramm-playground-save'><i class='bi bi-download'></i> Als SVG speichern</button>
</div>
</div>

<script>
window.addEventListener('DOMContentLoaded', function () {
    var S = window.Struktogramm;
    var box = document.querySelector('.struktogramm-playground');
    if (!S || !box) return;
    var input = box.querySelector('.struktogramm-playground-input');
    var diagram = box.querySelector('.struktogramm-playground-diagram');
    var problems = box.querySelector('.struktogramm-playground-problems');
    var measure = S.canvasMeasure();
    function draw() {
        var result = S.svg(input.value, { measure: measure });
        problems.innerHTML = result.errors.map(function (p) {
            return '<div>Zeile ' + p.line + ': ' + S.esc(p.msg) + '</div>';
        }).join('');
        diagram.classList.toggle('struktogramm-playground-stale', !result.svg);
        if (result.svg) diagram.innerHTML = result.svg; // otherwise the last good one stays, faded
    }
    box.querySelector('.struktogramm-playground-save').addEventListener('click', function () {
        var result = S.svg(input.value, { measure: measure, background: '#ffffff' });
        if (!result.svg) return;
        var name = (result.diagrams[0] && result.diagrams[0].title || 'struktogramm').replace(/\(.*$/, '');
        var link = document.createElement('a');
        link.href = URL.createObjectURL(new Blob([result.svg], { type: 'image/svg+xml' }));
        link.download = name + '.svg';
        link.click();
        URL.revokeObjectURL(link.href);
    });
    input.addEventListener('input', draw);
    (document.fonts && document.fonts.ready ? document.fonts.ready : Promise.resolve()).then(draw);
});
</script>
<!-- /part -->

<!-- part: pseudocode -->
## Pseudocode

Pseudocode sieht aus wie ein Programm, ist aber für Menschen geschrieben: Er sagt genau, was in welcher Reihenfolge geschieht, und lässt weg, was nur eine bestimmte Sprache verlangt. Im Handbuch sind die Schlüsselwörter Englisch, wie in fast allen Programmiersprachen; Bemerkungen auf Deutsch stehen in Klammern.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Pseudocode</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>sum ← 0</code></td><td>eine Zuweisung: <code>sum</code> bekommt den Wert 0. Der Pfeil zeigt, wohin der Wert geht.</td></tr>
<tr><td><code>a == b</code>, <code>a != b</code>, <code>a &lt; b</code>, <code>a ≤ b</code></td><td>Vergleiche: gleich, ungleich, kleiner, kleiner oder gleich</td></tr>
<tr><td><code>and</code>, <code>or</code>, <code>not</code></td><td>Bedingungen verknüpfen</td></tr>
<tr><td><code>a mod b</code>, <code>a div b</code></td><td>der Rest und das ganzzahlige Ergebnis einer Division: <code>17 mod 5</code> ist 2, <code>17 div 5</code> ist 3</td></tr>
<tr><td><code>list[i]</code>, <code>length</code></td><td>das Element mit dem Index <code>i</code> (das erste hat den Index 0), die Zahl der Elemente</td></tr>
<tr><td><code>name(a, b):</code></td><td>eine Funktion mit ihren Parametern; eingerückt darunter, was sie tut</td></tr>
<tr><td><code>return x</code></td><td>die Funktion endet und gibt <code>x</code> zurück</td></tr>
</tbody>
</table></div>

Was zu einem `if`, einer Schleife oder einer Funktion gehört, steht eingerückt unter der Zeile mit dem Doppelpunkt. Eine einzelne Anweisung darf auch gleich hinter dem Doppelpunkt stehen: `if x < 0: x ← -x`.
<!-- /part -->

<!-- part: bausteine -->
## Die Bausteine eines Struktogramms

Ein Struktogramm ist ein Rechteck, das von oben nach unten gelesen wird. Jeder Baustein ist wieder ein Rechteck, und Bausteine stecken ineinander, wie Blöcke im Code. Es gibt keine Pfeile: Wohin es weitergeht, ergibt sich daraus, wo ein Rechteck steht.

### Anweisungen nacheinander

Eine **Anweisung** ist ein Kasten. Mehrere Anweisungen untereinander werden nacheinander ausgeführt, von oben nach unten; das heißt **Sequenz**.

```struktogramm
width ← 4
height ← 3
area ← width * height
```

### Verzweigung

Bei einer **Verzweigung** steht oben im Dreieck eine Bedingung. Ist sie wahr, geht es links weiter, sonst rechts. Fehlt `else`, bleibt die rechte Spalte leer. Danach geht es für beide Seiten unter der Verzweigung weiter.

```struktogramm
if temperature < 0:
    print("Glatteis")
else:
    print("kein Glatteis")
```

Mit `else if` steckt in der rechten Spalte eine weitere Verzweigung; so wählt das Struktogramm aus mehr als zwei Fällen:

```struktogramm
if points ≥ 90:
    grade ← 1
else if points ≥ 75:
    grade ← 2
else:
    grade ← 3
```

### Schleife mit der Bedingung am Anfang

Eine **kopfgesteuerte Schleife** prüft ihre Bedingung, bevor sie den Block ausführt, und dann vor jedem weiteren Mal. Ist die Bedingung schon am Anfang falsch, läuft der Block kein einziges Mal. Der Block steht rechts neben dem Balken.

```struktogramm
while n > 0:
    digits ← digits + 1
    n ← n div 10
```

Eine **Zählschleife** zählt eine Variable von einem Wert bis zu einem anderen, oder sie geht der Reihe nach durch alle Elemente einer Liste. Im Struktogramm sieht sie aus wie eine kopfgesteuerte Schleife:

```struktogramm
for i from 1 to 10:
    print(i * i)
```

### Schleife mit der Bedingung am Ende

Eine **fußgesteuerte Schleife** führt den Block zuerst aus und prüft erst danach, ob sie aufhört. Der Block läuft also mindestens einmal; das passt, wenn man erst etwas versuchen muss, um zu wissen, ob es gereicht hat.

```struktogramm
repeat:
    guess ← read_number()
until guess == secret
```

### Eine Funktion

Über dem Struktogramm einer Funktion steht ihr Name mit den Parametern. `return` beendet sie, auch mitten in einer Schleife.

```struktogramm
contains(list, wanted):
    for each element in list:
        if element == wanted: return true
    return false
```
<!-- /part -->

<!-- part: code -->
## Vom Pseudocode zum Code

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th>Pseudocode</th><th>Ruby</th><th>Python</th></tr></thead>
<tbody>
<tr><th><code>sum ← 0</code></th><td><code>sum = 0</code></td><td><code>sum = 0</code></td></tr>
<tr><th><code>a == b and not c</code></th><td><code>a == b &amp;&amp; !c</code></td><td><code>a == b and not c</code></td></tr>
<tr><th><code>a mod b</code>, <code>a div b</code></th><td><code>a % b</code>, <code>a / b</code> (bei ganzen Zahlen)</td><td><code>a % b</code>, <code>a // b</code></td></tr>
<tr><th><code>if x &lt; 0:</code> … <code>else if x == 0:</code> … <code>else:</code></th><td><code>if x &lt; 0</code> … <code>elsif x == 0</code> … <code>else</code> … <code>end</code></td><td><code>if x &lt; 0:</code> … <code>elif x == 0:</code> … <code>else:</code></td></tr>
<tr><th><code>while n &gt; 0:</code></th><td><code>while n &gt; 0</code> … <code>end</code></td><td><code>while n &gt; 0:</code></td></tr>
<tr><th><code>for i from 0 to 11:</code></th><td><code>(0..11).each do |i|</code> … <code>end</code></td><td><code>for i in range(0, 12):</code></td></tr>
<tr><th><code>for each x in list:</code></th><td><code>list.each do |x|</code> … <code>end</code></td><td><code>for x in list:</code></td></tr>
<tr><th><code>repeat:</code> … <code>until done</code></th><td><code>loop do</code> … <code>break if done</code> <code>end</code></td><td><code>while True:</code> … <code>if done: break</code></td></tr>
<tr><th><code>name(a, b):</code> … <code>return x</code></th><td><code>def name(a, b)</code> … <code>return x</code> <code>end</code></td><td><code>def name(a, b):</code> … <code>return x</code></td></tr>
</tbody>
</table></div>

Vorsicht bei der Zählschleife: `for i from 0 to 11` zählt die 11 mit. Ruby schreibt das genauso (`0..11`), Pythons `range` hört dagegen eins vorher auf, darum `range(0, 12)`.
<!-- /part -->

## Zum Nachschlagen

- Die Sinnbilder der Struktogramme sind in der Norm DIN 66261 (1985) festgelegt; sie ist nicht frei im Netz.
- Pseudocode für viele Algorithmen, Schritt für Schritt zum Durchklicken: [Sortieren](/reference/sortieren), [Listen, Stack und Queue](/reference/listen), [Graphen](/reference/graphen).
- Die Grundlagen beider Sprachen nebeneinander: [Ruby und Python](/reference/ruby-python).
