<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Prozedurale Texturen

<p class='abstract'>
Holz, Fliesen, Marmor, Tapete: Eine Textur muss kein Foto sein. Man kann sie auch ausrechnen, für jeden Punkt einer Fläche aus seiner Stelle, mit Zufall, der immer gleich bleibt, und mit Rauschen. Hier steht, wie das geht, wie eine Textur nahtlos wird, wie sie sich gar nicht mehr wiederholt und warum feine Linien in der Ferne flimmern. Jedes Beispiel läuft auf dieser Seite, in <a href='/reference/glsl'>GLSL</a>, und lässt sich ändern.
</p>

## Rezept statt Bild

Eine **prozedurale Textur** ist kein gespeichertes Bild, sondern ein Rezept: eine Funktion, die für jede Stelle einer Fläche ausrechnet, welche Farbe sie hat. Das hat Vorteile:

- Es gibt keine Bilddateien, die geladen werden müssen.
- Die Textur hat keine Auflösung: Aus der Nähe bleibt sie scharf.
- Mit einer anderen Zahl im Rezept gibt es eine andere Farbe, andere Bretter, andere Adern.
- Sie muss sich nicht wiederholen: Eine Wand von 100 m kostet so viel wie eine von einem Meter.

Es gibt zwei Wege. Entweder malt ein Programm das Rezept einmal in ein Bild, etwa mit einem `canvas` im Browser, und das Bild kommt wie ein Foto auf die Fläche, Kachel für Kachel. Oder der **Fragment-Shader** rechnet das Rezept beim Zeichnen aus, für jeden Pixel, jedes Mal. Die Beispiele hier gehen den zweiten Weg. In jedem steht eine Funktion `color(p)`: `p` ist die Stelle auf der Fläche in Metern, das Ergebnis ihre Farbe, Rot, Grün und Blau von 0 bis 1.

## Zufall, der gleich bleibt

Ein Shader kennt keinen Zufall, und er soll auch keinen haben: Würde eine Fliese bei jedem Bild ihre Farbe wechseln, flackerte die Wand. Gebraucht wird eine Zahl, die zufällig aussieht, aber für dieselbe Stelle immer dieselbe ist. Das macht eine **Hash-Funktion**: Sie rührt die Bits einer Zahl so durcheinander, dass zwei benachbarte Eingaben ganz verschiedene Ergebnisse geben.

```glsl
float hash(vec2 p) {
    vec3 q = fract(vec3(p.xyx) * 0.1031);
    q += dot(q, q.yzx + 33.33);
    return fract((q.x + q.y) * q.z);
}
```

`hash` gibt eine Zahl zwischen 0 und 1. Diese stammt von Dave Hoskins, [»Hash without Sine«](https://www.shadertoy.com/view/4djSRW): Viele Beispiele im Netz nehmen `fract(sin(…) * 43758.5)`, doch das gibt auf manchen Grafikkarten Muster statt Zufall. Gibt man ihr die Nummer eines Felds, `floor(p * 4.0)`, bekommt jedes Feld von 25 cm seinen eigenen Ton:

<div class='shader-lab' data-metres='2'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 cell = floor(p * 4.0);    // die Nummer des Felds, 4 je Meter
    float t = hash(cell);          // 0 bis 1, für jedes Feld eine andere
    return vec3(t);
}</textarea>
</div>

Ändere die `4.0`: Wie groß werden die Felder? Mit `return vec3(t, 0.5, 1.0 - t);` sind sie bunt.

## Fliesen

`floor(p * 10.0)` ist die Nummer einer Fliese von 10 cm, `fract(p * 10.0)` die Stelle in ihr, von 0 bis 1. Nahe 0 oder 1 liegt der Rand: dort die Fuge. Und jede Fliese bekommt aus ihrer Nummer einen etwas anderen Ton:

<div class='shader-lab' data-metres='1'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 cell = floor(p * 10.0), inside = fract(p * 10.0);
    vec2 edge = min(inside, 1.0 - inside);   // zum Rand: 0 bis 0.5
    float grout = step(min(edge.x, edge.y), 0.04);
    float tone = 0.9 + 0.1 * hash(cell);
    vec3 tile = vec3(0.85, 0.92, 0.93) * tone;
    return mix(tile, vec3(0.6), grout);
}</textarea>
</div>

Wie versetzt man jede zweite Reihe um eine halbe Fliese, wie bei Mauersteinen? Tipp: Schieb `p.x` um 5 cm, wenn `mod(floor(p.y * 10.0), 2.0)` 1 ist, bevor du die Nummer der Fliese ausrechnest.

## Rauschen

Zufall pro Feld gibt Kanten. Für Wolken, Flecken und Stein braucht es Zufall ohne Kanten: **Rauschen** (englisch Noise). Auf den Ecken eines Gitters liegt je eine Zufallszahl aus `hash`, und dazwischen wird weich gemischt:

```glsl
float noise(vec2 p) {
    vec2 i = floor(p), f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);   // weich: am Rand flach, wie smoothstep
    return mix(mix(hash(i), hash(i + vec2(1.0, 0.0)), u.x),
               mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), u.x), u.y);
}
```

Weil `mix` mit `u` statt mit `f` mischt, gibt es an den Gitterlinien keinen Knick. Diese Art heißt **Value Noise**; Ken Perlin rechnete 1985 statt Zahlen Richtungen auf die Ecken, das gibt das bekanntere [Perlin-Rauschen](https://de.wikipedia.org/wiki/Perlin-Noise), ähnlich, nur gleichmäßiger.

<div class='shader-lab' data-metres='4'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    float n = noise(p * 2.0);   // ein Gitter alle 50 cm
    return vec3(n);
}</textarea>
</div>

**Turbulenz.** Dasselbe Rauschen in mehreren Größen, jede doppelt so fein und halb so stark wie die vorige, zusammengezählt, sieht aus wie die Natur: groß und klein zugleich. Man sagt auch fraktales Rauschen.

```glsl
float turbulence(vec2 p) {
    float sum = 0.0, size = 1.0;
    for (int i = 0; i < 4; i++) {
        sum += noise(p * size) / size;
        size *= 2.0;
    }
    return sum / 1.875;   // 1 + 1/2 + 1/4 + 1/8: wieder 0 bis 1
}
```

## Marmor und Holz

**Marmor**, wie Ken Perlin ihn 1985 in [»An Image Synthesizer«](https://dl.acm.org/doi/10.1145/325165.325247) beschrieb: Ein Sinus quer über die Fläche gibt gerade Streifen; eine Ader ist dort, wo er nahe null ist. Zählt man die Turbulenz dazu, verbiegen sich die Streifen zu Adern.

<div class='shader-lab' data-metres='2'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    float t = turbulence(p * 2.0);
    float vein = abs(sin((p.x + p.y) * 3.0 + t * 6.0));
    float line = 1.0 - smoothstep(0.0, 0.12, vein);
    return mix(vec3(0.93, 0.91, 0.86), vec3(0.45, 0.45, 0.47), line * 0.7);
}</textarea>
</div>

Was ändert `t * 6.0`, wenn du 6 durch 15 ersetzt? Und was `0.12`?

**Holz**: Bretter, 12,5 cm breit, jedes in seinem Ton aus `hash`, und eine Maserung aus einem Sinus quer über das Brett, mit Rauschen gebogen:

<div class='shader-lab' data-metres='1.5'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    float board = floor(p.x * 8.0), across = fract(p.x * 8.0);
    float tone = 0.8 + 0.3 * hash(vec2(board, 1.0));
    float bend = noise(vec2(board * 7.0, p.y * 1.5)) * 6.0;
    float grain = 0.5 + 0.5 * sin(across * 40.0 + bend);
    float joint = step(min(across, 1.0 - across), 0.03);
    vec3 wood = vec3(0.62, 0.42, 0.25) * tone * (0.88 + 0.12 * grain);
    return mix(wood, vec3(0.2, 0.12, 0.07), joint);
}</textarea>
</div>

## Nahtlos

Malt man eine Textur in ein Bild, sagen wir ein Quadrat von einem Meter, und legt es auf einer Wand nebeneinander, muss es am rechten Rand genau so weitergehen, wie es links anfängt, und unten wie oben. Sonst sieht man an jeder Wiederholung eine Naht. Ein Bild mit dieser Eigenschaft heißt **nahtlos** (englisch seamless) oder kachelbar.

Für Muster ist das leicht: Streifen und Rauten müssen nur glatt in die Breite des Bilds aufgehen. Für Rauschen nimmt man ein Gitter, das sich selbst wiederholt: Die Ecke hinter dem rechten Rand bekommt dieselbe Zufallszahl wie die am linken. Dafür genügt `mod` auf der Nummer der Ecke. Hier mit 4 Gitterfeldern je Kachel, und die roten Linien zeigen, wo eine Kachel endet:

<div class='shader-lab' data-metres='3'>
<textarea spellcheck='false'>float wrapped(vec2 p, float n) {
    vec2 i = floor(p), f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    return mix(mix(hash(mod(i, n)), hash(mod(i + vec2(1.0, 0.0), n)), u.x),
               mix(hash(mod(i + vec2(0.0, 1.0), n)), hash(mod(i + vec2(1.0, 1.0), n)), u.x), u.y);
}
vec3 color(vec2 p) {
    float n = wrapped(p * 4.0, 4.0);   // eine Kachel ist 1 m, 4 Gitterfelder
    vec2 edge = abs(fract(p + 0.5) - 0.5);
    float seam = step(min(edge.x, edge.y), 0.006);
    return mix(vec3(n), vec3(1.0, 0.2, 0.3), seam);
}</textarea>
</div>

Lösche `mod(…, n)` an einer Stelle, oder ersetze das zweite `4.0` durch `3.0`: Wo bricht das Muster an den roten Linien ab?

## Ohne Wiederholung

Nahtlos ist nicht genug. Auf einer großen Wand sieht man ein Muster, das sich jeden Meter wiederholt, auch ohne Naht: derselbe Fleck, immer wieder, in einer Reihe. Ein Shader braucht das Bild gar nicht. Er rechnet das Rezept aus der Stelle in der Welt aus, und keine Stelle kommt zweimal vor. Jedes Brett hat eine eigene Nummer, also einen eigenen Ton; das Rauschen läuft über die ganze Wand, und Flecken liegen, wo sie wollen. Das kostet nicht mehr: Die Rechnung ist für jeden Pixel dieselbe, wie groß die Fläche auch ist.

Rechnet man in drei Dimensionen, mit `vec3` statt `vec2`, laufen die Adern eines Marmors sogar über die Ecke von der Wand auf den Boden weiter, wie bei einem echten Block aus Stein.

## Flimmern

Feine Linien machen Ärger, wenn die Fläche weit weg ist. Ein Pixel misst dann mehr als eine Fuge breit ist, und ob er die Fuge trifft oder nicht, entscheidet ein winziger Unterschied: Beim Bewegen springen die Fugen hin und her, die Fläche flimmert, und es entstehen Muster, die es gar nicht gibt (Moiré). Schieb den Regler unter dem ersten Bild nach rechts:

<div class='shader-lab' data-metres='1' data-far='60'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 inside = fract(p * 10.0);
    vec2 edge = min(inside, 1.0 - inside) / 10.0;   // Abstand zur Fuge, in Metern
    float grout = step(min(edge.x, edge.y), 0.003); // 3 mm: Fuge
    return mix(vec3(0.85, 0.92, 0.93), vec3(0.45), grout);
}</textarea>
</div>

Die Lösung ist `fwidth`: Es sagt, wie viel sich ein Wert von einem Pixel zum nächsten ändert, hier also, wie viel Fläche ein Pixel zeigt. Ist er breiter als die Fuge, zeichnet man sie nicht ganz oder gar nicht, sondern so stark, wie sie den Pixel füllt. Aus der Nähe bleibt sie scharf, in der Ferne wird sie zu einem gleichmäßigen, blassen Grau:

<div class='shader-lab' data-metres='1' data-far='60'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 inside = fract(p * 10.0);
    vec2 edge = min(inside, 1.0 - inside) / 10.0;
    float pixel = max(fwidth(p.x), fwidth(p.y));          // so viel misst ein Pixel
    float d = min(edge.x, edge.y);
    float grout = 1.0 - smoothstep(0.003 - pixel, 0.003 + pixel, d);
    grout *= clamp(0.006 / pixel, 0.0, 1.0);              // weit weg: blasser
    return mix(vec3(0.85, 0.92, 0.93), vec3(0.45), grout);
}</textarea>
</div>

Dasselbe gilt für die Maserung im Holz und die Adern im Marmor: Werden sie feiner als ein Pixel, lässt man sie verblassen, statt sie flimmern zu lassen.

## Zum Nachschlagen

- [The Book of Shaders](https://thebookofshaders.com/10/?lan=de) von Patricio Gonzalez Vivo und Jen Lowe, auf Deutsch: die Kapitel [Zufall](https://thebookofshaders.com/10/?lan=de), [Rauschen](https://thebookofshaders.com/11/?lan=de) und [Fraktale Brownsche Bewegung](https://thebookofshaders.com/13/?lan=de), jedes mit Beispielen zum Ändern
- [Perlin-Rauschen](https://de.wikipedia.org/wiki/Perlin-Noise) und [Value Noise](https://en.wikipedia.org/wiki/Value_noise) in der Wikipedia
- Inigo Quilez über [fraktales Rauschen](https://iquilezles.org/articles/fbm/) und über [Filtern gegen Flimmern](https://iquilezles.org/articles/filtering/) (englisch)
- Ken Perlin, [»An Image Synthesizer«](https://dl.acm.org/doi/10.1145/325165.325247), SIGGRAPH 1985: wo Rauschen, Turbulenz und Marmor herkommen
- Dave Hoskins, [»Hash without Sine«](https://www.shadertoy.com/view/4djSRW): Hash-Funktionen ohne Sinus, die auf jeder Grafikkarte gleich rechnen
- [`fwidth`](https://registry.khronos.org/OpenGL-Refpages/es3.0/html/fwidth.xhtml) in der Referenz der Khronos Group
- [GLSL](/reference/glsl): die Sprache der Shader, ihre Typen und Funktionen

<script src='shader-lab.js'></script>
