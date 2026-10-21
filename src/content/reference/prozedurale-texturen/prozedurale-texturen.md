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
Holz, Fliesen, Marmor, Mauerwerk: Eine Textur muss kein Foto sein. Man kann sie auch ausrechnen, für jeden Punkt einer Fläche aus seiner Stelle, mit Zufall, der immer gleich bleibt, mit Rauschen, mit Zellen und mit Formen. Hier steht, wie das geht, wie eine Textur nahtlos wird, wie sie sich gar nicht mehr wiederholt und warum feine Linien in der Ferne flimmern. Jedes Beispiel läuft auf dieser Seite, in <a href='/reference/glsl'>GLSL</a>, und lässt sich ändern.
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

Wie versetzt man jede zweite Reihe um eine halbe Fliese, wie bei Mauersteinen? Tipp: Schieb `p.x` um 5 cm, wenn `mod(floor(p.y * 10.0), 2.0)` 1 ist, bevor du die Nummer der Fliese ausrechnest. Die Lösung steht im nächsten Abschnitt.

## Versetzte Reihen

Eine Mauer aus Ziegeln ist eine Fliesenwand mit zwei Unterschieden. Die Steine sind länger als hoch, hier 24 cm × 7,1 cm mit 1 cm Fuge. Und jede zweite Reihe ist um einen halben Stein verschoben, damit keine Fuge durch die ganze Wand läuft (Läuferverband). Dafür kommt zuerst die Nummer der Reihe, dann die Stelle in ihr, schon verschoben:

<div class='shader-lab' data-metres='1.2'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    float row = floor(p.y / 0.081);               // 7,1 cm Stein und 1 cm Fuge
    float x = p.x / 0.25 + 0.5 * mod(row, 2.0);   // jede zweite Reihe einen halben Stein weiter
    float tone = 0.8 + 0.3 * hash(vec2(floor(x), row));
    vec3 stone = vec3(0.62, 0.3, 0.22) * tone;
    float across = fract(p.y / 0.081);
    float ex = min(fract(x), 1.0 - fract(x)) * 0.25;   // zur nächsten Fuge, in Metern
    float ey = min(across, 1.0 - across) * 0.081;
    float mortar = step(min(ex, ey), 0.005);
    float shade = across > 0.5 ? 0.8 : 0.95;   // oben, unter dem Stein darüber: im Schatten
    return mix(stone, vec3(0.62, 0.6, 0.56) * shade, mortar);
}</textarea>
</div>

Die Zeile mit `shade` ist ein kleiner Trick: Der Mörtel liegt etwas tiefer als die Steine, also fällt von oben Schatten in die Fuge. Ist ihre obere Hälfte dunkler als die untere, sieht die Wand gleich weniger flach aus, obwohl sie es ist.

Ersetze `0.5 * mod(row, 2.0)` durch `0.25 * row`: Jede Reihe rutscht um einen Viertelstein weiter, und die Fugen laufen schräg über die Wand. Und was wird aus der Mauer, wenn du `shade` überall auf 0.95 setzt?

## Abwechselnd gedreht

`mod(id.x + id.y, 2.0)` ist für die Felder eines Schachbretts abwechselnd 0 und 1: Geht man ein Feld nach rechts oder nach oben, ändert sich die Summe um eins. Damit lässt sich mehr machen als Schwarz und Weiß. Teppichfliesen im Büro liegen oft so, dass jede quer zu ihren Nachbarn liegt, damit man die Nähte nicht sieht. Ihre Rippen laufen in der einen Fliese entlang `x`, in der nächsten entlang `y`:

<div class='shader-lab' data-metres='2'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 id = floor(p / 0.5);                    // Fliesen von 50 cm
    bool turned = mod(id.x + id.y, 2.0) > 0.5;   // jede zweite, wie auf einem Schachbrett
    float rib = 0.5 + 0.5 * sin((turned ? p.x : p.y) * 6.2832 / 0.02);   // Rippen alle 2 cm
    float tone = 0.9 + 0.12 * hash(id) + 0.1 * (rib - 0.5);
    return vec3(0.3, 0.36, 0.42) * tone;
}</textarea>
</div>

Dasselbe Muster ohne Rippen, nur mit zwei Farben, ist ein Schachbrett. Wie man es zeichnet, ohne dass es in der Ferne flimmert, steht unten bei »Flimmern«.

## Fischgrät

Nicht jedes Muster ist ein Raster aus Rechtecken. Beim Fischgrätparkett liegen kurze Stäbe abwechselnd längs und quer: ein Längsstab und an seinem rechten Ende ein Querstab nach unten, das ist ein Paar. Das nächste Paar liegt eine Stabbreite schräg darüber, so entsteht eine Treppe, und viele Treppen nebeneinander füllen die Fläche. Die Paare liegen also auf einem schiefen Gitter mit den Schritten `(1, 1)` und `(n, -n)`, in Stabbreiten, wenn ein Stab `n`-mal so lang wie breit ist.

Für einen Punkt rechnet man aus, in welcher Masche dieses Gitters er liegt (die beiden Schritte als Koordinaten, abgerundet), und prüft dann diese Masche und ihre Nachbarn: Liegt der Punkt, von ihrem Paar aus gesehen, im Längs- oder im Querstab? Ein Paar ragt weit über seine Masche hinaus, deshalb geht die Suche ein paar Maschen zurück. Der Stab, in dem der Punkt liegt, gibt den Ton (`hash`) und wie weit es bis zur nächsten Fuge ist:

<div class='shader-lab' data-metres='1.5'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    const float W = 0.07, N = 6.0;                 // Stäbe 7 cm × 42 cm
    // in Stabbreiten, um 45° gedreht: So läuft das Zickzack quer
    vec2 q = mat2(0.7071, -0.7071, 0.7071, 0.7071) * p / W;
    vec2 base = floor(vec2((q.x + q.y) * 0.5, (q.x - q.y) / (2.0 * N)));
    vec2 id = base; float edge = 0.0, kind = 0.0;
    for (int j = -1; j <= 1; j++) for (int i = -5; i <= 1; i++) {
        vec2 c = base + vec2(float(i), float(j));
        vec2 r = q - c.x * vec2(1.0, 1.0) - c.y * vec2(N, -N);   // vom Paar aus
        // im Längsstab?
        if (r.x >= 0.0 && r.x < N && r.y >= 0.0 && r.y < 1.0) {
            id = c; kind = 0.0;
            edge = min(min(r.x, N - r.x), min(r.y, 1.0 - r.y));
        }
        // im Querstab?
        if (r.x >= N && r.x < N + 1.0 && r.y >= 1.0 - N && r.y < 1.0) {
            id = c; kind = 1.0;
            edge = min(min(r.x - N, N + 1.0 - r.x), min(r.y - 1.0 + N, 1.0 - r.y));
        }
    }
    float tone = 0.82 + 0.25 * hash(id + kind * 31.7);
    float joint = 1.0 - smoothstep(0.0, 0.04, edge);   // Fuge am Rand
    return vec3(0.72, 0.5, 0.28) * tone * (1.0 - 0.5 * joint);
}</textarea>
</div>

Ändert man `N` auf 4, werden die Stäbe kürzer; die Suche muss dann weniger weit zurück, schadet aber nicht. Ohne die Drehung (`mat2`) läuft das Zickzack schräg.

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

<div class='person'>
<img src='perlin.webp' alt='Ken Perlin 2007 auf einem Podium: ein Mann mit dunklem Haar in blauem Hemd, lächelnd zur Seite.'>
<p><strong>Ken Perlin</strong> (geboren 1954), Informatiker in New York. Nach den Computerbildern für den Film <em>Tron</em> (1982) störte ihn, wie glatt und sauber Oberflächen aus dem Computer aussahen, und er erfand das Rauschen, das seinen Namen trägt. Dafür bekam er 1997 einen Oscar für technische Leistungen.</p>
<p>Wolken, Feuer und ganze Landschaften in Spielen wie Minecraft entstehen aus Perlin-Rauschen und seinen Verwandten.</p>
<p class='person-credit'>Foto: Ambuj Saxena, 2007, CC BY 2.5, zugeschnitten (<a href='https://commons.wikimedia.org/wiki/File:Ken_Perlin.jpg'>Wikimedia Commons</a>)</p>
</div>

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

## Zellen

Terrazzo ist ein Boden aus gebrochenen Steinchen in Zement. Die Steinchen sind eckig wie Scherben und liegen dicht an dicht. Dafür liegt in jedem Feld eines Gitters ein Punkt an einer zufälligen Stelle, und jeder Punkt der Fläche gehört zu dem Punkt, der ihm am nächsten liegt. So zerfällt die Fläche in Zellen mit geraden Kanten, ein **Voronoi-Diagramm**. Gesucht wird nur in den neun Feldern um die Stelle herum, denn weiter weg liegt kein Punkt, der näher sein könnte.

Merkt man sich den Abstand zum nächsten Punkt (`f1`) und zum zweitnächsten (`f2`), weiß man auch, wo eine Kante liegt: Dort sind beide fast gleich weit weg. Wo `f2 - f1` klein ist, liegt Zement, sonst ein Steinchen, und jedes bekommt aus der Nummer seines Felds eine Farbe:

<div class='shader-lab' data-metres='0.6'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 g = p / 0.04;            // ein Feld alle 4 cm
    vec2 i = floor(g);
    float f1 = 9.0, f2 = 9.0;     // der nächste und der zweitnächste Punkt
    vec2 nearest = vec2(0.0);
    for (int y = -1; y != 2; y++) {
        for (int x = -1; x != 2; x++) {
            vec2 c = i + vec2(float(x), float(y));
            vec2 at = c + vec2(hash(c), hash(c + 17.0));   // ein Punkt irgendwo im Feld c
            float d = length(g - at);
            if (f1 > d) { f2 = f1; f1 = d; nearest = c; }
            else if (f2 > d) { f2 = d; }
        }
    }
    float pick = hash(nearest + 9.0);
    vec3 stone = pick > 0.5 ? vec3(0.95, 0.93, 0.89) : pick > 0.2 ? vec3(0.55) : vec3(0.64, 0.34, 0.24);
    float chip = step(0.2, f2 - f1);   // nahe einer Kante: Zement
    return mix(vec3(0.82, 0.78, 0.7), stone, chip);
}</textarea>
</div>

Steven Worley hat diese Art Rauschen 1996 beschrieben; es heißt nach ihm **Worley Noise** oder zelluläres Rauschen. Mit `return vec3(f1);` sieht man die Abstände selbst: In jeder Zelle wird es zur Mitte hin dunkel, wie bei Zellen unter dem Mikroskop, Schuppen oder Kieseln. Mit `return vec3(f2 - f1);` bleiben nur die Kanten.

Ändere `0.2` in `step(0.2, f2 - f1)`: Bei 0.05 liegen die Steinchen dicht an dicht, bei 0.5 schwimmen sie im Zement. Was passiert, wenn du in `at` statt `vec2(hash(c), hash(c + 17.0))` nur `vec2(0.5)` schreibst?

## Formen

Löcher, Pfeile, ein Etikett: Auch Formen sind Rechnungen. Eine Form ist die Menge der Stellen, für die eine Bedingung gilt. Ein Kreis ist alles, was näher als sein Radius an seiner Mitte liegt, also `length(p - mitte) < r`. Ein Rechteck ist alles, was in x zwischen zwei Werten liegt und in y auch. Zwei Formen zugleich (und) ist ihr Produkt, eine von beiden (oder) ihr Maximum. Ein Dreieck ist ein Band, das nach oben schmaler wird: seine halbe Breite hängt von y ab.

Damit die Ränder nicht treppig werden, ist jede Bedingung einen Pixel breit weich, wie bei den Fugen unten unter »Flimmern«. Hier Löcher auf einem Raster, wie die Anker in einer Wand aus Beton, und ein Pfeil:

<div class='shader-lab' data-metres='1'>
<textarea spellcheck='false'>// 1 zwischen a und b, sonst 0, an den Rändern einen Pixel weich
float band(float x, float a, float b, float px) {
    return smoothstep(a - px, a + px, x) - smoothstep(b - px, b + px, x);
}
vec3 color(vec2 p) {
    float px = max(fwidth(p.x), fwidth(p.y));
    // Löcher alle 25 cm: die Stelle im eigenen Feld, von seiner Mitte aus
    vec2 q = (fract(p / 0.25) - 0.5) * 0.25;
    float hole = 1.0 - smoothstep(0.02 - px, 0.02 + px, length(q));
    // ein Pfeil: Schaft (ein schmales Band in x und eins in y) und Spitze
    vec2 a = p - vec2(0.0, -0.2);
    float shaft = band(a.x, -0.015, 0.015, px) * band(a.y, 0.0, 0.2, px);
    float head = band(a.y, 0.2, 0.32, px) * band(abs(a.x), -1.0, (0.32 - a.y) * 0.7, px);
    float arrow = max(shaft, head);
    vec3 c = vec3(0.6, 0.6, 0.58);
    c = mix(c, vec3(0.3), hole);
    return mix(c, vec3(0.75, 0.15, 0.1), arrow);
}</textarea>
</div>

Was eine Form hat, kann sie aus `hash` bekommen: Ein Etikett, das auf jeder Kiste woanders klebt und anders groß ist, ist ein Rechteck, dessen Ecken aus der Nummer der Kiste kommen. Für viele Formen gibt es fertige Abstandsfunktionen (Signed Distance Functions), die für jede Stelle sagen, wie weit sie vom Rand der Form entfernt ist, außen positiv, innen negativ.

Mach aus `0.7` in der Spitze `1.5`: Wie ändert sich der Pfeil? Und wie bekommt er einen zweiten, etwas weiter rechts? Tipp: `max` aus zwei Pfeilen.

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

Ein Schachbrett hat dasselbe Problem an jeder Kante zwischen Schwarz und Weiß. `sin(q * 3.14159)` wechselt an jeder Kante das Vorzeichen; `smoothstep` macht daraus 0 oder 1, über einen Pixel weich. Weit weg, wo ein Pixel viele Felder zeigt, wird daraus ein gleichmäßiges Grau statt eines flimmernden Musters:

<div class='shader-lab' data-metres='2' data-far='80'>
<textarea spellcheck='false'>vec3 color(vec2 p) {
    vec2 q = p / 0.3;                                  // Felder von 30 cm
    float pixel = max(fwidth(q.x), fwidth(q.y));
    vec2 s = smoothstep(-pixel, pixel, sin(q * 3.14159) * 0.32);
    float dark = s.x * s.y + (1.0 - s.x) * (1.0 - s.y);   // gleich in x und y: dunkel
    return mix(vec3(0.9, 0.88, 0.84), vec3(0.06), dark);
}</textarea>
</div>

Dasselbe gilt für die Maserung im Holz, die Adern im Marmor und die Steinchen im Terrazzo: Werden sie feiner als ein Pixel, lässt man sie verblassen, statt sie flimmern zu lassen, und zeigt ihre mittlere Farbe.

## Zum Nachschlagen

- [The Book of Shaders](https://thebookofshaders.com/10/?lan=de) von Patricio Gonzalez Vivo und Jen Lowe, auf Deutsch: die Kapitel [Zufall](https://thebookofshaders.com/10/?lan=de), [Rauschen](https://thebookofshaders.com/11/?lan=de) und [Fraktale Brownsche Bewegung](https://thebookofshaders.com/13/?lan=de), jedes mit Beispielen zum Ändern
- In The Book of Shaders auch [Formen](https://thebookofshaders.com/07/?lan=de), [Muster](https://thebookofshaders.com/09/?lan=de) (versetzte und gedrehte Kacheln) und [Zelluläres Rauschen](https://thebookofshaders.com/12/?lan=de)
- [Perlin-Rauschen](https://de.wikipedia.org/wiki/Perlin-Noise), [Value Noise](https://en.wikipedia.org/wiki/Value_noise), [Voronoi-Diagramm](https://de.wikipedia.org/wiki/Voronoi-Diagramm) und [Worley Noise](https://en.wikipedia.org/wiki/Worley_noise) in der Wikipedia
- Steven Worley, [»A Cellular Texture Basis Function«](https://dl.acm.org/doi/10.1145/237170.237267), SIGGRAPH 1996: wo das zelluläre Rauschen herkommt
- Inigo Quilez über [fraktales Rauschen](https://iquilezles.org/articles/fbm/), [Filtern gegen Flimmern](https://iquilezles.org/articles/filtering/), [Kanten von Voronoi-Zellen](https://iquilezles.org/articles/voronoilines/) und [Abstandsfunktionen für Formen](https://iquilezles.org/articles/distfunctions2d/) (englisch)
- Ken Perlin, [»An Image Synthesizer«](https://dl.acm.org/doi/10.1145/325165.325247), SIGGRAPH 1985: wo Rauschen, Turbulenz und Marmor herkommen
- Dave Hoskins, [»Hash without Sine«](https://www.shadertoy.com/view/4djSRW): Hash-Funktionen ohne Sinus, die auf jeder Grafikkarte gleich rechnen
- [`fwidth`](https://registry.khronos.org/OpenGL-Refpages/es3.0/html/fwidth.xhtml) in der Referenz der Khronos Group
- [GLSL](/reference/glsl): die Sprache der Shader, ihre Typen und Funktionen

<script src='shader-lab.js'></script>
