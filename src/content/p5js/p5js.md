<div class='meta'>
section: programming
image: pyramid.webp:0:67
visible: development
</div>

# Programmieren in p5.js

<p class='abstract'>
p5.js ist eine JavaScript-Bibliothek, die speziell für Künstler und Designer entwickelt wurde. Sie macht es einfach, interaktive Grafiken und Animationen zu erstellen.
</p>

Extensions: p5canvas

```javascript
function setup() {
    createCanvas(400, 400);
}

var phi = 2.7;

function draw() {
    let c1 = color('#0da6eb'); let c2 = color('#052534');
    let c3 = color('#f5e026'); let c4 = color('#be2a0b');
    let c5 = color('#f3ead4'); let c6 = color('#a18d5a');
    let c7 = color('#e9c46a'); let c8 = color('#333333');
    let y = sin(phi) * 0.5 + 0.5;
    background(220);
    noStroke();
    fill(lerpColor(c1, c2, y));
    rect(0, 0, 400, 200);
    fill(lerpColor(c3, c4, y));
    circle(200 + cos(phi) * 200, 200 + sin(phi) * 150, 100);
    fill(lerpColor(c7, c8, y));
    rect(0, 200, 400, 200);
    fill(lerpColor(lerpColor(c7, c8, y), c8, 0.2));
    triangle(200 - cos(phi) * 100 * y, 300 - sin(phi) * 100 * (1-y), 120, 300, 280, 300);
    fill(lerpColor(c5, c8, y));
    triangle(200, 150, 120, 300, 280, 300);
    fill(lerpColor(c6, c8, y));
    triangle(200, 150, 280, 300, 290, 270);
    phi += 0.01;
}
```
