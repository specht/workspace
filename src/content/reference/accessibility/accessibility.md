<div class='meta'>
kind: reference
search: Barrierefreiheit, Accessibility, a11y, Screenreader, Kontrast, Alt-Text, Tastatur, WCAG, BITV, BFSG
</div>

<script src='/include/a11y_demo.js'></script>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Barrierefreiheit

<p class='abstract'>
Eine Website ist barrierefrei, wenn alle sie benutzen können: wer nicht sieht und sie sich vorlesen lässt, wer nur die Tastatur benutzt, wer Farben schlecht unterscheidet, wer die Schrift groß braucht oder nicht hört, was ein Video sagt. Hier stehen die wichtigsten Regeln, was das Gesetz dazu sagt und wie du deine eigene Seite prüfst.
</p>

## Was Barrierefreiheit heißt

Menschen benutzen das Internet auf sehr verschiedene Weise. Wer blind ist, hört
die Seite von einem **Screenreader**, einem Programm, das vorliest, was auf ihr
steht, und springt mit der Tastatur von Überschrift zu Überschrift. Wer die
Maus nicht halten kann, bedient alles mit der Tastatur oder mit der Stimme. Wer
schlecht sieht, vergrößert die Seite auf das Doppelte oder mehr. Und viele
Barrieren treffen alle einmal: helle Sonne auf dem Bildschirm, ein Video in der
Bahn ohne Ton, ein gebrochener Arm.

Barrierefreiheit ist darum keine Zugabe für wenige. Sie entsteht beim Bauen: mit
dem richtigen HTML-Element, einem Text für jedes Bild, genug Kontrast. Hinterher
lässt sie sich nur mühsam nachrüsten.

Die Regeln dafür stehen in den **Web Content Accessibility Guidelines**
(WCAG) des W3C, heute in der Fassung 2.2. Sie haben drei Stufen, A, AA und
AAA; Gesetze verlangen meist AA.

<!-- part: barrierefreiheit-regeln -->
<!-- short: Die vier Grundsätze der WCAG (wahrnehmbar, bedienbar, verständlich, robust) mit den wichtigsten Regeln: Alt-Text, Kontrast, nicht nur Farbe, Tastatur, sichtbarer Fokus, Beschriftungen, Überschriften, Sprache, Untertitel, Zoom, große Klickflächen. -->
## Die wichtigsten Regeln

Die WCAG ordnen ihre Regeln nach vier Grundsätzen. In Klammern steht die Nummer
der Regel in den WCAG 2.2 und ihre Stufe.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Regel</th><th>Was sie verlangt</th></tr></thead>
<tbody>
<tr><td colspan='2'><strong>Wahrnehmbar:</strong> alles, was die Seite zeigt, kommt auch an, wenn man es nicht sieht oder hört</td></tr>
<tr><td>Text für Bilder<br><small>1.1.1, A</small></td><td>Jedes Bild hat ein <code>alt</code>, das sagt, was es zeigt; ein Bild nur zur Zierde hat <code>alt=""</code>, damit der Screenreader es überspringt.</td></tr>
<tr><td>Untertitel<br><small>1.2.2, A</small></td><td>Ein Video mit Ton hat Untertitel.</td></tr>
<tr><td>Kontrast<br><small>1.4.3, AA</small></td><td>Text hebt sich mindestens 4,5 : 1 vom Hintergrund ab, großer Text (ab 18 Punkt, fett ab 14) mindestens 3 : 1.</td></tr>
<tr><td>Nicht nur Farbe<br><small>1.4.1, A</small></td><td>Was eine Farbe sagt, sagt auch noch etwas anderes: ein Fehler ist rot und hat ein Zeichen oder ein Wort, ein Link ist blau und unterstrichen.</td></tr>
<tr><td>Vergrößern<br><small>1.4.4 und 1.4.10, AA</small></td><td>Auf das Doppelte vergrößert und auf 320 Pixel Breite geht nichts verloren und nichts muss seitwärts geschoben werden.</td></tr>
<tr><td colspan='2'><strong>Bedienbar:</strong> alles lässt sich bedienen, auch ohne Maus</td></tr>
<tr><td>Tastatur<br><small>2.1.1, A</small></td><td>Jeder Link, Knopf und jedes Feld ist mit <kbd>Tab</kbd> erreichbar und mit <kbd>Enter</kbd> oder der Leertaste zu benutzen.</td></tr>
<tr><td>Fokus sichtbar<br><small>2.4.7, AA</small></td><td>Man sieht, wo man mit der Tastatur gerade ist; <code>outline: none</code> ohne Ersatz nimmt das weg.</td></tr>
<tr><td>Große Klickflächen<br><small>2.5.8, AA</small></td><td>Was man anklickt oder antippt, ist mindestens 24 × 24 Pixel groß oder hat so viel Abstand zum nächsten.</td></tr>
<tr><td colspan='2'><strong>Verständlich:</strong> man weiß, was die Seite tut und was sie will</td></tr>
<tr><td>Sprache<br><small>3.1.1, A</small></td><td><code>&lt;html lang="de"&gt;</code>: Der Screenreader liest dann deutsch vor, nicht mit englischer Aussprache.</td></tr>
<tr><td>Beschriftungen<br><small>3.3.2, A</small></td><td>Jedes Eingabefeld hat ein <code>&lt;label&gt;</code>, das sagt, was hinein soll.</td></tr>
<tr><td colspan='2'><strong>Robust:</strong> Browser und Hilfsprogramme verstehen, was auf der Seite ist</td></tr>
<tr><td>Das richtige Element<br><small>4.1.2, A</small></td><td>Ein Knopf ist ein <code>&lt;button&gt;</code>, ein Link ein <code>&lt;a href&gt;</code>, eine Überschrift ein <code>&lt;h2&gt;</code>: Nur dann wissen Screenreader, was es ist, und die Tastatur kommt hin. Ein <code>&lt;div&gt;</code> mit <code>onclick</code> kann das alles nicht von selbst.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

### Nur Farbe

Eine Rot-Grün-Schwäche ist häufig: In Europa hat sie etwa jeder zwölfte Mann.
Wer sie hat, sieht Rot und Grün als ähnliche Brauntöne. Eine Liste, in der nur
ein grüner oder ein roter Punkt sagt, was gelöst ist, sagt dann nichts mehr,
und ein Feld, dessen Fehler nur ein roter Rahmen zeigt, sieht aus wie jedes
andere. Ein Zeichen und ein Wort dazu gehen nicht verloren, auch nicht in Grau,
auf einem schlechten Beamer oder beim Ausdruck.

<div class='a11y-demo' data-demo='color'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell »Rot-Grün-Schwäche (grün)« ein: Welche Aufgaben sind links gelöst, welche rechts? Und welches Feld hat einen Fehler? Dann »in Grau«: Was bleibt links übrig?
</div>

### Kontrast

Wie gut sich Text vom Hintergrund abhebt, misst das **Kontrastverhältnis**: von
1 : 1, Grau auf demselben Grau, bis 21 : 1, Schwarz auf Weiß. Helles Grau auf
Weiß sieht auf einem guten Bildschirm elegant aus, auf dem Handy in der Sonne
ist es kaum zu lesen.

<div class='a11y-demo' data-demo='contrast'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Schieb das Grau langsam dunkler: Ab wann reicht es für großen Text, ab wann für normalen? Wie dunkel muss es sein, bis du es selbst gut lesen kannst?
</div>

### Was ein Screenreader hört

Ein Screenreader sieht die Seite nicht, er liest ihr HTML. Was dort nicht steht,
erfährt niemand, der ihm zuhört.

<div class='a11y-demo' data-demo='reader'></div>


<!-- part: barrierefreiheit-gesetz -->
<!-- short: BITV 2.0 für öffentliche Stellen, das Barrierefreiheitsstärkungsgesetz seit 28. Juni 2025 für Online-Shops und andere Dienstleistungen, Kleinstunternehmen ausgenommen. -->
## Was das Gesetz sagt

- **Öffentliche Stellen:** Websites und Apps von Behörden müssen barrierefrei
  sein, für den Bund nach der Barrierefreie-Informationstechnik-Verordnung
  (BITV 2.0), in den Ländern nach ihren eigenen Gesetzen. Maßstab sind die
  harmonisierten europäischen Normen, für Websites die EN 301 549. Jede Seite
  muss eine **Erklärung zur Barrierefreiheit** verlinken, die sagt, was noch
  nicht barrierefrei ist, und jährlich erneuert wird (§ 7 BITV 2.0).
- **Unternehmen:** Seit dem 28. Juni 2025 gilt das
  Barrierefreiheitsstärkungsgesetz (BFSG), mit dem Deutschland den European
  Accessibility Act der EU umsetzt. Es verlangt Barrierefreiheit unter anderem
  von Online-Shops und anderen Diensten, mit denen Verbraucherinnen und
  Verbraucher im Internet einen Vertrag schließen (§ 1 Abs. 3 Nr. 5 BFSG).
  Ausgenommen sind Kleinstunternehmen, die Dienstleistungen anbieten: weniger
  als zehn Beschäftigte und höchstens 2 Millionen Euro Umsatz oder Bilanzsumme
  im Jahr (§ 3 Abs. 3 und § 2 Nr. 17 BFSG).
<!-- /part -->

<!-- part: barrierefreiheit-html -->
<!-- short: Eine Seite mit lang, Bildern mit alt, Feldern mit label, echten Knöpfen und Überschriften der Reihe nach; ein sichtbarer Fokus und weniger Bewegung in CSS. -->
## Eine zugängliche Seite

Die Elemente erklärt die Seite [HTML](/reference/html), das Aussehen [CSS](/reference/css).

```html
<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="utf-8">
  <title>Anmeldung zur Hacker-AG</title>
</head>
<body>
  <h1>Anmeldung zur Hacker-AG</h1>
  <img src="ag.jpg" alt="Sechs Lernende an Laptops, einer zeigt auf seinen Bildschirm">
  <h2>Deine Angaben</h2>
  <form>
    <label for="name">Dein Name</label>
    <input id="name" type="text" autocomplete="name">
    <button type="submit">Anmelden</button>
  </form>
</body>
</html>
```

- `lang="de"` sagt dem Screenreader die Sprache.
- Die Überschriften gehen der Reihe nach, `h1` dann `h2`; wer nicht sieht,
  springt an ihnen entlang durch die Seite.
- `for` und `id` verbinden die Beschriftung mit dem Feld: Ein Klick auf »Dein
  Name« setzt den Cursor hinein, und der Screenreader liest sie vor.
- Ein echter `<button>` ist mit der Tastatur erreichbar und sagt, dass er ein
  Knopf ist.

Dazu gehört etwas CSS:

```css
:focus-visible {
  outline: 3px solid #1a5fb4;
  outline-offset: 2px;
}

@media (prefers-reduced-motion: reduce) {
  * {
    animation: none !important;
    transition: none !important;
  }
}
```

Der Rahmen zeigt, wo die Tastatur gerade ist, und wer im System »weniger
Bewegung« eingestellt hat, bekommt keine Animationen, die schwindlig machen
können.
<!-- /part -->

<!-- part: barrierefreiheit-pruefen -->
<!-- short: Mit der Tastatur durch die Seite, auf 200 % vergrößern, einen Screenreader zuhören, Kontrast messen, Lighthouse und den Barrierefreiheits-Inspektor laufen lassen. -->
## Selbst prüfen

1. **Nur Tastatur:** Leg die Maus weg und geh mit <kbd>Tab</kbd> und
   <kbd>Shift</kbd>+<kbd>Tab</kbd> durch die Seite. Kommst du überall hin,
   siehst du immer, wo du bist, und geht alles mit <kbd>Enter</kbd> und der
   Leertaste?
2. **Vergrößern:** <kbd>Strg</kbd>+<kbd>+</kbd> bis 200 %. Ist noch alles da,
   ohne dass du seitwärts schieben musst?
3. **Vorlesen lassen:** Ein Screenreader ist in jedem System schon dabei oder
   kostenlos: VoiceOver auf dem Mac und dem iPhone, TalkBack auf Android,
   Orca unter Linux, NVDA für Windows. Hör dir deine Seite mit
   geschlossenen Augen an.
4. **Kontrast messen:** Der Contrast Checker von WebAIM rechnet aus zwei
   Farben das Verhältnis aus.
5. **Werkzeuge im Browser:** Lighthouse in den Entwicklerwerkzeugen von
   Chrome findet fehlende <code>alt</code>, Beschriftungen und zu wenig Kontrast;
   der Barrierefreiheits-Inspektor von Firefox zeigt, was ein Screenreader von
   jedem Element erfährt. Was ein Werkzeug nicht findet, ist damit noch nicht
   in Ordnung: Ob ein <code>alt</code> das Bild wirklich beschreibt, sieht nur ein
   Mensch.
<!-- /part -->

## Zum Nachschlagen

- [WCAG 2.2](https://www.w3.org/TR/WCAG22/): alle Regeln mit ihren Stufen, auf
  Englisch; [Understanding WCAG 2.2](https://www.w3.org/WAI/WCAG22/Understanding/)
  erklärt jede mit Beispielen
- [MDN: Barrierefreiheit](https://developer.mozilla.org/de/docs/Web/Accessibility):
  wie man sie in HTML, CSS und JavaScript umsetzt
- [Bundesfachstelle Barrierefreiheit](https://www.bundesfachstelle-barrierefreiheit.de/):
  was in Deutschland gilt, für Behörden und Unternehmen
- [BITV 2.0](https://www.gesetze-im-internet.de/bitv_2_0/) und
  [BFSG](https://www.gesetze-im-internet.de/bfsg/): die beiden Gesetze im Wortlaut
- [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/): den
  Kontrast zweier Farben messen
