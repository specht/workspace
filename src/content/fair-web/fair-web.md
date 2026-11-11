<div class='meta'>
kind: labor
duration: 4 Doppelstunden
curriculum: "Sek I: Leben in und mit vernetzten Systemen (Gestalten von Internetseiten, Stylesheets; Gefahren bei E-Mails: Phishing); Oberstufe: Softwareprojekt (Ergonomie), Vertiefung Informatik und Gesellschaft"
topics: [webseiten, datensicherheit, gesellschaft, zeichenketten]
language: HTML / CSS / Ruby / Python
languages: [ruby, python]
requires: [static-website]
reference: [accessibility, dark-patterns, html, css, ruby-python]
parts: true
</div>

<script src='/include/a11y_demo.js'></script>
<script src='/include/dark_patterns_demo.js'></script>
<script src='/include/mail_demo.js'></script>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Fair, barrierefrei, sicher

<p class='abstract'>
Die Hacker-AG hat eine Website, die gut aussieht und trotzdem viele aussperrt: Wer nur die Tastatur benutzt, kommt nicht zum Knopf, wer schlecht sieht, kann den Text kaum lesen, und der Cookie-Banner trickst. In diesem Labor findest du die Barrieren und die Tricks und baust sie aus, Schritt für Schritt geprüft. Im dritten Teil geht es um Mails, die so tun, als kämen sie von deiner Schule.
</p>

## Eine Seite für alle

Die Seite der Hacker-AG hat einen Fehler, den man nicht sieht, wenn man sie mit
der Maus und guten Augen benutzt. Alle anderen merken ihn sofort: Wer blind ist,
lässt sich die Seite von einem **Screenreader** vorlesen, einem Programm, das
ihr HTML liest. Wer die Maus nicht halten kann, springt mit der Tastatur von
Link zu Link. Wer schlecht sieht, vergrößert die Seite oder braucht kräftige
Farben. Und manche Barrieren treffen alle einmal: helle Sonne auf dem Handy, ein
gebrochener Arm, ein Beamer, der Farben verschluckt.

Die Regeln für eine Seite, die alle benutzen können, stehen in den **Web
Content Accessibility Guidelines** (WCAG), im Handbuch unter
[Barrierefreiheit](/reference/accessibility). Hier wendest du sie an.

### Die Seite holen

Öffne ein Terminal in deinem Workspace und hol dir die Seite der AG:

```bash
mkdir -p fair-web/bilder
cd fair-web
wget ${web_root}/fair-web/index.html ${web_root}/fair-web/style.css ${web_root}/fair-web/script.js
wget -P bilder ${web_root}/fair-web/bilder/logo.svg ${web_root}/fair-web/bilder/ag.svg
```

Öffne `fair-web/index.html` in VS Code und zeig sie mit »Go Live« im Browser
an, wie in [Statische Webseiten](/static-website). Sieh sie dir eine Minute lang
an: Ein Banner für Cookies, ein Menü, ein Bild, Termine, ein Formular.

### Benutzen wie andere

Leg die Maus zur Seite. Klick einmal in die Seite im Browser, damit sie die
Tastatur bekommt; mit <kbd>Tab</kbd> springst du dann zum nächsten Link,
Knopf oder Feld, mit <kbd>Umschalt</kbd>+<kbd>Tab</kbd> zurück, mit
<kbd>Enter</kbd> drückst du einen Knopf.

_task(tab-vorhersage)

<div class='hint try'>
<strong>Ausprobieren:</strong> Drück <kbd>Tab</kbd> immer wieder und zähl mit. Siehst du, wo du gerade bist? Kommst du zum Knopf »Anmelden«? Vergrößere die Seite dann mit <kbd>Strg</kbd>+<kbd>+</kbd> auf 200 %: Was rutscht aus dem Bild, was wird unlesbar?
</div>

<!-- after: tab-vorhersage -->
Der Knopf »Anmelden« ist gar nicht erreichbar: Er ist ein Element `div` mit
`onclick`, und ein `div` bekommt die Tastatur nie. Das Formular lässt sich so
ohne Maus nicht abschicken. Und du siehst nicht einmal, wo du bist, weil
`outline: none` in `style.css` den Rahmen um das gewählte Element weggenommen
hat.
<!-- /after -->

Ein Screenreader liest nicht, was auf dem Bildschirm steht, sondern das HTML
dahinter. So klingt eine Seite, deren HTML nicht sagt, was sie ist:

<div class='a11y-demo' data-demo='reader'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Vergleiche Zeile für Zeile, was der Screenreader links und rechts sagt. Wer die Seite nur hört: Woran merkt er links, dass es weitergeht, und wie kommt er dorthin?
</div>

Auf deinem Handy oder Rechner ist ein echter Screenreader eingebaut: VoiceOver
auf dem iPhone und dem Mac, TalkBack auf Android, die Sprachausgabe von Windows
(<kbd>Strg</kbd>+<kbd>Windows</kbd>+<kbd>Enter</kbd> schaltet sie an und aus).
Wer ihn einmal eine Minute lang benutzt hat, baut Seiten anders.

### Bilder und Sprache

Ein Bild ist für einen Screenreader nur eine Datei, bis das Attribut `alt` sagt,
was es zeigt. Fehlt es, liest er den Dateinamen vor, Buchstabe für Buchstabe
oder als »ag punkt s v g«. Ein guter Alt-Text sagt, was ein Mensch auf dem Bild
sieht und was davon für die Seite wichtig ist, in einem Satz. Ein Bild, das nur
schmückt, bekommt `alt=""`: Dann überspringt der Screenreader es.

Die Seite hat zwei Bilder. Das Logo neben dem Menü sagt, wessen Seite das ist.
Das große Bild zeigt den Raum der AG:

<img src='/fair-web/bilder/ag.svg' class='full' alt='Der Raum der AG: drei Laptops auf einem langen Tisch, auf dem ersten Code, auf dem zweiten ein Spiel, auf dem dritten eine Webseite; am Ende des Tischs sitzt ein kleiner Roboter und winkt'>

_task(alt-gut)

Die Sprache der Seite gehört in das erste Element, `<html lang="de">`. Fehlt
sie, liest ein Screenreader, der auf Englisch eingestellt ist, den deutschen
Text mit englischer Aussprache vor, und die Silbentrennung des Browsers rät.

_task(lang)

_task(alt)

### Formulare

In ein Feld mit `placeholder` steht ein grauer Text, solange es leer ist. Er
verschwindet, sobald man tippt, und manche Screenreader lesen ihn gar nicht.
Was in ein Feld gehört, sagt ein `<label>`, verbunden mit dem Feld über
`for` und `id`:

```html
<label for="name">Dein Name</label>
<input id="name" type="text" autocomplete="name">
```

Ein Klick auf den Text setzt dann auch den Cursor ins Feld, eine größere
Fläche zum Treffen. `autocomplete` sagt dem Browser, was hinein soll, damit er
es vorschlagen kann: `name`, `email`, `tel`.

Ein Fehler, den nur ein roter Rahmen zeigt, sieht niemand, der Rot und Grün
schlecht unterscheidet, und ein Screenreader sagt nichts davon. Zu einem Fehler
gehört ein Satz, der sagt, was fehlt, neben dem Feld:

```html
<p id="name-error" class="error-text" hidden>Bitte gib deinen Namen ein.</p>
```

Das Attribut `hidden` versteckt ihn, bis `script.js` es entfernt; mit
`aria-describedby="name-error"` am Feld liest der Screenreader ihn zum Feld
vor.

_task(labels)

### Knöpfe und Links

Ein `<button>` kann von selbst, was ein Knopf braucht: Er bekommt die Tastatur,
reagiert auf <kbd>Enter</kbd> und die Leertaste, und der Screenreader sagt
»Schalter«. Ein `<div onclick>` kann nichts davon. Im Formular schickt ein
`<button type="submit">` das Formular ab; `script.js` fängt das mit einem
Listener für `submit` ab, statt mit `onclick` an einem Element:

```javascript
document.getElementById('form').addEventListener('submit', (event) => {
  event.preventDefault();
  // hier prüfen, was eingetippt wurde
});
```

Auch das »↑« rechts unten ist ein `<div>`. Was zu einer Stelle der Seite
springt, ist ein Link: `<a href="#" class="top">Nach oben</a>`.

Screenreader haben eine Liste aller Links einer Seite, damit man nicht alles
anhören muss. Auf der Seite der AG steht dort zweimal »hier klicken«.

_task(linkliste)

Ein Link sagt, wohin er führt: `<a href="#termine">Termine</a>`. Und ein
Menü ist ein `<nav>`, das kommt im Abschnitt »Struktur«.

_task(knopf)

### Farbe und Kontrast

Wie gut sich Text von seinem Hintergrund abhebt, misst das
**Kontrastverhältnis**, von 1 : 1 (Grau auf demselben Grau) bis 21 : 1
(Schwarz auf Weiß). Die WCAG verlangen für normalen Text mindestens 4,5 : 1,
für großen Text (ab 24 Pixel, fett ab knapp 19) mindestens 3 : 1.

<div class='a11y-demo' data-demo='contrast'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Schieb das Grau langsam dunkler: Ab welchem Grau reicht es für großen Text, ab welchem für normalen? Ab wann kannst du es selbst bequem lesen?
</div>

Und manches sagt die Seite nur mit Farbe. Wer eine Rot-Grün-Schwäche hat, in
Europa etwa jeder zwölfte Mann, sieht Rot und Grün als ähnliche Brauntöne:

<div class='a11y-demo' data-demo='color'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Stell »Rot-Grün-Schwäche (grün)« ein: Welche Aufgaben sind links gelöst, welche rechts? Welches Feld hat einen Fehler? Auf der Seite der AG sagt nur die Farbe, welcher Termin ausfällt.
</div>

Wie ein Browser das Verhältnis ausrechnet, steht in den WCAG, und du kannst es
selbst programmieren. Zuerst bekommt jede Farbe eine **relative Leuchtdichte**
\\(L\\) zwischen 0 (Schwarz) und 1 (Weiß). Eine Farbe wie `#2b8a3e` besteht
aus drei Kanälen, Rot `2b`, Grün `8a` und Blau `3e`, jeder eine Zahl von 0 bis
255 in hexadezimaler Schreibweise. Jeder Kanal wird erst durch 255 geteilt,
dann **linearisiert**: Ein Bildschirm macht die Zahlen nicht gleichmäßig hell,
die Hälfte von 255 leuchtet viel weniger als halb so hell.

$$c' = \frac{c}{255}, \qquad c\_{\mathrm{lin}} = \begin{cases} \dfrac{c'}{12{,}92} & \text{wenn } c' \le 0{,}04045 \\\\[1.2ex] \left(\dfrac{c' + 0{,}055}{1{,}055}\right)^{2{,}4} & \text{sonst} \end{cases}$$

Dann zählt jeder Kanal so viel, wie das Auge ihn wahrnimmt, Grün am meisten,
Blau am wenigsten:

$$L = 0{,}2126 \cdot R\_{\mathrm{lin}} + 0{,}7152 \cdot G\_{\mathrm{lin}} + 0{,}0722 \cdot B\_{\mathrm{lin}}$$

_task(luminance)

Das Verhältnis zweier Farben ist dann der Quotient ihrer Leuchtdichten, die
hellere oben, mit 0,05 auf beiden Seiten für das Licht, das immer auf einen
Bildschirm fällt:

$$\mathrm{Kontrast} = \frac{L\_{\mathrm{hell}} + 0{,}05}{L\_{\mathrm{dunkel}} + 0{,}05}$$

_task(grau-vorhersage)

_task(contrast-ratio)

<!-- after: grau-vorhersage -->
`#777777` auf Weiß hat 4,48 : 1 und reicht knapp nicht; `#767676`, eine
Stufe dunkler, hat 4,54 : 1 und ist das hellste Grau, das auf Weiß für
normalen Text genügt. Mit bloßem Auge siehst du den Unterschied nicht. Darum
rechnet man nach.
<!-- /after -->

<div class='hint try'>
<strong>Ausprobieren:</strong> Rechne mit deiner Funktion die Farben aus <code>style.css</code> nach: <code>#999999</code> auf Weiß, die grünen und roten Termine, der weiße Text auf dem blauen Knopf. Welches Paar ist am schlechtesten?
</div>

Ändere `style.css`, bis jede Schrift auf ihrem Hintergrund 4,5 : 1 erreicht,
auch im Banner und auf dem Knopf. Dunkler wird es, wenn du die Zahlen
kleiner machst: `#333333` statt `#999999`. Damit der ausgefallene Termin nicht
nur rot ist, schreib dazu, was mit ihm ist: `<strong>(fällt aus)</strong>`.

_task(kontrast)

### Struktur

Ein Screenreader springt von Überschrift zu Überschrift, so wie du beim
Überfliegen einer Seite. Die Ebenen sagen dabei, was wozu gehört: Nach einem
`<h1>` kommt `<h2>`, darunter `<h3>`. Die Seite der AG springt von `<h1>`
gleich zu `<h4>`, weil `<h4>` in `style.css` so groß ist, wie es aussehen
sollte. Die Größe einer Überschrift gehört ins CSS, ihre Ebene ins HTML.

Dazu kommen die Bereiche der Seite: `<nav>` für das Menü, `<main>` für den
Inhalt. Mit ihnen springt ein Screenreader direkt zum Inhalt, statt jedes Mal
das Menü anzuhören.

_task(ueberschriften)

_task(main-nav)

### Fokus und Bewegung

Wer mit der Tastatur arbeitet, muss sehen, wo er gerade ist. Browser zeichnen
dafür einen Rahmen um das Element, das den **Fokus** hat; `outline: none` in
`style.css` nimmt ihn allen weg. Lösch die Regel und gib dem Fokus einen
eigenen, gut sichtbaren Rahmen. `:focus-visible` gilt nur, wenn die Tastatur
den Fokus gebracht hat, nicht bei einem Klick mit der Maus:

```css
:focus-visible {
  outline: 3px solid #1a5fb4;
  outline-offset: 2px;
}
```

_task(fokus)

Was blinkt, lenkt ab, und manche Menschen bekommen von Bewegung auf dem
Bildschirm Kopfschmerzen oder Schwindel. Im Betriebssystem lässt sich »Bewegung
reduzieren« einschalten, und eine Seite kann das abfragen:

```css
@media (prefers-reduced-motion: reduce) {
  .scarcity {
    animation: none;
  }
}
```

Ob die Zeile mit dem Countdown überhaupt bleiben sollte, kommt im nächsten
Teil.

_task(bewegung)

### Prüfen

Browser haben Werkzeuge, die vieles davon selbst finden. In Chrome und Edge
heißt es **Lighthouse** (<kbd>F12</kbd>, Reiter »Lighthouse«, nur
»Barrierefreiheit« ankreuzen), in Firefox gibt es im selben Fenster den Reiter
»Barrierefreiheit«. Lass deine Seite prüfen.

Was ein Werkzeug nicht findet: ob ein Alt-Text zum Bild passt, ob ein Link
sagt, wohin er führt, ob die Reihenfolge der Seite Sinn ergibt. Dafür braucht
es einen Menschen, am besten einen, der die Seite wirklich mit Screenreader
oder Tastatur benutzt.

Websites von Behörden müssen barrierefrei sein und eine **Erklärung zur
Barrierefreiheit** haben, die sagt, was es noch nicht ist (das Handbuch,
[Was das Gesetz sagt](/reference/accessibility#was-das-gesetz-sagt)).

_task(erklaerung)

## Fair statt trickreich

Die Seite der AG ist jetzt für alle benutzbar. Ehrlich ist sie noch nicht: Der
Cookie-Banner, das Häkchen im Formular und der Countdown sind **Dark
Patterns**, Gestaltung, die Menschen zu etwas bringt, das sie gar nicht wollen.
Der Name stammt von dem britischen Designer Harry Brignull, der sie 2010 zu
sammeln begann. Das Handbuch zeigt die häufigsten unter
[Dark Patterns](/reference/dark-patterns).

Wenn du den ersten Teil ausgelassen hast, hol dir die Seite wie dort unter
[Die Seite holen](/fair-web#die-seite-holen) beschrieben.

### Der Banner

Der Banner der AG hat einen großen grünen Knopf »Alle akzeptieren« und
darunter, klein und hellgrau, »Einstellungen«. Ablehnen steht nirgends. Unten
sind zwei Banner nebeneinander, links einer wie auf vielen großen Seiten,
rechts ein fairer.

_task(banner-vorhersage)

<div class='dp-demo' data-demo='banner'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Lehne in beiden Bannern alles ab, was nicht nötig ist, und zähl mit. Wo musstest du suchen? Was ist links schon eingeschaltet, wenn du die Einstellungen öffnest?
</div>

Die Datenschutz-Grundverordnung verlangt, dass eine Einwilligung freiwillig ist
und dass Ablehnen so leicht geht wie Zustimmen. Bau den Banner der AG um: eine
Frage, die sagt, wofür das Cookie ist, und zwei Knöpfe derselben Art, einer
zum Ablehnen und einer zum Zustimmen. Beide rufen `closeBanner()` auf.

```html
<div class="cookie-banner" id="banner">
  <p>Dürfen wir mit einem Cookie zählen, welche Seiten du liest?</p>
  <button class="choice" onclick="closeBanner()">Ablehnen</button>
  <button class="choice" onclick="closeBanner()">Zustimmen</button>
</div>
```

_task(banner-fair)

### Das Häkchen

_task(doppelt-verneint)

<!-- after: doppelt-verneint -->
Wer das Häkchen stehen lässt, bekommt Werbung: »keine Angebote nicht verpassen«
heißt »Angebote bekommen«. Die doppelte Verneinung soll verwirren, und das
Häkchen ist schon gesetzt, damit nichts tun Zustimmen heißt.
<!-- /after -->

<div class='dp-demo' data-demo='wording'></div>

Ein faires Häkchen ist nicht gesetzt, und sein Text sagt geradeaus, was es
bedeutet: »Ja, schickt mir Neuigkeiten der AG per E-Mail.« Nimm `checked` weg
und schreib den Satz in ein `<label>` für das Häkchen.

_task(kein-haken)

### Der Countdown

»Nur noch 2 Plätze frei! Die Anmeldung endet in 05:00.« Der rote, blinkende
Satz soll Eile machen: Wer Angst hat, etwas zu verpassen, denkt weniger nach.

_task(countdown-vorhersage)

<!-- after: countdown-vorhersage -->
Nach dem Neuladen steht wieder 05:00 da. `script.js` zählt bei jedem Laden von
300 Sekunden herunter; die Anmeldung endet in Wahrheit am 10. Mai, und wie
viele Plätze frei sind, weiß die Seite gar nicht.
<!-- /after -->

<div class='dp-demo' data-demo='urgency'></div>

Nimm die Zeile mit dem Countdown aus `index.html` und das Zählen aus
`script.js`, und schreib stattdessen, was wirklich gilt: »Die Anmeldung ist bis
zum 10. Mai offen.« Wenn die Plätze wirklich knapp sind, darf das dastehen,
aber nur, wenn es stimmt.

_task(kein-countdown)

## Phishing

Eine Mail, die aussieht, als käme sie von der Schule, von einem Paketdienst
oder einer Bank, und die dich auf eine Seite lockt, die dein Passwort will:
Das ist **Phishing**, von »fishing«, Angeln, nach Passwörtern. Die Seite sieht
aus wie die echte, und wer dort sein Passwort eintippt, gibt es dem, der sie
gebaut hat.

Fast immer verraten sich solche Mails. Unten ist ein Postfach mit fünf Mails,
zwei davon Phishing. Alle Adressen enden auf `.example`: Diese Endung gehört
niemandem und ist für Beispiele reserviert, und jeder Dienst und jede Person in
den Mails ist ausgedacht.

### Hinweise finden

<div class='mail-demo'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Öffne jede Mail und klick auf alles, was dir seltsam vorkommt: den Absender, Sätze, den Anhang. Zeig mit der Maus auf jeden Link und lies unten in der Leiste, wohin er führt. Entscheide dann: Phishing oder echt? Findest du in beiden Phishing-Mails alle Hinweise?
</div>

_task(hinweise)

### Wem gehört ein Link?

Unten in der Leiste stand bei der ersten Phishing-Mail
`https://schule-am-see.example.konto-pruefung.example/login`. Vorne steht die
Adresse der Schule, und doch gehört der Link jemand anderem.

_task(wem-gehoert)

<!-- after: wem-gehoert -->
Eine Adresse besteht aus dem Protokoll (`https://`), dem **Rechnernamen** und
dem Pfad (`/login`). Der Rechnername wird von rechts gelesen: `.example` ist
die Endung, `konto-pruefung.example` die **Domain**, die jemand gekauft hat.
Alles davor sind Namen, die sich ihr Besitzer selbst ausdenkt, so viele er
will, auch `schule-am-see.example.konto-pruefung.example`. Wem ein Link gehört,
steht also am Ende des Rechnernamens, direkt vor dem ersten einzelnen
Schrägstrich, nicht am Anfang.
<!-- /after -->

Das kannst du programmieren. Ein **Link-Prüfer** liest aus einer Adresse den
Rechnernamen und daraus die Domain. Zuerst der Rechnername: Er steht hinter
`://` und endet vor dem ersten `/`. Zwei Tricks gibt es dabei. Ein `:` mit
einer Zahl dahinter ist ein Port (`:8080`) und gehört nicht zum Namen. Und ein
`@` trennt einen Benutzernamen vom Rechner:
`https://schule-am-see.example@konto-pruefung.example/` öffnet
`konto-pruefung.example`, alles vor dem `@` ist nur Tarnung.

_task(host)

Die Domain sind die letzten beiden Teile des Rechnernamens, die Teile durch
Punkte getrennt.

_task(real-domain)

<div class='hint'>
Für die Endungen <code>.example</code>, <code>.de</code> oder <code>.com</code> stimmt das. Es gibt aber
Endungen aus zwei Teilen, <code>.co.uk</code> in Großbritannien oder <code>.com.au</code> in
Australien: Dort gehören die letzten drei Teile zusammen. Browser haben dafür
eine Liste aller solcher Endungen, die <a href='https://publicsuffix.org/'>Public Suffix List</a>.
</div>

### Fast dieselbe Domain

Der Absender der ersten Phishing-Mail war `support@schuIe-am-see.example`, mit
einem großen I statt eines kleinen l. In vielen Schriften sehen beide gleich
aus. Andere Paare, die leicht zu verwechseln sind: die Ziffer `0` und das
`o`, die Ziffer `1` und das `l`, `rn` und `m`.

Schreib eine Funktion, die zwei Domains vergleicht und sagt, ob sie verschieden
sind und doch gleich aussehen. Ersetze dafür in beiden jedes Paar durch ein
Zeichen, zum Beispiel alle `I`, `1` durch `l`, `0` durch `o` und `rn` durch
`m`, und vergleiche dann.

_task(looks-like)

<div class='hint try'>
<strong>Ausprobieren:</strong> Prüfe mit deinen Funktionen beide Links aus den Phishing-Mails und den Link der AG. Welcher Link würde dein Prüfer melden, wenn er die Domain der Schule kennt? Fällt dir eine Adresse ein, die er nicht erkennt?
</div>

Echte Mailprogramme und Browser prüfen noch mehr: Schriftzeichen aus anderen
Alphabeten, die wie lateinische aussehen (ein kyrillisches а statt eines a),
Listen bekannter Phishing-Seiten, die jeden Tag wachsen. Ganz sicher ist kein
Filter. Darum zählt, was du selbst tust.

### Wenn es passiert ist

_task(passwort-wert)

<!-- after: passwort-wert -->
Mit deinem Passwort der Schule kann jemand deine Mails lesen und in deinem
Namen schreiben, an deine Klasse und deine Lehrkräfte, mit einem Link, dem sie
trauen, weil er von dir kommt. So verbreitet sich Phishing in einer Schule.
Und wer dasselbe Passwort auch woanders benutzt, verliert alle Konten auf
einmal.
<!-- /after -->

_task(nach-dem-klick)

Gegen ein gestohlenes Passwort hilft die **Anmeldung mit zweitem Faktor**:
Zum Passwort kommt ein Code, der jede halbe Minute wechselt und nur auf deinem
Handy entsteht, oder eine Bestätigung in einer App. Wer nur das Passwort hat,
kommt dann nicht hinein. Wie solche Codes entstehen, zeigt das Labor
[Zwei-Faktor-Codes](/totp).

Gut erklärt für alle, ohne Programmieren, stehen Phishing und andere Gefahren
im Netz beim Bundesamt für Sicherheit in der Informationstechnik (BSI) unter
[Spam, Phishing & Co](https://www.bsi.bund.de/dok/507910), mit einer Liste
zum Prüfen einer Mail unter
[Wie erkenne ich Phishing?](https://www.bsi.bund.de/DE/Themen/Verbraucherinnen-und-Verbraucher/Cyber-Sicherheitslage/Methoden-der-Cyber-Kriminalitaet/Spam-Phishing-Co/Passwortdiebstahl-durch-Phishing/Wie-erkenne-ich-Phishing-in-E-Mails-und-auf-Webseiten/wie-erkenne-ich-phishing-in-e-mails-und-auf-webseiten_node.html),
und für Jüngere im Internet-ABC im Lernmodul
[Fiese Tricks, Lügen und Betrug im Internet](https://www.internet-abc.de/lernmodul-online-sicherheit-scam/).

## Wie geht es weiter?

Die Seite der AG ist jetzt fair und für alle benutzbar; mach dasselbe mit
deiner eigenen Seite aus [Statische Webseiten](/static-website) und lass
Lighthouse darüber laufen. Was eine Webseite über dich erfährt, ohne dass du
etwas anklickst, zeigt [Was eine Webseite über dich erfährt](/tracking). Und
wie ein Server Passwörter aufbewahren muss, damit sie bei einem Einbruch nicht
alle offen daliegen, steht in [Kryptographie im Terminal](/crypto).
