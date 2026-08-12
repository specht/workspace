<div class='meta_no'>
image: title.webp
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3,h4">
</div>

# Interaktive Geschichten schreiben

<p class='abstract'>
Mit BIF kannst du interaktive Geschichten schreiben, bei denen die Leserinnen und Leser selbst entscheiden, wie es weitergeht. Die einzelnen Abschnitte deiner Geschichte schreibst du als einfache Markdown-Dateien und verbindest sie miteinander. In diesem Tutorial entwickelst du Schritt für Schritt eine kleine Geschichte und lernst dabei die wichtigsten Bausteine von BIF kennen: Seiten, Entscheidungen, Verzweigungen, lokale Aktionen, Variablen und Bedingungen.
</p>

## Repository klonen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>.

<!-- Screenshot: Workspace ohne geöffneten Ordner -->

<img class='full' src='fresh-start.webp'>

Für diese Anleitung brauchst du das BIF-Repository. Klicke auf den blauen Button »Clone Repository« und gib die folgende URL ein:

```text
https://github.com/specht/bif.git
```

Bestätige anschließend mit <span class='key'>Enter</span>.

<!-- Screenshot: Clone Repository mit eingetragener BIF-URL -->

<img class='full' src='git-clone.webp'>

Als nächstes musst du angeben, in welches Verzeichnis das Repository geklont werden soll. Bestätige den Standardpfad

```text
/workspace/
```

mit <span class='key'>Enter</span>.

<!-- Screenshot: Auswahl von /workspace/ -->

<img class='full' src='confirm-clone-path.webp'>

Beantworte anschließend die Frage »Would you like to open the cloned repository?« mit »Open«.

<!-- Screenshot: Dialog zum Öffnen des geklonten Repositorys -->

<img class='full' src='open-yes-no.webp'>

<!-- Screenshot: geöffnetes BIF-Projekt im Explorer -->

<img class='full' src='bif-project.webp'>

Wenn alles geklappt hat, siehst du links im Explorer unter anderem den Ordner `pages-starter` sowie die Dateien `config.js` und `index.html`.

## Geschichte starten

Öffne im Explorer den Ordner `pages-starter` und darin die Datei `1.md`.

<!-- Screenshot: 1.md geöffnet -->

<img class='full' src='story-begin.webp'>

Dort steht bereits der Anfang unserer kleinen Geschichte:

```markdown
# Nach Schulschluss

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.
```

Die Endung `.md` steht für [Markdown](https://de.wikipedia.org/wiki/Markdown). Markdown ist eine einfache Schreibweise für Texte und eignet sich gut für interaktive Geschichten.

Die Zeile

```markdown
# Nach Schulschluss
```

ist der **Titel der Geschichte**. Ein einzelnes `#` steht in Markdown für die größte Überschrift.

In einer BIF-Geschichte verwenden wir diese Überschrift für den Titel der gesamten Geschichte. Sie steht deshalb nur am Anfang von `1.md`. Die weiteren Seiten brauchen normalerweise keine eigene Überschrift.

Falls du innerhalb einer Seite doch einmal eine Zwischenüberschrift brauchst, verwendest du dafür `##`.

<div class='hint'>
Die Datei <code>1.md</code> hat bei BIF eine besondere Bedeutung: Jede Geschichte beginnt bei Seite 1.
</div>

## Vorschau starten

Damit du deine Geschichte im Browser ausprobieren kannst, brauchst du die Erweiterung **Live Server von Ritwick Dey**.

Öffne links die Extensions, suche nach `Live Server` und installiere die Erweiterung von **Ritwick Dey**. Achte auf den Herausgeber, denn es gibt mehrere Erweiterungen mit sehr ähnlichen Namen.

<!-- Screenshot: Live Server von Ritwick Dey in Extensions -->

<img class='full' src='live-server.webp'>

Nach der Installation solltest du unten rechts den Eintrag »Go Live« sehen.

<!-- Screenshot: Go-Live-Button -->

<img src='go-live.webp' class='r' style='width: 21em;'>

Klicke auf »Go Live«. Es öffnet sich ein neuer Tab mit deiner Geschichte.

BIF startet standardmäßig in der **Entwicklungsansicht**. Dort siehst du neben der Geschichte bereits den Graphen. Am Anfang besteht er nur aus einem einzigen Knoten für `1.md`. Du kannst die Geschichte direkt in dieser Ansicht ausprobieren; der Graph aktualisiert sich während des Schreibens automatisch.

<!-- Screenshot: erste BIF-Seite im Browser -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-page.webp' style='max-width: 100%;'>
</div>

<div class='hint'>
Tipp: Ziehe den Workspace und die Vorschau nebeneinander. Dann kannst du links schreiben und rechts direkt ausprobieren, was sich verändert hat.
</div>

## Zweite Seite

Unsere Geschichte soll nicht auf der ersten Seite stehen bleiben.

Erstelle im Ordner `pages-starter` eine neue Datei:

```text
2.md
```

Schreibe hinein:

```markdown
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus. Am Ende steht ein verschlossener Materialschrank.
```

Speichere die Datei.

Damit besteht die Geschichte bereits aus zwei Seiten. Im Browser kommst du allerdings noch nicht von der ersten zur zweiten Seite. Dafür brauchen wir eine Entscheidung.

## Entscheidungen

Öffne wieder `1.md` und ergänze am Ende:

```markdown
- [Gehe in den Flur.](2)
```

Die vollständige Datei sieht jetzt so aus:

```markdown
# Nach Schulschluss

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.

- [Gehe in den Flur.](2)
```

Speichere die Datei und probiere die Geschichte im Browser aus.

<!-- Screenshot: erste anklickbare Entscheidung -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-choice.webp' style='max-width: 100%;'>
</div>

Die Zeile

```markdown
- [Gehe in den Flur.](2)
```

besteht aus zwei wichtigen Teilen.

Der Text

```text
[Gehe in den Flur.]
```

ist das, was die Leserin oder der Leser sieht.

Die Zahl

```text
(2)
```

bedeutet: Nach dieser Entscheidung geht die Geschichte mit `2.md` weiter.

<div class='hint books'>
Achtung: Du schreibst bei einer Entscheidung <code>(2)</code> und nicht <code>(2.md)</code>. BIF weiß bereits, dass sich die Zahl auf eine Markdown-Datei bezieht.
</div>

## Verzweigungen

Eine interaktive Geschichte wird interessanter, wenn nicht immer nur ein einziger Weg möglich ist.

Erstelle die Datei `3.md`:

```markdown
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Gehe zurück in den Flur.](2)
```

Erstelle außerdem die Datei `4.md`:

```markdown
Im Treppenhaus ist es still. Auf dem Absatz liegt nur ein vergessener Turnbeutel.

- [Gehe zurück in den Flur.](2)
```

Öffne jetzt `2.md` und ergänze zwei Entscheidungen:

```markdown
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus. Am Ende steht ein verschlossener Materialschrank.

- [Sieh im Büro nach.](3)
- [Gehe ins Treppenhaus.](4)
```

Speichere die Dateien und probiere beide Wege aus.

<!-- Screenshot: zwei Entscheidungen im Flur -->

<div style='text-align: center; margin: 1em 0;'>
<img src='branching-story.webp' style='max-width: 100%;'>
</div>

Die Geschichte verzweigt sich jetzt auf Seite 2. Beide Wege führen anschließend wieder zurück in den Flur.

## Graph

Der Graph ist in der Entwicklungsansicht bereits seit dem Start sichtbar. Jetzt, nachdem unsere Geschichte mehrere Seiten und Verzweigungen hat, lohnt sich ein genauerer Blick darauf.

<!-- Screenshot: Graph der bisherigen Geschichte -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-graph.webp' style='max-width: 100%;'>
</div>

Jede Datei wird als Knoten dargestellt. Die Pfeile zeigen, welche Entscheidungen von einer Seite zu einer anderen führen.

Bei unserer Geschichte sieht man jetzt zum Beispiel:

- Seite 1 führt zu Seite 2.
- Seite 2 führt zu Seite 3 oder Seite 4.
- Seite 3 und Seite 4 führen zurück zu Seite 2.

So wird auch sichtbar, dass `1.md` nur der Einstieg ist. Später kehren wir nicht mehr auf diese Seite zurück.

<div class='hint'>
Je größer deine Geschichte wird, desto nützlicher wird der Graph. Du kannst damit schnell erkennen, welche Wege möglich sind und ob Teile deiner Geschichte gar nicht erreicht werden können.
</div>

## Lokale Entscheidungen

Nicht jede Entscheidung soll zu einer anderen Seite führen.

Vielleicht möchtest du mit einer Person sprechen, einen Gegenstand untersuchen oder eine Schublade öffnen. Dafür gibt es in BIF **lokale Entscheidungen**.

Öffne `3.md` und ändere den Inhalt zu:

```markdown
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Frage nach dem Materialschrank.](.)

    > „Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür.“

- [Gehe zurück in den Flur.](2)
```

Der wichtige Unterschied ist der Punkt:

```text
(.)
```

Er bedeutet: **Bleibe auf dieser Seite.**

Der eingerückte Text darunter erscheint erst, nachdem die Entscheidung ausgewählt wurde.

<!-- Screenshot: lokale Entscheidung vor dem Anklicken -->

<div style='text-align: center; margin: 1em 0;'>
<img src='dialogue-before.webp' style='max-width: 100%;'>
</div>

Klicke auf die Frage.

<!-- Screenshot: lokale Entscheidung nach dem Anklicken -->

<div style='text-align: center; margin: 1em 0;'>
<img src='dialogue-after.webp' style='max-width: 100%;'>
</div>

Die vier Leerzeichen vor der Antwort sind wichtig. Sie zeigen BIF, dass dieser Text zu der Entscheidung darüber gehört.

Lokale Entscheidungen eignen sich zum Beispiel für:

- Gespräche
- das Untersuchen eines Gegenstands
- das Lesen eines Briefs
- das Öffnen einer Schublade
- das Betätigen eines Schalters
- kleine Aktionen, die keinen neuen Ort benötigen

Wechsle danach noch einmal kurz zum Graphen. Die Frage im Büro hat keinen neuen Knoten erzeugt, weil sie innerhalb derselben Seite stattfindet.

## Variablen

Bisher hängt der Verlauf nur davon ab, welche Seite gerade geöffnet wird. Eine Geschichte kann sich aber auch etwas **merken**.

In unserem Beispiel soll gespeichert werden, ob du den Schlüssel aus dem Büro genommen hast.

Öffne `1.md` und füge direkt unter dem Titel ein:

```html
<script>
has_key = false;
</script>
```

Die Datei sieht jetzt so aus:

```markdown
# Nach Schulschluss

<script>
has_key = false;
</script>

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.

- [Gehe in den Flur.](2)
```

`has_key` ist eine Variable. Der Wert

```text
false
```

bedeutet hier: Du hast den Schlüssel noch nicht.

Wir setzen diesen Anfangswert in `1.md`, weil diese Seite nur einmal am Anfang besucht wird.

Öffne anschließend `3.md` und ergänze eine weitere lokale Entscheidung:

```markdown
- [Nimm den Schlüssel.](.)

    <script>
    has_key = true;
    </script>

    Du nimmst den kleinen Schlüssel vom Haken.
```

Die vollständige Datei kann jetzt so aussehen:

```markdown
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Frage nach dem Materialschrank.](.)

    > „Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür.“

- [Nimm den Schlüssel.](.)

    <script>
    has_key = true;
    </script>

    Du nimmst den kleinen Schlüssel vom Haken.

- [Gehe zurück in den Flur.](2)
```

Wenn die Entscheidung ausgewählt wird, ändert sich `has_key` von `false` zu `true`.

Öffne in der Entwicklungsansicht den Bereich **State** und beobachte den Wert beim Spielen.

<!-- Screenshot: Schlüssel genommen / State mit has_key: true -->

<div style='text-align: center; margin: 1em 0;'>
<img src='take-key.webp' style='max-width: 100%;'>
</div>

Damit hat eine Entscheidung zum ersten Mal etwas verändert, das auf einer späteren Seite noch wichtig sein kann.

<div class='hint'>
Auf einer Seite, die mehrfach besucht werden kann, darfst du einen Anfangswert nicht einfach jedes Mal neu setzen. Für solche Fälle gibt es später zum Beispiel <code>has_key ??= false;</code>. Diese Schreibweise setzt den Wert nur dann, wenn die Variable noch gar keinen Wert hat. Für unsere Einstiegsgeschichte brauchen wir das nicht, weil <code>1.md</code> nur einmal besucht wird.
</div>

## Bedingungen

Jetzt soll sich der Schlüssel auf den weiteren Verlauf auswirken.

Erstelle zuerst die Datei `5.md`:

```markdown
Der Schlüssel passt.

Im Materialschrank liegt deine Projektmappe zwischen zwei Kartons. Du steckst sie ein. Jetzt kannst du endlich nach Hause.

**Ende.**
```

Öffne danach `2.md` und ergänze eine Entscheidung zum Materialschrank:

```markdown
- [Öffne den Materialschrank.](5){condition="has_key"}
```

Die vollständige Datei sieht jetzt so aus:

```markdown
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus. Am Ende steht ein verschlossener Materialschrank.

- [Sieh im Büro nach.](3)
- [Gehe ins Treppenhaus.](4)
- [Öffne den Materialschrank.](5){condition="has_key"}
```

Das Besondere steht hinter der Entscheidung:

```text
{condition="has_key"}
```

`condition` bedeutet **Bedingung**.

Die Entscheidung wird nur angezeigt, wenn `has_key` den Wert `true` hat.

Starte die Geschichte neu und probiere es aus:

1. Gehe in den Flur.
2. Die Entscheidung zum Öffnen des Materialschranks sollte noch nicht sichtbar sein.
3. Gehe ins Büro.
4. Nimm den Schlüssel.
5. Gehe zurück in den Flur.

Jetzt sollte die neue Entscheidung erscheinen.

<!-- Screenshot: Flur ohne Schlüssel -->

<div style='text-align: center; margin: 1em 0;'>
<img src='locked-door.webp' style='max-width: 100%;'>
</div>

<!-- Screenshot: Flur mit Schlüssel und zusätzlicher Entscheidung -->

<div style='text-align: center; margin: 1em 0;'>
<img src='unlocked-door.webp' style='max-width: 100%;'>
</div>

Bedingungen können auch normalen Text steuern. Ergänze in `2.md` direkt vor den Entscheidungen:

```html
<p condition="!has_key">
Der Materialschrank ist abgeschlossen.
</p>

<p condition="has_key">
Du hast den kleinen Schlüssel dabei.
</p>
```

Das Ausrufezeichen in

```text
!has_key
```

bedeutet hier ungefähr **nicht**. Der erste Absatz wird also nur angezeigt, solange du den Schlüssel noch nicht hast.

Damit kann sich sogar die Beschreibung desselben Ortes verändern, obwohl du immer wieder dieselbe Datei `2.md` besuchst.

Du kannst außerdem verhindern, dass der Schlüssel bei einem späteren Besuch im Büro noch einmal angeboten wird. Ergänze die Bedingung direkt an der lokalen Entscheidung:

```markdown
- [Nimm den Schlüssel.](.){condition="!has_key"}
```

Nun verschwindet diese Möglichkeit, sobald `has_key` den Wert `true` hat.

<div class='hint'>
Mit Variablen und Bedingungen können frühere Entscheidungen später Folgen haben. Eine interaktive Geschichte muss deshalb nicht aus immer neuen Verzweigungen bestehen.
</div>

## Ende

Öffne jetzt den Materialschrank.

Die Datei `5.md` enthält keine weitere Entscheidung. Deshalb erkennt BIF diese Seite automatisch als mögliches Ende der Geschichte.

<!-- Screenshot: Ende mit Neustart-Button -->

Am Ende erscheint der Button

```text
Gedrückt halten, um erneut zu spielen
```

Er muss kurz gedrückt gehalten werden. So wird verhindert, dass der gesamte Spielstand versehentlich durch einen einfachen Klick gelöscht wird.

Beim Neustart beginnt die Geschichte wieder bei `1.md`. Dadurch wird auch

```js
has_key = false;
```

erneut ausgeführt und der Anfangszustand ist wieder hergestellt.

## Fehler finden

Beim Schreiben passieren leicht Fehler.

Vielleicht verweist eine Entscheidung auf eine Seite, die gar nicht existiert. Vielleicht hast du einen Dateinamen falsch geschrieben oder eine Seite angelegt, zu der kein Weg führt.

BIF überprüft die Geschichte deshalb während des Schreibens.

Probiere absichtlich einen Fehler aus. Ändere in `2.md` vorübergehend:

```markdown
- [Sieh im Büro nach.](3)
```

zu:

```markdown
- [Sieh im Büro nach.](99)
```

obwohl es keine Datei `99.md` gibt.

Öffne anschließend die Entwicklungsansicht und den Bereich **Problems**.

<!-- Screenshot: Problems-Ansicht mit fehlender Seite 99 -->

<img class='full' src='missing-page-problem.webp'>

BIF sollte jetzt anzeigen, dass das Ziel der Entscheidung fehlt.

Korrigiere die Zahl danach wieder zu:

```markdown
- [Sieh im Büro nach.](3)
```

Die Fehlermeldung sollte verschwinden.

BIF kann unter anderem auf folgende Probleme hinweisen:

- fehlende Seiten
- Seiten, die von nirgendwo erreicht werden können
- fehlende Bilder oder andere Dateien
- ungültige Bedingungen
- Fehler in Skripten

<div class='hint'>
Schau beim Schreiben regelmäßig auf den Graphen und die Problems-Ansicht. Gerade bei größeren Geschichten findest du damit viele Fehler, bevor du die Geschichte komplett durchspielen musst.
</div>

## Weitere Möglichkeiten

Mit den Bausteinen aus der kleinen Geschichte kannst du bereits viele interaktive Geschichten schreiben. BIF kann darüber hinaus noch einiges mehr.

### Markdown

Für die normalen Texte kannst du die üblichen Markdown-Schreibweisen verwenden.

Fetter Text:

```markdown
Auf dem Tisch liegt ein **kleiner Schlüssel**.
```

Kursiver Text:

```markdown
Aus dem Nebenraum hörst du ein *leises Klopfen*.
```

Ein Zitat:

```markdown
> „Ich würde diese Tür nicht öffnen.“
```

Eine Zwischenüberschrift innerhalb einer Seite:

```markdown
## Ein Hinweis
```

Denke daran: `#` verwenden wir für den Titel der gesamten Geschichte. Für Unterüberschriften innerhalb einer Seite beginnt die Hierarchie bei `##`.

### Bilder

Bilder speicherst du am besten innerhalb deines Geschichtenordners, zum Beispiel:

```text
pages-meine-geschichte/
├── 1.md
├── 2.md
└── images/
    └── door.jpg
```

In Markdown kannst du das Bild so einfügen:

```markdown
![Eine verschlossene Tür](images/door.jpg)
```

Der Text in den eckigen Klammern beschreibt das Bild und hilft zum Beispiel Menschen, die einen Screenreader verwenden.

<!-- Screenshot: BIF-Seite mit Bild -->

<div style='text-align: center; margin: 1em 0;'>
<img src='story-image.webp' style='max-width: 100%;'>
</div>

### Audio und Video

Auch Audio- und Videodateien können Teil einer Geschichte sein:

```html
<audio controls src="audio/door.mp3"></audio>
```

oder:

```html
<video controls>
    <source src="video/train.webm" type="video/webm">
</video>
```

### Werte im Text

Variablen können nicht nur `true` und `false`, sondern auch Zahlen oder Texte enthalten.

Zum Beispiel:

```html
<script>
points = 3;
</script>
```

Mit doppelten eckigen Klammern kannst du einen Wert in den Text einsetzen:

```markdown
Du hast [[ points ]] Punkte.
```

Wenn eine Variable auf einer Seite einen Anfangswert bekommen soll, die mehrfach besucht werden kann, ist `??=` praktisch:

```js
points ??= 0;
```

Der Wert wird damit nur gesetzt, wenn `points` noch nicht existiert. Werte wie `0`, `false` oder ein bereits vorhandener Text bleiben unverändert.

### Zufall

Für manche Geschichten kann Zufall interessant sein.

```js
Math.w6()
```

würfelt mit einem sechsseitigen Würfel.

```js
Math.chance(50)
```

liefert mit einer Wahrscheinlichkeit von 50 Prozent `true`.

Zufall eignet sich gut für kleine Überraschungen. Wichtige Folgen sind oft interessanter, wenn sie von vorherigen Entscheidungen abhängen.

### Graph-Gruppen

Bei größeren Geschichten kannst du Seiten im Graphen zu Bereichen zusammenfassen.

Ganz oben in einer Seite kann zum Beispiel stehen:

```markdown
<!-- Schule -- Büro -->
```

Eine andere Seite könnte beginnen mit:

```markdown
<!-- Schule -- Flur -->
```

Diese Kommentare sind für die Leserinnen und Leser unsichtbar. Sie helfen nur dabei, den Graphen übersichtlich zu halten.

<!-- Screenshot: Graph mit gruppierten Seiten -->

<div style='text-align: center; margin: 1em 0;'>
<img src='grouped-graph.webp' style='max-width: 100%;'>
</div>

### JavaScript

Für normale Entscheidungen reichen die Markdown-Links fast immer aus:

```markdown
- [Gehe nach links.](5)
- [Gehe nach rechts.](6)
```

Für besondere Fälle kann BIF Entscheidungen auch aus JavaScript erzeugen. Das ist eher eine fortgeschrittene Möglichkeit und normalerweise nicht nötig.

<div class='hint'>
Benutze zusätzliche Technik nur dann, wenn sie deiner Geschichte etwas bringt. Mehr Variablen, Skripte oder Verzweigungen machen eine Geschichte nicht automatisch besser.
</div>

### Geschichte prüfen

Während du arbeitest, prüft BIF die Geschichte automatisch. Am Ende kannst du zusätzlich im Terminal eine vollständige Überprüfung starten:

```bash
npm run check
```

<!-- Screenshot: npm run check ohne Fehler -->

<img class='full' src='check-story.webp'>

Wenn keine Fehler gemeldet werden, ist die technische Struktur der Geschichte in Ordnung.

Das bedeutet noch nicht, dass jede Entscheidung sinnvoll oder jeder Text fertig ist. Spiele die Geschichte deshalb selbst noch einmal durch und lasse sie am besten auch von jemand anderem ausprobieren.

## Eigene Geschichte

Die kleine Geschichte **Nach Schulschluss** war nur dazu da, die Technik kennenzulernen.

Für deine eigene Geschichte kannst du einen neuen Ordner anlegen, zum Beispiel:

```text
pages-meine-geschichte
```

Lege darin wieder eine `1.md` an.

Anschließend wählst du den neuen Geschichtenordner in `config.js` aus:

```js
export const path = "pages-meine-geschichte";
```

Damit beginnt dein eigenes Projekt wieder ganz klein.

### Eine Idee entwickeln

Bevor du viele Seiten anlegst, überlege dir zunächst, worum deine Geschichte geht.

**Ausgangssituation**  
Wo beginnt die Geschichte? Was ist gerade passiert?

**Figur**  
Wer handelt in der Geschichte? Was will diese Person?

**Ziel**  
Was soll gefunden, erreicht, verhindert oder herausgefunden werden?

**Setting**  
Wo und wann spielt die Geschichte? Was macht diesen Ort interessant?

**Konflikt oder Hindernis**  
Warum lässt sich das Ziel nicht einfach sofort erreichen?

**Entscheidungen**  
Was kann die Leserin oder der Leser wirklich entscheiden?

**Folgen**  
Welche Entscheidungen sollen später noch eine Rolle spielen?

**Ende**  
Woran merkt man, dass die Geschichte abgeschlossen ist? Kann es verschiedene Enden geben?

Eine gute Entscheidung ist meistens interessanter als nur:

```text
Gehe nach links.
Gehe nach rechts.
```

Beide Möglichkeiten sollten einen Grund haben. Eine Entscheidung kann etwas über die Figur zeigen, eine Information preisgeben, einen Gegenstand kosten, Vertrauen verändern oder erst später Folgen haben.

### Ideen

Falls dir noch eine Ausgangssituation fehlt, kannst du zum Beispiel mit einer dieser Ideen beginnen:

- Etwas Wichtiges ist verschwunden.
- Du bekommst eine Nachricht, die nicht für dich bestimmt war.
- Du musst rechtzeitig einen bestimmten Ort erreichen.
- Du kommst an einen Ort, an dem etwas nicht stimmt.
- Eine Person erzählt dir etwas, aber du weißt nicht, ob sie die Wahrheit sagt.
- Du musst dich zwischen zwei Menschen oder zwei Zielen entscheiden.
- Du erzählst eine Sage oder ein Märchen aus der Sicht einer Nebenfigur.
- Du lässt die Leserinnen und Leser eine historische Situation aus einer bestimmten Perspektive erleben.

Beginne nicht sofort mit zwanzig oder dreißig Seiten. Ein paar Seiten reichen für den Anfang. Spiele sie durch, schau auf den Graphen und erweitere die Geschichte Schritt für Schritt.

Du musst auch nicht jede Funktion von BIF verwenden. Eine kleine Geschichte mit guten Entscheidungen ist besser als eine komplizierte Geschichte voller Technik, die eigentlich nichts bewirkt.