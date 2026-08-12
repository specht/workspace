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
Mit BIF kannst du interaktive Geschichten schreiben, bei denen die Leserinnen und Leser selbst entscheiden, wie es weitergeht. Die einzelnen Abschnitte deiner Geschichte schreibst du als einfache Markdown-Dateien und verbindest sie miteinander. Später kannst du Dialoge, Variablen, Bedingungen und Bilder hinzufügen. In diesem Tutorial lernst du die wichtigsten Möglichkeiten von BIF kennen und entwickelst anschließend deine eigene interaktive Geschichte.
</p>

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke einfach den Shortcut für »Ordner schließen«: <span class='key'>Strg</span><span class='key'>K</span> und dann <span class='key'>F</span>. Dein Workspace sollte jetzt ungefähr so aussehen:

<!-- Screenshot: Workspace ohne geöffneten Ordner -->

<img class='full' src='fresh-start.webp'>

## Repository klonen

Für diese Anleitung brauchst du das BIF-Repository. Klicke auf den blauen Button »Clone Repository« und gib die folgende URL ein:

```bash
https://github.com/specht/bif.git
```

Bestätige anschließend mit <span class='key'>Enter</span>.

<!-- Screenshot: Clone Repository mit eingetragener BIF-URL -->

<img class='full' src='git-clone.webp'>

Als nächstes musst du angeben, in welches Verzeichnis das Repository geklont werden soll. Bestätige den Standardpfad `/workspace/` mit <span class='key'>Enter</span>.

<!-- Screenshot: Auswahl von /workspace/ -->

<img class='full' src='confirm-clone-path.webp'>

Beantworte die Frage »Would you like to open the cloned repository?« mit »Open«.

<!-- Screenshot: Dialog zum Öffnen des geklonten Repositorys -->

<img class='full' src='open-yes-no.webp'>

Wenn alles geklappt hat, siehst du links im Explorer unter anderem die Dateien `config.js` und `index.html`.

<!-- Screenshot: geöffnetes BIF-Projekt im Explorer -->

<img class='full' src='bif-project.webp'>

Im Hackschule-Workspace ist außerdem die Erweiterung »BIF Authoring Tools« installiert. Sie erkennt das BIF-Projekt automatisch und kümmert sich im Hintergrund um die Werkzeuge, die für die Entwicklungsansicht benötigt werden.

<div class='hint'>
Du musst für BIF normalerweise keine Befehle im Terminal eingeben. Speichere einfach deine Dateien und aktualisiere bei Bedarf die Vorschau.
</div>

## Eine eigene Geschichte anlegen

Eine BIF-Geschichte besteht aus mehreren Markdown-Dateien. Wir legen für unsere kleine Beispielgeschichte zunächst einen neuen Ordner an.

Klicke im Explorer mit der rechten Maustaste in einen freien Bereich und wähle »New Folder«. Nenne den Ordner:

```text
pages-meine-geschichte
```

<!-- Screenshot: neuer Ordner pages-meine-geschichte -->

<img class='full' src='new-story-folder.webp'>

Öffne jetzt die Datei `config.js`. Dort wird festgelegt, in welchem Ordner sich die Geschichte befindet.

Ersetze den bisherigen Inhalt durch:

```js
export const path = "pages-meine-geschichte";
```

Speichere die Datei mit <span class='key'>Strg</span><span class='key'>S</span>.

<div class='hint'>
Der Name in <code>config.js</code> muss genau mit dem Namen deines Geschichtenordners übereinstimmen.
</div>

## Die erste Seite

Erstelle im Ordner `pages-meine-geschichte` eine neue Datei mit dem Namen:

```text
1.md
```

Schreibe hinein:

```markdown
# Der Flur

Du stehst in einem leeren Flur. Eine Tür führt in ein kleines Büro.
```

Die Endung `.md` steht für [Markdown](https://de.wikipedia.org/wiki/Markdown). Markdown ist eine einfache Schreibweise, mit der du normalen Text formatieren kannst.

Die Zeile

```markdown
# Der Flur
```

ist eine Überschrift. Das `#` am Anfang bedeutet, dass es sich um eine große Überschrift handelt.

Die Datei `1.md` hat bei BIF eine besondere Bedeutung: **Jede Geschichte beginnt bei Seite 1.**

Speichere die Datei.

## Vorschau starten

Damit du deine Geschichte im Browser ausprobieren kannst, brauchst du die Erweiterung »Live Server«.

Falls du sie noch nicht installiert hast, öffne links die Extensions, suche nach »Live Server« und klicke auf »Install«.

<!-- Screenshot: Live Server in Extensions -->

<img class='full' src='live-server.webp'>

Wenn alles geklappt hat, solltest du unten rechts den Eintrag »Go Live« sehen.

<!-- Screenshot: Go-Live-Button -->

<img src='go-live.webp' class='r' style='width: 21em;'>

Klicke auf »Go Live«. Es öffnet sich ein neuer Tab mit deiner Geschichte.

<div style='clear: both;'></div>

<!-- Screenshot: erste BIF-Seite im Browser -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-page.webp' style='max-width: 100%;'>
</div>

<div class='hint'>
Tipp: Ziehe den Workspace und die Vorschau nebeneinander. Dann kannst du links schreiben und rechts direkt ausprobieren, was sich verändert hat.
</div>

## Eine zweite Seite

Eine interaktive Geschichte wird interessant, wenn die Leserinnen und Leser Entscheidungen treffen können.

Erstelle im Ordner `pages-meine-geschichte` eine zweite Datei:

```text
2.md
```

Schreibe hinein:

```markdown
# Das Büro

Eine Person sitzt an einem Schreibtisch und sieht von ihrem Bildschirm auf.

- [Gehe zurück in den Flur.](1)
```

Öffne anschließend wieder `1.md` und ergänze dort eine Entscheidung:

```markdown
# Der Flur

Du stehst in einem leeren Flur. Eine Tür führt in ein kleines Büro.

- [Gehe in das Büro.](2)
```

Speichere beide Dateien und probiere die Geschichte aus.

<!-- Screenshot: erste anklickbare Entscheidung -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-choice.webp' style='max-width: 100%;'>
</div>

Die Zeile

```markdown
- [Gehe in das Büro.](2)
```

besteht aus zwei wichtigen Teilen:

```text
[Gehe in das Büro.]
```

ist der Text, den die Leserin oder der Leser sieht.

Die Zahl

```text
(2)
```

bedeutet: Wenn diese Entscheidung ausgewählt wird, geht die Geschichte mit `2.md` weiter.

<div class='hint books'>
Achtung: Du schreibst bei einer Entscheidung <code>(2)</code> und nicht <code>(2.md)</code>. BIF weiß bereits, dass sich die Zahl auf eine Markdown-Datei bezieht.
</div>

## Verzweigungen

Natürlich kann eine Seite auch mehrere Entscheidungen anbieten.

Erstelle eine neue Datei `3.md`:

```markdown
# Das Treppenhaus

Eine Treppe führt nach oben und nach unten.

- [Gehe zurück in den Flur.](1)
```

Ergänze jetzt in `1.md` eine zweite Entscheidung:

```markdown
# Der Flur

Du stehst in einem leeren Flur. Eine Tür führt in ein kleines Büro. Daneben befindet sich das Treppenhaus.

- [Gehe in das Büro.](2)
- [Gehe in das Treppenhaus.](3)
```

Jetzt kann sich die Geschichte verzweigen.

<!-- Screenshot: zwei Entscheidungen auf Seite 1 -->

<div style='text-align: center; margin: 1em 0;'>
<img src='branching-story.webp' style='max-width: 100%;'>
</div>

**Aufgabe:** Erstelle eine vierte Seite mit einem neuen Ort und füge irgendwo in deiner Geschichte eine Entscheidung ein, die zu dieser Seite führt.

Du kannst zum Beispiel einen Keller, einen Klassenraum, einen Dachboden oder einen ganz anderen Ort verwenden.

<div style='display: none;'>

Eine mögliche Lösung wäre eine Datei `4.md`:

```markdown
# Der Keller

Eine einzelne Lampe beleuchtet den Keller.

- [Gehe zurück in den Flur.](1)
```

und die zusätzliche Entscheidung in `1.md`:

```markdown
- [Gehe in den Keller.](4)
```

</div>

## Der Graph

Schon bei wenigen Seiten kann es schwierig werden, den Überblick über alle Verbindungen zu behalten. Deshalb kann BIF deine Geschichte als **Graphen** anzeigen.

Oben in der Entwicklungsansicht findest du einen kleinen Button, mit dem du zwischen der Geschichte und dem Graphen wechseln kannst.

<!-- Screenshot: Button zum Wechseln zwischen Geschichte und Graph -->

<img class='full' src='graph-button.webp'>

Wechsle zur Graphansicht.

<!-- Screenshot: Graph der kleinen Beispielgeschichte -->

<div style='text-align: center; margin: 1em 0;'>
<img src='first-graph.webp' style='max-width: 100%;'>
</div>

Jede Seite deiner Geschichte wird als Knoten dargestellt. Die Pfeile zeigen die Entscheidungen zwischen den Seiten.

Bei unserer Geschichte führt zum Beispiel ein Pfeil von Seite 1 zu Seite 2, weil in `1.md` die folgende Entscheidung steht:

```markdown
- [Gehe in das Büro.](2)
```

<div class='hint'>
Je größer deine Geschichte wird, desto nützlicher wird der Graph. Du kannst damit schnell erkennen, welche Wege möglich sind und ob Teile deiner Geschichte gar nicht erreicht werden können.
</div>

## Text mit Markdown gestalten

Die Seiten deiner Geschichte sind normale Markdown-Dateien. Deshalb kannst du deinen Text auf einfache Weise gestalten.

Mit zwei Sternchen wird Text **fett**:

```markdown
Auf dem Tisch liegt ein **kleiner Schlüssel**.
```

Mit einem Sternchen wird Text *kursiv*:

```markdown
Aus dem Nebenraum hörst du ein *leises Klopfen*.
```

Eine wörtliche Rede kannst du zum Beispiel als Zitat schreiben:

```markdown
> „Ich würde diese Tür nicht öffnen“, sagt die Person.
```

Und mit weiteren `#` kannst du kleinere Überschriften erzeugen:

```markdown
## Ein Hinweis

Hier könnte ein wichtiger Hinweis stehen.
```

**Aufgabe:** Ergänze eine deiner Seiten um etwas fett oder kursiv formatierten Text und eine kurze wörtliche Rede.

## Dialoge und kleine Aktionen

Nicht jede Entscheidung soll zu einer neuen Seite führen.

Vielleicht möchtest du mit einer Person sprechen, einen Gegenstand untersuchen oder eine Schublade öffnen. Dafür gibt es in BIF **lokale Entscheidungen**.

Öffne `2.md` und ergänze:

```markdown
# Das Büro

Eine Person sitzt an einem Schreibtisch und sieht von ihrem Bildschirm auf.

- [Frage, was hinter der Ausgangstür ist.](.)

    > „Keine Ahnung. Die Tür ist abgeschlossen.“

- [Gehe zurück in den Flur.](1)
```

Der wichtige Unterschied ist der Punkt:

```text
(.)
```

Er bedeutet: **Bleibe auf dieser Seite.**

Der eingerückte Text darunter erscheint erst, nachdem die Entscheidung ausgewählt wurde.

<!-- Screenshot: Dialogentscheidung vor dem Anklicken -->

<div style='text-align: center; margin: 1em 0;'>
<img src='dialogue-before.webp' style='max-width: 100%;'>
</div>

Klicke auf die Frage.

<!-- Screenshot: Dialogentscheidung nach dem Anklicken -->

<div style='text-align: center; margin: 1em 0;'>
<img src='dialogue-after.webp' style='max-width: 100%;'>
</div>

Die gewählte Frage und die Antwort bleiben jetzt als Teil des Gesprächs sichtbar.

<div class='hint books'>
Achtung: Der Antworttext muss eingerückt sein. Setze vor jede Zeile der Antwort vier Leerzeichen. So erkennt BIF, dass die Antwort zu der Entscheidung darüber gehört.
</div>

Lokale Entscheidungen eignen sich zum Beispiel für:

* Gespräche
* das Untersuchen eines Gegenstands
* das Lesen eines Briefs
* das Öffnen einer Schublade
* das Betätigen eines Schalters
* kleine Aktionen, die keinen neuen Ort benötigen

**Aufgabe:** Füge dem Büro eine zweite Frage hinzu, die ebenfalls auf derselben Seite beantwortet wird.

<div style='display: none;'>

Zum Beispiel:

```markdown
- [Frage, wie lange die Person schon hier sitzt.](.)

    > „Seit heute Morgen.“
```

</div>

Wechsle anschließend noch einmal zum Graphen.

Du wirst sehen: Die zusätzlichen Fragen haben keine neuen Knoten erzeugt. Der Graph bleibt übersichtlich, weil die Unterhaltung innerhalb derselben Seite stattfindet.

## Die Geschichte kann sich etwas merken

Bisher hängt der weitere Verlauf nur davon ab, welche Seite du auswählst. BIF kann sich aber auch Dinge merken.

Zum Beispiel könnte die Person im Büro einen Schlüssel besitzen. Erst wenn du nach diesem Schlüssel gefragt hast, kannst du später eine verschlossene Tür öffnen.

Dafür verwenden wir eine **Variable**.

Öffne `1.md` und füge direkt unter der Überschrift diesen Abschnitt ein:

```html
<script>
has_key = has_key ?? false;
</script>
```

Die Variable heißt:

```text
has_key
```

und kann zwei Werte haben:

```text
true
false
```

`true` bedeutet »ja« und `false` bedeutet »nein«.

Die Zeile

```js
has_key = has_key ?? false;
```

bedeutet ungefähr:

> Falls `has_key` noch keinen Wert hat, setze den Wert auf `false`. Ansonsten behalte den bisherigen Wert.

So kann sich die Geschichte merken, ob der Schlüssel bereits gefunden wurde.

<div class='hint'>
Variablennamen dürfen keine Leerzeichen enthalten. Namen wie <code>has_key</code>, <code>door_open</code> oder <code>trust</code> sind möglich.
</div>

## Eine Entscheidung verändert den Zustand

Gehe wieder zu `2.md` und ergänze eine weitere lokale Entscheidung:

```markdown
- [Frage nach einem Schlüssel.](.)

    <script>
    has_key = true;
    </script>

    Die Person gibt dir einen kleinen Schlüssel.
```

Wenn diese Entscheidung ausgewählt wird, führt BIF den eingerückten `<script>`-Abschnitt aus.

Die Zeile

```js
has_key = true;
```

merkt sich, dass du den Schlüssel jetzt besitzt.

<!-- Screenshot: Schlüssel-Dialog nach Auswahl -->

<div style='text-align: center; margin: 1em 0;'>
<img src='take-key.webp' style='max-width: 100%;'>
</div>

Damit haben wir zum ersten Mal eine Entscheidung getroffen, die **den späteren Verlauf der Geschichte verändert**.

## Bedingte Entscheidungen

Jetzt soll im Flur eine verschlossene Ausgangstür erscheinen, die nur mit dem Schlüssel geöffnet werden kann.

Erstelle eine neue Datei `5.md`:

```markdown
# Draußen

Du schließt die Tür auf und trittst nach draußen.

**Ende**
```

Eine Seite ohne weitere Entscheidungen ist automatisch ein mögliches Ende der Geschichte.

Ergänze jetzt in `1.md` eine weitere Entscheidung:

```markdown
- [Schließe die Ausgangstür auf.](5){condition="has_key"}
```

Das Besondere steht hinter der Entscheidung:

```text
{condition="has_key"}
```

`condition` bedeutet **Bedingung**.

Die Entscheidung wird nur angezeigt, wenn `has_key` den Wert `true` hat.

Starte die Geschichte neu und probiere es aus:

1. Gehe zunächst noch **nicht** ins Büro.
2. Die Entscheidung zum Aufschließen der Tür sollte nicht sichtbar sein.
3. Gehe ins Büro.
4. Frage nach dem Schlüssel.
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

<div class='hint'>
Mit Variablen und Bedingungen kannst du dafür sorgen, dass frühere Entscheidungen später Folgen haben. Dadurch muss eine interaktive Geschichte nicht einfach nur aus immer neuen Verzweigungen bestehen.
</div>

**Aufgabe:** Baue eine zweite Bedingung in deine Geschichte ein.

Zum Beispiel:

* eine Information, die man zuerst erfahren muss
* einen Gegenstand, den man finden muss
* eine Person, mit der man gesprochen haben muss
* einen Schalter, den man betätigen muss

<div style='display: none;'>

Zum Beispiel könnte eine lokale Entscheidung eine Variable setzen:

```markdown
- [Lies den Zettel auf dem Tisch.](.)

    <script>
    knows_code = true;
    </script>

    Auf dem Zettel steht die Zahl 731.
```

Eine spätere Entscheidung kann davon abhängig sein:

```markdown
- [Gib den Code ein.](6){condition="knows_code"}
```

</div>

## Text nur unter bestimmten Bedingungen anzeigen

Bedingungen können nicht nur Entscheidungen steuern. Du kannst auch Text nur dann anzeigen lassen, wenn eine Bedingung erfüllt ist.

Füge zum Beispiel in `1.md` ein:

```html
<p condition="has_key">
Du spürst den kleinen Schlüssel in deiner Tasche.
</p>
```

Dieser Satz erscheint nur, wenn `has_key` den Wert `true` besitzt.

Du kannst eine Bedingung auch umkehren:

```html
<p condition="!has_key">
Die Ausgangstür ist verschlossen.
</p>
```

Das Ausrufezeichen `!` bedeutet hier ungefähr **nicht**.

```text
!has_key
```

bedeutet also:

> Du hast den Schlüssel nicht.

Damit kann sich sogar die Beschreibung eines Ortes verändern, obwohl du immer dieselbe Seite besuchst.

## Werte im Text anzeigen

Variablen können nicht nur `true` oder `false` enthalten. Sie können zum Beispiel auch Zahlen oder Texte speichern.

Ein möglicher Anfang einer Geschichte könnte so aussehen:

```html
<script>
minutes = minutes ?? 10;
</script>
```

Den aktuellen Wert kannst du mit doppelten eckigen Klammern direkt im Text anzeigen:

```markdown
Du hast noch [[ minutes ]] Minuten.
```

Das Ergebnis könnte dann so aussehen:

```text
Du hast noch 10 Minuten.
```

Eine Entscheidung könnte die Zeit verändern:

```markdown
- [Durchsuche den Raum.](.)

    <script>
    minutes -= 2;
    </script>

    Die Suche kostet dich zwei Minuten.
```

Danach würde im weiteren Verlauf nur noch der neue Wert gespeichert.

<div class='hint'>
Du brauchst Zahlen und Variablen nicht für jede Geschichte. Benutze sie nur dann, wenn sie für deine Idee wirklich etwas bewirken.
</div>

## Bilder verwenden

Du kannst Bilder direkt in deine Geschichte einbauen.

Lege dafür innerhalb von `pages-meine-geschichte` einen Ordner mit dem Namen `images` an:

```text
pages-meine-geschichte/
├── 1.md
├── 2.md
├── 3.md
└── images/
```

Lege dort zum Beispiel eine Bilddatei mit dem Namen `door.jpg` ab.

In einer Markdown-Datei kannst du das Bild dann so anzeigen:

```markdown
![Eine verschlossene Tür](images/door.jpg)
```

Der Text in den eckigen Klammern beschreibt das Bild. Diese Beschreibung ist wichtig, damit die Geschichte auch mit Hilfstechnologien wie Screenreadern verständlich bleibt.

<!-- Screenshot: BIF-Seite mit Bild -->

<div style='text-align: center; margin: 1em 0;'>
<img src='story-image.webp' style='max-width: 100%;'>
</div>

<div class='hint'>
Speichere Bilder und andere Dateien, die zu deiner Geschichte gehören, am besten immer innerhalb deines Geschichtenordners.
</div>

## Audio und Video

Du kannst auch Audio- und Videodateien verwenden.

Eine Audiodatei kannst du zum Beispiel so einfügen:

```html
<audio controls src="audio/door.mp3"></audio>
```

Ein Video kannst du so einfügen:

```html
<video controls>
    <source src="video/train.webm" type="video/webm">
</video>
```

Dafür kannst du innerhalb deines Geschichtenordners entsprechende Unterordner wie `audio` oder `video` anlegen.

## Fehler finden

Beim Schreiben einer größeren Geschichte passieren leicht Fehler.

Vielleicht verweist eine Entscheidung auf eine Seite, die gar nicht existiert. Vielleicht hast du eine Bilddatei falsch geschrieben oder eine Seite angelegt, zu der kein Weg führt.

BIF überprüft deine Geschichte deshalb während des Schreibens.

Probiere absichtlich einen Fehler aus. Ändere in `1.md` zum Beispiel eine Entscheidung zu:

```markdown
- [Gehe in das Büro.](99)
```

obwohl es keine Datei `99.md` gibt.

Öffne anschließend die Entwicklungsansicht.

<!-- Screenshot: Problems-Ansicht mit fehlender Seite 99 -->

<img class='full' src='missing-page-problem.webp'>

BIF sollte jetzt anzeigen, dass das Ziel der Entscheidung fehlt.

Korrigiere die Zahl anschließend wieder zu:

```markdown
- [Gehe in das Büro.](2)
```

Die Fehlermeldung sollte verschwinden.

BIF kann unter anderem auf folgende Probleme hinweisen:

* fehlende Seiten
* Seiten, die von nirgendwo erreicht werden können
* fehlende Bilder oder andere Dateien
* ungültige Bedingungen
* Fehler in Skripten

<div class='hint'>
Schau beim Schreiben regelmäßig auf den Graphen und die Problems-Ansicht. Gerade bei größeren Geschichten findest du damit viele Fehler, bevor du die Geschichte komplett durchspielen musst.
</div>

## Den Graphen übersichtlicher machen

Bei einer größeren Geschichte kannst du Seiten im Graphen zu Bereichen zusammenfassen.

Dafür kannst du ganz oben in einer Seite einen unsichtbaren Kommentar einfügen:

```markdown
<!-- Gebäude -- Büro -->

# Das Büro
```

Der erste Teil

```text
Gebäude
```

ist die Gruppe.

Der zweite Teil

```text
Büro
```

ist eine kurze Bezeichnung für den Knoten im Graphen.

Eine andere Seite könnte zum Beispiel beginnen mit:

```markdown
<!-- Gebäude -- Flur -->

# Der Flur
```

Mehrere Seiten mit derselben Gruppe werden in der Entwicklungsansicht zusammen dargestellt.

<!-- Screenshot: Graph mit gruppierten Seiten -->

<div style='text-align: center; margin: 1em 0;'>
<img src='grouped-graph.webp' style='max-width: 100%;'>
</div>

Diese Kommentare verändern die Geschichte für die Leserinnen und Leser nicht. Sie helfen nur beim Schreiben.

## Übungsaufgaben

Jetzt kennst du bereits die wichtigsten Bausteine von BIF.

Versuche eine oder mehrere der folgenden Aufgaben.

### Ein Gespräch

Baue eine Seite mit einer Person, die mindestens drei verschiedene Fragen beantwortet.

Verwende dafür lokale Entscheidungen mit `(.)`.

Mindestens eine Antwort soll eine weitere Frage freischalten.

<div style='display: none;'>

Zum Beispiel:

```markdown
<script>
knows_box = knows_box ?? false;
</script>

- [Frage nach der Kiste.](.)

    <script>
    knows_box = true;
    </script>

    > „Die stand schon hier, als ich gekommen bin.“

- [Frage, wer die Kiste gebracht hat.](.){condition="knows_box"}

    > „Ich habe nur einen roten Lieferwagen gesehen.“
```

</div>

### Ein Gegenstand

Baue einen Gegenstand ein, den man finden oder erhalten kann.

Eine spätere Entscheidung soll nur dann möglich sein, wenn man diesen Gegenstand besitzt.

### Zwei verschiedene Enden

Schreibe eine kleine Geschichte mit mindestens zwei verschiedenen Enden.

Versuche dabei, nicht einfach nur am Anfang zwei völlig getrennte Wege zu bauen. Eine frühere Entscheidung soll sich erst später auswirken.

### Eine eigene interaktive Geschichte

Entwickle jetzt eine eigene Idee.

Deine Geschichte sollte zunächst ungefähr diese Bestandteile enthalten:

* mindestens fünf verschiedene Seiten
* mindestens eine echte Verzweigung
* mindestens einen Weg, der wieder zu einer früheren Seite zurückführt
* mindestens einen lokalen Dialog oder eine lokale Aktion
* mindestens eine Variable
* mindestens eine bedingte Entscheidung
* mindestens ein Ende

Du kannst deine Geschichte danach beliebig erweitern.

<div class='hint'>
Plane nicht sofort zwanzig oder dreißig Seiten. Beginne mit wenigen Seiten, probiere sie aus und erweitere die Geschichte Schritt für Schritt. Der Graph hilft dir dabei, den Überblick zu behalten.
</div>

Falls dir noch eine Idee fehlt, kannst du zum Beispiel mit einer sehr einfachen Situation beginnen:

* Du suchst einen verschwundenen Gegenstand.
* Du musst rechtzeitig einen bestimmten Ort erreichen.
* Du erkundest ein Gebäude.
* Du versuchst herauszufinden, was passiert ist.
* Du erzählst eine bekannte Sage aus einer anderen Perspektive.
* Du lässt die Leserin oder den Leser eine historische Entscheidung treffen.

Die eigentliche Geschichte, die Figuren und die Dialoge bestimmst du selbst.

## Zufall

Für manche Geschichten kann Zufall interessant sein.

BIF besitzt dafür einige kleine Hilfsfunktionen.

Mit

```js
Math.w6()
```

kannst du zum Beispiel einen normalen sechsseitigen Würfel werfen.

```html
<script>
dice = Math.w6();
</script>

Du hast eine [[ dice ]] gewürfelt.
```

Bei jedem neuen Durchlauf kann ein anderes Ergebnis entstehen.

Mit

```js
Math.chance(50)
```

kannst du eine Wahrscheinlichkeit angeben.

Zum Beispiel:

```html
<script>
found_note = Math.chance(50);
</script>

<p condition="found_note">
Unter dem Schrank findest du einen Zettel.
</p>

<p condition="!found_note">
Unter dem Schrank liegt nur Staub.
</p>
```

`Math.chance(50)` bedeutet eine Chance von 50 Prozent.

<div class='hint'>
Zufall kann eine Geschichte abwechslungsreicher machen. Für wichtige Entscheidungen ist es aber oft interessanter, wenn das Ergebnis von vorherigen Entscheidungen der Spielerinnen und Spieler abhängt.
</div>

**Aufgabe:** Baue eine kleine Zufallsentscheidung in eine Geschichte ein. Probiere anschließend mehrmals einen Neustart aus.

## Entscheidungen mit JavaScript

Für die meisten Geschichten reichen die normalen Markdown-Entscheidungen vollkommen aus:

```markdown
- [Gehe nach links.](5)
- [Gehe nach rechts.](6)
```

Für besondere Fälle kann BIF Entscheidungen aber auch direkt in einem Skript erzeugen.

```html
<script>
const direction = await presentChoice([
    ['left', 'Gehe nach links'],
    ['right', 'Gehe nach rechts'],
]);

if (direction === 'left') {
    await goToPage('5');
} else {
    await goToPage('6');
}
</script>
```

Mit `presentChoice()` wird eine Auswahl angezeigt. Mit `goToPage()` kannst du anschließend zu einer Seite wechseln.

<div class='hint books'>
Für normale Verzweigungen solltest du weiterhin die einfachen Markdown-Links verwenden. JavaScript lohnt sich erst dann, wenn sich Entscheidungen nicht mehr einfach mit normalen Seiten und Bedingungen ausdrücken lassen.
</div>

## Eine Geschichte überprüfen

Während du arbeitest, überprüft BIF deine Geschichte automatisch.

Du kannst am Ende zusätzlich eine vollständige Überprüfung im Terminal starten.

Öffne ein Terminal und gib ein:

```bash
npm run check
```

BIF untersucht dann die ausgewählte Geschichte und meldet gefundene Probleme.

<!-- Screenshot: npm run check ohne Fehler -->

<img class='full' src='check-story.webp'>

Wenn keine Fehler gemeldet werden, ist die technische Struktur deiner Geschichte in Ordnung.

Das bedeutet natürlich noch nicht, dass jede Entscheidung sinnvoll oder jeder Text fertig ist. Spiele deine Geschichte deshalb anschließend selbst noch einmal von Anfang bis Ende durch und lasse sie am besten auch von jemand anderem ausprobieren.

## Und jetzt?

Die kleine Geschichte mit Flur, Büro und Schlüssel war nur dazu da, die Technik kennenzulernen.

Für deine eigene Geschichte brauchst du sie nicht weiterzuverwenden.

Du kannst einen neuen Geschichtenordner anlegen, zum Beispiel:

```text
pages-meine-neue-geschichte
```

und anschließend in `config.js` auswählen:

```js
export const path = "pages-meine-neue-geschichte";
```

Beginne wieder mit einer `1.md` und entwickle deine Geschichte von dort aus Schritt für Schritt.

Die wichtigsten Bausteine kennst du jetzt:

```text
1.md
```

startet eine Geschichte.

```markdown
- [Gehe weiter.](2)
```

führt zu einer anderen Seite.

```markdown
- [Frage nach.](.)

    Eine Antwort erscheint.
```

bleibt auf derselben Seite.

```html
<script>
has_key = true;
</script>
```

merkt sich einen Zustand.

```markdown
- [Öffne die Tür.](3){condition="has_key"}
```

macht eine Entscheidung von diesem Zustand abhängig.

Alles Weitere entsteht aus deiner Geschichte.
