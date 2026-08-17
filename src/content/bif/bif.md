<div class='meta'>
image: bif.webp
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Interaktive Geschichten schreiben

<p class='abstract'>
Mit BIF kannst du interaktive Geschichten schreiben, bei denen die Leserinnen und Leser selbst entscheiden, wie es weitergeht. Die einzelnen Abschnitte deiner Geschichte schreibst du als einfache Markdown-Dateien und verbindest sie miteinander. In diesem Tutorial entwickelst du Schritt für Schritt eine kleine Geschichte und lernst dabei die wichtigsten Bausteine von BIF kennen: Seiten, Entscheidungen, Verzweigungen, lokale Aktionen, Variablen und Bedingungen.
</p>

## Repository klonen

Stelle zuerst sicher, dass du keinen Ordner geöffnet hast. Um sicherzugehen, drücke den Shortcut für »Ordner schließen«: <kbd>Strg</kbd><kbd>K</kbd> und dann <kbd>F</kbd>.

<!-- tutorial-screenshot
# Workspace ohne geöffneten Ordner
show-left-sidebar
left-sidebar-width: 300
-->

<img class='full' src='fresh-start.webp' alt=''>

Für diese Anleitung brauchst du das BIF-Repository. Klicke auf den blauen Button »Clone Repository« und gib die folgende URL ein:

```text
https://github.com/specht/bif.git
```

Bestätige anschließend mit <kbd>Enter</kbd>.


<!-- tutorial-screenshot
# Clone Repository mit eingetragener BIF-URL
clone-start: https://github.com/specht/bif.git @ b5215fa72545f05f00d2ba23865c4e2eeff691a2
crop-bottom: 67%
-->

<img class='full' src='git-clone.webp' alt=''>

Als nächstes musst du angeben, in welches Verzeichnis das Repository geklont werden soll. Bestätige den Standardpfad

```text
/workspace/
```

mit <kbd>Enter</kbd>.

<!-- tutorial-screenshot
# Auswahl von /workspace/
clone-confirm-url
crop-bottom: 54%
-->

<img class='full' src='confirm-clone-path.webp' alt=''>

Beantworte anschließend die Frage »Would you like to open the cloned repository?« mit »Open«.

<!-- tutorial-screenshot
# Dialog zum Öffnen des geklonten Repositorys
clone-accept-destination
-->

<img class='full' src='open-yes-no.webp' alt=''>

<!-- tutorial-screenshot
# geöffnetes BIF-Projekt im Explorer
clone-open
wait-for-file: bif/node_modules/markdown-it/LICENSE
wait-for-file: bif/.story-tools/analysis.json
-->

<img class='full' src='bif-project.webp' alt=''>

Wenn alles geklappt hat, siehst du links im Explorer unter anderem den Ordner `pages-starter` sowie die Dateien `config.js` und `index.html`.

## Geschichte starten

Öffne im Explorer den Ordner `pages-starter` und darin die Datei `1.md`.

<!-- tutorial-screenshot
# 1.md geöffnet
open-file: pages-starter/1.md
close-tab: Welcome
-->

<img class='full' src='story-begin.webp' alt=''>

Dort steht bereits der Anfang unserer kleinen Geschichte:

```markdown_wrap
# Nach Schulschluss

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.
```

Die Endung `.md` steht für [Markdown](https://de.wikipedia.org/wiki/Markdown). Markdown ist eine einfache Schreibweise für Texte und eignet sich gut für interaktive Geschichten.
Die Zeile

```markdown_wrap
# Nach Schulschluss
```

ist der **Titel der Geschichte**. Ein einzelnes `#` steht in Markdown für die größte Überschrift. In einer BIF-Geschichte verwenden wir diese Überschrift für den Titel der gesamten Geschichte. Sie steht deshalb nur am Anfang von `1.md`. Die weiteren Seiten brauchen normalerweise keine eigene Überschrift.

Falls du innerhalb einer Seite doch einmal eine Zwischenüberschrift brauchst, verwendest du dafür `##`.

<div class='hint'>
Die Datei <code>1.md</code> hat bei BIF eine besondere Bedeutung: Jede Geschichte beginnt bei Seite 1.
</div>

## Vorschau starten

<img src='go-live.webp' class='r' style='width: 21em;' alt=''>

Damit du deine Geschichte im Browser ausprobieren kannst, ist im Workspace bereits die Erweiterung **Live Server** installiert.
Klicke rechts unten auf »Go Live«. Es öffnet sich ein neuer Tab mit deiner Geschichte:

<div style='clear: both;'></div>

<!-- tutorial-screenshot
# erste BIF-Vorschau
go-live
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='first-page.webp' alt='Der Story-Graph enthält eine Startseite; daneben erscheint der Beginn der Geschichte, allerdings noch ohne Entscheidungen.'>

BIF startet standardmäßig in der Entwicklungsansicht. Rechts siehst du die Geschichte, die momentan noch sehr kurz ist und noch keine Entscheidungsmöglichkeiten bietet. Mit dem Button rechts oben kannst du zwischen der Entwicklungsansicht und der Leseansicht hin- und herwechseln. Wenn du deine Geschichte später veröffentlichst, bekommen deine Leser:innen nur die Leseansicht zu sehen.

## Zweite Seite

<img src='new-file.webp' class='r' style='width: 18em;' alt=''>

Unsere Geschichte soll nicht auf der ersten Seite stehen bleiben. Erstelle im Ordner `pages-starter` eine neue Datei, indem du auf das entsprechende Icon klickst. Nenne die Datei `2.md`.

Schreibe hinein:

```markdown_wrap
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus. Am Ende steht ein verschlossener Materialschrank.
```

Speichere die Datei, indem du <kbd>Strg</kbd><kbd>S</kbd> drückst. Die Vorschau sollte nun so aussehen:

<div style='clear: both;'></div>

<!-- tutorial-screenshot
write-file: pages-starter/2.md <- previous-code
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/2.md
preview-reload
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='unreachable-page.webp' alt='Eine zweite Seite ist im Story-Graph nicht mit der Startseite verbunden und als unerreichbar markiert.'>

Damit besteht die Geschichte bereits aus zwei Seiten, allerdings gibt es noch keine Verbindung von der ersten zur zweiten Seite – dafür brauchen wir eine Entscheidung.

## Entscheidungen

Öffne wieder `1.md` und ergänze am Ende:

```markdown_wrap
- [Gehe in den Flur.](2)
```

Die vollständige Datei sieht jetzt so aus:

```markdown_wrap
# Nach Schulschluss

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.

- [Gehe in den Flur.](2)
```

Speichere die Datei und probiere die Geschichte im Browser aus:

<!-- tutorial-screenshot
write-file: pages-starter/1.md <- previous-code
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
preview-reload
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='two-pages.webp' alt='Zwei Seiten sind im Story-Graph durch eine Entscheidung miteinander verbunden.'>

Die Geschichte endet jetzt mit einer Entscheidungsmöglichkeit: wenn du in den Flur gehst, geht die Geschichte bei Seite 2 weiter. Links im Graphen siehst du immer, wo du dich gerade innerhalb der Geschichte befindest.

Die Zeile

```markdown_wrap
- [Gehe in den Flur.](2)
```

besteht aus zwei wichtigen Teilen: in eckigen Klammern steht der Text, der angezeigt wird und in runden Klammern steht die Seitenzahl, mit der es bei dieser Entscheidung weitergehen soll.

## Verzweigungen

Eine interaktive Geschichte wird interessanter, wenn nicht immer nur ein einziger Weg möglich ist.
Erstelle die Datei `3.md`:

<!-- screenshot-code: 3-new -->
```markdown_wrap
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Gehe zurück in den Flur.](2)
```

Erstelle außerdem die Datei `4.md`:

<!-- screenshot-code: 4-new -->
```markdown_wrap
Im Treppenhaus ist es still. Auf dem Absatz liegt nur ein vergessener Turnbeutel.

- [Gehe zurück in den Flur.](2)
```

Öffne jetzt `2.md` und ergänze zwei Entscheidungen:

<!-- screenshot-code: 2-new -->
```markdown_wrap
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus. Am Ende steht ein verschlossener Materialschrank.

- [Sieh im Büro nach.](3)
- [Gehe ins Treppenhaus.](4)
```

Speichere die Dateien und probiere beide Wege aus.

<!-- tutorial-screenshot
write-file: pages-starter/3.md <- previous-code (3-new)
write-file: pages-starter/4.md <- previous-code (4-new)
write-file: pages-starter/2.md <- previous-code (2-new)
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/2.md
preview-reload
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
click: Gehe in den Flur.
wait-for-text: Sieh im Büro nach.
click: Sieh im Büro nach.
wait-for-text: Gehe zurück in den Flur.
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='branching-story.webp' alt='Der Story-Graph verzweigt sich vom Flur zu Büro und Treppenhaus.'>

Die Geschichte verzweigt sich jetzt auf Seite 2. Beide Wege führen anschließend wieder zurück in den Flur.

Jetzt, nachdem unsere Geschichte mehrere Seiten und Verzweigungen hat, lohnt sich ein genauerer Blick auf den Graphen auf der linken Seite: Jede Seite wird als Knoten dargestellt. Die Pfeile zeigen, welche Entscheidungen von einer Seite zu einer anderen führen. Bei unserer Geschichte sieht man jetzt zum Beispiel:

- Seite 1 führt zu Seite 2.
- Seite 2 führt zu Seite 3 oder Seite 4.
- Seite 3 und Seite 4 führen zurück zu Seite 2.

So wird auch sichtbar, dass `1.md` nur der Einstieg ist. Später kehren wir nicht mehr auf diese Seite zurück. Du siehst auch, auf welcher Seite du dich gerade in der Geschichte befindest und welchen Pfad du bisher zurückgelegt hast.

<div class='hint'>
Je größer deine Geschichte wird, desto nützlicher wird der Graph. Du kannst damit schnell erkennen, welche Wege möglich sind und ob Teile deiner Geschichte gar nicht erreicht werden können.
</div>

## Lokale Entscheidungen

Nicht jede Entscheidung soll zu einer anderen Seite führen.

Vielleicht möchtest du mit einer Person sprechen, einen Gegenstand untersuchen oder eine Schublade öffnen. Dafür gibt es in BIF **lokale Entscheidungen**.

Öffne `3.md` und ändere den Inhalt zu:

<!-- screenshot-code: 3-newer -->
```markdown_wrap
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Frage nach dem Materialschrank.](.)

    > "Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür."

- [Gehe zurück in den Flur.](2)
```

Der wichtige Unterschied ist der Punkt: `(.)` – er bedeutet, dass die Geschichte auf dieser Seite bleiben soll. Der eingerückte Text darunter erscheint erst, nachdem die Entscheidung ausgewählt wurde.

<!-- tutorial-screenshot
write-file: pages-starter/3.md <- previous-code (3-newer)
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/2.md
preview-reload
hold: 1.5s selector:.story-restart-control
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
click: Gehe in den Flur.
wait-for-text: Sieh im Büro nach.
click: Sieh im Büro nach.
wait-for-text: Frage nach dem Materialschrank.
click: Frage nach dem Materialschrank.
wait-for-text: Gehe zurück in den Flur.
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='local-choice.webp' alt='Die Frage nach dem Materialschrank führt nicht zu einem Seitenwechsel, da es sich um eine lokale Entscheidung handelt.'>

Die vier Leerzeichen vor der Antwort sind wichtig. Durch die Einrückung erkennt BIF, dass der eingerückte Text zur Entscheidung direkt darüber gehört. Alles, was nach dieser lokalen Entscheidung passieren soll, wird deshalb entsprechend eingerückt.

Probiere die neue Entscheidung aus. Nachdem du Frau Neumann gefragt hast, gehört die Frage zusammen mit ihrer Antwort zum bisherigen Verlauf. Sie steht nicht mehr bei den noch offenen Entscheidungsmöglichkeiten und kann während dieses Besuchs nicht noch einmal ausgewählt werden. Die Entscheidung »Gehe zurück in den Flur« bleibt dagegen weiterhin verfügbar.

Lokale Entscheidungen eignen sich zum Beispiel für:

- Gespräche
- das Untersuchen eines Gegenstands
- das Lesen eines Briefs
- das Öffnen einer Schublade
- das Betätigen eines Schalters
- kleine Aktionen, die keinen neuen Ort benötigen

## Variablen

Bisher hängt der Verlauf nur davon ab, welche Seite gerade geöffnet wird. Eine Geschichte kann sich aber auch etwas **merken**. In unserem Beispiel soll gespeichert werden, ob du den Schlüssel aus dem Büro genommen hast.

Öffne `1.md` und füge direkt unter dem Titel ein:

```html
<script>
has_key = false;
</script>
```

Die Datei sieht jetzt so aus:

```markdown_wrap
# Nach Schulschluss

<script>
has_key = false;
</script>

Du willst gerade gehen, da fällt dir ein: Deine Projektmappe liegt noch im Materialschrank.

- [Gehe in den Flur.](2)
```

`has_key` ist eine Variable. Der Wert `false` bedeutet hier: Du hast den Schlüssel noch nicht. Wir setzen diesen Anfangswert in `1.md`, weil diese Seite nur einmal am Anfang besucht wird.

Öffne anschließend `3.md` und ergänze eine weitere lokale Entscheidung:

```markdown_wrap
- [Nimm den Schlüssel.](.)

    <script>
    has_key = true;
    </script>

    Du nimmst den kleinen Schlüssel vom Haken.
```

Die vollständige Datei sieht jetzt so aus:

```markdown_wrap
Im Büro sitzt Frau Neumann an einem Schreibtisch. Neben der Tür hängt ein kleiner Schlüssel an einem Haken.

- [Frage nach dem Materialschrank.](.)

    > "Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür."

- [Nimm den Schlüssel.](.)

    <script>
    has_key = true;
    </script>

    Du nimmst den kleinen Schlüssel vom Haken.

- [Gehe zurück in den Flur.](2)
```

Wenn du »Nimm den Schlüssel« auswählst, wird das Skript ausgeführt und `has_key` ändert sich von `false` zu `true`.

Öffne in der Entwicklungsansicht den Bereich **State** und beobachte den Wert beim Spielen.

<!-- Screenshot: Schlüssel genommen / State mit has_key: true -->

<img class='full full-shadow' src='take-key.webp' alt='Nach dem Aufnehmen des Schlüssels verschwindet die lokale Entscheidung aus der Geschichte.'>

Die Geschichte hat sich damit zum ersten Mal etwas gemerkt. Solange du im Büro bleibst, sieht auch alles richtig aus: »Nimm den Schlüssel« ist bereits erledigt und kann nicht noch einmal ausgewählt werden.

Jetzt lohnt sich aber ein kleiner Test:

1. Nimm den Schlüssel.
2. Gehe zurück in den Flur.
3. Betritt das Büro erneut.

Nun stimmt plötzlich einiges nicht mehr. Der Schlüssel hängt laut Beschreibung wieder neben der Tür, du kannst ihn noch einmal nehmen und Frau Neumann behauptet weiterhin, dass er dort hängt. Gleichzeitig zeigt **State** immer noch:

```text
has_key: true
```

<!-- Screenshot: Büro nach erneutem Betreten, obwohl has_key true ist -->

Das ist kein Fehler in der Variable. `has_key` hat sich korrekt gemerkt, dass du den Schlüssel besitzt.

Der Grund liegt bei den lokalen Entscheidungen: BIF merkt sich innerhalb eines Besuchs, welche lokalen Entscheidungen bereits ausgeführt wurden. Wenn du eine Seite verlässt und später erneut betrittst, wird die Seite neu aufgebaut. Die lokalen Entscheidungen stehen dann zunächst wieder zur Verfügung.

Ob etwas **dauerhaft für den weiteren Verlauf der Geschichte** passiert ist, speichern wir deshalb in Variablen. Jetzt müssen wir dafür sorgen, dass die Seite diesen gespeicherten Zustand auch berücksichtigt.

## Bedingungen

Dafür gibt es **Bedingungen**. Mit einer Bedingung können wir festlegen, dass Text oder eine Entscheidung nur in einem bestimmten Zustand der Geschichte erscheint.

Wir beheben zuerst das offensichtlichste Problem: Wenn du den Schlüssel schon besitzt, darf »Nimm den Schlüssel« bei einem späteren Besuch nicht noch einmal angeboten werden.

Ergänze die Entscheidung in `3.md`:

```markdown_wrap
- [Nimm den Schlüssel.](.){condition="!has_key"}
```

Das Besondere steht hinter der Entscheidung:

```text
{condition="!has_key"}
```

`condition` bedeutet **Bedingung**. Das Ausrufezeichen vor `has_key` bedeutet **nicht**. Die Entscheidung erscheint also nur, wenn `has_key` nicht `true` ist – solange du den Schlüssel noch nicht hast.

Speichere die Datei und probiere den Weg noch einmal aus: Schlüssel nehmen, in den Flur gehen und anschließend wieder ins Büro. Diesmal wird »Nimm den Schlüssel« nicht erneut angeboten.

Damit ist aber erst ein Teil des Problems gelöst. Auch die Beschreibung des Büros behauptet noch, dass der Schlüssel am Haken hängt.

Ändere den Anfang von `3.md`:

```html_wrap
Im Büro sitzt Frau Neumann an einem Schreibtisch.

<p condition="!has_key">
Neben der Tür hängt ein kleiner Schlüssel an einem Haken.
</p>

<p condition="has_key">
Der Haken neben der Tür ist leer.
</p>
```

BIF zeigt nun abhängig vom Wert von `has_key` genau einen der beiden Absätze an.

Bedingungen können also nicht nur Entscheidungen, sondern auch normalen Text steuern. Dadurch kann sich derselbe Ort verändern, ohne dass wir dafür eine neue Seite anlegen müssen.

Es gibt noch einen Widerspruch: Wenn du das Büro mit dem Schlüssel erneut betrittst, sollte Frau Neumann nicht wieder sagen, dass er neben der Tür hängt. Auch das können wir mit Bedingungen lösen.

Die bisherige Frage soll nur erscheinen, solange du den Schlüssel noch nicht hast:

```markdown_wrap
- [Frage nach dem Materialschrank.](.){condition="!has_key"}

    > "Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür."
```

Sobald du den Schlüssel besitzt, bieten wir stattdessen eine passende Frage an:

```markdown_wrap
- [Frage, ob der Schlüssel zum Materialschrank passt.](.){condition="has_key"}

    > "Ja, genau der ist für den Materialschrank."
```

`3.md` sieht damit vollständig so aus:

<!-- screenshot-code: 3-cond -->
```markdown_wrap
Im Büro sitzt Frau Neumann an einem Schreibtisch.

<p condition="!has_key">
Neben der Tür hängt ein kleiner Schlüssel an einem Haken.
</p>

<p condition="has_key">
Der Haken neben der Tür ist leer.
</p>

- [Frage nach dem Materialschrank.](.){condition="!has_key"}

    > "Der ist abgeschlossen. Der kleine Schlüssel hängt hier neben der Tür."

- [Nimm den Schlüssel.](.){condition="!has_key"}

    <script>
    has_key = true;
    </script>

    Du nimmst den kleinen Schlüssel vom Haken.

- [Frage, ob der Schlüssel zum Materialschrank passt.](.){condition="has_key"}

    > "Ja, genau der ist für den Materialschrank."

- [Gehe zurück in den Flur.](2)
```

Probiere das Büro jetzt noch einmal von Anfang an aus. Wenn du den Schlüssel nimmst, verändert sich die Seite sofort: Der Haken ist leer und die passenden Entscheidungen ändern sich. Wenn du das Büro verlässt und erneut betrittst, bleibt die Geschichte trotzdem logisch, weil `has_key` weiterhin `true` ist.

<!-- Screenshot: Büro nach dem Nehmen des Schlüssels -->

Jetzt soll der Schlüssel auch außerhalb des Büros eine Folge haben.

Erstelle die Datei `5.md`:

<!-- screenshot-code: 5-cond -->
```markdown_wrap
Der Schlüssel passt.

Im Materialschrank liegt deine Projektmappe zwischen zwei Kartons. Du steckst sie ein. Jetzt kannst du endlich nach Hause.

**Ende.**
```

Öffne anschließend `2.md`. Auch der Flur kann auf den gespeicherten Zustand reagieren. Ersetze den Inhalt durch:

<!-- screenshot-code: 2-cond -->
```markdown_wrap
Du stehst in einem leeren Flur. Links ist ein kleines Büro. Daneben führt eine Tür ins Treppenhaus.

<p condition="!has_key">
Am Ende steht ein verschlossener Materialschrank.
</p>

<p condition="has_key">
Am Ende steht der Materialschrank. Du hast den passenden Schlüssel dabei.
</p>

- [Sieh im Büro nach.](3)
- [Gehe ins Treppenhaus.](4)
- [Öffne den Materialschrank.](5){condition="has_key"}
```

Die Entscheidung zum Öffnen des Materialschranks wird nur angezeigt, wenn `has_key` den Wert `true` hat.

Starte die Geschichte neu und probiere es aus:

1. Gehe in den Flur.
2. Die Entscheidung zum Öffnen des Materialschranks ist noch nicht sichtbar.
3. Gehe ins Büro.
4. Nimm den Schlüssel.
5. Gehe zurück in den Flur.

Jetzt verändert sich die Beschreibung des Flurs und die neue Entscheidung erscheint.

<!-- tutorial-screenshot
write-file: pages-starter/2.md <- previous-code (2-cond)
write-file: pages-starter/3.md <- previous-code (3-cond)
write-file: pages-starter/5.md <- previous-code (5-cond)
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/2.md
preview-reload
hold: 1.5s selector:.story-restart-control
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
click: Gehe in den Flur.
wait-for-text: Sieh im Büro nach.
click: Sieh im Büro nach.
wait-for-text: Frage nach dem Materialschrank.
click: Nimm den Schlüssel.
wait-for-text: Gehe zurück in den Flur.
click: Gehe zurück in den Flur.
move-mouse: No problems
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='locked-door.webp' alt='Im Flur erscheint mit gespeichertem Schlüssel die neue Entscheidung „Öffne den Materialschrank“.'>

Damit haben Variablen und Bedingungen unterschiedliche Aufgaben:

- Eine **Variable** merkt sich einen Zustand der Geschichte, auch wenn du eine Seite verlässt.
- Eine **Bedingung** entscheidet anhand dieses Zustands, welcher Text und welche Entscheidungen gerade sinnvoll sind.
- Eine bereits ausgeführte **lokale Entscheidung** ist nur für den aktuellen Besuch abgeschlossen. Bei einem späteren Besuch sorgt der gespeicherte Zustand zusammen mit Bedingungen dafür, dass die Seite trotzdem konsistent bleibt.

<div class='hint'>
Gerade beim Testen einer interaktiven Geschichte lohnt es sich, Orte mehrmals zu besuchen und Entscheidungen in unterschiedlicher Reihenfolge auszuprobieren. So fallen Widersprüche auf, die beim ersten Durchspielen leicht unbemerkt bleiben.
</div>

## Ende

Öffne jetzt den Materialschrank:

<!-- tutorial-screenshot
tab: preview
zoom: 1.25
click: Öffne den Materialschrank.
move-mouse: No problems
sleep: 0.5
-->

<img class='full full-shadow scroll-right' src='unlocked-door.webp' alt='Der geöffnete Materialschrank bildet das Ende der Geschichte; darunter erscheint der Neustart-Button.'>

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

Beim Schreiben von Skripten kann leicht ein Tippfehler passieren. BIF überprüft deine Geschichte deshalb während des Schreibens und zeigt gefundene Probleme unten im Bereich **Problems** an.

Probiere das absichtlich aus. Öffne `3.md` und ändere vorübergehend

```html_wrap
<script>
has_key = true;
</script>
```

zu:

```html_wrap
<script>
has_key = ;
</script>
```

Speichere die Datei.

BIF erkennt, dass das JavaScript nicht gültig ist. Im Bereich **Problems** siehst du, in welcher Datei und in welcher Zeile der Fehler gefunden wurde. Auch im Graphen wird die betroffene Seite als fehlerhaft markiert.

<!-- tutorial-screenshot
write-file: pages-starter/3.md <- file:3-with-error.md.txt
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/3.md
preview-reload
hold: 1.5s selector:.story-restart-control
tab: preview
zoom: 1.25
click: Fit graph
sleep: 0.5
-->
<img class='full full-shadow' src='script-error.webp' alt='Der Problems-Bereich meldet den JavaScript-Syntaxfehler; die betroffene Seite ist im Graphen rot markiert.'>

Korrigiere die Zeile anschließend wieder:

```js
has_key = true;
```

Nach dem Speichern sollte der Fehler aus der Problems-Liste verschwinden.

BIF kann unter anderem auf folgende Probleme hinweisen:

- Fehler in Skripten
- ungültige Bedingungen
- fehlende Seiten
- Seiten, die von nirgendwo erreicht werden können
- fehlende Bilder oder andere Dateien

<div class='hint'>
Schau beim Schreiben regelmäßig auf den Graphen und die Problems-Ansicht. Gerade bei größeren Geschichten findest du damit viele Fehler, bevor du die Geschichte komplett durchspielen musst.
</div>

## Weitere Möglichkeiten

Mit den Bausteinen aus der kleinen Geschichte kannst du bereits viele interaktive Geschichten schreiben. BIF kann darüber hinaus noch einiges mehr.

### Aussehen anpassen

Das Aussehen deiner Geschichte stellst du ganz oben in `1.md` ein. Dafür kannst du einen sogenannten **Front-Matter-Block** verwenden. Er steht noch vor dem Titel der Geschichte.
Am einfachsten wählst du ein fertiges Theme:

```markdown_wrap
---
theme: mystery
---

# Nach Schulschluss
```

BIF bringt mehrere Themes mit: `default`, `paper`, `playful`, `mystery`, `midnight` und `terminal`.
Ein Theme legt Farben und Schriften fest und verändert außerdem einige Details wie Abstände und abgerundete Ecken. Du kannst es deshalb einfach so verwenden, wie es ist.

<div class='row'>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-default.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    click: selector:.story-view-toggle
    hold: 1.5s selector:.story-restart-control
    move-mouse: Nach Schulschluss
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-default.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im hellen Standard-Theme.'>
    </div>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-paper.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-paper.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im cremefarbenen Paper-Theme mit Serifenschrift.'>
    </div>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-playful.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-playful.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im Playful-Theme mit abgerundeten Flächen.'>
    </div>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-mystery.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-mystery.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im dunklen Mystery-Theme.'>
    </div>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-midnight.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-midnight.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im dunkelblauen Midnight-Theme.'>
    </div>
    <!-- tutorial-screenshot
    write-file: pages-starter/1.md <- file:theme-terminal.md.txt
    wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
    tab: preview
    preview-reload
    zoom: 2.75
    sleep: 0.5
    -->
    <div class='col-md-4 mb-3'>
        <img src='theme-terminal.webp' style='width: 100%; border-radius: 0.5em;' class='full-shadow' alt='Die Geschichte im schwarzen Terminal-Theme mit grüner Monospace-Schrift.'>
    </div>
</div>

Du kannst ein Theme aber auch verändern. Zum Beispiel:

```markdown_wrap
---
theme: midnight
accent: "#ff7a18"
font_heading: Bungee
---
```

`accent` ist die Akzentfarbe für Links und interaktive Elemente. Mit `font_heading` änderst du nur die Schrift der Überschriften.

Du musst überhaupt kein Theme auswählen. Wenn du `theme:` weglässt, kannst du die Standarddarstellung selbst mit Farben und Schriften anpassen:

```markdown_wrap
---
background: "#fff7fb"
text: "#382d38"
accent: "#9d3b77"
font_body: Nunito
font_heading: Fredoka
---
```

Die drei Grundfarben haben unterschiedliche Aufgaben:

- `background` – Hintergrund
- `text` – normaler Text und Überschriften
- `accent` – Links, Hervorhebungen und interaktive Elemente

Die übrigen Farbtöne für Flächen, Rahmen und Schatten leitet BIF automatisch daraus ab. Du musst also nicht für jedes kleine Element eine eigene Farbe festlegen.

Mit

```markdown_wrap
brightness: light
```

oder

```markdown_wrap
brightness: dark
```

kannst du eine helle oder dunkle Darstellung erzwingen. `brightness: system` folgt stattdessen der Einstellung des Geräts.

Für `font_body` und `font_heading` kannst du den Namen einer Schrift von [Google Fonts](https://fonts.google.com/) eintragen. Schreibe den Namen genau so, wie er dort steht. BIF lädt die benötigten Schriftdateien beim Entwickeln automatisch herunter und speichert sie lokal im Ordner `bif-assets`. Diesen Ordner solltest du deshalb nicht selbst für Bilder oder andere Dateien verwenden.

<div class='hint'>
Gestaltung kann viel zur Stimmung beitragen. Eine Mystery-Geschichte darf anders aussehen als eine Komödie oder ein Terminal-Abenteuer. Achte aber darauf, dass der Text gut lesbar bleibt.
</div>

### Markdown

Für die normalen Texte kannst du die üblichen Markdown-Schreibweisen verwenden.

Fetter Text:

```markdown_wrap
Auf dem Tisch liegt ein **kleiner Schlüssel**.
```

Kursiver Text:

```markdown_wrap
Aus dem Nebenraum hörst du ein *leises Klopfen*.
```

Eine Zwischenüberschrift innerhalb einer Seite:

```markdown_wrap
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

```markdown_wrap
![Ein Schlüssel hängt an der Wand](key.jpg)
```

Der Text in den eckigen Klammern beschreibt das Bild und hilft zum Beispiel Menschen, die einen Screenreader verwenden.

<!-- Screenshot: BIF-Seite mit Bild -->
<!-- tutorial-screenshot
write-file: pages-starter/1.md <- file:theme-default.md.txt
write-file: pages-starter/3.md <- file:3-with-image.md.txt
write-file: pages-starter/key.jpg <- file:key.jpg
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
tab: preview
preview-reload
click: selector:.story-view-toggle
hold: 1.5s selector:.story-restart-control
click: Gehe in den Flur.
wait-for-text: Sieh im Büro nach.
click: Sieh im Büro nach.
wait-for-text: Gehe zurück in den Flur.
#move-mouse: No problems
zoom: 1.25
sleep: 1.5
-->

<img class='full full-shadow scroll-right' src='story-image.webp' alt='Ein Schlüsselbild ergänzt die Büroseite der Geschichte.'>

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

Zum Ausprobieren kannst du zum Beispiel diesen Block auf eine Seite schreiben:

```html_wrap
<script>
if (Math.chance(50)) {
    print("Aus dem Treppenhaus hörst du Schritte.");
} else {
    print("Im Treppenhaus bleibt alles still.");
}
</script>
```

Lade die Seite mehrmals über einen Neustart der Geschichte. Mal hörst du die Schritte, mal bleibt es still.

Zufall eignet sich gut für kleine Überraschungen. Wichtige Folgen sind oft interessanter, wenn sie von vorherigen Entscheidungen abhängen.

### Graph-Gruppen

Bei größeren Geschichten kannst du Seiten im Graphen zu Bereichen zusammenfassen.

Ganz oben in einer Seite kann zum Beispiel stehen:

```markdown_wrap
<!-- Schule -- Büro -->
```

Eine andere Seite könnte beginnen mit:

```markdown_wrap
<!-- Schule -- Flur -->
```

Diese Kommentare sind für die Leserinnen und Leser unsichtbar. Sie helfen nur dabei, den Graphen übersichtlich zu halten.

<!-- Screenshot: Graph mit gruppierten Seiten -->
<!-- tutorial-screenshot
write-file: pages-starter/1.md <- file:1-with-groups.md.txt
write-file: pages-starter/2.md <- file:2-with-groups.md.txt
write-file: pages-starter/3.md <- file:3-with-groups.md.txt
write-file: pages-starter/4.md <- file:4-with-groups.md.txt
write-file: pages-starter/5.md <- file:5-with-groups.md.txt
wait-for-file-newer: bif/.story-tools/analysis.json <- bif/pages-starter/1.md
tab: preview
preview-reload
hold: 1.5s selector:.story-restart-control
move-mouse: No problems
click: Fit graph
zoom: 1.25
sleep: 0.5
-->
<img class='full full-shadow' src='grouped-graph.webp' alt='Farbig hinterlegte Gruppen gliedern den Story-Graphen in mehrere Bereiche.'>

### JavaScript

Für normale Entscheidungen reichen die Markdown-Links fast immer aus:

```markdown_wrap
- [Gehe nach links.](5)
- [Gehe nach rechts.](6)
```

JavaScript wird interessant, wenn Entscheidungen nicht einfach fest im Text stehen sollen, sondern erst aus Daten oder dem aktuellen Zustand der Geschichte entstehen.

BIF stellt Skripten dafür unter anderem die Funktionen `print`, `presentChoice` und `goToPage` zur Verfügung. Das folgende Beispiel baut die Entscheidungsmöglichkeiten mit JavaScript zusammen. Wenn `has_key` den Wert `true` hat, kommt eine zusätzliche Möglichkeit dazu:

```html_wrap
<script>
const choices = [
    ["3", "Sieh im Büro nach."],
    ["4", "Gehe ins Treppenhaus."]
];

if (has_key) {
    print("Du spürst den kleinen Schlüssel in deiner Tasche.");
    choices.push(["5", "Öffne den Materialschrank."]);
}

const target = await presentChoice(choices);
await goToPage(target);
</script>
```

`presentChoice` zeigt die erzeugten Entscheidungen an und liefert zurück, welcher Wert ausgewählt wurde. In diesem Beispiel ist das direkt die Nummer der Zielseite. `goToPage` öffnet anschließend diese Seite.

Mit JavaScript könntest du auf diese Weise zum Beispiel Entscheidungen aus einer Liste erzeugen, kleine Rätsel programmieren oder Abläufe bauen, die mit normalen Markdown-Entscheidungen umständlich würden. Für die meisten Stellen einer Geschichte bleiben Markdown, Variablen und Bedingungen aber übersichtlicher.

<div class='hint'>
Benutze zusätzliche Technik nur dann, wenn sie deiner Geschichte etwas bringt. Mehr Variablen, Skripte oder Verzweigungen machen eine Geschichte nicht automatisch besser.
</div>

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

Beim Geschichtenerzählen gibt es keine allgemein gültige Liste wie die zwölf Prinzipien der Animation. Es gibt aber ein paar Bausteine, die fast immer helfen. Für eine BIF-Geschichte kannst du dir zunächst diese Fragen stellen.

**Figur**  
Wer handelt in der Geschichte? Was ist dieser Person wichtig? Eine Figur wird interessanter, wenn sie nicht nur irgendwo herumläuft, sondern etwas will.

**Ziel**  
Was möchte die Figur erreichen, finden, verhindern oder herausbekommen? Ein klares Ziel gibt der Geschichte eine Richtung.

**Setting**  
Wo und wann spielt die Geschichte? Ein Schulgebäude nach Unterrichtsschluss fühlt sich anders an als dieselbe Schule während der großen Pause. Überlege, welche Orte, Geräusche, Gegenstände oder Regeln für dein Setting typisch sind.

**Konflikt oder Hindernis**  
Warum kann die Figur ihr Ziel nicht sofort erreichen? Eine verschlossene Tür, eine Person, die nicht die Wahrheit sagt, zu wenig Zeit oder zwei Ziele, die nicht gleichzeitig erreicht werden können, erzeugen Spannung.

**Plot oder Handlung**  
Der Plot ist nicht nur eine Liste von Orten. Er beschreibt, was passiert und warum sich die Situation verändert. Eine einfache Grundform reicht oft schon:

```text
Eine Figur will etwas.
Etwas steht im Weg.
Sie muss handeln oder sich entscheiden.
Die Entscheidung hat eine Folge.
Dadurch entsteht eine neue Situation.
```

Diese Folge kann wieder zu einer neuen Entscheidung führen. So entwickelt sich die Handlung Schritt für Schritt.

**Entscheidungen**  
Was darf die Leserin oder der Leser wirklich entscheiden? Interessant wird eine Entscheidung, wenn beide Möglichkeiten einen Grund haben. Statt nur

```text
Gehe nach links.
Gehe nach rechts.
```

könnte die Wahl zum Beispiel lauten:

```text
Folge den Stimmen aus dem dunklen Flur.
Kehre zurück und hole Hilfe.
```

Jetzt steckt bereits ein kleiner Konflikt in der Entscheidung: Neugier gegen Vorsicht.

**Folgen**  
Eine gute Entscheidung verändert etwas. Manchmal sieht man die Folge sofort, manchmal erst später. Variablen und Bedingungen sind besonders nützlich für solche späteren Folgen: Eine Person erinnert sich an dein Verhalten, ein Gegenstand ist verbraucht oder ein neuer Weg wird möglich.

**Ende**  
Woran merkt man, dass die Geschichte abgeschlossen ist? Am Ende sollte sich etwas gegenüber dem Anfang verändert haben. Das Ziel kann erreicht oder verfehlt worden sein, eine Frage wurde beantwortet oder die Figur hat etwas verstanden. Eine interaktive Geschichte kann natürlich mehrere unterschiedliche Enden haben.

Für interaktive Geschichten sind außerdem ein paar praktische Regeln hilfreich:

- **Eine Seite braucht eine Aufgabe.** Sie sollte etwas zeigen, verändern oder zu einer interessanten Entscheidung führen.
- **Entscheidungen dürfen wieder zusammenlaufen.** Du musst nicht nach jeder Wahl zwei völlig getrennte Geschichten weiterschreiben. Zwei Wege können später wieder am selben Ort zusammentreffen.
- **Zeige Folgen.** Wenn eine Entscheidung wichtig war, sollte die Leserin oder der Leser irgendwann merken, was sie bewirkt hat.
- **Verzweige nicht zu schnell.** Beginne mit wenigen Seiten und erweitere die Geschichte erst, wenn der bisherige Teil funktioniert. Der Graph hilft dir dabei.
- **Teste ungewöhnliche Wege.** Besuche Orte erneut, triff Entscheidungen in anderer Reihenfolge und lass die Geschichte auch von jemand anderem spielen. Andere Menschen probieren oft Dinge aus, an die man beim Schreiben nicht gedacht hat.

<div class='hint'>
Eine kleine Geschichte braucht nicht alles. Für einen guten Anfang reichen oft schon eine Figur mit einem Ziel, ein Hindernis und zwei Entscheidungen, die unterschiedliche Folgen haben.
</div>

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