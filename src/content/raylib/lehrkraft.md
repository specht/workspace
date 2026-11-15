Ein 3D-Spiel, das auf die Lernenden reagiert: eine Welt aus Blöcken, durch die sie laufen, in der sie bauen und abbauen, mit Licht, Tag und Nacht. Anders als bei Anaglyph, wo eine Szene beschrieben und angezeigt wird, steht hier die Echtzeit im Mittelpunkt: eine Spielschleife, Eingaben, Bewegung pro Zeit, Kollision, ein Strahl zum Zielen und ein eigener Licht-Shader.

<table class='table'>
<tr><th>Ziele</th><td>Die Lernenden können danach<ul>
<li>eine Welt aus Blöcken als Array beschreiben und Koordinaten im Raum mit drei Achsen lesen,</li>
<li>die Blickrichtung aus zwei Winkeln berechnen und Bewegung pro Zeit mit <code>dt</code> statt pro Bild schreiben,</li>
<li>Kollisionen eine Richtung nach der anderen prüfen und begründen, warum man so an einer Wand entlanggleitet, statt an ihr zu kleben,</li>
<li>einen Strahl in kleinen Schritten verfolgen, um den Block zu finden, auf den man zielt, und die Helligkeit einer Fläche in einem Shader berechnen.</li>
</ul></td></tr>
<tr><th>Voraussetzungen</th><td>C auf dem Niveau von »Echtzeitgrafik mit C« (PixelRAM): Variablen, Schleifen, Funktionen, <code>#define</code>. Vektoren, Bogenmaß und das Skalarprodukt werden unterwegs erklärt; Sinus und Kosinus sollten aus dem Mathematikunterricht bekannt sein.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten, Koordinaten und Kamera, die Welt als Array mit Hügeln und Bäumen.</li>
<li><b>2. Doppelstunde:</b> mit eigenen Augen (Blickrichtung aus zwei Winkeln, Laufen mit <code>dt</code>), Schwerkraft und Wände.</li>
<li><b>3. Doppelstunde:</b> Zielen, Bauen, Abbauen, die Leiste; danach Licht mit eigenen Flächen und dem Shader.</li>
<li><b>4. Doppelstunde:</b> Tag und Nacht, Fackeln, Nebel.</li>
<li><b>5. Doppelstunde:</b> Texturen, eigene Kacheln im 2D Game Studio, die eigene Welt und die Abgabe.</li>
<li><b>Für Schnelle</b> (auf der Seite als »Extra« markiert): zwei Kapitel zum Lesen ohne Code, »Was vorne liegt« (Tiefenpuffer, BSP-Baum) und »Wenn die Welt wächst« (Chunks, Culling, Speicher).</li>
</ul></td></tr>
<tr><th>Vorlage</th><td><code>specht/raylib-starter</code> wird nach <code>/workspace/raylib-starter</code> geklont. <code>make</code> übersetzt <code>main.c</code> mit dem im Workspace installierten raylib zu <code>main.html</code> und bettet die Shader <code>light.vs</code> und <code>light.fs</code> und die Textur <code>textures.png</code> ein. Die Webseite um das Programm kommt beim ersten <code>make</code> von PixelRAM (Konsole für <code>printf</code>, Vollbild, CRT-Filter).</td></tr>
<tr><th>Zwischenstände</th><td>Das Programm nach jedem Kapitel liegt in diesem Repository unter <code>src/content/raylib/stufen/</code> (<code>1.c</code> bis <code>8.c</code>, dazu <code>6.fs</code>, <code>7.fs</code>, <code>8.fs</code> und <code>8.vs</code>). Wer den Anschluss verloren hat, kann damit weitermachen; jede Stufe übersetzt ohne Warnung.</td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>An acht Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: die Säulen der Hügel in <code>generate_world</code>, <code>visible</code>, <code>view_direction</code>, <code>collides</code>, die Schwerkraft in <code>move_player</code>, die Schleife in <code>aim</code>, die beiden Zeilen für den Nebel in <code>light.fs</code> und <code>tile_for</code>. Die Seite gibt dazu meist ein Gerüst mit einer Lücke (<code>// hier: …</code>) und geht danach mit ihrem eigenen Code weiter.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Beispiel von Hand und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li>Unter jedem Kasten prüft eine Aufgabe den eigenen Code; wer sie löst, kann den Code der Seite sofort öffnen und vergleichen. Den Nebel prüft »Dein Nebel« am Text des Shaders, weil sich ein Shader im Workspace nicht ohne Fenster ausführen lässt; ob er wirklich stimmt, zeigt das Bild.</li>
<li>Wo sich <code>main</code> oder <code>light.fs</code> nur an wenigen Stellen ändern, zeigt die Seite nur diese Stellen, mit blassen Zeilen drumherum, die schon in der Datei stehen. Wer dabei eine Stelle übersieht, vergleicht mit der Stufe des Kapitels.</li>
</ul></td></tr>
<tr><th>Abgabe</th><td>»Deine Welt« kopiert den Ordner <code>raylib-starter</code> ohne <code>.git</code>. Die abgegebene <code>main.html</code> enthält alles und lässt sich in der Klassenübersicht mit »Starten« spielen.</td></tr>
</table>

#### Was schiefgeht

- Ein vergessenes `EndMode3D()` oder `EndShaderMode()` zeigt ein leeres oder verzerrtes Bild ohne Fehlermeldung.
- Ein falsch geschriebener Name in `GetShaderLocation` liefert −1, und der Wert kommt im Shader still nicht an: Licht oder Fackeln fehlen dann einfach.
- Fehler im Shader selbst stehen in der Konsole der Seite (Terminal-Symbol unten rechts im Bild).
- Läuft die Figur auf langsamen Rechnern merklich langsamer, fehlt `dt` bei der Bewegung.
- Die Maus lässt sich im Browser nur nach einem Klick fangen; wer nach <kbd>Esc</kbd> nicht mehr klickt, dreht nur noch mit den Pfeiltasten.
- Tasten kommen erst an, wenn einmal ins Bild geklickt wurde.

#### Eigene Texturen

Die Kacheln zeichnen die Lernenden im 2D Game Studio als acht Frames eines Sprites und speichern sie mit »Funktionen« › »Sprite« › »Als Bild speichern« › »alle Frames dieses Zustands nebeneinander« als Streifen. Die Datei kommt per Ziehen in den Explorer in den Workspace und heißt dort `textures.png`. Wer die Reihenfolge der Kacheln vertauscht, bekommt Gras aus Stein; die Reihenfolge steht im `enum` am Anfang des Kapitels.

#### Leistung

Bis Kapitel »Licht« zeichnet das Programm jeden sichtbaren Block als ganzen Würfel mit Kanten, auf schwachen Rechnern mit wenigen Bildern pro Sekunde. Das ist gewollt: Mit den eigenen Flächen sinkt die Zahl der Dreiecke deutlich, und die Anzeige oben links macht den Unterschied messbar.

#### Die Aufgaben

- »Übersetzt« sieht nach, ob `main.html` entstanden ist.
- »Deine Hügel«, »Dein visible«, »Deine Blickrichtung«, »Dein collides«, »Deine Schwerkraft«, »Dein aim« und »Dein tile_for« übersetzen `main.c` im Workspace, als Gast, mit den Headern von raylib, und rufen die Funktionen der Lernenden auf, ohne ihr `main` zu starten (Prüfart `c`). Statt Fenster, Tasten und Zufall stehen kleine Ersatzfunktionen in der Prüfung, und die Welt ist für jeden Fall eine eigene: leer mit einem einzelnen Stein, ganz aus Stein mit einem einzelnen Feld Luft, ein Boden aus Stein oder die Hügel von `generate_world`. Die Rückmeldung nennt den ersten Fall, der nicht passt, und was dort richtig wäre; für `collides` am Rand der Welt auch den Unterschied zwischen `(int)` und `floorf`.
- »Ein Körper«, »Bauen und abbauen«, »Licht«, »Fackeln und Nebel« und »Texturen« lesen `main.c` beziehungsweise `light.fs` und suchen typische Bausteine (`collides`, `vy -=`, `aim`, `MOUSE_BUTTON_RIGHT`, `rlNormal3f`, `BeginShaderMode`, `torches[`, `LoadTexture`, `rlTexCoord2f`); die Karte nennt die Zeile, in der sie etwas gefunden hat.
- »Dein Nebel« liest `light.fs` und sucht die Teile der Rechnung: die Entfernung `length(position - eyes)`, `- 16.0`, `/ 20.0`, `clamp` (oder `smoothstep(16.0, 36.0, …)`), `mix(result, sky, fog)` und `gl_FragColor`. Die Rückmeldung nennt den ersten Teil, der fehlt.
- »Von oben« und »Kleben oder gleiten?« sind Vorhersagen.
- Die Rechenaufgaben (Felder des Arrays, Schritt pro Bild, Sprunghöhe, Seiten einer Wiese, Skalarprodukt der Sonne, Abfall des Fackellichts) haben eine Lösung mit Rechenweg.

#### Eigene Projekte

Wie man ein Projekt mit Meilensteinen und Kriterien begleitet: [Eigene Projekte begleiten](/lehrmaterial-projekte).

Die Seite endet mit eigenen Ideen für die Welt, von Blockarten bis zur Goldsuche auf Zeit. Die Ideen hier sind größer; jede nimmt etwas von der Seite wieder auf. Bewertet wird beim Vorführen und am Erklären des eigenen Codes. Beispiele zu Modellen, Tönen und Schatten gibt es unter [raylib.com/examples](https://www.raylib.com/examples.html); sie übersetzen mit derselben Vorlage.

- **Ein Spiel in der Blockwelt** (ein Projekt): ein Parcours mit Startblock, Zielblock und Zeit, ein Labyrinth unter der Erde oder eine Insel, auf der man vor der Nacht ein Haus bauen muss. Der Strahl aus [»Wohin zielst du?«](#wohin-zielst-du) und die Kollision aus [»Fest oder nicht?«](#fest-oder-nicht) kommen wieder, dazu Spielregeln, die die Lernenden selbst erfinden. Zum Vorführen spielt jemand aus der Klasse. *Meilensteine:* die Spielregeln mit Ziel und Ende; ein Level, in dem Start und Ziel funktionieren; das ganze Spiel im Spieltest. *Kriterien:* Das Spiel lässt sich ohne Erklärung bis zum Ziel spielen, die eigenen Regeln greifen, und die Lernenden erklären, wie das Spiel den Strahl oder die Kollision nutzt.
- **Blöcke, die sich bewegen** (eine Doppelstunde): Sand, der rieselt, Wasser, das in Löcher fließt, Gras, das sich ausbreitet. Wie bei einem zellulären Automaten schaut jeder Block bei jedem Takt auf seine Nachbarn im Array aus [»Die Welt ist ein Array«](#die-welt-ist-ein-array); schwierig ist, dass ein Block im selben Takt nicht zweimal fällt. *Meilensteine:* die Regel jeder Blockart auf Papier (wohin sie fällt oder fließt); eine Blockart, die sich nach ihrer Regel bewegt; viele Blöcke, ohne dass einer zweimal fällt. *Kriterien:* Die Blöcke folgen ihrer Regel, keiner bewegt sich in einem Takt zweimal, und die Lernenden erklären, wie sie das verhindern.
- **Die Welt speichern** (eine Doppelstunde): Die Welt wird in eine Datei geschrieben und beim Start wieder geladen. Sie besteht zum großen Teil aus Luft und Stein in langen Reihen; mit der Lauflängenkodierung aus [»Komprimieren mit Lauflängen«](/rle) wird die Datei ein Bruchteil so groß, und die Lernenden messen, wie viel. *Meilensteine:* das Format der Datei auf Papier; Speichern und Laden ohne Kompression; die Datei mit Lauflängen und ihre gemessene Größe. *Kriterien:* Eine geladene Welt gleicht der gespeicherten Block für Block, und die Lernenden nennen, wie viel kleiner die Datei wird und warum.
- **Ein eigener Shader-Effekt** (eine Doppelstunde): Wasser, das sich bewegt, ein Unterwasserblau unter der Wasserlinie, Blitze in der Nacht. Der Shader aus [»Ein Shader rechnet das Licht«](#ein-shader-rechnet-das-licht) bekommt dafür einen Wert mehr, die Zeit oder die Höhe der Kamera. *Meilensteine:* der Effekt und der neue Wert für den Shader auf Papier; der Wert kommt im Shader an; der Effekt in der Welt. *Kriterien:* Der Effekt reagiert auf den neuen Wert, und die Lernenden erklären die Zeile im Shader, die ihn verwendet.
