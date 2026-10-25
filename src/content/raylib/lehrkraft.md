Ein 3D-Spiel, das auf die Lernenden reagiert: eine Welt aus Blöcken, durch die sie laufen, in der sie bauen und abbauen, mit Licht, Tag und Nacht. Anders als bei Anaglyph, wo eine Szene beschrieben und angezeigt wird, steht hier die Echtzeit im Mittelpunkt: eine Spielschleife, Eingaben, Bewegung pro Zeit, Kollision, ein Strahl zum Zielen und ein eigener Licht-Shader.

<table class='table'>
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
<li>An fünf Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: die Säulen der Hügel in <code>generate_world</code>, <code>view_direction</code>, <code>collides</code>, die Schwerkraft in <code>move_player</code> und die Schleife in <code>aim</code>. Die Seite gibt dazu ein Gerüst mit einer Lücke (<code>// hier: …</code>) und geht danach mit ihrem eigenen Code weiter.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Beispiel von Hand und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li>Unter jedem Kasten prüft eine Aufgabe den eigenen Code; wer sie löst, kann den Code der Seite sofort öffnen und vergleichen.</li>
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
- »Deine Hügel«, »Deine Blickrichtung«, »Dein collides«, »Deine Schwerkraft« und »Dein aim« übersetzen `main.c` im Workspace, als Gast, mit den Headern von raylib, und rufen die Funktionen der Lernenden auf, ohne ihr `main` zu starten (Prüfart `c`). Statt Fenster, Tasten und Zufall stehen kleine Ersatzfunktionen in der Prüfung, und die Welt ist für jeden Fall eine eigene: leer mit einem einzelnen Stein, ein Boden aus Stein oder die Hügel von `generate_world`. Die Rückmeldung nennt den ersten Fall, der nicht passt, und was dort richtig wäre; für `collides` am Rand der Welt auch den Unterschied zwischen `(int)` und `floorf`.
- »Ein Körper«, »Bauen und abbauen«, »Licht«, »Fackeln und Nebel« und »Texturen« lesen `main.c` beziehungsweise `light.fs` und suchen typische Bausteine (`collides`, `vy -=`, `aim`, `MOUSE_BUTTON_RIGHT`, `rlNormal3f`, `BeginShaderMode`, `torches[`, `LoadTexture`, `rlTexCoord2f`); die Karte nennt die Zeile, in der sie etwas gefunden hat.
- »Von oben« und »Kleben oder gleiten?« sind Vorhersagen.
- Die Rechenaufgaben (Felder des Arrays, Schritt pro Bild, Sprunghöhe, Seiten einer Wiese, Skalarprodukt der Sonne, Abfall des Fackellichts) haben eine Lösung mit Rechenweg.

#### Weiter

Die Ideen am Ende der Seite reichen von neuen Blockarten bis zu einem kleinen Spiel mit Zeitmessung. Wer weiter will, findet unter [raylib.com/examples](https://www.raylib.com/examples.html) Beispiele zu Modellen, Texturen, Tönen und Schatten, die alle mit derselben Vorlage übersetzen.
