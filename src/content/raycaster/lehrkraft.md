Lernende schreiben in C mit PixelRAM einen Raycaster wie Wolfenstein 3D, Schritt für Schritt: von einer Karte aus Kästchen über Strahlen und Wände bis zu Texturen mit Licht, Böden und Decken.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die ersten Abschnitte von <a href='/pixelram'>Echtzeitgrafik mit C</a> bis »Ein Bild aus einer Formel«; aus der Mathematik Vektoren in der Ebene, Sinus und Kosinus und das Skalarprodukt.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> eine Karte aus Kästchen von oben, Bewegung mit der Zeit, Strahlen über eine Kamera-Ebene (Vektoren, keine Matrizen), erst in kleinen Schritten.</li>
<li><b>2. Doppelstunde:</b> die Grid-DDA, Wände mit dem direkten Abstand (Fischauge) und mit dem Skalarprodukt (gerade), Kollision mit Gleiten an Wänden.</li>
<li><b>3. Doppelstunde:</b> eine Palette aus 16 Farbtönen mit je 16 Helligkeiten und eine Schattentabelle wie DOOMs COLORMAP, Texturen aus dem 2D Game Studio, Boden und Decke Zeile für Zeile.</li>
<li><b>4. Doppelstunde:</b> Mipmaps, eine eigene Welt.</li>
<li><b>Für Schnelle</b> (auf der Seite als »Extra« markiert): »Das fertige Programm« mit Böden aus der Karte, Dithering, einer durchsichtigen Karte und einer Fackel (Stufe 12), und der Ausblick »Nach Wolfenstein: DOOM«.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/raycaster</code> (ein eigener Klon des PixelRAM-Starters), eine einzige Datei <code>raycaster.c</code>, übersetzt mit <code>make PROGRAM=raycaster</code>.</li>
<li>Jede Stufe liegt vollständig unter <a href='https://github.com/specht/workspace/tree/main/src/content/raycaster/stufen'><code>src/content/raycaster/stufen</code></a>, von <code>01-karte.c</code> bis <code>12-fertig.c</code> (knapp 570 Zeilen), ab Stufe 9 mit der <code>palette.h</code>, die <code>palette.rb</code> aus den mitgelieferten Kacheln macht.</li>
<li>Wer den Anschluss verliert, macht mit der passenden Stufe weiter.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>»Deine Kamera« und »Dein is_free« übersetzen <code>raycaster.c</code> im Workspace, als Gast, und rufen <code>update_camera</code> und <code>is_free</code> der Lernenden auf, ohne ihr <code>main</code> zu starten (Prüfart <code>c</code>). Dafür muss <code>make</code> einmal gelaufen sein.</li>
<li><code>is_free</code> wird an Stellen geprüft, an denen nur eine Ecke der Figur in einer Säule steht.</li>
<li>Die übrigen Aufgaben prüfen Antworten: <code>delta_x</code> eines Strahls, Vorhersagen (warum sich die Wände wölben, was ohne getrennte Prüfung von x und y an einer Wand passiert, wie der Verlauf zum Horizont aussieht), ein Nachschlagen in der Schattentabelle und der Abstand einer Bodenzeile.</li>
<li>Am Ende beschreiben die Lernenden ihre eigene Welt (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>An vier Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: den Rumpf von <code>update_camera</code>, den Abstand entlang der Blickrichtung gegen das Fischauge, <code>is_free</code> mit den vier Ecken und <code>u</code> für die Texturen. Die Seite geht danach mit ihrem eigenen Code weiter.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li><code>update_camera</code> und <code>is_free</code> prüfen die Aufgaben unter dem Kasten; wer sie löst, kann den Code der Seite sofort öffnen und mit dem eigenen vergleichen. Den Abstand gegen das Fischauge und <code>u</code> prüft keine Aufgabe, man sieht sie im Bild.</li>
</ul></td></tr>
<tr><th>Steuerung</th><td><kbd>W</kbd> <kbd>S</kbd> oder <kbd>↑</kbd> <kbd>↓</kbd> vor und zurück, <kbd>A</kbd> <kbd>D</kbd> seitwärts, <kbd>←</kbd> <kbd>→</kbd> drehen; ab Stufe 5 <kbd>Tab</kbd> für die Karte, ab Stufe 11 <kbd>M</kbd> für die Mipmaps. Vorher einmal ins Bild klicken, damit es die Tasten bekommt.</td></tr>
</table>

#### Was schiefgeht

- **y zeigt nach unten.** Wie auf dem Bildschirm wächst y nach unten; ein größerer Winkel dreht im Uhrzeigersinn, und »rechts« von `(x, y)` ist `(−y, x)`. Wer y nach oben denkt, bekommt an genau diesen Stellen Vorzeichenfehler.
- **Das Bild ist seitenverkehrt.** Zeigt `plane` in `update_camera` nach links (`plane_x = dir_y * PLANE`, `plane_y = -dir_x * PLANE`), schaut die linke Bildspalte nach rechts: Das Bild ist gespiegelt, und die Strahlen in der Karte sehen trotzdem richtig aus.
- **Die Karte ist nicht geschlossen.** Die DDA liest die Karte ohne Prüfung am Rand; ein Loch im Rand lässt einen Strahl aus dem Array laufen.
- **Division durch null.** Ein Strahl parallel zu einer Achse hat `ray_x == 0`. `1 / 0.0` ist unendlich, aber `0 · unendlich` keine Zahl; darum `1e30`.
- **Textur gespiegelt.** Ohne die Zeile mit `u = 1 - u` erscheinen Texturen auf zwei der vier Seiten eines Kästchens spiegelverkehrt; bei symmetrischen Mustern fällt es kaum auf, bei Schrift sofort.
- **Kollision.** Die Vier-Ecken-Prüfung reicht, solange `PLAYER_RADIUS` unter 0,5 bleibt. Ohne die Grenze für `dt` springt die Figur nach einem Ruckler (Tab im Hintergrund) durch dünne Wände.
- **`light_at` und `shaded` stehen zu weit unten.** In Stufe 9 benutzt `draw_wall_column` `shaded`; die beiden Funktionen müssen darüberstehen, sonst meldet der Compiler einen Fehler.

#### Mathematik

- **Warum `H / distance` passt:** `PLANE` ist 0,89 ≈ W / (2H); dann ist ein Stück Wand im Abstand 1 auf dem Bild so breit wie hoch. Wer `PLANE` ändert, staucht oder streckt die Wände.
- **Das Skalarprodukt in Stufe 6** ist genau das `t` der DDA, weil `dir` die Länge 1 hat und senkrecht auf `plane` steht; wer mag, lässt die Lernenden das entdecken.
- **Die Mipmap-Stufe für den Boden** (geometrisches Mittel quer und in die Tiefe) ist ein Kompromiss: Das Maximum, wie Grafikkarten es nehmen, macht den Boden verwaschen, die quere Richtung allein flimmert noch.

#### Texturen

- Ab dem Abschnitt »Texturen« kommen die Texturen aus dem 2D Game Studio: sieben Kacheln (Boden, fünf Wandarten, Decke) als Streifen `textures.png`. Mitgeliefert ist ein Streifen mit Ziegel, Stein, Holz, Metall und Moos; eigene Kacheln sind freiwillig.
- Das fertige Werkzeug `palette.rb` (Ruby, braucht `gem install chunky_png`; beides holt `wget` aus `specht/workspace-files`) macht daraus mit Median Cut eine Palette mit 256 Farben (16 reserviert für Karte und Anzeige), die Schattentabelle durch Suche der nächsten Farbe und die Texturen als Farbnummern, alles in `palette.h`; mit `--dither` nach Floyd und Steinberg. Jeder Schritt speichert ein Bild.
- Die Palette aus 16 Farbtönen vom Abschnitt »Licht« wird dabei ersetzt; die Aufgabe dazu bleibt dort. `make` übersetzt auch neu, wenn sich nur `palette.h` geändert hat.
- Das Werkzeug liegt auch unter `src/content/raycaster/werkzeug/`, die alten Texturen aus Code unter `bilder/texturen-aus-code.c`.

#### Weiter

Türen, Sprites mit einem Abstandspuffer je Spalte, die Maus zum Umsehen, Licht je Kästchen. Das Gegenstück ohne Echtzeit und mit echten Farben ist der [Raytracer](/raytracing).
