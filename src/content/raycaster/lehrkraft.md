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
<li>Sechs Aufgaben übersetzen <code>raycaster.c</code> im Workspace, als Gast: »Deine Kamera«, »Dein march«, »Dein is_free« und »Dein mip_level« rufen <code>update_camera</code>, <code>march</code>, <code>is_free</code> und <code>mip_level</code> der Lernenden auf, ohne ihr <code>main</code> zu starten (Prüfart <code>c</code>). Dafür muss <code>make</code> einmal gelaufen sein.</li>
<li><code>march</code> schickt Strahlen von zwei Stellen der Karte los, auch einen, der nicht 1 lang ist, und lässt einen Schritt Spielraum. Es gibt <code>march</code> nur bis zum Abschnitt »Von Kästchen zu Kästchen«, dann ersetzt <code>cast_ray</code> es; wer erst danach prüft, bekommt »gibt es <code>march</code> noch nicht«.</li>
<li>»Dein Abstand entlang der Blickrichtung« und »Dein u« rufen <code>draw_view</code> auf, mit einem eigenen <code>set_pixel</code> der Prüfung statt dem von PixelRAM. Der Abstand: Die Figur steht 2,5 Kästchen vor einer geraden Wand und schaut sie an, einmal nach links, einmal nach oben; die Wand muss in der Mitte und an beiden Rändern 72 Pixel hoch sein (das Fischauge gibt am Rand 54). <code>u</code>: Jedes Texel bekommt als Farbe die Nummer seiner Spalte in der Textur, die Schattentabelle ändert keine Farbe, und die Prüfung liest, welche Spalte mitten im Bild landet, einmal an einer Wand quer zur x-Achse, einmal an einer quer zur y-Achse. Beide Blickrichtungen sind so gewählt, dass die Zeilen gegen das Spiegeln nichts ändern; mit und ohne sie ist die Aufgabe gelöst.</li>
<li><code>is_free</code> wird an Stellen geprüft, an denen nur eine Ecke der Figur in einer Säule steht.</li>
<li><code>mip_level</code> wird an den Grenzen geprüft (1,9 und 2 Texel pro Pixel), an der letzten Stufe und mit ausgeschalteten Mipmaps.</li>
<li>Die übrigen Aufgaben prüfen Antworten: <code>delta_x</code> eines Strahls, Vorhersagen (warum sich die Wände wölben, was ohne getrennte Prüfung von x und y an einer Wand passiert, wie der Verlauf zum Horizont aussieht), ein Nachschlagen in der Schattentabelle und der Abstand einer Bodenzeile.</li>
<li>Am Ende beschreiben die Lernenden ihre eigene Welt (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>An sechs Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: den Rumpf von <code>update_camera</code>, <code>march</code> in kleinen Schritten, den Abstand entlang der Blickrichtung gegen das Fischauge, <code>is_free</code> mit den vier Ecken, <code>u</code> für die Texturen und <code>mip_level</code> für die Mipmaps. Die Seite geht danach mit ihrem eigenen Code weiter.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li>Unter jedem Kasten prüft eine Aufgabe den eigenen Code; wer sie löst, kann den Code der Seite sofort öffnen und mit dem eigenen vergleichen.</li>
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

#### Eigene Projekte

Nach der Seite haben die Lernenden ein Programm, das sie ganz verstehen. Daraus wird ein eigenes Spiel: Jede Idee nimmt etwas von der Seite wieder auf, sieht bei jeder und jedem anders aus und lässt sich ohne Prüfung bewerten, beim Vorführen und beim Erklären des eigenen Codes.

- **Ein Level mit Ziel** (eine Doppelstunde): eine eigene Karte mit einem Ausgang und einem Schalter oder Schlüssel, der eine Wand verschwinden lässt. Neu ist nur Spiellogik auf der Karte: Ein Kästchen ändert seinen Wert, und der nächste Strahl sieht dort keine Wand mehr. Dafür muss die Karte änderbar sein; als `const char *map[]` zeigt sie auf feste Texte, ein Schreiben darin bricht das Programm ab. `char map[MAP_H][MAP_W + 1]` mit denselben Zeilen geht. Zum Vorführen spielt jemand aus der Klasse das Level, ohne die Karte zu kennen.
- **Türen, die sich schieben** (eine Doppelstunde): eine Wandart, deren Wandscheibe in der Mitte ihres Kästchens steht und zur Seite gleitet, wenn man davorsteht. Der Strahl geht dafür ein halbes Kästchen weiter, und das `u` aus [»Wo trifft der Strahl die Wand?«](#wo-trifft-der-strahl-die-wand) entscheidet, ob er die Tür trifft oder an ihr vorbeigeht. Wer das erklären kann, hat die DDA verstanden.
- **Gegenstände und Gegner** (ein Projekt über zwei, drei Doppelstunden): Bilder aus dem 2D Game Studio, die mitten im Raum stehen (Sprites). Die [Kamera-Ebene](#die-kamera-ebene) kommt umgekehrt wieder vor: Aus der Lage eines Sprites zur Figur wird eine Bildspalte. Dazu schreibt `draw_view` je Spalte den Abstand der Wand in ein Array, einen Abstandspuffer; ein Sprite erscheint nur dort, wo er näher ist. Wer Gegner bewegt, braucht die Kollision ein zweites Mal.
- **Ein Labyrinth vom Computer** (eine Doppelstunde; mit Graphen ein Projekt): Die Karte entsteht beim Start aus einem Algorithmus, etwa einer Tiefensuche, die Gänge gräbt, und jeder Start ist ein neues Level. Dazu passt die Breitensuche aus dem [Routenplaner](/routenplaner), die den kürzesten Weg zum Ausgang findet.
- **Licht an Orten** (eine Doppelstunde): Jedes Kästchen bekommt ein eigenes Licht, eine Fackel an der Wand, ein dunkler Gang. Die [Schattentabelle](#eine-palette-aus-farbtönen) bleibt; nur der Index, mit dem man in ihr nachschlägt, kommt aus der Karte statt allein aus dem Abstand. Zu bewerten ist, wie jemand die Helligkeit an den Grenzen der Kästchen weich macht.

Das Gegenstück ohne Echtzeit und mit echten Farben ist der [Raytracer](/raytracing).
