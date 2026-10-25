Lernende schreiben in C mit PixelRAM einen Raytracer, Schritt für Schritt: von einem Strahl je Pixel über Licht, Schatten, Spiegel und Glas bis zu Lack, Metall und matten Spiegeln. Gerechnet wird nur mit Vektoren (Addieren, Skalieren, Skalarprodukt, Kreuzprodukt), ohne Matrizen.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Die ersten Abschnitte von <a href='/pixelram'>Echtzeitgrafik mit C</a> bis »Ein Bild aus einer Formel«; aus der Mathematik der Satz des Pythagoras im Raum, quadratische Gleichungen und Sinus und Kosinus.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Vektoren, ein Strahl je Pixel, die erste Kugel mit der quadratischen Gleichung, glatte Kanten aus vielen zufälligen Strahlen je Pixel.</li>
<li><b>2. Doppelstunde:</b> Licht nach Lambert und Gamma, mehrere Kugeln und der Boden, Schatten mit der Shadow Acne.</li>
<li><b>3. Doppelstunde:</b> eine Kamera aus Kreuzprodukten, eine Textur aus Längen- und Breitengrad, Spiegel, Glas mit Totalreflexion und Fresnel, die hohle Glaskugel wie in Whitteds Bild von 1979.</li>
<li><b>4. Doppelstunde:</b> weiche Schatten und Himmelslicht (wie das Antialiasing viele zufällige Strahlen und ihr Durchschnitt), Lack mit Glanzpunkten, Metall und matte Spiegel, eine eigene Szene.</li>
<li><b>Für Schnelle</b> (auf der Seite als »Extra« markiert): Tiefenschärfe, Path Tracing und Belichtung mit Tone Mapping, Stufen 15, 16 und 17, jede auf dem fertigen Programm.</li>
<li>Path Tracing zeigt sich neben einer petrolfarbenen und einer orangen Wand in der Sonne, die die matten weißen Kugeln und den Boden vor ihnen färben (alles ohne Spiegelung, damit nur dieses Licht den Unterschied macht). Die Wände sind Kugeln mit dem Radius 1000 wie in Kevin Beasons smallpt. In der offenen Szene bei Tag ist sein Beitrag zu klein, um ihn zu sehen.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/raytracer</code> (ein eigener Klon des PixelRAM-Starters), eine einzige Datei <code>raytracer.c</code>, übersetzt mit <code>make PROGRAM=raytracer</code>.</li>
<li>Jede Stufe liegt vollständig unter <a href='https://github.com/specht/workspace/tree/main/src/content/raytracing/stufen'><code>src/content/raytracing/stufen</code></a>, von <code>01-himmel.c</code> über <code>14-metall.c</code> bis <code>raytracer.c</code> (gut 400 Zeilen), danach <code>15-schaerfe.c</code>, <code>16-pathtracing.c</code> und <code>17-belichtung.c</code>. Wer den Anschluss verliert, macht mit der passenden Stufe weiter.</li>
<li><code>06-akne.c</code> ist die Stufe mit Schatten, aber noch ohne <code>EPS</code>; <code>10-hohl.c</code> ist <code>10-glas.c</code> mit der hohlen Glaskugel, die ab dann in jeder Stufe der Szene steht (nicht in der Wand-Szene von Stufe 16).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Vier Aufgaben übersetzen <code>raytracer.c</code> im Workspace, als Gast, und rufen Funktionen der Lernenden auf, ohne ihr <code>main</code> zu starten (Prüfart <code>c</code>): <code>hit_floor</code>, das Schachbrett über <code>nearest_hit</code>, die Kamera mit <code>set_camera</code> und <code>reflect</code>. Dafür muss <code>make</code> einmal gelaufen sein, das <code>pixelram.h</code> holt.</li>
<li>Das Schachbrett wird an Stellen nebeneinander geprüft, auch links der Mitte, wo <code>%</code> und <code>(int)</code> bei negativen Zahlen Fehler zeigen.</li>
<li>Die übrigen Aufgaben prüfen Antworten: Rechnungen mit den Formeln der Seite (<code>sx</code> eines Pixels, <code>t</code> eines Treffers, die Helligkeit, der Boden, das Kreuzprodukt, die Spiegelung, der Grenzwinkel der Totalreflexion, wie viel Lack spiegelt), drei Vorhersagen vor dem Ausprobieren (gibt es ohne Schattenstrahl Schatten, die Shadow Acne, ein Spiegel mit <code>MAX_DEPTH</code> 0) und eine Frage, warum das Bild mit den Durchgängen glatter wird.</li>
<li>Am Ende beschreiben die Lernenden ihre eigene Szene (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td><ul>
<li>An fünf Stellen schreiben die Lernenden den Code zuerst selbst, nachdem die Seite erklärt hat, was er tun soll: <code>hit_floor</code> aus der Formel, das Schachbrett, den Schattenstrahl in <code>sunlight</code>, die vier Zeilen der Kamera und <code>reflect</code> mit dem neuen <code>trace</code>. Die Seite geht danach mit ihrem eigenen Code weiter.</li>
<li>Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.</li>
<li>Unter jedem Kasten prüft eine Aufgabe die eigene Funktion (außer beim Schattenstrahl, der von der Szene abhängt); wer sie löst, kann den Code der Seite sofort öffnen und mit dem eigenen vergleichen.</li>
</ul></td></tr>
<tr><th>Tempo</th><td><ul>
<li>Das fertige Programm braucht im Browser etwa 30 bis 50 ms je Durchgang (320 × 180 Pixel), also 20 bis 30 Durchgänge in der Sekunde. Nach zwei, drei Sekunden ist das Bild fast glatt, die matten Metalle ab Stufe 14 brauchen etwa zehn Sekunden.</li>
<li>Die hohle Glaskugel ab Stufe 10 macht jeden Durchgang spürbar langsamer (nativ gemessen etwa anderthalbmal so lang): In ihrer dünnen Wand wird ein Strahl immer wieder gespiegelt und gebrochen, bis <code>MAX_DEPTH</code> erreicht ist.</li>
<li>Ab Stufe 3 rechnet das Programm in jedem Bild einen weiteren Durchgang; bewegt sich die Kamera, beginnt es von vorn.</li>
</ul></td></tr>
</table>

#### Was schiefgeht

- **`dir` ohne `normalize`.** Die kurze Form der quadratischen Gleichung setzt voraus, dass `dir` die Länge 1 hat. Ohne `normalize` sind die Kugeln verzerrt oder verschwinden.
- **Shadow Acne.** Mit `t > 0` statt `t > EPS` liegen auf den hellen Feldern dunkle Sprenkel; die Seite zeigt das absichtlich, mit einer Vorhersage davor. Die Augenhöhe 1,2 ist so gewählt, dass man es sieht: Bei einer runden Höhe wie 1 rechnet der Computer den Treffpunkt auf dem Boden genau aus, und die Akne bleibt aus. Weil das Bild ab Stufe 3 der Durchschnitt vieler Durchgänge ist, sieht man die Sprenkel vor allem in der ersten Sekunde; danach verdünnen sie sich zu einem kaum sichtbaren Grauschleier. Ein guter Moment für den Unterschied zwischen Rauschen, das der Durchschnitt beseitigt, und einem Fehler, den er nur versteckt.
- **Die Vorab-Deklaration fehlt.** `glass` ruft `trace` auf, das weiter unten steht; ohne die Zeile `vec trace(vec origin, vec dir, int depth);` meldet der Compiler einen Fehler.
- **Die hohle Kugel sieht aus wie eine volle oder ist innen schwarz.** Die innere Kugel aus Luft braucht `.glass = 1 / 1.5` (Brechungsindex innen geteilt durch außen); mit `1.5` wird sie zu einer zweiten Glaskugel im Glas. Ohne `.color = {1, 1, 1}` ist ihre Farbe 0, und `glass` multipliziert alles Gebrochene damit. Ihr Schatten ist etwas dunkler als der der vollen Kugel, weil `light_through` beide Kugeln als Glas zählt (0,6 · 0,6); in Wirklichkeit wäre er heller. Das ist eine bewusste Vereinfachung.
- **Die zweite Lösung in `hit_sphere` fehlt.** Dann findet ein Strahl im Glas die Rückseite der Kugel nicht, und die Glaskugel ist innen schwarz oder zeigt den Himmel.
- **Das Schachbrett ist auf einer Hälfte hell.** Mit `% 2 == 1` statt `% 2 == 0` fehlen die dunklen Felder dort, wo die Summe der Feldnummern negativ ist: In C ist `-3 % 2` gleich −1. Die Lösung erklärt das.
- **Das Bild steht auf dem Kopf oder ist gespiegelt.** In der Kamera ist die Reihenfolge im Kreuzprodukt vertauscht: `cross((vec){0, 1, 0}, forward)` lässt `right` nach links zeigen, `cross(forward, right)` lässt `up` nach unten zeigen.
- **`passes` wird beim Bewegen nicht auf 0 gesetzt.** Dann mischt das Bild alte und neue Kamerapositionen und verschmiert.
- **Kein Glanzpunkt.** Ein Glanzpunkt ist die gespiegelte Sonnenscheibe. Ist sie nur wenig heller als <code>sun_color</code>, spiegelt der Lack mit seinen 4 Prozent fast nichts davon; darum wird sie in Stufe 13 so hell, wie ihre kleine Fläche verlangt (etwa fünfzigmal). Wer <code>SUN_SIZE</code> größer macht, bekommt größere, aber schwächere Glanzpunkte.
- **Path Tracing glitzert.** Ohne die Grenze von 1 in <code>bounce</code> bringen Strahlen, die über einen Spiegel in die Sonne schauen, so viel Licht mit, dass helle Punkte noch nach Hunderten Durchgängen glitzern. Ein Durchgang dauert mit Path Tracing etwa doppelt so lange.
- **Gold sieht aus wie Silber.** Ohne <code>mul(reflected, h.mat.color)</code> spiegelt ein Metall farblos.
- **Alles ist zu hell oder zu dunkel.** Farben über 1 schneidet `to_byte` ab. Wer die Sonne heller macht, sollte die Farben der Materialien unter 1 lassen.

#### Mathematik

Die quadratische Gleichung, das Skalarprodukt als Maß für Winkel, das Kreuzprodukt, Bogenmaß bei `atan2` und `asin`, das Brechungsgesetz: Die Seite ist eine Gelegenheit, Vektorrechnung aus dem Mathematikunterricht anzuwenden. Die Herleitung der Brechung als Vektorformel steht nicht auf der Seite; wer möchte, leitet sie mit der Zerlegung in einen Teil entlang der Normalen und einen senkrecht dazu her, wie bei der Spiegelung.

#### Weiter

Die Seite endet mit Ideen: eigene Szenen und Texturen, weitere Formen (Scheibe, Würfel), Tiefenschärfe und Path Tracing, bei dem der Strahl für das Himmelslicht mit `trace` weiterverfolgt wird. Das Gegenstück mit Echtzeit und 8-Bit-Farben ist der [Raycaster](/raycaster).
