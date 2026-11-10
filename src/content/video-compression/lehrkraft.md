Lernende bauen aus Muybridges Fotos von 1878 einen Film, lesen Videos mit Python und numpy Bild für Bild, lassen Bilder aus und zeigen das vorige länger, bis es ruckelt, bauen Halbbilder wie im alten Fernsehen und trennen Helligkeit von Farbe, um an der Farbe zu sparen. Die Seite wächst zu einem Weg über Farbe, Blöcke und Bewegungsschätzung bis zu H.264, AV1 und AV2; bis dahin steht sie nur in der Entwicklung im Katalog.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Python mit numpy wie in <a href='/audio-compression'>Audio komprimieren</a> (Arrays, Ausschnitte mit <code>::</code>, <code>np.repeat</code>); <a href='/jpeg'>Eine JPEG-Datei decodieren</a> für Blöcke und die DCT, die später kurz wiederholt werden.</td></tr>
<tr><th>Sprache</th><td>Nur Python, mit numpy, Pillow und ffmpeg aus dem Workspace-Image.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> was eine Sekunde Video unkomprimiert kostet (Rechnung zu Full HD), Muybridges Pferd von 1878, Bilder über eine Pipe von ffmpeg lesen und schreiben, das Pferd als Film, die vier Ausschnitte aus Spring auf der Seite und in VS Code, <code>held</code> selbst (weniger Bilder pro Sekunde, 24 im Kino seit dem Tonfilm), <code>interlace</code> selbst (Halbbilder, die Vorhersage zum Combing, die größere Datei).</li>
<li><b>2. Doppelstunde:</b> Farbe sparen: Farbfernsehen als Ursprung, <code>to_ycbcr</code> selbst, die drei Ebenen eines Bildes mit der Vorhersage zur roten Jacke, Chroma Subsampling mit Zeichnung (4:4:4, 4:2:2, 4:2:0), die Bytes eines Bildes in 4:2:0, <code>subsample</code> selbst, gröbere Farbe gegen gröbere Helligkeit im Ausschnitt, warum ein Bild aus einer MP4-Datei schon 4:2:0 ist.</li>
<li><b>Weitere Doppelstunden</b> folgen: Blöcke und Vorhersage im Bild, Unterschiede von Bild zu Bild, Bewegungsschätzung, I-, P- und B-Frames, Artefakte und Filter, Bitrate, Geschichte und heute.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/video</code>: <code>video.py</code> (Lesen und Schreiben über ffmpeg), <code>pferd.py</code> (Muybridges Pferd), <code>frames.py</code> (<code>held</code>, <code>interlace</code>; schreibt <code>schwenk-6.mp4</code>, <code>aufwaerts-halbbilder.mp4</code> und <code>combing.png</code>), <code>chroma.py</code> (<code>to_ycbcr</code>, <code>subsample</code>; schreibt die Ebenen <code>y.png</code>, <code>cb.png</code>, <code>cr.png</code> und drei Ausschnitte <code>detail*.png</code>).</li>
<li>Die Ausschnitte laden die Lernenden mit <code>wget</code> von der Website (<code>/video/</code>): vier Stücke von je vier Sekunden aus »Spring« (Blender Foundation, CC BY 4.0), 960 × 400 Pixel, 24 Bilder pro Sekunde, gemacht von <code>clips/make.py</code>; dazu Muybridges Tafel (gemeinfrei) und <code>hang.png</code>, ein Bild aus dem Film, das nie durch H.264 ging und darum noch fast seine ganze Farbe hat.</li>
<li>Jeder Stand liegt unter <code>src/content/video-compression/stufen</code>.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Zwei Aufgaben rufen die Funktionen der Lernenden auf (Prüfart <code>python</code>): <code>held</code> (kleine Folgen, auch eine, die nicht aufgeht, und echte Bilder) und <code>interlace</code> (welche Zeilen aus welchem Bild kommen, das Array bleibt unverändert).</li>
<li>Zwei weitere Aufgaben rufen Funktionen auf: <code>to_ycbcr</code> (Grau, ein farbiges Pixel, auch als Bytes, die Form eines Arrays mit vier Achsen) und <code>subsample</code> (Mittelwerte bei <code>n</code> = 2 und 3, die Form).</li>
<li>Zwei Rechnungen (eine Sekunde Full HD in Mbit/s, ein Bild in 4:2:0 in Bytes) und zwei Vorhersagen (das Combing, die rote Jacke in Cb und Cr); was eine Vorhersage verrät, erscheint erst nach der Antwort.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>Die Lernenden schreiben <code>held</code>, <code>interlace</code>, <code>to_ycbcr</code> und <code>subsample</code> selbst, mit Tipps in Stufen; <code>read_frames</code>, <code>write_video</code>, das Programm zum Pferd, <code>from_ycbcr</code> und <code>upsample</code> stehen auf der Seite und werden erklärt.</td></tr>
</table>

#### Was schiefgeht

- **`ValueError: cannot reshape array`.** Die Datei hat eine andere Größe als 960 × 400; `WIDTH` und `HEIGHT` in `video.py` gelten für die Ausschnitte dieser Seite.
- **`ValueError: assignment destination is read-only`.** Das Array aus `read_frames` kommt direkt aus den Bytes von ffmpeg und lässt sich nicht ändern; mit `.copy()` entsteht eines, das sich ändern lässt.
- **VS Code spielt das Video nicht ab.** Die Vorschau spielt, was der Browser kann; H.264 in MP4 können alle verbreiteten Browser.
- **`ValueError: cannot reshape array` in `subsample`.** Höhe und Breite müssen Vielfache von `n` sein; bei 960 × 400 geht das für 2, 4 und 8, nicht für 3.
- **Die Ausschnitte mit gröberer Farbe und das Original sehen gleich aus.** Wer statt `hang.png` ein Bild aus `hang.mp4` nimmt, mittelt eine Farbe, die H.264 schon gemittelt hat; das klärt der Abschnitt »Schon gespart«.
