Lernende programmieren Gaussian Splatting in Python mit numpy: erst ein Gemälde aus 500 Splats, die durch Gradientenabstieg ihre Lage, Form und Farbe lernen, dann einen Renderer, der eine Szene aus 49.817 Splats im Raum mit einer Kamera abbildet, nach der Tiefe sortiert und von vorne nach hinten mischt. Am Ende drehen sie fertige Szenen in einem Viewer auf der Seite, der dasselbe auf der Grafikkarte tut.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Schleifen und Listen in Python (die Katas bis <a href='/kata-lists'>Listen</a>); <a href='/webgl'>3D-Grafik mit WebGL</a> davor, denn Kamera, Perspektive, Matrizen und der Z-Buffer kommen dort vor und hier wieder; aus der Mathematik die e-Funktion, Sinus und Kosinus, Ableitungen und Matrizen, die man multipliziert. numpy ist neu und wird auf der Seite erklärt, so weit sie es braucht (<code>np.mgrid</code>, Rechnen mit ganzen Arrays, <code>[:, :, None]</code>, <code>@</code>, <code>np.argsort</code>).</td></tr>
<tr><th>Sprache</th><td>Nur Python: Die Rechnungen auf ganzen Bildern brauchen numpy, in Ruby gäbe es nichts Vergleichbares, das in einer Stunde schnell genug ist. numpy und Pillow installieren die Lernenden selbst mit <code>pip install numpy pillow</code>; sie liegen dann in ihrem Workspace und bleiben nach einem Neustart.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten, die Glockenkurve, ein Splat als Bild, gestreckte und gedrehte Splats, Splats addieren, der mittlere quadratische Fehler, 200 zufällige Splats.</li>
<li><b>2. Doppelstunde:</b> Gradientenabstieg mit einem Regler, die Farben von 500 festen Splats lernen (eigene Funktion <code>step</code>), die Lernrate, die zu groß ist, dann alles lernen mit Adam, der Film vom Lernen.</li>
<li><b>3. Doppelstunde:</b> ein Splat im Raum, die Kovarianzmatrix, die Kamera, die Ellipse auf dem Bildschirm (ausgearbeitet), Mischen von vorne nach hinten, der Sessel mit und ohne Sortieren, die Szenen im Viewer mit der Gasse, durch die man gehen kann, und drei Fragen an den Viewer: scharfe Kanten aus weichen Splats, Sortieren in jedem Bild, warum kein Z-Buffer.</li>
<li><b>Für Schnelle</b> (auf der Seite als »Extra« markiert): eine eigene Szene mit Brush und COLMAP am eigenen Rechner mit Grafikkarte, oder mit den Bildern der DX.GL-Datensätze; dazu die Kästen »Ausprobieren« (andere Szenen, eine Kamera, die kreist, als GIF).</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/splatting</code>: <code>blobs.py</code> (alle Funktionen der 2D-Hälfte, mit einem Block <code>if __name__ == '__main__':</code> zum Ausprobieren), <code>train.py</code> (das Programm, das lernt, holt die Funktionen aus <code>blobs.py</code>) und <code>render3d.py</code>.</li>
<li>Heruntergeladen mit <code>wget</code> aus <a href='https://github.com/specht/workspace-files/tree/main/gaussian-splatting'><code>specht/workspace-files</code></a>: <code>pearl-earring.png</code> (Vermeer, gemeinfrei, 128 Pixel breit) und <code>modern-arm-chair.ply</code>; dort liegen fünf weitere Szenen und die Gasse.</li>
<li>Jeder Stand liegt unter <a href='https://github.com/specht/workspace/tree/main/src/content/gaussian-splatting/stufen'><code>src/content/gaussian-splatting/stufen</code></a>: <code>blobs-1.py</code> bis <code>blobs-4.py</code>, <code>train-1.py</code> (nur Farben) und <code>train-2.py</code> (alles, mit Adam), <code>render3d-1.py</code> (die Funktionen) und <code>render3d-2.py</code> (der Renderer); <code>vergleich-ohne-sortieren.py</code> ist nur für das Bild nach der Vorhersage.</li>
<li>Die Gasse »Artillery Passage« (David Fletcher, CC BY 4.0, aus 1.575 Fotos) macht <code>werkzeug/prepare_street.py</code> aus seinen 5 Millionen Splats eine Datei von 80 MB im kompakten Format des Viewers mit 16 Bytes je Splat, das Klassenzimmer (Hite Spive, CC BY 4.0) eine von 19 MB. Beide sind zu groß für das Repository: Der Server holt sie beim Start aus <code>specht/workspace-files</code> nach <code>/dl</code>, und der Viewer lädt eine erst, wenn jemand sie auswählt. Auf schwachen Rechnern ruckelt die Gasse; dann hilft »Automatisch flüssig« oder »Anzahl« weiter links.</li>
<li>Die Szenen der Seite macht <code>werkzeug/prepare_scenes.py</code> aus den Splats der DX.GL-Datensätze (CC0, trainiert aus Modellen von Poly Haven): eine Vierteldrehung, damit y nach oben zeigt, die Mitte in den Ursprung, nur die Farbe ohne die Abhängigkeit von der Blickrichtung.</li>
</ul></td></tr>
<tr><th>Laufzeiten</th><td>Gemessen mit einem Prozessorkern, wie ihn ein Workspace bekommt, wenn eine ganze Klasse rechnet: <code>train.py</code> mit nur den Farben etwa 6 Sekunden, mit allem etwa 13 Sekunden für 300 Schritte, dazu etwa 1 Sekunde für die beiden großen Bilder von <code>paint</code>; <code>render3d.py</code> etwa 5 Sekunden für ein Bild des Sessels mit 640 × 480 Pixeln (die Zeit hängt kaum an der Größe des Bildes, sondern an der Zahl der Splats).</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Sieben Aufgaben rufen die Funktionen der Lernenden auf, mit Zahlen und mit numpy-Arrays (Prüfart <code>python</code>): <code>blob</code>, <code>weight</code>, <code>error</code> und <code>step</code> in <code>blobs.py</code>, <code>covariance</code>, <code>to_screen</code> und <code>blend</code> in <code>render3d.py</code>. Ein Aufruf mit Arrays fällt auf, wenn jemand <code>math.exp</code> statt <code>np.exp</code> nimmt.</li>
<li>Fünf Vorhersagen vor dem Ausprobieren oder Erklären (überlappende Splats, die doppelte Lernrate, wie viel das Wandern bringt, ein Renderer ohne Sortieren, warum der Viewer keinen Z-Buffer nimmt); was sie verrät, erscheint erst nach der Antwort.</li>
<li>Eine Rechnung von Hand (der erste Schritt Gradientenabstieg) und zum Schluss eine Erklärung in eigenen Worten, warum die Reihenfolge in 2D egal ist und in 3D nicht (Klassenboard).</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>An sieben Stellen schreiben die Lernenden die Funktion zuerst selbst, nachdem die Seite die Formel erklärt hat, mit Tipps in Stufen; der Code der Seite ist die letzte Stufe, und die Aufgabe darunter prüft die eigene Fassung. Die Gradienten von Lage, Breiten und Winkel, Adam und die Projektion der Ellipse sind ausgearbeitete Beispiele, die die Seite erklärt, aber nicht schreiben lässt.</td></tr>
</table>

#### Was schiefgeht

- **`ModuleNotFoundError: No module named 'numpy'`.** `pip install numpy pillow` fehlt oder lief in einem anderen Workspace.
- **`FileNotFoundError` für `pearl-earring.png` oder `modern-arm-chair.ply`.** Das Programm läuft in einem anderen Ordner; vorher `cd ~/splatting`.
- **`blobs.py` malt beim Start von `train.py` ein Bild.** Der Teil zum Ausprobieren steht nicht unter `if __name__ == '__main__':`.
- **Der Fehler in `train.py` wächst ins Riesige.** Eine zu große Lernrate, oder `step` addiert statt abzuziehen; die Aufgabe zu `step` fängt das Zweite.
- **Der Sessel steht auf dem Kopf oder ist von hinten zu sehen.** Die Kamera hat andere Richtungen als in `look_at` auf der Seite, etwa `up` statt `down` als zweite Zeile.
- **Der Viewer bleibt grau.** Der Browser kann kein WebGL2; mit einem aktuellen Firefox, Chrome oder Safari geht es.

#### Die Aufgaben

- **Ein Splat** und **Gestreckt und gedreht:** die Rückmeldungen erkennen den fehlenden Faktor 2, ein nicht quadriertes Sigma, vertauschte Vorzeichen von Sinus und `math.exp`.
- **Wie weit daneben?:** die Summe statt des Mittelwerts und den Betrag statt des Quadrats.
- **Der erste Schritt bergab:** 0,14; −0,14 ist ein Vorzeichenfehler, 0,07 der fehlende Faktor 2.
- **Die Form im Raum:** \\(S\\) nur einmal (die Breiten stehen nicht im Quadrat da) oder das transponierte \\(R\\) fehlt (die Matrix ist nicht symmetrisch).
- **Auf den Bildschirm:** nicht durch `z` geteilt oder die Mitte des Bildes vergessen.
- **Von vorne nach hinten:** die Durchlässigkeit wird nicht kleiner, oder der Hintergrund fehlt.

#### Eigene Projekte

- **Ein eigenes Bild aus Splats** (eine Doppelstunde): Die Lernenden nehmen ein eigenes Bild oder Foto statt des Gemäldes, wählen die Zahl der Splats und zeigen, ab wie vielen es erkennbar wird. Wiederkommen Fehler, Gradientenabstieg und Adam; bewertet wird am Vorführen und an der Erklärung, was die Splats gelernt haben.
- **Splats verdichten** (ein Projekt): Wie beim echten Training teilt `train.py` Splats, deren Gradient der Lage lange groß bleibt, in zwei kleinere, und entfernt Splats, deren Farbe fast schwarz ist. Wiederkommen die Gradienten der Seite; bewertet wird am Vergleich des Fehlers mit und ohne und am Erklären des eigenen Codes.
- **Ein Film um eine Szene** (eine Doppelstunde): eine Kamera auf einer Bahn, die nicht nur kreist (näher heran, von oben), als GIF, mit einer Szene von <code>specht/workspace-files</code>. Wiederkommen Kamera, Sortieren und Mischen; bewertet wird am Film und an der Erklärung, warum sich die Reihenfolge der Splats in jedem Bild ändert.
- **Eine eigene Szene** (ein Projekt, mit eigener Grafikkarte zu Hause): Fotos eines Objekts, COLMAP und Brush wie im Extra der Seite, die Szene im Viewer vorführen und erklären, wo sie gut und wo sie schlecht geworden ist (Glanz, Durchsichtiges, zu wenige Fotos).
- **Ein Rundgang durch die Schule** (ein Projekt für die ganze Klasse, mit Rechnern mit Grafikkarte): Gruppen nehmen je einen Ort auf (Eingang, Flur, Fachraum, Schulhof; mit einer Drohne das Gebäude von außen), mit Handy, Actioncam oder Fotos, wie unter »Kamera und Vorgehen« auf der Seite. Jede Gruppe trainiert ihre Szene und führt sie im Viewer vor, mit den Stellen, an denen sie gut aussieht, und denen, an denen sie zerfällt, und erklärt, warum (zu wenige Fotos aus dieser Richtung, Glanz, Glas). Vorher mit der Schulleitung klären, was aufgenommen werden darf und wo die Szenen gezeigt werden; aufnehmen, wenn niemand im Bild ist; für eine Drohne die Regeln für das Gelände prüfen. Bewertet wird an der Szene, an der Vorführung und an der Erklärung der Schwächen.
