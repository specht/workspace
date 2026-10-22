<div class='meta'>
image: interactive-3.webp:0:50
kind: labor
language: Python
template: https://github.com/specht/digits-experiment.git
template_name: des Digits-Experiments
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Trainiere ein neuronales Netzwerk

<p class='abstract'>
In diesem Experiment lernst du, wie du ein neuronales Netzwerk trainierst, das deine handgeschriebenen Ziffern erkennt. Dazu verwendest du TensorFlow und Keras, zwei Bibliotheken für maschinelles Lernen in Python.
</p>

In diesem Artikel bekommst du eine Schritt-für-Schritt-Anleitung. Lies sie genau und befolge einen Schritt nach dem anderen.

## Vorbereitung

### Repository klonen

Für diese Anleitung brauchst du die Vorlage des Experiments:

_include(workspace#repository-klonen)

Du siehst nun auf der linken Seite ein paar Verzeichnisse und Dateien, die wir für dieses Experiment verwenden werden.

<img class='full' src='lets-start.webp' alt='Der geöffnete Ordner digits-experiment im Explorer, mit den Ordnern 01-pages bis 05-model, den beiden Notebooks, bootstrap.sh, index.html, index.js, requirements.txt und worker.js.'>

### Python-Umgebung einrichten

Öffne nun ein Terminal, indem du z. B. <kbd>Strg</kbd><kbd>J</kbd> drückst:

<img class='full' src='got-terminal.webp' alt='Das offene Terminal im Ordner ~/digits-experiment.'>

Starte nun das Skript `bootstrap.sh`, indem du den folgenden Befehl eingibst:

```bash
./bootstrap.sh
```

<div class='hint'>
Du musst nicht den vollständigen Dateinamen schreiben. Schreib einfach <code>./bo</code> und drücke <kbd>Tab</kbd>, um den Dateinamen automatisch zu <code>./bootstrap.sh</code> vervollständigen zu lassen. Drücke <kbd>Enter</kbd>, um das Skript auszuführen.
</div>

<img class='r' src='noticed-new-environment.webp' style='width: 25em;' alt='Eine Meldung von Python rechts unten, »We noticed a new environment has been created. Do you want to select it for the workspace folder?«, mit den Knöpfen Yes, No und Don&#39;t show again.'>

Dieses Skript richtet eine Python-Umgebung ein und installiert die nötigen Pakete für das KI-Experiment. Dabei werden ca. 500 MB heruntergeladen und dein Verzeichnis ist danach ca. 2 GB groß (das ist aber in der Hackschule kein Problem). Wenn du den grünen Haken siehst, hat alles geklappt und du kannst das Terminal wieder schließen:

Wenn du zwischendurch diesen Hinweis siehst (s. oben rechts), kannst du mit »Yes« antworten, da wir die frisch eingerichtete Python-Umgebung in diesem Verzeichnis verwenden wollen.

<img class='full' src='bootstrap-finished.webp' alt='Das Terminal am Ende von bootstrap.sh, eine lange Liste installierter Pakete von MarkupSafe bis wrapt und darunter »Pakete installiert«.'>

## Trainingsmaterial herstellen

Du hast nun zwei Möglichkeiten: entweder verwendest du fertige Scans mit handgeschriebenen Ziffern, oder du schreibst selbst Ziffern &ndash; das ist natürlich viel spannender, weil du in diesem Fall dein neuronales Netzwerk mit deinen eigenen Daten trainieren wirst. In jedem Fall brauchst du mindestens zwei Seiten mit handgeschriebenen Ziffern als Trainingsmaterial.

### Variante 1: Fertige Scans verwenden

Du kannst diese Seiten im PNG-Format hier herunterladen und per Drag & Drop in deinem Workspace in das Verzeichnis `01-pages` ziehen:

<a href='/cache/blatt1.png' target='_blank'><img src='blatt1.png' data-noconvert='1' style='height: 16em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 1 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt2.png' target='_blank'><img src='blatt2.png' data-noconvert='1' style='height: 16em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 2 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt3.png' target='_blank'><img src='blatt3.png' data-noconvert='1' style='height: 16em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 3 mit Feldern für handgeschriebene Ziffern.'></a>

### Variante 2: Ziffern selbst schreiben

Drucke <a href='/docs/grid-digits.pdf' target='_blank'>diese PDF-Vorlage</a> ohne zusätzliche Seitenränder zweimal aus und trage in jedes Feld die entsprechenden Ziffern ein. Scanne die Blätter anschließend mit einer Auflösung von 300 dpi, speichere sie im PNG-Format und ziehe sie per Drag & Drop in deinen Workspace in das Verzeichnis `01-pages`.

<div class='hint'>
Wenn du deinen eigenen Blätter beschreibst, kannst du statt Ziffern auch Symbole verwenden. Dadurch ist dein neuronales Netzwerk später in der Lage, diese Symbole zu erkennen.
</div>

## Trainingsmaterial vorbereiten

Stelle sicher, dass du mindestens zwei Bilddateien im Verzeichnis `01-pages` hast. Du kannst die Dateien öffnen, um zu überprüfen, ob sie korrekt sind:

<img class='full' src='pages-ready.webp' alt='Im Ordner 01-pages liegen blatt1.png, blatt2.png und blatt3.png; blatt1.png ist offen, ein Blatt mit einem Raster voller handgeschriebener Ziffern.'>

### Das Notebook starten

Öffne nun das erste Jupyter-Notebook `01-prepare-training-data.ipynb`, und führe den ersten Codeblock aus, indem du auf das Play-Symbol klickst.

Bevor der Code ausgeführt werden kann, musst du noch einen Python-Interpreter auswählen. Bestätige die Auswahl »Python Environments« und »★ .venv (Python 3.12.3)«:

<img class='full' src='choose-kernel.webp' alt='Oben das Feld »Select Kernel« mit der Auswahl »Python Environments…« und »Existing Jupyter Server…«.'>

<img class='full' src='python-venv.webp' alt='Das Feld »Select a Python Environment«, ganz oben mit Stern .venv (Python 3.12.3), als Recommended markiert.'>

### Helligkeit und Kontrast

Im ersten Schritt werden die Helligkeit und der Kontrast der eingescannten Seiten automatisch angepasst:

<img class='full' src='colors-adjusted.webp' alt='Die Ausgabe des Codeblocks, für jedes der drei Blätter »Farben angepasst« von 01-pages nach 02-cache.'>

Im nächsten Schritt siehst du den Effekt der Farbanpassung:

<img class='full' src='step-2.webp' alt='Vorher-nachher-Vergleich: Graue handgeschriebene Ziffern werden kontrastreich schwarz auf weiß dargestellt.'>

### Rahmen erkennen

Wenn die Bilder korrekt angezeigt werden, führe den nächsten Codeblock aus. In diesem Schritt werden die gedruckten Rahmen in jeder Bilddatei gesucht:

<img class='full' src='detect-frame.webp' alt='Die Ausgabe des Codeblocks, für jedes der drei Blätter in 02-cache »Rahmen erkannt«.'>

Der nächste Schritt zeigt dir, wo die Rahmen gefunden wurden:

<img class='full' src='frames.webp' alt='Rote Rahmen markieren die automatisch erkannten Bereiche der drei ausgefüllten Trainingsblätter.'>

### Ziffern ausschneiden

Wenn das Ergebnis zufriedenstellend aussieht, führe den nächsten Codeblock aus. In diesem Schritt werden die einzelnen Ziffern aus den Bildern ausgeschnitten:

<img class='full' src='extracting-digits.webp' alt='Die Ausgabe »Extrahiere Ziffern…« mit einem Fortschrittsbalken pro Blatt und je um die 600 extrahierten Ziffern.'>

Im nächsten Schritt werden 10 Bilddateien generiert, in denen die einzelnen Ziffern gesammelt werden:

<img class='full' src='collect-digits.webp' alt='Die Ausgabe für die Symbole 0 bis 9, je 176 bis 180 Beispiele, zusammengefasst in 04-sheets/0.png bis 9.png.'>

Du kannst dir die Bilder anschauen, indem du auf das Verzeichnis `04-sheets` klickst:

<img class='full' src='4.webp' alt='Ein Raster enthält viele ausgeschnittene Varianten der handgeschriebenen Ziffer 4.'>

Wenn alle Schritte erfolgreich durchgelaufen sind, können wir mit dem Training des Modells beginnen.

## Modell trainieren

Öffne nun das zweite Jupyter-Notebook `02-train-model.ipynb`, indem du darauf klickst. Es enthält den Code, um unser neuronales Netzwerk zu trainieren, das die handgeschriebenen Ziffern erkennen soll. Im ersten Schritt wird die Menge aller Trainingsdaten in drei Teile aufgeteilt: 80% für das Training, 10% für die Validierung und 10% für den Test.

Bevor du den ersten Codeblock starten kannst, musst du wieder den Kernel in ».venv« auswählen:

<img class='full' src='choose-kernel-again.webp' alt='Das Feld »Select kernel for 02-train-model.ipynb« mit .venv (Python 3.12.3) als erstem Eintrag.'>

<img class='full' src='train-groups.webp' alt='Die Ausgabe »Trainingsdaten: 1434, Validierungsdaten: 179, Testdaten: 180«.'>

### Die Architektur des Modells

Im nächsten Schritt wird die Architektur des Modells festgelegt. Es handelt sich um ein einfaches neuronales Netzwerk mit vier Schichten:

- die erste Schicht ist ein »Flatten Layer«, das die 28x28 Pixel großen Bilder in einen Vektor mit 784 Elementen umwandelt.
- die zweite und dritte Schicht sind jeweils ein »Dense Layer« mit 100 Neuronen und der Aktivierungsfunktion »ReLU«.
- die vierte Schicht ist ebenfalls ein »Dense Layer« mit 10 Neuronen, die die jeweiligen Ziffern repräsentieren, jedoch mit der Aktivierungsfunktion »Softmax«.

<img class='full' src='architecture.webp' alt='Die Tabelle der Schichten, flatten mit 784 Ausgaben, dann dense mit 100, dense_1 mit 100 und dense_2 mit 10 Neuronen, darunter Total params 89.610.'>

Dieses Modell verfügt über eine Gesamtzahl von 89.610 Parametern, die während des Trainings optimiert werden.

### Training

Wir sind jetzt bereit, das Modell zu trainieren. Klicke auf das Play-Symbol, um den Codeblock auszuführen. Nach 30 Trainingsepochen ist das Training beendet.

<img class='full' src='training-finished.webp' alt='Die Ausgabe des Trainings, Epoche für Epoche mit accuracy, loss und val_accuracy, bis Epoch 30/30 und »Modell trainiert«.'>

<div class='hint'>
Du kannst hier gut beobachten, wie der »Loss« immer geringer wird – am Anfang ist er noch relativ groß und nach 30 Trainingsrunden schon bei 10<sup>-4</sup>.
</div>

Im nächsten Schritt wird das Modell im Verzeichnis `05-model` gespeichert:

<img class='full' src='saved-model.webp' alt='Die letzte Zeile der Ausgabe, »Modell gespeichert«.'>

### Das Modell testen

Jetzt ermitteln wir, wie gut das Modell ist. Dazu verwenden wir die Testdaten, die wir zuvor beiseite gelegt haben. Die wichtigen Kennwerte sind der Verlust (loss) und die Genauigkeit (accuracy):

- der Verlust sollte möglichst klein sein, idealerweise nahe bei 0.
- die Genauigkeit sollte möglichst groß sein, idealerweise nahe bei 100%.

<img class='full' src='evaluate.webp' alt='Die Ausgabe »Das trainierte Modell erreicht einen Loss von 0.0384 und eine Accuracy von 98.33% auf dem Testdatensatz.«'>

Um den vorherigen Schritt zu visualisieren, können wir uns nun ein paar Testbilder anzeigen lassen und die Vorhersagen des Modells überprüfen:

<img class='full' src='test-bitmaps.webp' alt='Ein Raster vergleicht handgeschriebene Ziffern mit den Vorhersagen des ersten Modells; falsche Ergebnisse sind rot markiert.'>

Die roten Ziffern markieren die falschen Vorhersagen. Das Modell ist noch nicht perfekt, aber es ist bereits in der Lage, handgeschriebene Ziffern zu erkennen.

## Modell ausprobieren

Im nächsten Schritt wollen wir unser Modell interaktiv im Webbrowser testen. Dazu müssen wir das gespeicherte Modell zunächst in ein anderes Format namens [ONNX](https://de.wikipedia.org/wiki/ONNX) konvertieren, damit wir es im Webbrowser laufen lassen können. Führe dazu den nächsten Codeblock aus:

<img class='full' src='convert-model.webp' alt='Die Ausgabe »Modell konvertiert (4 hidden layers)«.'>

<img src='../reference/workspace/go-live.webp' class='r' style='width: 21em;' alt='Unten rechts in der Statusleiste der Knopf »Go Live« mit dem Mauszeiger darauf und dem Hinweis »Click to run live server«.'>

Wenn das Modell erfolgreich konvertiert wurde, können wir es im Webbrowser testen. Klicke dazu rechts unten auf den Button »Go Live«.
Du kannst nun links oben mit der Maus Ziffern schreiben und zuschauen, wie sich die Aktivierungen der Neuronen im Modell verändern. Du siehst die einzelnen Schichten und die Aktivierungen der Neuronen. Ganz rechts siehst du die Vorhersage des Modells.

Versuche, die Grenze zwischen zwei Ziffern zu finden, um zu sehen, wie das Modell reagiert. Mit der rechten Maustaste kannst du deine Zeichnung wieder wegradieren.

<div style='clear: both;'></div>

<img class='full' src='interactive-1.webp' alt='Das dichte neuronale Netz erkennt eine gezeichnete 2 und visualisiert die Aktivierungen seiner Schichten.'>

## Modell verbessern

Um unser Modell zu verbessern, können wir ein [Convolutional Neural Network](https://de.wikipedia.org/wiki/Convolutional_Neural_Network) (CNN) verwenden. Dabei werden die Pixel nicht mehr unabhängig voneinander betrachtet, sondern unter Berücksichtigung ihrer Lage zueinander.


<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Backpropagation Applied to Handwritten Zip Code Recognition</strong>, Yann LeCun und andere, Neural Computation, 1989. Der Aufsatz, in dem ein Netz mit Faltungsschichten die Filter für Bilder handgeschriebener Ziffern selbst lernt, statt dass jemand sie von Hand festlegt: <a href='http://yann.lecun.com/exdb/publis/pdf/lecun-89e.pdf'>als PDF auf LeCuns Seite</a>.</p>
</div>

### Die neue Architektur

Um ein CNN zu verwenden, scrolle nach oben zum Abschnitt »Architektur des Modells« und setze die Variable `use_model` auf den Wert 2. Führe dann diesen und alle folgenden Codeblöcke erneut aus.

<img src='model-2.webp' class='full' alt='Der Abschnitt »Architektur des Modells« im Notebook, im Codeblock steht use_model = 2.'>

Das neue Modell hat eine etwas andere Architektur:

- die erste Schicht ist ein »Conv2D Layer«, das das 28x28 Pixel großen Eingabebild mit 32 3x3-Filtern in 32 26x26 Pixel große Bilder umwandelt.
- die zweite Schicht ist ein »MaxPooling2D Layer«, das die Größe der Bilder halbiert (13x13).
- die dritte Schicht ist ein weiteres »Conv2D Layer«, das die 13x13 großen Bilder mit 64 3x3-Filtern in 64 11x11 Pixel große Bilder umwandelt.
- die vierte Schicht ist ein weiteres »MaxPooling2D Layer«, das die Größe der Bilder halbiert (5x5).
- die fünfte Schicht ist ein »Flatten Layer«, das die 5x5 Pixel großen Bilder in einen Vektor mit 1600 Elementen umwandelt.
- die sechste Schicht ist ein »Dense Layer« mit 128 Neuronen und der Aktivierungsfunktion »ReLU«.
- die siebte Schicht ist ein weiteres »Dense Layer« mit 10 Neuronen, die die jeweiligen Ziffern repräsentieren, jedoch mit der Aktivierungsfunktion »Softmax«.

Das verbesserte Modell verfügt über eine Gesamtzahl von 225.034 Parametern, also ca. 2,5 mal so viele Parameter wie im vorherigen Modell:

<img class='full' src='new-model-parameters.webp' alt='Die Tabelle der Schichten des neuen Modells, zwei Conv2D- und zwei MaxPooling2D-Schichten, Flatten und zwei Dense-Schichten, Total params 225.034.'>

### Training und Test

Da das neue Modell mehr Parameter hat, dauert das Training auch länger. Nach 30 Epochen ist das Training beendet:

<img class='full' src='second-training-finished.webp' alt='Die Ausgabe des zweiten Trainings bis Epoch 30/30 und »Modell trainiert«.'>

Im nächsten Schritt wird das Modell wieder gespeichert (natürlich mit mehr Layern als vorher):

<img class='full' src='saved-model-2.webp' alt='Das Ende der Ausgabe beim Speichern, eine Liste von TensorSpec-Zeilen und »Modell gespeichert«.'>

Bei der Evaluation des neuen Modells siehst du, dass Loss und Accuracy besser sein sollten als beim ersten Modell:

<img class='full' src='evaluate-again.webp' alt='Die Ausgabe »Das trainierte Modell erreicht einen Loss von 0.0009 und eine Accuracy von 100.00% auf dem Testdatensatz.«'>

Die Testbilder sehen jetzt so aus:

<img class='full' src='test-bitmaps-2.webp' alt='Das zweite Modell erkennt fast alle handgeschriebenen Ziffern im Testraster korrekt.'>

### Im Browser ausprobieren

Um das Modell im Browser zu testen, musst du es erst wieder konvertieren:

<img class='full' src='convert-again.webp' alt='Die Ausgabe »Modell konvertiert (7 hidden layers)«.'>

Wenn du das Modell im Webbrowser testest, siehst du, wie die Pixel nicht mehr im ersten Schritt voneinander getrennt werden, sondern über mehrere Schichten hinweg im Quadrat angeordnet gemeinsam behandelt werden:

<img class='full' src='interactive-2.webp' alt='Das faltende neuronale Netz erkennt eine gezeichnete 4 und zeigt die Aktivierungen seiner Filter und Schichten.'>

### Weiter experimentieren

Falls du ein noch besseres Modell haben möchtest, kannst du die Anzahl der Epochen erhöhen oder die Architektur des Modells weiter anpassen. Du kannst auch die Anzahl der Trainingsdaten erhöhen, indem du mehr Blätter mit handgeschriebenen Ziffern verwendest. Hier findest du weitere Blätter, die du verwenden kannst:

<div style='text-align: left; margin-bottom: 1em;'>
<a href='/cache/blatt4.png' target='_blank'><img src='blatt4.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 4 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt5.png' target='_blank'><img src='blatt5.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 5 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt6.png' target='_blank'><img src='blatt6.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 6 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt7.png' target='_blank'><img src='blatt7.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 7 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt8.png' target='_blank'><img src='blatt8.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 8 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt9.png' target='_blank'><img src='blatt9.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 9 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt10.png' target='_blank'><img src='blatt10.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 10 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt11.png' target='_blank'><img src='blatt11.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 11 mit Feldern für handgeschriebene Ziffern.'></a>
<a href='/cache/blatt12.png' target='_blank'><img src='blatt12.png' data-noconvert='1' style='height: 10em; box-shadow: 0 0 2px rgba(0,0,0,0.5);' alt='Trainingsblatt 12 mit Feldern für handgeschriebene Ziffern.'></a>
</div>

In den bisherigen Beispielen war es so, dass wir nur weiß und blau gesehen haben, wobei blau für »positiv« steht. Wenn du statt `ReLU` eine Aktivierungsfunktion wählst, die auch negative Werte zurückgeben kann (wie z. B. `tanh`), dann wirst du auch orangefarbene Neuronen bekommen (für negative Werte):

<img class='full' src='interactive-3.webp' alt='Das Netz erkennt eine gezeichnete 7; blaue und orange Verbindungen kennzeichnen positive und negative Gewichte.'>


## Zusammenfassung

In diesem Experiment hast du gelernt, wie du ein neuronales Netzwerk trainierst, das handgeschriebene Ziffern erkennt. Du hast gesehen, wie du die Trainingsdaten vorbereitest, das Modell trainierst und es im Webbrowser testest. Du hast auch gesehen, wie du das Modell verbessern kannst, indem du ein Convolutional Neural Network verwendest.