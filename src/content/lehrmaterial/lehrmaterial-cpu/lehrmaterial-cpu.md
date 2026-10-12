<div class='meta'>
image: cpu-sandbox.webp
</div>

# Rechnerarchitektur mit der MiniCPU

<p class='abstract'>
Die CPU Sandbox macht Register, Flags, Speicher, Befehlsschritte und
Ein-/Ausgabegeräte einer kleinen 8-Bit-CPU sichtbar und lässt Lernende
Programme in Assembler und C entwickeln.
</p>

## CPU Sandbox

Drei Ausbaustufen der MiniCPU ermöglichen eine schrittweise Einführung in
die Prozessorarchitektur. Der Simulator zeigt die Ausführung einzelner Befehle
und auf Wunsch die internen Phasen. Vorgefertigte Aufgaben enthalten
Testfälle; Beispielprogramme demonstrieren unter anderem Grafikausgabe.

<!-- tutorial-screenshot
terminal-open
terminal-run: cd ~ && git clone -q https://github.com/specht/cpu-sandbox.git && cd cpu-sandbox && git checkout -q 730060800243aaa5fa1622b6b94567c9857fa1a0
terminal-wait-for-prompt
terminal-run: cd ~/cpu-sandbox && (nohup ./run --port 18765 > /tmp/cpu-sandbox.log 2>&1 < /dev/null &)
terminal-wait-for-prompt
preview-port: 18765
tab: preview
wait-for-text: Examples
viewport: 1600x900
-->
<img class='full' src='cpu-sandbox.webp' alt='CPU Sandbox mit Quelltext, Registeranzeige und Speicher.'>

## Einsatz im Unterricht

Beginne mit wenigen Maschinenbefehlen zum Laden, Addieren und Speichern. Bei
der schrittweisen Ausführung können die Lernenden verfolgen, wie sich Register
und Flags verändern. Danach lassen sich Schleifen, bedingte Sprünge und
Unterprogramme einführen. Der spätere Vergleich zwischen Assembler und C
macht den Zusammenhang zwischen Hochsprache und Maschinencode greifbar.

Für Klassenprogramme kann zusätzlich ein Git-Repository in `programs`
verwendet werden. Die Projektbeschreibung enthält Aufgaben, Beispiele
und weiterführende Unterrichtshinweise.

[Quelltext, Installation und Anleitung](https://github.com/specht/cpu-sandbox)
