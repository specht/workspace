Lernende schreiben in Python mit numpy WAV-Dateien Byte für Byte, erzeugen Töne als Zahlen und hören, was Abtastrate, Bits und Clipping mit einem Klang machen. Die Seite wächst zu einem Weg von der unkomprimierten Datei über Frequenzen und das Gehör bis zu MP3 und Opus; bis dahin steht sie nur in der Entwicklung im Katalog.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen und Listen in Python; <a href='/huffman'>Komprimieren mit Huffman</a> für das spätere Kapitel über Bits. numpy wird auf der Seite erklärt, so weit sie es braucht (Arrays, mit denen man auf einmal rechnet, <code>np.arange</code>, <code>np.sin</code>, <code>np.clip</code>). Aus der Mathematik der Sinus; Hexadezimalzahlen helfen, das Handbuch <a href='/reference/bits-und-bytes'>Bits und Bytes</a> erklärt sie.</td></tr>
<tr><th>Sprache</th><td>Nur Python: numpy ist für Signalverarbeitung das, was man benutzt, und ist mit ffmpeg und Pillow im Workspace-Image.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Abtastrate und Bits im Widget hören (Musik bei 8.000 Hz, Rauschen bei wenigen Bits), PCM und die CD mit der Rechnung zum Lied, eine WAV-Datei Byte für Byte, <code>wav_header</code> selbst, <code>write_wav</code> und <code>read_wav</code>, <code>tone</code> selbst, die eigene Datei im Hexdump mit der auf der Seite vergleichen, die Vorhersage zum Akkord und Clipping.</li>
<li><b>Weitere Doppelstunden</b> folgen mit den nächsten Kapiteln: μ-law und Vorhersage, Frequenzen und FFT, das Gehör, MDCT, Quantisieren nach Gehör, echte MP3-Dateien, Opus und die Codecs von heute.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/audio</code>: <code>wav.py</code> (Header, Schreiben, Lesen) und <code>tones.py</code> (Töne, das Programm, das <code>kurz.wav</code>, <code>a.wav</code> und <code>akkord.wav</code> schreibt).</li>
<li>Die Klänge laden die Lernenden mit <code>wget</code> von der Website (<code>/audio/</code>): <code>stimme.wav</code> (Thorsten, eine freie Stimme für Sprachausgabe, CC0) und <code>musik.wav</code> (ausgerechnet, kein Ausschnitt eines echten Stücks); beide macht <code>sounds/make.py</code>.</li>
<li>Jeder Stand liegt unter <code>src/content/audio-compression/stufen</code>: <code>wav.py</code>, <code>tones-1.py</code> (der Ton), <code>tones-2.py</code> (der Akkord, der clippt), <code>tones-3.py</code> (der Akkord durch 3).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Zwei Aufgaben rufen die Funktionen der Lernenden auf (Prüfart <code>python</code>): <code>wav_header</code> (die Bytes jedes Chunks, auch bei 44.100 Hz) und <code>tone</code> (Länge, Zeit durch die Abtastrate geteilt, halb so laut).</li>
<li>Eine Rechnung (wie viele MB ein Lied auf der CD braucht) und eine Vorhersage (der Akkord aus drei Tönen); was die Vorhersage verrät, erscheint erst nach der Antwort.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>Die Lernenden schreiben <code>wav_header</code> aus den Tabellen der Seite und <code>tone</code> aus der Formel selbst, mit Tipps in Stufen. <code>write_wav</code> und <code>read_wav</code> stehen auf der Seite und werden Zeile für Zeile erklärt.</td></tr>
</table>

#### Was schiefgeht

- **`ModuleNotFoundError: No module named 'numpy'`.** Der Workspace läuft noch mit einem älteren Image ohne numpy; bis zum Update hilft `pip install numpy`.
- **`wave.Error: file does not start with RIFF id`.** Der Header ist kaputt, meist eine Zahl im Big Endian (`>` statt `<`) oder ein Feld mit der falschen Länge; `hexdump -C kurz.wav` neben die Datei auf der Seite legen.
- **Die Datei ist still.** In `tone` fehlt `/ rate`: Dann ist jede Zeit eine ganze Zahl, und der Sinus einer ganzen Zahl von Schwingungen ist immer 0.
- **VS Code spielt die Datei nicht ab.** Sie ist zu kurz (`kurz.wav` dauert eine Millisekunde) oder der Header stimmt nicht; `a.wav` muss gehen.
