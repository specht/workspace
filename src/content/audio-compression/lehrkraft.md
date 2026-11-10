Lernende schreiben in Python mit numpy WAV-Dateien Byte für Byte, erzeugen Töne als Zahlen und hören, was Abtastrate, Bits und Clipping mit einem Klang machen. Danach sparen sie Bits ohne ein Modell des Gehörs: Stufen nach μ-law für leise Stellen, eine Vorhersage der nächsten Messung und Rice-Codes für die Reste, die Idee von FLAC. Die Seite wächst zu einem Weg von der unkomprimierten Datei über Frequenzen und das Gehör bis zu MP3 und Opus; bis dahin steht sie nur in der Entwicklung im Katalog.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen und Listen in Python; <a href='/huffman'>Komprimieren mit Huffman</a> für das spätere Kapitel über Bits. numpy wird auf der Seite erklärt, so weit sie es braucht (Arrays, mit denen man auf einmal rechnet, <code>np.arange</code>, <code>np.sin</code>, <code>np.clip</code>). Aus der Mathematik der Sinus; Hexadezimalzahlen helfen, das Handbuch <a href='/reference/bits-und-bytes'>Bits und Bytes</a> erklärt sie.</td></tr>
<tr><th>Sprache</th><td>Nur Python: numpy ist für Signalverarbeitung das, was man benutzt, und ist mit ffmpeg und Pillow im Workspace-Image.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Abtastrate und Bits im Widget hören (Musik bei 8.000 Hz, Rauschen bei wenigen Bits), PCM und die CD mit der Rechnung zum Lied, eine WAV-Datei Byte für Byte, <code>wav_header</code> selbst, <code>write_wav</code> und <code>read_wav</code>, <code>tone</code> selbst, die eigene Datei im Hexdump mit der auf der Seite vergleichen, die Vorhersage zum Akkord und Clipping.</li>
<li><b>2. Doppelstunde:</b> die leise Stimme im Widget mit gleichmäßigen Stufen und mit μ-law, die Formel und G.711, <code>unmulaw</code> selbst, der Signal-Rausch-Abstand in dB mit der Vorhersage zur viermal leiseren Stimme; die Gerade durch die letzten beiden Messungen, <code>residual</code> selbst mit Ausschnitten von numpy-Arrays, <code>restore</code> bringt jede Messung zurück; Zickzack und Rice-Codes, <code>rice</code> selbst, ein <code>k</code> je Block mit der Vorhersage, welche Datei mehr gewinnt; der Vergleich mit FLAC aus ffmpeg.</li>
<li><b>Weitere Doppelstunden</b> folgen mit den nächsten Kapiteln: Frequenzen und FFT, das Gehör, MDCT, Quantisieren nach Gehör, echte MP3-Dateien, Opus und die Codecs von heute.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/audio</code>: <code>wav.py</code> (Header, Schreiben, Lesen), <code>tones.py</code> (Töne, das Programm, das <code>kurz.wav</code>, <code>a.wav</code> und <code>akkord.wav</code> schreibt), <code>mulaw.py</code> (μ-law, Runden, dB; schreibt <code>leise-gleichmaessig.wav</code> und <code>leise-mulaw.wav</code>), <code>predict.py</code> (die Reste und zurück) und <code>rice.py</code> (Zickzack, Rice-Codes, die Größe mit einem <code>k</code> und mit einem je Block).</li>
<li>Die Klänge laden die Lernenden mit <code>wget</code> von der Website (<code>/audio/</code>): <code>stimme.wav</code> (Thorsten, eine freie Stimme für Sprachausgabe, CC0) und <code>musik.wav</code> (ausgerechnet, kein Ausschnitt eines echten Stücks); beide macht <code>sounds/make.py</code>.</li>
<li>Jeder Stand liegt unter <code>src/content/audio-compression/stufen</code>: <code>wav.py</code>, <code>tones-1.py</code> (der Ton), <code>tones-2.py</code> (der Akkord, der clippt), <code>tones-3.py</code> (der Akkord durch 3), <code>mulaw.py</code>, <code>predict.py</code>, <code>rice-1.py</code> (ein <code>k</code>) und <code>rice-2.py</code> (ein <code>k</code> je Block).</li>
</ul></td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Fünf Aufgaben rufen die Funktionen der Lernenden auf (Prüfart <code>python</code>): <code>wav_header</code> (die Bytes jedes Chunks, auch bei 44.100 Hz), <code>tone</code> (Länge, Zeit durch die Abtastrate geteilt, halb so laut), <code>unmulaw</code> (einzelne Werte, das Vorzeichen, ein ganzes Array hin und zurück), <code>residual</code> (eine Gerade ergibt Reste 0, die ersten beiden bleiben, <code>x</code> bleibt unverändert) und <code>rice</code> (führende Nullen im zweiten Teil, lange Codes).</li>
<li>Eine Rechnung (wie viele MB ein Lied auf der CD braucht) und drei Vorhersagen (der Akkord aus drei Tönen, die viermal leisere Stimme in dB, welche Datei ein <code>k</code> je Block kleiner macht); was eine Vorhersage verrät, erscheint erst nach der Antwort.</li>
</ul></td></tr>
<tr><th>Jetzt bist du dran</th><td>Die Lernenden schreiben <code>wav_header</code> aus den Tabellen der Seite, <code>tone</code> aus der Formel, <code>unmulaw</code> durch Umstellen der Formel von μ-law, <code>residual</code> mit Ausschnitten und <code>rice</code> aus der Beschreibung des Codes selbst, mit Tipps in Stufen. <code>write_wav</code>, <code>read_wav</code>, <code>quantise</code>, <code>snr</code>, <code>restore</code> und die Größe der Rice-Codes stehen auf der Seite und werden erklärt.</td></tr>
</table>

#### Was schiefgeht

- **`ModuleNotFoundError: No module named 'numpy'`.** Der Workspace läuft noch mit einem älteren Image ohne numpy; bis zum Update hilft `pip install numpy`.
- **`wave.Error: file does not start with RIFF id`.** Der Header ist kaputt, meist eine Zahl im Big Endian (`>` statt `<`) oder ein Feld mit der falschen Länge; `hexdump -C kurz.wav` neben die Datei auf der Seite legen.
- **Die Datei ist still.** In `tone` fehlt `/ rate`: Dann ist jede Zeit eine ganze Zahl, und der Sinus einer ganzen Zahl von Schwingungen ist immer 0.
- **VS Code spielt die Datei nicht ab.** Sie ist zu kurz (`kurz.wav` dauert eine Millisekunde) oder der Header stimmt nicht; `a.wav` muss gehen.
- **`unmulaw(-1)` ergibt fast 0.** Das Vorzeichen fehlt: Mit `(1 + MU) ** y` wird aus einem negativen `y` eine Zahl knapp unter 1, nicht eine negative. Mit `np.abs(y)` rechnen und mit `np.sign(y)` multiplizieren.
- **`TypeError: only length-1 arrays can be converted`.** `math.log` oder `math.pow` statt `np.log` und `**`: `math` rechnet nur mit einer Zahl, nicht mit einem Array.
- **`restore` gibt etwas anderes zurück als die Messungen.** `residual` hat `x` selbst verändert (`x[2:] = …` statt einer Kopie); dann sind die Messungen danach schon die Reste.
- **Rice-Codes zu kurz.** `format(rest, 'b')` lässt führende Nullen weg; der zweite Teil braucht genau `k` Stellen, `f'0{k}b'`.
