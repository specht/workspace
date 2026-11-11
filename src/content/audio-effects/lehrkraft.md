Lernende bauen in Python mit numpy die Effekte eines Tonstudios: einen Equalizer, ein Echo, Hall, einen Chorus, einen Kompressor, eine andere Tonhöhe, Autotune und einen Vocoder. Bei jedem hören sie zuerst im Widget, was seine Regler tun, sagen etwas voraus und schreiben dann die Funktion selbst. Die Seite ist im Aufbau (`docs/plan-audio-effects.md`); bisher stehen Equalizer, Echo, Chorus und Hall.

<table class='table'>
<tr><th>Voraussetzungen</th><td>»Audio komprimieren«: Samples als Zahlen, WAV-Dateien, Dezibel, die FFT.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Einrichten; der Equalizer im Widget (auch als Bandpass), die Vorhersage zu 12 dB, <code>bell</code> selbst, zwei Dateien mit mehr Bass und ohne Hi-Hat.</li>
<li><b>2. Doppelstunde:</b> das Echo im Widget, die Vorhersage zur Höhe eines Echos, <code>echo</code> selbst als Schleife über die Samples, ein Echo und ein Raum aus derselben Formel; »Let It Be« in der Fassung des Albums (Bandecho auf der Hi-Hat) und von »Naked« (trocken), auf YouTube.</li>
<li><b>3. Doppelstunde:</b> der Chorus im Widget, <code>read</code> selbst (zwischen zwei Samples lesen, wie der Viertelpixel in »Video komprimieren«), der Flanger als Ausprobieren; der Hall nach Schroeder im Widget, <code>allpass</code> selbst, die Kämme sind das eigene <code>echo</code>.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/effects</code>: <code>wav.py</code> (gegeben), <code>eq.py</code> (schreibt <code>musik-bass.wav</code> und <code>musik-ohne-hihat.wav</code>), <code>echo.py</code> (schreibt <code>stimme-echo.wav</code> und <code>stimme-halle.wav</code>), <code>chorus.py</code> (<code>stimme-chorus.wav</code>), <code>reverb.py</code> (nimmt <code>echo</code> aus <code>echo.py</code>; <code>stimme-raum.wav</code> und <code>stimme-kirche.wav</code>, etwa zwei Sekunden Rechenzeit).</td></tr>
<tr><th>Prüfung</th><td><ul>
<li><code>bell</code>, <code>echo</code>, <code>read</code> und <code>allpass</code> werden mit kleinen Arrays geprüft; die Beispiele in <code>task-examples.yaml</code>.</li>
</ul></td></tr>
</table>

### Was schiefgeht

- **Die Rückkopplung zu hoch.** Ab 1 wird jedes Echo so laut wie das vorige oder lauter; `save` macht dann alles leiser, und die Stimme verschwindet unter ihren Echos. Das ist die Antwort auf das letzte Ausprobieren.
- **Gerundet statt gemischt.** Wer in `read` die Stelle rundet, hört kaum einen Unterschied, die Prüfung aber findet ihn: An der Stelle 1,5 liegt der Wert in der Mitte zwischen zwei Samples. Bei langsamem Tempo klingt die gerundete Verzögerung leise knisternd, weil sie in Sprüngen von einem Sample wandert.
- **Der Hall braucht Zeit.** `allpass` ist eine Schleife in Python über alle Samples, zweimal; mit `echo` viermal dazu dauert `reverb.py` einige Sekunden.
