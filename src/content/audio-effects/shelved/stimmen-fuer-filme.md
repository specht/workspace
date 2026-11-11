# Shelved: »Stimmen für Filme«

Taken off the page on 11 November 2026 (the maintainer: it does not sound
right yet). To bring it back: the chapter goes after »Vocoder« in
audio-effects.md, the tasks into tasks.yaml, the examples into
task-examples.yaml, the sources into content/history-sources.yaml. The
widget's effect »voices« (effect_widget.js), stufen/voices.py and
fehler/ring-*.py stayed in place. Open: the distortion of »Grievous« turns
the voice into square waves; a gentler one, and listening to the presets.

## The chapter

## Stimmen für Filme

Im Film sprechen Streifenhörnchen, Riesen und Roboter, und oft steckt dahinter eine menschliche Stimme mit einem Effekt aus dieser Seite. Ein Streifenhörnchen ist eine Stimme eine Oktave höher, ein Riese eine, die einige Halbtöne tiefer liegt. Für Roboter nimmt man oft einen **Ringmodulator**: Er multipliziert die Stimme Sample für Sample mit einer Sinusschwingung. Wo der Sinus durch null geht, ist auch die Stimme still, und zwischen zwei Nullstellen schwingt sie mal mit, mal gegen sich selbst: Aus jeder Frequenz \\(f\\) der Stimme werden zwei, \\(f - r\\) und \\(f + r\\), wenn \\(r\\) die Frequenz des Sinus ist. Die Stimme klingt danach metallisch, und keine ihrer Frequenzen passt mehr zu den anderen.

<div class='effect-widget' data-effect='voices'></div>

<div class='hint try'>
<strong>Ausprobieren:</strong> Probier die Knöpfe unter den Reglern und schau im Bild, was mit der Welle geschieht. Wodurch unterscheiden sich Darth Vader und General Grievous? Stell dann den Ringmodulator langsam von 0 auf 200 Hz: Ab wann hörst du ein Flattern, ab wann einen metallischen Klang? Was passiert, wenn du die Stimme zusätzlich tiefer machst?
</div>

<div class='history' id='history-film-voices'>
<p><strong>Streifenhörnchen und Daleks</strong>, 1958 und 1963. Ross Bagdasarian sang die Stimmen seiner Streifenhörnchen selbst, langsam, und nahm sie mit halber Geschwindigkeit auf Tonband auf. Mit normaler Geschwindigkeit abgespielt, klangen sie eine ganze Oktave höher, im richtigen Tempo. »The Chipmunk Song« verkaufte sich in sieben Wochen 4,5 Millionen Mal. Die Daleks aus der britischen Serie »Doctor Who« tauchten 1963 zum ersten Mal auf; ihre Stimmen machte Brian Hodgson vom BBC Radiophonic Workshop mit einem Ringmodulator.</p>
</div>

_task(ring-frequencies)

Lege `voices.py` an. Es nimmt `shift` und `save` aus `pitch.py`:

<!-- page-code: own -->
```python
import numpy as np

from pitch import save, shift
from wav import read_wav


def ring(x, rate, freq):
    # hier: x, Sample für Sample mit einer Sinusschwingung von freq Hz multipliziert


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-chipmunk.wav', shift(x, rate, 2), rate)
    save('stimme-riese.wav', shift(x, rate, 2 ** (-8 / 12)), rate)
    save('stimme-dalek.wav', ring(x, rate, 30), rate)
```

<div class='hint own' data-check='ring'>
<strong>Jetzt bist du dran:</strong> Schreib <code>ring(x, rate, freq)</code>: <code>x</code>, Sample für Sample mit einer Sinusschwingung von <code>freq</code> Hz multipliziert.
</div>

<!-- hint -->
Das Sample mit dem Index `n` gehört zur Zeit `n / rate` Sekunden. Die Sinusschwingung ist dort `np.sin(2 * np.pi * freq * n / rate)`; mit `n = np.arange(len(x))` rechnet numpy das für alle Samples auf einmal.
<!-- /hint -->

<!-- solution -->
```python
def ring(x, rate, freq):
    n = np.arange(len(x))
    return x * np.sin(2 * np.pi * freq * n / rate)
```
<!-- /solution -->

_task(ring)

```bash
python3 voices.py
```

<!-- tutorial-step
write-file: effects/voices.py <- file:stufen/voices.py
check-task: ring
terminal-run: cd /workspace/effects && python3 voices.py
terminal-wait-for-prompt: 60
wait-for-text: stimme-dalek.wav
-->

Hör dir die drei Dateien an. Das Streifenhörnchen ist mit `shift` gemacht, nicht schneller abgespielt wie bei Bagdasarian: Es spricht im Tempo der Stimme.

<div class='history' id='history-star-wars'>
<p><strong>Darth Vader und General Grievous</strong>, 1977 und 2005. Darth Vader spricht in allen Filmen mit der Stimme von James Earl Jones, die kaum verändert ist; was ihn ausmacht, ist sein Atmen. Das machte der Sounddesigner Ben Burtt aus veränderten Aufnahmen eines Tauchgeräts. General Grievous dagegen, ein Cyborg aus »Die Rache der Sith«, sollte nach dem Wunsch von George Lucas klingen, als spräche er durch einen künstlichen Kehlkopf. Matthew Wood sprach ihn mit rauer, harter Stimme, die Bearbeitung am Rechner gab ihr etwas Kratziges, und Lucas wünschte sich dazu einen Husten wie bei Asthma.</p>
</div>


## tasks.yaml

```yaml
  - id: ring-frequencies
    title: Zwei Frequenzen aus einer
    prediction: true
    prompt: |
      Eine Stimme singt einen Ton mit 150 Hz. Ein Ringmodulator multipliziert
      sie mit einer Sinusschwingung von 30 Hz. Heraus kommen zwei Frequenzen:
      Wie hoch ist die höhere, in Hz?
    check:
      type: number
      expect: 180
      tolerance: 0.5
    feedback:
      - when: { equals: 150 }
        say: Der Ton von 150 Hz bleibt nicht; aus ihm werden zwei andere.
      - when: { equals: 120 }
        say: 120 Hz ist die tiefere der beiden, 150 − 30.
      - when: { equals: 4500 }
        say: Die Samples werden multipliziert, nicht die Frequenzen.
    solution: |
      180 Hz: Aus 150 Hz werden 150 − 30 = 120 Hz und 150 + 30 = 180 Hz.

  - id: ring
    title: Ein Ringmodulator
    prompt: |
      Schreib in `effects/voices.py` die Funktion `ring(x, rate, freq)`: `x`,
      Sample für Sample mit einer Sinusschwingung von `freq` Hz multipliziert.
    check:
      type: python
      file: effects/voices.py
      cases:
        - call: '[round(v, 4) for v in ring(np.ones(5), 4, 1)]'
          expect: '[0.0, 1.0, 0.0, -1.0, -0.0]'
          say: 'Mit 4 Samples in der Sekunde und 1 Hz macht der Sinus in vier Samples eine Schwingung: 0, 1, 0, −1. Rechnest du `np.sin(2 * np.pi * freq * n / rate)` und multiplizierst `x` damit?'
        - call: '[round(v, 4) for v in ring(np.array([2.0, 2, 2]), 4, 1)]'
          expect: '[0.0, 2.0, 0.0]'
          say: Der Sinus wird mit `x` multipliziert, nicht dazugezählt.
    solution: |
      ```python
      def ring(x, rate, freq):
          n = np.arange(len(x))
          return x * np.sin(2 * np.pi * freq * n / rate)
      ```
```

## task-examples.yaml

```yaml
  ring:
    pass:
      - { files: { effects/voices.py: stufen/voices.py, effects/pitch.py: stufen/pitch.py, effects/chorus.py: stufen/chorus.py, effects/wav.py: stufen/wav.py } }
    fail:
      - { files: { effects/voices.py: fehler/ring-plus.py, effects/pitch.py: stufen/pitch.py, effects/chorus.py: stufen/chorus.py, effects/wav.py: stufen/wav.py }, say: 'multiplizierst' }
      - { files: { effects/voices.py: fehler/ring-ohne-rate.py, effects/pitch.py: stufen/pitch.py, effects/chorus.py: stufen/chorus.py, effects/wav.py: stufen/wav.py }, say: 'n / rate' }
```

## history-sources.yaml

```yaml
film-voices:
  - text: Streifenhörnchen und Daleks, 1958 und 1963
    sources:
      - url: https://en.wikipedia.org/wiki/Alvin_and_the_Chipmunks
        quote: "The Chipmunks first officially appeared in a novelty record released in late fall 1958 by Bagdasarian."
      - url: https://en.wikipedia.org/wiki/Dalek
        quote: "They first appeared in the 1963 Doctor Who serial The Daleks."
  - text: Ross Bagdasarian sang die Stimmen seiner Streifenhörnchen selbst, langsam, und nahm sie mit halber Geschwindigkeit auf Tonband auf
    sources:
      - url: https://en.wikipedia.org/wiki/Alvin_and_the_Chipmunks
        quote: "sung slowly but recorded at half speed on the tape recorder"
  - text: Mit normaler Geschwindigkeit abgespielt, klangen sie eine ganze Oktave höher, im richtigen Tempo
    sources:
      - url: https://en.wikipedia.org/wiki/Alvin_and_the_Chipmunks
        quote: "When the tape was played back at normal speed, they would sound a full octave higher in pitch, at normal tempo."
  - text: »The Chipmunk Song« verkaufte sich in sieben Wochen 4,5 Millionen Mal
    sources:
      - url: https://en.wikipedia.org/wiki/Alvin_and_the_Chipmunks
        quote: "The novelty record was highly successful, selling 4.5 million copies in seven weeks."
  - text: Die Daleks aus der britischen Serie »Doctor Who« tauchten 1963 zum ersten Mal auf
    sources:
      - url: https://en.wikipedia.org/wiki/Dalek
        quote: "They first appeared in the 1963 Doctor Who serial The Daleks."
  - text: ihre Stimmen machte Brian Hodgson vom BBC Radiophonic Workshop mit einem Ringmodulator
    sources:
      - url: https://en.wikipedia.org/wiki/Ring_modulation
        quote: "One of the best-known applications of the ring modulator may be its use by Brian Hodgson of the BBC Radiophonic Workshop to produce the distinctive voice of the Daleks in the television series Doctor Who"

star-wars:
  - text: Darth Vader und General Grievous, 1977 und 2005
    sources:
      - url: https://en.wikipedia.org/wiki/Darth_Vader
        quote: "first appeared in the original Star Wars film (1977)"
      - url: https://en.wikipedia.org/wiki/Star_Wars:_Episode_III_%E2%80%93_Revenge_of_the_Sith
        quote: "Star Wars: Episode III – Revenge of the Sith is a 2005 American epic space opera film written and directed by George Lucas."
  - text: Darth Vader spricht in allen Filmen mit der Stimme von James Earl Jones, die kaum verändert ist
    sources:
      - url: https://en.wikipedia.org/wiki/Darth_Vader
        quote: "James Earl Jones provides his voice in all of the films and in other media."
    note: der Artikel nennt keine Bearbeitung der Stimme; »kaum verändert« daher vorsichtig
  - text: was ihn ausmacht, ist sein Atmen
    none: Bewertung, die zum nächsten Satz führt
  - text: Das machte der Sounddesigner Ben Burtt aus veränderten Aufnahmen eines Tauchgeräts
    sources:
      - url: https://en.wikipedia.org/wiki/Darth_Vader
        quote: "The sound of Vader's breathing was created by the film's sound designer, Ben Burtt, using modified recordings of a scuba breathing apparatus."
  - text: General Grievous dagegen, ein Cyborg aus »Die Rache der Sith«, sollte nach dem Wunsch von George Lucas klingen, als spräche er durch einen künstlichen Kehlkopf
    sources:
      - url: https://en.wikipedia.org/wiki/General_Grievous
        quote: "He is a powerful Kaleesh cyborg. … Lucas wanted Grievous to sound as if he was speaking through a cybernetic voice box."
  - text: Matthew Wood sprach ihn mit rauer, harter Stimme, die Bearbeitung am Rechner gab ihr etwas Kratziges, und Lucas wünschte sich dazu einen Husten wie bei Asthma
    sources:
      - url: https://en.wikipedia.org/wiki/General_Grievous
        quote: "Wood approached Grievous as a \"classic villain\" and used a gruff and harsh voice. … A \"gravelly quality\" was added to Wood's voice when it was processed. … he instructed Wood to give Grievous an asthmatic cough."
```

The picture for its history box (Ross Bagdasarian, Liberty Records press photo, 1972, public domain, https://commons.wikimedia.org/wiki/File:Ross_Bagdasarian_1972.jpg) is shelved/film-voices.webp; credit »Foto: Liberty Records, 1972, gemeinfrei, zugeschnitten (Wikimedia Commons)«, alt »Ross Bagdasarian, Schwarzweißfoto: ein Mann mit dunklem, zurückgekämmtem Haar und offenem Hemdkragen vor hohem Gras«.
