# Plan: »Audioeffekte« (audio effects)

A new activity, `src/content/audio-effects`, decided by the maintainer on
11 November 2026: the effects of a studio and of a phone app, each built in
Python with numpy and each with a widget to listen to while turning its
knobs. It builds on »Audio komprimieren« (`requires: [audio-compression]`):
samples as numbers from −1 to 1, WAV files, Dezibel, the FFT. Until it is
finished it stands in the catalog with a dot (`- .audio-effects`), only in
development.

## Chapters (one Doppelstunde each, roughly)

1. **Einrichten, Lautstärke.** The sounds (`musik.wav`, `stimme.wav` and the
   instruments of the music from »Audio komprimieren«), `wav.py` given;
   gain in dB as a factor (10^(dB/20)), fading in and out.
2. **Equalizer.** Moved here from »Audio komprimieren«, where it was
   written first (the widget, `bell` of one's own, the band-pass with
   `rest`). The compression page keeps a sentence and a link.
3. **Echo.** A delayed copy: y[n] = x[n] + g · y[n − D], with feedback;
   numpy block by block (each block of D samples from the one before), so
   no loop over single samples.
4. **Chorus and flanger.** A copy whose delay wobbles: d(t) = base +
   depth · sin(2π · rate · t), read between samples with linear
   interpolation (the quarter pixel of »Video komprimieren« in time).
5. **Hall.** Many echoes: Schroeder's reverberator (1962), four comb
   filters side by side and two all-pass filters after them; the room's
   size scales the delays, the feedback the length of the hall.
6. **Compressor.** (The maintainer: the industry's English terms, Compressor, Threshold, Ratio, Attack, Release, Make-up Gain.) Loud parts quieter: an envelope follower (attack,
   release), above a threshold the level divided by the ratio, make-up
   gain. One loop over the samples (the only one, and why it cannot be
   avoided: each value needs the one before).
7. **Tonhöhe.** Finding the pitch of the voice per block by
   autocorrelation; shifting the pitch without changing the length with
   grains (short windowed pieces, each resampled, added back at their old
   places); then autotune: every block's pitch moved to the nearest
   semitone.
8. **Vocoder.** The voice's loudness in each band (the bands of »Audio
   komprimieren«) given to a chord of saw waves: the robot voice.

Each chapter: a widget first (to hear the effect and find what its knobs
do), a prediction, the function of one's own with tips in steps and a
check, a file to listen to, an »Ausprobieren« box that says what to listen
for. Persons and papers where they belong (Schroeder, the vocoder of Homer
Dudley 1938, Auto-Tune by Andy Hildebrand 1997, …), every fact checked.

## The widget

One widget for all effects, `effect_widget.js`, `<div class='effect-widget'
data-effect='echo'>`: a choice of sound, play and stop, »Effekt an« to
compare, the knobs of the effect as sliders with marks, and a picture that
shows what the knobs do (the EQ's curve, the echoes as spikes over time,
the compressor's curve of input against output level, the pitch over
time). The sound is computed in a worker with the same algorithm as the
Python of the page; a change crosses over exactly where the old sound is,
also while a knob is dragged (as the EQ widget of »Audio komprimieren«
does already: `eq_widget.js`, which becomes the first effect of it).

## State (11 November 2026)

Built, each with a widget, a prediction, a function of one's own with
checks and examples, a file to listen to: Lautstärke in Dezibel (gain),
Equalizer (bell, with presets old radio, phone, party next door), Echo,
Chorus (read between samples), Hall (allpass), Compressor (gain_db),
Pitch Shifting (pitch by autocorrelation, grains aligned with
np.correlate), Autotune (nearest_note; in the widget NASA's commentary of the
Artemis II launch as a Songify song, bilder/liftoff_song.py; Kennedy's
»We choose to go to the Moon« was tried first, its 1962 sound too poor), Vocoder
(vocode_frame), Stimmen für Filme (ring; chipmunk, giant, Dalek, Vader,
Grievous). History boxes with sources in content/history-sources.yaml.
The widget plays fifteen recordings under free licences (audio/effects/,
credits in CLIPS of effect_widget.js); one widget plays at a time.

## Real sounds instead of generated ones

The maintainer (11 November 2026): remove the generated music wherever
possible and replace the Thorsten voice samples with a modern, clear
recording. What that touches:

- »Audioeffekte«: musik.wav and stimme.wav are clips and the files students
  fetch in »Einrichten«; easy to swap.
- »Audio komprimieren«: musik.wav and its six stems (the spectrogram's
  switches), stimme.wav (a pangram rich in s, f, sch, z), kastagnetten.wav
  (pre-echo) are read by the stages, tasks and examples, and many numbers
  in the text are measured from them (sizes, ratios, frames). Swapping means
  measuring again and correcting every number; screenshots from recipes.
  waehlen.wav (DTMF) stays generated: the synthesis is the subject.
- Telefonspiel: Thorsten stays as the speech of the games (what students
  hear is what their game will say); only its listening examples and the
  cave music hoehle.mp3 could change.
- Found (11 November 2026, licences checked on the source pages):
  - German voice, all »Gesprochene Wikipedia«, CC BY-SA 4.0: Ursonaten
    reading »Ralf Dahrendorf« (»Er war ein deutsch-britischer Soziologe,
    Publizist und Politiker …«, 6,8 s, the cleanest), Jonsonr reading
    »Konsequentialismus« (4,0 s), MsAntimodes reading »Palais Meckel« (the
    most sibilants, but from a 60 kbit/s file without anything above
    15 kHz, wrong for a page about compression). A page that uses one
    becomes CC BY-SA for that file; credit line needed.
  - No free song with separate stems that could be verified (ccMixter sets
    are partial and refuse scripted downloads; MedleyDB and MUSDB18 are NC).
    So musik.wav from real samples: DRSKit drums (CC BY 4.0, audio/drums),
    FreePats »Clean Electric Bass YR« (CC0), Salamander Grand Piano (CC BY
    3.0) or VCSL (CC0: organ, pianos, vibraphone), VSCO 2 CE strings (CC0)
    as a pad. Its six stems come for free.
  - Castanets: »castanets dry.wav« by Satoration (CC BY 4.0, Freesound
    57299), clicks with 0,6 s of silence between; or »Castanet« by
    JarredGibb (CC0, Freesound 263882), a children's castanet.

## Open for the maintainer

- The title is »Audioeffekte« (decided).
- A topic key for sound processing (`themen.yaml` has none; for now
  `codierung` and `listen`).
- A recording of a singing voice under a free licence for autotune; until
  then the spoken `stimme.wav`, which autotune turns into a melody.
