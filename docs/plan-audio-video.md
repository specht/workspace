# Audio and video compression: a plan

Two activities, one for sound and one for moving pictures, each a ride from
uncompressed data through the ideas of compression to the codecs of today,
with programming tasks, listening and watching at every step, and the
history beside it. Not a reimplementation of MP3 or H.264: the students build
small coders that work on the same principles, and read real files where a
real format is the point. Written 10 November 2026 for the maintainer to
check before anything is built; nothing of it exists yet.

## What the Workspace needs

Checked in the current image (`hs_code_server`, Ubuntu 24.04):

- **Listening and watching work already.** code-server brings VS Code's
  `media-preview`: a WAV, MP3, MP4 or WebM file opens in the editor and plays
  in the student's browser. Every step can end with »open the file and
  listen«.
- **Missing: numpy, ffmpeg, Pillow.** Without numpy a naive Fourier
  transform of a few seconds of sound takes minutes in Python or Ruby;
  without ffmpeg there is no way to read or write a real video, to make MP3,
  AAC and Opus files of a clip, or to show a real encoder's motion vectors.
- **numpy needs no compiler** (it ships binary wheels), and »Fraktale
  Bildkompression« already has students run `pip install numpy pillow`
  themselves. It belongs in the image anyway: nothing to download in class,
  the same version for every student and for the task checks (which run in
  the Workspace), and it works without network. Fraktale Bildkompression
  would then lose its install step.

Proposed for the image:

- `ffmpeg` (with `ffprobe`) from Ubuntu's packages.
- `numpy` and `Pillow` for Python, pinned like `pytest`.
- A note in CHANGELOG (new Workspace image) and the size it adds, measured.

## Activity 1: sound (`audio-compression`)

Title proposals: »Audio komprimieren«, after »Komprimieren mit Huffman« and
»Komprimieren mit LZW«; or »Von der CD bis Opus«. Oberstufe, about five
double lessons, Python. Builds on »Komprimieren mit Huffman«; the
section on sampling in »Ein Spiel am Telefon« is the short version for Sek I
and links here.

Each part: a little history where it belongs, an idea, a widget to hear or
see it, and a small function to write first (»Jetzt bist du dran«), then
listening to what it does.

1. **Ton als Zahlen (PCM).** Sampling and bit depth, the sizes (a minute of
   CD audio is about 10 MB). Write a WAV file by hand: the 44-byte header,
   then 16-bit samples; a sine tone, two tones mixed, what happens when the
   sum leaves the range (clipping). Widget: one clip at several sample rates
   and bit depths, 16 down to 4 bits (the noise of quantisation).
   History: PCM (Alec Reeves, 1938), the CD (1982, 44,1 kHz, 16 bit).
2. **Kleine Tricks ohne Ohr.** Before any model of hearing: μ-law and A-law
   (G.711, 1972), 8 logarithmic bits instead of 16 linear ones, the
   telephone's 64 kbit/s; storing differences and predicting the next
   sample (ADPCM). Lossless with prediction and Rice codes, the idea of FLAC
   (link to Huffman: the residuals are small numbers). Tasks: μ-law there
   and back, compare the error with 8 linear bits; a predictor and the size
   of its residuals.
3. **Frequenzen.** What a sound is made of: the DFT on a few samples by hand,
   then an FFT of one's own (the reason it exists: a second of sound in
   milliseconds instead of minutes), compared with numpy's. Widget: a live
   spectrum and spectrogram of the clips. Task linked to the Telefonspiel:
   read a phone number from a WAV of DTMF tones.
4. **Was das Ohr nicht hört.** The threshold in quiet (a formula of Terhardt
   for it), critical bands (Bark), masking at the same time and shortly
   before and after a loud sound. Widgets: find one's own threshold at a few
   frequencies (with a clear word on volume), and a masking experiment: a
   loud tone and a quieter one beside it, the quieter one disappears. Task:
   remove what lies below the threshold, listen, count what is left.
5. **Blöcke und Fenster.** Frames, windows and overlap; the MDCT, whose
   overlapping blocks cancel each other's errors (TDAC). Task: an MDCT and
   its inverse with a sine window, and the check that nothing is lost. Pre-echo:
   castanets in long blocks, the noise smeared before the click; short
   blocks against it (block switching).
6. **Quantisieren nach Gehör.** Fewer bits where the ear masks the error:
   bands, scale factors, the bit allocation that keeps the noise under the
   masking threshold. Task: quantise per band, listen to the result and to
   the difference, the sound of what was thrown away. Then the bits: an
   entropy estimate and Huffman codes of the quantised values, the bit rate
   of one's own coder.
7. **Echte Dateien.** An MP3 frame header byte by byte (sync word, bit rate,
   sample rate, padding, frame length), the number of frames and the
   duration of a real file, the ID3 tag. ffmpeg makes the same clip as MP3,
   AAC and Opus at several bit rates: sizes beside each other, and a
   listening test in the browser (ABX: A, B and an unknown X; who can tell
   at 128 kbit/s?).
8. **Heute.** MP3 (Fraunhofer IIS and Karlheinz Brandenburg, MPEG-1 Layer III
   1992/93, the name ».mp3« in July 1995, the patents ended in 2017, tuned
   on Suzanne Vega's »Tom's Diner«); AAC (1997); Vorbis; Opus (2012,
   RFC 6716: SILK for speech and CELT for music in one codec, the codec of
   WebRTC); LC3 for Bluetooth LE Audio; Opus 1.5 (March 2024) with machine
   learning for lost packets and for speech at 6 kbit/s; neural codecs
   (SoundStream 2021, EnCodec 2022) that send a few kilobits and let a
   network make the sound again. What that means: a codec that invents
   details.

Person boxes: Karlheinz Brandenburg (photo with a free licence to find);
perhaps Alec Reeves and Jean-Marc Valin (Opus).

Material, all ours or freely licensed: Thorsten (CC0) for speech, music and
castanets synthesised as for the cave (`telefonspiel/sounds/make.py`), DTMF
tones, recordings under CC0 where a real instrument matters (Freesound),
each with its source in a README beside it.

## Activity 2: moving pictures (`video-compression`)

Title proposals: »Video komprimieren«; or »Von H.261 bis AV2«. Oberstufe,
about five double lessons, Python. The JPEG decoder (C, four double
lessons) is too heavy as a requirement: the parts about blocks and the DCT
come again in short, with the existing iDCT widget (`idct_stepper.js`), and
link to JPEG for those who did it.

1. **Bilder pro Sekunde.** What a second of video is uncompressed (1080p at
   25 frames, 24 bit: about 1,2 Gbit/s), against what a stream gets (a few
   Mbit/s). Read frames of a real video into an array (ffmpeg through a
   pipe), change them, write a video, watch it in VS Code. History: film at
   24 frames, television's interlacing.
2. **Farbe sparen.** YCbCr and chroma subsampling (4:2:0): a quarter of the
   colour, and the eye hardly sees it. Task: convert and subsample a frame,
   look at the three planes.
3. **Innerhalb eines Bildes.** Blocks, DCT and quantisation in short (iDCT
   widget, a quality slider), then what video adds: predicting a block from
   its neighbours (intra prediction, the directions of H.264). Task: predict
   blocks from the row above and the column to the left, measure the
   residual.
4. **Von Bild zu Bild.** Most of a frame is the frame before: difference
   images, small when the camera stands still, large when it pans. Task:
   difference images and their size after compression.
5. **Bewegung schätzen.** Block matching: for each block the shift that fits
   best (sum of absolute differences), full search against a faster search.
   Widget: drag a block over the next frame and watch the error. Task: motion
   vectors of a clip, drawn as arrows; then ffmpeg's `codecview` shows the
   vectors a real H.264 encoder found in the same clip.
6. **I-, P- und B-Frames.** Groups of pictures, frames that predict from
   later ones, why a player can only jump to a key frame. Task: the frame
   types of a real file with ffprobe. Datamoshing: a clip whose key frame is
   removed, the movement of one scene painted over the next.
7. **Artefakte und Filter.** Blocks, banding, the deblocking filter, film
   grain that AV1 removes before coding and adds again in the player.
8. **Bitrate steuern.** Constant quality against constant rate, two passes,
   the ladder of an adaptive stream (HLS, DASH). Task: the same clip at
   several qualities with ffmpeg, size against PSNR and SSIM, as a curve.
9. **Geschichte und heute.** H.120 (1984), H.261 (1988, the start of
   macroblocks, DCT and motion compensation together), MPEG-1 (Video CD),
   MPEG-2 (DVD, digital television), H.264/AVC (2003), HEVC (2013), VP9,
   AV1 (2018, royalty-free, AOMedia), VVC (2020), AV2 (specification 1.0 on
   28 May 2026, according to AOMedia about 30 % smaller than AV1; hardware
   not before about 2028). Patents and licences as the reason for the
   royalty-free codecs; neural video codecs as research.

Material: our own clips rendered for the purpose (a still camera, a pan, a
fast movement, fine texture, noise), and the Blender open movies (Big Buck
Bunny, Sintel, Tears of Steel: CC BY, with a credit line). Kept small: a few
seconds at 720p each.

## Order of work

1. The image: ffmpeg, numpy, Pillow; build, measure, test in a
   Workspace; CHANGELOG. Done on 10 November 2026: ffmpeg 6.1.1 with lame,
   Opus, x264 and libaom-av1, numpy 2.5.4, Pillow 13.0.0, 248 MB; tested as
   the student user. Once the image is in production, »Fraktale
   Bildkompression« can drop its `pip install numpy pillow`.
2. Sound: the material and its generator, the widgets, the page part by
   part, tasks with checks and `task-examples.yaml`, teacher note,
   `curriculum:` and `topics:` (docs/lehrplan.md, docs/bundeslaender.md),
   `sections.yaml`, the /lehrplan graph, the review of docs/checkliste.md.
   Begun on 10 November 2026, dev-only (`.audio-compression` after `jpeg`
   in `sections.yaml`), working title »Audio komprimieren«. Part 1 is
   written: the clips (`sounds/make.py`: `stimme.wav`, Thorsten's pangram,
   and `musik.wav`, a synthesised band, in `src/static/audio/`), the
   sampling widget (`sampling_widget.js`: rate, bits, μ-law with
   `data-mulaw`, a quiet clip with `data-level`), a WAV file of 60 bytes in
   the style of the GIF and JPEG pages (hexdump, field diagrams, byte
   tables), `wav_header` and `tone` as »Jetzt bist du dran«, the chord that
   clips; the teaser is the spectrogram of the music, computed by `bilder/teaser.py`. Part 2 is written:
   the quiet voice in the widget with even steps and μ-law, a drawing of
   the levels (`bilder/mulaw-stufen.py`), `unmulaw` and the signal-to-noise
   ratio in dB, the line through the last two samples (`residual`, FLAC's
   fixed predictor of order 2), zigzag and Rice codes (`rice`), one `k` per
   block of 4.096, FLAC from ffmpeg for comparison (voice 33 %, music 60 %;
   ours 46 % and 67 %). The recipes fetch the clips from
   `http://workspace_nginx_1/audio/`, which a Workspace on the development
   machine reaches. Part 3 is written: the spectrogram widget
   (`spectrum_widget.js`: music, voice, the chord, a gliding tone; the
   spectrum at the chosen moment), the DFT by hand on eight samples, `dft`
   and `fft` of one's own against `np.fft.fft` (4.096 samples 0,6 s, 8.192
   2,2 s; fft 0,05 s; numpy 5 ms; measured in the Workspace image), Cooley
   and Tukey with Gauß, `strength` and the number in `waehlen.wav` (a Drama
   Number of the Bundesnetzagentur, 030 23125386). A person box for Fourier
   waits until the page is public (the Personen test wants every box on a
   visible page): the engraving after Boilly, »Fourier2 - restoration1.jpg«
   on Wikimedia Commons, public domain, cropped 1200 × 1500 from (230, 140).
   Part 4 is written: cochlea and Bark, the
   threshold-in-quiet widget and the masking widget (`hearing_widget.js`,
   right after their predictions; the masking model waits for its switch
   »Modell zeigen«, the threshold curve for the student's own note at
   1.000 Hz, so neither gives the prediction away; nothing plays louder than -20 dBFS, a reference tone sets the
   volume once), `threshold` after Terhardt (via Painter and Spanias 2000)
   and `audible` with the 96 dB assumption (music keeps 53 % of the
   frequencies, the voice 39 %; what goes is mostly bass under 60 Hz and
   highs over 14 kHz), masking in time in text. Part 5 is written: why
   blocks side by side crack, windows and overlap, `mdct` of one's own
   (`imdct`, `window`, `encode`, `decode` on the page), a single block and
   its mirror images as a prediction, a drawing of TDAC (`bilder/tdac.py`,
   computed with the stage), the castanets (`kastagnetten.wav` from
   `sounds/make.py`) rounded per block in long and short blocks, a drawing
   of the pre-echo (`bilder/vorecho.py`), block switching in AAC (1.024 and
   8 × 128) and MP3 (576 and 3 × 192). Part 6 is written: the MDCT as a
   matrix (`tones` with `functools.cache`), 25 bands after Bark, `allowed`
   of one's own (Schroeder's spreading, energies added, never under the
   threshold in quiet), one step per band from the noise it may have
   (`step ** 2 / 12`), the rest as a prediction, `entropy` of one's own and
   the bit rate (music 76 kbit/s mono at `OFFSET` 15 against 706; 70 % of
   the numbers are 0; the rest is 14 dB under the music, 61 % of its
   energy under 200 Hz). Not yet listened to by a person: whether 15 dB is
   transparent. Part 7 is written: `musik.mp3` from
   ffmpeg in a hexdump (ID3 tag with its synchsafe size, the frame header
   bit by bit), `frame_length` of one's own, all 309 frames walked and the
   Info frame, the duration against ffprobe; MP3, AAC and Opus at three
   rates each and their sizes (Opus takes the rate as a target: 50 kbit/s
   for 32 here); the ABX widget (`abx_widget.js`, Munson and Gardner 1950)
   with pieces made by `sounds/abx.py`: coded, decoded by ffmpeg, cut to the
   same five seconds as lossless FLAC (4,4 MB for ten pieces), checked to
   start at the same sample. The page was split into parts for a while
   (`parts:`), then put back on one page at the maintainer's word: eight
   chapters do not need it. Part 8
   is written: MP3's history (Seitzer's jukebox over ISDN, Brandenburg's
   thesis 1989, MPEG-1 1992/93, Tom's Diner, .mp3 in 1995, the patents to
   2017), AAC, Vorbis, Opus (SILK and CELT, WebRTC), LC3, Opus at 6 kbit/s
   as a try box, Opus 1.5, SoundStream and EnCodec, a reflection on codecs
   that invent details; the picture is a timeline. The audio activity is
   complete in its eight parts. Open: listening by a person (the clips, the
   codec at OFFSET 15, the pre-echo), the titles, a person box for Fourier
   and perhaps Brandenburg once the page is public, the section's new
   name; then the video activity. The section is
   renamed and the /lehrplan graph recomputed when the page leaves
   development, not before: until then no public page is about sound.
3. Moving pictures the same way.

Each activity is several sessions of work; the widgets (spectrum and
spectrogram, threshold and masking, quantisation with listening, ABX, block
matching) are most of it.

## Facts checked so far

- MP3: MPEG-1 Layer III standardised 1992 to 1993 (ISO/IEC 11172-3); ».mp3«
  chosen by Fraunhofer researchers in July 1995 (sources differ on the 12th
  and the 14th); the licensing programme ended on 23 April 2017; Brandenburg
  tested with »Tom's Diner«.
  [mp3-history.com](https://www.mp3-history.com/en/timeline.html),
  [Wikipedia](https://en.wikipedia.org/wiki/MP3)
- Opus 1.5, 4 March 2024: Deep Redundancy (DRED), Deep PLC, LACE and NoLACE
  for speech down to 6 kbit/s, all switched off by default.
  [opus-codec.org](https://opus-codec.org/demo/opus-1.5/)
- AV2: specification 1.0.0 on 28 May 2026, announced 9 June 2026, about
  30 % more efficient than AV1 according to AOMedia.
  [AOMedia](http://aomedia.org/press%20releases/Alliance-for-Open-Media-Releases-AV2-Codec/)

Every other date above is from memory and gets a source when its page is
written.

## Decided (10 November 2026)

- **Python only**, like »Fraktale Bildkompression«: numpy is what signal
  processing uses, one version of every task. The numo gems are not needed.
- **The section** »Bilder und Kompression« becomes »Bilder, Ton und
  Kompression«; both activities go after JPEG.
- **The titles** are still open; the folders are `audio-compression` and
  `video-compression` whatever the titles become.

## Open decisions for the maintainer

- **The titles** (proposals above).
- **The hearing threshold widget** plays quiet and loud tones: a warning to
  start quietly, and a maximum level in the code.
- **A real MP3 decoder in C** (like JPEG) stays out; perhaps later for strong
  courses.
