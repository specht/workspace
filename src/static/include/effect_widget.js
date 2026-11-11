// Audio effects (»Audioeffekte«): one widget for every effect of the
// page. A sound plays in a loop; the knobs of the effect are sliders with
// marks; »Effekt an« compares with the sound as it is; a picture shows
// what the knobs do. The sound is computed in a worker with the same
// algorithm as the Python of the page; on a change the new sound crosses
// over exactly where the old one is, also while a knob is dragged, so the
// sound never stops or jumps. Effects that last in time (echo) are
// computed over the sound twice in a row, and the second time is played:
// so what the end leaves over reaches the start of the loop.
//
//   <script src='/include/effect_widget.js'></script>
//   <div class='effect-widget' data-effect='eq'></div>
//   <div class='effect-widget' data-effect='echo' data-clip='stimme'></div>
//
// The effects (EFFECTS below): gain, eq, echo, chorus, reverb, compressor, pitch, autotune, vocoder, voices. Each has its knobs, its picture
// and its function in WORKER, written like the Python of the page.
(function () {
    'use strict';
    // the cache buster of this script's address, also for the sounds: a
    // new version of a sound is fetched again, not taken from the cache
    const BUSTER = ((document.currentScript && document.currentScript.src) || '').split('?')[1] || '';
    const FADE = 0.03;
    const LABEL_W = 46, AXIS = 18;
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    const signedDb = (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)} dB`;
    // a factor as people read it: from 1 on one decimal (4,0), below two digits (0,5, 0,032)
    const factor = (f) => (f >= 1 ? de(f, 1) : f.toLocaleString('de-DE', { maximumSignificantDigits: 2 }));
    const hz = (f) => (f >= 1000 ? `${(f / 1000).toLocaleString('de-DE', { maximumFractionDigits: f < 10000 ? 1 : 0 })} kHz` : `${de(Math.round(f))} Hz`);

    const el = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'class') node.className = v;
            else if (k === 'text') node.textContent = v;
            else node.setAttribute(k, v);
        }
        for (const c of [].concat(children)) node.append(c);
        return node;
    };

    // the sounds: the files of »Audio komprimieren«
    const CLIPS = {
        musik: { label: 'Musik', url: '/audio/musik.wav' },
        stimme: { label: 'Stimme', url: '/audio/stimme.wav' },
        melodie: { label: 'Melodie', url: '/audio/musik/melody.flac' },
        // recordings under free licences (audio/effects/), each with its credit
        gesang: { label: 'Gesang', url: '/audio/effects/singing-voice-2.flac',
            credit: "»Running with Renegades (Main Vox)«, feat. Ashes and Dreams, CC BY 4.0 (<a href='https://freesound.org/people/ashesanddreams/sounds/610416/'>Freesound</a>)" },
        chor: { label: 'Chor', url: '/audio/effects/choir.flac',
            credit: "»Angels We Have Heard on High« (traditionell), Chor der U.S. Army Band, gemeinfrei (<a href='https://commons.wikimedia.org/wiki/File:U.S._Army_Band_-_Angels_We_Have_Heard_on_High.ogg'>Wikimedia Commons</a>)" },
        klavier: { label: 'Klavier', url: '/audio/effects/piano-2.flac',
            credit: "Scott Joplin, »Maple Leaf Rag«, gespielt von William J. Leslie, CC BY-SA 2.5 (<a href='https://commons.wikimedia.org/wiki/File:Maple_Leaf_RagQ.ogg'>Wikimedia Commons</a>)" },
        akustikgitarre: { label: 'Akustikgitarre', url: '/audio/effects/acoustic-guitar.flac',
            credit: "»acoustic guitar melody #2 d-dur« von Garuda1982, CC0 (<a href='https://freesound.org/people/Garuda1982/sounds/462723/'>Freesound</a>)" },
        egitarre: { label: 'E-Gitarre', url: '/audio/effects/electric-guitar.flac',
            credit: "»Clean Strumming &amp; Arpeggio« von loudernoises, CC0 (<a href='https://freesound.org/people/loudernoises/sounds/453384/'>Freesound</a>)" },
        orchester: { label: 'Orchester', url: '/audio/effects/orchestra.flac',
            credit: "Bedřich Smetana, »Die Moldau«, Czech National Symphony Orchestra (Musopen Symphony), gemeinfrei (<a href='https://archive.org/details/MusopenCollectionAsFlac'>Internet Archive</a>)" },
        jazz: { label: 'Jazz', url: '/audio/effects/jazz.flac',
            credit: "»Acoustic Jazz Quartet no1 (exploration)« von Claude Reid, CC BY-SA 4.0 (<a href='https://commons.wikimedia.org/wiki/File:Acoustic_Jazz_Quartet_no1_(exploration).flac'>Wikimedia Commons</a>)" },
        rock: { label: 'Rock', url: '/audio/effects/rock.flac',
            credit: "»Big Rock« von Kevin MacLeod (incompetech.com), CC BY 4.0 (<a href='https://incompetech.com/music/royalty-free/faq.html'>incompetech</a>)" },
        elektronik: { label: 'Elektronik', url: '/audio/effects/electronic.flac',
            credit: "»Beauty Flow« von Kevin MacLeod (incompetech.com), CC BY 4.0 (<a href='https://incompetech.com/music/royalty-free/faq.html'>incompetech</a>)" },
        funk: { label: 'Funk', url: '/audio/effects/funk.flac',
            credit: "»Funkorama« von Kevin MacLeod (incompetech.com), CC BY 4.0 (<a href='https://incompetech.com/music/royalty-free/faq.html'>incompetech</a>)" },
        gamelan: { label: 'Gamelan', url: '/audio/effects/gamelan.flac',
            credit: "»Javanese Court Gamelan 1 – Indonesia« von RTB45, CC BY 4.0 (<a href='https://freesound.org/people/RTB45/sounds/255544/'>Freesound</a>)" },
        veena: { label: 'Veena', url: '/audio/effects/veena.flac',
            credit: "»rAga kIravANi« von L. Ramakrishnan, CC0 (<a href='https://archive.org/details/LR_kIravANI'>Internet Archive</a>)" },
        schlagzeug: { label: 'Schlagzeug', url: '/audio/effects/drums.flac',
            credit: "»Rock-funk backbeat« von jmarcosfer, CC0 (<a href='https://freesound.org/people/jmarcosfer/sounds/592756/'>Freesound</a>)" },
        funkdrums: { label: 'Funk-Schlagzeug', url: '/audio/effects/funk-drums.flac',
            credit: "»Drums_120bpm_closedhat_snareghost_loop1« von tichec, CC0 (<a href='https://freesound.org/people/tichec/sounds/169870/'>Freesound</a>)" },
        dynamik: { label: 'Orchester leise und laut', url: '/audio/effects/dynamic-orchestra.flac',
            credit: "Ludwig van Beethoven, Egmont-Ouvertüre op. 84, Czech National Symphony Orchestra (Musopen Symphony), gemeinfrei (<a href='https://commons.wikimedia.org/wiki/File:Beethoven_-_Egmont_Overture,_Op._84_(Musopen_Symphony).flac'>Wikimedia Commons</a>)" },
        // Kennedy at Rice University, 1962, and a melody from his voice (bilder/moon_song.py)
        kennedy: { label: 'Kennedy', url: '/audio/kennedy.flac',
            credit: "John F. Kennedy, »We choose to go to the Moon«, Rice University, 12. September 1962, gemeinfrei (<a href='https://commons.wikimedia.org/wiki/File:Jfk_rice_university_we_choose_to_go_to_the_moon.ogg'>Wikimedia Commons</a>); seine Sätze im Takt geschnitten" },
        kennedyMelodie: { label: 'Melodie', url: '/audio/kennedy-melodie.flac',
            credit: 'Die Melodie zu Kennedys Worten: ein Ton für jedes Wort, in seiner Stimmlage.' },
        kennedyBand: { label: 'Band', url: '/audio/kennedy-band.flac',
            credit: "Bm, G, D, A, 95 Schläge in der Minute, mit 808-Bass; Schlagzeug »DRSKit« von Lars Muldjord und Deva (DrumGizmo), CC BY 4.0 (<a href='https://drumgizmo.org/wiki/doku.php?id=kits:drskit'>drumgizmo.org</a>), Klatschen von DCphotofilm, CC0 (<a href='https://freesound.org/people/DCphotofilm/sounds/456800/'>Freesound</a>)" },
    };

    // a logarithmic slider: 0 to 1000 between lo and hi
    const logSlider = (lo, hi) => ({
        // two significant digits, so the middle of 1k shows 1 kHz, not 999 Hz
        to: (v) => Number((lo * (hi / lo) ** (v / 1000)).toPrecision(2)),
        from: (f) => Math.round(1000 * Math.log(f / lo) / Math.log(hi / lo)),
    });
    const CENTRE = logSlider(40, 16000);

    // the bell of the equalizer, as students write it in Python, with rest
    // for the frequencies far away (0: the bell of the page)
    const bellDb = (f, centre, width, gain, rest) => {
        const d = Math.log2(Math.max(f, 1) / centre);
        return rest + (gain - rest) * 2 ** (-((d / width) ** 2));
    };

    const RATIO = logSlider(1, 20), ATTACK = logSlider(0.001, 0.1), RELEASE = logSlider(0.02, 1);
    const ms = (v) => `${de(v * 1000)} ms`;

    // the compressor's gain in dB at a level in dB, as students write it
    const gainDb = (level, threshold, ratio) => (level <= threshold ? 0 : (threshold - level) * (1 - 1 / ratio));

    // the EQ's curve in dB at f: the bell, or a high-pass or low-pass whose
    // edge lies at the centre and is »width« octaves wide
    const eqDb = (f, p) => {
        if (p.shape === 'high' || p.shape === 'low') {
            const d = Math.log2(Math.max(f, 1) / p.centre) * (p.shape === 'high' ? 1 : -1);
            return p.rest + (p.gain - p.rest) / (1 + Math.exp(-4 * d / p.width));
        }
        return bellDb(f, p.centre, p.width, p.gain, p.rest);
    };

    const EFFECTS = {
        gain: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['musik', 'stimme', 'klavier', 'rock'],
            knobs: [
                { key: 'db', label: 'Verstärkung', min: -40, max: 12, step: 1, value: -6,
                  marks: [[-40, '−40'], [-30, '−30'], [-20, '−20'], [-12, '−12'], [-6, '−6'], [0, '0'], [6, '+6'], [12, '+12 dB']], show: signedDb },
            ],
            clip: true,                          // too loud is cut off, as a converter does
            neutral: (p) => p.db === 0,
            caption: 'Oben die Dezibel, darunter der Faktor, mit dem jedes Sample multipliziert wird; der Strich ist die Einstellung.',
            line: (p) => {
                const f = 10 ** (p.db / 20);
                const words = p.db === 0 ? 'alles bleibt, wie es ist'
                    : p.db === -6 ? 'jede Schwingung etwa halb so hoch'
                    : p.db === -20 ? 'jede Schwingung ein Zehntel so hoch'
                    : p.db === 6 ? 'jede Schwingung etwa doppelt so hoch'
                    : p.db > 0 ? 'was über 1 hinausginge, wird abgeschnitten' : 'leiser';
                return `${signedDb(p.db)}: jedes Sample mal ${factor(f)}, ${words}; die Leistung mal ${factor(f * f)}.`;
            },
            draw: drawGain,
        },
        eq: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['musik', 'stimme', 'jazz', 'elektronik', 'orchester', 'funk'],
            knobs: [
                { key: 'centre', label: 'Frequenz', min: 0, max: 1000, step: 1, value: CENTRE.from(1000), scale: CENTRE,
                  marks: [[50, '50'], [100, '100'], [200, '200'], [500, '500'], [1000, '1k'], [2000, '2k'], [5000, '5k'], [10000, '10k']],
                  show: (v) => hz(v) },
                { key: 'width', label: 'Breite', min: 0.1, max: 4, step: 0.1, value: 1,
                  marks: [[0.5, '½'], [1, '1'], [2, '2'], [3, '3'], [4, '4']], show: (v) => `${de(v, 1)} Oktaven` },
                { key: 'gain', label: 'Verstärkung', min: -24, max: 24, step: 1, value: 12,
                  marks: [[-24, '−24'], [-12, '−12'], [0, '0'], [12, '+12'], [24, '+24']], show: signedDb },
                { key: 'rest', label: 'Übrige', min: -24, max: 0, step: 1, value: -12,
                  marks: [[-24, '−24'], [-12, '−12'], [0, '0']], show: signedDb },
            ],
            neutral: (p) => p.gain === 0 && p.rest === 0,   // in every form
            // settings to start from: the knobs move there and show how
            presets: [
                ['Altes Radio', { shape: 'bell', centre: 1000, width: 1.4, gain: 0, rest: -24 }],
                ['Telefon', { shape: 'bell', centre: 1000, width: 0.9, gain: 0, rest: -24 }],
                ['Party nebenan', { shape: 'low', centre: 250, width: 1, gain: 0, rest: -24 }],
            ],
            choices: [
                { key: 'shape', label: 'Form', value: 'bell', options: [['bell', 'Glocke'], ['high', 'Hochpass'], ['low', 'Tiefpass']] },
            ],
            caption: 'Lila Linie: wie viel dB das EQ jeder Frequenz gibt. Graue Linie: das Spektrum des Klangs ohne EQ, lila Fläche: mit EQ.',
            line: (p) => (p.shape === 'high' ? `Hochpass: unter ${hz(p.centre)} wird es leiser, bis ${signedDb(p.rest)}; darüber bleibt alles.`
                : p.shape === 'low' ? `Tiefpass: über ${hz(p.centre)} wird es leiser, bis ${signedDb(p.rest)}; darunter bleibt alles.`
                : p.rest === 0
                ? `Um ${hz(p.centre)} ${signedDb(p.gain)}, die Schwingung dort mal ${factor(10 ** (p.gain / 20))}; ${de(p.width, 1)} Oktaven daneben noch die Hälfte.`
                : `Um ${hz(p.centre)} ${signedDb(p.gain)}, weit weg ${signedDb(p.rest)}; ${de(p.width, 1)} Oktaven daneben genau dazwischen.`),
            draw: drawEq,
        },
        echo: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['stimme', 'schlagzeug', 'funkdrums', 'akustikgitarre', 'klavier', 'gesang'],
            knobs: [
                { key: 'delay', label: 'Verzögerung', min: 0.05, max: 1, step: 0.01, value: 0.3,
                  marks: [[0.1, '0,1'], [0.25, '0,25'], [0.5, '0,5'], [0.75, '0,75'], [1, '1 s']], show: (v) => `${de(v, 2)} s` },
                { key: 'feedback', label: 'Rückkopplung', min: 0, max: 0.9, step: 0.01, value: 0.5,
                  marks: [[0, '0'], [0.25, '0,25'], [0.5, '0,5'], [0.75, '0,75'], [0.9, '0,9']], show: (v) => de(v, 2) },
                { key: 'mix', label: 'Mischung', min: 0, max: 1, step: 0.01, value: 0.6,
                  marks: [[0, '0'], [0.5, '0,5'], [1, '1']], show: (v) => de(v, 2) },
            ],
            twice: true,
            neutral: (p) => p.mix === 0,
            caption: 'Was aus einem einzigen Knall wird: der Knall und seine Echos über drei Sekunden.',
            line: (p) => {
                const k = Math.floor(1 / p.delay + 1e-9);
                return `Ein Echo alle ${de(p.delay, 2)} s, jedes ${de(p.feedback, 2)}-mal so hoch wie das vorige; das letzte vor einer Sekunde, das ${k}., hat ${de(100 * p.mix * p.feedback ** k, 1)} % der Höhe des Knalls.`;
            },
            draw: drawEcho,
        },
        chorus: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['melodie', 'akustikgitarre', 'egitarre', 'gesang', 'stimme'],
            knobs: [
                { key: 'depth', label: 'Tiefe', min: 0, max: 0.01, step: 0.0005, value: 0.004,
                  marks: [[0, '0'], [0.0025, '2,5'], [0.005, '5'], [0.0075, '7,5'], [0.01, '10 ms']], show: (v) => `${de(v * 1000, 1)} ms` },
                { key: 'speed', label: 'Tempo', min: 0.1, max: 5, step: 0.1, value: 1,
                  marks: [[0.5, '0,5'], [1, '1'], [2, '2'], [3, '3'], [4, '4'], [5, '5 Hz']], show: (v) => `${de(v, 1)} Hz` },
                { key: 'mix', label: 'Mischung', min: 0, max: 1, step: 0.01, value: 0.7,
                  marks: [[0, '0'], [0.5, '0,5'], [1, '1']], show: (v) => de(v, 2) },
            ],
            twice: true,
            neutral: (p) => p.mix === 0,
            caption: 'Wie weit die Kopie hinter dem Klang herläuft, über zwei Sekunden: 15 Millisekunden und dazu die Tiefe, im Tempo hin und her.',
            line: (p) => `Die Kopie läuft zwischen ${de(15, 0)} und ${de(15 + p.depth * 1000, 1)} ms hinterher, ${de(p.speed, 1)}-mal in der Sekunde hin und zurück.`,
            draw: drawChorus,
        },
        reverb: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['schlagzeug', 'klavier', 'gesang', 'chor', 'stimme'],
            knobs: [
                { key: 'size', label: 'Raumgröße', min: 0.5, max: 2, step: 0.05, value: 1,
                  marks: [[0.5, '½'], [1, '1'], [1.5, '1½'], [2, '2']], show: (v) => `${de(v, 2)}-mal` },
                { key: 'time', label: 'Nachhall', min: 0.2, max: 5, step: 0.1, value: 1.5,
                  marks: [[0.5, '0,5'], [1, '1'], [2, '2'], [3, '3'], [4, '4'], [5, '5 s']], show: (v) => `${de(v, 1)} s` },
                { key: 'mix', label: 'Mischung', min: 0, max: 1, step: 0.01, value: 0.4,
                  marks: [[0, '0'], [0.5, '0,5'], [1, '1']], show: (v) => de(v, 2) },
            ],
            twice: true,
            neutral: (p) => p.mix === 0,
            caption: 'Was aus einem einzigen Knall wird: die vielen Echos des Halls über zwei Sekunden.',
            line: (p) => `Nach ${de(p.time, 1)} s ist der Hall um 60 dB leiser, ein Tausendstel der Höhe; die Echos kommen ${de(29.7 * p.size, 0)} bis ${de(43.7 * p.size, 0)} ms auseinander.`,
            draw: drawReverb,
        },
        pitch: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['stimme', 'gesang', 'klavier', 'veena', 'gamelan', 'musik'],
            knobs: [
                { key: 'semitones', label: 'Halbtöne', min: -12, max: 12, step: 1, value: 5,
                  marks: [[-12, '−12'], [-7, '−7'], [0, '0'], [7, '+7'], [12, '+12']],
                  show: (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)}` },
            ],
            live: true,
            neutral: (p) => p.semitones === 0,
            caption: 'Die Tonhöhe, vier Sekunden um die Stelle, die gerade spielt (Strich): grau ohne Effekt, lila mit. Zum Spulen ziehen.',
            line: (p) => `Jede Frequenz mal ${de(2 ** (p.semitones / 12), 3)}${p.semitones % 12 === 0 && p.semitones ? `, ${Math.abs(p.semitones / 12)} Oktave ${p.semitones > 0 ? 'höher' : 'tiefer'}` : ''}; der Klang bleibt so lang, wie er ist.`,
            draw: drawPitch,
        },
        autotune: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['kennedy', 'kennedyMelodie', 'kennedyBand'],
            plain: ['kennedyMelodie', 'kennedyBand'], // played as they are
            melody: 'kennedyMelodie',            // the target of »Melodie« …
            melodyFor: 'kennedy',                // … for this clip
            startOff: true,                      // first the voice as it is
            choices: [
                { key: 'target', label: 'Ziel', value: 'melody', options: [['melody', 'die Melodie'], ['note', 'der nächste Halbton']] },
            ],
            // other clips of the same length, mixed in when switched on
            switches: [{ key: 'withMelody', label: 'Melodie dazu', value: false, mix: 'kennedyMelodie', level: 0.6 },
                { key: 'withBand', label: 'Band dazu', value: false, mix: 'kennedyBand', level: 0.7 }],
            knobs: [
                { key: 'speed', label: 'Retune Speed', min: 0, max: 0.2, step: 0.005, value: 0,
                  marks: [[0, '0'], [0.05, '50'], [0.1, '100'], [0.15, '150'], [0.2, '200 ms']], show: ms },
                { key: 'transpose', label: 'Transpose', min: -12, max: 12, step: 1, value: 0,
                  marks: [[-12, '−12'], [-7, '−7'], [0, '0'], [7, '+7'], [12, '+12']],
                  show: (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)} Halbtöne` },
            ],
            live: true,
            neutral: () => false,
            caption: 'Die Tonhöhe, vier Sekunden um die Stelle, die gerade spielt (Strich): grau ohne Effekt, lila mit Autotune, dunkel die Melodie; die dünnen Linien sind die Halbtöne. Zum Spulen ziehen.',
            line: (p) => {
                const goal = p.target === 'melody' ? 'den Ton der Melodie' : 'den nächsten Halbton';
                return p.speed === 0
                    ? `Jeder Block springt sofort auf ${goal}: Die Stimme klingt wie eine Maschine.`
                    : `Jeder Block geht in etwa ${de(p.speed * 1000)} ms auf ${goal} zu.`;
            },
            draw: drawPitch,
        },
        vocoder: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['stimme', 'gesang'],
            choices: [
                { key: 'carrier', label: 'Träger', value: 'chord', options: [['chord', 'Akkord'], ['noise', 'Rauschen']] },
            ],
            knobs: [
                { key: 'bands', label: 'Bänder', min: 2, max: 32, step: 1, value: 16,
                  marks: [[2, '2'], [4, '4'], [8, '8'], [16, '16'], [24, '24'], [32, '32']], show: (v) => `${v}` },
            ],
            live: true,
            neutral: () => false,
            caption: 'Wie stark jedes Band gerade ist, von 100 Hz bis 8 kHz: grau in der Stimme, lila im Ergebnis, dem Träger mit der Stärke der Stimme.',
            line: (p) => `${p.bands} Bänder, jedes ${de(80 ** (1 / p.bands), 2)}-mal so breit wie das darunter; der Träger: ${{ chord: 'ein Akkord aus drei Sägezahnschwingungen', melody: 'eine Sägezahnschwingung auf den Tönen der Melodie', noise: 'Rauschen' }[p.carrier]}.`,
            draw: drawVocoder,
        },
        voices: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['stimme', 'kennedy'],
            knobs: [
                { key: 'semitones', label: 'Halbtöne', min: -12, max: 12, step: 1, value: 0,
                  marks: [[-12, '−12'], [-7, '−7'], [0, '0'], [7, '+7'], [12, '+12']],
                  show: (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)}` },
                { key: 'ring', label: 'Ringmodulator', min: 0, max: 200, step: 1, value: 30,
                  marks: [[0, 'aus'], [30, '30'], [50, '50'], [100, '100'], [150, '150'], [200, '200 Hz']],
                  show: (v) => (v ? `${v} Hz` : 'aus') },
                { key: 'drive', label: 'Verzerrung', min: 0, max: 1, step: 0.05, value: 0,
                  marks: [[0, 'aus'], [0.25, '¼'], [0.5, '½'], [0.75, '¾'], [1, 'ganz']],
                  show: (v) => (v ? `${Math.round(v * 100)} %` : 'aus') },
            ],
            presets: [
                ['Streifenhörnchen', { semitones: 12, ring: 0, drive: 0 }],
                ['Riese', { semitones: -8, ring: 0, drive: 0 }],
                ['Dalek', { semitones: 0, ring: 30, drive: 0 }],
                ['Darth Vader', { semitones: -4, ring: 0, drive: 0 }],
                ['Grievous', { semitones: -2, ring: 0, drive: 0.7 }],
            ],
            live: true,
            neutral: (p) => p.semitones === 0 && p.ring === 0 && p.drive === 0,
            caption: 'Vierzig Millisekunden der Welle an der Stelle, die gerade spielt: grau ohne Effekt, lila mit, gestrichelt der Sinus des Ringmodulators.',
            line: (p) => {
                const parts = [];
                if (p.semitones) parts.push(`${Math.abs(p.semitones)} Halbtöne ${p.semitones > 0 ? 'höher' : 'tiefer'}`);
                if (p.ring) parts.push(`mal einer Sinusschwingung von ${p.ring} Hz: ein Ton von 150 Hz wird zu ${150 - p.ring} und ${150 + p.ring} Hz`);
                if (p.drive) parts.push(`verzerrt: Was laut ist, wird platt gedrückt, und es kratzt`);
                return parts.length ? `Die Stimme ${parts.join(', dann ')}.` : 'Die Stimme, wie sie ist.';
            },
            draw: drawVoices,
        },
        compressor: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['funkdrums', 'schlagzeug', 'dynamik', 'stimme', 'rock'],
            knobs: [
                { key: 'threshold', label: 'Threshold', min: -48, max: 0, step: 1, value: -24,
                  marks: [[-48, '−48'], [-36, '−36'], [-24, '−24'], [-12, '−12'], [0, '0 dB']], show: signedDb },
                { key: 'ratio', label: 'Ratio', min: 0, max: 1000, step: 1, value: RATIO.from(4), scale: RATIO,
                  marks: [[1, '1'], [2, '2'], [4, '4'], [8, '8'], [20, '20']],
                  show: (v) => `${v.toLocaleString('de-DE', { maximumFractionDigits: 1 })} : 1` },
                { key: 'attack', label: 'Attack', min: 0, max: 1000, step: 1, value: ATTACK.from(0.005), scale: ATTACK,
                  marks: [[0.001, '1'], [0.003, '3'], [0.01, '10'], [0.03, '30'], [0.1, '100 ms']], show: ms },
                { key: 'release', label: 'Release', min: 0, max: 1000, step: 1, value: RELEASE.from(0.1), scale: RELEASE,
                  marks: [[0.02, '20'], [0.05, '50'], [0.1, '100'], [0.2, '200'], [0.5, '500'], [1, '1000 ms']], show: ms },
                { key: 'makeup', label: 'Make-up Gain', min: 0, max: 24, step: 1, value: 9,
                  marks: [[0, '0'], [6, '+6'], [12, '+12'], [18, '+18'], [24, '+24 dB']], show: signedDb },
            ],
            twice: true,
            live: true,
            neutral: (p) => p.ratio === 1 && p.makeup === 0,
            caption: 'Links: wie laut ein Klang herauskommt, lila mit Compressor, gestrichelt ohne; der Punkt ist der Pegel, der gerade spielt. Rechts: wie viele dB der Compressor gibt, vier Sekunden um die Stelle, die gerade spielt (Strich).',
            line: (p) => {
                const one = (v) => v.toLocaleString('de-DE', { maximumFractionDigits: 1 });
                return `Was über ${signedDb(p.threshold)} liegt, wird ${one(p.ratio)} : 1 gestaucht: 12 dB darüber kommen als ${one(12 / p.ratio)} dB darüber heraus; danach alles ${signedDb(p.makeup)}.`;
            },
            draw: drawCompressor,
        },
    };

    // Schroeder's reverberator, the same in the worker and for the picture:
    // four comb filters side by side (delays in ms, times the room's size),
    // each fading by 60 dB in time seconds, then two all-pass filters
    const COMBS = [29.7, 37.1, 41.1, 43.7], ALLPASS = [[5.0, 0.7], [1.7, 0.7]];
    function reverb(x, rate, p) {
        const wet = new Float32Array(x.length);
        for (const ms of COMBS) {
            const D = Math.max(1, Math.round(ms / 1000 * p.size * rate));
            const g = 10 ** (-3 * D / rate / p.time);
            const y = Float32Array.from(x);
            for (let n = D; n < y.length; n++) y[n] += g * y[n - D];
            for (let n = 0; n < y.length; n++) wet[n] += y[n] / COMBS.length;
        }
        let z = wet;
        for (const [ms, g] of ALLPASS) {
            const D = Math.max(1, Math.round(ms / 1000 * rate));
            const y = new Float32Array(z.length);
            for (let n = 0; n < z.length; n++) y[n] = -g * z[n] + (n >= D ? z[n - D] + g * y[n - D] : 0);
            z = y;
        }
        const out = new Float32Array(x.length);
        for (let n = 0; n < x.length; n++) out[n] = x[n] + p.mix * z[n];
        return out;
    }
    // the chorus: a copy read 15 ms plus a wobbling depth behind, between samples
    // the level follows |x| (attack when it rises, release when it falls);
    // each sample gets the gain of gainDb and the makeup
    function compressor(x, rate, p) {
        const a = Math.exp(-1 / (p.attack * rate)), r = Math.exp(-1 / (p.release * rate));
        const out = new Float32Array(x.length);
        let level = 0;
        for (let n = 0; n < x.length; n++) {
            const v = Math.abs(x[n]), c = v > level ? a : r;
            level = c * level + (1 - c) * v;
            const db = 20 * Math.log10(Math.max(level, 1e-9));
            out[n] = x[n] * 10 ** ((gainDb(db, p.threshold, p.ratio) + p.makeup) / 20);
        }
        return out;
    }
    // the pitch of the voice, as pitch() and pitches() of the page: per HOP
    // samples a block of SIZE; quiet blocks 0, and blocks that do not fit
    // themselves one period later (applause, hisses). Here on a quarter of the rate
    // (each four samples averaged) and with the lag refined between two
    // samples, so a whole clip takes a fraction of a second
    const SIZE = 2048, HOP = 512;
    function track(x, rate) {
        const q = 4, r = rate / q, n = Math.floor(x.length / q), d = new Float32Array(n);
        for (let i = 0; i < n; i++) d[i] = (x[q * i] + x[q * i + 1] + x[q * i + 2] + x[q * i + 3]) / q;
        const size = SIZE / q, hop = HOP / q, maxLag = Math.floor(r / 60);
        const out = [], rs = new Float64Array(maxLag + 2);
        for (let s = 0; s + size <= n; s += hop) {
            let e = 0;
            for (let k = 0; k < size; k++) e += d[s + k] * d[s + k];
            if (Math.sqrt(e / size) < 0.02) { out.push(0); continue; }
            let best = 0, bestLag = 0, dipped = false;
            for (let lag = 1; lag <= maxLag + 1; lag++) {
                let acc = 0;
                for (let k = 0; k < size - lag; k++) acc += d[s + k] * d[s + k + lag];
                rs[lag] = acc;
                if (lag > maxLag) break;
                if (acc < 0) dipped = true;
                else if (dipped && acc > best) { best = acc; bestLag = lag; }
            }
            // a voice fits itself one period later; applause and hisses do not
            if (!bestLag || best < 0.5 * e) { out.push(0); continue; }
            const a = rs[bestLag - 1], b = rs[bestLag], c = rs[bestLag + 1];
            const bend = a - 2 * b + c, lag = bendLag(bestLag, a, c, bend);
            out.push(r / lag);
        }
        return Float32Array.from(out);
    }
    function bendLag(lag, a, c, bend) {
        return bend < 0 ? lag + 0.5 * (a - c) / bend : lag;
    }
    // per block the factor of autotune: the nearest semitone of the pitch
    // (moved by transpose), approached in the time speed
    // (or, with the target melody, to the melody's pitch in that block)
    function tuneFactors(found, rate, p, melody) {
        const out = new Float32Array(found.length), move = 2 ** (p.transpose / 12);
        const step = p.speed > 0 ? 1 - Math.exp(-HOP / rate / p.speed) : 1;
        const toMelody = p.target === 'melody' && melody;
        let now = move;
        for (let k = 0; k < found.length; k++) {
            const f = found[k], voiced = f > 60 && f < 1000;
            let want = move;
            if (voiced && !toMelody) want = 440 * 2 ** (Math.round(12 * Math.log2(f * move / 440)) / 12) / f;
            else if (voiced && melody[k] > 0) want = melody[k] * move / f;
            now += (want - now) * step;
            out[k] = now;
        }
        return out;
    }
    // the grains of shift() of the page: around every quarter grain a piece
    // read factor-times as fast, moved by up to one period until it fits what
    // is already there (first every fourth place, then exactly), windowed and
    // added; around the end of the clip back to its start, so the loop is seamless
    function shift(x, rate, factorAt, grain) {
        const L = x.length, G = Math.round(grain * rate), D = Math.floor(rate / 60), H = G >> 2, half = G >> 1;
        const win = new Float32Array(G);
        for (let i = 0; i < G; i++) win[i] = 0.5 - 0.5 * Math.cos(2 * Math.PI * (i + 0.5) / G);
        const y = new Float32Array(L), wsum = new Float32Array(L);
        const piece = new Float32Array(G + 2 * D), here = new Float32Array(G);
        for (let c = 0; c < L; c += H) {
            const f = factorAt(c);
            for (let i = 0; i < piece.length; i++) {
                const pos = c + (i - half - D) * f, p = ((pos % L) + L) % L, i0 = Math.floor(p), fr = p - i0;
                piece[i] = x[i0] * (1 - fr) + x[(i0 + 1) % L] * fr;
            }
            let energy = 0;
            for (let k = 0; k < G; k++) { here[k] = y[((c - half + k) % L + L) % L]; energy += here[k] * here[k]; }
            let at = D;
            if (energy > 0) {
                const corr = (s, step) => { let acc = 0; for (let k = 0; k < G; k += step) acc += piece[s + k] * here[k]; return acc; };
                let best = -Infinity;
                for (let s = 0; s <= 2 * D; s += 4) { const v = corr(s, 8); if (v > best) { best = v; at = s; } }
                const from = Math.max(0, at - 3), to = Math.min(2 * D, at + 3);
                best = -Infinity;
                for (let s = from; s <= to; s++) { const v = corr(s, 1); if (v > best) { best = v; at = s; } }
            }
            for (let k = 0; k < G; k++) {
                const o = ((c - half + k) % L + L) % L;
                y[o] += win[k] * piece[at + k];
                wsum[o] += win[k];
            }
        }
        for (let i = 0; i < L; i++) y[i] /= Math.max(wsum[i], 1e-3);
        return y;
    }
    // the vocoder of the page: per block of SIZE (every HOP, around the end
    // of the clip back to its start) the carrier's spectrum, in each band as
    // strong as the voice's; the bands from 100 to 8000 Hz, each the same
    // number of times wider than the one below
    function vocoder(x, carrier, rate, bands) {
        const L = x.length, N = SIZE, half = N / 2;
        const edges = [];
        for (let b = 0; b <= bands; b++) edges.push(Math.round(100 * 80 ** (b / bands) * N / rate));
        const win = new Float64Array(N);
        for (let i = 0; i < N; i++) win[i] = 0.5 - 0.5 * Math.cos(2 * Math.PI * i / N);
        const y = new Float32Array(L);
        const vr = new Float64Array(N), vi = new Float64Array(N), cr = new Float64Array(N), ci = new Float64Array(N);
        for (let start = 0; start < L; start += HOP) {
            for (let i = 0; i < N; i++) {
                const j = (start + i) % L;
                vr[i] = x[j] * win[i]; vi[i] = 0;
                cr[i] = carrier[j] * win[i]; ci[i] = 0;
            }
            fft(vr, vi, false); fft(cr, ci, false);
            const scale = new Float64Array(half + 1);
            for (let b = 0; b < bands; b++) {
                let v = 0, c = 0;
                for (let k = edges[b]; k < edges[b + 1]; k++) { v += vr[k] * vr[k] + vi[k] * vi[k]; c += cr[k] * cr[k] + ci[k] * ci[k]; }
                const f = c > 0 ? Math.sqrt(v / c) : 0;
                for (let k = edges[b]; k < edges[b + 1]; k++) scale[k] = f;
            }
            for (let k = 0; k <= half; k++) {
                cr[k] *= scale[k]; ci[k] *= scale[k];
                if (k > 0 && k < half) { cr[N - k] = cr[k]; ci[N - k] = -ci[k]; }
            }
            fft(cr, ci, true);
            for (let i = 0; i < N; i++) y[(start + i) % L] += cr[i] / N * win[i] / 1.5;
        }
        return y;
    }
    // the carriers: a chord of saw waves (C, E, G), a saw wave on the
    // melody's notes (each held until the next), noise
    function carrierFor(kind, n, rate, melody) {
        const out = new Float32Array(n);
        if (kind === 'noise') { for (let i = 0; i < n; i++) out[i] = Math.random() * 2 - 1; return out; }
        const freqs = kind === 'melody' && melody ? null : [130.8, 164.8, 196.0];
        const phase = [0, 0, 0];
        let held = 130.8;
        for (let i = 0; i < n; i++) {
            if (freqs) {
                let sum = 0;
                for (let k = 0; k < 3; k++) { phase[k] = (phase[k] + freqs[k] / rate) % 1; sum += 2 * phase[k] - 1; }
                out[i] = sum / 3;
            } else {
                const f = melody[Math.min(melody.length - 1, Math.max(0, Math.floor((i - SIZE / 2) / HOP)))];
                if (f > 60 && f < 1000) held = f;
                phase[0] = (phase[0] + held / rate) % 1;
                out[i] = 2 * phase[0] - 1;
            }
        }
        return out;
    }
    const blockAt = (c, n) => Math.min(n - 1, Math.max(0, Math.floor((c - SIZE / 2) / HOP)));
    function chorus(x, rate, p) {
        const out = new Float32Array(x.length);
        for (let n = 0; n < x.length; n++) {
            const delay = (0.015 + p.depth * (1 + Math.sin(2 * Math.PI * p.speed * n / rate)) / 2) * rate;
            const pos = n - delay;
            let wet = 0;
            if (pos >= 0) {
                const i = Math.floor(pos), f = pos - i;
                wet = (1 - f) * x[i] + f * x[Math.min(i + 1, x.length - 1)];
            }
            out[n] = x[n] + p.mix * wet;
        }
        return out;
    }

    // ------------------------------------------------------------------
    // the worker: one function per effect, as the Python of the page
    const WORKER = `'use strict';
const bellDb = ${bellDb.toString()};
const eqDb = ${eqDb.toString()};
${fft.toString()}
const clips = {};        // id: { x, rate, spectrum }
function eq(clip, p) {
    if (!clip.spectrum) {
        const n = clip.x.length, size = 1 << Math.ceil(Math.log2(n * 1.25)), lead = (size - n) >> 1;
        const re = new Float64Array(size), im = new Float64Array(size);
        for (let i = 0; i < size; i++) re[i] = clip.x[((i - lead) % n + n) % n];
        fft(re, im, false);
        clip.spectrum = { re, im, size, lead };
    }
    const { re: re0, im: im0, size, lead } = clip.spectrum;
    const re = new Float64Array(size), im = new Float64Array(size);
    for (let k = 0; k <= size / 2; k++) {
        const factor = 10 ** (eqDb(k * clip.rate / size, p) / 20);
        re[k] = re0[k] * factor; im[k] = im0[k] * factor;
        if (k > 0 && k < size / 2) { re[size - k] = re[k]; im[size - k] = -im[k]; }
    }
    fft(re, im, true);
    const out = new Float32Array(clip.x.length);
    for (let i = 0; i < out.length; i++) out[i] = re[lead + i] / size;
    return out;
}
// y[n] = x[n] + feedback * y[n - D]; out = x + mix * (y - x)
function echo(x, rate, p) {
    const D = Math.max(1, Math.round(p.delay * rate));
    const y = Float32Array.from(x);
    for (let n = D; n < y.length; n++) y[n] += p.feedback * y[n - D];
    const out = new Float32Array(x.length);
    for (let n = 0; n < x.length; n++) out[n] = x[n] + p.mix * (y[n] - x[n]);
    return out;
}
${reverb.toString()}
${chorus.toString()}
${compressor.toString()}
${track.toString()}
${bendLag.toString()}
${tuneFactors.toString()}
${shift.toString()}
${vocoder.toString()}
${carrierFor.toString()}
const SIZE = ${SIZE}, HOP = ${HOP};
const blockAt = ${blockAt.toString()};
const gainDb = ${gainDb.toString()};
const COMBS = ${JSON.stringify(COMBS)}, ALLPASS = ${JSON.stringify(ALLPASS)};
const gain = (x, rate, p) => { const f = 10 ** (p.db / 20); return x.map((v) => v * f); };
const EFFECTS = { echo, chorus, reverb, compressor, gain };
onmessage = (e) => {
    const m = e.data;
    if (m.load) { clips[m.load] = { x: m.x, rate: m.rate }; return; }
    const clip = clips[m.clip];
    let out;
    let found = null;
    if (!m.on) {
        out = Float32Array.from(clip.x);
        if (m.effect === 'pitch' || m.effect === 'autotune') {
            if (!clip.track) clip.track = track(clip.x, clip.rate);
            found = clip.track;
        }
    } else if (m.effect === 'vocoder') {
        let melody = null;
        if (m.params.carrier === 'melody' && clips[m.melody]) {
            const mc = clips[m.melody];
            if (!mc.track) mc.track = track(mc.x, mc.rate);
            melody = mc.track;
        }
        out = vocoder(clip.x, carrierFor(m.params.carrier, clip.x.length, clip.rate, melody), clip.rate, m.params.bands);
    } else if (m.effect === 'voices') {
        out = m.params.semitones ? shift(clip.x, clip.rate, () => 2 ** (m.params.semitones / 12), 0.05) : Float32Array.from(clip.x);
        if (m.params.ring) for (let n = 0; n < out.length; n++) out[n] *= Math.sin(2 * Math.PI * m.params.ring * n / clip.rate);
        // distortion: the wave through tanh, loud parts flattened (a gravelly voice)
        if (m.params.drive) {
            let peak = 1e-6;
            for (let n = 0; n < out.length; n++) peak = Math.max(peak, Math.abs(out[n]));
            const k = 1 + 30 * m.params.drive, norm = Math.tanh(k);
            for (let n = 0; n < out.length; n++) out[n] = Math.tanh(k * out[n] / peak) / norm * peak;
        }
    } else if (m.effect === 'pitch' || m.effect === 'autotune') {
        if (!clip.track) clip.track = track(clip.x, clip.rate);
        found = clip.track;
        const n = found.length;
        let melody = null;
        if (m.effect === 'autotune' && clips[m.melody]) {
            const mc = clips[m.melody];
            if (!mc.track) mc.track = track(mc.x, mc.rate);
            melody = mc;
        }
        const factors = m.effect === 'autotune' ? tuneFactors(found, clip.rate, m.params, melody && melody.track) : null;
        const constant = 2 ** ((m.params.semitones || 0) / 12);
        out = shift(clip.x, clip.rate, factors ? (c) => factors[blockAt(c, n)] : () => constant, 0.05);
    } else if (m.effect === 'eq') out = eq(clip, m.params);
    else if (m.twice) {
        // the sound twice in a row; the second time, with what the first leaves over
        const n = clip.x.length, x2 = new Float32Array(2 * n);
        x2.set(clip.x); x2.set(clip.x, n);
        out = EFFECTS[m.effect](x2, clip.rate, m.params).slice(n);
    } else out = EFFECTS[m.effect](clip.x, clip.rate, m.params);
    for (const [id, level] of m.mixes || []) {
        const other = clips[id] && clips[id].x;
        if (other) for (let i = 0; i < out.length; i++) out[i] += level * other[i % other.length];
    }
    if (m.cut) {
        // as a converter does: what is above full scale is cut off
        for (let i = 0; i < out.length; i++) out[i] = Math.max(-1, Math.min(1, out[i]));
        postMessage({ out, id: m.id, track: null, melodyTrack: null });
        return;
    }
    // never louder than full scale: quieter as a whole instead of clipped
    let peak = 0;
    for (let i = 0; i < out.length; i++) peak = Math.max(peak, Math.abs(out[i]));
    if (peak > 0.99) for (let i = 0; i < out.length; i++) out[i] *= 0.99 / peak;
    const melodyTrack = m.melody && clips[m.melody] && clips[m.melody].track;
    postMessage({ out, id: m.id, track: found ? Float32Array.from(found) : null, melodyTrack: melodyTrack ? Float32Array.from(melodyTrack) : null });
};`;

    // an FFT in place, iterative: bit reversal, then the butterflies;
    // inverse: with the conjugate factors (the caller divides by n)
    function fft(re, im, inverse) {
        const n = re.length;
        for (let i = 1, j = 0; i < n; i++) {
            let bit = n >> 1;
            for (; j & bit; bit >>= 1) j ^= bit;
            j ^= bit;
            if (i < j) { let t = re[i]; re[i] = re[j]; re[j] = t; t = im[i]; im[i] = im[j]; im[j] = t; }
        }
        for (let len = 2; len <= n; len <<= 1) {
            const angle = (inverse ? 2 : -2) * Math.PI / len;
            const wr = Math.cos(angle), wi = Math.sin(angle);
            for (let i = 0; i < n; i += len) {
                let cr = 1, ci = 0;
                for (let k = 0; k < len / 2; k++) {
                    const a = i + k, b = a + len / 2;
                    const tr = re[b] * cr - im[b] * ci, ti = re[b] * ci + im[b] * cr;
                    re[b] = re[a] - tr; im[b] = im[a] - ti;
                    re[a] += tr; im[a] += ti;
                    const nr = cr * wr - ci * wi;
                    ci = cr * wi + ci * wr; cr = nr;
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // the pictures: each gets the canvas context, its size, the colours,
    // the knobs and what is playing (for the EQ both spectra)
    function frame(g, w, h, col) {
        g.fillStyle = col.ground;
        g.fillRect(LABEL_W, 0, w - LABEL_W, h - AXIS);
        g.font = '12px system-ui, sans-serif';
    }
    function drawEq(g, w, h, col, p, on, live) {
        frame(g, w, h, col);
        const F0 = 40, F1 = 20000;
        const plotW = w - LABEL_W, plotH = h - AXIS;
        const xOf = (f) => LABEL_W + Math.log(f / F0) / Math.log(F1 / F0) * plotW;
        const yOf = (db) => plotH / 2 - (db / 24) * (plotH / 2 - 6);
        g.textAlign = 'right'; g.textBaseline = 'middle';
        for (const db of [24, 12, 0, -12, -24]) {
            const y = Math.round(yOf(db)) + 0.5;
            g.strokeStyle = col.quiet; g.lineWidth = db === 0 ? 1.5 : 1;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            g.fillStyle = col.ink;
            g.fillText(signedDb(db), LABEL_W - 6, Math.min(plotH - 6, Math.max(6, y)));
        }
        g.textAlign = 'center'; g.textBaseline = 'bottom';
        for (const f of plotW < 420 ? [100, 1000, 10000] : [50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000]) {
            g.fillText(f >= 1000 ? `${de(f / 1000)} kHz` : `${f} Hz`, Math.min(w - 22, Math.max(LABEL_W + 20, xOf(f))), h);
        }
        if (live) {
            // the spectrum where the sound is: lila filled with the effect, a grey line without
            const path = (bins) => {
                g.beginPath();
                g.moveTo(LABEL_W, plotH);
                for (let px = 0; px <= plotW; px += 2) {
                    const f = F0 * (F1 / F0) ** (px / plotW);
                    const db = bins[Math.min(bins.length - 1, Math.round(f / live.binHz))];
                    g.lineTo(LABEL_W + px, plotH - Math.max(0, Math.min(1, (db + 90) / 90)) * plotH);
                }
                g.lineTo(LABEL_W + plotW, plotH);
            };
            path(live.after);
            g.closePath();
            g.fillStyle = on ? col.mark : col.sound; g.globalAlpha = 0.25; g.fill(); g.globalAlpha = 1;
            path(live.before);
            g.strokeStyle = col.sound; g.lineWidth = 1.5; g.stroke();
        }
        g.beginPath();
        for (let px = 0; px <= plotW; px++) {
            const f = F0 * (F1 / F0) ** (px / plotW);
            const y = yOf(on ? eqDb(f, p) : 0);
            if (px === 0) g.moveTo(LABEL_W, y); else g.lineTo(LABEL_W + px, y);
        }
        g.strokeStyle = col.mark; g.lineWidth = 2.5; g.stroke();
    }
    function drawEcho(g, w, h, col, p, on) {
        frame(g, w, h, col);
        const T = 3, plotW = w - LABEL_W, plotH = h - AXIS;
        const xOf = (t) => LABEL_W + t / T * plotW;
        const yOf = (a) => plotH - a * (plotH - 10);
        g.textAlign = 'center'; g.textBaseline = 'bottom'; g.fillStyle = col.ink;
        for (let t = 0; t <= T; t += 0.5) g.fillText(`${de(t, t % 1 ? 1 : 0)} s`, Math.min(w - 14, Math.max(LABEL_W + 10, xOf(t))), h);
        g.textAlign = 'right'; g.textBaseline = 'middle';
        for (const a of [0, 0.5, 1]) {
            const y = Math.round(yOf(a)) + 0.5;
            g.strokeStyle = col.quiet; g.lineWidth = 1;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            g.fillText(de(a, a % 1 ? 1 : 0), LABEL_W - 6, y);
        }
        const spike = (t, a, colour) => {
            g.strokeStyle = colour; g.lineWidth = 3;
            g.beginPath(); g.moveTo(xOf(t), yOf(0)); g.lineTo(xOf(t), yOf(a)); g.stroke();
        };
        spike(0, 1, col.ink);
        if (!on) return;
        // the k-th echo: mix · feedback^k (y[n] = x[n] + feedback · y[n − D])
        for (let k = 1; k * p.delay <= T; k++) spike(k * p.delay, p.mix * p.feedback ** k, col.mark);
    }

    function timeAxis(g, w, h, col, T, step) {
        const xOf = (t) => LABEL_W + t / T * (w - LABEL_W);
        g.textAlign = 'center'; g.textBaseline = 'bottom'; g.fillStyle = col.ink;
        for (let t = 0; t <= T + 1e-9; t += step) g.fillText(`${de(t, t % 1 ? 1 : 0)} s`, Math.min(w - 14, Math.max(LABEL_W + 10, xOf(t))), h);
        return xOf;
    }
    function drawChorus(g, w, h, col, p, on) {
        frame(g, w, h, col);
        const T = 2, plotH = h - AXIS;
        const xOf = timeAxis(g, w, h, col, T, 0.5);
        const yOf = (ms) => plotH - (ms / 26) * (plotH - 8);
        g.textAlign = 'right'; g.textBaseline = 'middle';
        for (const ms of [0, 10, 20]) {
            const y = Math.round(yOf(ms)) + 0.5;
            g.strokeStyle = col.quiet; g.lineWidth = 1;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            g.fillStyle = col.ink; g.fillText(`${ms} ms`, LABEL_W - 6, y);
        }
        if (!on) return;
        g.beginPath();
        for (let px = 0; px <= w - LABEL_W; px++) {
            const t = px / (w - LABEL_W) * T;
            const ms = 15 + p.depth * 1000 * (1 + Math.sin(2 * Math.PI * p.speed * t)) / 2;
            if (px === 0) g.moveTo(LABEL_W, yOf(ms)); else g.lineTo(LABEL_W + px, yOf(ms));
        }
        g.strokeStyle = col.mark; g.lineWidth = 2.5; g.stroke();
    }
    let reverbCache = null;
    function drawReverb(g, w, h, col, p, on) {
        frame(g, w, h, col);
        const T = 2, plotH = h - AXIS, rate = 8000;
        const xOf = timeAxis(g, w, h, col, T, 0.5);
        const yOf = (a) => plotH - a * (plotH - 10);
        g.strokeStyle = col.quiet; g.lineWidth = 1;
        g.beginPath(); g.moveTo(LABEL_W, Math.round(yOf(0)) + 0.5); g.lineTo(w, Math.round(yOf(0)) + 0.5); g.stroke();
        g.strokeStyle = col.ink; g.lineWidth = 3;
        g.beginPath(); g.moveTo(xOf(0), yOf(0)); g.lineTo(xOf(0), yOf(1)); g.stroke();
        if (!on) return;
        // the answer to a bang, at a low rate for the picture: per pixel the largest
        const key = `${p.size}|${p.time}`;
        if (!reverbCache || reverbCache.key !== key) {
            const bang = new Float32Array(T * rate); bang[0] = 1;
            const y = reverb(bang, rate, { size: p.size, time: p.time, mix: 1 });
            y[0] = 0;
            reverbCache = { key, y };
        }
        const y = reverbCache.y, cols = w - LABEL_W;
        let top = 0;
        for (let i = 1; i < y.length; i++) top = Math.max(top, Math.abs(y[i]));
        g.strokeStyle = col.mark; g.lineWidth = 1;
        g.beginPath();
        for (let px = 0; px < cols; px++) {
            const a = Math.floor(px / cols * y.length), b = Math.floor((px + 1) / cols * y.length);
            let m = 0;
            for (let i = a; i < b; i++) m = Math.max(m, Math.abs(y[i]));
            if (m > 0) { g.moveTo(LABEL_W + px + 0.5, yOf(0)); g.lineTo(LABEL_W + px + 0.5, yOf(p.mix * m / top * 0.8)); }
        }
        g.stroke();
    }

    // forty milliseconds of the wave where the sound is: grey without, lila
    // with, the ring modulator's sine dashed
    function drawVoices(g, w, h, col, p, on, live) {
        frame(g, w, h, col);
        const plotW = w - LABEL_W, plotH = h - AXIS, mid = plotH / 2, T = 0.04;
        g.strokeStyle = col.quiet; g.lineWidth = 1;
        g.beginPath(); g.moveTo(LABEL_W, mid + 0.5); g.lineTo(w, mid + 0.5); g.stroke();
        g.fillStyle = col.ink; g.textAlign = 'center'; g.textBaseline = 'bottom';
        for (const ms of [0, 10, 20, 30, 40]) g.fillText(`${ms} ms`, Math.min(w - 20, Math.max(LABEL_W + 16, LABEL_W + ms / 40 * plotW)), h);
        if (!live || !live.before) {
            g.textBaseline = 'middle';
            g.fillText('Spiel den Klang ab, dann erscheint hier die Welle.', LABEL_W + plotW / 2, plotH / 4);
            return;
        }
        const { before, after, at, rate } = live, L = before.length, n = Math.round(T * rate);
        let top = 1e-6;
        for (let i = 0; i < n; i++) top = Math.max(top, Math.abs(before[(at + i) % L]), Math.abs(after[(at + i) % L]));
        const curve = (value, colour, width, dash) => {
            g.setLineDash(dash || []); g.strokeStyle = colour; g.lineWidth = width;
            g.beginPath();
            for (let px = 0; px <= plotW; px++) {
                const i = Math.floor(px / plotW * n), y = mid - value(i) * (plotH / 2 - 6);
                if (px === 0) g.moveTo(LABEL_W, y); else g.lineTo(LABEL_W + px, y);
            }
            g.stroke(); g.setLineDash([]);
        };
        if (on && p.ring) curve((i) => Math.sin(2 * Math.PI * p.ring * ((at + i) % L) / rate), col.ink, 1, [4, 4]);
        curve((i) => before[(at + i) % L] / top, col.sound, 1.5);
        if (on) curve((i) => after[(at + i) % L] / top, col.mark, 2);
    }

    // dB above, the factor below, on one ruler: the factor 10^(dB/20)
    function drawGain(g, w, h, col, p, on) {
        const D0 = -40, D1 = 12, left = LABEL_W + 10, right = w - 24;
        const xOf = (db) => left + (db - D0) / (D1 - D0) * (right - left);
        const top = 34, bottom = h - 46;
        g.font = '12px system-ui, sans-serif';
        g.strokeStyle = col.ink; g.lineWidth = 1.5;
        for (const y of [top, bottom]) { g.beginPath(); g.moveTo(left, y + 0.5); g.lineTo(right, y + 0.5); g.stroke(); }
        g.fillStyle = col.ink; g.textAlign = 'right'; g.textBaseline = 'middle';
        g.fillText('dB', LABEL_W - 6, top); g.fillText('Faktor', LABEL_W - 6, bottom);
        g.textAlign = 'center';
        g.lineWidth = 1;
        for (const db of [-40, -30, -20, -12, -6, 0, 6, 12]) {
            const x = Math.round(xOf(db)) + 0.5;
            g.beginPath(); g.moveTo(x, top - 6); g.lineTo(x, top); g.stroke();
            g.textBaseline = 'bottom'; g.fillText(db > 0 ? `+${db}` : db < 0 ? `−${-db}` : '0', x, top - 8);
        }
        for (const [f, text] of [[0.01, '0,01'], [0.03, '0,03'], [0.1, '0,1'], [0.25, '¼'], [0.5, '½'], [1, '1'], [2, '2'], [4, '4']]) {
            const x = Math.round(xOf(20 * Math.log10(f))) + 0.5;
            g.beginPath(); g.moveTo(x, bottom); g.lineTo(x, bottom + 6); g.stroke();
            g.textBaseline = 'top'; g.fillText(text, x, bottom + 8);
        }
        // between the rulers: a line from each dB down to its factor, faint
        g.strokeStyle = col.quiet;
        for (const db of [-40, -20, -6, 0, 6, 12]) { const x = Math.round(xOf(db)) + 0.5; g.beginPath(); g.moveTo(x, top); g.lineTo(x, bottom); g.stroke(); }
        // full scale: above 0 dB a loud sound can be cut off
        g.fillStyle = col.ground; g.fillRect(xOf(0), top + 1, right - xOf(0), bottom - top - 1);
        g.fillStyle = col.ink; g.textBaseline = 'middle'; g.textAlign = 'center';
        g.fillText('kann übersteuern', (xOf(0) + right) / 2, (top + bottom) / 2);
        const db = on ? p.db : 0, x = xOf(db);
        g.strokeStyle = col.mark; g.lineWidth = 3;
        g.beginPath(); g.moveTo(x, top - 4); g.lineTo(x, bottom + 4); g.stroke();
        g.fillStyle = col.mark;
        for (const y of [top, bottom]) { g.beginPath(); g.arc(x, y, 5, 0, 2 * Math.PI); g.fill(); }
        const f = 10 ** (db / 20);
        g.textBaseline = 'top'; g.font = '600 13px system-ui, sans-serif';
        g.fillText(`mal ${factor(f)}`, Math.min(right - 30, Math.max(left + 30, x)), bottom + 26);
    }

    // the pitch over four seconds: grey the voice, lila with the effect
    function drawPitch(g, w, h, col, p, on, live) {
        frame(g, w, h, col);
        const F0 = 60, F1 = 600, plotW = w - LABEL_W, plotH = h - AXIS;
        const yOf = (f) => plotH - 4 - Math.log(f / F0) / Math.log(F1 / F0) * (plotH - 8);
        if (p.speed !== undefined) {
            g.strokeStyle = col.quiet; g.lineWidth = 0.5;
            for (let k = Math.ceil(12 * Math.log2(F0 / 440)); 440 * 2 ** (k / 12) < F1; k++) {
                const y = Math.round(yOf(440 * 2 ** (k / 12))) + 0.5;
                g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            }
        }
        g.textAlign = 'right'; g.textBaseline = 'middle'; g.fillStyle = col.ink; g.strokeStyle = col.quiet; g.lineWidth = 1;
        for (const f of [100, 200, 400]) {
            const y = Math.round(yOf(f)) + 0.5;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            g.fillText(`${f} Hz`, LABEL_W - 4, y);
        }
        g.textAlign = 'center'; g.textBaseline = 'bottom';
        g.fillText('−2 s', LABEL_W + 14, h); g.fillText('jetzt', LABEL_W + plotW / 2, h); g.fillText('+2 s', w - 14, h);
        if (!live) {
            g.textBaseline = 'middle';
            g.fillText('Spiel den Klang ab, dann erscheint hier seine Tonhöhe.', LABEL_W + plotW / 2, plotH / 2);
            return;
        }
        const { found, factors, at, rate } = live, n = found.length, span = 4 * rate, L = live.length;
        const xOfBlock = (k, from) => {
            let c = k * HOP + SIZE / 2 - from;
            c = ((c % L) + L) % L;
            return LABEL_W + c / span * plotW;
        };
        const from = at - span / 2;
        const dots = (value, colour, r) => {
            g.fillStyle = colour;
            for (let k = 0; k < n; k++) {
                const f = value(k);
                if (!(found[k] > 60 && found[k] < 1000) || f < F0 || f > F1) continue;
                const x = xOfBlock(k, from);
                if (x < LABEL_W || x > w) continue;
                g.fillRect(x - r, yOf(f) - r, 2 * r, 2 * r);
            }
        };
        if (live.melody && !live.plain) {
            // the melody as short lines: the target
            g.fillStyle = col.ink;
            for (let k = 0; k < live.melody.length; k++) {
                const f = live.melody[k];
                if (!(f > F0 && f < F1)) continue;
                const x = xOfBlock(k, from);
                if (x >= LABEL_W && x <= w) g.fillRect(x - 2, yOf(f) - 0.75, 4, 1.5);
            }
        }
        dots((k) => found[k], col.sound, 2.5);
        if (on && !live.plain) dots((k) => found[k] * factors(k), col.mark, 1.5);
        markStart(g, LABEL_W, plotW, 0, plotH, at / rate, L / rate, 4, col);
        const xm = Math.round(LABEL_W + plotW / 2) + 0.5;
        g.strokeStyle = col.ink; g.lineWidth = 1.5;
        g.beginPath(); g.moveTo(xm, 0); g.lineTo(xm, plotH); g.stroke();
    }

    // per band its strength in dB: grey the voice, lila the result
    function drawVocoder(g, w, h, col, p, on, live) {
        frame(g, w, h, col);
        const plotW = w - LABEL_W, plotH = h - AXIS, F0 = 100, F1 = 8000;
        const xOf = (f) => LABEL_W + Math.log(f / F0) / Math.log(F1 / F0) * plotW;
        const yOf = (db) => plotH - Math.max(0, Math.min(1, (db + 60) / 60)) * (plotH - 6);
        g.textAlign = 'right'; g.textBaseline = 'middle'; g.fillStyle = col.ink; g.strokeStyle = col.quiet; g.lineWidth = 1;
        for (const db of [0, -20, -40]) {
            const y = Math.round(yOf(db)) + 0.5;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
            g.fillText(`${db === 0 ? '0' : '−' + -db} dB`, LABEL_W - 4, y);
        }
        g.textAlign = 'center'; g.textBaseline = 'bottom';
        for (const f of [100, 200, 500, 1000, 2000, 5000]) g.fillText(f >= 1000 ? `${f / 1000} kHz` : `${f} Hz`, Math.max(LABEL_W + 18, xOf(f)), h);
        const edges = [];
        for (let b = 0; b <= p.bands; b++) edges.push(F0 * (F1 / F0) ** (b / p.bands));
        g.strokeStyle = col.quiet;
        for (const f of edges) { const x = Math.round(xOf(f)) + 0.5; g.beginPath(); g.moveTo(x, 0); g.lineTo(x, plotH); g.stroke(); }
        if (!live || !live.before) {
            g.fillStyle = col.ink; g.textBaseline = 'middle';
            g.fillText('Spiel den Klang ab, dann erscheinen hier die Bänder.', LABEL_W + plotW / 2, plotH / 2);
            return;
        }
        const level = (dbs, lo, hi) => {
            let sum = 0;
            for (let k = Math.max(1, Math.round(lo / live.binHz)); k < Math.max(2, Math.round(hi / live.binHz)); k++) sum += 10 ** (dbs[k] / 10);
            return 10 * Math.log10(Math.max(sum, 1e-12));
        };
        for (let b = 0; b < p.bands; b++) {
            const x0 = xOf(edges[b]) + 1, x1 = xOf(edges[b + 1]) - 1, mid = (x0 + x1) / 2;
            const v = level(live.before, edges[b], edges[b + 1]), o = level(live.after, edges[b], edges[b + 1]);
            g.fillStyle = col.sound; g.fillRect(x0, yOf(v), mid - x0, plotH - yOf(v));
            if (on) { g.fillStyle = col.mark; g.fillRect(mid, yOf(o), x1 - mid, plotH - yOf(o)); }
        }
    }

    // left the curve (input level against output level, with the make-up
    // gain), right the gain over four seconds: each knob changes one of them
    const compressorLayout = (w) => {
        const left = Math.round((w - 2 * LABEL_W - 16) * 0.4);
        return { left, x2: 2 * LABEL_W + left + 16, right: w - (2 * LABEL_W + left + 16) };
    };
    function drawCompressor(g, w, h, col, p, on, live) {
        const { left, x2, right } = compressorLayout(w);
        const plotH = h - AXIS;
        g.font = '12px system-ui, sans-serif';
        g.fillStyle = col.ground;
        g.fillRect(LABEL_W, 0, left, plotH);
        g.fillRect(x2, 0, right, plotH);
        // the curve: in −60 … 0 dB, out −60 … +24 dB
        const LO = -60, HI = 24;
        const xOf = (db) => LABEL_W + (db - LO) / -LO * left;
        const yOf = (db) => 4 + (HI - db) / (HI - LO) * (plotH - 8);
        g.lineWidth = 1; g.strokeStyle = col.quiet; g.fillStyle = col.ink;
        g.textAlign = 'right'; g.textBaseline = 'middle';
        for (const db of [24, 0, -24, -48]) {
            const y = Math.round(yOf(db)) + 0.5;
            g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(LABEL_W + left, y); g.stroke();
            g.fillText(signedDb(db), LABEL_W - 6, y);
        }
        g.textAlign = 'center'; g.textBaseline = 'bottom';
        for (const db of [-48, -24, 0]) g.fillText(signedDb(db), Math.min(LABEL_W + left - 16, xOf(db)), h);
        g.save();
        g.beginPath(); g.rect(LABEL_W, 0, left, plotH); g.clip();
        g.setLineDash([5, 4]); g.strokeStyle = col.sound; g.lineWidth = 1.5;
        g.beginPath(); g.moveTo(xOf(LO), yOf(LO)); g.lineTo(xOf(0), yOf(0)); g.stroke();
        g.setLineDash([]);
        const out = (db) => db + (on ? gainDb(db, p.threshold, p.ratio) + p.makeup : 0);
        g.beginPath();
        for (let px = 0; px <= left; px++) {
            const db = LO + px / left * -LO;
            if (px === 0) g.moveTo(LABEL_W, yOf(out(db))); else g.lineTo(LABEL_W + px, yOf(out(db)));
        }
        g.strokeStyle = col.mark; g.lineWidth = 2.5; g.stroke();
        if (live && live.level !== undefined) {
            const db = Math.max(LO, Math.min(0, live.level));
            g.beginPath(); g.arc(xOf(db), yOf(out(db)), 6, 0, 2 * Math.PI);
            g.fillStyle = col.mark; g.fill();
            g.lineWidth = 2; g.strokeStyle = col.ground; g.stroke();
        }
        g.restore();
        // the gain over time: −36 … +24 dB
        const G0 = -36, G1 = 24;
        const gy = (db) => 4 + (G1 - db) / (G1 - G0) * (plotH - 8);
        g.lineWidth = 1; g.strokeStyle = col.quiet; g.fillStyle = col.ink;
        g.textAlign = 'right'; g.textBaseline = 'middle';
        for (const db of [24, 12, 0, -12, -24, -36]) {
            const y = Math.round(gy(db)) + 0.5;
            g.lineWidth = db === 0 ? 1.5 : 1;
            g.beginPath(); g.moveTo(x2, y); g.lineTo(x2 + right, y); g.stroke();
            g.fillText(signedDb(db), x2 - 6, y);
        }
        g.textAlign = 'center'; g.textBaseline = 'bottom';
        g.fillText('−2 s', x2 + 14, h); g.fillText('jetzt', x2 + right / 2, h); g.fillText('+2 s', x2 + right - 14, h);
        if (live && live.gains) {
            g.save();
            g.beginPath(); g.rect(x2, 0, right, plotH); g.clip();
            g.beginPath();
            live.gains.forEach((db, px) => { if (px === 0) g.moveTo(x2, gy(db)); else g.lineTo(x2 + px, gy(db)); });
            g.strokeStyle = col.mark; g.lineWidth = 2; g.stroke();
            g.restore();
            const xm = Math.round(x2 + right / 2) + 0.5;
            g.strokeStyle = col.ink; g.lineWidth = 1.5;
            g.beginPath(); g.moveTo(xm, 0); g.lineTo(xm, plotH); g.stroke();
        } else {
            g.fillStyle = col.ink; g.textAlign = 'center'; g.textBaseline = 'middle';
            g.fillText('Spiel den Klang ab.', x2 + right / 2, plotH / 2);
        }
    }

    // where the sound starts again (the loop's beginning) in a window of
    // seconds around a place: a dashed line with »Anfang«
    function markStart(g, x0, width, top, bottom, place, length, seconds, col) {
        for (let k = -1; k <= 1; k++) {
            const t = Math.ceil((place - seconds / 2) / length + k) * length - place;
            if (Math.abs(t) > seconds / 2) continue;
            const x = Math.round(x0 + (t / seconds + 0.5) * width) + 0.5;
            g.save();
            g.setLineDash([4, 3]); g.strokeStyle = col.ink; g.lineWidth = 1;
            g.beginPath(); g.moveTo(x, top); g.lineTo(x, bottom); g.stroke();
            g.setLineDash([]);
            g.font = '11px system-ui, sans-serif'; g.fillStyle = col.ink; g.textAlign = 'left'; g.textBaseline = 'top';
            g.fillText('Anfang', x + 3, top + 1);
            g.restore();
        }
    }

    // ------------------------------------------------------------------
    // the widgets that play: starting one stops the others
    const players = new Set();
    function setUp(root) {
        const effect = EFFECTS[root.dataset.effect] || EFFECTS.eq;
        const name = root.dataset.effect in EFFECTS ? root.dataset.effect : 'eq';
        let clipId = effect.clips.includes(root.dataset.clip) ? root.dataset.clip : effect.clips[0];
        const params = {};
        let on = !effect.startOff;
        let ctx = null, output = null, voice = null, playing = false, startedAt = 0, offset = 0;
        let current = null, original = null, rate = 44100;
        const loaded = {}, peaks = {}, tracks = {};           // clip: Float32Array, its largest value
        let smooth = null, shownPeak = { of: null, peak: 1 };                       // the EQ's spectra, smoothed over time
        let worker = null, busy = false, again = false, asked = 0;
        const uid = Math.random().toString(36).slice(2, 8);
        root.dataset.chip = effect.chip;

        // a slider with marks under it, each centred on the thumb's place (the thumb is 1rem wide)
        const resets = [], setters = {};
        // a choice of a few values as buttons, and a switch
        const choice = (c) => {
            const group = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': c.label });
            const inputs = c.options.map(([value, text]) => {
                const id = `fx-${c.key}-${value}-${uid}`;
                const input = el('input', { type: 'radio', class: 'btn-check', name: `fx-${c.key}-${uid}`, id, autocomplete: 'off' });
                input.checked = value === c.value;
                input.addEventListener('change', () => { if (input.checked) { params[c.key] = value; changed(); } });
                group.append(input, el('label', { class: 'btn btn-outline-secondary', for: id, text }));
                return [value, input];
            });
            params[c.key] = c.value;
            resets.push(() => { params[c.key] = c.value; inputs.forEach(([v, input]) => { input.checked = v === c.value; }); });
            setters[c.key] = (value) => { params[c.key] = value; inputs.forEach(([v, input]) => { input.checked = v === value; }); };
            return el('div', { class: 'fx-row' }, [el('span', { class: 'fx-label', text: c.label }), el('div', { class: 'fx-track' }, [group]), el('span')]);
        };
        // buttons that move the knobs to a setting
        const presetRow = () => {
            const buttons = effect.presets.map(([text, values]) => {
                const b = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', text });
                b.addEventListener('click', () => { for (const [k, v] of Object.entries(values)) setters[k](v); changed(); });
                return b;
            });
            return el('div', { class: 'fx-row' }, [el('span', { class: 'fx-label', text: 'So klingt' }), el('div', { class: 'fx-track fx-presets' }, buttons), el('span')]);
        };
        const toggle = (t) => {
            const id = `fx-${t.key}-${uid}`;
            const input = el('input', { type: 'checkbox', class: 'form-check-input', id, role: 'switch' });
            input.checked = t.value;
            params[t.key] = t.value;
            input.addEventListener('change', () => { params[t.key] = input.checked; changed(); });
            resets.push(() => { input.checked = t.value; params[t.key] = t.value; });
            return el('div', { class: 'form-check form-switch fx-switch' }, [input, el('label', { class: 'form-check-label', for: id, text: t.label })]);
        };
        const slider = (knob) => {
            const id = `fx-${knob.key}-${uid}`;
            const input = el('input', { type: 'range', class: 'form-range fx-slider', id, min: knob.min, max: knob.max, step: knob.step, value: knob.value });
            const out = el('span', { class: 'fx-value' });
            const scale = el('div', { class: 'fx-scale', 'aria-hidden': 'true' });
            for (const [v, text] of knob.marks) {
                const at = knob.scale ? knob.scale.from(v) : v;
                const p = (at - knob.min) / (knob.max - knob.min);
                const mark = el('span', { class: 'fx-mark', text });
                mark.style.left = `calc(${(p * 100).toFixed(2)}% + ${(0.5 - p).toFixed(3)}rem)`;
                scale.append(mark);
            }
            const read = () => (knob.scale ? knob.scale.to(Number(input.value)) : Number(input.value));
            const update = () => { params[knob.key] = read(); out.textContent = knob.show(params[knob.key]); };
            input.addEventListener('input', () => { update(); changed(); });
            resets.push(() => { input.value = knob.value; update(); });
            setters[knob.key] = (v) => { input.value = knob.scale ? knob.scale.from(v) : v; update(); };
            update();
            return el('div', { class: 'fx-row' }, [el('label', { class: 'fx-label', for: id, text: knob.label }),
                el('div', { class: 'fx-track' }, [input, scale]), out]);
        };

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary fx-play' });
        const showPlay = () => playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }), playing ? ' Anhalten' : ' Abspielen');
        showPlay();
        const fromStart = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary fx-from-start' },
            [el('i', { class: 'bi bi-skip-start-fill' }), ' Von vorn']);
        fromStart.addEventListener('click', () => { if (playing) start(0); else { offset = 0; draw(); drawWave(); } });
        const clock = el('span', { class: 'fx-clock', 'aria-live': 'off' });
        const resetButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary fx-reset' },
            [el('i', { class: 'bi bi-arrow-counterclockwise' }), ' Zurücksetzen']);
        resetButton.addEventListener('click', () => { resets.forEach((r) => r()); changed(); });
        const onSwitch = el('input', { type: 'checkbox', class: 'form-check-input', id: `fx-on-${uid}`, role: 'switch' });
        onSwitch.checked = on;
        const clipGroup = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': 'Klang' });
        for (const id of effect.clips) {
            const input = el('input', { type: 'radio', class: 'btn-check', name: `fx-clip-${uid}`, id: `fx-clip-${id}-${uid}`, autocomplete: 'off' });
            input.checked = id === clipId;
            input.addEventListener('change', () => { if (input.checked) pickClip(id); });
            clipGroup.append(input, el('label', { class: 'btn btn-outline-secondary', for: `fx-clip-${id}-${uid}`, text: CLIPS[id].label }));
        }
        const chart = el('canvas', { class: 'fx-chart', role: 'img' });
        const line = el('p', { class: 'fx-line', 'aria-live': 'polite' });
        // who made the sound that plays (CLIPS, trusted HTML of this file)
        const credit = el('p', { class: 'fx-credit' });
        const showCredit = () => { credit.innerHTML = CLIPS[clipId].credit || ''; credit.hidden = !CLIPS[clipId].credit; };
        showCredit();
        const wave = el('canvas', { class: 'fx-wave', role: 'img', 'aria-label': 'Der Klang ohne und mit Effekt, vier Sekunden um die Stelle, die gerade spielt.' });
        root.append(
            el('div', { class: 'fx-controls' }, [
                ...(effect.clips.length > 1 ? [clipGroup] : []), playButton, fromStart, clock, resetButton,
                el('div', { class: 'form-check form-switch fx-switch' }, [onSwitch, el('label', { class: 'form-check-label', for: `fx-on-${uid}`, text: 'Effekt an' })]),
                ...(effect.switches || []).map(toggle)]),
            credit,
            ...(effect.choices || []).map(choice),
            ...effect.knobs.map(slider),
            ...(effect.presets ? [presetRow()] : []),
            el('figure', { class: 'fx-figure' }, [el('figcaption', { text: effect.caption }), chart]),
            line,
            el('figure', { class: 'fx-figure' }, [el('figcaption', { text: 'Der Klang, vier Sekunden um die Stelle, die gerade spielt (Strich): oben ohne Effekt, unten mit. Zum Spulen ziehen.' }), wave]));
        onSwitch.addEventListener('change', () => { on = onSwitch.checked; changed(); });

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (n) => s.getPropertyValue(n).trim();
            return { ink: get('--fx-ink'), quiet: get('--fx-quiet'), mark: get('--fx-mark'), ground: get('--fx-ground'), sound: get('--fx-sound') };
        };
        const prepare = (canvas) => {
            const w = canvas.clientWidth, h = canvas.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return null;
            if (canvas.width !== Math.round(w * dpr)) { canvas.width = Math.round(w * dpr); canvas.height = Math.round(h * dpr); }
            const g = canvas.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            g.clearRect(0, 0, w, h);
            g.font = '12px system-ui, sans-serif';
            return { g, w, h };
        };
        const sampleNow = () => Math.floor(positionAt(ctx ? ctx.currentTime : 0) * rate);

        // the spectrum of 4096 samples around a place, in dB (0 dB: a sine at full scale)
        const N = 4096;
        const hann = Float64Array.from({ length: N }, (_, i) => 0.5 - 0.5 * Math.cos(2 * Math.PI * i / N));
        function spectrum(x, at, last) {
            const re = new Float64Array(N), im = new Float64Array(N), L = x.length;
            for (let i = 0; i < N; i++) re[i] = x[((at - N / 2 + i) % L + L) % L] * hann[i];
            fft(re, im, false);
            const db = new Float32Array(N / 2);
            for (let k = 0; k < N / 2; k++) {
                db[k] = 20 * Math.log10(Math.max(1e-9, Math.hypot(re[k], im[k]) * 4 / N));
                if (last && playing) db[k] = 0.6 * last[k] + 0.4 * db[k];
            }
            return db;
        }
        function draw() {
            const c = prepare(chart);
            if (!c) return;
            let live = null;
            if ((name === 'eq' || name === 'vocoder') && original) {
                const at = sampleNow();
                smooth = { before: spectrum(original, at, smooth && smooth.before), after: spectrum(current || original, at, smooth && smooth.after) };
                live = { ...smooth, binHz: rate / N };
            } else if ((name === 'pitch' || name === 'autotune') && original && tracks[clipId]) {
                const found = tracks[clipId];
                const melody = melodyNow() && tracks[effect.melody];
                const tuned = name === 'autotune' ? tuneFactors(found, rate, params, melody) : null;
                const constant = 2 ** ((params.semitones || 0) / 12);
                const plain = (effect.plain || []).includes(clipId);
                live = { found, factors: tuned && !plain ? (k) => tuned[k] : () => constant, at: sampleNow(), rate, length: original.length,
                    melody: melody && params.target === 'melody' ? melody : null, plain };
            } else if (name === 'voices' && original) {
                live = { before: original, after: current || original, at: sampleNow(), rate };
            } else if (name === 'compressor' && original) {
                // the level as the compressor follows it, from half a second
                // before the window on, and per column of the right picture its gain
                const L = original.length, span = 4 * rate, cols = Math.max(1, compressorLayout(c.w).right);
                const from = sampleNow() - span / 2, warm = Math.round(0.5 * rate);
                const a = Math.exp(-1 / (params.attack * rate)), r = Math.exp(-1 / (params.release * rate));
                const gains = new Float32Array(cols);
                let level = 0, nowLevel = 0;
                for (let i = -warm; i < span; i++) {
                    const v = Math.abs(original[((Math.floor(from) + i) % L + L) % L]), k = v > level ? a : r;
                    level = k * level + (1 - k) * v;
                    if (i === Math.floor(span / 2)) nowLevel = level;
                    if (i >= 0) {
                        const px = Math.floor(i / span * cols);
                        if (Math.floor((i + 1) / span * cols) !== px || i === span - 1) {
                            const db = 20 * Math.log10(Math.max(level, 1e-9));
                            gains[px] = on ? gainDb(db, params.threshold, params.ratio) + params.makeup : 0;
                        }
                    }
                }
                live = { gains, level: 20 * Math.log10(Math.max(nowLevel, 1e-9)) };
            }
            effect.draw(c.g, c.w, c.h, colours(), params, on, live);
            line.textContent = on ? effect.line(params) : 'Der Effekt ist aus: der Klang, wie er ist.';
            chart.setAttribute('aria-label', line.textContent);
        }

        // both sounds, four seconds around where the sound is: per column the
        // largest and the smallest sample, on the same scale
        const WAVE_SECONDS = 4;
        function drawWave() {
            const c = prepare(wave);
            if (!c) return;
            const { g, w, h } = c, col = colours();
            clock.textContent = original ? `${de(positionAt(ctx.currentTime), 1)} s von ${de(original.length / rate, 1)} s` : '';
            if (!original) {
                g.fillStyle = col.ink; g.textAlign = 'center'; g.textBaseline = 'middle';
                g.fillText('Spiel den Klang ab, dann erscheint er hier.', w / 2, h / 2);
                return;
            }
            const gap = 6, laneH = (h - gap) / 2, plotW = w - LABEL_W;
            const span = WAVE_SECONDS * rate, from = sampleNow() - span / 2;
            // one scale for both, so that louder looks louder and nothing is cut off
            const shown = on ? current || original : original;
            if (shown !== original && shownPeak.of !== shown) shownPeak = { of: shown, peak: shown.reduce((m, v) => Math.max(m, Math.abs(v)), 1e-6) };
            const scale = 0.95 / Math.max(peaks[clipId], shown === original ? 0 : shownPeak.peak);
            [['ohne', original, col.sound], ['mit', shown, on ? col.mark : col.sound]].forEach(([label, x, colour], k) => {
                const mid = k * (laneH + gap) + laneH / 2, L = x.length;
                g.fillStyle = col.ground; g.fillRect(LABEL_W, k * (laneH + gap), plotW, laneH);
                g.fillStyle = col.ink; g.textAlign = 'right'; g.textBaseline = 'middle';
                g.fillText(label, LABEL_W - 6, mid);
                g.strokeStyle = colour; g.lineWidth = 1;
                g.beginPath();
                for (let px = 0; px < plotW; px++) {
                    const a = Math.floor(from + px / plotW * span), b = Math.max(a + 1, Math.floor(from + (px + 1) / plotW * span));
                    let lo = 0, hi = 0;
                    for (let i = a; i < b; i++) {
                        const v = x[(i % L + L) % L];
                        if (v < lo) lo = v; else if (v > hi) hi = v;
                    }
                    g.moveTo(LABEL_W + px + 0.5, mid - Math.min(1, hi * scale) * laneH / 2);
                    g.lineTo(LABEL_W + px + 0.5, mid - Math.max(-1, lo * scale) * laneH / 2 + 1);
                }
                g.stroke();
            });
            markStart(g, LABEL_W, plotW, 0, h, sampleNow() / rate, original.length / rate, WAVE_SECONDS, col);
            const x0 = Math.round(LABEL_W + plotW / 2) + 0.5;
            g.strokeStyle = col.ink; g.lineWidth = 1.5;
            g.beginPath(); g.moveTo(x0, 0); g.lineTo(x0, h); g.stroke();
        }

        // where the sound is at a time on the audio clock
        function positionAt(time) {
            if (!playing || !current) return offset;
            const d = current.length / rate;
            return ((offset + Math.max(0, time - startedAt)) % d + d) % d;
        }
        function fadeOut(old, when) {
            if (!old) return;
            old.gain.gain.cancelScheduledValues(when);
            old.gain.gain.setValueAtTime(1, when);
            old.gain.gain.linearRampToValueAtTime(0, when + FADE);
            old.source.stop(when + FADE + 0.01);
            setTimeout(() => { old.source.disconnect(); old.gain.disconnect(); }, (when - ctx.currentTime + FADE) * 1000 + 200);
        }
        // the new sound starts exactly where the old one will be then, and the
        // two cross over: the same sound, aligned to the sample
        function start(from) {
            if (!current) return;
            if (ctx.state === 'suspended') ctx.resume();
            const when = ctx.currentTime + 0.03;
            const place = from === undefined ? positionAt(when) : from;
            const buffer = ctx.createBuffer(1, current.length, rate);
            buffer.copyToChannel(current, 0);
            const gainNode = ctx.createGain();
            gainNode.gain.setValueAtTime(0, when);
            gainNode.gain.linearRampToValueAtTime(1, when + FADE);
            gainNode.connect(output);
            const source = ctx.createBufferSource();
            source.buffer = buffer;
            source.loop = true;
            source.connect(gainNode);
            source.start(when, place);
            fadeOut(voice, when);
            voice = { source, gain: gainNode };
            offset = place; startedAt = when;
            if (!playing) {
                for (const other of players) if (other !== me) other();
                playing = true; showPlay(); requestAnimationFrame(tick);
            }
        }
        const me = () => { if (playing) stop(); };
        players.add(me);
        function stop() {
            offset = positionAt(ctx.currentTime);
            if (voice) fadeOut(voice, ctx.currentTime);
            voice = null;
            playing = false;
            showPlay();
            draw(); drawWave();
        }
        function tick() {
            if (!playing) return;
            if (name === 'eq' || effect.live) draw();
            drawWave();
            requestAnimationFrame(tick);
        }

        // a knob moved: the picture at once, the sound as soon as the worker
        // has the last one (also while dragging), always the last setting
        // the switched-on clips to mix in, where they belong to the clip that plays
        const mixesNow = () => (effect.switches || []).filter((t) => t.mix && params[t.key] && melodyNow()).map((t) => [t.mix, t.level]);
        const effectNow = () => on && !effect.neutral(params) && !(effect.plain || []).includes(clipId);
        function message() {
            return { effect: name, clip: clipId, melody: melodyNow(), params: { ...params }, twice: !!effect.twice, id: ++asked,
                on: effectNow(), mixes: mixesNow(), cut: !!effect.clip };
        }
        function ask() {
            if (!effectNow() && !mixesNow().length) { current = original; if (playing) start(); return; }
            busy = true;
            worker.postMessage(message());
        }
        function changed() {
            draw(); drawWave();
            if (!worker || !original) return;
            if (busy) { again = true; return; }
            ask();
        }
        // the melody clip, where it belongs to the clip that plays
        function melodyNow() {
            return effect.melody && (!effect.melodyFor || effect.melodyFor === clipId) ? effect.melody : null;
        }
        function keepTracks(e) {
            if (e.data.track) tracks[clipId] = e.data.track;
            if (e.data.melodyTrack) tracks[effect.melody] = e.data.melodyTrack;
        }
        function heard(e) {
            busy = false;
            current = e.data.out;
            keepTracks(e);
            if (playing) start(); else drawWave();
            if (again) { again = false; ask(); }
        }
        async function loadClip(id) {
            if (!loaded[id]) {
                const response = await fetch(BUSTER ? `${CLIPS[id].url}?${BUSTER}` : CLIPS[id].url);
                if (!response.ok) throw new Error(response.status);
                const decoded = await ctx.decodeAudioData(await response.arrayBuffer());
                loaded[id] = decoded.getChannelData(0).slice();
                peaks[id] = loaded[id].reduce((m, v) => Math.max(m, Math.abs(v)), 1e-6);
                worker.postMessage({ load: id, x: loaded[id], rate });
            }
            return loaded[id];
        }
        async function ready() {
            const AC = window.AudioContext || window.webkitAudioContext;
            if (!ctx) {
                ctx = new AC();
                rate = ctx.sampleRate;
                output = ctx.createGain();
                output.connect(ctx.destination);
                worker = new Worker(URL.createObjectURL(new Blob([WORKER], { type: 'text/javascript' })));
                worker.onmessage = heard;
            }
            if (effect.melody) await loadClip(effect.melody);
            for (const t of effect.switches || []) if (t.mix) await loadClip(t.mix);
            original = await loadClip(clipId);
            current = original;
            const needsTrack = (name === 'pitch' || name === 'autotune') && !tracks[clipId];
            if (effectNow() || mixesNow().length || needsTrack) {
                // the first sound of a clip waits for the worker once
                await new Promise((done) => {
                    worker.onmessage = (e) => { worker.onmessage = heard; current = e.data.out; keepTracks(e); done(); };
                    worker.postMessage(message());
                });
            }
        }
        async function pickClip(id) {
            clipId = id;
            showCredit();
            if (!ctx) return;
            const was = playing;
            if (playing) stop();
            offset = 0;
            smooth = null;
            await ready();
            draw(); drawWave();
            if (was) start(0);
        }
        playButton.addEventListener('click', async () => {
            if (playing) { stop(); return; }
            playButton.disabled = true;
            try {
                await ready();
            } catch (e) {
                line.textContent = 'Der Klang ließ sich nicht laden.';
                return;
            } finally {
                playButton.disabled = false;
            }
            start(offset);
        });
        // dragging a picture of the time around the sound (the wave, the
        // pitch) moves through it: to the right goes back
        const draggable = (canvas) => {
            let drag = null;
            canvas.classList.add('fx-draggable');
            canvas.addEventListener('pointerdown', (e) => {
                if (!original) return;
                canvas.setPointerCapture(e.pointerId);
                drag = { x: e.clientX, from: positionAt(ctx.currentTime), last: 0 };
            });
            canvas.addEventListener('pointermove', (e) => {
                if (!drag) return;
                const d = original.length / rate, plotW = Math.max(1, canvas.clientWidth - LABEL_W);
                const place = (((drag.from - (e.clientX - drag.x) / plotW * WAVE_SECONDS) % d) + d) % d;
                if (playing) {
                    if (performance.now() - drag.last > 80) { drag.last = performance.now(); start(place); }
                } else { offset = place; draw(); drawWave(); }
                drag.place = place;
            });
            const end = () => { if (drag && playing && drag.place !== undefined) start(drag.place); drag = null; };
            canvas.addEventListener('pointerup', end);
            canvas.addEventListener('pointercancel', end);
        };
        draggable(wave);
        if (effect.draw === drawPitch) draggable(chart);
        if (typeof ResizeObserver !== 'undefined') {
            new ResizeObserver(draw).observe(chart);
            new ResizeObserver(drawWave).observe(wave);
        }
        draw(); drawWave();
    }

    const go = () => document.querySelectorAll('.effect-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
