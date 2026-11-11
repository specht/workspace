// Frequencies (»Audio komprimieren«): a clip as a spectrogram, time to the
// right and frequency upwards on a logarithmic scale from 40 Hz to 20 kHz,
// the louder the darker (in the dark theme the brighter); under it the
// spectrum at the chosen moment, how strong each frequency is, in dB, with
// the strongest one named. A click, a drag or the arrow keys choose the moment;
// while the clip plays, the moment follows it.
//
//   <script src='/include/spectrum_widget.js'></script>
//   <div class='spectrum-widget'></div>
//   <div class='spectrum-widget' data-clip='akkord'></div>
//
// The clips: the files students fetch (/audio/*.wav, read as in
// sampling_widget.js): the music and the voice; for the music switches for
// its six instruments, from
// /audio/<clip>/*.flac, made by sounds/make.py so that they add up to it;
// the chord of »Drei Töne auf einmal« (each tone 0,5,
// the sum divided by 3) and a sweep, a tone that glides from 100 Hz to 10 kHz, both
// computed here. The spectra come from an FFT of 4.096 measurements under
// a Hann window, every 512 measurements, computed once per clip.
(function () {
    'use strict';
    const SOURCE = 44100;
    const N = 4096, HOP = 512;
    const F0 = 40, F1 = 20000;               // the frequencies shown
    const FLOOR = -90, TOP = -6;             // dB: from nothing to the loudest colour
    const AXIS = 18;                         // the labels under a picture
    const LABEL_W = 56;                      // the frequency labels at the left of the spectrogram
    const still = window.matchMedia('(prefers-reduced-motion: reduce)');
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    let counter = 0;

    const tone = (seconds, freq) => {
        const x = new Float32Array(Math.round(seconds * SOURCE));
        for (let i = 0; i < x.length; i++) x[i] = freq(i / SOURCE);
        return x;
    };
    const CLIPS = [
        { id: 'musik', label: 'Musik', at: 2.6 },
        { id: 'stimme', label: 'Stimme', at: 0.7 },
        { id: 'akkord', label: 'Akkord', at: 1, make: () => tone(2, (t) =>
            [440, 554.37, 659.26].reduce((s, f) => s + 0.5 * Math.sin(2 * Math.PI * f * t), 0) / 3) },
        // a tone gliding from 100 Hz to 10 kHz in 4 s, evenly on the logarithmic scale
        { id: 'sweep', label: 'Sweep', at: 2, make: () => {
            const k = Math.log(100) / 4;
            return tone(4, (t) => 0.4 * Math.sin(2 * Math.PI * 100 * (Math.exp(k * t) - 1) / k));
        } },
    ];

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

    // a WAV file as numbers from -1 to 1 (the chunk »data«, 16 bit, Little Endian)
    const files = {};
    function load(clip) {
        if (clip.make) return Promise.resolve(files[clip.id] || (files[clip.id] = clip.make()));
        if (!files[clip.id]) {
            files[clip.id] = fetch(`/audio/${clip.id}.wav`).then((r) => {
                if (!r.ok) throw new Error(r.status);
                return r.arrayBuffer();
            }).then((buffer) => {
                const view = new DataView(buffer);
                let pos = 12;
                while (pos + 8 <= buffer.byteLength) {
                    const name = String.fromCharCode(...new Uint8Array(buffer, pos, 4));
                    const size = view.getUint32(pos + 4, true);
                    if (name === 'data') {
                        const n = Math.min(size, buffer.byteLength - pos - 8) >> 1;
                        const x = new Float32Array(n);
                        for (let i = 0; i < n; i++) x[i] = view.getInt16(pos + 8 + 2 * i, true) / 32768;
                        return x;
                    }
                    pos += 8 + size + (size & 1);
                }
                throw new Error('no data chunk');
            });
        }
        return Promise.resolve(files[clip.id]);
    }

    // the FFT of N numbers, iterative, in place: bit reversal, then N/2 butterflies per round
    const LOG = Math.log2(N);
    const reversed = new Uint16Array(N);
    for (let i = 0; i < N; i++) {
        let r = 0;
        for (let b = 0; b < LOG; b++) r |= ((i >> b) & 1) << (LOG - 1 - b);
        reversed[i] = r;
    }
    const cosT = new Float64Array(N / 2), sinT = new Float64Array(N / 2);
    for (let i = 0; i < N / 2; i++) { cosT[i] = Math.cos(2 * Math.PI * i / N); sinT[i] = -Math.sin(2 * Math.PI * i / N); }
    const hann = new Float64Array(N);
    for (let i = 0; i < N; i++) hann[i] = 0.5 - 0.5 * Math.cos(2 * Math.PI * i / N);
    const windowSum = hann.reduce((s, v) => s + v, 0);

    function fft(re, im) {
        for (let i = 0; i < N; i++) {
            const j = reversed[i];
            if (j > i) { [re[i], re[j]] = [re[j], re[i]]; [im[i], im[j]] = [im[j], im[i]]; }
        }
        for (let size = 2; size <= N; size *= 2) {
            const half = size / 2, step = N / size;
            for (let start = 0; start < N; start += size) {
                for (let k = 0; k < half; k++) {
                    const a = start + k, b = a + half;
                    const wr = cosT[k * step], wi = sinT[k * step];
                    const tr = re[b] * wr - im[b] * wi, ti = re[b] * wi + im[b] * wr;
                    re[b] = re[a] - tr; im[b] = im[a] - ti;
                    re[a] += tr; im[a] += ti;
                }
            }
        }
    }

    // every HOP measurements the spectrum in dB, scaled so that a sine with
    // amplitude 1 has 0 dB
    function spectra(x) {
        const count = Math.max(1, Math.floor((x.length - N) / HOP) + 1);
        const out = [];
        const re = new Float64Array(N), im = new Float64Array(N);
        for (let f = 0; f < count; f++) {
            for (let i = 0; i < N; i++) { re[i] = (x[f * HOP + i] || 0) * hann[i]; im[i] = 0; }
            fft(re, im);
            const db = new Float32Array(N / 2 + 1);
            for (let k = 0; k <= N / 2; k++) {
                const a = 2 * Math.hypot(re[k], im[k]) / windowSum;
                db[k] = 20 * Math.log10(a + 1e-9);
            }
            out.push(db);
        }
        return out;
    }

    // the instruments of the music, each a track of its own (sounds/make.py)
    // the pieces of music and their instruments, each a track of its own
    // (/audio/<clip>/<name>.flac)
    const INSTRUMENTS = {
        musik: [['kick', 'Kick'], ['snare', 'Snare'], ['hi-hat', 'Hi-Hat'], ['bass', 'Bass'], ['chords', 'Akkorde'], ['melody', 'Melodie']],
    };
    const tracks = {};
    function loadTracks(clipId) {
        if (!tracks[clipId]) {
            const OAC = window.OfflineAudioContext || window.webkitOfflineAudioContext;
            tracks[clipId] = Promise.all(INSTRUMENTS[clipId].map(([name]) => fetch(`/audio/${clipId}/${name}.flac`)
                .then((r) => { if (!r.ok) throw new Error(r.status); return r.arrayBuffer(); })
                .then((data) => new OAC(1, 1, SOURCE).decodeAudioData(data))
                .then((buffer) => buffer.getChannelData(0))));
        }
        return tracks[clipId];
    }

    const BIN = SOURCE / N;
    // the level between bin b0 and bin b1 (not whole numbers): low down,
    // where a pixel is less than a bin, between the two nearest bins; high
    // up the loudest of the bins it covers
    function level(spectrum, b0, b1) {
        if (b1 - b0 < 1) {
            const b = (b0 + b1) / 2, i = Math.floor(b), t = b - i;
            return spectrum[i] * (1 - t) + spectrum[Math.min(N / 2, i + 1)] * t;
        }
        let db = -Infinity;
        for (let i = Math.floor(b0); i <= Math.min(N / 2, Math.ceil(b1)); i++) db = Math.max(db, spectrum[i]);
        return db;
    }
    const logPos = (f) => Math.log(f / F0) / Math.log(F1 / F0);      // 0 at F0, 1 at F1
    const LABELS = [50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000];

    function setUp(root) {
        const id = `spectrum-${++counter}`;
        let clip = CLIPS.find((c) => c.id === root.dataset.clip) || CLIPS[0];
        let samples = null, frames = null, at = clip.at;
        const muted = new Set();                       // instruments switched off
        let ctx = null, source = null, startedAt = 0, offset = 0, playing = false;

        const group = (name, label, items, checked, onPick) => {
            const box = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': label });
            items.forEach((item) => {
                const inputId = `${id}-${name}-${item.value}`;
                const input = el('input', { type: 'radio', class: 'btn-check', name: `${id}-${name}`, id: inputId, autocomplete: 'off' });
                if (item.value === checked) input.checked = true;
                input.addEventListener('change', () => { if (input.checked) onPick(item.value); });
                box.append(input, el('label', { class: 'btn btn-outline-secondary', for: inputId, text: item.label }));
            });
            return el('div', { class: 'spectrum-group' }, [el('span', { class: 'spectrum-label', text: label }), box]);
        };

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-success spectrum-play' });
        // the instruments of a piece of music, each a switch of its own (not a
        // bar), made anew for each piece; all on at first
        const instruments = el('div', { class: 'spectrum-instruments', role: 'group', 'aria-label': 'Instrumente' });
        function makeSwitches() {
            muted.clear();
            instruments.replaceChildren();
            (INSTRUMENTS[clip.id] || []).forEach(([name, label]) => {
                const inputId = `${id}-instrument-${clip.id}-${name}`;
                const input = el('input', { type: 'checkbox', class: 'btn-check', id: inputId, autocomplete: 'off' });
                input.checked = true;
                input.addEventListener('change', () => {
                    if (input.checked) muted.delete(name); else muted.add(name);
                    remix();
                });
                instruments.append(input, el('label', { class: 'btn btn-sm btn-outline-secondary', for: inputId, text: label }));
            });
        }
        const instrumentRow = el('div', { class: 'spectrum-controls' }, [
            el('div', { class: 'spectrum-group' }, [el('span', { class: 'spectrum-label', text: 'Instrumente' }), instruments])]);
        const showPlay = () => {
            playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }), playing ? ' Anhalten' : ' Abspielen');
        };
        showPlay();
        const gram = el('canvas', { class: 'spectrum-gram', tabindex: '0', role: 'slider', 'aria-label': 'Zeitpunkt im Spektrogramm' });
        const chart = el('canvas', { class: 'spectrum-chart', role: 'img' });
        const line = el('p', { class: 'spectrum-line', 'aria-live': 'polite' });
        root.append(
            el('div', { class: 'spectrum-controls' }, [
                group('clip', 'Klang', CLIPS.map((c) => ({ value: c.id, label: c.label })), clip.id, (v) => pickClip(v)),
                playButton,
            ]),
            instrumentRow,
            el('figure', { class: 'spectrum-figure' }, [
                el('figcaption', { text: 'Spektrogramm: die Zeit nach rechts, die Frequenz nach oben, je lauter, desto kräftiger die Farbe. Hier klicken oder ziehen wählt den Zeitpunkt.' }),
                gram]),
            el('figure', { class: 'spectrum-figure' }, [
                el('figcaption', { text: 'Spektrum am gewählten Zeitpunkt: wie stark jede Frequenz ist' }),
                chart]),
            line);

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (name) => s.getPropertyValue(name).trim();
            return { ink: get('--spectrum-ink'), quiet: get('--spectrum-quiet'), mark: get('--spectrum-mark'),
                ground: get('--spectrum-ground'), hot: get('--spectrum-hot') };
        };
        const rgb = (colour) => {
            const m = colour.match(/^#([0-9a-f]{6})$/i);
            if (m) return [0, 2, 4].map((i) => parseInt(m[1].slice(i, i + 2), 16));
            const n = colour.match(/\d+(\.\d+)?/g);
            return n ? n.slice(0, 3).map(Number) : [0, 0, 0];
        };

        function sizeCanvas(c) {
            const w = c.clientWidth, h = c.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return null;
            if (c.width !== Math.round(w * dpr) || c.height !== Math.round(h * dpr)) {
                c.width = Math.round(w * dpr);
                c.height = Math.round(h * dpr);
            }
            const g = c.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            g.clearRect(0, 0, w, h);
            return { g, w, h };
        }

        const duration = () => samples.length / SOURCE;
        const frameAt = (t) => Math.max(0, Math.min(frames.length - 1, Math.round((t * SOURCE - N / 2) / HOP)));
        const now = () => (playing ? position() : at);

        // the spectrogram as an image, made again only when the size, the clip or the theme change
        let image = null, imageKey = '';
        function gramImage(plotW, plotH, col) {
            const key = `${clip.id} ${plotW} ${plotH} ${col.ground} ${col.hot}`;
            if (image && imageKey === key) return image;
            // for each row the bins it covers: high up many, low down a part of one
            const rows = [];
            for (let y = 0; y < plotH; y++) {
                const fTop = F0 * (F1 / F0) ** (1 - y / plotH), fBottom = F0 * (F1 / F0) ** (1 - (y + 1) / plotH);
                rows.push([fBottom / BIN, fTop / BIN]);
            }
            const ground = rgb(col.ground), hot = rgb(col.hot);
            const data = new ImageData(plotW, plotH);
            for (let x = 0; x < plotW; x++) {
                const spectrum = frames[Math.min(frames.length - 1, Math.floor(x / plotW * frames.length))];
                for (let y = 0; y < plotH; y++) {
                    const db = level(spectrum, rows[y][0], rows[y][1]);
                    const v = Math.max(0, Math.min(1, (db - FLOOR) / (TOP - FLOOR)));
                    const k = v * v, o = (y * plotW + x) * 4;
                    data.data[o] = ground[0] + (hot[0] - ground[0]) * k;
                    data.data[o + 1] = ground[1] + (hot[1] - ground[1]) * k;
                    data.data[o + 2] = ground[2] + (hot[2] - ground[2]) * k;
                    data.data[o + 3] = 255;
                }
            }
            const c = document.createElement('canvas');
            c.width = plotW; c.height = plotH;
            c.getContext('2d').putImageData(data, 0, 0);
            image = c; imageKey = key;
            return image;
        }

        function drawGram() {
            const s = sizeCanvas(gram);
            if (!s) return;
            const { g, w, h } = s;
            const col = colours();
            const plotW = Math.max(10, Math.floor(w - LABEL_W)), plotH = h - AXIS;
            g.fillStyle = col.ground;
            g.fillRect(LABEL_W, 0, plotW, plotH);
            g.font = '12px system-ui, sans-serif';
            if (!frames) {
                g.fillStyle = col.ink; g.textAlign = 'center'; g.textBaseline = 'middle';
                g.fillText('lädt …', LABEL_W + plotW / 2, plotH / 2);
                return;
            }
            g.imageSmoothingEnabled = false;
            g.drawImage(gramImage(plotW, plotH, col), LABEL_W, 0, plotW, plotH);
            // the frequencies at the left, every other one where it is low
            g.fillStyle = col.ink; g.textAlign = 'right'; g.textBaseline = 'middle';
            const each = plotH < 200 ? [100, 1000, 10000] : [50, 100, 200, 500, 1000, 2000, 5000, 10000];
            for (const f of each) {
                const y = (1 - logPos(f)) * plotH;
                g.fillText(f >= 1000 ? `${de(f / 1000)} kHz` : `${f} Hz`, LABEL_W - 6, y);
                g.fillRect(LABEL_W - 4, Math.round(y), 3, 1);
            }
            // the time below, and the chosen moment
            g.textBaseline = 'bottom';
            g.textAlign = 'left'; g.fillText('0 s', LABEL_W, h);
            g.textAlign = 'right'; g.fillText(`${de(duration(), 1)} s`, w, h);
            const x = LABEL_W + now() / duration() * plotW;
            g.fillStyle = col.mark;
            g.fillRect(Math.round(x) - 1, 0, 2, plotH);
        }

        function drawChart() {
            const s = sizeCanvas(chart);
            if (!s) return;
            const { g, w, h } = s;
            const col = colours();
            const plotW = Math.max(10, Math.floor(w - LABEL_W)), plotH = h - AXIS;
            const xOf = (f) => LABEL_W + logPos(f) * plotW;
            const yOf = (db) => (Math.max(FLOOR, Math.min(0, db)) / FLOOR) * plotH;
            g.fillStyle = col.ground;
            g.fillRect(LABEL_W, 0, plotW, plotH);
            g.font = '12px system-ui, sans-serif';
            // the dB lines and their labels
            g.textAlign = 'right'; g.textBaseline = 'middle';
            for (let db = 0; db >= FLOOR; db -= 20) {
                const y = Math.round(yOf(db)) + 0.5;
                g.strokeStyle = col.quiet; g.lineWidth = 1;
                g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
                g.fillStyle = col.ink;
                g.fillText(`${db === 0 ? '' : '−'}${Math.abs(db)} dB`, LABEL_W - 6, Math.min(plotH - 6, Math.max(6, y)));
            }
            // the frequencies under it, fewer on a narrow screen
            g.textAlign = 'center'; g.textBaseline = 'bottom';
            const labels = plotW < 420 ? [100, 1000, 10000] : LABELS;
            for (const f of labels) {
                const x = xOf(f);
                g.fillText(f >= 1000 ? `${de(f / 1000)} kHz` : `${f} Hz`, Math.min(w - 22, Math.max(LABEL_W + 20, x)), h);
            }
            if (!frames) return;
            const spectrum = frames[frameAt(now())];
            // the spectrum: per pixel the loudest bin it covers, as a filled area
            g.beginPath();
            g.moveTo(LABEL_W, plotH);
            for (let px = 0; px <= plotW; px++) {
                const b0 = F0 * (F1 / F0) ** (px / plotW) / BIN, b1 = F0 * (F1 / F0) ** ((px + 1) / plotW) / BIN;
                g.lineTo(LABEL_W + px, yOf(level(spectrum, b0, b1)));
            }
            g.lineTo(LABEL_W + plotW, plotH);
            g.closePath();
            g.fillStyle = col.mark; g.globalAlpha = 0.25; g.fill(); g.globalAlpha = 1;
            g.strokeStyle = col.mark; g.lineWidth = 1.5; g.stroke();
            // the strongest frequency: a dot and its value
            const peak = strongest(spectrum);
            if (peak) {
                const x = xOf(peak.f), y = yOf(peak.db);
                g.fillStyle = col.mark;
                g.beginPath(); g.arc(x, y, 4, 0, 2 * Math.PI); g.fill();
            }
        }

        // the bin with the most, between F0 and F1, its frequency refined
        // by a parabola through it and its neighbours
        function strongest(spectrum) {
            let best = -1;
            for (let i = Math.ceil(F0 / BIN); i < Math.min(N / 2, F1 / BIN); i++) if (best < 0 || spectrum[i] > spectrum[best]) best = i;
            if (best < 1 || spectrum[best] < FLOOR + 20) return null;
            const a = spectrum[best - 1], b = spectrum[best], c = spectrum[best + 1];
            const shift = (a - c) / (2 * (a - 2 * b + c)) || 0;
            return { f: (best + shift) * BIN, db: b };
        }

        function describe() {
            if (!frames) { line.textContent = ''; return; }
            const peak = strongest(frames[frameAt(now())]);
            const when = `Bei ${de(now(), 2)} s`;
            line.innerHTML = peak
                ? `${when} ist die stärkste Frequenz <strong>${de(Math.round(peak.f))}&nbsp;Hz</strong> (${peak.db < 0 ? '−' : ''}${de(Math.abs(peak.db))} dB).`
                : `${when} ist es still.`;
            chart.setAttribute('aria-label', line.textContent);
        }

        function draw() { drawGram(); drawChart(); describe(); }

        // sound --------------------------------------------------------------------
        // A change (instruments, a new moment) never stops the sound first:
        // the new source is scheduled a little ahead on the audio clock, at
        // the place the old one will have reached (or the place asked for),
        // and the two cross over in FADE seconds.
        const FADE = 0.02;
        let voice = null;                                       // { source, gain }
        function position() {
            if (!playing) return offset;
            const d = duration();
            return ((offset + Math.max(0, ctx.currentTime - startedAt)) % d + d) % d;
        }
        function start(from, smooth = playing) {
            const AC = window.AudioContext || window.webkitAudioContext;
            if (!AC || !samples) return;
            if (!ctx) ctx = new AC();
            if (ctx.state === 'suspended') ctx.resume();
            const d = duration();
            const when = ctx.currentTime + (smooth ? 0.03 : 0.005);
            const place = from === null ? ((offset + when - startedAt) % d + d) % d : from;
            const buffer = ctx.createBuffer(1, samples.length, SOURCE);
            buffer.copyToChannel(samples, 0);
            const gain = ctx.createGain();
            gain.gain.setValueAtTime(0, when);
            gain.gain.linearRampToValueAtTime(1, when + FADE);
            gain.connect(ctx.destination);
            const source = ctx.createBufferSource();
            source.buffer = buffer;
            source.loop = true;
            source.connect(gain);
            source.start(when, place);
            fadeOut(voice, when);
            voice = { source, gain };
            offset = place; startedAt = when;
            playing = true;
            showPlay();
            requestAnimationFrame(tick);
        }
        function fadeOut(old, when) {
            if (!old) return;
            old.gain.gain.cancelScheduledValues(when);
            old.gain.gain.setValueAtTime(1, when);
            old.gain.gain.linearRampToValueAtTime(0, when + FADE);
            old.source.stop(when + FADE + 0.01);
            setTimeout(() => { old.source.disconnect(); old.gain.disconnect(); }, (when - ctx.currentTime + FADE) * 1000 + 200);
        }
        function stopSource() {
            if (voice) fadeOut(voice, ctx.currentTime);
            voice = null;
        }
        function stop() {
            at = offset = position();
            stopSource();
            playing = false;
            showPlay();
            draw();
        }
        let lastTick = 0;
        function tick(stamp) {
            if (!playing) return;
            if (!still.matches || stamp - lastTick > 250) { draw(); lastTick = stamp; }
            requestAnimationFrame(tick);
        }

        // the music with only the instruments switched on: the sum of their tracks
        let remixing = 0;
        async function remix() {
            if (!INSTRUMENTS[clip.id]) return;
            const piece = clip.id;
            const ticket = ++remixing;
            const from = now();
            const wasPlaying = playing;
            let mixed;
            try {
                if (muted.size === 0) {
                    mixed = await load(clip);
                } else {
                    const parts = await loadTracks(piece);
                    mixed = new Float32Array(parts[0].length);
                    INSTRUMENTS[piece].forEach(([name], i) => {
                        if (muted.has(name)) return;
                        const part = parts[i];
                        for (let j = 0; j < mixed.length; j++) mixed[j] += part[j];
                    });
                }
            } catch (e) {
                line.textContent = 'Die Instrumente ließen sich nicht laden.';
                return;
            }
            if (ticket !== remixing || clip.id !== piece) return;
            samples = mixed;
            frames = spectra(mixed);
            image = null;
            at = from;
            // playing, the new mix takes over where the sound is; else it waits at the moment
            if (wasPlaying) start(null); else { offset = from; draw(); }
        }

        async function pickClip(value) {
            clip = CLIPS.find((c) => c.id === value);
            instrumentRow.hidden = !INSTRUMENTS[clip.id];
            makeSwitches();
            if (playing) { stopSource(); playing = false; showPlay(); }
            samples = null; frames = null; image = null;
            at = offset = clip.at;
            draw();
            try {
                const x = await load(clip);
                if (clip.id !== value) return;
                samples = x;
                frames = spectra(x);
                if (INSTRUMENTS[clip.id] && muted.size) { remix(); return; }
            } catch (e) {
                line.textContent = 'Der Klang ließ sich nicht laden.';
                return;
            }
            draw();
        }

        playButton.addEventListener('click', () => { if (playing) stop(); else start(at); });
        // restart: while dragging only the pictures follow; the sound goes
        // on from the new moment when the pointer lets go
        const moveTo = (t, restart = true) => {
            at = Math.max(0, Math.min(duration() - 0.01, t));
            if (!playing) offset = at;
            else if (restart) start(at);
            else { offset = at; startedAt = ctx.currentTime; }
            draw();
        };
        // a click chooses the moment, and dragging moves it
        let dragging = false;
        const fromPointer = (e) => {
            const r = gram.getBoundingClientRect();
            return (e.clientX - r.left - LABEL_W) / (r.width - LABEL_W) * duration();
        };
        gram.addEventListener('pointerdown', (e) => {
            if (!frames) return;
            dragging = true;
            gram.setPointerCapture(e.pointerId);
            moveTo(fromPointer(e), false);
        });
        gram.addEventListener('pointermove', (e) => { if (dragging) moveTo(fromPointer(e), false); });
        const release = (e) => {
            if (!dragging) return;
            dragging = false;
            moveTo(fromPointer(e), true);
        };
        gram.addEventListener('pointerup', release);
        gram.addEventListener('pointercancel', release);
        gram.addEventListener('keydown', (e) => {
            if (!frames || !['ArrowLeft', 'ArrowRight'].includes(e.key)) return;
            e.preventDefault();
            moveTo(now() + (e.key === 'ArrowRight' ? 1 : -1) * (e.shiftKey ? 0.01 : 0.1));
        });

        new ResizeObserver(() => draw()).observe(root);
        new MutationObserver(() => draw())
            .observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
        const near = new IntersectionObserver((entries) => {
            if (!entries[0].isIntersecting) return;
            near.disconnect();
            pickClip(clip.id);
        }, { rootMargin: '600px' });
        near.observe(root);
        draw();
    }

    const go = () => document.querySelectorAll('.spectrum-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
