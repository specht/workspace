// An equalizer (»Audio komprimieren«, »Frequenzen lauter und leiser«): the
// music plays, and one bell of the EQ makes the frequencies around a centre
// louder or quieter. Four sliders: the centre (40 Hz to 16 kHz, on a
// logarithmic scale), the width in octaves, the gain in dB in the middle
// and the dB of the frequencies far away (»Übrige«, 0 for the bell of the
// page, lower for a band-pass); a switch
// compares with the music as it is. The bell is the one students write in
// Python: at d octaves from the centre the gain is gain · 2^(−(d/width)²)
// dB, so one width away half of it is left, and the wave is multiplied by
// 10^(dB/20). It is applied the same way too: the FFT of the whole piece,
// each frequency multiplied, back with the inverse FFT, in a worker; on a
// change the new sound crosses over exactly where the old one is, also
// while a slider is dragged, so the music never stops or jumps. Under the sliders the bell
// in purple over the spectrum that is playing.
//
//   <script src='/include/eq_widget.js'></script>
//   <div class='eq-widget'></div>
(function () {
    'use strict';
    const RATE = 44100;
    const F0 = 40, F1 = 20000;               // the frequencies shown
    const CENTRE0 = 40, CENTRE1 = 16000;     // the centre's slider
    const GAIN = 24;                         // dB up and down
    const FADE = 0.03;
    const LABEL_W = 46, AXIS = 18;
    const LABELS = [50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000];
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });

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

    // the bell: how many dB at f, and the factor for the wave
    // the bell from rest (far away) to gain (in the middle); rest 0 is the
    // bell students write, rest below gain a band-pass
    const bellDb = (f, centre, width, gain, rest = 0) => {
        const d = Math.log2(Math.max(f, 1) / centre);
        return rest + (gain - rest) * 2 ** (-((d / width) ** 2));
    };

    // the music as numbers from -1 to 1 (the chunk »data«, 16 bit, Little Endian)
    let music = null;
    function load() {
        if (!music) {
            music = fetch('/audio/musik.wav').then((r) => {
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
        return music;
    }

    // an FFT in place, iterative: bit reversal, then the butterflies;
    // inverse: with the conjugate factors (the caller divides by n)
    function fft(re, im, inverse) {
        const n = re.length;
        for (let i = 1, j = 0; i < n; i++) {
            let bit = n >> 1;
            for (; j & bit; bit >>= 1) j ^= bit;
            j ^= bit;
            if (i < j) { [re[i], re[j]] = [re[j], re[i]]; [im[i], im[j]] = [im[j], im[i]]; }
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

    // The filtering runs in a worker, so that dragging a slider stays
    // smooth: it keeps the spectrum of the piece and answers each setting
    // with the filtered piece. The piece is laid into the FFT as it loops:
    // its end before it and its start after it, so that the seam of the
    // loop stays seamless; only the middle comes back.
    const WORKER = `'use strict';
const RATE = ${RATE};
${fft.toString()}
const bellDb = ${bellDb.toString()};
let re0 = null, im0 = null, n = 0, size = 0, lead = 0;
onmessage = (e) => {
    const m = e.data;
    if (m.samples) {
        const x = m.samples;
        n = x.length;
        size = 1 << Math.ceil(Math.log2(n * 1.25));
        lead = (size - n) >> 1;
        re0 = new Float64Array(size); im0 = new Float64Array(size);
        for (let i = 0; i < size; i++) re0[i] = x[((i - lead) % n + n) % n];
        fft(re0, im0, false);
        return;
    }
    const { centre, width, gain, rest, id } = m;
    const re = new Float64Array(size), im = new Float64Array(size);
    for (let k = 0; k <= size / 2; k++) {
        const factor = 10 ** (bellDb(k * RATE / size, centre, width, gain, rest) / 20);
        re[k] = re0[k] * factor; im[k] = im0[k] * factor;
        if (k > 0 && k < size / 2) { re[size - k] = re[k]; im[size - k] = -im[k]; }
    }
    fft(re, im, true);
    const out = new Float32Array(n);
    let peak = 0;
    for (let i = 0; i < n; i++) { out[i] = re[lead + i] / size; peak = Math.max(peak, Math.abs(out[i])); }
    // never louder than full scale: quieter as a whole instead of clipped
    if (peak > 0.99) for (let i = 0; i < n; i++) out[i] *= 0.99 / peak;
    postMessage({ out, id }, [out.buffer]);
};`;

    function setUp(root) {
        let samples = null, worker = null, asked = 0;
        let centre = 1000, width = 1, gain = 12, rest = 0, on = true;
        let ctx = null, voice = null, analyser = null, playing = false, startedAt = 0, offset = 0;
        let current = null;                    // the filtered piece, Float32Array

        // a slider with marks under it: [value, label], each a tick at the
        // place the thumb's middle has at that value (the thumb is 1rem wide)
        const slider = (label, min, max, step, value, marks, show, onInput) => {
            const id = `eq-${label}-${Math.random().toString(36).slice(2, 8)}`;
            const input = el('input', { type: 'range', class: 'form-range eq-slider', id, min, max, step, value });
            const out = el('span', { class: 'eq-value' });
            const scale = el('div', { class: 'eq-scale', 'aria-hidden': 'true' });
            for (const [v, text] of marks) {
                const p = (v - min) / (max - min);
                const mark = el('span', { class: 'eq-mark', text });
                mark.style.left = `calc(${(p * 100).toFixed(2)}% + ${(0.5 - p).toFixed(3)}rem)`;
                scale.append(mark);
            }
            const update = () => { out.textContent = show(Number(input.value)); };
            input.addEventListener('input', () => { update(); onInput(Number(input.value)); });
            update();
            return el('div', { class: 'eq-row' }, [el('label', { class: 'eq-label', for: id, text: label }),
                el('div', { class: 'eq-track' }, [input, scale]), out]);
        };
        // the centre's slider runs from 0 to 1000 over a logarithmic scale
        const toCentre = (v) => CENTRE0 * (CENTRE1 / CENTRE0) ** (v / 1000);
        const fromCentre = (f) => Math.round(1000 * Math.log(f / CENTRE0) / Math.log(CENTRE1 / CENTRE0));
        const hz = (f) => (f >= 1000 ? `${de(f / 1000, f < 10000 ? 1 : 0)} kHz` : `${de(Math.round(f))} Hz`);

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary eq-play' });
        const showPlay = () => playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }), playing ? ' Anhalten' : ' Abspielen');
        showPlay();
        const switchId = `eq-on-${Math.random().toString(36).slice(2, 8)}`;
        const onSwitch = el('input', { type: 'checkbox', class: 'form-check-input', id: switchId, role: 'switch' });
        onSwitch.checked = true;
        const chart = el('canvas', { class: 'eq-chart', role: 'img' });
        const line = el('p', { class: 'eq-line', 'aria-live': 'polite' });
        root.append(
            el('div', { class: 'eq-controls' }, [playButton,
                el('div', { class: 'form-check form-switch eq-switch' }, [onSwitch, el('label', { class: 'form-check-label', for: switchId, text: 'EQ an' })])]),
            slider('Frequenz', 0, 1000, 1, fromCentre(centre),
                [[50, '50'], [100, '100'], [200, '200'], [500, '500'], [1000, '1k'], [2000, '2k'], [5000, '5k'], [10000, '10k']]
                    .map(([f, text]) => [fromCentre(f), text]),
                (v) => hz(toCentre(v)), (v) => { centre = toCentre(v); changed(); }),
            slider('Breite', 0.1, 4, 0.1, width, [[0.5, '½'], [1, '1'], [2, '2'], [3, '3'], [4, '4']],
                (v) => `${de(v, 1)} Oktaven`, (v) => { width = v; changed(); }),
            slider('Verstärkung', -GAIN, GAIN, 1, gain, [[-24, '−24'], [-12, '−12'], [0, '0'], [12, '+12'], [24, '+24']],
                (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)} dB`, (v) => { gain = v; changed(); }),
            slider('Übrige', -GAIN, 0, 1, rest, [[-24, '−24'], [-12, '−12'], [0, '0']],
                (v) => `${v < 0 ? '−' : ''}${Math.abs(v)} dB`, (v) => { rest = v; changed(); }),
            el('figure', { class: 'eq-figure' }, [
                el('figcaption', { text: 'Lila: wie viel dB das EQ jeder Frequenz gibt. Grau: das Spektrum, das gerade spielt.' }),
                chart]),
            line);
        onSwitch.addEventListener('change', () => { on = onSwitch.checked; changed(); });

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (name) => s.getPropertyValue(name).trim();
            return { ink: get('--eq-ink'), quiet: get('--eq-quiet'), mark: get('--eq-mark'), ground: get('--eq-ground'), sound: get('--eq-sound') };
        };
        const logPos = (f) => Math.log(f / F0) / Math.log(F1 / F0);

        function draw() {
            const w = chart.clientWidth, h = chart.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return;
            if (chart.width !== Math.round(w * dpr)) { chart.width = Math.round(w * dpr); chart.height = Math.round(h * dpr); }
            const g = chart.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            g.clearRect(0, 0, w, h);
            const col = colours();
            const plotW = w - LABEL_W, plotH = h - AXIS;
            const xOf = (f) => LABEL_W + logPos(f) * plotW;
            const yOf = (db) => plotH / 2 - (db / GAIN) * (plotH / 2 - 6);
            g.fillStyle = col.ground;
            g.fillRect(LABEL_W, 0, plotW, plotH);
            g.font = '12px system-ui, sans-serif';
            g.textAlign = 'right'; g.textBaseline = 'middle';
            for (const db of [GAIN, 12, 0, -12, -GAIN]) {
                const y = Math.round(yOf(db)) + 0.5;
                g.strokeStyle = col.quiet; g.lineWidth = db === 0 ? 1.5 : 1;
                g.beginPath(); g.moveTo(LABEL_W, y); g.lineTo(w, y); g.stroke();
                g.fillStyle = col.ink;
                g.fillText(`${db > 0 ? '+' : db < 0 ? '−' : ''}${Math.abs(db)} dB`, LABEL_W - 6, Math.min(plotH - 6, Math.max(6, y)));
            }
            g.textAlign = 'center'; g.textBaseline = 'bottom';
            for (const f of plotW < 420 ? [100, 1000, 10000] : LABELS) {
                g.fillText(f >= 1000 ? `${de(f / 1000)} kHz` : `${f} Hz`, Math.min(w - 22, Math.max(LABEL_W + 20, xOf(f))), h);
            }
            // the spectrum that is playing, grey, from −90 to 0 dB over the whole height
            if (analyser && playing) {
                const bins = new Float32Array(analyser.frequencyBinCount);
                analyser.getFloatFrequencyData(bins);
                const binHz = ctx.sampleRate / analyser.fftSize;
                g.beginPath();
                g.moveTo(LABEL_W, plotH);
                for (let px = 0; px <= plotW; px += 2) {
                    const f = F0 * (F1 / F0) ** (px / plotW);
                    const db = bins[Math.min(bins.length - 1, Math.round(f / binHz))];
                    g.lineTo(LABEL_W + px, plotH - Math.max(0, Math.min(1, (db + 90) / 90)) * plotH);
                }
                g.lineTo(LABEL_W + plotW, plotH);
                g.closePath();
                g.fillStyle = col.sound; g.globalAlpha = 0.35; g.fill(); g.globalAlpha = 1;
            }
            // the bell
            g.beginPath();
            for (let px = 0; px <= plotW; px++) {
                const f = F0 * (F1 / F0) ** (px / plotW);
                const y = yOf(on ? bellDb(f, centre, width, gain, rest) : 0);
                if (px === 0) g.moveTo(LABEL_W, y); else g.lineTo(LABEL_W + px, y);
            }
            g.strokeStyle = col.mark; g.lineWidth = 2.5; g.stroke();
            const signed = (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)} dB`;
            line.textContent = !on ? 'Das EQ ist aus: die Musik, wie sie ist.'
                : rest === 0
                    ? `Um ${hz(centre)} ${signed(gain)}, die Schwingung dort mal ${de(10 ** (gain / 20), 2)}; ${de(width, 1)} Oktaven daneben noch die Hälfte.`
                    : `Um ${hz(centre)} ${signed(gain)}, weit weg ${signed(rest)}; ${de(width, 1)} Oktaven daneben genau dazwischen.`;
            chart.setAttribute('aria-label', line.textContent);
        }

        // where the piece is at a time on the audio clock
        function positionAt(time) {
            if (!playing) return offset;
            const d = samples.length / RATE;
            return ((offset + Math.max(0, time - startedAt)) % d + d) % d;
        }
        const position = () => positionAt(ctx ? ctx.currentTime : 0);
        function fadeOut(old, when) {
            if (!old) return;
            old.gain.gain.cancelScheduledValues(when);
            old.gain.gain.setValueAtTime(1, when);
            old.gain.gain.linearRampToValueAtTime(0, when + FADE);
            old.source.stop(when + FADE + 0.01);
            setTimeout(() => { old.source.disconnect(); old.gain.disconnect(); }, (when - ctx.currentTime + FADE) * 1000 + 200);
        }
        function start() {
            const AC = window.AudioContext || window.webkitAudioContext;
            if (!AC || !current) return;
            if (!ctx) {
                ctx = new AC();
                analyser = ctx.createAnalyser();
                analyser.fftSize = 4096;
                analyser.smoothingTimeConstant = 0.6;
                analyser.connect(ctx.destination);
            }
            if (ctx.state === 'suspended') ctx.resume();
            // the new sound starts exactly where the old one will be then,
            // and the two cross over: the same piece, aligned to the sample
            const when = ctx.currentTime + 0.03;
            const place = positionAt(when);
            const buffer = ctx.createBuffer(1, current.length, RATE);
            buffer.copyToChannel(current, 0);
            const gainNode = ctx.createGain();
            gainNode.gain.setValueAtTime(0, when);
            gainNode.gain.linearRampToValueAtTime(1, when + FADE);
            gainNode.connect(analyser);
            const source = ctx.createBufferSource();
            source.buffer = buffer;
            source.loop = true;
            source.connect(gainNode);
            source.start(when, place);
            fadeOut(voice, when);
            voice = { source, gain: gainNode };
            offset = place; startedAt = when;
            if (!playing) { playing = true; showPlay(); requestAnimationFrame(tick); }
        }
        function stop() {
            offset = position();
            if (voice) fadeOut(voice, ctx.currentTime);
            voice = null;
            playing = false;
            showPlay();
            draw();
        }
        function tick() {
            if (!playing) return;
            draw();
            requestAnimationFrame(tick);
        }
        // a slider moved: the curve at once, the sound as soon as the
        // worker has the last one (also while dragging), always the last setting
        let busy = false, again = false;
        function ask() {
            if (!on || (gain === 0 && rest === 0)) { current = samples; if (playing) start(); return; }
            busy = true;
            worker.postMessage({ centre, width, gain, rest, id: ++asked });
        }
        function changed() {
            draw();
            if (!worker) return;
            if (busy) { again = true; return; }
            ask();
        }
        function heard(e) {
            busy = false;
            current = e.data.out;
            if (playing) start();
            if (again) { again = false; ask(); }
        }
        playButton.addEventListener('click', async () => {
            if (playing) { stop(); return; }
            if (!samples) {
                playButton.disabled = true;
                try {
                    samples = await load();
                    worker = new Worker(URL.createObjectURL(new Blob([WORKER], { type: 'text/javascript' })));
                    worker.onmessage = heard;
                    worker.postMessage({ samples });
                    current = samples;
                    if (on && (gain !== 0 || rest !== 0)) {
                        // the first sound waits for the worker once
                        await new Promise((done) => {
                            worker.onmessage = (e) => { worker.onmessage = heard; current = e.data.out; done(); };
                            worker.postMessage({ centre, width, gain, rest, id: ++asked });
                        });
                    }
                } catch (e) {
                    line.textContent = 'Die Musik ließ sich nicht laden.';
                    return;
                } finally {
                    playButton.disabled = false;
                }
            }
            start();
        });
        if (typeof ResizeObserver !== 'undefined') new ResizeObserver(draw).observe(chart);
        draw();
    }

    const go = () => document.querySelectorAll('.eq-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
