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
// The effects (EFFECTS below): eq, echo, chorus, reverb. Each has its knobs, its picture
// and its function in WORKER, written like the Python of the page.
(function () {
    'use strict';
    const FADE = 0.03;
    const LABEL_W = 46, AXIS = 18;
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    const signedDb = (v) => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v)} dB`;
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
        schlagzeug: { label: 'Schlagzeug', url: '/audio/musik/snare.flac' },
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

    const EFFECTS = {
        eq: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['musik', 'stimme'],
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
            neutral: (p) => p.gain === 0 && p.rest === 0,
            caption: 'Lila Linie: wie viel dB das EQ jeder Frequenz gibt. Graue Linie: das Spektrum des Klangs ohne EQ, lila Fläche: mit EQ.',
            line: (p) => (p.rest === 0
                ? `Um ${hz(p.centre)} ${signedDb(p.gain)}, die Schwingung dort mal ${de(10 ** (p.gain / 20), 1)}; ${de(p.width, 1)} Oktaven daneben noch die Hälfte.`
                : `Um ${hz(p.centre)} ${signedDb(p.gain)}, weit weg ${signedDb(p.rest)}; ${de(p.width, 1)} Oktaven daneben genau dazwischen.`),
            draw: drawEq,
        },
        echo: {
            chip: 'Zum Ausprobieren: abspielen und regeln',
            clips: ['stimme', 'schlagzeug', 'musik'],
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
            clips: ['melodie', 'stimme', 'musik'],
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
            clips: ['schlagzeug', 'stimme', 'melodie'],
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
        const factor = 10 ** (bellDb(k * clip.rate / size, p.centre, p.width, p.gain, p.rest) / 20);
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
const COMBS = ${JSON.stringify(COMBS)}, ALLPASS = ${JSON.stringify(ALLPASS)};
const EFFECTS = { echo, chorus, reverb };
onmessage = (e) => {
    const m = e.data;
    if (m.load) { clips[m.load] = { x: m.x, rate: m.rate }; return; }
    const clip = clips[m.clip];
    let out;
    if (m.effect === 'eq') out = eq(clip, m.params);
    else if (m.twice) {
        // the sound twice in a row; the second time, with what the first leaves over
        const n = clip.x.length, x2 = new Float32Array(2 * n);
        x2.set(clip.x); x2.set(clip.x, n);
        out = EFFECTS[m.effect](x2, clip.rate, m.params).slice(n);
    } else out = EFFECTS[m.effect](clip.x, clip.rate, m.params);
    // never louder than full scale: quieter as a whole instead of clipped
    let peak = 0;
    for (let i = 0; i < out.length; i++) peak = Math.max(peak, Math.abs(out[i]));
    if (peak > 0.99) for (let i = 0; i < out.length; i++) out[i] *= 0.99 / peak;
    postMessage({ out, id: m.id }, [out.buffer]);
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
            const y = yOf(on ? bellDb(f, p.centre, p.width, p.gain, p.rest) : 0);
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

    // ------------------------------------------------------------------
    function setUp(root) {
        const effect = EFFECTS[root.dataset.effect] || EFFECTS.eq;
        const name = root.dataset.effect in EFFECTS ? root.dataset.effect : 'eq';
        let clipId = effect.clips.includes(root.dataset.clip) ? root.dataset.clip : effect.clips[0];
        const params = {};
        let on = true;
        let ctx = null, output = null, voice = null, playing = false, startedAt = 0, offset = 0;
        let current = null, original = null, rate = 44100;
        const loaded = {}, peaks = {};           // clip: Float32Array, its largest value
        let smooth = null;                       // the EQ's spectra, smoothed over time
        let worker = null, busy = false, again = false, asked = 0;
        const uid = Math.random().toString(36).slice(2, 8);
        root.dataset.chip = effect.chip;

        // a slider with marks under it, each centred on the thumb's place (the thumb is 1rem wide)
        const resets = [];
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
            update();
            return el('div', { class: 'fx-row' }, [el('label', { class: 'fx-label', for: id, text: knob.label }),
                el('div', { class: 'fx-track' }, [input, scale]), out]);
        };

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary fx-play' });
        const showPlay = () => playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }), playing ? ' Anhalten' : ' Abspielen');
        showPlay();
        const resetButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary fx-reset' },
            [el('i', { class: 'bi bi-arrow-counterclockwise' }), ' Zurücksetzen']);
        resetButton.addEventListener('click', () => { resets.forEach((r) => r()); changed(); });
        const onSwitch = el('input', { type: 'checkbox', class: 'form-check-input', id: `fx-on-${uid}`, role: 'switch' });
        onSwitch.checked = true;
        const clipGroup = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': 'Klang' });
        for (const id of effect.clips) {
            const input = el('input', { type: 'radio', class: 'btn-check', name: `fx-clip-${uid}`, id: `fx-clip-${id}-${uid}`, autocomplete: 'off' });
            input.checked = id === clipId;
            input.addEventListener('change', () => { if (input.checked) pickClip(id); });
            clipGroup.append(input, el('label', { class: 'btn btn-outline-secondary', for: `fx-clip-${id}-${uid}`, text: CLIPS[id].label }));
        }
        const chart = el('canvas', { class: 'fx-chart', role: 'img' });
        const line = el('p', { class: 'fx-line', 'aria-live': 'polite' });
        const wave = el('canvas', { class: 'fx-wave', role: 'img', 'aria-label': 'Der Klang ohne und mit Effekt, vier Sekunden um die Stelle, die gerade spielt.' });
        root.append(
            el('div', { class: 'fx-controls' }, [
                ...(effect.clips.length > 1 ? [clipGroup] : []), playButton, resetButton,
                el('div', { class: 'form-check form-switch fx-switch' }, [onSwitch, el('label', { class: 'form-check-label', for: `fx-on-${uid}`, text: 'Effekt an' })])]),
            ...effect.knobs.map(slider),
            el('figure', { class: 'fx-figure' }, [el('figcaption', { text: effect.caption }), chart]),
            line,
            el('figure', { class: 'fx-figure' }, [el('figcaption', { text: 'Der Klang, vier Sekunden um die Stelle, die gerade spielt (Strich): oben ohne Effekt, unten mit.' }), wave]));
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
            if (name === 'eq' && original) {
                const at = sampleNow();
                smooth = { before: spectrum(original, at, smooth && smooth.before), after: spectrum(current || original, at, smooth && smooth.after) };
                live = { ...smooth, binHz: rate / N };
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
            if (!original) {
                g.fillStyle = col.ink; g.textAlign = 'center'; g.textBaseline = 'middle';
                g.fillText('Spiel den Klang ab, dann erscheint er hier.', w / 2, h / 2);
                return;
            }
            const gap = 6, laneH = (h - gap) / 2, plotW = w - LABEL_W;
            const span = WAVE_SECONDS * rate, from = sampleNow() - span / 2;
            const scale = 0.95 / peaks[clipId];
            [['ohne', original, col.sound], ['mit', on ? current || original : original, on ? col.mark : col.sound]].forEach(([label, x, colour], k) => {
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
            if (!playing) { playing = true; showPlay(); requestAnimationFrame(tick); }
        }
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
            if (name === 'eq') draw();
            drawWave();
            requestAnimationFrame(tick);
        }

        // a knob moved: the picture at once, the sound as soon as the worker
        // has the last one (also while dragging), always the last setting
        function ask() {
            if (!on || effect.neutral(params)) { current = original; if (playing) start(); return; }
            busy = true;
            worker.postMessage({ effect: name, clip: clipId, params: { ...params }, twice: !!effect.twice, id: ++asked });
        }
        function changed() {
            draw(); drawWave();
            if (!worker || !original) return;
            if (busy) { again = true; return; }
            ask();
        }
        function heard(e) {
            busy = false;
            current = e.data.out;
            if (playing) start(); else drawWave();
            if (again) { again = false; ask(); }
        }
        async function loadClip(id) {
            if (!loaded[id]) {
                const response = await fetch(CLIPS[id].url);
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
            original = await loadClip(clipId);
            current = original;
            if (on && !effect.neutral(params)) {
                // the first sound of a clip waits for the worker once
                await new Promise((done) => {
                    worker.onmessage = (e) => { worker.onmessage = heard; current = e.data.out; done(); };
                    worker.postMessage({ effect: name, clip: clipId, params: { ...params }, twice: !!effect.twice, id: ++asked });
                });
            }
        }
        async function pickClip(id) {
            clipId = id;
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
