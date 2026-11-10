// What the ear hears (»Audio komprimieren«), two experiments to do with
// headphones:
//
//   <div class='hearing-widget' data-kind='threshold'></div>
//   <div class='hearing-widget' data-kind='masking'></div>
//
// threshold: a reference tone first, to set the device's volume once; then
// a beeping tone at one of nine frequencies, made quieter with a slider
// until it can no longer be heard, and that level noted. The notes are
// drawn over the threshold in quiet after Terhardt, laid on the note at
// 1.000 Hz (a browser knows nothing of the volume in the ear, so only the
// shape can be compared).
//
// masking: a band of noise from 920 to 1.080 Hz, and a quiet beeping tone
// beside it, made quieter until the noise covers it; noted the same way and
// drawn over a model of the masking threshold (the spreading function of
// Schroeder, Atal and Hall over Zwicker's Bark scale, 5 dB under the noise).
//
// Levels are in dB of the loudest a file can hold (dBFS), as RMS; nothing
// plays louder than -20 dBFS.
(function () {
    'use strict';
    const RATE = 44100;
    const TOP = -20;                          // the loudest anything plays
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    const dbText = (db) => `${db < 0 ? '−' : ''}${de(Math.abs(db))} dB`;
    const fText = (f) => `${de(f)} Hz`;
    let counter = 0;
    let ctx = null;

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

    // the threshold in quiet after Terhardt (1979), in dB SPL
    const terhardt = (f) => {
        const k = f / 1000;
        return 3.64 * k ** -0.8 - 6.5 * Math.exp(-0.6 * (k - 3.3) ** 2) + 0.001 * k ** 4;
    };
    // Zwicker's Bark scale and the spreading function of Schroeder, Atal and Hall (1979)
    const bark = (f) => 13 * Math.atan(0.00076 * f) + 3.5 * Math.atan((f / 7500) ** 2);
    const spread = (dz) => 15.81 + 7.5 * (dz + 0.474) - 17.5 * Math.sqrt(1 + (dz + 0.474) ** 2);

    function audio() {
        const AC = window.AudioContext || window.webkitAudioContext;
        if (!AC) return null;
        if (!ctx) ctx = new AC();
        if (ctx.state === 'suspended') ctx.resume();
        return ctx;
    }

    // a beep: 0,4 s of a sine with ramps of 20 ms, then 0,4 s of silence, looped;
    // RMS 1 at the top (amplitude √2), so that the gain is the RMS level
    const beeps = {};
    function beep(freq) {
        if (beeps[freq]) return beeps[freq];
        const c = audio();
        const buffer = c.createBuffer(1, Math.round(0.8 * RATE), RATE);
        const data = buffer.getChannelData(0);
        const on = Math.round(0.4 * RATE), ramp = Math.round(0.02 * RATE);
        for (let i = 0; i < on; i++) {
            const env = Math.min(1, i / ramp, (on - 1 - i) / ramp);
            data[i] = Math.SQRT2 * env * Math.sin(2 * Math.PI * freq * i / RATE);
        }
        return (beeps[freq] = buffer);
    }
    // the noise: 2 s of sines every 1 Hz from 920 to 1.080 Hz with random phases (a seeded
    // generator, the same noise every time), RMS 1, so that it loops without a click
    let noise = null;
    function noiseBuffer() {
        if (noise) return noise;
        const c = audio();
        const n = 2 * RATE;
        const data = new Float32Array(n);
        let seed = 7;
        const random = () => ((seed = (seed * 16807) % 2147483647) / 2147483647);
        for (let f = 920; f <= 1080; f += 1) {
            const phase = 2 * Math.PI * random(), step = 2 * Math.PI * f / RATE;
            for (let i = 0; i < n; i++) data[i] += Math.sin(phase + step * i);
        }
        let sum = 0;
        for (let i = 0; i < n; i++) sum += data[i] * data[i];
        const scale = 1 / Math.sqrt(sum / n);
        noise = c.createBuffer(1, n, RATE);
        const out = noise.getChannelData(0);
        for (let i = 0; i < n; i++) out[i] = data[i] * scale;
        return noise;
    }

    function setUp(root) {
        const kind = root.dataset.kind === 'masking' ? 'masking' : 'threshold';
        const id = `hearing-${++counter}`;
        const FREQS = kind === 'threshold'
            ? [125, 250, 500, 1000, 2000, 4000, 8000, 12000, 16000]
            : [500, 700, 850, 1200, 1400, 2000, 3000];
        const MASKER = TOP;                            // the noise's level
        const range = kind === 'threshold' ? [-100, TOP] : [-80, TOP];
        const x0 = kind === 'threshold' ? 80 : 300, x1 = kind === 'threshold' ? 20000 : 4000;
        let freq = kind === 'threshold' ? 1000 : 1400;
        let level = -40;
        let masker = true;
        const notes = new Map();                       // frequency → the level noted
        let playing = null;                            // the beep that plays: a function that stops it, with its gain
        let refStop = null;                            // the reference tone, while it plays

        const group = (name, label, items, checked, onPick) => {
            const box = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': label });
            items.forEach((item) => {
                const inputId = `${id}-${name}-${item.value}`;
                const input = el('input', { type: 'radio', class: 'btn-check', name: `${id}-${name}`, id: inputId, autocomplete: 'off' });
                if (item.value === checked) input.checked = true;
                input.addEventListener('change', () => { if (input.checked) onPick(item.value); });
                box.append(input, el('label', { class: 'btn btn-outline-secondary', for: inputId, text: item.label }));
            });
            return el('div', { class: 'hearing-group' }, [el('span', { class: 'hearing-label', text: label }), box]);
        };

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' });
        const noteButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' },
            [el('i', { class: 'bi bi-pin-angle' }), kind === 'threshold' ? ' Nicht mehr zu hören: merken' : ' Verdeckt: merken']);
        const clearButton = el('button', { type: 'button', class: 'btn btn-sm btn-link hearing-clear', text: 'Alles vergessen' });
        const showPlay = () => playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }),
            playing ? ' Anhalten' : ' Abspielen');
        showPlay();
        const slider = el('input', { type: 'range', class: 'form-range hearing-slider', id: `${id}-level`,
            min: String(range[0]), max: String(range[1]), step: '1', value: String(level) });
        const levelText = el('span', { class: 'hearing-level' });
        const chart = el('canvas', { class: 'hearing-chart', role: 'img' });
        const line = el('p', { class: 'hearing-line', 'aria-live': 'polite' });

        if (kind === 'threshold') {
            const refButton = el('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary' },
                [el('i', { class: 'bi bi-volume-down' }), ' Bezugston: 1.000 Hz, −30 dB']);
            refButton.addEventListener('click', () => {
                stop();
                if (refStop) { refStop(); return; }
                refStop = start(1000, -30, false, () => { refStop = null; });
                setTimeout(() => { if (refStop) refStop(); }, 4000);
            });
            root.append(el('div', { class: 'hearing-step' }, [
                el('p', { text: 'Erst die Lautstärke: Spiel den Bezugston ab und stell dein Gerät so ein, dass du ihn leise, aber deutlich hörst. Danach drehst du nicht mehr daran.' }),
                refButton]));
        }
        root.append(
            el('div', { class: 'hearing-controls' }, [
                group('freq', kind === 'threshold' ? 'Ton' : 'Leiser Ton', FREQS.map((f) => ({ value: String(f), label: de(f) })), String(freq),
                    (v) => { freq = Number(v); level = -40; slider.value = String(level); if (playing) restart(); draw(); }),
            ]),
            kind === 'masking'
                ? el('div', { class: 'hearing-controls' }, [group('noise', 'Rauschen um 1.000 Hz', [{ value: 'on', label: 'an' }, { value: 'off', label: 'aus' }], 'on',
                    (v) => { masker = v === 'on'; if (playing) restart(); draw(); })])
                : '',
            el('div', { class: 'hearing-controls' }, [
                el('label', { class: 'hearing-label', for: slider.id, text: 'Pegel' }), slider, levelText]),
            el('div', { class: 'hearing-controls' }, [playButton, noteButton, clearButton]),
            el('figure', { class: 'hearing-figure' }, [chart, el('div', { class: 'hearing-legend' }, [
                el('span', { class: 'hearing-l-notes' }, [el('i'), kind === 'threshold' ? 'deine Messungen' : 'wo der Ton für dich verschwand']),
                el('span', { class: 'hearing-l-model' }, [el('i'), kind === 'threshold'
                    ? 'Hörschwelle nach Terhardt, an deine Messung bei 1.000 Hz gelegt'
                    : 'Mithörschwelle nach einem Modell']),
                ...(kind === 'masking' ? [el('span', { class: 'hearing-l-noise' }, [el('i'), 'das Rauschen'])] : []),
            ])]),
            line);

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (name) => s.getPropertyValue(name).trim();
            return { ink: get('--hearing-ink'), quiet: get('--hearing-quiet'), mark: get('--hearing-mark'),
                model: get('--hearing-model'), ground: get('--hearing-ground'), noise: get('--hearing-noise') };
        };

        // sound ------------------------------------------------------------------
        // start a beep at f with level dB (and the noise, if asked); returns a function that stops it
        function start(f, db, withNoise, onEnd) {
            const c = audio();
            if (!c) return null;
            const gain = c.createGain();
            gain.gain.value = 10 ** (Math.min(TOP, db) / 20);
            gain.connect(c.destination);
            const tone = c.createBufferSource();
            tone.buffer = beep(f);
            tone.loop = true;
            tone.connect(gain);
            tone.start();
            const sources = [tone];
            let noiseGain = null;
            if (withNoise) {
                noiseGain = c.createGain();
                noiseGain.gain.value = 10 ** (MASKER / 20);
                noiseGain.connect(c.destination);
                const n = c.createBufferSource();
                n.buffer = noiseBuffer();
                n.loop = true;
                n.connect(noiseGain);
                n.start();
                sources.push(n);
            }
            const stopIt = () => {
                sources.forEach((s) => { try { s.stop(); } catch (e) { /* stopped */ } s.disconnect(); });
                gain.disconnect();
                if (noiseGain) noiseGain.disconnect();
                if (onEnd) onEnd();
            };
            return Object.assign(stopIt, { gain });
        }
        function restart() {
            stop();
            if (refStop) refStop();
            const s = start(freq, level, kind === 'masking' && masker);
            if (s) playing = s;
            showPlay();
        }
        function stop() {
            if (playing) { playing(); playing = null; }
            showPlay();
        }

        playButton.addEventListener('click', () => { if (playing) stop(); else restart(); });
        slider.addEventListener('input', () => {
            level = Number(slider.value);
            if (playing) playing.gain.gain.setTargetAtTime(10 ** (Math.min(TOP, level) / 20), ctx.currentTime, 0.03);
            draw();
        });
        noteButton.addEventListener('click', () => { notes.set(freq, level); draw(); });
        clearButton.addEventListener('click', () => { notes.clear(); draw(); });

        // the chart --------------------------------------------------------------
        const LEFT = 50, AXIS = 20;
        function draw() {
            levelText.textContent = dbText(level);
            const w = chart.clientWidth, h = chart.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return;
            if (chart.width !== Math.round(w * dpr) || chart.height !== Math.round(h * dpr)) {
                chart.width = Math.round(w * dpr); chart.height = Math.round(h * dpr);
            }
            const g = chart.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            g.clearRect(0, 0, w, h);
            const col = colours();
            const plotW = w - LEFT - 8, plotH = h - AXIS;
            const xOf = (f) => LEFT + Math.log(f / x0) / Math.log(x1 / x0) * plotW;
            const yOf = (db) => (range[1] - db) / (range[1] - range[0]) * (plotH - 18) + 9;
            g.fillStyle = col.ground;
            g.fillRect(LEFT, 0, plotW, plotH);
            g.font = '12px system-ui, sans-serif';
            g.textAlign = 'right'; g.textBaseline = 'middle';
            for (let db = range[1]; db >= range[0]; db -= 20) {
                const y = Math.round(yOf(db)) + 0.5;
                g.strokeStyle = col.quiet; g.lineWidth = 1;
                g.beginPath(); g.moveTo(LEFT, y); g.lineTo(LEFT + plotW, y); g.stroke();
                g.fillStyle = col.ink; g.fillText(dbText(db), LEFT - 6, y);
            }
            g.textAlign = 'center'; g.textBaseline = 'bottom';
            const ticks = kind === 'threshold'
                ? (plotW < 420 ? [100, 1000, 10000] : [100, 250, 500, 1000, 2000, 4000, 8000, 16000])
                : (plotW < 420 ? [500, 1000, 2000] : [300, 500, 700, 1000, 1400, 2000, 3000]);
            for (const f of ticks) {
                const x = Math.min(w - 24, Math.max(LEFT + 18, xOf(f)));
                g.fillText(f >= 1000 ? `${de(f / 1000, f % 1000 ? 1 : 0)} kHz` : `${f} Hz`, x, h);
            }
            g.save();
            g.beginPath(); g.rect(LEFT, 0, plotW, plotH); g.clip();
            // the model: Terhardt on the note at 1 kHz, or the masking threshold of the noise
            let model = null;
            if (kind === 'threshold') {
                const at1k = notes.has(1000) ? notes.get(1000) : null;
                if (at1k !== null) model = (f) => terhardt(f) - terhardt(1000) + at1k;
            } else if (masker) {
                model = (f) => MASKER - 5 + spread(bark(f) - bark(1000));
                // the noise as a band
                g.fillStyle = col.noise; g.globalAlpha = 0.35;
                g.fillRect(xOf(920), yOf(MASKER), xOf(1080) - xOf(920), plotH - yOf(MASKER));
                g.globalAlpha = 1;
            }
            if (model) {
                g.strokeStyle = col.model; g.lineWidth = 2; g.setLineDash([6, 4]);
                g.beginPath();
                for (let px = 0; px <= plotW; px += 2) {
                    const f = x0 * (x1 / x0) ** (px / plotW);
                    const y = yOf(model(f));
                    px === 0 ? g.moveTo(LEFT + px, y) : g.lineTo(LEFT + px, y);
                }
                g.stroke(); g.setLineDash([]);
            }
            // the notes, joined
            const list = [...notes.entries()].sort((a, b) => a[0] - b[0]);
            g.strokeStyle = col.mark; g.fillStyle = col.mark; g.lineWidth = 2;
            if (list.length > 1) {
                g.beginPath();
                list.forEach(([f, db], i) => (i ? g.lineTo(xOf(f), yOf(db)) : g.moveTo(xOf(f), yOf(db))));
                g.stroke();
            }
            for (const [f, db] of list) { g.beginPath(); g.arc(xOf(f), yOf(db), 4.5, 0, 2 * Math.PI); g.fill(); }
            // the tone now: a ring at its frequency and level
            g.strokeStyle = col.mark; g.lineWidth = 2;
            g.beginPath(); g.arc(xOf(freq), yOf(level), 7, 0, 2 * Math.PI); g.stroke();
            g.restore();

            const noted = notes.size ? `Gemerkt: ${list.map(([f, db]) => `${fText(f)} bei ${dbText(db)}`).join(', ')}.` : '';
            line.textContent = `${fText(freq)} mit ${dbText(level)}${kind === 'masking' ? (masker ? ', Rauschen an' : ', Rauschen aus') : ''}. ${noted}`;
            chart.setAttribute('aria-label', line.textContent);
        }

        new ResizeObserver(() => draw()).observe(root);
        new MutationObserver(() => draw()).observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
        // a widget out of sight does not keep playing
        new IntersectionObserver((entries) => { if (!entries[0].isIntersecting) stop(); }).observe(root);
        draw();
    }

    const go = () => document.querySelectorAll('.hearing-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
