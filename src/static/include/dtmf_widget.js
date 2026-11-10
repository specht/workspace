// The keys of a telephone as sound (»Ein Spiel am Telefon«): every key is
// two tones at once, one for its row and one for its column (DTMF, the
// table the emulator plays, src/phone/emulator/page.html, TONES). Pressing a
// key plays both for as long as it is held (at least 150 ms, as the phone
// does); beside the keypad the two tones and their sum as a wave over 10 ms,
// and a waterfall from the browser's own FFT (an AnalyserNode): time to the
// right, frequency upwards, so that a number dialled shows as pairs of
// dashes. »Alle Tasten« plays them in order; the riddle plays three keys
// without showing them, to be read off the waterfall.
//
//   <script src='/include/dtmf_widget.js'></script>
//   <div class='dtmf-widget'></div>
//
// Keys: the digits, * and # on the keyboard while the widget has the focus.
// The waterfall only runs while it has something to show and is on screen.
(function () {
    'use strict';
    const ROWS = [697, 770, 852, 941];
    const COLS = [1209, 1336, 1477];
    const KEYS = [['1', '2', '3'], ['4', '5', '6'], ['7', '8', '9'], ['*', '0', '#']];
    const LOW = 600, HIGH = 1600;           // the waterfall's frequencies
    const WINDOW = 0.010;                   // the wave: 10 ms
    const SLOW = 400;                       // while a key sounds the wave runs, this many times slower
    const still = window.matchMedia('(prefers-reduced-motion: reduce)');
    const MIN_TONE = 0.15, LEVEL = 0.18;    // as the phone: 150 ms; each tone's amplitude
    const FLOOR = -95, TOP = -35;           // dB from the analyser, dark to bright

    const tonesOf = (key) => {
        for (let r = 0; r < 4; r++) {
            const c = KEYS[r].indexOf(key);
            if (c >= 0) return [ROWS[r], COLS[c], r, c];
        }
        return null;
    };
    const hz = (f) => f.toLocaleString('de-DE') + ' Hz';
    const el = (tag, cls, text) => {
        const node = document.createElement(tag);
        if (cls) node.className = cls;
        if (text != null) node.textContent = text;
        return node;
    };

    function setUp(root) {
        let ctx = null, analyser = null, out = null, bins = null;
        let sounding = null;                 // { key, oscs, gain, started, release }
        let shown = '5';                     // the key the wave shows
        let hidden = false, riddle = null, busy = false;
        let columns = [], running = false, quietFrames = 0, visible = true;
        let waveTime = 0, lastFrame = 0;         // where the wave's window starts, in seconds

        // the keypad, with the frequencies of its rows and columns
        const pad = el('div', 'dtmf-pad');
        pad.appendChild(el('span', 'dtmf-corner', 'Hz'));
        COLS.forEach((f, c) => { const s = el('span', 'dtmf-freq dtmf-col', f.toLocaleString('de-DE')); s.dataset.col = c; pad.appendChild(s); });
        const buttons = {};
        KEYS.forEach((row, r) => {
            const s = el('span', 'dtmf-freq dtmf-row', ROWS[r].toLocaleString('de-DE'));
            s.dataset.row = r;
            pad.appendChild(s);
            row.forEach((key, c) => {
                const b = el('button', 'dtmf-key', key);
                b.type = 'button';
                b.setAttribute('aria-label', `Taste ${key}: ${hz(ROWS[r])} und ${hz(COLS[c])}`);
                b.dataset.key = key;
                buttons[key] = b;
                pad.appendChild(b);
            });
        });

        const wave = el('canvas', 'dtmf-wave');
        wave.setAttribute('role', 'img');
        const fall = el('canvas', 'dtmf-fall');
        fall.setAttribute('role', 'img');
        fall.setAttribute('aria-label', 'Wasserfall: nach rechts die Zeit, nach oben die Frequenz von 600 bis 1.600 Hz; jede Taste zeigt sich als zwei helle Striche übereinander.');
        const waveBox = el('figure', 'dtmf-figure');
        waveBox.append(el('figcaption', null, 'Die Schwingung, 10 Millisekunden, 400-mal langsamer'), wave);
        const fallBox = el('figure', 'dtmf-figure');
        fallBox.append(el('figcaption', null, 'Der Wasserfall: welche Frequenzen wann erklingen'), fall);
        const views = el('div', 'dtmf-views');
        views.append(waveBox, fallBox);
        const body = el('div', 'dtmf-body');
        body.append(pad, views);

        const message = el('p', 'dtmf-message');
        message.setAttribute('aria-live', 'polite');
        const all = el('button', 'btn btn-sm btn-outline-secondary', 'Alle Tasten nacheinander');
        all.type = 'button';
        const ask = el('button', 'btn btn-sm btn-outline-secondary', 'Rätsel: drei Tasten');
        ask.type = 'button';
        const solve = el('button', 'btn btn-sm btn-outline-secondary', 'Auflösen');
        solve.type = 'button';
        solve.hidden = true;
        const actions = el('div', 'dtmf-actions');
        actions.append(all, ask, solve);
        root.append(body, message, actions);
        root.tabIndex = -1;

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (name) => s.getPropertyValue(name).trim();
            return { ink: get('--dtmf-ink'), quiet: get('--dtmf-quiet'), row: get('--dtmf-row'), col: get('--dtmf-col'),
                sum: get('--dtmf-sum'), ground: get('--dtmf-ground'), hot: get('--dtmf-hot') };
        };

        const say = (key) => {
            const t = tonesOf(key);
            message.innerHTML = `Taste <strong>${key}</strong>: <span class='dtmf-r'>${hz(t[0])}</span> und `
                + `<span class='dtmf-c'>${hz(t[1])}</span> zugleich.`;
        };

        function mark(key) {
            pad.querySelectorAll('.on').forEach((n) => n.classList.remove('on'));
            if (!key || hidden) return;
            const [, , r, c] = tonesOf(key);
            buttons[key].classList.add('on');
            pad.querySelector(`[data-row='${r}']`).classList.add('on');
            pad.querySelector(`[data-col='${c}']`).classList.add('on');
        }

        // the wave: both tones faint, their sum strong -----------------------------
        function drawWave() {
            const w = wave.clientWidth, h = wave.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return;
            if (wave.width !== Math.round(w * dpr) || wave.height !== Math.round(h * dpr)) {
                wave.width = Math.round(w * dpr);
                wave.height = Math.round(h * dpr);
            }
            const g = wave.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            const col = colours();
            const [f1, f2] = tonesOf(shown);
            const pad_ = 8, mid = (h - 18) / 2, amp = (h - 18) / 2 - pad_;
            g.clearRect(0, 0, w, h);
            g.strokeStyle = col.quiet;
            g.lineWidth = 1;
            g.beginPath(); g.moveTo(0, mid); g.lineTo(w, mid); g.stroke();
            const curve = (fn, colour, width) => {
                g.strokeStyle = colour; g.lineWidth = width; g.beginPath();
                for (let x = 0; x <= w; x++) {
                    const t = waveTime + x / w * WINDOW;
                    const y = mid - amp * fn(t);
                    x ? g.lineTo(x, y) : g.moveTo(x, y);
                }
                g.stroke();
            };
            const s1 = (t) => Math.sin(2 * Math.PI * f1 * t), s2 = (t) => Math.sin(2 * Math.PI * f2 * t);
            g.globalAlpha = 0.7;
            curve((t) => 0.5 * s1(t), col.row, 1.2);
            curve((t) => 0.5 * s2(t), col.col, 1.2);
            g.globalAlpha = 1;
            curve((t) => 0.5 * (s1(t) + s2(t)), col.sum, 2.4);
            g.fillStyle = col.ink;
            g.font = '12px system-ui, sans-serif';
            g.textBaseline = 'bottom';
            g.textAlign = 'left'; g.fillText('0 ms', 0, h);
            g.textAlign = 'center'; g.fillText('5 ms', w / 2, h);
            g.textAlign = 'right'; g.fillText('10 ms', w, h);
            wave.setAttribute('aria-label', hidden ? 'Die Schwingung ist im Rätsel verdeckt.'
                : `Die Schwingung der Taste ${shown}: ${hz(f1)} und ${hz(f2)}, schwach gezeichnet, und ihre Summe kräftig.`);
            if (hidden) {
                g.fillStyle = col.ground;
                g.fillRect(0, 0, w, h - 16);
                g.fillStyle = col.ink;
                g.textAlign = 'center'; g.textBaseline = 'middle';
                g.fillText('verdeckt', w / 2, mid);
            }
        }

        // the waterfall ------------------------------------------------------------
        const LABEL_W = 48;
        function freqY(f, h) { return h - (f - LOW) / (HIGH - LOW) * h; }

        function drawFall() {
            const w = fall.clientWidth, h = fall.clientHeight, dpr = window.devicePixelRatio || 1;
            if (!w) return;
            if (fall.width !== Math.round(w * dpr)) { fall.width = Math.round(w * dpr); fall.height = Math.round(h * dpr); }
            const g = fall.getContext('2d');
            g.setTransform(dpr, 0, 0, dpr, 0, 0);
            const col = colours();
            const plotW = Math.max(10, Math.floor(w - LABEL_W));
            g.fillStyle = col.ground;
            g.fillRect(0, 0, w, h);
            // the columns, newest at the right; brightness from the analyser's dB
            const ground = rgb(col.ground), hot = rgb(col.hot);
            const image = g.createImageData(plotW, h);
            const start = Math.max(0, columns.length - plotW);
            for (let i = start; i < columns.length; i++) {
                const x = plotW - (columns.length - i);
                const column = columns[i];
                for (let y = 0; y < h; y++) {
                    const v = column[Math.floor(y / h * column.length)] || 0;
                    const o = (y * plotW + x) * 4;
                    image.data[o] = ground[0] + (hot[0] - ground[0]) * v;
                    image.data[o + 1] = ground[1] + (hot[1] - ground[1]) * v;
                    image.data[o + 2] = ground[2] + (hot[2] - ground[2]) * v;
                    image.data[o + 3] = 255;
                }
            }
            // putImageData ignores the transform: a canvas of its own, drawn scaled
            const tmp = drawFall.tmp || (drawFall.tmp = document.createElement('canvas'));
            tmp.width = plotW; tmp.height = h;
            tmp.getContext('2d').putImageData(image, 0, 0);
            g.imageSmoothingEnabled = false;
            g.drawImage(tmp, LABEL_W, 0, plotW, h);
            // the seven frequencies: a fine line and the number at the left
            g.font = '12px system-ui, sans-serif';
            g.textAlign = 'right'; g.textBaseline = 'middle';
            for (const [list, colour] of [[ROWS, col.row], [COLS, col.col]]) {
                for (const f of list) {
                    const y = Math.round(freqY(f, h)) + 0.5;
                    g.strokeStyle = colour; g.globalAlpha = 0.35; g.lineWidth = 1;
                    g.beginPath(); g.moveTo(LABEL_W - 4, y); g.lineTo(w, y); g.stroke();
                    g.globalAlpha = 1; g.fillStyle = colour;
                    g.fillText(f.toLocaleString('de-DE'), LABEL_W - 7, y);
                }
            }
            if (!columns.length) {
                g.fillStyle = col.ink; g.textAlign = 'center';
                g.fillText('Drück eine Taste', LABEL_W + plotW / 2, h / 2 - 22);
            }
        }

        function rgb(colour) {
            const m = colour.match(/^#([0-9a-f]{6})$/i);
            if (m) return [0, 2, 4].map((i) => parseInt(m[1].slice(i, i + 2), 16));
            const n = colour.match(/\d+(\.\d+)?/g);
            return n ? n.slice(0, 3).map(Number) : [0, 0, 0];
        }

        function sample() {
            analyser.getFloatFrequencyData(bins);
            const h = fall.clientHeight || 200;
            const column = new Float32Array(h);
            const binHz = ctx.sampleRate / analyser.fftSize;
            let loud = false;
            for (let y = 0; y < h; y++) {
                const f = LOW + (HIGH - LOW) * (1 - (y + 0.5) / h);
                const db = bins[Math.round(f / binHz)];
                const v = Math.max(0, Math.min(1, (db - FLOOR) / (TOP - FLOOR)));
                column[y] = v * v;
                if (v > 0.3) loud = true;
            }
            columns.push(column);
            const keep = (fall.clientWidth || 600) + 10;
            if (columns.length > keep) columns.splice(0, columns.length - keep);
            return loud;
        }

        function loop() {
            if (!running) return;
            const loud = sample();
            quietFrames = loud || sounding ? 0 : quietFrames + 1;
            drawFall();
            // the wave runs on while a key sounds, slowed down
            const now = performance.now();
            if (sounding && !hidden && !still.matches && lastFrame) {
                waveTime += Math.min(100, now - lastFrame) / 1000 / SLOW;
                drawWave();
            }
            lastFrame = now;
            // stops when the last tone has run out of the picture, or off screen
            if (!visible || quietFrames > fall.clientWidth + 10) { running = false; return; }
            requestAnimationFrame(loop);
        }

        function run() {
            if (running || !analyser) return;
            running = true;
            quietFrames = 0;
            lastFrame = 0;
            requestAnimationFrame(loop);
        }

        // sound --------------------------------------------------------------------
        function audio() {
            if (!ctx) {
                const AC = window.AudioContext || window.webkitAudioContext;
                if (!AC) return null;
                ctx = new AC();
                analyser = ctx.createAnalyser();
                analyser.fftSize = 4096;
                analyser.smoothingTimeConstant = 0;
                bins = new Float32Array(analyser.frequencyBinCount);
                out = ctx.createGain();
                out.connect(analyser);
                analyser.connect(ctx.destination);
            }
            if (ctx.state === 'suspended') ctx.resume();
            return ctx;
        }

        function press(key) {
            if (!audio()) return;
            release(0);
            const [f1, f2] = tonesOf(key);
            const gain = ctx.createGain();
            const now = ctx.currentTime;
            gain.gain.setValueAtTime(0, now);
            gain.gain.linearRampToValueAtTime(LEVEL, now + 0.005);
            gain.connect(out);
            const oscs = [f1, f2].map((f) => {
                const o = ctx.createOscillator();
                o.frequency.value = f;
                o.connect(gain);
                o.start(now);
                return o;
            });
            sounding = { key, oscs, gain, started: now };
            if (!hidden) { shown = key; say(key); drawWave(); }
            mark(key);
            run();
        }

        // the tone ends after at least length seconds on the audio clock (a fresh
        // AudioContext starts its clock late; the wall clock would cut the tone)
        function release(length = MIN_TONE) {
            if (!sounding) return;
            const s = sounding;
            sounding = null;
            const end = Math.max(ctx.currentTime, s.started + length);
            s.gain.gain.setValueAtTime(LEVEL, end);
            s.gain.gain.linearRampToValueAtTime(0, end + 0.005);
            s.oscs.forEach((o) => o.stop(end + 0.01));
            const wait = Math.max(0, (end - ctx.currentTime) * 1000);
            setTimeout(() => { if (!sounding) mark(null); }, wait);
        }

        const pause = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

        async function play(keys) {
            busy = true;
            all.disabled = ask.disabled = true;
            for (const key of keys) {
                press(key);
                const started = sounding ? sounding.started : 0;
                release(0.18);
                // the next key when this one has sounded and a pause after it
                // (at most a second: a context the browser keeps suspended has no clock)
                for (let waited = 0; ctx && ctx.currentTime < started + 0.3 && waited < 1000; waited += 20) await pause(20);
            }
            busy = false;
            all.disabled = ask.disabled = false;
        }

        // input --------------------------------------------------------------------
        pad.addEventListener('pointerdown', (e) => {
            const b = e.target.closest('.dtmf-key');
            if (!b || busy) return;
            e.preventDefault();
            b.setPointerCapture?.(e.pointerId);
            endRiddle();
            press(b.dataset.key);
        });
        const up = () => release();
        pad.addEventListener('pointerup', up);
        pad.addEventListener('pointercancel', up);
        pad.addEventListener('click', (e) => {
            // the keyboard's Enter and space on a key (a pointer has played it already)
            const b = e.target.closest('.dtmf-key');
            if (!b || busy || e.detail !== 0) return;
            endRiddle();
            press(b.dataset.key);
            release();
        });
        root.addEventListener('keydown', (e) => {
            if (e.repeat || busy || e.ctrlKey || e.metaKey || e.altKey || !tonesOf(e.key)) return;
            if (e.target.closest('button') && (e.key === ' ' || e.key === 'Enter')) return;
            e.preventDefault();
            endRiddle();
            press(e.key);
        });
        root.addEventListener('keyup', (e) => { if (sounding && e.key === sounding.key) release(); });

        all.addEventListener('click', () => { endRiddle(); play(KEYS.flat()); });
        ask.addEventListener('click', async () => {
            riddle = Array.from({ length: 3 }, () => KEYS.flat()[Math.floor(Math.random() * 12)]);
            hidden = true;
            mark(null);
            drawWave();
            message.textContent = 'Drei Tasten erklingen. Welche? Lies ihre Töne im Wasserfall ab.';
            await play(riddle);
            solve.hidden = false;
        });
        solve.addEventListener('click', () => {
            message.innerHTML = 'Das waren ' + riddle.map((k) => `<strong>${k}</strong>`).join(', ') + '.';
            endRiddle(true);
        });

        function endRiddle(keepMessage) {
            if (!hidden) return;
            hidden = false;
            riddle = keepMessage ? riddle : null;
            solve.hidden = true;
            drawWave();
        }

        new ResizeObserver(() => { drawWave(); drawFall(); }).observe(views);
        new IntersectionObserver((entries) => {
            visible = entries[0].isIntersecting;
            if (visible && columns.length && (sounding || quietFrames <= fall.clientWidth + 10)) run();
        }).observe(fall);
        new MutationObserver(() => { drawWave(); drawFall(); })
            .observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
        message.textContent = 'Drück eine Taste und hör hin; die Schwingung zeigt bis dahin die Taste 5.';
        drawWave();
        drawFall();
    }

    const start = () => document.querySelectorAll('.dtmf-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
