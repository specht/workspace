// Sampling and quantising (»Audio komprimieren«): a clip, measured anew at a
// lower sample rate and with fewer bits per measurement, played in a loop,
// so that switching while it plays lets one hear the difference at once.
// The picture shows 5 ms of the clip: the wave before measuring (faint),
// the measurements as dots on the levels the bits allow, and the wave that
// is heard, made again from the measurements (purple, what is current).
// The strip under it is the whole clip; a click or a drag there chooses
// the 5 ms and, while it plays, the place to play from.
//
//   <script src='/include/sampling_widget.js'></script>
//   <div class='sampling-widget'></div>
//   <div class='sampling-widget' data-mulaw data-bits='8' data-clip='stimme' data-level='0.06'></div>
//
// data-mulaw adds the choice between even levels and μ-law (G.711),
// data-bits and data-rate the setting it starts with, data-clip the clip,
// data-level a factor for the clip (a quiet voice, where μ-law helps).
// The clips are the files students fetch (/audio/*.wav: mono, 16 bit,
// 44.100 Hz, a plain 44-byte header, read here byte by byte as a student's
// read_wav does). Measuring at a lower rate filters first, as a recorder
// does (a windowed sinc); playing makes the wave again the same way.
(function () {
    'use strict';
    const SOURCE = 44100;
    const RATES = [44100, 22050, 11025, 8000, 4000];
    const BITS = [16, 12, 8, 6, 4, 3];
    const MU = 255;
    const WINDOW = 0.005;                    // the picture: 5 ms
    // at: where the picture of 5 ms starts, in seconds (a hi-hat stroke, a held vowel)
    const CLIPS = [
        { id: 'funk', label: 'Funk', at: 5.503, url: '/audio/effects/funk.flac',
          credit: "»Funkorama« von Kevin MacLeod (incompetech.com), CC BY 4.0 (<a href='https://incompetech.com/music/royalty-free/faq.html'>incompetech</a>)" },
        { id: 'schlagzeug', label: 'Schlagzeug', at: 5.573, url: '/audio/effects/drums.flac',
          credit: "»Rock-funk backbeat« von jmarcosfer, CC0 (<a href='https://freesound.org/people/jmarcosfer/sounds/592756/'>Freesound</a>)" },
        { id: 'gesang', label: 'Gesang', at: 8.62, url: '/audio/effects/singing-voice-2.flac',
          credit: "»Running with Renegades (Main Vox)«, feat. Ashes and Dreams, CC BY 4.0 (<a href='https://freesound.org/people/ashesanddreams/sounds/610416/'>Freesound</a>)" },
        { id: 'stimme', label: 'Stimme', at: 0.62 },
    ];
    const still = window.matchMedia('(prefers-reduced-motion: reduce)');
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    let counter = 0;

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

    // a WAV file as numbers from -1 to 1: the chunks one after the other,
    // the samples in the one called »data«, 16 bit, Little Endian
    // a recording under a free licence (the clips of »Audioeffekte«, FLAC):
    // decoded by the browser at 44.100 Hz, its channels averaged
    function decoded(url) {
        const OAC = window.OfflineAudioContext || window.webkitOfflineAudioContext;
        return fetch(url).then((r) => {
            if (!r.ok) throw new Error(r.status);
            return r.arrayBuffer();
        }).then((data) => new OAC(1, 1, SOURCE).decodeAudioData(data)).then((buffer) => {
            const x = new Float32Array(buffer.length);
            for (let c = 0; c < buffer.numberOfChannels; c++) {
                const channel = buffer.getChannelData(c);
                for (let i = 0; i < x.length; i++) x[i] += channel[i] / buffer.numberOfChannels;
            }
            return x;
        });
    }
    const files = {};
    function load(clip) {
        const id = clip.id;
        if (!files[id] && clip.url) files[id] = decoded(clip.url);
        if (!files[id]) {
            files[id] = fetch(`/audio/${id}.wav`).then((r) => {
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
        return files[id];
    }

    // from one rate to another through a low-pass filter at cutoff Hz: a sinc,
    // eight zero crossings on each side, under a Hann window
    function resample(x, from, to, cutoff) {
        const n = Math.floor(x.length * to / from);
        const out = new Float32Array(n);
        const fc = cutoff / from;
        const half = Math.ceil(8 / (2 * fc));
        for (let j = 0; j < n; j++) {
            const p = j * from / to;
            const i0 = Math.max(0, Math.ceil(p - half)), i1 = Math.min(x.length - 1, Math.floor(p + half));
            let s = 0;
            for (let i = i0; i <= i1; i++) {
                const d = i - p;
                const a = 2 * fc * d;
                const sinc = a === 0 ? 1 : Math.sin(Math.PI * a) / (Math.PI * a);
                s += x[i] * sinc * (0.5 + 0.5 * Math.cos(Math.PI * d / half));
            }
            out[j] = s * 2 * fc;
        }
        return out;
    }

    // the levels: whole numbers from -top to top, evenly spaced or after μ-law
    const squeeze = (v) => Math.sign(v) * Math.log1p(MU * Math.abs(v)) / Math.log1p(MU);
    const expand = (v) => Math.sign(v) * (Math.pow(1 + MU, Math.abs(v)) - 1) / MU;
    function quantise(x, bits, mulaw) {
        if (bits >= 16 && !mulaw) return x;
        const top = 2 ** (bits - 1) - 1;
        const out = new Float32Array(x.length);
        for (let i = 0; i < x.length; i++) {
            let v = Math.max(-1, Math.min(1, x[i]));
            if (mulaw) v = squeeze(v);
            v = Math.round(v * top) / top;
            out[i] = mulaw ? expand(v) : v;
        }
        return out;
    }
    // the levels between -limit and limit, if there are few enough to draw
    function levels(bits, mulaw, limit) {
        const top = 2 ** (bits - 1) - 1;
        const list = [];
        for (let k = -top; k <= top; k++) {
            const v = mulaw ? expand(k / top) : k / top;
            if (Math.abs(v) <= limit * 1.02) list.push(v);
            if (list.length > 48) return [];
        }
        return list;
    }

    function setUp(root) {
        const id = `sampling-${++counter}`;
        const mulawChoice = root.hasAttribute('data-mulaw');
        const level = Number(root.dataset.level || 1);
        let clip = CLIPS.find((c) => c.id === root.dataset.clip) || CLIPS[0];
        let rate = RATES.includes(Number(root.dataset.rate)) ? Number(root.dataset.rate) : 44100;
        let bits = BITS.includes(Number(root.dataset.bits)) ? Number(root.dataset.bits) : 16;
        let mulaw = false;
        let original = null, measured = null, heard = null;    // the clip; at rate with bits; made again at 44.100 Hz
        let at = clip.at;                                       // where the picture is, in seconds
        let ctx = null, source = null, startedAt = 0, offset = 0, playing = false;
        const measuredCache = new Map();

        const group = (name, label, items, checked, onPick) => {
            const box = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': label });
            items.forEach((item) => {
                const inputId = `${id}-${name}-${item.value}`;
                const input = el('input', { type: 'radio', class: 'btn-check', name: `${id}-${name}`, id: inputId, autocomplete: 'off' });
                if (item.value === checked) input.checked = true;
                input.addEventListener('change', () => { if (input.checked) onPick(item.value); });
                box.append(input, el('label', { class: 'btn btn-outline-secondary', for: inputId, text: item.label }));
            });
            return el('div', { class: 'sampling-group' }, [el('span', { class: 'sampling-label', text: label }), box]);
        };

        const playButton = el('button', { type: 'button', class: 'btn btn-sm btn-success sampling-play' });
        const showPlay = () => {
            playButton.replaceChildren(el('i', { class: `bi ${playing ? 'bi-stop-fill' : 'bi-play-fill'}` }), playing ? ' Anhalten' : ' Abspielen');
        };
        showPlay();
        const controls = el('div', { class: 'sampling-controls' }, [
            group('clip', 'Klang', CLIPS.map((c) => ({ value: c.id, label: c.label })), clip.id, (v) => pickClip(v)),
            playButton,
        ]);
        // who made the clip, where it is someone else's (CLIPS, trusted HTML of this file)
        const credit = el('p', { class: 'fx-credit' });
        const showCredit = () => { credit.innerHTML = clip.credit || ''; credit.hidden = !clip.credit; };
        showCredit();
        const settings = el('div', { class: 'sampling-controls' }, [
            group('rate', 'Samples je Sekunde', RATES.map((r) => ({ value: String(r), label: de(r) })), String(rate),
                (v) => { rate = Number(v); update(); }),
            group('bits', 'Bits je Sample', BITS.map((b) => ({ value: String(b), label: String(b) })), String(bits),
                (v) => { bits = Number(v); update(); }),
        ]);
        if (mulawChoice) {
            settings.append(group('scale', 'Stufen', [{ value: 'even', label: 'gleichmäßig' }, { value: 'mulaw', label: 'μ-law' }], 'even',
                (v) => { mulaw = v === 'mulaw'; update(); }));
        }

        const picture = el('canvas', { class: 'sampling-picture', role: 'img' });
        const legend = el('div', { class: 'sampling-legend' }, [
            el('span', { class: 'sampling-l-orig' }, [el('i'), 'vor dem Messen']),
            el('span', { class: 'sampling-l-dots' }, [el('i'), 'die Samples']),
            el('span', { class: 'sampling-l-heard' }, [el('i'), 'was du hörst']),
        ]);
        const strip = el('canvas', { class: 'sampling-strip', tabindex: '0', role: 'slider', 'aria-label': 'Stelle im Klang' });
        const rateLine = el('p', { class: 'sampling-rate', 'aria-live': 'polite' });
        root.append(controls, credit, settings,
            el('figure', { class: 'sampling-figure' }, [el('figcaption', { text: level === 1 ? '5 Millisekunden des Klangs' : `5 Millisekunden des leisen Klangs, ${de(1 / level)}-mal vergrößert` }), picture, legend]),
            el('figure', { class: 'sampling-figure' }, [el('figcaption', { text: 'Der ganze Klang: hier klicken oder ziehen wählt die Stelle' }), strip]),
            rateLine);

        const colours = () => {
            const s = getComputedStyle(root);
            const get = (name) => s.getPropertyValue(name).trim();
            return { ink: get('--sampling-ink'), quiet: get('--sampling-quiet'), orig: get('--sampling-orig'),
                heard: get('--sampling-heard'), ground: get('--sampling-ground') };
        };

        function describe() {
            const perSecond = rate * bits;
            rateLine.innerHTML = `${de(rate)} Samples je Sekunde mit je ${bits} Bit: `
                + `<strong>${de(perSecond)} Bit</strong> für jede Sekunde Klang (${de(perSecond / 1000, 1)} kbit/s)`
                + (mulaw ? ', mit Stufen nach μ-law' : '') + '.';
            const top = 2 ** (bits - 1) - 1;
            picture.setAttribute('aria-label', `5 Millisekunden bei ${de(at, 2)} s: ${de(Math.round(rate * WINDOW))} Samples auf `
                + `${de(2 * top + 1)} Stufen${mulaw ? ' nach μ-law, dicht bei null enger' : ''}.`);
        }

        // the measurements at this rate (cached per clip and rate), then the bits, then the wave again
        function compute() {
            if (!original) return;
            const key = `${clip.id} ${rate}`;
            let atRate = measuredCache.get(key);
            if (!atRate) {
                atRate = rate === SOURCE ? original : resample(original, SOURCE, rate, rate * 0.47);
                measuredCache.set(key, atRate);
            }
            measured = quantise(atRate, bits, mulaw);
            heard = rate === SOURCE ? measured : resample(measured, rate, SOURCE, rate * 0.47);
        }

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

        function drawPicture() {
            const s = sizeCanvas(picture);
            if (!s) return;
            const { g, w, h } = s;
            const col = colours();
            const plotH = h - 18, mid = plotH / 2, amp = plotH / 2 - 6;
            const y = (v) => mid - amp * v / level;
            g.fillStyle = col.ground;
            g.fillRect(0, 0, w, plotH);
            // the levels the bits allow, where there are few enough to draw
            g.strokeStyle = col.quiet;
            g.lineWidth = 1;
            for (const v of levels(bits, mulaw, level)) {
                g.beginPath(); g.moveTo(0, Math.round(y(v)) + 0.5); g.lineTo(w, Math.round(y(v)) + 0.5); g.stroke();
            }
            if (!original) {
                g.fillStyle = col.ink; g.font = '13px system-ui, sans-serif'; g.textAlign = 'center'; g.textBaseline = 'middle';
                g.fillText('lädt …', w / 2, mid);
                return;
            }
            const t0 = at, t1 = at + WINDOW;
            const curve = (x, r, colour, width) => {
                g.strokeStyle = colour; g.lineWidth = width; g.beginPath();
                const i0 = Math.max(0, Math.floor(t0 * r) - 1), i1 = Math.min(x.length - 1, Math.ceil(t1 * r) + 1);
                for (let i = i0; i <= i1; i++) {
                    const px = (i / r - t0) / WINDOW * w;
                    i === i0 ? g.moveTo(px, y(x[i])) : g.lineTo(px, y(x[i]));
                }
                g.stroke();
            };
            curve(original, SOURCE, col.orig, 1.3);
            curve(heard, SOURCE, col.heard, 2.2);
            // the measurements: a dot each, where they are far enough apart to see
            const gap = w / (rate * WINDOW);
            if (gap >= 7) {
                g.fillStyle = col.heard;
                const radius = Math.min(3.5, gap / 3);
                for (let i = Math.ceil(t0 * rate); i <= Math.floor(t1 * rate); i++) {
                    g.beginPath();
                    g.arc((i / rate - t0) / WINDOW * w, y(measured[i]), radius, 0, 2 * Math.PI);
                    g.fill();
                }
            }
            g.fillStyle = col.ink;
            g.font = '12px system-ui, sans-serif';
            g.textBaseline = 'bottom';
            g.textAlign = 'left'; g.fillText(`${de(at, 3)} s`, 0, h);
            g.textAlign = 'right'; g.fillText(`${de(at + WINDOW, 3)} s`, w, h);
        }

        let envelope = null;
        function drawStrip() {
            const s = sizeCanvas(strip);
            if (!s) return;
            const { g, w, h } = s;
            const col = colours();
            g.fillStyle = col.ground;
            g.fillRect(0, 0, w, h);
            if (!original) return;
            if (!envelope || envelope.length !== Math.floor(w)) {
                envelope = new Float32Array(Math.floor(w));
                const per = original.length / envelope.length;
                for (let x = 0; x < envelope.length; x++) {
                    let m = 0;
                    for (let i = Math.floor(x * per); i < Math.floor((x + 1) * per); i++) m = Math.max(m, Math.abs(original[i]));
                    envelope[x] = m;
                }
            }
            g.fillStyle = col.orig;
            for (let x = 0; x < envelope.length; x++) {
                const v = Math.min(1, envelope[x] / level) * (h / 2 - 3);
                g.fillRect(x, h / 2 - v, 1, Math.max(1, 2 * v));
            }
            const duration = original.length / SOURCE;
            g.fillStyle = col.heard;
            g.fillRect(Math.round(at / duration * w) - 1, 0, 3, h);
            if (playing) {
                g.fillStyle = col.ink;
                g.fillRect(Math.round(position() / duration * w), 0, 1, h);
            }
        }

        function draw() { describe(); drawPicture(); drawStrip(); }

        // sound ----------------------------------------------------------------------
        // Playing: one looping source with its own gain. A change (rate,
        // bits, μ-law, place) never stops the sound first: the new source is
        // scheduled a little ahead on the audio clock, at the place the old
        // one will have reached by then, and the two cross over in FADE
        // seconds, so that nothing breaks.
        const FADE = 0.02;
        let voice = null;                                       // { source, gain }
        function position() {
            if (!playing || !original) return offset;
            const duration = original.length / SOURCE;
            return ((offset + Math.max(0, ctx.currentTime - startedAt)) % duration + duration) % duration;
        }
        const announce = window.audio_exclusive ? window.audio_exclusive(() => { if (playing) stop(); }) : () => {};
        function start(from, smooth = playing) {
            const AC = window.AudioContext || window.webkitAudioContext;
            if (!AC || !heard) return;
            announce();
            if (!ctx) ctx = new AC();
            if (ctx.state === 'suspended') ctx.resume();
            const duration = original.length / SOURCE;
            const when = ctx.currentTime + (smooth ? 0.03 : 0.005);
            // where the old source will be at that moment, unless asked for another place
            const place = from === null ? ((offset + when - startedAt) % duration + duration) % duration : from;
            const buffer = ctx.createBuffer(1, heard.length, SOURCE);
            // a quiet clip is played loud again, so that the noise of the levels is heard
            buffer.copyToChannel(level === 1 ? heard : heard.map((v) => Math.max(-1, Math.min(1, v / level))), 0);
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
            offset = place;
            startedAt = when;
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
            offset = position();
            stopSource();
            playing = false;
            showPlay();
            drawStrip();
        }

        let lastTick = 0;
        function tick(now) {
            if (!playing) return;
            if (!still.matches || now - lastTick > 250) { drawStrip(); lastTick = now; }
            requestAnimationFrame(tick);
        }

        function update() {
            compute();
            draw();
            if (playing) start(null);
        }

        async function pickClip(value) {
            clip = CLIPS.find((c) => c.id === value);
            showCredit();
            const wasPlaying = playing;
            if (playing) stop();
            original = null; measured = null; heard = null; envelope = null;
            at = clip.at; offset = 0;
            draw();
            try {
                const x = await load(clip);
                if (clip.id !== value) return;
                original = level === 1 ? x : x.map((v) => v * level);
            } catch (e) {
                rateLine.textContent = 'Der Klang ließ sich nicht laden.';
                return;
            }
            measuredCache.clear();
            compute();
            draw();
            if (wasPlaying) start(0);
        }

        playButton.addEventListener('click', () => { if (playing) stop(); else start(offset); });

        // restart: while dragging only the picture follows; the sound
        // goes on from the new place when the pointer lets go
        const moveTo = (t, restart = true) => {
            const duration = original.length / SOURCE;
            at = Math.max(0, Math.min(duration - WINDOW, t));
            if (!playing) offset = at;
            else if (restart) start(at);
            draw();
        };
        // a click chooses the place, and dragging moves it
        let dragging = false;
        const fromPointer = (e) => {
            const r = strip.getBoundingClientRect();
            return (e.clientX - r.left) / r.width * original.length / SOURCE;
        };
        strip.addEventListener('pointerdown', (e) => {
            if (!original) return;
            dragging = true;
            strip.setPointerCapture(e.pointerId);
            moveTo(fromPointer(e), false);
        });
        strip.addEventListener('pointermove', (e) => { if (dragging) moveTo(fromPointer(e), false); });
        const release = (e) => {
            if (!dragging) return;
            dragging = false;
            moveTo(fromPointer(e), true);
        };
        strip.addEventListener('pointerup', release);
        strip.addEventListener('pointercancel', release);
        strip.addEventListener('keydown', (e) => {
            if (!original || !['ArrowLeft', 'ArrowRight'].includes(e.key)) return;
            e.preventDefault();
            moveTo(at + (e.key === 'ArrowRight' ? 1 : -1) * (e.shiftKey ? 0.005 : 0.1));
        });

        new ResizeObserver(() => { envelope = null; drawPicture(); drawStrip(); }).observe(root);
        new MutationObserver(() => { drawPicture(); drawStrip(); })
            .observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
        // the clip loads when the widget comes near the screen
        const near = new IntersectionObserver((entries) => {
            if (!entries[0].isIntersecting) return;
            near.disconnect();
            pickClip(clip.id);
        }, { rootMargin: '600px' });
        near.observe(root);
        draw();
    }

    const go = () => document.querySelectorAll('.sampling-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
