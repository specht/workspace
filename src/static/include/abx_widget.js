// A listening test (»Audio komprimieren«): A is the original, B the same
// five seconds after a codec at a bit rate, X one of the two, drawn anew for
// each round. One listens to A, B and X as often as one likes, switching at
// the same place in the music, and says whether X is A or B. The tally says
// how often guessing alone would do as well (the binomial distribution):
// 9 of 10 right by guessing happens about once in a hundred.
//
//   <script src='/include/abx_widget.js'></script>
//   <div class='abx-widget'></div>
//
// The pieces are /audio/abx/*.flac, made by sounds/abx.py: coded and decoded
// again by ffmpeg, so every piece is lossless FLAC, starts at the same
// sample and plays the same in every browser.
(function () {
    'use strict';
    const CODECS = [
        { id: 'mp3', label: 'MP3', rates: [32, 64, 128] },
        { id: 'aac', label: 'AAC', rates: [32, 64, 128] },
        { id: 'opus', label: 'Opus', rates: [16, 32, 64] },
    ];
    const FADE = 0.012;                      // seconds to fade when switching, so that no click tells them apart
    const GAIN = 0.8;
    let counter = 0;
    let ctx = null;
    const buffers = {};

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
    const de = (n, digits = 0) => n.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });

    function audio() {
        const AC = window.AudioContext || window.webkitAudioContext;
        if (!AC) return null;
        if (!ctx) ctx = new AC();
        if (ctx.state === 'suspended') ctx.resume();
        return ctx;
    }
    function load(name) {
        if (!buffers[name]) {
            buffers[name] = fetch(`/audio/abx/${name}.flac`)
                .then((r) => { if (!r.ok) throw new Error(r.status); return r.arrayBuffer(); })
                .then((data) => audio().decodeAudioData(data));
        }
        return buffers[name];
    }

    // how likely k or more right in n rounds is by guessing
    function guessing(k, n) {
        let p = 0, c = 1;
        for (let i = 0; i <= n; i++) {
            if (i > 0) c = c * (n - i + 1) / i;
            if (i >= k) p += c;
        }
        return p / 2 ** n;
    }

    function setUp(root) {
        const id = `abx-${++counter}`;
        let codec = CODECS[0], rate = 128;
        let x = null;                        // 'A' or 'B'
        let right = 0, rounds = 0;
        let playing = null;                  // { which, source, gain }
        let startedAt = 0, offset = 0;
        let pieces = null;                   // { A: buffer, B: buffer }

        const group = (name, label, items, checked, onPick) => {
            const box = el('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': label });
            items.forEach((item) => {
                const inputId = `${id}-${name}-${item.value}`;
                const input = el('input', { type: 'radio', class: 'btn-check', name: `${id}-${name}`, id: inputId, autocomplete: 'off' });
                if (item.value === checked) input.checked = true;
                input.addEventListener('change', () => { if (input.checked) onPick(item.value); });
                box.append(input, el('label', { class: 'btn btn-outline-secondary', for: inputId, text: item.label }));
            });
            return box;
        };
        const codecGroup = group('codec', 'Codec', CODECS.map((c) => ({ value: c.id, label: c.label })), codec.id, (v) => {
            codec = CODECS.find((c) => c.id === v);
            rate = codec.rates[codec.rates.length - 1];
            rateBox.replaceChildren(rateGroup());
            choose();
        });
        const rateGroup = () => group(`rate-${codec.id}`, 'Bitrate', codec.rates.map((r) => ({ value: String(r), label: `${r} kbit/s` })), String(rate),
            (v) => { rate = Number(v); choose(); });
        const rateBox = el('span', {}, [rateGroup()]);

        const playButtons = {};
        const playRow = el('div', { class: 'abx-play' });
        for (const [which, label] of [['A', 'A: Original'], ['B', 'B: mit Codec'], ['X', 'X: A oder B?']]) {
            const button = el('button', { type: 'button', class: 'btn btn-outline-secondary abx-button', 'aria-pressed': 'false' },
                [el('i', { class: 'bi bi-play-fill', 'aria-hidden': 'true' }), ` ${label}`]);
            button.addEventListener('click', () => (playing && playing.which === which ? stop() : play(which)));
            playButtons[which] = button;
            playRow.append(button);
        }
        const answerA = el('button', { type: 'button', class: 'btn btn-outline-secondary' }, 'X ist A');
        const answerB = el('button', { type: 'button', class: 'btn btn-outline-secondary' }, 'X ist B');
        const again = el('button', { type: 'button', class: 'btn btn-sm btn-link abx-again', text: 'Von vorn zählen' });
        const verdict = el('p', { class: 'abx-verdict', 'aria-live': 'polite' });
        const tally = el('p', { class: 'abx-tally', 'aria-live': 'polite' });
        root.append(
            el('div', { class: 'abx-controls' }, [el('span', { class: 'abx-label', text: 'Codec' }), codecGroup]),
            el('div', { class: 'abx-controls' }, [el('span', { class: 'abx-label', text: 'Bitrate' }), rateBox]),
            playRow,
            el('div', { class: 'abx-controls abx-answer' }, [answerA, answerB, again]),
            verdict, tally);

        function describe() {
            if (!rounds) {
                tally.textContent = 'Noch keine Runde. Hör dir A, B und X an, so oft du willst, und sag dann, ob X das Original ist.';
                return;
            }
            const p = guessing(right, rounds);
            const percent = p < 0.001 ? 'weniger als 0,1' : de(100 * p, p < 0.1 ? 1 : 0);
            tally.innerHTML = `<strong>${right} von ${rounds}</strong> richtig. Wer nur rät, schafft so viele oder mehr in ${percent} % der Fälle.`
                + (rounds >= 10 && p < 0.05 ? ' Du hörst einen Unterschied.' : '');
        }

        function newX() {
            x = Math.random() < 0.5 ? 'A' : 'B';
        }

        async function choose() {
            stop();
            right = 0; rounds = 0;
            verdict.textContent = '';
            pieces = null;
            describe();
            newX();
            const wanted = `${codec.id}-${rate}`;
            try {
                const [a, b] = await Promise.all([load('original'), load(wanted)]);
                if (`${codec.id}-${rate}` !== wanted) return;
                pieces = { A: a, B: b };
            } catch (e) {
                tally.textContent = 'Die Klänge ließen sich nicht laden.';
            }
        }

        function position() {
            const duration = pieces.A.duration;
            return playing ? (offset + ctx.currentTime - startedAt) % duration : offset;
        }

        function play(which) {
            if (!pieces || !audio()) return;
            const from = position();
            stop(true);
            const gain = ctx.createGain();
            gain.gain.setValueAtTime(0, ctx.currentTime);
            gain.gain.linearRampToValueAtTime(GAIN, ctx.currentTime + FADE);
            gain.connect(ctx.destination);
            const source = ctx.createBufferSource();
            source.buffer = pieces[which === 'X' ? x : which];
            source.loop = true;
            source.connect(gain);
            source.start(0, from);
            offset = from;
            startedAt = ctx.currentTime;
            playing = { which, source, gain };
            show();
        }

        function stop(switching) {
            if (playing) {
                if (!switching) offset = position();
                const { source, gain } = playing;
                gain.gain.cancelScheduledValues(ctx.currentTime);
                gain.gain.setValueAtTime(gain.gain.value, ctx.currentTime);
                gain.gain.linearRampToValueAtTime(0, ctx.currentTime + FADE);
                source.stop(ctx.currentTime + FADE + 0.005);
                setTimeout(() => { source.disconnect(); gain.disconnect(); }, 100);
                playing = null;
            }
            show();
        }

        function show() {
            for (const [which, button] of Object.entries(playButtons)) {
                const on = playing && playing.which === which;
                button.classList.toggle('active', !!on);
                button.setAttribute('aria-pressed', on ? 'true' : 'false');
                button.firstChild.className = `bi ${on ? 'bi-stop-fill' : 'bi-play-fill'}`;
            }
        }

        function answer(guess) {
            if (!pieces) return;
            rounds++;
            const correct = guess === x;
            if (correct) right++;
            verdict.textContent = `${correct ? 'Richtig' : 'Falsch'}: X war ${x === 'A' ? 'das Original' : 'mit Codec'}. Neue Runde, X ist neu gelost.`;
            verdict.classList.toggle('abx-right', correct);
            const wasX = playing && playing.which === 'X';
            newX();
            if (wasX) play('X');
            describe();
        }
        answerA.addEventListener('click', () => answer('A'));
        answerB.addEventListener('click', () => answer('B'));
        again.addEventListener('click', () => { right = 0; rounds = 0; verdict.textContent = ''; newX(); describe(); });

        // a widget out of sight stops playing; the pieces load when it comes near
        new IntersectionObserver((entries) => { if (!entries[0].isIntersecting) stop(); }).observe(root);
        const near = new IntersectionObserver((entries) => {
            if (!entries[0].isIntersecting) return;
            near.disconnect();
            choose();
        }, { rootMargin: '600px' });
        near.observe(root);
        describe();
    }

    const go = () => document.querySelectorAll('.abx-widget').forEach((root) => {
        if (!root.dataset.ready) { root.dataset.ready = '1'; setUp(root); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
