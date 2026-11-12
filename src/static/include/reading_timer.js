// How long reading a Referat's slides takes, against how long saying them
// takes (»Eine Folie, ein Gedanke«): the slides of the page's own Referat,
// one after the other as on a projector; the reader starts, reads each slide
// and goes on (button, → or the space bar), and at the end sees the time
// read beside the time a talk of these words takes at a comfortable pace
// (about 110 words a minute; slide presentations take 100 to 125). The
// slides are the widget's own markup:
//
//   <script src='/include/reading_timer.js'></script>
//   <div class='reading-timer'>
//     <div class='rt-slide' data-title='Zug'><ul><li>…</li></ul></div>
//     …
//   </div>
(function () {
    'use strict';
    const SPOKEN_PER_MINUTE = 110;
    const el = (tag, cls, text) => {
        const node = document.createElement(tag);
        if (cls) node.className = cls;
        if (text !== undefined) node.textContent = text;
        return node;
    };
    const words = (text) => (text.match(/[\p{L}\p{N}][\p{L}\p{N}'’-]*/gu) || []).length;
    const seconds = (s) => {
        const n = Math.round(s);
        return n < 60 ? `${n} Sekunden` : `${Math.floor(n / 60)} Minute${n >= 120 ? 'n' : ''} und ${n % 60} Sekunden`;
    };

    function setUp(root) {
        const slides = Array.from(root.querySelectorAll('.rt-slide')).map((s) => ({
            title: s.dataset.title || '', html: s.innerHTML, words: words(s.dataset.title + ' ' + s.textContent),
        }));
        root.textContent = '';
        const total = slides.reduce((n, s) => n + s.words, 0);

        const head = el('div', 'rt-head');
        head.append(el('span', 'rt-title', 'Lesen gegen Zuhören'),
            el('span', 'rt-pill', `${slides.length} Folien, ${total} Wörter`));
        const screen = el('div', 'rt-screen');
        screen.setAttribute('aria-live', 'polite');
        const controls = el('div', 'rt-controls');
        const start = el('button', 'btn btn-primary btn-sm', 'Lesen beginnen');
        start.type = 'button';
        const next = el('button', 'btn btn-outline-secondary btn-sm', 'Weiter');
        next.type = 'button';
        next.hidden = true;
        const clock = el('span', 'rt-clock');
        controls.append(start, next, clock);
        const result = el('div', 'rt-result');
        result.hidden = true;
        result.setAttribute('aria-live', 'polite');
        root.append(head, screen, controls, result);

        let at = -1, began = 0, timer = null;
        const idle = () => {
            screen.textContent = '';
            const p = el('p', 'rt-idle', 'Stell dir vor, die Folien hängen vorn an der Wand. Lies jede so, wie du sie im Publikum lesen würdest, und geh dann weiter.');
            screen.appendChild(p);
        };
        const show = () => {
            const s = slides[at];
            screen.textContent = '';
            const frame = el('div', 'rt-slide-shown');
            frame.append(el('div', 'rt-slide-title', s.title));
            const body = el('div', 'rt-slide-body');
            body.innerHTML = s.html;
            frame.append(body, el('div', 'rt-slide-number', `${at + 1} / ${slides.length}`));
            screen.appendChild(frame);
            next.textContent = at === slides.length - 1 ? 'Fertig gelesen' : 'Weiter';
        };
        const tick = () => { clock.textContent = seconds((performance.now() - began) / 1000); };
        const finish = () => {
            clearInterval(timer);
            const read = (performance.now() - began) / 1000;
            const spoken = total / SPOKEN_PER_MINUTE * 60;
            next.hidden = true;
            start.hidden = false;
            start.textContent = 'Noch einmal';
            clock.textContent = '';
            idle();
            result.hidden = false;
            result.textContent = '';
            result.append(
                el('p', null, `Du hast ${seconds(read)} zum Lesen gebraucht.`),
                el('p', null, `Wer dieselben ${total} Wörter in ruhigem Vortragstempo sagt (etwa ${SPOKEN_PER_MINUTE} Wörter in der Minute), braucht ${seconds(spoken)}.`),
                el('p', 'rt-moral', read < spoken
                    ? 'Dein Publikum ist also lange vor dir fertig und wartet, oder es liest und hört dir nicht mehr zu.'
                    : 'Du hast gründlich gelesen. Wer vorn spricht, ist in dieser Zeit längst weiter, und was gesagt wurde, während du gelesen hast, hast du verpasst.'));
        };
        start.addEventListener('click', () => {
            at = 0;
            began = performance.now();
            result.hidden = true;
            start.hidden = true;
            next.hidden = false;
            show();
            tick();
            clearInterval(timer);
            timer = setInterval(tick, 250);
            next.focus();
        });
        next.addEventListener('click', () => {
            if (at < slides.length - 1) {
                at += 1;
                show();
            } else {
                finish();
            }
        });
        root.addEventListener('keydown', (e) => {
            if (next.hidden || (e.key !== 'ArrowRight' && e.key !== ' ')) return;
            e.preventDefault();
            next.click();
        });
        idle();
    }

    function init() {
        document.querySelectorAll('.reading-timer').forEach((root) => {
            if (root.dataset.ready) return;
            root.dataset.ready = '1';
            setUp(root);
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
    else init();
})();
