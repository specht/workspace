// The card of a series in the catalog (sections.yaml series:, Main#card_body_html):
// one picture, title and text per member, the others hidden; small dots at
// the foot of the picture and dragging sideways (mouse or finger) show another, and the link over the
// whole card leads to the one shown. Nothing moves by itself. The picture
// is on top of the card or beside the text, depending on the width, so the
// dots are placed under the picture shown.
(function () {
    'use strict';
    function setUp(card) {
        const slides = Array.from(card.querySelectorAll('.card-series-slide'));
        const link = card.querySelector('.card-series-link');
        const dotsBox = card.querySelector('.card-series-dots');
        const dots = Array.from(dotsBox.querySelectorAll('button'));
        let current = 0;
        function place() {
            const img = Array.from(slides[current].querySelectorAll('img')).find((i) => i.offsetWidth > 0);
            if (!img) return;
            const box = card.getBoundingClientRect(), pic = img.getBoundingClientRect();
            dotsBox.style.left = `${pic.left - box.left + pic.width / 2}px`;
            dotsBox.style.top = `${pic.bottom - box.top - dotsBox.offsetHeight - 6}px`;
        }
        // it stops at the first and the last picture, it does not go round
        function show(index) {
            current = Math.max(0, Math.min(slides.length - 1, index));
            slides.forEach((slide, i) => { slide.hidden = i !== current; });
            dots.forEach((dot, i) => {
                dot.classList.toggle('active', i === current);
                dot.setAttribute('aria-pressed', i === current ? 'true' : 'false');
            });
            link.href = slides[current].dataset.href;
            link.querySelector('.visually-hidden').textContent = slides[current].getAttribute('aria-label');
            place();
        }
        dots.forEach((dot, i) => dot.addEventListener('click', (event) => {
            event.preventDefault();
            event.stopPropagation();
            show(i);
        }));
        // dragging sideways, with a mouse as with a finger: all pictures
        // lie side by side in a frame of the picture's size (the picture is
        // on top of the card or beside the text), and one drag runs through
        // the whole gallery; the dots and the text follow the nearest one.
        // Past the first or the last picture the strip gives less the
        // further it is pulled, and springs back. Let go, it settles on the
        // nearest picture; a short flick of more than 40 px moves on by one.
        // A drag opens no link, and the browser's own dragging of links and
        // pictures is off.
        link.draggable = false;
        card.querySelectorAll('img').forEach((img) => { img.draggable = false; });
        // the other pictures load once the pointer is on the card
        card.addEventListener('pointerenter', () => {
            card.querySelectorAll('img[loading=lazy]').forEach((img) => { img.loading = 'eager'; });
        }, { once: true });
        let startX = null, dx = 0, dragged = false, stage = null, from = 0;
        const shown = (slide) => Array.from(slide.querySelectorAll('img'));
        function makeStage() {
            const img = shown(slides[current]).find((i) => i.offsetWidth > 0);
            if (!img) return null;
            const box = card.getBoundingClientRect(), pic = img.getBoundingClientRect();
            const frame = document.createElement('span');
            frame.className = 'card-series-stage';
            Object.assign(frame.style, { left: `${pic.left - box.left}px`, top: `${pic.top - box.top}px`,
                width: `${pic.width}px`, height: `${pic.height}px` });
            const strip = document.createElement('span');
            strip.className = 'card-series-strip';
            frame.append(strip);
            slides.forEach((slide, index) => shown(slide).forEach((source) => {
                const copy = source.cloneNode();
                copy.loading = 'eager';
                Object.assign(copy.style, { left: `${index * pic.width}px`, width: `${pic.width}px`, height: `${pic.height}px`, visibility: '' });
                strip.append(copy);
            }));
            card.append(frame);
            card.classList.add('card-series-dragging');
            return { frame, strip, width: pic.width };
        }
        function dropStage() {
            if (!stage) return;
            stage.frame.remove();
            card.classList.remove('card-series-dragging');
            stage = null;
        }
        // where the strip stands for a drag of dx: beyond the ends a pull that
        // gives less and less, never more than a third of a picture
        function offset(dx) {
            const w = stage.width, last = -(slides.length - 1) * w;
            const x = -from * w + dx;
            const give = (over) => (w / 3) * over / (over + w / 3);
            if (x > 0) return give(x);
            if (x < last) return last - give(last - x);
            return x;
        }
        const nearest = (x) => Math.max(0, Math.min(slides.length - 1, Math.round(-x / stage.width)));
        card.addEventListener('pointerdown', (e) => {
            if (e.button !== 0 || e.target.closest('.card-series-dots')) return;
            startX = e.clientX;
            dx = 0;
            dragged = false;
        });
        card.addEventListener('pointermove', (e) => {
            if (startX === null) return;
            dx = e.clientX - startX;
            if (Math.abs(dx) > 8 && !dragged) {
                dragged = true;
                card.setPointerCapture(e.pointerId);
                dropStage();
                from = current;
                stage = makeStage();
            }
            if (!stage) return;
            const x = offset(dx);
            stage.strip.style.transition = 'none';
            stage.strip.style.transform = `translateX(${x}px)`;
            const near = nearest(x);
            if (near !== current) show(near);
        });
        const end = () => {
            if (startX === null) return;
            startX = null;
            if (!stage) return;
            let target = nearest(offset(dx));
            // a short flick still moves on by one
            if (target === from && Math.abs(dx) > 40) target = Math.max(0, Math.min(slides.length - 1, from + (dx < 0 ? 1 : -1)));
            const finished = stage;
            if (target !== current) show(target);
            finished.strip.style.transition = 'transform 0.25s ease-out';
            finished.strip.style.transform = `translateX(${-target * finished.width}px)`;
            const done = () => { if (stage === finished) dropStage(); };
            finished.strip.addEventListener('transitionend', done, { once: true });
            setTimeout(done, 400);
        };
        card.addEventListener('pointerup', end);
        card.addEventListener('pointercancel', end);
        // the click that ends a drag
        link.addEventListener('click', (e) => {
            if (dragged) { e.preventDefault(); dragged = false; }
        });
        new ResizeObserver(place).observe(card);
        show(0);
    }
    const go = () => document.querySelectorAll('[data-card-series]').forEach((card) => {
        if (!card.dataset.ready) { card.dataset.ready = '1'; setUp(card); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
