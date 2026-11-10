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
        // dragging sideways, with a mouse as with a finger: the picture
        // follows and the next or the previous one comes in beside it, in a
        // frame of the picture's size (the picture is on top of the card or
        // beside the text); past 40 px it slides on, else back. At the first
        // and the last picture it moves only a little. A drag opens no link,
        // and the browser's own dragging of links and pictures is off.
        link.draggable = false;
        card.querySelectorAll('img').forEach((img) => { img.draggable = false; });
        // the neighbours' pictures load once the pointer is on the card
        card.addEventListener('pointerenter', () => {
            card.querySelectorAll('img[loading=lazy]').forEach((img) => { img.loading = 'eager'; });
        }, { once: true });
        let startX = null, dx = 0, dragged = false, stage = null;
        const shown = (slide) => Array.from(slide.querySelectorAll('img'));
        function makeStage() {
            const slide = slides[current];
            const pictures = shown(slide);
            const img = pictures.find((i) => i.offsetWidth > 0);
            if (!img) return null;
            const box = slide.getBoundingClientRect(), pic = img.getBoundingClientRect();
            const frame = document.createElement('div');
            frame.className = 'card-series-stage';
            Object.assign(frame.style, { left: `${pic.left - box.left}px`, top: `${pic.top - box.top}px`,
                width: `${pic.width}px`, height: `${pic.height}px` });
            const strip = document.createElement('div');
            strip.className = 'card-series-strip';
            frame.append(strip);
            const add = (index, at) => {
                if (index < 0 || index >= slides.length) return;
                shown(slides[index]).forEach((source) => {
                    const copy = source.cloneNode();
                    copy.loading = 'eager';
                    Object.assign(copy.style, { left: `${at * pic.width}px`, width: `${pic.width}px`, height: `${pic.height}px` });
                    strip.append(copy);
                });
            };
            add(current - 1, -1);
            add(current, 0);
            add(current + 1, 1);
            slide.append(frame);
            pictures.forEach((i) => { i.style.visibility = 'hidden'; });
            return { frame, strip, pictures, width: pic.width };
        }
        function dropStage() {
            if (!stage) return;
            stage.pictures.forEach((i) => { i.style.visibility = ''; });
            stage.frame.remove();
            stage = null;
        }
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
                stage = makeStage();
            }
            if (!stage) return;
            const edge = (dx > 0 && current === 0) || (dx < 0 && current === slides.length - 1);
            stage.strip.style.transition = 'none';
            stage.strip.style.transform = `translateX(${edge ? dx / 5 : dx}px)`;
        });
        const end = () => {
            if (startX === null) return;
            startX = null;
            if (!stage) return;
            const edge = (dx > 0 && current === 0) || (dx < 0 && current === slides.length - 1);
            const go = dragged && !edge && Math.abs(dx) > 40 ? (dx < 0 ? 1 : -1) : 0;
            const finished = stage;
            finished.strip.style.transition = 'transform 0.22s ease-out';
            finished.strip.style.transform = `translateX(${-go * finished.width}px)`;
            finished.strip.addEventListener('transitionend', () => {
                if (stage !== finished) return;
                if (go) show(current + go);
                dropStage();
            }, { once: true });
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
