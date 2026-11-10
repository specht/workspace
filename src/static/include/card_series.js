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
        // follows, and past 40 px the next or the previous one comes; a
        // drag does not open the link, and the browser's own dragging of
        // links and pictures is off
        link.draggable = false;
        card.querySelectorAll('img').forEach((img) => { img.draggable = false; });
        let startX = null, dx = 0, dragged = false;
        const pictures = () => slides[current].querySelectorAll('img');
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
            }
            // past the first or the last picture it moves only a little
            const edge = (dx > 0 && current === 0) || (dx < 0 && current === slides.length - 1);
            if (dragged) pictures().forEach((img) => { img.style.transition = 'none'; img.style.transform = `translateX(${edge ? dx / 5 : dx}px)`; });
        });
        const end = () => {
            if (startX === null) return;
            startX = null;
            pictures().forEach((img) => { img.style.transition = ''; img.style.transform = ''; });
            if (dragged && Math.abs(dx) > 40) show(current + (dx < 0 ? 1 : -1));
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
