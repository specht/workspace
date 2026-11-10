// The card of a series in the catalog (sections.yaml series:, Main#card_body_html):
// one picture, title and text per member, the others hidden; the arrows,
// the dots and a swipe show another, and the link over the whole card
// leads to the one shown. Nothing moves by itself.
(function () {
    'use strict';
    function setUp(card) {
        const slides = Array.from(card.querySelectorAll('.card-series-slide'));
        const link = card.querySelector('.card-series-link');
        const dots = Array.from(card.querySelectorAll('.card-series-dots > span'));
        let current = 0;
        function show(index) {
            current = (index + slides.length) % slides.length;
            slides.forEach((slide, i) => { slide.hidden = i !== current; });
            dots.forEach((dot, i) => dot.classList.toggle('active', i === current));
            link.href = slides[current].dataset.href;
            link.querySelector('.visually-hidden').textContent = slides[current].getAttribute('aria-label');
        }
        const step = (by) => (event) => { event.preventDefault(); event.stopPropagation(); show(current + by); };
        card.querySelector('.card-series-prev').addEventListener('click', step(-1));
        card.querySelector('.card-series-next').addEventListener('click', step(1));
        // a swipe sideways on a touch screen
        let startX = null;
        card.addEventListener('touchstart', (e) => { startX = e.touches[0].clientX; }, { passive: true });
        card.addEventListener('touchend', (e) => {
            if (startX === null) return;
            const dx = e.changedTouches[0].clientX - startX;
            startX = null;
            if (Math.abs(dx) > 40) show(current + (dx < 0 ? 1 : -1));
        });
        show(0);
    }
    const go = () => document.querySelectorAll('[data-card-series]').forEach((card) => {
        if (!card.dataset.ready) { card.dataset.ready = '1'; setUp(card); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
