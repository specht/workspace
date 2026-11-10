// The card of a series in the catalog (sections.yaml series:, Main#card_body_html):
// one picture, title and text per member, the others hidden; small dots at
// the foot of the picture and a swipe show another, and the link over the
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
        function show(index) {
            current = (index + slides.length) % slides.length;
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
        // a swipe sideways on a touch screen
        let startX = null;
        card.addEventListener('touchstart', (e) => { startX = e.touches[0].clientX; }, { passive: true });
        card.addEventListener('touchend', (e) => {
            if (startX === null) return;
            const dx = e.changedTouches[0].clientX - startX;
            startX = null;
            if (Math.abs(dx) > 40) show(current + (dx < 0 ? 1 : -1));
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
