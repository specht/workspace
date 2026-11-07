// /statistik (UsageStats): the number of a bar or a cell of the weekly
// grid, from its data-tip, in a small box above it: when the pointer is
// over it, or after a tap on a phone.
(function () {
    const tip = document.createElement('div');
    tip.className = 'usage-tip';
    tip.hidden = true;
    document.body.appendChild(tip);
    let active = null;

    function show(el) {
        if (active) active.classList.remove('usage-active');
        active = el;
        el.classList.add('usage-active');
        tip.textContent = el.dataset.tip;
        tip.hidden = false;
        const r = el.getBoundingClientRect();
        const w = tip.offsetWidth;
        const h = tip.offsetHeight;
        const x = Math.min(Math.max(4, r.left + r.width / 2 - w / 2), window.innerWidth - w - 4);
        const above = r.top - h - 6;
        tip.style.left = `${x}px`;
        tip.style.top = `${above > 4 ? above : r.bottom + 6}px`;
    }

    function hide() {
        if (active) active.classList.remove('usage-active');
        active = null;
        tip.hidden = true;
    }

    document.addEventListener('pointerover', (e) => {
        const el = e.target.closest('.usage-charts [data-tip]');
        if (el) show(el); else if (e.pointerType === 'mouse') hide();
    });
    document.addEventListener('click', (e) => {
        const el = e.target.closest('.usage-charts [data-tip]');
        if (el) show(el); else hide();
    });
    window.addEventListener('scroll', hide, { passive: true });
})();
