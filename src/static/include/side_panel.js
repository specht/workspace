// A reference in a panel from the right (side_panel.rb, »Auf einen Blick« in
// the Routenplaner): a tab on the right edge opens it, and so does every
// link to its id. On a wide screen the page moves aside while it is open, so
// that both stay readable, and the panel opens again after a reload if it
// was open; on a phone it covers the page and has to be opened again.
(function () {
    'use strict';
    if (window.SidePanel) return;
    window.SidePanel = true;

    const WIDE = window.matchMedia('(min-width: 1200px)');
    const key = (panel) => 'side-panel:' + location.pathname + ':' + panel.id;
    const remember = (panel, open) => {
        try {
            if (open) localStorage.setItem(key(panel), '1');
            else localStorage.removeItem(key(panel));
        } catch (e) { /* no storage: the panel just starts closed */ }
    };
    const remembered = (panel) => {
        try { return localStorage.getItem(key(panel)) === '1'; } catch (e) { return false; }
    };

    function setUp() {
        document.querySelectorAll('.side-panel-box').forEach(function (box) {
            // Out of the chapters and the page's container, so that nothing
            // that dims or hides a part of the page (a meeting point) reaches
            // it and no stacking context of the page holds it below the menu;
            // the container's language (which parts to show) goes with it.
            const chooser = box.closest('[data-code-language]');
            document.body.appendChild(box);
            if (chooser) {
                const mirror = function () { box.dataset.codeLanguage = chooser.dataset.codeLanguage || ''; };
                mirror();
                new MutationObserver(mirror).observe(chooser, { attributes: true, attributeFilter: ['data-code-language'] });
            }
            const panel = box.querySelector('.side-panel');
            if (!panel || !window.bootstrap) return;
            const offcanvas = bootstrap.Offcanvas.getOrCreateInstance(panel);
            panel.addEventListener('show.bs.offcanvas', function () {
                document.documentElement.classList.add('side-panel-open');
                remember(panel, true);
            });
            panel.addEventListener('hide.bs.offcanvas', function () {
                document.documentElement.classList.remove('side-panel-open');
                remember(panel, false);
            });
            if (location.hash === '#' + panel.id || (WIDE.matches && remembered(panel))) offcanvas.show();
        });
    }

    // A link to the panel opens it instead of jumping; Ctrl or the middle
    // button still opens the page in a new tab, where the panel opens itself.
    document.addEventListener('click', function (e) {
        if (e.defaultPrevented || e.button !== 0 || e.ctrlKey || e.metaKey || e.shiftKey || e.altKey) return;
        const link = e.target.closest && e.target.closest('a[href^="#"]');
        if (!link) return;
        const panel = document.getElementById(decodeURIComponent(link.getAttribute('href').slice(1)));
        if (!panel || !panel.classList.contains('side-panel') || !window.bootstrap) return;
        e.preventDefault();
        bootstrap.Offcanvas.getOrCreateInstance(panel).show();
    });

    // Escape closes it from anywhere on the page (Bootstrap only listens
    // inside the panel), unless a dialog is open
    document.addEventListener('keydown', function (e) {
        if (e.key !== 'Escape' || e.defaultPrevented || document.querySelector('.modal.show') || !window.bootstrap) return;
        document.querySelectorAll('.side-panel.show').forEach(function (panel) {
            bootstrap.Offcanvas.getOrCreateInstance(panel).hide();
        });
    });

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setUp);
    else setUp();
})();
