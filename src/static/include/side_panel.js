// A reference in a panel from the right (side_panel.rb, »Auf einen Blick« in
// the Routenplaner): a tab on the right edge opens it, and so does every
// link to its id. On a wide screen the page moves aside while it is open, so
// that both stay readable, and the panel opens again after a reload if it
// was open; on a phone it covers the page, has to be opened again, and the
// browser's Back closes it.
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

    // Closes a panel, also one still sliding in (Bootstrap would add show
    // after a hide during the slide): then as soon as it is in.
    const OPEN = '.side-panel.show, .side-panel.showing';
    const close = function (panel, before) {
        const now = function () {
            if (before) before(true);
            try { bootstrap.Offcanvas.getOrCreateInstance(panel).hide(); } finally { if (before) before(false); }
        };
        if (panel.classList.contains('showing')) panel.addEventListener('shown.bs.offcanvas', now, { once: true });
        else now();
    };

    // Back on a phone: the panel whose history step was left closes
    let closingByBack = false;
    window.addEventListener('popstate', function () {
        if (!window.bootstrap) return;
        document.querySelectorAll(OPEN).forEach(function (panel) {
            if (history.state && history.state.sidePanel === panel.id) return;
            close(panel, function (on) { closingByBack = on; });
        });
    });

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
                // Where it covers the page (a phone), it is a step in the
                // history: Back closes it instead of leaving the page.
                if (!WIDE.matches && !(history.state && history.state.sidePanel === panel.id)) {
                    history.pushState({ sidePanel: panel.id }, '');
                }
            });
            panel.addEventListener('hide.bs.offcanvas', function () {
                document.documentElement.classList.remove('side-panel-open');
                remember(panel, false);
                // closed with × or Escape: take its step out of the history again
                if (!closingByBack && history.state && history.state.sidePanel === panel.id) history.back();
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
        document.querySelectorAll(OPEN).forEach(function (panel) { close(panel); });
    });

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setUp);
    else setUp();
})();
