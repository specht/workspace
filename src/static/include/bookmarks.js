// Bookmarks in the table of contents on the left (»Auf dieser Seite«), for
// a logged-in person (the template loads this only then): beside each
// entry a bookmark, which a click sets or removes; set ones stay visible as
// a filled bookmark, the others show on hover or focus. Any number per page.
// Kept on the server with the account (include/bookmarks.rb), so they follow
// the person to every computer.
(function () {
    'use strict';
    const page = () => (location.pathname.replace(/\/+$/, '') || '/').toLowerCase();

    function setUp() {
        const nav = document.querySelector('.autotoc-secondary');
        if (!nav || nav.dataset.bookmarks) return;
        nav.dataset.bookmarks = '1';
        const links = [...nav.querySelectorAll('.autotoc-secondary-items > a')];
        if (!links.length) return;
        const marked = new Set();
        const buttons = new Map();

        // hovering the bookmark itself shows what a click does: a plus to
        // set it, an × to remove it
        const show = (anchor) => {
            const button = buttons.get(anchor);
            if (!button) return;
            const on = marked.has(anchor);
            const hover = button.matches(':hover');
            button.classList.toggle('set', on);
            button.classList.toggle('preview', hover);
            button.setAttribute('aria-pressed', String(on));
            button.title = on ? 'Lesezeichen entfernen' : 'Lesezeichen setzen';
            button.querySelector('i').className = `bi ${hover ? (on ? 'bi-bookmark-x-fill' : 'bi-bookmark-plus') : (on ? 'bi-bookmark-fill' : 'bi-bookmark')}`;
        };

        links.forEach((link) => {
            const anchor = decodeURIComponent((link.getAttribute('href') || '').replace(/^#/, ''));
            if (!anchor) return;
            const entry = document.createElement('div');
            entry.className = 'toc-entry';
            link.before(entry);
            entry.append(link);
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'toc-bookmark';
            button.setAttribute('aria-label', `Lesezeichen: ${link.textContent.trim()}`);
            button.innerHTML = "<i class='bi bi-bookmark' aria-hidden='true'></i>";
            button.addEventListener('click', (event) => {
                event.preventDefault();
                event.stopPropagation();
                const on = !marked.has(anchor);
                if (on) marked.add(anchor); else marked.delete(anchor);
                show(anchor);
                api_call('/api/bookmarks/set', { page: page(), anchor, on: String(on) }, (data) => {
                    // the server refused (too many on this page): undo
                    if (!data || !data.ok) {
                        if (on) marked.delete(anchor); else marked.add(anchor);
                        show(anchor);
                    }
                }, { no_please_wait: true });
            });
            button.addEventListener('mouseenter', () => show(anchor));
            button.addEventListener('mouseleave', () => show(anchor));
            entry.append(button);
            entry.classList.toggle('level-2', link.classList.contains('level-2'));
            buttons.set(anchor, button);
            show(anchor);
        });

        api_call('/api/bookmarks/list', { page: page() }, (data) => {
            if (!data || !Array.isArray(data.anchors)) return;
            data.anchors.forEach((anchor) => { marked.add(anchor); show(anchor); });
        }, { no_please_wait: true });
    }

    document.addEventListener('autotoc-secondary-ready', setUp);
    // the table of contents may already be there
    if (document.readyState !== 'loading') setUp();
    else document.addEventListener('DOMContentLoaded', () => setTimeout(setUp, 0));
})();
