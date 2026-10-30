// The teacher note of an activity (main.rb, teacher_note_panel), opened by
// its button in the page's strip; every page starts with it closed. On a
// wide screen it opens beside the text, as a third column right of it: the
// left margin goes, the text gets narrower, and the note scrolls on its own
// while the text scrolls with the window. A link in the note to a place on
// the page (a heading by its text without its number, #die-karte-von-oben,
// or a task, #aufgabe-march) moves the text there and leaves the note where
// one was reading. A page without the side table of contents (a Kata) gets
// a layout of its own for it. Narrower than 1500 px it opens where it
// stands, above the text.
(function () {
    'use strict';
    const WIDE = window.matchMedia('(min-width: 1500px)');
    const STILL = window.matchMedia('(prefers-reduced-motion: reduce)');
    const toggle = document.querySelector('.teacher-note-toggle');
    const note = document.getElementById('teacher-note');
    if (!toggle || !note) return;

    // where the note stands in the page, to put it back there
    const home = document.createComment('teacher-note');
    note.parentNode.insertBefore(home, note);
    let open = false;
    let layout = null;

    // beside the text: a head with the note's name and a way to close it,
    // since the button in the strip scrolls away with the text
    const head = document.createElement('div');
    head.className = 'teacher-note-side-head';
    head.innerHTML = "<span><i class='bi bi-person-video3' aria-hidden='true'></i>Notiz für Lehrkräfte</span>" +
        "<button type='button' class='teacher-note-close' title='Notiz schließen' aria-label='Notiz schließen'>" +
        "<i class='bi bi-x-lg' aria-hidden='true'></i></button>";
    note.insertBefore(head, note.firstChild);
    head.querySelector('button').addEventListener('click', () => { set(false); toggle.focus(); });

    // the side table of contents' layout, or one of its own around the page
    function ensureLayout() {
        layout = document.querySelector('.autotoc-secondary-layout');
        if (layout) return;
        const page = toggle.closest('.container');
        if (!page) return;
        layout = document.createElement('div');
        layout.className = 'teacher-note-layout';
        page.parentNode.insertBefore(layout, page);
        layout.appendChild(page);
    }

    function place() {
        const side = open && WIDE.matches && layout;
        if (side && note.parentNode !== layout) layout.appendChild(note);
        if (!side && note.previousSibling !== home) home.parentNode.insertBefore(note, home.nextSibling);
        document.body.classList.toggle('has-teacher-note', !!layout);
        document.body.classList.toggle('teacher-note-side', !!side);
        note.hidden = !open;
        toggle.setAttribute('aria-expanded', open);
    }

    function set(value) {
        open = value;
        place();
    }

    toggle.addEventListener('click', () => set(!open));
    WIDE.addEventListener('change', place);

    // A place on the page: an element with that id outside the note, or a
    // heading whose text without its number is the name, as an id would
    // spell it (#die-karte-von-oben for »2. Die Karte von oben«): a
    // chapter's number is in its id and changes when chapters move
    const key = (text) => String(text).toLowerCase().trim().replace(/^\d+\.\s*/, '').replace(/\s+/g, '-')
        .replace(/[^\p{L}\p{N}-]+/gu, '').replace(/-+/g, '-').replace(/^-|-$/g, '');
    function find(name) {
        const element = document.getElementById(name);
        if (element && !note.contains(element)) return element;
        const want = key(name);
        const label = (h) => (typeof heading_label === 'function' ? heading_label(h) : h.textContent);
        return Array.from(document.querySelectorAll('h2, h3, h4'))
            .find((h) => !note.contains(h) && !h.closest('.autotoc-secondary, .autotoc') && key(label(h)) === want) || null;
    }

    note.addEventListener('click', (event) => {
        const link = event.target.closest('a[href^="#"]');
        if (!link) return;
        const target = find(decodeURIComponent(link.getAttribute('href').slice(1)));
        if (!target) return;
        event.preventDefault();
        const top = target.getBoundingClientRect().top + window.scrollY - 90;
        window.scrollTo({ top, behavior: STILL.matches ? 'auto' : 'smooth' });
        if (target.id) history.replaceState(null, '', '#' + target.id);
        // marked for a moment, so that the eye finds it
        target.classList.remove('teacher-note-target');
        void target.offsetWidth;
        target.classList.add('teacher-note-target');
    });

    // the side table of contents builds its layout once the page is there
    let started = false;
    const start = () => { if (!started) { started = true; ensureLayout(); place(); } };
    if (document.querySelector('.autotoc-secondary-layout')) start();
    else {
        document.addEventListener('autotoc-secondary-ready', start);
        // a page without the side table of contents never sends it
        if (document.readyState === 'complete') start();
        else window.addEventListener('load', start);
    }
})();
