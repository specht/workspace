// ← and → step through a step-by-step widget: the Huffman tree, the LZW
// tables, the GIF decoder, the JPEG decoder. A widget is an element with
// data-stepper; its buttons say what they do with data-go (first, prev,
// next, last), and the keys press them, so a widget does nothing extra.
//
// With several widgets on a page, the keys step the one that takes up most
// of the window. They leave alone a text field (the cursor moves there), a
// key with Ctrl, Alt or Cmd, and an open dialog such as a picture opened
// large, which steps with the arrow keys itself.
document.addEventListener('keydown', (event) => {
    if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return;
    if (event.ctrlKey || event.altKey || event.metaKey || event.defaultPrevented) return;
    const target = event.target;
    if (target instanceof HTMLElement &&
        (target.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(target.tagName))) return;
    if (document.querySelector('dialog[open]')) return;

    let best = null;
    let bestArea = 0;
    for (const widget of document.querySelectorAll('[data-stepper]')) {
        const rect = widget.getBoundingClientRect();
        const width = Math.min(rect.right, window.innerWidth) - Math.max(rect.left, 0);
        const height = Math.min(rect.bottom, window.innerHeight) - Math.max(rect.top, 0);
        const area = Math.max(0, width) * Math.max(0, height);
        if (area > bestArea) {
            best = widget;
            bestArea = area;
        }
    }
    if (!best) return;

    const button = best.querySelector(`[data-go='${event.key === 'ArrowLeft' ? 'prev' : 'next'}']`);
    if (!button || button.disabled) return;
    event.preventDefault();
    button.click();
});

// On a touch screen a swipe to the left or right across a widget steps it,
// like turning a page: next to the left, back to the right. Not in a text
// field or on an element with data-no-swipe (a timeline to drag), and not when the swipe is more up and down than sideways (that
// scrolls the page).
(() => {
    let start = null;
    document.addEventListener('pointerdown', (event) => {
        if (event.pointerType !== 'touch') return;
        const widget = event.target instanceof Element && event.target.closest('[data-stepper]');
        if (!widget || event.target.closest('input, textarea, select, [contenteditable], [data-no-swipe]')) return;
        start = { widget, x: event.clientX, y: event.clientY, time: event.timeStamp };
    });
    document.addEventListener('pointerup', (event) => {
        if (!start || event.pointerType !== 'touch') return;
        const { widget, x, y, time } = start;
        start = null;
        const dx = event.clientX - x;
        const dy = event.clientY - y;
        if (Math.abs(dx) < 60 || Math.abs(dy) > Math.abs(dx) * 0.6 || event.timeStamp - time > 700) return;
        const button = widget.querySelector(`[data-go='${dx < 0 ? 'next' : 'prev'}']`);
        if (button && !button.disabled) button.click();
    });
    document.addEventListener('pointercancel', () => { start = null; });
})();

// Every widget gets a button at its top right that opens it in a lightbox
// over the page, larger, for a demo on the big screen. In the lightbox the
// keys and the sideways swipe step the widget as before; the arrows at the
// edges, Page Up and Page Down, or a swipe up and down go to the previous or
// next widget of the page. Esc, the button again or the browser's back
// button close it.
(() => {
    let open = null;
    let nav = null;
    // while open, the widget is moved under body (out of any stacking
    // context of the page, which would keep the page's header on top), and
    // a placeholder keeps its place
    let placeholder = null;
    const lift = (widget) => {
        placeholder = document.createElement('div');
        placeholder.style.height = `${widget.offsetHeight}px`;
        widget.before(placeholder);
        document.body.appendChild(widget);
    };
    const putBack = (widget) => {
        if (placeholder) placeholder.replaceWith(widget);
        placeholder = null;
    };
    const widgets = () => [...document.querySelectorAll('[data-stepper]')].filter((w) => w !== open).concat(open ? [open] : [])
        .sort((a, b) => ((a === open ? placeholder : a).compareDocumentPosition(b === open ? placeholder : b) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1));
    const setLabel = (button, full) => {
        button.replaceChildren(Object.assign(document.createElement('i'), { className: `bi ${full ? 'bi-fullscreen-exit' : 'bi-fullscreen'}` }));
        button.setAttribute('aria-label', full ? 'Lightbox schließen' : 'In der Lightbox zeigen');
        button.title = button.getAttribute('aria-label');
    };
    const navButton = (icon, label, go) => {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `btn btn-outline-secondary stepper-nav stepper-nav-${go < 0 ? 'prev' : 'next'}`;
        button.setAttribute('aria-label', label);
        button.title = label;
        button.appendChild(Object.assign(document.createElement('i'), { className: `bi ${icon}` }));
        button.addEventListener('click', () => move(go));
        return button;
    };
    // as large as possible and everything visible: the largest font size at
    // which the widget fits the lightbox without scrolling (a binary search;
    // the pictures grow with the font). Again when the window changes or the
    // widget redraws itself (a step).
    let fitting = 0;
    const fit = () => {
        if (!open) return;
        let lo = 12;
        let hi = 48;
        for (let i = 0; i < 8; i++) {
            const mid = (lo + hi) / 2;
            open.style.fontSize = `${mid}px`;
            if (open.scrollHeight <= open.clientHeight + 1 && open.scrollWidth <= open.clientWidth + 1) lo = mid;
            else hi = mid;
        }
        // a little below the limit, so that rounding leaves no scrolling
        open.style.fontSize = `${Math.floor(lo * 0.98 * 10) / 10}px`;
    };
    const fitSoon = () => { cancelAnimationFrame(fitting); fitting = requestAnimationFrame(fit); };
    const watch = new MutationObserver(fitSoon);
    window.addEventListener('resize', fitSoon);
    const show = (widget) => {
        if (open) { open.classList.remove('stepper-lightbox'); open.style.fontSize = ''; setLabel(open.querySelector(':scope > .stepper-full'), false); putBack(open); }
        lift(widget);
        open = widget;
        widget.classList.add('stepper-lightbox');
        document.documentElement.classList.add('stepper-lightbox-open');
        setLabel(widget.querySelector(':scope > .stepper-full'), true);
        if (!nav) {
            nav = document.createElement('div');
            nav.className = 'stepper-lightbox-nav';
            nav.append(navButton('bi-chevron-up', 'Voriges Widget', -1), navButton('bi-chevron-down', 'Nächstes Widget', 1));
            document.body.appendChild(nav);
        }
        const all = widgets();
        nav.querySelector('.stepper-nav-prev').disabled = all.indexOf(widget) <= 0;
        nav.querySelector('.stepper-nav-next').disabled = all.indexOf(widget) >= all.length - 1;
        nav.hidden = all.length < 2;
        widget.scrollTop = 0;
        watch.disconnect();
        watch.observe(widget, { childList: true, subtree: true });
        window.dispatchEvent(new Event('resize'));
        fit();
    };
    const close = (fromHistory) => {
        if (!open) return;
        open.classList.remove('stepper-lightbox');
        open.style.fontSize = '';
        watch.disconnect();
        setLabel(open.querySelector(':scope > .stepper-full'), false);
        putBack(open);
        open.scrollIntoView({ block: 'center' });
        open = null;
        document.documentElement.classList.remove('stepper-lightbox-open');
        if (nav) nav.hidden = true;
        if (!fromHistory && history.state && history.state.stepperLightbox) history.back();
        window.dispatchEvent(new Event('resize'));
    };
    const move = (go) => {
        const all = widgets();
        const next = all[all.indexOf(open) + go];
        if (next) show(next);
    };
    const add = () => {
        for (const widget of widgets()) {
            if (widget.querySelector(':scope > .stepper-full')) continue;
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'btn btn-sm btn-outline-secondary stepper-full';
            setLabel(button, false);
            button.addEventListener('click', () => {
                if (open === widget) close(false);
                else {
                    // the browser's back button closes the lightbox
                    if (!open) history.pushState({ stepperLightbox: true }, '');
                    show(widget);
                }
            });
            widget.appendChild(button);
        }
    };
    window.addEventListener('popstate', () => close(true));
    document.addEventListener('keydown', (event) => {
        if (!open) return;
        if (event.key === 'Escape') { event.preventDefault(); close(false); }
        else if (event.key === 'PageDown') { event.preventDefault(); move(1); }
        else if (event.key === 'PageUp') { event.preventDefault(); move(-1); }
    });
    // a swipe up or down in the lightbox: the next or the previous widget
    let start = null;
    document.addEventListener('pointerdown', (event) => {
        if (!open || event.pointerType !== 'touch' || !open.contains(event.target) || event.target.closest('[data-no-swipe]')) return;
        start = { x: event.clientX, y: event.clientY, time: event.timeStamp };
    });
    document.addEventListener('pointerup', (event) => {
        if (!start || !open) return;
        const dx = event.clientX - start.x;
        const dy = event.clientY - start.y;
        const quick = event.timeStamp - start.time < 700;
        start = null;
        if (quick && Math.abs(dy) > 80 && Math.abs(dx) < Math.abs(dy) * 0.5 && open.scrollHeight <= open.clientHeight + 2) move(dy < 0 ? 1 : -1);
    });
    // after the widgets have built themselves
    if (document.readyState === 'complete') add();
    else window.addEventListener('load', add);
})();
