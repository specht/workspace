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

// Every widget gets a button at its top right that shows it full screen, for
// a demo on the big screen: larger, the keys and the swipe work as before; Esc
// or the button again leaves. Where the browser cannot show an element full
// screen (an iPhone), the widget fills the window as a lightbox instead.
(() => {
    const setLabel = (button, full) => {
        button.replaceChildren(Object.assign(document.createElement('i'), { className: `bi ${full ? 'bi-fullscreen-exit' : 'bi-arrows-fullscreen'}` }));
        button.setAttribute('aria-label', full ? 'Vollbild verlassen' : 'Im Vollbild zeigen');
        button.title = button.getAttribute('aria-label');
    };
    const isFull = (widget) => document.fullscreenElement === widget || widget.classList.contains('stepper-lightbox');
    const toggle = (widget) => {
        if (isFull(widget)) {
            if (document.fullscreenElement === widget) document.exitFullscreen();
            widget.classList.remove('stepper-lightbox');
        } else if (widget.requestFullscreen && document.fullscreenEnabled) widget.requestFullscreen().catch(() => widget.classList.add('stepper-lightbox'));
        else widget.classList.add('stepper-lightbox');
        setTimeout(() => window.dispatchEvent(new Event('resize')), 60);
    };
    const add = () => {
        for (const widget of document.querySelectorAll('[data-stepper]')) {
            if (widget.querySelector(':scope > .stepper-full')) continue;
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'btn btn-sm btn-outline-secondary stepper-full';
            setLabel(button, false);
            button.addEventListener('click', () => toggle(widget));
            widget.appendChild(button);
        }
    };
    const update = () => {
        for (const button of document.querySelectorAll('.stepper-full')) setLabel(button, isFull(button.parentElement));
        window.dispatchEvent(new Event('resize'));
    };
    document.addEventListener('fullscreenchange', update);
    document.addEventListener('keydown', (event) => {
        if (event.key !== 'Escape') return;
        const box = document.querySelector('.stepper-lightbox');
        if (box) { box.classList.remove('stepper-lightbox'); update(); }
    });
    // after the widgets have built themselves
    if (document.readyState === 'complete') add();
    else window.addEventListener('load', add);
})();
