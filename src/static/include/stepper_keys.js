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
