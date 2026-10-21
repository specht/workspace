// The bit-plane widget (steganography): a photo with only some of its bits.
// A slider with two handles over the bits 7 to 0 (the highest on the left,
// as in a binary number): the bits between them stay, every other bit of
// every channel becomes 0. »Verstärken« stretches what is left to the full
// range, so that the low bits, which on their own are almost black, can be
// seen; with both handles on one bit that is exactly a bit plane. Under the
// slider one pixel of the photo, its red value in binary, the kept bits
// marked.
//
//   <div class='bitplane-widget' data-x='300' data-y='40' data-credit='…'>
//   <img data-noconvert src='strandwagen.png' alt='…'></div>
//
// The photo is the <img> inside (the page serves it, without the script it
// is simply shown); a PNG, so that every bit is the photo's own.
(function () {
    'use strict';
    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) if (k === 'text') node.textContent = v; else node.setAttribute(k, v);
        for (const child of children) node.appendChild(typeof child === 'string' ? document.createTextNode(child) : child);
        return node;
    };
    let count = 0;
    function mount(el) {
        const id = `bitplane-${++count}`;
        const photo = el.querySelector('img');
        if (!photo) return;
        const canvas = html('canvas', { class: 'bitplane-picture', role: 'img', 'aria-label': photo.alt || '' });
        // the two handles: values are bit numbers, 7 on the left
        const high = html('input', { type: 'range', min: 0, max: 7, step: 1, value: 7, dir: 'rtl', 'aria-label': 'Höchstes Bit, das bleibt' });
        const low = html('input', { type: 'range', min: 0, max: 7, step: 1, value: 0, dir: 'rtl', 'aria-label': 'Niedrigstes Bit, das bleibt' });
        const cells = [7, 6, 5, 4, 3, 2, 1, 0].map((bit) => html('span', { class: 'bitplane-cell', 'data-bit': bit, text: String(bit) }));
        const stretch = html('input', { type: 'checkbox', class: 'form-check-input', id: `${id}-stretch` });
        const say = html('div', { class: 'bitplane-say', 'aria-live': 'polite' });
        const sample = html('div', { class: 'bitplane-sample' });
        el.replaceChildren(
            html('div', { class: 'bitplane-controls' }, [
                html('div', { class: 'bitplane-bits' }, [html('div', { class: 'bitplane-cells' }, cells), html('div', { class: 'bitplane-range' }, [high, low])]),
                html('div', { class: 'form-check' }, [stretch, html('label', { class: 'form-check-label', for: `${id}-stretch`, text: 'verstärken' })]),
            ]),
            say, canvas, sample);
        // the photo's credit, written in the page (data-credit, HTML)
        if (el.dataset.credit) {
            const credit = html('div', { class: 'bitplane-credit' });
            credit.innerHTML = el.dataset.credit;
            el.appendChild(credit);
        }
        const image = new Image();
        image.crossOrigin = 'anonymous';
        let source = null;
        const ctx = canvas.getContext('2d', { willReadFrequently: true });
        const draw = () => {
            if (!source) return;
            let hi = Number(high.value);
            let lo = Number(low.value);
            // the handles may cross: the range is between them either way
            if (lo > hi) [lo, hi] = [hi, lo];
            const width = hi - lo + 1;
            const keep = ((1 << width) - 1) << lo;
            const out = ctx.createImageData(source.width, source.height);
            const from = source.data;
            const to = out.data;
            const top = (1 << width) - 1;
            for (let i = 0; i < from.length; i += 4) {
                for (let c = 0; c < 3; c++) {
                    const v = from[i + c] & keep;
                    to[i + c] = stretch.checked ? Math.round(((v >> lo) * 255) / top) : v;
                }
                to[i + 3] = 255;
            }
            ctx.putImageData(out, 0, 0);
            for (const cell of cells) cell.classList.toggle('on', Number(cell.dataset.bit) >= lo && Number(cell.dataset.bit) <= hi);
            say.textContent = (lo === hi ? `Nur Bit ${lo}, die Bitebene ${lo}` : lo === 0 && hi === 7 ? 'Alle acht Bits: das Foto, wie es ist' : `Bits ${hi} bis ${lo}`)
                + (lo === 0 && hi === 7 ? '.' : `; die übrigen Bits jedes Kanals sind 0${stretch.checked ? ', und was bleibt, ist auf 0 bis 255 gestreckt' : ''}.`);
            // one pixel as an example: its red value, the kept bits marked
            const x = Number(el.dataset.x || 0);
            const y = Number(el.dataset.y || 0);
            const red = from[(y * source.width + x) * 4];
            const bits = red.toString(2).padStart(8, '0').split('').map((b, k) => html('span', { class: 7 - k >= lo && 7 - k <= hi ? 'kept' : 'gone', text: b }));
            const kept = red & keep;
            sample.replaceChildren(`Ein Pixel im Himmel, sein Rot: ${red} = `, html('code', {}, bits), ' → ', html('code', { text: kept.toString(2).padStart(8, '0') }),
                ` = ${kept}${stretch.checked && !(lo === 0 && hi === 7) ? `, gestreckt ${Math.round(((kept >> lo) * 255) / top)}` : ''}`);
        };
        image.addEventListener('load', () => {
            canvas.width = image.naturalWidth;
            canvas.height = image.naturalHeight;
            ctx.drawImage(image, 0, 0);
            source = ctx.getImageData(0, 0, canvas.width, canvas.height);
            draw();
        });
        image.src = photo.currentSrc || photo.src;
        for (const input of [high, low, stretch]) input.addEventListener('input', draw);
    }
    const start = () => document.querySelectorAll('.bitplane-widget').forEach(mount);
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
