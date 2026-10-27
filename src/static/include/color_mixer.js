// Handbuch »Farben angeben«: the colour mixer and the Cling palette.
// RGB and OKLCH sliders show the same colour; every change updates the
// other set, the slider backgrounds (what moving that slider would do) and
// the codes. OKLCH conversion after Björn Ottosson's OKLab; colours outside
// sRGB are brought back by lowering the chroma until they fit.
(function () {
    const mixer = document.querySelector('.color-mixer');
    if (!mixer) return;

    // --- conversions ---------------------------------------------------
    const toLinear = (c) => (c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4));
    const fromLinear = (c) => (c <= 0.0031308 ? 12.92 * c : 1.055 * Math.pow(c, 1 / 2.4) - 0.055);

    // rgb: 0..255 each -> { l: 0..1, c, h: degrees }
    function rgbToOklch([r, g, b]) {
        const [lr, lg, lb] = [r, g, b].map((v) => toLinear(v / 255));
        const l = Math.cbrt(0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb);
        const m = Math.cbrt(0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb);
        const s = Math.cbrt(0.0883024619 * lr + 0.2817188376 * lg + 0.6299787005 * lb);
        const L = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s;
        const A = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s;
        const B = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s;
        const c = Math.hypot(A, B);
        let h = (Math.atan2(B, A) * 180) / Math.PI;
        if (h < 0) h += 360;
        return { l: L, c, h: c < 1e-4 ? 0 : h };
    }

    // { l, c, h } -> linear rgb (may be outside 0..1)
    function oklchToLinear({ l: L, c, h }) {
        const A = c * Math.cos((h * Math.PI) / 180);
        const B = c * Math.sin((h * Math.PI) / 180);
        const l = Math.pow(L + 0.3963377774 * A + 0.2158037573 * B, 3);
        const m = Math.pow(L - 0.1055613458 * A - 0.0638541728 * B, 3);
        const s = Math.pow(L - 0.0894841775 * A - 1.2914855480 * B, 3);
        return [
            4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
            -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
            -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s,
        ];
    }

    const inGamut = (lin) => lin.every((v) => v >= -0.0005 && v <= 1.0005);

    // { l, c, h } -> [r, g, b] 0..255 and whether the chroma had to shrink
    function oklchToRgb(color) {
        let lin = oklchToLinear(color);
        let clipped = false;
        if (!inGamut(lin)) {
            clipped = true;
            let lo = 0;
            let hi = color.c;
            for (let i = 0; i < 24; i++) {
                const mid = (lo + hi) / 2;
                if (inGamut(oklchToLinear({ ...color, c: mid }))) lo = mid; else hi = mid;
            }
            lin = oklchToLinear({ ...color, c: lo });
        }
        const rgb = lin.map((v) => Math.round(Math.min(1, Math.max(0, fromLinear(Math.min(1, Math.max(0, v))))) * 255));
        return { rgb, clipped };
    }

    const hex = (rgb) => '#' + rgb.map((v) => v.toString(16).padStart(2, '0')).join('');
    const parseHex = (text) => {
        let t = text.trim().replace(/^#/, '');
        if (/^[0-9a-f]{3}$/i.test(t)) t = t.split('').map((ch) => ch + ch).join('');
        if (!/^[0-9a-f]{6}$/i.test(t)) return null;
        return [0, 2, 4].map((i) => parseInt(t.slice(i, i + 2), 16));
    };
    const round = (v, digits) => {
        const f = Math.pow(10, digits);
        return Math.round(v * f) / f;
    };

    // --- state and elements --------------------------------------------
    const sliders = {};
    mixer.querySelectorAll('input[type=range]').forEach((input) => { sliders[input.dataset.channel] = input; });
    const hexInput = mixer.querySelector('.color-mixer-hex input');
    const sample = mixer.querySelector('.color-mixer-sample');
    const code = (name) => mixer.querySelector(`[data-code="${name}"]`);
    const palette = [...document.querySelectorAll('.color-palette-swatch')].map((b) => ({
        index: Number(b.dataset.index), color: b.dataset.color, lch: rgbToOklch(parseHex(b.dataset.color)),
    }));

    let rgb = parseHex(mixer.dataset.start || '#31a8e0');
    let lch = rgbToOklch(rgb);

    function nearest() {
        // Distance in OKLab, so »nearest« is what looks nearest.
        const lab = ({ l, c, h }) => [l, c * Math.cos((h * Math.PI) / 180), c * Math.sin((h * Math.PI) / 180)];
        const here = lab(lch);
        let best = null;
        let bestDistance = Infinity;
        for (const p of palette) {
            const there = lab(p.lch);
            const d = Math.hypot(here[0] - there[0], here[1] - there[1], here[2] - there[2]);
            if (d < bestDistance) { bestDistance = d; best = p; }
        }
        return best;
    }

    // A grey has no hue; keep the one the H slider had.
    function fromRgb(next) {
        rgb = next;
        const converted = rgbToOklch(rgb);
        if (converted.c < 0.0005) converted.h = lch.h;
        lch = converted;
    }

    function gradient(stops) {
        return `linear-gradient(to right, ${stops.join(', ')})`;
    }

    function paintSliders() {
        sliders.r.style.background = gradient([hex([0, rgb[1], rgb[2]]), hex([255, rgb[1], rgb[2]])]);
        sliders.g.style.background = gradient([hex([rgb[0], 0, rgb[2]]), hex([rgb[0], 255, rgb[2]])]);
        sliders.b.style.background = gradient([hex([rgb[0], rgb[1], 0]), hex([rgb[0], rgb[1], 255])]);
        const steps = 12;
        const along = (key, max) => Array.from({ length: steps + 1 }, (_, i) =>
            hex(oklchToRgb({ ...lch, [key]: (max * i) / steps }).rgb));
        sliders.l.style.background = gradient(along('l', 1));
        sliders.c.style.background = gradient(along('c', 0.37));
        sliders.h.style.background = gradient(along('h', 360));
    }

    function show({ from = null } = {}) {
        const color = hex(rgb);
        sample.style.background = color;
        if (from !== 'hex' && document.activeElement !== hexInput) hexInput.value = color;
        if (from !== 'rgb') ['r', 'g', 'b'].forEach((k, i) => { sliders[k].value = rgb[i]; });
        if (from !== 'lch') {
            sliders.l.value = lch.l * 100;
            sliders.c.value = lch.c;
            sliders.h.value = lch.h;
        }
        ['r', 'g', 'b'].forEach((k, i) => { sliders[k].nextElementSibling.textContent = rgb[i]; });
        sliders.l.nextElementSibling.textContent = `${round(lch.l * 100, 1)} %`;
        sliders.c.nextElementSibling.textContent = round(lch.c, 3).toFixed(3);
        sliders.h.nextElementSibling.textContent = `${Math.round(lch.h)}°`;

        const shown = rgbToOklch(rgb);
        code('hex').textContent = color;
        code('rgb').textContent = `rgb(${rgb.join(', ')})`;
        code('oklch').textContent = `oklch(${round(shown.l * 100, 1)}% ${round(shown.c, 3)} ${Math.round(shown.h)})`;
        code('anaglyph').textContent = `fill = ${rgb.map((v) => round(v / 255, 2)).join(', ')}`;
        const near = nearest();
        if (near) {
            code('nearest').textContent = `${near.index}: ${near.color}`;
            code('nearest-swatch').style.background = near.color;
        }
        paintSliders();
    }

    ['r', 'g', 'b'].forEach((k) => sliders[k].addEventListener('input', () => {
        fromRgb(['r', 'g', 'b'].map((c) => Number(sliders[c].value)));
        show({ from: 'rgb' });
    }));
    ['l', 'c', 'h'].forEach((k) => sliders[k].addEventListener('input', () => {
        lch = { l: Number(sliders.l.value) / 100, c: Number(sliders.c.value), h: Number(sliders.h.value) };
        const result = oklchToRgb(lch);
        rgb = result.rgb;
        show({ from: 'lch' });
    }));
    hexInput.addEventListener('input', () => {
        const parsed = parseHex(hexInput.value);
        if (!parsed) return;
        fromRgb(parsed);
        show({ from: 'hex' });
    });
    hexInput.addEventListener('blur', () => { hexInput.value = hex(rgb); });

    document.querySelectorAll('.color-palette-swatch').forEach((button) => button.addEventListener('click', () => {
        fromRgb(parseHex(button.dataset.color));
        show();
        document.querySelectorAll('.color-palette-swatch[aria-pressed]').forEach((b) => b.removeAttribute('aria-pressed'));
        button.setAttribute('aria-pressed', 'true');
    }));

    // A click on a code selects it, ready to copy.
    mixer.querySelectorAll('.color-mixer-codes code').forEach((el) => el.addEventListener('click', () => {
        const range = document.createRange();
        range.selectNodeContents(el);
        const selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
    }));

    show();
})();
