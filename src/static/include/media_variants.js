// Images and videos with variants (main.rb add_media_variants): the dark one
// in the dark theme, the anaglyph one once the reader switched on the
// glasses button in an image's corner. The glasses hold for every image on
// every page, until switched off again.
(function () {
    const STORAGE_KEY = 'hackschule-anaglyph';
    const media = [...document.querySelectorAll('[data-variant-dark], [data-variant-anaglyph], [data-variant-dark-anaglyph]')];
    if (media.length === 0) return;

    let anaglyph = false;
    try {
        anaglyph = localStorage.getItem(STORAGE_KEY) === '1';
    } catch (e) {
        // Without storage the glasses only last for this page.
    }

    const dark = () => document.documentElement.getAttribute('data-bs-theme') === 'dark';
    const variant = (element, name) => element.getAttribute(`data-variant-${name}`);

    // The closest variant there is: with glasses, a light one with glasses
    // beats a dark one without; a missing dark one falls back to light.
    function pick(element) {
        const wanted = [];
        if (dark() && anaglyph) wanted.push('dark-anaglyph');
        if (anaglyph) wanted.push('anaglyph');
        if (dark()) wanted.push('dark');
        for (const name of wanted) {
            if (variant(element, name)) return variant(element, name);
        }
        return element.dataset.variantBase;
    }

    const buttons = [];

    function apply() {
        media.forEach((element) => {
            const src = pick(element);
            if (element.getAttribute('src') === src) return;
            element.setAttribute('src', src);
            if (element.tagName === 'VIDEO') element.play?.().catch(() => {});
        });
        buttons.forEach((button) => button.setAttribute('aria-pressed', anaglyph ? 'true' : 'false'));
    }

    media.forEach((element) => {
        element.dataset.variantBase = element.getAttribute('src');
        if (!variant(element, 'anaglyph')) return;
        const box = document.createElement('span');
        box.className = 'media-variant-box';
        element.parentNode.insertBefore(box, element);
        box.appendChild(element);
        const button = document.createElement('button');
        button.type = 'button';
        button.className = 'media-variant-glasses';
        button.title = 'Für die Rot-Cyan-Brille (Anaglyph 3D)';
        button.setAttribute('aria-label', 'Bilder für die Rot-Cyan-Brille zeigen');
        // Cardboard 3D glasses: a red and a cyan lens.
        button.innerHTML = "<svg class='icon-3d-glasses' viewBox='0 0 24 24' aria-hidden='true'><path d='M1.5 8.5H22.5V15a1.5 1.5 0 0 1-1.5 1.5H15.2l-1.6-2.4a1.9 1.9 0 0 0-3.2 0L8.8 16.5H3A1.5 1.5 0 0 1 1.5 15Z' fill='none' stroke='currentColor' stroke-width='1.5' stroke-linejoin='round'/><rect x='3.6' y='10.1' width='6.2' height='4.4' rx='0.8' fill='#e0342c'/><rect x='14.2' y='10.1' width='6.2' height='4.4' rx='0.8' fill='#19b6c8'/></svg>";
        button.addEventListener('click', () => {
            anaglyph = !anaglyph;
            try {
                localStorage.setItem(STORAGE_KEY, anaglyph ? '1' : '0');
            } catch (e) {
                // See above.
            }
            apply();
        });
        box.appendChild(button);
        buttons.push(button);
    });

    new MutationObserver(apply).observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
    apply();
})();
