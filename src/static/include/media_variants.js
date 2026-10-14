// Images and videos with variants (main.rb add_media_variants): the dark one
// in the dark theme, and for every toggle the reader switched on in an
// image's corner the matching one: the anaglyph pictures for the red-cyan
// glasses, the CRT pictures with the scanlines of a tube screen (PixelRAM),
// the HTML and CSS examples of the Handbuch on a Shower slide instead of a
// page. A toggle holds for every image on every page, until switched off
// again.
//
// The slide toggle follows the activity instead: an activity page says which
// view fits it (data-media-context='folie' for Shower, 'seite' for the web
// page), and the Handbuch pages opened from there in the same tab show that
// view. A click changes it until the next activity page; it is kept for the
// tab only (sessionStorage), so a choice made in one activity does not stick
// to the other.
(function () {
    const TOGGLES = [
        {
            name: 'anaglyph',
            storageKey: 'hackschule-anaglyph',
            title: 'Für die Rot-Cyan-Brille (Anaglyph 3D)',
            label: 'Bilder für die Rot-Cyan-Brille zeigen',
            // Cardboard 3D glasses: a red and a cyan lens.
            icon: "<svg class='icon-3d-glasses' viewBox='0 0 24 24' aria-hidden='true'><path d='M1.5 8.5H22.5V15a1.5 1.5 0 0 1-1.5 1.5H15.2l-1.6-2.4a1.9 1.9 0 0 0-3.2 0L8.8 16.5H3A1.5 1.5 0 0 1 1.5 15Z' fill='none' stroke='currentColor' stroke-width='1.5' stroke-linejoin='round'/><rect x='3.6' y='10.1' width='6.2' height='4.4' rx='0.8' fill='#e0342c'/><rect x='14.2' y='10.1' width='6.2' height='4.4' rx='0.8' fill='#19b6c8'/></svg>",
        },
        {
            name: 'crt',
            storageKey: 'hackschule-crt',
            title: 'Wie auf einem Röhrenbildschirm (CRT-Filter)',
            label: 'Bilder wie auf einem Röhrenbildschirm zeigen',
            // The CRT button of PixelRAM's own display: a screen with scanlines.
            icon: "<svg class='icon-crt' viewBox='0 0 24 24' aria-hidden='true'><rect x='3' y='4' width='18' height='15' rx='2'/><path d='M7 8h10M7 11.5h10M7 15h10'/></svg>",
        },
        {
            name: 'folie',
            storageKey: 'hackschule-folie',
            perTab: true,
            title: 'Als Folie zeigen (Shower)',
            label: 'Beispiele als Folie statt als Webseite zeigen',
            // A projection screen on its stand.
            icon: "<svg class='icon-slide' viewBox='0 0 24 24' aria-hidden='true'><rect x='3' y='4' width='18' height='12' rx='1.5'/><path d='M12 16v3.5M8.5 21l3.5-1.5 3.5 1.5M7 8.5h6M7 11.5h9'/></svg>",
        },
    ];
    const selector = ['dark', ...TOGGLES.map((t) => t.name)].map((name) => `[data-variant-${name}]`).join(', ');
    const media = [...document.querySelectorAll(selector)];
    if (media.length === 0) return;

    const store = (toggle) => (toggle.perTab ? sessionStorage : localStorage);
    const context = document.querySelector('[data-media-context]')?.dataset.mediaContext;
    TOGGLES.forEach((toggle) => {
        toggle.on = false;
        toggle.buttons = [];
        try {
            if (toggle.perTab && context) {
                toggle.on = context === toggle.name;
                store(toggle).setItem(toggle.storageKey, toggle.on ? '1' : '0');
            } else {
                toggle.on = store(toggle).getItem(toggle.storageKey) === '1';
            }
        } catch (e) {
            // Without storage a toggle only lasts for this page.
            if (toggle.perTab && context) toggle.on = context === toggle.name;
        }
    });

    const dark = () => document.documentElement.getAttribute('data-bs-theme') === 'dark';
    const variant = (element, name) => element.getAttribute(`data-variant-${name}`);

    // The closest variant there is: with a toggle on, a light one with it
    // beats a dark one without; a missing dark one falls back to light.
    function pick(element) {
        const on = TOGGLES.filter((t) => t.on && variant(element, t.name)).map((t) => t.name);
        const wanted = [];
        if (on.length > 0) {
            if (dark()) wanted.push(['dark', ...on].join('-'));
            wanted.push(on.join('-'));
        }
        if (dark()) wanted.push('dark');
        for (const name of wanted) {
            if (variant(element, name)) return variant(element, name);
        }
        return element.dataset.variantBase;
    }

    function apply() {
        media.forEach((element) => {
            const src = pick(element);
            if (element.getAttribute('src') === src) {
                delete element.dataset.variantWanted;
                return;
            }
            // The new file is loaded before it replaces the old one: an
            // image without a picture has no height, and the page would
            // jump up and down while it loads.
            element.dataset.variantWanted = src;
            const swap = () => {
                if (element.dataset.variantWanted !== src) return;
                delete element.dataset.variantWanted;
                element.setAttribute('src', src);
            };
            if (element.tagName === 'VIDEO') {
                element.style.minHeight = `${element.offsetHeight}px`;
                element.addEventListener('loadeddata', () => { element.style.minHeight = ''; }, { once: true });
                swap();
                element.play?.().catch(() => {});
            } else {
                const next = new Image();
                next.src = src;
                next.decode().then(swap, swap);
            }
        });
        TOGGLES.forEach((toggle) => {
            toggle.buttons.forEach((button) => button.setAttribute('aria-pressed', toggle.on ? 'true' : 'false'));
        });
    }

    media.forEach((element) => {
        element.dataset.variantBase = element.getAttribute('src');
        const toggles = TOGGLES.filter((t) => variant(element, t.name));
        if (toggles.length === 0) return;
        // A preview that enlarges on a click is a button itself; the
        // switches go beside it, not into it.
        const preview = element.closest('.tutorial-preview');
        // A wide image scrolls sideways in its .scroll-x (on a tablet): the
        // box goes around the scrolling part, so the switches stay in the
        // visible top right corner while the image moves beneath them.
        const scroller = !preview && element.parentElement && element.parentElement.classList.contains('scroll-x')
            ? element.parentElement : null;
        const framed = preview || scroller || element;
        const box = document.createElement('span');
        box.className = preview ? 'media-variant-box media-variant-box-preview'
            : scroller ? 'media-variant-box media-variant-box-scroll' : 'media-variant-box';
        framed.parentNode.insertBefore(box, framed);
        box.appendChild(framed);
        const bar = document.createElement('span');
        bar.className = 'media-variant-toggles';
        toggles.forEach((toggle) => {
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'media-variant-toggle';
            button.title = toggle.title;
            button.setAttribute('aria-label', toggle.label);
            button.innerHTML = toggle.icon;
            button.addEventListener('click', () => {
                toggle.on = !toggle.on;
                try {
                    store(toggle).setItem(toggle.storageKey, toggle.on ? '1' : '0');
                } catch (e) {
                    // See above.
                }
                apply();
            });
            bar.appendChild(button);
            toggle.buttons.push(button);
        });
        box.appendChild(bar);
    });

    new MutationObserver(apply).observe(document.documentElement, { attributes: true, attributeFilter: ['data-bs-theme'] });
    apply();
})();
