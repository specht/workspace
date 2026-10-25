// A picture made of fine dots (a scan of a halftone print, Whitted's of
// 1979) shown so that every pixel of it covers whole pixels of the screen:
// 1, 2, 3 … device pixels, with nearest-neighbour scaling. At any other size
// the browser blends neighbouring dots, and the dots of the print and the
// pixels of the screen lay a second dot pattern over the picture (moiré):
// with display scaling of 125 or 150 %, with the browser's zoom, or in a
// column narrower than the picture.
//
//   <img class='drawing pixel-exact' src='whitted-1979.webp' alt='…'>
//   <img class='drawing pixel-exact-small' src='whitted-1979-grau.webp' alt='…' hidden>
//
// Where not even one screen pixel per picture pixel fits, or the picture
// would be too small to see (a phone with three device pixels per CSS
// pixel), the picture without dots right after it (pixel-exact-small) takes
// its place, scaled to the column like any picture. Without JavaScript the scan is shown as
// any picture.
(function () {
    // narrower than this (CSS pixels) the picture without dots is shown
    const MIN_WIDTH = 320;

    function fit(img) {
        const small = img.nextElementSibling && img.nextElementSibling.classList.contains('pixel-exact-small')
            ? img.nextElementSibling : null;
        const parent = img.parentElement;
        if (!img.naturalWidth || !parent) return;
        const dpr = window.devicePixelRatio || 1;
        const style = getComputedStyle(parent);
        const room = parent.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);
        const k = Math.floor(room * dpr / img.naturalWidth);
        const width = img.naturalWidth * k / dpr;
        // on a phone one picture pixel per device pixel is too small to see
        if (k >= 1 && width >= Math.min(room, MIN_WIDTH)) {
            img.hidden = false;
            img.style.width = width + 'px';
            img.style.imageRendering = 'pixelated';
            if (small) small.hidden = true;
        } else if (small) {
            img.hidden = true;
            small.hidden = false;
        } else {
            img.style.width = '';
            img.style.imageRendering = '';
        }
    }

    function install(img) {
        const refit = () => fit(img);
        if (img.complete && img.naturalWidth) refit();
        else img.addEventListener('load', refit, { once: true });
        new ResizeObserver(refit).observe(img.parentElement);
        // a zoom or a move to another screen changes devicePixelRatio
        window.addEventListener('resize', refit);
    }

    function installAll() {
        document.querySelectorAll('img.pixel-exact').forEach(install);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', installAll);
    else installAll();
})();
