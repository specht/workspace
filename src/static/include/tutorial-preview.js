// Pictures that open large on a click: the previews at the start of a
// tutorial (.tutorial-preview, a button around the picture) and every picture
// in a .gallery that is not a link, the grids of pictures with captions. A
// gallery picture fills the window, shows its caption, and ← → (or the
// buttons at the sides, or a swipe on a tablet) go through the pictures of
// its gallery. On a phone a preview already fills the width and does not
// open; a gallery picture opens there too when it is clearly narrower than
// the screen (two in a row). A gallery picture with class 'inspect' (a whole
// light map, a large plan) always opens and can be looked at closely there:
// the mouse wheel or two fingers zoom where they point, dragging moves it, a
// double click zooms in and back out.
document.addEventListener('DOMContentLoaded', () => {
    const previews = document.querySelectorAll('.tutorial-preview');
    const galleryImages = [...document.querySelectorAll('.gallery figure img')].filter((image) => !image.closest('a'));
    if (previews.length === 0 && galleryImages.length === 0) return;

    const ANIMATION_TIME = 180;
    const lightbox = document.createElement('dialog');
    lightbox.className = 'tutorial-preview-lightbox';
    lightbox.innerHTML = `
        <button class='tutorial-preview-lightbox-close' type='button' aria-label='Vorschau schließen'><i class='bi bi-x-lg' aria-hidden='true'></i></button>
        <button class='tutorial-preview-lightbox-step tutorial-preview-lightbox-prev' type='button' aria-label='Voriges Bild'><i class='bi bi-chevron-left' aria-hidden='true'></i></button>
        <button class='tutorial-preview-lightbox-step tutorial-preview-lightbox-next' type='button' aria-label='Nächstes Bild'><i class='bi bi-chevron-right' aria-hidden='true'></i></button>
        <figure class='tutorial-preview-lightbox-content'></figure>
        <div class='tutorial-preview-lightbox-dots'></div>
    `;

    lightbox.tabIndex = -1;
    document.body.appendChild(lightbox);

    const content = lightbox.querySelector('.tutorial-preview-lightbox-content');
    const closeButton = lightbox.querySelector('.tutorial-preview-lightbox-close');
    const prevButton = lightbox.querySelector('.tutorial-preview-lightbox-prev');
    const nextButton = lightbox.querySelector('.tutorial-preview-lightbox-next');
    const dots = lightbox.querySelector('.tutorial-preview-lightbox-dots');
    // The gallery picture that is open, to step from it; null for a preview.
    let current = null;

    const label = (image, fallback) =>
        image.alt.trim() ? `${image.alt.trim()} vergrößern` : fallback;

    previews.forEach((preview) => {
        const image = preview.querySelector('img');
        if (!image || preview.getAttribute('aria-label')) return;
        preview.setAttribute('aria-label', label(image, 'Vorschau vergrößern'));
    });

    // The breakpoint of tutorial-preview.css, where previews go full width
    const narrow = window.matchMedia('(max-width: 760px)');

    // a gallery picture opens large unless it already almost fills the screen
    const zoomable = (image) => !narrow.matches || image.classList.contains('inspect') || image.getBoundingClientRect().width < window.innerWidth * 0.75;

    function setupGallery() {
        galleryImages.forEach((image) => {
            const zoom = zoomable(image);
            image.classList.toggle('gallery-zoom', zoom);
            if (!zoom) {
                image.removeAttribute('tabindex');
                image.removeAttribute('role');
                image.removeAttribute('aria-label');
            } else {
                image.setAttribute('tabindex', '0');
                image.setAttribute('role', 'button');
                image.setAttribute('aria-label', label(image, 'Bild vergrößern'));
            }
        });
    }
    setupGallery();
    narrow.addEventListener('change', setupGallery);
    window.addEventListener('resize', setupGallery);

    function show() {
        // already open (the next picture of a gallery): stays visible, no fading
        if (lightbox.open && lightbox.classList.contains('is-visible')) return;
        lightbox.classList.remove('is-visible');
        if (!lightbox.open) {
            if (typeof lightbox.showModal === 'function') {
                lightbox.showModal();
            } else {
                lightbox.setAttribute('open', '');
            }
            // the dialog itself, not its first button: showModal() would focus ×
            // and draw a focus ring around it; Tab still reaches the buttons
            lightbox.focus({ preventScroll: true });
        }
        requestAnimationFrame(() => {
            lightbox.classList.add('is-visible');
        });
    }

    function openPreview(preview) {
        const previewImage = preview.querySelector('img');
        if (!previewImage) return;

        const image = document.createElement('img');
        image.src = preview.dataset.full || previewImage.src;
        image.alt = previewImage.alt || '';

        current = null;
        lightbox.classList.remove('is-gallery');
        content.replaceChildren(image);
        show();
    }

    // As large as the window allows, also larger than the file: the CPU
    // Sandbox pictures are a few hundred pixels wide. Pixel art that grows
    // a lot keeps sharp pixels instead of turning blurry.
    function fit(image) {
        if (!image.naturalWidth) return;
        const caption = content.querySelector('figcaption');
        const room = parseFloat(getComputedStyle(lightbox).paddingLeft) * 2;
        const width = window.innerWidth - room;
        const height = window.innerHeight - room - (caption ? caption.offsetHeight + 12 : 0);
        const scale = Math.min(width / image.naturalWidth, height / image.naturalHeight);
        image.style.width = `${Math.floor(image.naturalWidth * scale)}px`;
        image.style.height = `${Math.floor(image.naturalHeight * scale)}px`;
        image.style.imageRendering = scale >= 1.5 ? 'pixelated' : '';
        if (inspected && inspected.image === image) {
            inspected.fitted = scale;
            inspected.frame.style.width = image.style.width;
            inspected.frame.style.height = image.style.height;
            zoomTo(1, 0, 0);
        }
    }

    // A picture to inspect sits in a frame of its fitted size; zoomed in, the
    // frame covers the whole screen (from where the picture is, so it does not
    // jump), and the picture moves inside it. Scale 1 is the whole picture, at
    // most MAX_ZOOM pixels of the screen per pixel of the file; sharp pixels
    // from 1.5 on, like fit().
    const MAX_ZOOM = 12;
    const ZOOM_TIME = 220;   // a double click or tap glides in and out
    let inspected = null;   // { image, frame, fitted, scale, x, y }

    const place = (image, x, y, scale, time = 0) => {
        image.style.transition = time ? `transform ${time}ms ease` : 'none';
        image.style.transform = `translate(${x}px, ${y}px) scale(${scale})`;
    };

    const zoomed = (on) => {
        inspected.frame.classList.toggle('is-zoomed', on);
        lightbox.classList.toggle('is-zoomed', on);
    };

    function zoomTo(scale, x, y, animate = false) {
        const { image, frame, fitted } = inspected;
        const most = Math.max(1, MAX_ZOOM / fitted);
        scale = Math.min(Math.max(scale, 1), most);
        clearTimeout(inspected.settle);
        const wasZoomed = frame.classList.contains('is-zoomed');
        if (scale === 1 && wasZoomed && animate) {
            // back into the frame: glide to where the frame is, then leave the whole screen
            zoomed(false);
            const box = frame.getBoundingClientRect();
            zoomed(true);
            place(image, box.left, box.top, 1, ZOOM_TIME);
            Object.assign(inspected, { scale: 1, x: 0, y: 0 });
            inspected.settle = setTimeout(() => {
                if (!inspected || inspected.image !== image) return;   // closed meanwhile
                zoomed(false);
                place(image, 0, 0, 1);
                image.style.imageRendering = fitted >= 1.5 ? 'pixelated' : '';
            }, ZOOM_TIME);
            return;
        }
        if (scale > 1 && !wasZoomed) {
            // from the frame to the whole screen: the same place on the screen
            const box = frame.getBoundingClientRect();
            x += box.left;
            y += box.top;
            zoomed(true);
            place(image, box.left, box.top, 1);
            void image.offsetWidth;   // start the glide from there
        } else if (scale === 1) {
            zoomed(false);
            x = y = 0;
        }
        // the picture stays on the screen; where it is smaller, anywhere inside
        const width = frame.clientWidth, height = frame.clientHeight;
        const wide = image.offsetWidth * scale, high = image.offsetHeight * scale;
        x = Math.min(Math.max(width - wide, 0), Math.max(Math.min(width - wide, 0), x));
        y = Math.min(Math.max(height - high, 0), Math.max(Math.min(height - high, 0), y));
        Object.assign(inspected, { scale, x, y });
        place(image, x, y, scale, animate ? ZOOM_TIME : 0);
        image.style.imageRendering = fitted * scale >= 1.5 ? 'pixelated' : '';
    }

    // by FACTOR, the point under clientX, clientY stays where it is
    function zoomAt(clientX, clientY, factor, animate = false) {
        const box = inspected.frame.getBoundingClientRect();
        const px = clientX - box.left, py = clientY - box.top;
        const { scale, x, y } = inspected;
        const next = Math.min(Math.max(scale * factor, 1), Math.max(1, MAX_ZOOM / inspected.fitted));
        zoomTo(next, px - (px - x) * (next / scale), py - (py - y) * (next / scale), animate);
    }

    function inspect(image) {
        const frame = document.createElement('div');
        frame.className = 'tutorial-preview-lightbox-inspect';
        frame.append(image);
        inspected = { image, frame, fitted: 1, scale: 1, x: 0, y: 0 };
        const pointers = new Map();
        let last = null;   // where the fingers or the mouse were: { x, y, distance }
        const measure = () => {
            const points = [...pointers.values()];
            const x = points.reduce((sum, p) => sum + p.x, 0) / points.length;
            const y = points.reduce((sum, p) => sum + p.y, 0) / points.length;
            const distance = points.length > 1 ? Math.hypot(points[0].x - points[1].x, points[0].y - points[1].y) : 0;
            return { x, y, distance };
        };
        frame.addEventListener('wheel', (event) => {
            event.preventDefault();
            zoomAt(event.clientX, event.clientY, Math.exp(-event.deltaY * (event.deltaMode ? 0.04 : 0.0015)));
        }, { passive: false });
        // its own, not the swipe of the lightbox to the next picture
        frame.addEventListener('pointerdown', (event) => {
            event.stopPropagation();
            frame.setPointerCapture(event.pointerId);
            pointers.set(event.pointerId, { x: event.clientX, y: event.clientY });
            last = measure();
        });
        frame.addEventListener('pointermove', (event) => {
            if (!pointers.has(event.pointerId)) return;
            pointers.set(event.pointerId, { x: event.clientX, y: event.clientY });
            const now = measure();
            if (last && now.distance && last.distance) zoomAt(now.x, now.y, now.distance / last.distance);
            if (last) zoomTo(inspected.scale, inspected.x + now.x - last.x, inspected.y + now.y - last.y);
            last = now;
        });
        const lift = (event) => {
            pointers.delete(event.pointerId);
            last = pointers.size ? measure() : null;
        };
        frame.addEventListener('pointerup', lift);
        frame.addEventListener('pointercancel', lift);
        frame.addEventListener('dblclick', (event) => {
            if (inspected.scale > 1) zoomTo(1, 0, 0, true);
            else zoomAt(event.clientX, event.clientY, 4, true);
        });
        return frame;
    }

    function openGallery(source) {
        const figure = source.closest('figure');
        const image = document.createElement('img');
        // The variant on screen (dark, CRT, …), as media_variants.js chose it
        image.src = source.currentSrc || source.src;
        image.alt = source.alt || '';
        // invert-dark: black on white turns into the page colours in the
        // dark theme, large as small
        if (source.classList.contains('invert-dark')) image.classList.add('invert-dark');
        image.addEventListener('load', () => fit(image));

        const close = source.classList.contains('inspect');
        inspected = null;
        lightbox.classList.remove('is-zoomed');
        const parts = [close ? inspect(image) : image];
        const caption = figure && figure.querySelector('figcaption');
        if (caption || close) {
            const copy = document.createElement('figcaption');
            copy.innerHTML = caption ? caption.innerHTML : '';
            if (close) {
                const hint = document.createElement('span');
                hint.className = 'tutorial-preview-lightbox-hint';
                hint.textContent = narrow.matches
                    ? 'Mit zwei Fingern vergrößern, mit einem verschieben, doppelt tippen: hinein und zurück'
                    : 'Mausrad: vergrößern, ziehen: verschieben, Doppelklick: hinein und zurück';
                copy.append(hint);
            }
            parts.push(copy);
        }

        current = source;
        const siblings = gallerySiblings(source);
        lightbox.classList.add('is-gallery');
        lightbox.classList.toggle('has-steps', siblings.length > 1);
        showDots(siblings, source);
        content.replaceChildren(...parts);
        show();
        if (image.complete) fit(image);
    }

    // At the bottom a dot for each picture of the gallery, the open one
    // filled, a tap on one opens its picture. At most SLOTS are visible: the
    // open one in the middle, those further away smaller, the rest gone. The
    // row slides when stepping, the dots grow and shrink (tutorial-preview.css).
    const SLOTS = 7;
    let dotsFor = null;   // the gallery the dots belong to
    function showDots(siblings, source) {
        const index = siblings.indexOf(source);
        const gallery = source.closest('.gallery');
        if (siblings.length < 2) {
            dots.replaceChildren();
            dotsFor = null;
            return;
        }
        if (dotsFor !== gallery) {
            dotsFor = gallery;
            const track = document.createElement('div');
            track.className = 'tutorial-preview-lightbox-dots-track';
            track.append(...siblings.map((other, i) => {
                const dot = document.createElement('button');
                dot.type = 'button';
                dot.className = 'tutorial-preview-lightbox-dot';
                dot.setAttribute('aria-label', `Bild ${i + 1} von ${siblings.length}`);
                dot.addEventListener('click', () => openGallery(other));
                return dot;
            }));
            dots.replaceChildren(track);
            dots.style.setProperty('--slots', Math.min(SLOTS, siblings.length));
        }
        const track = dots.firstChild;
        const half = Math.floor(SLOTS / 2);
        const few = siblings.length <= SLOTS;
        [...track.children].forEach((dot, i) => {
            const away = Math.abs(i - index);
            const size = few || away <= 1 ? 1 : away === 2 ? 0.75 : away === half ? 0.45 : away < half ? 0.75 : 0;
            dot.style.setProperty('--size', size);
            dot.style.pointerEvents = size ? '' : 'none';
            dot.tabIndex = size ? 0 : -1;
            if (i === index) dot.setAttribute('aria-current', 'true');
            else dot.removeAttribute('aria-current');
        });
        track.style.setProperty('--shift', few ? 0 : half - index);
    }

    function gallerySiblings(image) {
        const gallery = image.closest('.gallery');
        return gallery ? [...gallery.querySelectorAll('figure img')].filter((other) => !other.closest('a')) : [image];
    }

    function step(delta) {
        if (!current) return;
        const siblings = gallerySiblings(current);
        if (siblings.length < 2) return;
        const index = siblings.indexOf(current);
        openGallery(siblings[(index + delta + siblings.length) % siblings.length]);
    }

    function closePreview() {
        if (!lightbox.open) return;

        lightbox.classList.remove('is-visible');

        window.setTimeout(() => {
            if (typeof lightbox.close === 'function') {
                lightbox.close();
            } else {
                lightbox.removeAttribute('open');
            }

            content.replaceChildren();
            inspected = null;
            lightbox.classList.remove('is-zoomed');
            if (current) current.focus({ preventScroll: true });
            current = null;
        }, ANIMATION_TIME);
    }

    document.addEventListener('click', (event) => {
        const preview = !narrow.matches && event.target.closest('.tutorial-preview');
        if (preview) {
            event.preventDefault();
            openPreview(preview);
            return;
        }
        const image = event.target.closest('.gallery-zoom');
        if (image) {
            event.preventDefault();
            openGallery(image);
        }
    });

    document.addEventListener('keydown', (event) => {
        if (lightbox.open) {
            // Esc here, not only through the dialog's cancel event: Chrome stops
            // sending that one after it was prevented (the fading out) without
            // a click in between, and the picture stayed open
            if (event.key === 'Escape') {
                event.preventDefault();
                closePreview();
                return;
            }
            if (event.key === 'ArrowLeft') step(-1);
            if (event.key === 'ArrowRight') step(1);
            return;
        }
        const image = event.target.closest && event.target.closest('.gallery-zoom');
        if (image && (event.key === 'Enter' || event.key === ' ')) {
            event.preventDefault();
            openGallery(image);
        }
    });

    window.addEventListener('resize', () => {
        const image = lightbox.open && lightbox.classList.contains('is-gallery') && content.querySelector('img');
        if (image) fit(image);
    });

    closeButton.addEventListener('click', closePreview);
    // after a click the lightbox keeps the focus, not the arrow: else, once a
    // key was pressed, the browser shows the keyboard's focus edge on it
    prevButton.addEventListener('click', () => { step(-1); lightbox.focus({ preventScroll: true }); });
    nextButton.addEventListener('click', () => { step(1); lightbox.focus({ preventScroll: true }); });

    // Swiping on a touch screen: the picture follows the finger sideways; let
    // go far enough (or quickly) and it slides out, the next one comes in
    // from the other side, both fully visible all the way (no fading).
    // Otherwise it springs back. Two fingers zoom as
    // usual (touch-action in tutorial-preview.css) and swipe nothing.
    const SWIPE_DISTANCE = 60;
    const SLIDE_TIME = 140;
    const still = window.matchMedia('(prefers-reduced-motion: reduce)');
    let swipe = null;
    let swiped = false;

    const canSwipe = () => lightbox.classList.contains('is-gallery') && lightbox.classList.contains('has-steps');

    function drag(dx) {
        content.style.transition = 'none';
        content.style.transform = `translateX(${dx}px)`;
    }

    function release() {
        content.style.transition = '';
        content.style.transform = '';
    }

    function swipeTo(delta) {
        if (still.matches) {
            release();
            step(delta);
            return;
        }
        const away = delta > 0 ? -1 : 1;
        content.style.transition = `transform ${SLIDE_TIME}ms ease-in`;
        content.style.transform = `translateX(${away * window.innerWidth}px)`;
        window.setTimeout(() => {
            step(delta);
            drag(-away * window.innerWidth);
            void content.offsetWidth;
            content.style.transition = `transform ${SLIDE_TIME}ms ease-out`;
            content.style.transform = '';
            window.setTimeout(release, SLIDE_TIME);
        }, SLIDE_TIME);
    }

    lightbox.addEventListener('pointerdown', (event) => {
        if (event.pointerType === 'mouse' || !canSwipe()) return;
        if (swipe) {
            // a second finger: zooming, not swiping
            swipe = null;
            release();
            return;
        }
        swipe = { id: event.pointerId, x: event.clientX, y: event.clientY, time: event.timeStamp, moving: false };
    });

    lightbox.addEventListener('pointermove', (event) => {
        if (!swipe || event.pointerId !== swipe.id) return;
        const dx = event.clientX - swipe.x;
        const dy = event.clientY - swipe.y;
        if (!swipe.moving) {
            if (Math.abs(dx) < 10 || Math.abs(dx) < Math.abs(dy)) return;
            swipe.moving = true;
        }
        drag(dx);
    });

    function endSwipe(event) {
        if (!swipe || event.pointerId !== swipe.id) return;
        const { moving, x, time } = swipe;
        swipe = null;
        if (!moving) return;
        swiped = true;
        window.setTimeout(() => { swiped = false; }, 400);
        const dx = event.clientX - x;
        const fast = Math.abs(dx) / Math.max(event.timeStamp - time, 1) > 0.5;
        if (event.type === 'pointerup' && (Math.abs(dx) > SWIPE_DISTANCE || (fast && Math.abs(dx) > 20))) {
            swipeTo(dx < 0 ? 1 : -1);
        } else {
            release();
        }
    }
    lightbox.addEventListener('pointerup', endSwipe);
    lightbox.addEventListener('pointercancel', endSwipe);

    lightbox.addEventListener('click', (event) => {
        // the end of a swipe on the dark part does not close the picture
        if (swiped) return;
        if (event.target === lightbox) {
            closePreview();
        }
    });

    lightbox.addEventListener('cancel', (event) => {
        event.preventDefault();
        closePreview();
    });
});
