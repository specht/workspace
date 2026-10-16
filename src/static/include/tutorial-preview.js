// Pictures that open large on a click: the previews at the start of a
// tutorial (.tutorial-preview, a button around the picture) and every picture
// in a .gallery that is not a link, the grids of pictures with captions. A
// gallery picture fills the window, shows its caption, and ← → (or the
// buttons at the sides) go through the pictures of its gallery. On a phone
// the pictures already fill the width, so nothing opens there.
document.addEventListener('DOMContentLoaded', () => {
    const previews = document.querySelectorAll('.tutorial-preview');
    const galleryImages = [...document.querySelectorAll('.gallery figure img')].filter((image) => !image.closest('a'));
    if (previews.length === 0 && galleryImages.length === 0) return;

    const ANIMATION_TIME = 180;
    const lightbox = document.createElement('dialog');
    lightbox.className = 'tutorial-preview-lightbox';
    lightbox.innerHTML = `
        <button class='tutorial-preview-lightbox-close' type='button' aria-label='Vorschau schließen'>×</button>
        <button class='tutorial-preview-lightbox-step tutorial-preview-lightbox-prev' type='button' aria-label='Voriges Bild'>‹</button>
        <button class='tutorial-preview-lightbox-step tutorial-preview-lightbox-next' type='button' aria-label='Nächstes Bild'>›</button>
        <figure class='tutorial-preview-lightbox-content'></figure>
    `;

    document.body.appendChild(lightbox);

    const content = lightbox.querySelector('.tutorial-preview-lightbox-content');
    const closeButton = lightbox.querySelector('.tutorial-preview-lightbox-close');
    const prevButton = lightbox.querySelector('.tutorial-preview-lightbox-prev');
    const nextButton = lightbox.querySelector('.tutorial-preview-lightbox-next');
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

    function setupGallery() {
        galleryImages.forEach((image) => {
            image.classList.toggle('gallery-zoom', !narrow.matches);
            if (narrow.matches) {
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

    function show() {
        lightbox.classList.remove('is-visible');
        if (!lightbox.open) {
            if (typeof lightbox.showModal === 'function') {
                lightbox.showModal();
            } else {
                lightbox.setAttribute('open', '');
            }
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

        const parts = [image];
        const caption = figure && figure.querySelector('figcaption');
        if (caption) {
            const copy = document.createElement('figcaption');
            copy.innerHTML = caption.innerHTML;
            parts.push(copy);
        }

        current = source;
        const siblings = gallerySiblings(source);
        lightbox.classList.add('is-gallery');
        lightbox.classList.toggle('has-steps', siblings.length > 1);
        content.replaceChildren(...parts);
        show();
        if (image.complete) fit(image);
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
            if (current) current.focus({ preventScroll: true });
            current = null;
        }, ANIMATION_TIME);
    }

    document.addEventListener('click', (event) => {
        if (narrow.matches) return;
        const preview = event.target.closest('.tutorial-preview');
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
    prevButton.addEventListener('click', () => step(-1));
    nextButton.addEventListener('click', () => step(1));

    lightbox.addEventListener('click', (event) => {
        if (event.target === lightbox) {
            closePreview();
        }
    });

    lightbox.addEventListener('cancel', (event) => {
        event.preventDefault();
        closePreview();
    });
});
