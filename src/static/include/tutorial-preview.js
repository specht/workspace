document.addEventListener('DOMContentLoaded', () => {
    const previews = document.querySelectorAll('.tutorial-preview');
    if (previews.length === 0) return;

    const ANIMATION_TIME = 180;
    const lightbox = document.createElement('dialog');
    lightbox.className = 'tutorial-preview-lightbox';
    lightbox.innerHTML = `
        <button class='tutorial-preview-lightbox-close' type='button' aria-label='Vorschau schließen'>×</button>
        <div class='tutorial-preview-lightbox-content'></div>
    `;

    document.body.appendChild(lightbox);

    const content = lightbox.querySelector('.tutorial-preview-lightbox-content');
    const closeButton = lightbox.querySelector('.tutorial-preview-lightbox-close');

    previews.forEach((preview) => {
        const image = preview.querySelector('img');
        if (!image || preview.getAttribute('aria-label')) return;

        preview.setAttribute(
            'aria-label',
            image.alt.trim() ? `${image.alt.trim()} vergrößern` : 'Vorschau vergrößern'
        );
    });

    function openPreview(preview) {
        const previewImage = preview.querySelector('img');
        if (!previewImage) return;

        const image = document.createElement('img');
        image.src = preview.dataset.full || previewImage.src;
        image.alt = previewImage.alt || '';

        content.replaceChildren(image);
        lightbox.classList.remove('is-visible');

        if (typeof lightbox.showModal === 'function') {
            lightbox.showModal();
        } else {
            lightbox.setAttribute('open', '');
        }

        requestAnimationFrame(() => {
            lightbox.classList.add('is-visible');
        });
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
        }, ANIMATION_TIME);
    }

    document.addEventListener('click', (event) => {
        const preview = event.target.closest('.tutorial-preview');
        if (!preview) return;

        event.preventDefault();
        openPreview(preview);
    });

    closeButton.addEventListener('click', closePreview);

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
