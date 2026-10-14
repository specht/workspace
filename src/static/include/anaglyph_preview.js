// Pictures of Anaglyph scenes for the task card and the class gallery
// (check option preview: anaglyph, docs/aufgaben.md). The site's copy of the
// viewer (/anaglyph-viewer/, AnaglyphViewer in main.rb) draws each scene in
// a hidden frame and hands the picture over. Always at 1280 × 800, so a
// picture looks the same everywhere, and one scene at a time, because a
// browser allows only a few 3D canvases at once.
window.AnaglyphPreview = (function () {
    const WIDTH = 1280;
    const HEIGHT = 800;
    const TIMEOUT_MS = 120000;
    const queue = [];
    let busy = false;

    function viewerUrl(scene, picture) {
        return '/anaglyph-viewer/index.html?scene=' + encodeURIComponent(scene) + '&abgabe' + (picture ? '&bild' : '');
    }

    function next() {
        if (busy || queue.length === 0) return;
        busy = true;
        const job = queue.shift();
        const frame = document.createElement('iframe');
        frame.className = 'scene-picture-frame';
        frame.setAttribute('aria-hidden', 'true');
        frame.tabIndex = -1;
        frame.style.cssText = `position: fixed; left: -${WIDTH + 100}px; top: 0; width: ${WIDTH}px; height: ${HEIGHT}px; border: 0;`;
        let timer = null;
        const done = (error, result) => {
            window.removeEventListener('message', onMessage);
            clearTimeout(timer);
            frame.remove();
            busy = false;
            if (error) job.reject(error); else job.resolve(result);
            next();
        };
        const onMessage = (event) => {
            if (event.origin !== window.location.origin || event.source !== frame.contentWindow) return;
            if (!event.data || event.data.type !== 'anaglyph-picture') return;
            done(null, { picture: event.data.picture, errors: event.data.errors || '' });
        };
        window.addEventListener('message', onMessage);
        timer = setTimeout(() => done(new Error('Die Szene ließ sich nicht zeichnen.')), TIMEOUT_MS);
        frame.src = viewerUrl(job.scene, true);
        document.body.appendChild(frame);
    }

    // A picture of a scene: resolves with { picture (data URL), errors }.
    function picture(scene) {
        return new Promise((resolve, reject) => {
            queue.push({ scene, resolve, reject });
            next();
        });
    }

    // Shows scenes one at a time, full size, in the real viewer: the
    // camera moves, the view buttons work. items: [{ scene, name }].
    function present(items, index) {
        const overlay = document.createElement('div');
        overlay.className = 'scene-present';
        overlay.setAttribute('role', 'dialog');
        overlay.setAttribute('aria-modal', 'true');
        overlay.innerHTML =
            "<div class='scene-present-bar'>" +
            "<button type='button' class='btn btn-sm btn-light' data-step='-1' title='Vorherige Szene (←)'><i class='bi bi-chevron-left'></i></button>" +
            "<span class='scene-present-name'></span>" +
            "<button type='button' class='btn btn-sm btn-light' data-step='1' title='Nächste Szene (→)'><i class='bi bi-chevron-right'></i></button>" +
            "<span class='scene-present-space'></span>" +
            "<button type='button' class='btn btn-sm btn-light scene-present-full' title='Vollbild'><i class='bi bi-arrows-fullscreen'></i></button>" +
            "<button type='button' class='btn btn-sm btn-light scene-present-close' title='Schließen (Esc)'><i class='bi bi-x-lg'></i></button>" +
            "</div><iframe class='scene-present-frame'></iframe>";
        const frame = overlay.querySelector('iframe');
        const name = overlay.querySelector('.scene-present-name');
        let current = index;

        function show(i) {
            current = (i + items.length) % items.length;
            name.textContent = items[current].name + (items.length > 1 ? ` (${current + 1} von ${items.length})` : '');
            frame.src = viewerUrl(items[current].scene, false);
            frame.focus();
        }
        function close() {
            window.removeEventListener('message', onMessage);
            document.removeEventListener('keydown', onKey);
            if (document.fullscreenElement === overlay) document.exitFullscreen().catch(() => {});
            overlay.remove();
        }
        function key(k) {
            if (k === 'ArrowLeft') show(current - 1);
            else if (k === 'ArrowRight') show(current + 1);
            else if (k === 'Escape') close();
        }
        // Keys pressed while the viewer has the focus arrive as messages.
        function onMessage(event) {
            if (event.origin !== window.location.origin || event.source !== frame.contentWindow) return;
            if (event.data && event.data.type === 'anaglyph-key') key(event.data.key);
        }
        function onKey(event) {
            if (['ArrowLeft', 'ArrowRight', 'Escape'].includes(event.key)) key(event.key);
        }

        overlay.querySelectorAll('[data-step]').forEach((button) => button.addEventListener('click', () => show(current + Number(button.dataset.step))));
        overlay.querySelectorAll('[data-step]').forEach((button) => { button.hidden = items.length < 2; });
        overlay.querySelector('.scene-present-close').addEventListener('click', close);
        overlay.querySelector('.scene-present-full').addEventListener('click', () => {
            if (document.fullscreenElement) document.exitFullscreen().catch(() => {});
            else overlay.requestFullscreen?.().catch(() => {});
        });
        window.addEventListener('message', onMessage);
        document.addEventListener('keydown', onKey);
        document.body.appendChild(overlay);
        show(index);
    }

    // The class gallery: a picture for every handed-in scene, drawn one
    // after another; a click shows the scenes full size.
    function installGalleries() {
        document.querySelectorAll('.scene-gallery').forEach((gallery) => {
            const figures = [...gallery.querySelectorAll('.scene-gallery-item')];
            const items = figures.map((figure) => ({ scene: figure.dataset.scene, name: figure.dataset.name }));
            figures.forEach((figure, i) => {
                figure.querySelector('button').addEventListener('click', () => present(items, i));
                const img = figure.querySelector('img');
                picture(items[i].scene).then((result) => {
                    img.src = result.picture;
                    figure.classList.add('scene-gallery-ready');
                    if (result.errors) figure.classList.add('scene-gallery-errors');
                }).catch(() => figure.classList.add('scene-gallery-failed'));
            });
        });
    }

    if (document.readyState === 'loading') window.addEventListener('DOMContentLoaded', installGalleries);
    else installGalleries();
    return { picture, present };
})();
