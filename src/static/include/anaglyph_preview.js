// Pictures of Anaglyph scenes for the task card and the class gallery
// (check option preview: anaglyph, docs/aufgaben.md). The viewer
// (AnaglyphViewer in main.rb) draws each scene in a hidden frame and hands
// the picture over. It runs on abgaben.<host>, sandboxed, because a scene's
// expressions are JavaScript: the site gives each scene a signed address
// there (/api/anaglyph/address), and the frame talks to this page only
// through messages. Always at 1280 × 800, so a
// picture looks the same everywhere, and one scene at a time, because a
// browser allows only a few 3D canvases at once.
window.AnaglyphPreview = (function () {
    const WIDTH = 1280;
    const HEIGHT = 800;
    const TIMEOUT_MS = 120000;
    const queue = [];
    let busy = false;

    async function viewerUrl(scene, picture) {
        const response = await fetch('/api/anaglyph/address?scene=' + encodeURIComponent(scene),
            { credentials: 'same-origin' });
        if (!response.ok) throw new Error('Die Szene ließ sich nicht laden.');
        const data = await response.json();
        return data.url + '&abgabe' + (picture ? '&bild' : '');
    }

    // A frame for the viewer: scripts yes, the site's origin no.
    function viewerFrame(frame) {
        frame.setAttribute('sandbox', 'allow-scripts');
        return frame;
    }

    function next() {
        if (busy || queue.length === 0) return;
        busy = true;
        const job = queue.shift();
        const frame = viewerFrame(document.createElement('iframe'));
        frame.className = 'scene-picture-frame';
        frame.setAttribute('aria-hidden', 'true');
        frame.tabIndex = -1;
        // In the viewport, but invisible: the browser stops drawing frames
        // from another origin that are off screen.
        frame.style.cssText = `position: fixed; left: 0; top: 0; width: ${WIDTH}px; height: ${HEIGHT}px; border: 0; ` +
            'opacity: 0; pointer-events: none; z-index: -1;';
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
            // The sandboxed frame has an opaque origin; it is known by its window.
            if (event.source !== frame.contentWindow) return;
            const data = event.data;
            if (!data || data.type !== 'anaglyph-picture') return;
            const picture = typeof data.picture === 'string' && data.picture.startsWith('data:image/') ? data.picture : '';
            // What counts as an object is what scene_dump.js counts, without
            // the grid; the viewer counts it. Without the number, the
            // hand-in is not held back for it.
            const objects = Number.isInteger(data.objects) ? data.objects : null;
            done(null, { picture, errors: typeof data.errors === 'string' ? data.errors : '', objects });
        };
        window.addEventListener('message', onMessage);
        timer = setTimeout(() => done(new Error('Die Szene ließ sich nicht zeichnen.')), TIMEOUT_MS);
        viewerUrl(job.scene, true).then((url) => {
            frame.src = url;
            document.body.appendChild(frame);
        }).catch((error) => done(error));
    }

    // A picture of a scene: resolves with { picture (data URL), errors,
    // objects (how many, or null) }.
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
            "<button type='button' class='scene-present-button' data-step='-1' title='Vorherige Szene (←)' aria-label='Vorherige Szene'><i class='bi bi-chevron-left'></i></button>" +
            "<span class='scene-present-name'></span>" +
            "<button type='button' class='scene-present-button' data-step='1' title='Nächste Szene (→)' aria-label='Nächste Szene'><i class='bi bi-chevron-right'></i></button>" +
            "<span class='scene-present-space'></span>" +
            "<button type='button' class='scene-present-button scene-present-full' title='Vollbild' aria-label='Vollbild'><i class='bi bi-fullscreen'></i></button>" +
            "<button type='button' class='scene-present-button scene-present-close' title='Schließen (Esc)' aria-label='Schließen'><i class='bi bi-x-lg'></i></button>" +
            "</div><iframe class='scene-present-frame'></iframe>";
        const frame = viewerFrame(overlay.querySelector('iframe'));
        const name = overlay.querySelector('.scene-present-name');
        let current = index;

        function show(i) {
            current = (i + items.length) % items.length;
            name.textContent = items[current].name + (items.length > 1 ? ` (${current + 1} von ${items.length})` : '');
            const wanted = current;
            viewerUrl(items[current].scene, false).then((url) => {
                if (wanted !== current) return;
                frame.src = url;
                frame.focus();
            }).catch(() => {});
        }
        function close() {
            window.removeEventListener('message', onMessage);
            document.removeEventListener('keydown', onKey);
            document.removeEventListener('fullscreenchange', onFullscreen);
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
            if (event.source !== frame.contentWindow) return;
            if (event.data && event.data.type === 'anaglyph-key') key(event.data.key);
        }
        function onKey(event) {
            if (['ArrowLeft', 'ArrowRight', 'Escape'].includes(event.key)) key(event.key);
        }

        overlay.querySelectorAll('[data-step]').forEach((button) => button.addEventListener('click', () => show(current + Number(button.dataset.step))));
        overlay.querySelectorAll('[data-step]').forEach((button) => { button.hidden = items.length < 2; });
        overlay.querySelector('.scene-present-close').addEventListener('click', close);
        const full = overlay.querySelector('.scene-present-full');
        full.addEventListener('click', () => {
            if (document.fullscreenElement) document.exitFullscreen().catch(() => {});
            else overlay.requestFullscreen?.().catch(() => {});
        });
        // The button says what it does next: into full screen or out of it.
        const onFullscreen = () => {
            const on = document.fullscreenElement === overlay;
            full.querySelector('.bi').className = on ? 'bi bi-fullscreen-exit' : 'bi bi-fullscreen';
            full.title = on ? 'Vollbild beenden' : 'Vollbild';
            full.setAttribute('aria-label', full.title);
        };
        document.addEventListener('fullscreenchange', onFullscreen);
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
