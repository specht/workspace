const TUTORIAL_SCREENSHOT_LIVE_AVAILABLE = true;
const tutorialScreenshotFresh = new Set();
const tutorialScreenshotPending = new Set();


function ensure_tutorial_screenshot_ui(showFilmstrip = false) {
    let monitor = document.getElementById('tutorial-screenshot-monitor');
    if (!monitor) {
        const footerLeft = document.querySelector('.alt-review-footer-left');
        if (footerLeft) {
            monitor = document.createElement('span');
            monitor.id = 'tutorial-screenshot-monitor';
            monitor.className = 'tutorial-screenshot-monitor';
            monitor.setAttribute('aria-live', 'polite');

            const icon = document.createElement('i');
            icon.id = 'tutorial-screenshot-monitor-icon';
            icon.className = 'bi bi-arrow-repeat bi-spin';
            const text = document.createElement('span');
            text.id = 'tutorial-screenshot-monitor-text';
            const progress = document.createElement('progress');
            progress.id = 'tutorial-screenshot-progress';
            progress.max = 1;
            progress.value = 0;
            monitor.append(icon, text, progress);
            footerLeft.appendChild(monitor);
        }
    }

    let strip = document.getElementById('tutorial-screenshot-filmstrip');
    if (!strip) {
        const footer = document.querySelector('footer');
        if (footer) {
            strip = document.createElement('div');
            strip.id = 'tutorial-screenshot-filmstrip';
            strip.className = 'tutorial-screenshot-filmstrip';
            strip.setAttribute('aria-label', 'Tutorial-Screenshots');
            footer.appendChild(strip);
        }
    }

    if (showFilmstrip && strip) {
        strip.classList.add('visible');
    }

    return { monitor, strip };
}

function tutorial_screenshot_slug() {
    const match = window.location.pathname.match(/^\/([a-zA-Z0-9_-]+)$/);
    return match ? match[1] : null;
}

function normalize_tutorial_screenshot_source(source) {
    return `${source || ''}`
        .split('#', 1)[0]
        .split('?', 1)[0]
        .replace(/^\.\//, '');
}

function tutorial_screenshot_images(target) {
    return Array.from(document.querySelectorAll('img[data-tutorial-screenshot-source]'))
        .filter(img => normalize_tutorial_screenshot_source(
            img.dataset.tutorialScreenshotSource,
        ) === target);
}

function tutorial_screenshot_elements(className, target) {
    return Array.from(document.querySelectorAll(`.${className}`))
        .filter(element => element.dataset.tutorialScreenshotTarget === target);
}

function mark_tutorial_screenshot_stale(item) {
    tutorial_screenshot_images(item.target).forEach(img => {
        if (item.missing) {
            if (!tutorial_screenshot_elements('tutorial-screenshot-placeholder', item.target).length) {
                const placeholder = document.createElement('div');
                placeholder.className = 'tutorial-screenshot-placeholder';
                placeholder.dataset.tutorialScreenshotTarget = item.target;
                placeholder.style.width = `${item.width}px`;
                placeholder.style.aspectRatio = `${item.width} / ${item.height}`;
                const label = document.createElement('span');
                label.textContent = `${item.target} · ${item.width}×${item.height} · wird gerendert …`;
                placeholder.appendChild(label);
                img.insertAdjacentElement('beforebegin', placeholder);
            }
            if (!img.dataset.tutorialScreenshotOriginalDisplay) {
                img.dataset.tutorialScreenshotOriginalDisplay = img.style.display || '__empty__';
            }
            img.style.display = 'none';
            return;
        }

        img.classList.add('tutorial-screenshot-stale');
        if (!tutorial_screenshot_elements('tutorial-screenshot-stale-note', item.target).length) {
            const note = document.createElement('div');
            note.className = 'tutorial-screenshot-stale-note';
            note.dataset.tutorialScreenshotTarget = item.target;
            note.textContent = `Veralteter Screenshot: ${item.target} – wird neu gerendert …`;
            const anchor = img.closest('.scroll-x') || img;
            anchor.insertAdjacentElement('afterend', note);
        }
    });
}

function clear_tutorial_screenshot_stale(target) {
    tutorial_screenshot_images(target).forEach(img => {
        img.classList.remove('tutorial-screenshot-stale');
        const originalDisplay = img.dataset.tutorialScreenshotOriginalDisplay;
        if (originalDisplay) {
            img.style.display = originalDisplay === '__empty__' ? '' : originalDisplay;
            delete img.dataset.tutorialScreenshotOriginalDisplay;
        }
    });
    tutorial_screenshot_elements('tutorial-screenshot-placeholder', target)
        .forEach(element => element.remove());
    tutorial_screenshot_elements('tutorial-screenshot-stale-note', target)
        .forEach(element => element.remove());
}

function tutorial_screenshot_image_url(slug, item) {
    const params = new URLSearchParams({
        slug,
        target: item.target,
    });
    if (item.revision) params.set('v', item.revision);
    return `/api/tutorial_screenshots/image?${params.toString()}`;
}

function tutorial_screenshot_item_label(item) {
    return item.label || item.target;
}

function tutorial_screenshot_should_be_in_filmstrip(item) {
    if (!item.revision) return false;
    if (item.error_preview || item.state === 'error' || item.state === 'fresh') return true;
    return item.stale === false;
}

function open_tutorial_screenshot_lightbox(src, target) {
    const overlay = document.createElement('div');
    overlay.className = 'tutorial-screenshot-lightbox';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', `Screenshot ${target}`);

    const figure = document.createElement('figure');
    const img = document.createElement('img');
    img.src = src;
    img.alt = `Neu gerenderter Screenshot ${target}`;
    const caption = document.createElement('figcaption');
    caption.textContent = target;
    figure.append(img, caption);
    overlay.appendChild(figure);

    const close = () => {
        document.removeEventListener('keydown', onKeydown);
        overlay.remove();
    };
    const onKeydown = event => {
        if (event.key === 'Escape') close();
    };
    overlay.addEventListener('click', event => {
        if (event.target === overlay) close();
    });
    document.addEventListener('keydown', onKeydown);
    document.body.appendChild(overlay);
}

function append_tutorial_screenshot_filmstrip(slug, item) {
    const { strip } = ensure_tutorial_screenshot_ui(true);
    if (!strip || !tutorial_screenshot_should_be_in_filmstrip(item)) return;

    const key = `${item.target}:${item.revision}`;
    if (Array.from(strip.children).some(
        element => element.dataset.tutorialScreenshotKey === key,
    )) return;

    const src = tutorial_screenshot_image_url(slug, item);
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'tutorial-screenshot-filmstrip-item';
    button.classList.toggle('error', item.error_preview || item.state === 'error');
    button.dataset.tutorialScreenshotKey = key;
    button.title = `${tutorial_screenshot_item_label(item)} groß anzeigen`;

    const img = document.createElement('img');
    img.src = src;
    img.alt = '';
    const label = document.createElement('span');
    label.textContent = tutorial_screenshot_item_label(item);
    button.append(img, label);
    button.addEventListener('click', () => open_tutorial_screenshot_lightbox(src, tutorial_screenshot_item_label(item)));

    strip.appendChild(button);
    strip.classList.add('visible');
    requestAnimationFrame(() => {
        strip.scrollLeft = strip.scrollWidth;
    });
}

function sync_tutorial_screenshot_filmstrip(slug, snapshot) {
    ensure_tutorial_screenshot_ui(true);
    (snapshot.screenshots || []).forEach(item => {
        append_tutorial_screenshot_filmstrip(slug, item);
    });
}

function hot_swap_tutorial_screenshot(slug, item) {
    if (item.state !== 'fresh' || !item.revision) return;
    const key = `${item.target}:${item.revision}`;
    if (tutorialScreenshotFresh.has(key) || tutorialScreenshotPending.has(key)) return;
    tutorialScreenshotPending.add(key);

    const src = tutorial_screenshot_image_url(slug, item);
    const images = tutorial_screenshot_images(item.target);
    let remaining = images.length;
    let failed = false;

    const completed = () => {
        tutorialScreenshotPending.delete(key);
        if (failed) return;
        tutorialScreenshotFresh.add(key);
        clear_tutorial_screenshot_stale(item.target);
        append_tutorial_screenshot_filmstrip(slug, item);
        if (localStorage.getItem('hackschule-alt-review-enabled') === '1' &&
            typeof annotate_alt_review === 'function') {
            annotate_alt_review();
        }
    };

    if (remaining === 0) {
        completed();
        return;
    }

    images.forEach(img => {
        const done = success => {
            img.removeEventListener('load', onLoad);
            img.removeEventListener('error', onError);
            failed ||= !success;
            remaining -= 1;
            if (remaining === 0) completed();
        };
        const onLoad = () => done(true);
        const onError = () => done(false);
        img.addEventListener('load', onLoad);
        img.addEventListener('error', onError);
        img.src = src;
    });
}

function update_tutorial_screenshot_monitor(snapshot) {
    const { monitor } = ensure_tutorial_screenshot_ui(true);
    const text = document.getElementById('tutorial-screenshot-monitor-text');
    const progress = document.getElementById('tutorial-screenshot-progress');
    const icon = document.getElementById('tutorial-screenshot-monitor-icon');
    if (!monitor || !text || !progress || !icon) return;

    monitor.classList.add('visible');
    monitor.classList.toggle('error', snapshot.status === 'error');
    monitor.classList.toggle('done', snapshot.status === 'done');

    const total = Math.max(1, Number(snapshot.total) || 1);
    const completed = Math.min(total, Number(snapshot.completed) || 0);
    progress.max = total;
    progress.value = completed;

    icon.className = 'bi';
    if (snapshot.status === 'done') {
        icon.classList.add('bi-check-lg');
        text.textContent = `Screenshots fertig · ${snapshot.generated || 0} neu`;
    } else if (snapshot.status === 'error') {
        icon.classList.add('bi-exclamation-triangle-fill');
        text.textContent = `Screenshots fehlgeschlagen${snapshot.error ? `: ${snapshot.error}` : ''}`;
    } else if (snapshot.status === 'queued') {
        icon.classList.add('bi-hourglass-split');
        text.textContent = `Screenshots warten · ${completed}/${snapshot.total || 0}`;
    } else if (snapshot.status === 'preparing') {
        icon.classList.add('bi-arrow-repeat', 'bi-spin');
        text.textContent = `Screenshots vorbereiten · ${completed}/${snapshot.total || 0}`;
    } else {
        icon.classList.add('bi-arrow-repeat', 'bi-spin');
        const target = snapshot.current_target ? ` · ${snapshot.current_target}` : '';
        text.textContent = `Screenshots ${completed}/${snapshot.total || 0}${target}`;
    }
}

async function fetch_tutorial_screenshot_status(slug) {
    const response = await fetch(
        `/api/tutorial_screenshots/status?${new URLSearchParams({ slug })}`,
        { cache: 'no-store' },
    );
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    return response.json();
}

function apply_tutorial_screenshot_status(slug, snapshot) {
    sync_tutorial_screenshot_filmstrip(slug, snapshot);
    (snapshot.screenshots || []).forEach(item => {
        if (item.stale) mark_tutorial_screenshot_stale(item);
        hot_swap_tutorial_screenshot(slug, item);
    });
    update_tutorial_screenshot_monitor(snapshot);
}

async function install_tutorial_screenshot_live_preview() {
    if (!TUTORIAL_SCREENSHOT_LIVE_AVAILABLE) return;
    const slug = tutorial_screenshot_slug();
    if (!slug) return;

    let snapshot;
    try {
        snapshot = await fetch_tutorial_screenshot_status(slug);
    } catch (error) {
        console.warn('Tutorial screenshot status unavailable:', error);
        return;
    }

    if (snapshot.monitor !== true) return;
    apply_tutorial_screenshot_status(slug, snapshot);

    const poll = async () => {
        try {
            const next = await fetch_tutorial_screenshot_status(slug);
            snapshot = next;
            if (next.monitor !== true) return;
            apply_tutorial_screenshot_status(slug, next);
            if (['queued', 'preparing', 'running'].includes(next.status)) {
                setTimeout(poll, 500);
            }
        } catch (error) {
            update_tutorial_screenshot_monitor({
                status: 'error',
                total: snapshot.total || 0,
                completed: snapshot.completed || 0,
                error: `Status nicht erreichbar (${error.message})`,
            });
        }
    };

    if (['queued', 'preparing', 'running'].includes(snapshot.status)) {
        setTimeout(poll, 500);
    }
}

window.addEventListener('DOMContentLoaded', install_tutorial_screenshot_live_preview);
