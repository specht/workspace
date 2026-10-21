// ← and → step through a step-by-step widget: the Huffman tree, the LZW
// tables, the GIF decoder, the JPEG decoder. A widget built after the page
// has loaded sends document the event stepper-added (detail: the widget),
// so that it gets the lightbox, play, the timeline and its reserved heights. A widget is an element with
// data-stepper; its buttons say what they do with data-go (first, prev,
// next, last), and the keys press them, so a widget does nothing extra.
//
// With several widgets on a page, the keys step the one that takes up most
// of the window. They leave alone a text field (the cursor moves there), a
// key with Ctrl, Alt or Cmd, and an open dialog such as a picture opened
// large, which steps with the arrow keys itself.
document.addEventListener('keydown', (event) => {
    if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return;
    if (event.ctrlKey || event.altKey || event.metaKey || event.defaultPrevented) return;
    const target = event.target;
    if (target instanceof HTMLElement &&
        (target.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(target.tagName))) return;
    if (document.querySelector('dialog[open]')) return;

    let best = null;
    let bestArea = 0;
    for (const widget of document.querySelectorAll('[data-stepper]')) {
        const rect = widget.getBoundingClientRect();
        const width = Math.min(rect.right, window.innerWidth) - Math.max(rect.left, 0);
        const height = Math.min(rect.bottom, window.innerHeight) - Math.max(rect.top, 0);
        const area = Math.max(0, width) * Math.max(0, height);
        if (area > bestArea) {
            best = widget;
            bestArea = area;
        }
    }
    if (!best) return;

    const button = best.querySelector(`[data-go='${event.key === 'ArrowLeft' ? 'prev' : 'next'}']`);
    if (!button || button.disabled) return;
    event.preventDefault();
    button.click();
});

// On a touch screen a swipe to the left or right across a widget steps it,
// like turning a page: next to the left, back to the right. Not in a text
// field or on an element with data-no-swipe (a timeline to drag), and not when the swipe is more up and down than sideways (that
// scrolls the page).
(() => {
    let start = null;
    document.addEventListener('pointerdown', (event) => {
        if (event.pointerType !== 'touch') return;
        const widget = event.target instanceof Element && event.target.closest('[data-stepper]');
        if (!widget || event.target.closest('input, textarea, select, [contenteditable], [data-no-swipe]')) return;
        start = { widget, x: event.clientX, y: event.clientY, time: event.timeStamp };
    });
    document.addEventListener('pointerup', (event) => {
        if (!start || event.pointerType !== 'touch') return;
        const { widget, x, y, time } = start;
        start = null;
        const dx = event.clientX - x;
        const dy = event.clientY - y;
        if (Math.abs(dx) < 60 || Math.abs(dy) > Math.abs(dx) * 0.6 || event.timeStamp - time > 700) return;
        const button = widget.querySelector(`[data-go='${dx < 0 ? 'next' : 'prev'}']`);
        if (button && !button.disabled) button.click();
    });
    document.addEventListener('pointercancel', () => { start = null; });
})();

// Every widget gets a button at its top right that opens it in a lightbox
// over the page, larger, for a demo on the big screen. In the lightbox the
// keys and the sideways swipe step the widget as before; the arrows at the
// edges, Page Up and Page Down, or a swipe up and down go to the previous or
// next widget of the page. Esc, the button again or the browser's back
// button close it.
(() => {
    let open = null;
    let nav = null;
    // while open, the widget is moved under body (out of any stacking
    // context of the page, which would keep the page's header on top), and
    // a placeholder keeps its place
    let placeholder = null;
    const lift = (widget) => {
        placeholder = document.createElement('div');
        placeholder.style.height = `${widget.offsetHeight}px`;
        widget.before(placeholder);
        document.body.appendChild(widget);
    };
    const putBack = (widget) => {
        if (placeholder) placeholder.replaceWith(widget);
        placeholder = null;
    };
    const widgets = () => [...document.querySelectorAll('[data-stepper]')].filter((w) => w !== open).concat(open ? [open] : [])
        .sort((a, b) => ((a === open ? placeholder : a).compareDocumentPosition(b === open ? placeholder : b) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1));
    const setLabel = (button, full) => {
        button.replaceChildren(Object.assign(document.createElement('i'), { className: `bi ${full ? 'bi-fullscreen-exit' : 'bi-fullscreen'}` }));
        button.setAttribute('aria-label', full ? 'Lightbox schließen' : 'In der Lightbox zeigen');
        button.title = button.getAttribute('aria-label');
    };
    const navButton = (icon, label, go) => {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `btn btn-outline-secondary stepper-nav stepper-nav-${go < 0 ? 'prev' : 'next'}`;
        button.setAttribute('aria-label', label);
        button.title = label;
        button.appendChild(Object.assign(document.createElement('i'), { className: `bi ${icon}` }));
        button.addEventListener('click', () => move(go));
        return button;
    };
    // As large as possible and everything visible: the largest font size at
    // which the widget fits the lightbox without scrolling (a binary search).
    // The picture of an Algo Stepper gives up height for it only while its
    // labels (12 units) stay at least 70 % of the font size: a
    // picture that is small because it is wide stays as large as its width
    // allows. Beside the code it keeps at least 60 % of the size it would
    // have across the whole width (at most 60 % of the height). Both layouts
    // are tried, the picture beside the code and above it; the one with the
    // larger text wins.
    // Again when the window changes or the widget redraws itself (a step).
    let fitting = 0;
    const LABEL = 12;
    const fits = (size, picture) => {
        open.style.setProperty('--lightbox-shift', '0px');
        open.style.fontSize = `${size}px`;
        if (!picture) return open.scrollHeight <= open.clientHeight + 0.5 && open.scrollWidth <= open.clientWidth + 0.5;
        picture.style.maxHeight = 'none';
        const view = picture.viewBox.baseVal;
        const scaleOf = () => { const box = picture.getBoundingClientRect(); return view && view.width ? Math.min(box.width / view.width, box.height / view.height) : 1; };
        const body = picture.parentElement.getBoundingClientRect();
        const full = view && view.width ? Math.min(body.width / view.width, 0.6 * open.clientHeight / view.height) : 1;
        const uncapped = scaleOf();
        let over = open.scrollHeight - open.clientHeight;
        if (over > 0.5) {
            picture.style.maxHeight = `${Math.max(40, picture.getBoundingClientRect().height - over)}px`;
            over = open.scrollHeight - open.clientHeight;
        }
        if (over > 0.5 || open.scrollWidth > open.clientWidth + 0.5) return false;
        const scale = scaleOf();
        return scale >= Math.min(uncapped, 0.7 * size / LABEL) - 0.001 && scale >= 0.6 * full - 0.001;
    };
    const largest = (picture, hi = 32) => {
        let lo = 12;
        for (let i = 0; i < 9; i++) {
            const mid = (lo + hi) / 2;
            if (fits(mid, picture)) lo = mid;
            else hi = mid;
        }
        // a little below the limit, so that rounding leaves no scrolling
        return Math.floor(lo * 0.98 * 10) / 10;
    };
    // the layouts: the code beside the picture, 24em or 32em wide (fewer
    // lines wrap), or under it
    const LAYOUTS = [['24em', false], ['32em', false], ['24em', true]];
    const use = ([side, stacked]) => {
        open.style.setProperty('--stepper-side', side);
        open.classList.toggle('stepper-stacked', stacked);
    };
    // fitted: the size and layout chosen when the widget opened (or the
    // window changed); a step keeps them as long as they fit, so that the
    // text does not change its size from step to step, and only shrinks
    let fitted = null;
    const fit = (fresh = true) => {
        if (!open) return;
        const picture = open.querySelector('.algo-body > svg.algo-picture');
        // a step that still fits keeps size and layout; one that does not
        // gets the best layout again, at most as large as before
        const keep = !fresh && fitted && fitted.widget === open;
        // In the middle of the window, for the classroom: the content moved
        // down by half the room its tallest step leaves (measured with the
        // size, at the same steps); never so far that a step scrolls
        const shift = () => {
            open.style.setProperty('--lightbox-shift', `${fitted.shift}px`);
            const over = open.scrollHeight - open.clientHeight;
            if (over > 0) open.style.setProperty('--lightbox-shift', `${Math.max(0, fitted.shift - over)}px`);
        };
        if (keep && fits(fitted.size, picture)) { shift(); return; }
        const hi = keep ? fitted.size : 32;
        // when the lightbox opens: the size that fits the first, the last
        // and the current step, so that a widget that grows (the LZW
        // table) keeps one size from the start (stepperGo, data-step)
        const probe = (measure) => {
            const steps = Number(open.dataset.steps);
            if (keep || typeof open.stepperGo !== 'function' || !(steps > 1)) return measure();
            const at = Number(open.dataset.step) || 0;
            let least = Infinity;
            for (const s of new Set([at, 0, steps - 1])) { open.stepperGo(s); least = Math.min(least, measure()); }
            open.stepperGo(at);
            return least;
        };
        // how tall the content is: down to the end of its last part (the
        // scroll height is never less than the window)
        const contentHeight = () => {
            const box = open.getBoundingClientRect();
            let bottom = box.top;
            for (const child of open.children) {
                const position = getComputedStyle(child).position;
                if (position !== 'absolute' && position !== 'fixed' && child.offsetParent !== null) bottom = Math.max(bottom, child.getBoundingClientRect().bottom);
            }
            return bottom - box.top + parseFloat(getComputedStyle(open).paddingBottom);
        };
        const center = () => {
            let tallest = 0;
            probe(() => { fits(fitted.size, picture); tallest = Math.max(tallest, contentHeight()); return 0; });
            fits(fitted.size, picture);
            fitted.shift = Math.max(0, (open.clientHeight - tallest) / 2);
            shift();
        };
        if (!picture) {
            fitted = { widget: open, size: probe(() => largest(null, hi)) };
            center();
            return;
        }
        let best = null;
        for (const layout of LAYOUTS) {
            use(layout);
            const size = probe(() => largest(picture, hi));
            if (!best || size > best.size + 0.2) best = { layout, size };
        }
        use(best.layout);
        fitted = { widget: open, size: best.size };
        center();
    };
    const unfit = (widget) => {
        widget.classList.remove('stepper-stacked');
        widget.style.removeProperty('--lightbox-shift');
        widget.style.removeProperty('--stepper-side');
        const picture = widget.querySelector('.algo-body > svg.algo-picture');
        if (picture) picture.style.maxHeight = '';
    };
    // a step keeps the size if it can; a new window size fits afresh
    let fresh = false;
    // measured only when the picture rests: halfway through a movement its
    // view is between two steps
    const fitSoon = (again) => {
        fresh = fresh || again;
        cancelAnimationFrame(fitting);
        clearTimeout(fitting);
        const moving = open && open.querySelector('svg.algo-picture[data-moving]');
        if (moving) { fitting = setTimeout(() => fitSoon(false), 80); return; }
        fitting = requestAnimationFrame(() => { fit(fresh); fresh = false; });
    };
    // the lines from the calls to the nodes are redrawn while things move;
    // they never change the size
    const watch = new MutationObserver((records) => {
        if (records.some((r) => !(r.target.closest && r.target.closest('svg.algo-links')))) fitSoon(false);
    });
    window.addEventListener('resize', () => fitSoon(true));
    const show = (widget) => {
        if (open) { open.classList.remove('stepper-lightbox'); open.style.fontSize = ''; unfit(open); setLabel(open.querySelector(':scope > .stepper-full'), false); putBack(open); }
        lift(widget);
        open = widget;
        widget.classList.add('stepper-lightbox');
        document.documentElement.classList.add('stepper-lightbox-open');
        setLabel(widget.querySelector(':scope > .stepper-full'), true);
        if (!nav) {
            nav = document.createElement('div');
            nav.className = 'stepper-lightbox-nav';
            nav.append(navButton('bi-chevron-up', 'Voriges Widget', -1), navButton('bi-chevron-down', 'Nächstes Widget', 1));
            document.body.appendChild(nav);
        }
        const all = widgets();
        nav.querySelector('.stepper-nav-prev').disabled = all.indexOf(widget) <= 0;
        nav.querySelector('.stepper-nav-next').disabled = all.indexOf(widget) >= all.length - 1;
        nav.hidden = all.length < 2;
        widget.scrollTop = 0;
        watch.disconnect();
        watch.observe(widget, { childList: true, subtree: true });
        window.dispatchEvent(new Event('resize'));
        fit();
    };
    const close = (fromHistory) => {
        if (!open) return;
        open.classList.remove('stepper-lightbox');
        open.style.fontSize = '';
        unfit(open);
        watch.disconnect();
        setLabel(open.querySelector(':scope > .stepper-full'), false);
        putBack(open);
        open.scrollIntoView({ block: 'center' });
        open = null;
        document.documentElement.classList.remove('stepper-lightbox-open');
        if (nav) nav.hidden = true;
        if (!fromHistory && history.state && history.state.stepperLightbox) history.back();
        window.dispatchEvent(new Event('resize'));
    };
    const move = (go) => {
        const all = widgets();
        const next = all[all.indexOf(open) + go];
        if (next) show(next);
    };
    const add = () => {
        for (const widget of widgets()) {
            if (widget.querySelector(':scope > .stepper-full')) continue;
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'btn btn-sm btn-outline-secondary stepper-full';
            setLabel(button, false);
            button.addEventListener('click', () => {
                if (open === widget) close(false);
                else {
                    // the browser's back button closes the lightbox
                    if (!open) history.pushState({ stepperLightbox: true }, '');
                    show(widget);
                }
            });
            widget.appendChild(button);
        }
    };
    window.addEventListener('popstate', () => close(true));
    document.addEventListener('keydown', (event) => {
        if (!open) return;
        if (event.key === 'Escape') { event.preventDefault(); close(false); }
        else if (event.key === 'PageDown') { event.preventDefault(); move(1); }
        else if (event.key === 'PageUp') { event.preventDefault(); move(-1); }
    });
    // a swipe up or down in the lightbox: the next or the previous widget
    let start = null;
    document.addEventListener('pointerdown', (event) => {
        if (!open || event.pointerType !== 'touch' || !open.contains(event.target) || event.target.closest('[data-no-swipe]')) return;
        start = { x: event.clientX, y: event.clientY, time: event.timeStamp };
    });
    document.addEventListener('pointerup', (event) => {
        if (!start || !open) return;
        const dx = event.clientX - start.x;
        const dy = event.clientY - start.y;
        const quick = event.timeStamp - start.time < 700;
        start = null;
        if (quick && Math.abs(dy) > 80 && Math.abs(dx) < Math.abs(dy) * 0.5 && open.scrollHeight <= open.clientHeight + 2) move(dy < 0 ? 1 : -1);
    });
    // after the widgets have built themselves; a widget built later (in
    // Automatron) says so with the event stepper-added
    if (document.readyState === 'complete') add();
    else window.addEventListener('load', add);
    document.addEventListener('stepper-added', add);
})();

// Play for every widget whose step buttons have none of their own (the Algo
// Stepper has one that also speeds up its movements): next, again and
// again, with a pause to read, until next is disabled. The speed button
// cycles 1×, 2× and ½×; a step by hand (a button, a key, a swipe) stops it,
// and play at the end starts again from the beginning.
(() => {
    const SPEEDS = [[1, '1×'], [2, '2×'], [0.5, '½×']];
    const STEP_TIME = 2200;
    const add = (widget) => {
        const next = widget.querySelector("[data-go='next']");
        if (!next) return;
        // back and next say so beside their arrows (on a desktop; the
        // stylesheet hides the words where room is short)
        const prev = widget.querySelector("[data-go='prev']");
        const word = (text) => Object.assign(document.createElement('span'), { className: 'stepper-word', textContent: text });
        if (prev && !prev.querySelector('.stepper-word')) prev.append(word('Zurück'));
        if (!next.querySelector('.stepper-word')) next.prepend(word('Weiter'));
        if (widget.querySelector('[data-play]')) return;
        const first = widget.querySelector("[data-go='first']");
        const row = next.parentElement;
        const last = [...row.querySelectorAll(':scope > [data-go]')].pop();
        const play = document.createElement('button');
        play.type = 'button';
        play.className = 'btn btn-sm btn-outline-success';
        play.dataset.play = '';
        play.setAttribute('aria-label', 'Abspielen');
        play.appendChild(Object.assign(document.createElement('i'), { className: 'bi bi-play-fill' }));
        const speedButton = document.createElement('button');
        speedButton.type = 'button';
        speedButton.className = 'btn btn-sm btn-outline-success';
        speedButton.dataset.speed = '';
        speedButton.setAttribute('aria-label', 'Tempo beim Abspielen');
        speedButton.textContent = SPEEDS[0][1];
        // spaces between them, like between the step buttons in a row of
        // inline buttons (a flex row ignores them)
        last.after(' ', play, ' ', speedButton);
        let speed = 0;
        let timer = null;
        let auto = false;
        const press = (button) => { auto = true; button.click(); auto = false; };
        const stop = () => {
            clearTimeout(timer);
            timer = null;
            play.firstChild.className = 'bi bi-play-fill';
            play.setAttribute('aria-label', 'Abspielen');
        };
        const tick = () => {
            if (next.disabled) { stop(); return; }
            press(next);
            timer = setTimeout(tick, STEP_TIME / SPEEDS[speed][0]);
        };
        play.addEventListener('click', () => {
            if (timer) { stop(); return; }
            play.firstChild.className = 'bi bi-pause-fill';
            play.setAttribute('aria-label', 'Anhalten');
            if (next.disabled && first) { press(first); timer = setTimeout(tick, STEP_TIME / SPEEDS[speed][0]); } else tick();
        });
        speedButton.addEventListener('click', () => {
            speed = (speed + 1) % SPEEDS.length;
            speedButton.textContent = SPEEDS[speed][1];
        });
        widget.addEventListener('click', (event) => { if (!auto && event.target.closest('[data-go]')) stop(); }, true);
        // a step chosen on the timeline stops it too
        widget.addEventListener('pointerdown', (event) => { if (event.target.closest('.stepper-timeline')) stop(); }, true);
    };
    const all = () => document.querySelectorAll('[data-stepper]').forEach(add);
    if (document.readyState === 'complete') all();
    else window.addEventListener('load', all);
    document.addEventListener('stepper-added', (event) => add(event.detail));
})();

// A timeline for widgets that have none of their own (the Algo Stepper
// has): a dot for every step, joined by a fine line, the steps done
// coloured, the current one marked; a tap or a drag on it goes to that
// step. A widget takes part by keeping data-step (from 0) and data-steps
// up to date and offering widget.stepperGo(step).
(() => {
    const add = (widget) => {
        if (typeof widget.stepperGo !== 'function' || widget.querySelector('.algo-timeline, .stepper-timeline')) return;
        const next = widget.querySelector("[data-go='next']");
        if (!next) return;
        const box = document.createElement('div');
        box.className = 'stepper-timeline';
        box.dataset.noSwipe = '';
        box.setAttribute('role', 'slider');
        box.setAttribute('aria-label', 'Die Schritte');
        next.parentElement.after(box);
        const render = () => {
            const steps = Number(widget.dataset.steps) || 0;
            const at = Number(widget.dataset.step) || 0;
            box.hidden = steps < 2;
            if (box.children.length !== steps) box.replaceChildren(...Array.from({ length: steps }, () => document.createElement('span')));
            [...box.children].forEach((tick, i) => { tick.className = i < at ? 'done' : i === at ? 'current' : ''; });
            box.setAttribute('aria-valuemax', Math.max(0, steps - 1));
            box.setAttribute('aria-valuenow', at);
        };
        new MutationObserver(render).observe(widget, { attributes: true, attributeFilter: ['data-step', 'data-steps'] });
        render();
        let dragging = false;
        const stepAt = (event) => {
            const steps = Number(widget.dataset.steps) || 0;
            const rect = box.getBoundingClientRect();
            const to = Math.max(0, Math.min(steps - 1, Math.floor((event.clientX - rect.left) / rect.width * steps)));
            if (to !== Number(widget.dataset.step)) widget.stepperGo(to);
        };
        box.addEventListener('pointerdown', (event) => { dragging = true; box.setPointerCapture(event.pointerId); stepAt(event); });
        box.addEventListener('pointermove', (event) => { if (dragging) stepAt(event); });
        box.addEventListener('pointerup', () => { dragging = false; });
        box.addEventListener('pointercancel', () => { dragging = false; });
    };
    const all = () => document.querySelectorAll('[data-stepper]').forEach(add);
    if (document.readyState === 'complete') all();
    else window.addEventListener('load', all);
    document.addEventListener('stepper-added', (event) => add(event.detail));
})();

// Nothing moves while one steps or slides through a widget: every part of
// it keeps the height of its tallest step (min-height, in em, so that it
// follows the text), measured by going through all steps once when the
// page has loaded, again when the width changes or a text in the widget is
// edited. Not in the lightbox, whose fit measures for itself.
(() => {
    const parts = (widget) => [...widget.children].filter((c) => !['absolute', 'fixed'].includes(getComputedStyle(c).position));
    const clear = (widget) => { for (const part of parts(widget)) part.style.minHeight = ''; };
    const reserve = (widget) => {
        if (typeof widget.stepperGo !== 'function' || widget.classList.contains('stepper-lightbox')) return;
        const steps = Number(widget.dataset.steps);
        if (!(steps > 1)) return;
        const at = Number(widget.dataset.step) || 0;
        const list = parts(widget);
        clear(widget);
        const least = list.map(() => Infinity);
        const most = list.map(() => 0);
        for (let s = 0; s < steps; s++) {
            widget.stepperGo(s);
            list.forEach((part, i) => {
                const h = part.getBoundingClientRect().height;
                least[i] = Math.min(least[i], h);
                most[i] = Math.max(most[i], h);
            });
        }
        widget.stepperGo(at);
        list.forEach((part, i) => {
            if (most[i] - least[i] > 1) part.style.minHeight = `${most[i] / (parseFloat(getComputedStyle(part).fontSize) || 16)}em`;
        });
    };
    const all = () => document.querySelectorAll('[data-stepper]').forEach(reserve);
    let waiting = 0;
    const soon = () => { clearTimeout(waiting); waiting = setTimeout(all, 150); };
    let width = window.innerWidth;
    window.addEventListener('resize', () => {
        // the lightbox opens or closes: at once, before its fit measures
        document.querySelectorAll('[data-stepper].stepper-lightbox').forEach(clear);
        if (window.innerWidth !== width || !document.querySelector('.stepper-lightbox')) { width = window.innerWidth; soon(); }
    });
    document.addEventListener('input', (event) => { const widget = event.target.closest && event.target.closest('[data-stepper]'); if (widget) { clearTimeout(waiting); waiting = setTimeout(() => reserve(widget), 400); } });
    const start = () => (window.requestIdleCallback ? requestIdleCallback(all, { timeout: 1500 }) : setTimeout(all, 300));
    if (document.readyState === 'complete') start();
    else window.addEventListener('load', start);
    document.addEventListener('stepper-added', (event) => setTimeout(() => reserve(event.detail), 50));
})();
