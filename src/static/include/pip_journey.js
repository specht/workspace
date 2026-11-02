// The Wichtel's map at the top of a Pip page (docs/katas.md, »Pip«): the
// tunnels of the page as one strip, with an old lamp for each chapter. The
// digits 1 to 9 in the map are the lamps, in the order of the chapters (the
// h2 headings after the map). A lamp burns once every task of its chapter
// is solved (an Extra does not count, a prediction counts once it has an
// answer, as for what waits after it, tasks.js); Pip stands below the last
// lamp that burns, or at P. Each lamp is a link to its chapter. Drawn by
// PipView (pip_world.js) again whenever a lamp changes.
//
//   <div class='pip-journey'><pre>
//   ######################
//   #..1....2....3....4..#
//   #P...................#
//   ######################
//   </pre><p>a caption</p></div>
(function () {
    'use strict';

    const SOLVED = ['correct', 'sent'];
    const ANSWERED = ['correct', 'wrong', 'sent', 'stale'];

    // the chapters after the map: [{ heading, done }], one per h2
    function chapters(box) {
        const headings = Array.from(document.querySelectorAll('h2')).filter((h) => box.compareDocumentPosition(h) & Node.DOCUMENT_POSITION_FOLLOWING);
        const cards = Array.from(document.querySelectorAll('.task-card'));
        return headings.map((heading, i) => {
            const next = headings[i + 1];
            const mine = cards.filter((card) => (heading.compareDocumentPosition(card) & Node.DOCUMENT_POSITION_FOLLOWING) &&
                !(next && next.compareDocumentPosition(card) & Node.DOCUMENT_POSITION_FOLLOWING) && !card.querySelector('.task-extra'));
            // one card per language for the same task: any of them counts
            const tasks = {};
            mine.forEach((card) => {
                const ok = (card.dataset.prediction === '1' ? ANSWERED : SOLVED).includes(card.dataset.status);
                tasks[card.dataset.task] = tasks[card.dataset.task] || ok;
            });
            const keys = Object.keys(tasks);
            return { heading, done: keys.length > 0 && keys.every((k) => tasks[k]) };
        });
    }

    // the heading's id, made as the page's contents make it
    // (_template.html, slugify) where it has none yet
    function anchor(heading) {
        if (!heading.id) {
            const base = heading.textContent.toLowerCase().trim().replace(/\s+/g, '-').replace(/[^\p{L}\p{N}-]+/gu, '')
                .replace(/-+/g, '-').replace(/^-|-$/g, '') || 'kapitel';
            let id = base;
            for (let n = 2; document.getElementById(id); n++) id = `${base}-${n}`;
            heading.id = id;
        }
        return heading.id;
    }

    function draw(box, text, lit) {
        const map = window.PipWorld.parse(text.replace(/[1-9]/g, '.'));
        const lamps = [];
        text.split('\n').filter((row) => row.trim() !== '').forEach((row, y) => {
            row.split('').forEach((c, x) => { if (/[1-9]/.test(c)) lamps[Number(c) - 1] = [x, y]; });
        });
        map.layout.burning = lamps.filter((p, i) => p && lit[i]);
        map.layout.unlit = lamps.filter((p, i) => p && !lit[i]);
        // Pip below the last lamp that burns, looking on along the tunnel
        let last = -1;
        lamps.forEach((p, i) => { if (p && lit[i]) last = i; });
        if (last >= 0) map.start.pip = { x: lamps[last][0], y: lamps[last][1] + 1, facing: 'east' };
        // the map above its caption
        const mapBox = box.querySelector('.pip-map-box') || document.createElement('div');
        mapBox.className = 'pip-map-box';
        mapBox.replaceChildren();
        box.prepend(mapBox);
        const view = new window.PipWorld.PipView(mapBox, map.layout, map.start, { width: map.layout.width, height: map.layout.height, columns: map.layout.width + 2, ratio: Infinity });
        return { view, lamps };
    }

    function links(box, view, lamps, list) {
        const layer = document.createElement('div');
        layer.className = 'pip-journey-links';
        const [ox, oy] = view.offset;
        lamps.forEach((p, i) => {
            const chapter = list[i];
            if (!p || !chapter) return;
            const a = document.createElement('a');
            a.className = 'pip-journey-lamp';
            a.href = '#' + anchor(chapter.heading);
            const name = chapter.heading.textContent.trim().replace(/^\d+\.\s*/, '');
            a.title = name;
            a.setAttribute('aria-label', `Kapitel ${i + 1}, »${name}«: ${chapter.done ? 'die Lampe brennt' : 'die Lampe ist noch dunkel'}`);
            a.style.setProperty('--x', p[0] + ox);
            a.style.setProperty('--y', p[1] + oy);
            layer.append(a);
        });
        view.world.append(layer);
    }

    function mount(box) {
        const source = box.querySelector('pre');
        if (!source || !window.PipWorld) return;
        const text = source.textContent;
        source.hidden = true;
        box.setAttribute('role', 'group');
        let shown = null;
        const update = () => {
            const list = chapters(box);
            const lit = list.map((c) => c.done);
            const key = lit.join();
            if (key === shown) return;
            shown = key;
            const { view, lamps } = draw(box, text, lit);
            const burning = lamps.filter((p, i) => p && lit[i]).length;
            const count = lamps.filter(Boolean).length;
            view.world.setAttribute('aria-label', `Die Karte des Wichtels: ${burning} von ${count} Lampen brennen.`);
            links(box, view, lamps, list);
        };
        update();
        // a task solved on the page: tasks.js sets data-status on its card
        let timer = null;
        new MutationObserver(() => {
            clearTimeout(timer);
            timer = setTimeout(update, 300);
        }).observe(document.body, { subtree: true, attributes: true, attributeFilter: ['data-status'] });
    }

    function start() {
        document.querySelectorAll('.pip-journey').forEach(mount);
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
