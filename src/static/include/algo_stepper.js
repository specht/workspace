// Step-by-step widgets for the Handbuch pages on algorithms and data
// structures (reference/graphen, reference/heap). A widget is an element
//
//   <div class='algo-stepper' data-algo='dijkstra' data-graph='stadt' data-start='A' data-goal='H'></div>
//   <div class='algo-stepper' data-algo='heap' data-ops='push 5, push 3, push 8, pop'></div>
//
// algo: bfs, dfs, dijkstra, astar (on a graph from GRAPHS), heap. The widget
// computes every step at once and shows one at a time; its buttons carry
// data-go like the other steppers, so stepper_keys.js steps it with ← and →.
(function () {
    'use strict';

    // Example graphs: positions in a 100 × 60 field, edges with lengths that
    // are never shorter than the straight line (so A* finds the shortest way).
    // »stadt«: the way from A to H with the fewest edges (over C and G) is
    // longer than the shortest, over B, E and F.
    const GRAPHS = {
        stadt: {
            nodes: { A: [6, 30], B: [26, 10], C: [12, 56], D: [50, 30], E: [50, 6], F: [74, 16], G: [74, 50], H: [94, 30] },
            edges: [['A', 'B', 29], ['A', 'C', 29], ['A', 'D', 80], ['B', 'D', 33], ['B', 'E', 26], ['C', 'D', 47],
                ['C', 'G', 70], ['D', 'F', 30], ['D', 'G', 33], ['E', 'F', 27], ['F', 'H', 25], ['G', 'H', 29]],
        },
    };

    const STYLE = `
.algo-stepper { margin: 1em 0 1.5em; --algo-accent: #6741d9; --algo-frontier: #ffe066; --algo-done: #b197fc;
    --algo-line: #adb5bd; --algo-ink: var(--bs-body-color); --algo-paper: var(--bs-body-bg); --algo-start: #2f9e44; --algo-goal: #e03131; }
html[data-bs-theme="dark"] .algo-stepper { --algo-accent: #b197fc; --algo-frontier: #c9a227; --algo-done: #5f3dc4; --algo-line: #5c636e; }
.algo-stepper .algo-controls { display: flex; gap: 0.3em; align-items: center; flex-wrap: wrap; margin-bottom: 0.6em; }
.algo-stepper .algo-count { margin-left: 0.6em; color: var(--bs-secondary-color); }
.algo-stepper .algo-body { display: flex; gap: 1.2em; flex-wrap: wrap; align-items: flex-start; }
.algo-stepper svg.algo-picture { flex: 1 1 22em; max-width: 34em; min-width: 0; height: auto; }
.algo-stepper .algo-side { flex: 1 1 14em; min-width: 12em; font-size: 0.92em; }
.algo-stepper .algo-side h5 { font-size: 0.8em; text-transform: uppercase; letter-spacing: 0.04em; color: var(--bs-secondary-color); margin: 0 0 0.3em; }
.algo-stepper .algo-items { display: flex; flex-wrap: wrap; gap: 0.25em; min-height: 1.9em; margin-bottom: 0.8em; }
.algo-stepper .algo-item { font-family: '0xProto Nerd Font Mono', monospace; padding: 0.1em 0.45em; border-radius: 0.3em;
    background: color-mix(in srgb, var(--algo-frontier) 55%, transparent); }
.algo-stepper .algo-item.next { outline: 2px solid var(--algo-accent); }
.algo-stepper .algo-item.stale { opacity: 0.45; text-decoration: line-through; }
.algo-stepper .algo-empty { color: var(--bs-secondary-color); font-style: italic; }
.algo-stepper table.algo-table { font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.95em; border-collapse: collapse; }
.algo-stepper table.algo-table th, .algo-stepper table.algo-table td { padding: 0.1em 0.6em; text-align: center; border-bottom: 1px solid color-mix(in srgb, var(--algo-line) 50%, transparent); }
.algo-stepper table.algo-table th { font-family: inherit; font-weight: 600; }
.algo-stepper table.algo-table tr.done td { color: var(--bs-secondary-color); }
.algo-stepper table.algo-table tr.current td { background: color-mix(in srgb, var(--algo-accent) 18%, transparent); font-weight: 600; }
.algo-stepper table.algo-table td.changed { background: color-mix(in srgb, var(--algo-frontier) 50%, transparent); }
.algo-stepper .algo-message { margin-top: 0.6em; min-height: 3em; }
.algo-stepper svg text { font-family: system-ui, sans-serif; }
`;

    function addStyle() {
        if (document.getElementById('algo-stepper-style')) return;
        const style = document.createElement('style');
        style.id = 'algo-stepper-style';
        style.textContent = STYLE;
        document.head.appendChild(style);
    }

    const el = (tag, attrs = {}, children = []) => {
        const node = tag.startsWith('svg:')
            ? document.createElementNS('http://www.w3.org/2000/svg', tag.slice(4))
            : document.createElement(tag);
        for (const [key, value] of Object.entries(attrs)) {
            if (key === 'text') node.textContent = value;
            else if (key === 'html') node.innerHTML = value;
            else node.setAttribute(key, value);
        }
        for (const child of children) node.appendChild(child);
        return node;
    };

    function neighbours(graph) {
        const list = {};
        for (const id of Object.keys(graph.nodes)) list[id] = [];
        for (const [a, b, w] of graph.edges) {
            list[a].push([b, w]);
            list[b].push([a, w]);
        }
        for (const id of Object.keys(list)) list[id].sort((x, y) => (x[0] < y[0] ? -1 : 1));
        return list;
    }

    const straight = (graph, a, b) => {
        const [x1, y1] = graph.nodes[a];
        const [x2, y2] = graph.nodes[b];
        return Math.round(Math.hypot(x2 - x1, y2 - y1));
    };

    const pathTo = (previous, goal) => {
        if (!(goal in previous)) return null;
        const path = [];
        for (let node = goal; node !== null; node = previous[node]) path.unshift(node);
        return path;
    };

    const names = (list) => (list.length === 1 ? list[0] : `${list.slice(0, -1).join(', ')} und ${list[list.length - 1]}`);

    // Breitensuche (queue) and Tiefensuche (stack): a node is marked when it
    // is found, so it goes into the queue or stack only once.
    function traceSearch(graph, start, goal, kind) {
        const adj = neighbours(graph);
        const previous = { [start]: null };
        const pending = [start];
        const order = [];
        const frames = [];
        const box = kind === 'bfs' ? 'Queue' : 'Stack';
        const inBox = kind === 'bfs' ? 'in der Queue' : 'auf dem Stack';
        const outOf = kind === 'bfs' ? 'aus der Queue' : 'vom Stack';
        const empty = kind === 'bfs' ? 'Die Queue ist leer' : 'Der Stack ist leer';
        const snapshot = (current, message, extra = {}) => frames.push({
            current, message, previous: { ...previous }, order: [...order],
            items: pending.map((id) => ({ label: id })), ...extra,
        });
        snapshot(null, `Am Anfang liegt nur der Start ${start} ${inBox}. Sein Vorgänger ist keiner, damit gilt er als gefunden.`);
        while (pending.length) {
            const node = kind === 'bfs' ? pending.shift() : pending.pop();
            order.push(node);
            if (node === goal) {
                snapshot(node, `${node} kommt ${outOf}: das Ziel. Der Weg steht in den Vorgängern, rückwärts gelesen: ${pathTo(previous, goal).join('–')}.`, { path: pathTo(previous, goal) });
                return frames;
            }
            const found = [];
            const known = [];
            for (const [next] of adj[node]) {
                if (next in previous) {
                    known.push(next);
                    continue;
                }
                previous[next] = node;
                pending.push(next);
                found.push(next);
            }
            const where = kind === 'bfs' ? 'hinten in die Queue' : 'oben auf den Stack';
            snapshot(node, `${node} kommt ${outOf}.` +
                (found.length ? ` Neu gefunden: ${names(found)}, mit ${node} als Vorgänger, ${where}.` : ' Kein neuer Nachbar.') +
                (known.length ? ` ${names(known)} ${known.length === 1 ? 'ist' : 'sind'} schon gefunden.` : ''));
        }
        snapshot(null, `${empty}, ohne dass ${goal} dran war: Es gibt keinen Weg.`);
        return frames;
    }

    // Dijkstra and A*: a priority queue of (priority, node); a node can be in
    // it more than once, an old entry is skipped when it comes out.
    function traceShortest(graph, start, goal, kind) {
        const adj = neighbours(graph);
        const h = (id) => (kind === 'astar' ? straight(graph, id, goal) : 0);
        const cost = { [start]: 0 };
        const previous = { [start]: null };
        const done = new Set();
        let queue = [{ priority: h(start), node: start }];
        const order = [];
        const frames = [];
        const snapshot = (current, message, changed = [], extra = {}) => {
            const sorted = [...queue].sort((a, b) => a.priority - b.priority || (a.node < b.node ? -1 : 1));
            frames.push({
                current, message, changed, previous: { ...previous }, order: [...order],
                cost: { ...cost }, done: new Set(done),
                items: sorted.map((entry, i) => ({ label: `${entry.priority} ${entry.node}`, next: i === 0, stale: done.has(entry.node) || cost[entry.node] + h(entry.node) < entry.priority })),
                ...extra,
            });
        };
        const what = kind === 'astar' ? 'Weg bis hier plus Luftlinie zum Ziel' : 'Weg bis hier';
        snapshot(null, `Am Anfang: der Start ${start} mit Abstand 0 in der Priority Queue. Die Priorität ist der ${what}.`);
        while (queue.length) {
            queue.sort((a, b) => a.priority - b.priority || (a.node < b.node ? -1 : 1));
            const { priority, node } = queue.shift();
            if (done.has(node)) {
                snapshot(node, `${node} mit ${priority} kommt heraus, aber ${node} ist schon fertig: ein alter Eintrag, die Suche überspringt ihn.`);
                continue;
            }
            done.add(node);
            order.push(node);
            if (node === goal) {
                snapshot(node, `${node} kommt heraus: das Ziel, ${cost[node]} vom Start, über ${pathTo(previous, goal).join('–')}. Kein anderer Weg kann kürzer sein, denn alles, was noch in der Priority Queue steht, ist mindestens so weit.`, [], { path: pathTo(previous, goal) });
                return frames;
            }
            const changed = [];
            const notes = [];
            for (const [next, w] of adj[node]) {
                if (done.has(next)) continue;
                const total = cost[node] + w;
                if (next in cost && cost[next] <= total) {
                    notes.push(`über ${node} wäre ${next} ${total}, schon bekannt ist ${cost[next]}`);
                    continue;
                }
                const before = cost[next];
                cost[next] = total;
                previous[next] = node;
                queue.push({ priority: total + h(next), node: next });
                changed.push(next);
                notes.push(before === undefined ? `${next} neu mit ${total}` : `${next} besser: ${total} statt ${before}`);
            }
            snapshot(node, `${node} hat den kleinsten Wert und ist fertig, ${cost[node]} vom Start.` +
                (notes.length ? ` Nachbarn: ${notes.join('; ')}.` : ' Alle Nachbarn sind schon fertig.'), changed);
        }
        snapshot(null, `Die Priority Queue ist leer: ${goal} ist nicht zu erreichen.`);
        return frames;
    }

    function drawGraph(svg, graph, frame, start, goal, kind) {
        svg.replaceChildren();
        const sx = (x) => x * 5 + 20;
        const sy = (y) => y * 5 + 20;
        const pathEdges = new Set();
        if (frame.path) for (let i = 1; i < frame.path.length; i++) pathEdges.add([frame.path[i - 1], frame.path[i]].sort().join());
        const treeEdges = new Set(Object.entries(frame.previous).filter(([, p]) => p).map(([n, p]) => [n, p].sort().join()));
        for (const [a, b, w] of graph.edges) {
            const key = [a, b].sort().join();
            const [x1, y1] = graph.nodes[a];
            const [x2, y2] = graph.nodes[b];
            const onPath = pathEdges.has(key);
            const inTree = treeEdges.has(key);
            svg.appendChild(el('svg:line', {
                x1: sx(x1), y1: sy(y1), x2: sx(x2), y2: sy(y2),
                stroke: onPath ? 'var(--algo-accent)' : inTree ? 'color-mix(in srgb, var(--algo-accent) 55%, transparent)' : 'var(--algo-line)',
                'stroke-width': onPath ? 6 : inTree ? 3.5 : 2, 'stroke-linecap': 'round',
            }));
            if (kind !== 'bfs' && kind !== 'dfs') {
                const mx = sx((x1 + x2) / 2);
                const my = sy((y1 + y2) / 2);
                svg.appendChild(el('svg:rect', { x: mx - 11, y: my - 9, width: 22, height: 17, rx: 4, fill: 'var(--algo-paper)', opacity: 0.9 }));
                svg.appendChild(el('svg:text', { x: mx, y: my + 4, 'text-anchor': 'middle', 'font-size': 12, fill: 'var(--algo-ink)', text: String(w) }));
            }
        }
        const pending = new Set((frame.items || []).filter((item) => !item.stale).map((item) => item.label.split(' ').pop()));
        const finished = frame.done || new Set(frame.order);
        for (const [id, [x, y]] of Object.entries(graph.nodes)) {
            const isCurrent = frame.current === id;
            const fill = finished.has(id) ? 'var(--algo-done)' : pending.has(id) ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            if (isCurrent) svg.appendChild(el('svg:circle', { cx: sx(x), cy: sy(y), r: 21, fill: 'none', stroke: 'var(--algo-ink)', 'stroke-width': 3 }));
            const ring = id === start ? 'var(--algo-start)' : id === goal ? 'var(--algo-goal)' : 'var(--algo-ink)';
            svg.appendChild(el('svg:circle', { cx: sx(x), cy: sy(y), r: 15, fill, stroke: ring, 'stroke-width': id === start || id === goal ? 3.5 : 1.5 }));
            svg.appendChild(el('svg:text', { x: sx(x), y: sy(y) + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: id }));
            if (frame.cost && id in frame.cost) {
                svg.appendChild(el('svg:text', { x: sx(x), y: sy(y) - 22, 'text-anchor': 'middle', 'font-size': 12, fill: 'var(--algo-ink)', text: String(frame.cost[id]) }));
            }
        }
    }

    function sideGraph(side, graph, frame, kind, goal) {
        side.replaceChildren();
        const title = { bfs: 'Queue (vorne → hinten)', dfs: 'Stack (unten → oben)', dijkstra: 'Priority Queue (kleinste zuerst)', astar: 'Priority Queue (kleinste zuerst)' }[kind];
        side.appendChild(el('h5', { text: title }));
        const items = el('div', { class: 'algo-items' });
        if (!frame.items.length) items.appendChild(el('span', { class: 'algo-empty', text: 'leer' }));
        for (const item of frame.items) items.appendChild(el('span', { class: `algo-item${item.next ? ' next' : ''}${item.stale ? ' stale' : ''}`, text: item.label }));
        side.appendChild(items);
        const ids = Object.keys(graph.nodes);
        const table = el('table', { class: 'algo-table' });
        const head = ['Knoten', ...(frame.cost ? ['Weg'] : []), ...(kind === 'astar' ? ['Luftlinie'] : []), 'Vorgänger'];
        table.appendChild(el('tr', {}, head.map((h) => el('th', { text: h }))));
        for (const id of ids) {
            if (!(id in frame.previous)) continue;
            const row = el('tr', { class: id === frame.current ? 'current' : (frame.done || new Set(frame.order)).has(id) ? 'done' : '' });
            row.appendChild(el('td', { text: id }));
            if (frame.cost) row.appendChild(el('td', { class: (frame.changed || []).includes(id) ? 'changed' : '', text: String(frame.cost[id]) }));
            if (kind === 'astar') row.appendChild(el('td', { text: String(straight(graph, id, goal)) }));
            row.appendChild(el('td', { class: (frame.changed || []).includes(id) ? 'changed' : '', text: frame.previous[id] ?? '–' }));
            table.appendChild(row);
        }
        side.appendChild(el('h5', { text: 'Gefunden' }));
        side.appendChild(table);
    }

    // The heap: an array; the children of index i are 2i + 1 and 2i + 2.
    function traceHeap(ops) {
        const items = [];
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ items: [...items], message, ...marks });
        snapshot('Ein leerer Heap.');
        for (const op of ops) {
            if (op.type === 'push') {
                items.push(op.value);
                let i = items.length - 1;
                snapshot(`push ${op.value}: hinten anhängen, an Index ${i}.`, { focus: [i] });
                while (i > 0) {
                    const parent = Math.floor((i - 1) / 2);
                    if (items[parent] <= items[i]) {
                        snapshot(`${items[i]} ist nicht kleiner als sein Elternteil ${items[parent]} (Index ${parent}): Es passt.`, { focus: [i], compare: [parent, i] });
                        break;
                    }
                    snapshot(`${items[i]} ist kleiner als sein Elternteil ${items[parent]} (Index ${parent}): tauschen.`, { focus: [i], compare: [parent, i] });
                    [items[parent], items[i]] = [items[i], items[parent]];
                    i = parent;
                    snapshot(`${items[i]} steht jetzt an Index ${i}.`, { focus: [i] });
                }
            } else {
                if (!items.length) {
                    snapshot('pop: Der Heap ist leer, es kommt nichts heraus.');
                    continue;
                }
                const top = items[0];
                const last = items.pop();
                if (!items.length) {
                    snapshot(`pop: ${top} kommt heraus, der Heap ist jetzt leer.`, { out: top });
                    continue;
                }
                items[0] = last;
                snapshot(`pop: ${top} kommt heraus. Der letzte Eintrag, ${last}, kommt an seine Stelle, Index 0.`, { out: top, focus: [0] });
                let i = 0;
                for (;;) {
                    const kids = [2 * i + 1, 2 * i + 2].filter((k) => k < items.length);
                    if (!kids.length) {
                        snapshot(`${items[i]} hat keine Kinder mehr: Es passt.`, { focus: [i], out: top });
                        break;
                    }
                    const small = kids.reduce((a, b) => (items[b] < items[a] ? b : a));
                    if (items[small] >= items[i]) {
                        snapshot(`Kein Kind von ${items[i]} ist kleiner: Es passt.`, { focus: [i], compare: [i, ...kids], out: top });
                        break;
                    }
                    snapshot(`Das kleinere Kind ${items[small]} (Index ${small}) ist kleiner als ${items[i]}: tauschen.`, { focus: [i], compare: [i, small], out: top });
                    [items[small], items[i]] = [items[i], items[small]];
                    i = small;
                    snapshot(`${items[i]} steht jetzt an Index ${i}.`, { focus: [i], out: top });
                }
            }
        }
        return frames;
    }

    function drawHeap(svg, frame, most) {
        svg.replaceChildren();
        const n = frame.items.length;
        // as high as the biggest heap of the example, so the picture keeps its size
        const levels = Math.max(1, Math.ceil(Math.log2(most + 1)));
        const width = 520;
        const pos = (i) => {
            const level = Math.floor(Math.log2(i + 1));
            const first = 2 ** level - 1;
            const count = 2 ** level;
            return [((i - first + 0.5) / count) * width, 30 + level * 62];
        };
        const compare = new Set(frame.compare || []);
        const focus = new Set(frame.focus || []);
        for (let i = 1; i < n; i++) {
            const [x1, y1] = pos(Math.floor((i - 1) / 2));
            const [x2, y2] = pos(i);
            svg.appendChild(el('svg:line', { x1, y1, x2, y2, stroke: 'var(--algo-line)', 'stroke-width': 2 }));
        }
        for (let i = 0; i < n; i++) {
            const [x, y] = pos(i);
            const fill = focus.has(i) ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            if (compare.has(i)) svg.appendChild(el('svg:circle', { cx: x, cy: y, r: 22, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 }));
            svg.appendChild(el('svg:circle', { cx: x, cy: y, r: 17, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { x, y: y + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.items[i]) }));
            svg.appendChild(el('svg:text', { x: x + 21, y: y - 14, 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
        }
        // the array below the tree
        const top = 30 + levels * 62 + 10;
        const cell = Math.min(44, (width - 20) / Math.max(most, 1));
        for (let i = 0; i < n; i++) {
            const x = 10 + i * cell;
            svg.appendChild(el('svg:rect', { x, y: top, width: cell - 3, height: 30, rx: 4, fill: focus.has(i) ? 'var(--algo-frontier)' : 'var(--algo-paper)', stroke: compare.has(i) ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': compare.has(i) ? 3 : 1.5 }));
            svg.appendChild(el('svg:text', { x: x + (cell - 3) / 2, y: top + 20, 'text-anchor': 'middle', 'font-size': 14, fill: 'var(--algo-ink)', text: String(frame.items[i]) }));
            svg.appendChild(el('svg:text', { x: x + (cell - 3) / 2, y: top + 44, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
        }
        svg.setAttribute('viewBox', `0 0 ${width} ${top + 52}`);
    }

    function setUp(widget) {
        const algo = widget.dataset.algo;
        let frames;
        let draw;
        const svg = el('svg:svg', { class: 'algo-picture', role: 'img' });
        const side = el('div', { class: 'algo-side' });
        if (algo === 'heap') {
            const ops = widget.dataset.ops.split(',').map((text) => text.trim().split(/\s+/))
                .map(([type, value]) => ({ type, value: Number(value) }));
            frames = traceHeap(ops);
            const most = Math.max(...frames.map((frame) => frame.items.length));
            const pops = ops.some((op) => op.type === 'pop');
            svg.setAttribute('aria-label', 'Der Heap als Baum, darunter dasselbe als Array mit den Indizes');
            if (!pops) side.hidden = true;
            draw = (frame) => {
                drawHeap(svg, frame, most);
                if (!pops) return;
                side.replaceChildren(el('h5', { text: 'Heraus mit pop' }),
                    el('div', { class: 'algo-items' }, [frame.out !== undefined
                        ? el('span', { class: 'algo-item next', text: String(frame.out) })
                        : el('span', { class: 'algo-empty', text: '–' })]));
            };
        } else {
            const graph = GRAPHS[widget.dataset.graph || 'stadt'];
            const start = widget.dataset.start || 'A';
            const goal = widget.dataset.goal || 'H';
            frames = algo === 'bfs' || algo === 'dfs' ? traceSearch(graph, start, goal, algo) : traceShortest(graph, start, goal, algo);
            svg.setAttribute('viewBox', '0 0 540 340');
            svg.setAttribute('aria-label', `Der Beispielgraph mit Knoten A bis H; ${start} ist der Start, ${goal} das Ziel`);
            draw = (frame) => {
                drawGraph(svg, graph, frame, start, goal, algo);
                sideGraph(side, graph, frame, algo, goal);
            };
        }
        const button = (go, icon, label) => el('button', { class: 'btn btn-sm btn-success', 'data-go': go, 'aria-label': label, type: 'button' },
            [el('i', { class: `bi ${icon}`, style: 'margin-right: 0;' })]);
        const buttons = [button('first', 'bi-chevron-bar-left', 'Zum Anfang'), button('prev', 'bi-chevron-left', 'Einen Schritt zurück'),
            button('next', 'bi-chevron-right', 'Nächster Schritt'), button('last', 'bi-chevron-bar-right', 'Zum Ende')];
        const count = el('span', { class: 'algo-count' });
        const message = el('div', { class: 'algo-message', 'aria-live': 'polite' });
        widget.replaceChildren(el('div', { class: 'algo-controls' }, [...buttons, count]), el('div', { class: 'algo-body' }, [svg, side]), message);
        widget.setAttribute('data-stepper', '');
        let index = 0;
        const show = () => {
            const frame = frames[index];
            draw(frame);
            message.textContent = frame.message;
            count.textContent = `Schritt ${index} von ${frames.length - 1}`;
            buttons[0].disabled = buttons[1].disabled = index === 0;
            buttons[2].disabled = buttons[3].disabled = index === frames.length - 1;
        };
        const go = { first: () => 0, prev: () => index - 1, next: () => index + 1, last: () => frames.length - 1 };
        for (const b of buttons) b.addEventListener('click', () => {
            index = Math.max(0, Math.min(frames.length - 1, go[b.dataset.go]()));
            show();
        });
        show();
    }

    function start() {
        addStyle();
        document.querySelectorAll('.algo-stepper').forEach(setUp);
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
