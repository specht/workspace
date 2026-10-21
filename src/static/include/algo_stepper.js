// Step-by-step widgets for the Handbuch pages on algorithms and data
// structures (reference/graphen, reference/heap). A widget is an element
//
//   <div class='algo-stepper' data-algo='dijkstra' data-graph='stadt' data-start='A' data-goal='H'></div>
//   <div class='algo-stepper' data-algo='heap' data-ops='push 5, push 3, push 8, pop'></div>
//   <div class='algo-stepper' data-algo='sort' data-sort='insertion' data-values='5, 2, 8, 1'></div>
//
// algo: bfs, dfs, dijkstra, astar (on a graph from GRAPHS), heap, sort
// (selection, insertion, bubble, merge, quick). The widget
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

    // How long the movement from one step to the next takes, in ms; a step
    // with pulses one after another takes PULSE_TIME for each
    const MOTION_TIME = 550;
    const PULSE_TIME = 650;

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
.algo-stepper svg [data-key] { transition: fill 0.3s, stroke 0.3s; }
.algo-stepper pre.algo-code { font-size: 0.85em; margin: 0; padding: 0.4em 0; white-space: pre-wrap; }
.algo-stepper pre.algo-code span.line { display: block; padding: 0.05em 0.6em; border-left: 3px solid transparent; transition: background-color 0.3s; }
.algo-stepper pre.algo-code span.line.current { background: rgba(255, 255, 255, 0.13); border-left-color: #b197fc; }
.algo-stepper ol.algo-stack { list-style: none; padding: 0; margin: 0 0 0.8em; font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.82em; }
.algo-stepper ol.algo-stack li { padding: 0.05em 0.5em; border-left: 2px solid var(--algo-line); }
.algo-stepper ol.algo-stack li.current { background: color-mix(in srgb, var(--algo-accent) 18%, transparent); border-left-color: var(--algo-accent); font-weight: 600; }
.algo-stepper ol.algo-stack li.returning { animation: algo-return 0.4s ease forwards; }
@keyframes algo-return { to { opacity: 0.3; } }
@media (prefers-reduced-motion: reduce) { .algo-stepper ol.algo-stack li.returning { animation: none; opacity: 0.3; } }
.algo-stepper .algo-ops { display: flex; flex-wrap: wrap; gap: 0.3em; list-style: none; padding: 0; margin: 0 0 0.7em;
    font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.85em; }
.algo-stepper .algo-ops li { padding: 0.05em 0.5em; border-radius: 0.3em; border: 1px solid color-mix(in srgb, var(--algo-line) 70%, transparent);
    transition: background-color 0.3s, opacity 0.3s; }
.algo-stepper .algo-ops li.done { opacity: 0.45; }
.algo-stepper .algo-ops li.current { background: color-mix(in srgb, var(--algo-accent) 22%, transparent); border-color: var(--algo-accent); font-weight: 600; }
@media (prefers-reduced-motion: reduce) { .algo-stepper svg [data-key] { transition: none; } }
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

    // Sorting: every comparison and every write is a step, counted like the
    // Sorting Sandbox counts them. sorted: indices already in their place.
    // ids: which of the numbers stands where (they may repeat), so that the
    // picture moves each bar to its new place.
    function traceSort(values, kind) {
        const a = [...values];
        const ids = values.map((_, i) => i);
        const frames = [];
        let comparisons = 0;
        let writes = 0;
        const snapshot = (message, marks = {}) => frames.push({ items: [...a], ids: [...ids], message, comparisons, writes, ...marks });
        const less = (i, j) => { comparisons += 1; return a[i] < a[j]; };
        const swap = (i, j) => { [a[i], a[j]] = [a[j], a[i]]; [ids[i], ids[j]] = [ids[j], ids[i]]; writes += 2; };
        const n = a.length;
        snapshot(`${n} Zahlen, unsortiert.`);
        if (kind === 'selection') {
            for (let i = 0; i < n - 1; i++) {
                let min = i;
                for (let j = i + 1; j < n; j++) {
                    const smaller = less(j, min);
                    snapshot(`Kleinste ab Index ${i} gesucht: ${a[j]} ${smaller ? 'ist kleiner als' : 'ist nicht kleiner als'} ${a[min]}.`, { compare: [j, min], sortedTo: i, mark: [min] });
                    if (smaller) min = j;
                }
                if (min !== i) swap(i, min);
                snapshot(min !== i ? `Die kleinste, ${a[i]}, tauscht mit Index ${i}. Bis Index ${i} ist alles fertig.` : `${a[i]} steht schon richtig. Bis Index ${i} ist alles fertig.`, { sortedTo: i + 1, mark: [i] });
            }
        } else if (kind === 'insertion') {
            for (let i = 1; i < n; i++) {
                const current = a[i];
                const currentId = ids[i];
                let j = i;
                snapshot(`${current} (Index ${i}) wird einsortiert, links davon ist alles sortiert.`, { lifted: i, sortedTo: i });
                while (j > 0) {
                    comparisons += 1;
                    if (!(a[j - 1] > current)) {
                        snapshot(`${a[j - 1]} ist nicht größer als ${current}: Hier ist sein Platz.`, { compare: [j - 1], lifted: j, sortedTo: i });
                        break;
                    }
                    a[j] = a[j - 1];
                    ids[j] = ids[j - 1];
                    writes += 1;
                    a[j - 1] = current;
                    ids[j - 1] = currentId;
                    snapshot(`${a[j]} ist größer als ${current} und rückt eins nach rechts.`, { compare: [j], lifted: j - 1, sortedTo: i });
                    j -= 1;
                }
                a[j] = current;
                ids[j] = currentId;
                writes += 1;
                snapshot(`${current} steht an Index ${j}. Bis Index ${i} ist alles sortiert.`, { mark: [j], sortedTo: i + 1 });
            }
        } else if (kind === 'bubble') {
            for (let end = n - 1; end > 0; end--) {
                let swapped = false;
                for (let i = 0; i < end; i++) {
                    const bigger = less(i + 1, i);
                    if (bigger) { swap(i, i + 1); swapped = true; }
                    snapshot(bigger ? `${a[i + 1]} ist größer als ${a[i]}: tauschen.` : `${a[i]} und ${a[i + 1]} stehen richtig herum.`, { compare: [i, i + 1], sortedFrom: end + 1 });
                }
                snapshot(`Ein Durchgang: Die größte bis hier, ${a[end]}, ist ganz nach rechts gewandert.`, { sortedFrom: end, mark: [end] });
                if (!swapped) {
                    snapshot('In diesem Durchgang wurde nichts getauscht: Alles ist sortiert.', { sortedFrom: 0 });
                    break;
                }
            }
        } else if (kind === 'merge') {
            const scratch = Array(n).fill(null);
            const scratchIds = Array(n).fill(null);
            const sort = (lo, hi) => {
                if (hi - lo < 2) return;
                const mid = Math.floor((lo + hi) / 2);
                sort(lo, mid);
                sort(mid, hi);
                snapshot(`Die beiden sortierten Hälften ${lo}–${mid - 1} und ${mid}–${hi - 1} werden zusammengeführt.`, { range: [lo, hi], split: mid });
                let i = lo;
                let j = mid;
                for (let k = lo; k < hi; k++) {
                    let take;
                    if (i < mid && j < hi) {
                        comparisons += 1;
                        take = a[i] <= a[j] ? i++ : j++;
                    } else take = i < mid ? i++ : j++;
                    scratch[k] = a[take];
                    scratchIds[k] = ids[take];
                    writes += 1;
                    snapshot(`Die kleinere der beiden vordersten, ${a[take]}, kommt in den Zwischenspeicher an Index ${k}.`, { range: [lo, hi], split: mid, compare: [take], scratch: [...scratch], scratchIds: [...scratchIds] });
                }
                for (let k = lo; k < hi; k++) { a[k] = scratch[k]; ids[k] = scratchIds[k]; writes += 1; }
                scratch.fill(null, lo, hi);
                scratchIds.fill(null, lo, hi);
                snapshot(`${lo}–${hi - 1} ist sortiert und steht wieder in der Liste.`, { range: [lo, hi], mark: Array.from({ length: hi - lo }, (_, k) => lo + k) });
            };
            sort(0, n);
        } else if (kind === 'quick') {
            const sort = (lo, hi) => {
                if (hi - lo < 1) return;
                const pivot = a[hi];
                snapshot(`Bereich ${lo}–${hi}: Das Pivot ist der letzte, ${pivot}.`, { range: [lo, hi + 1], pivot: hi });
                let store = lo;
                for (let i = lo; i < hi; i++) {
                    comparisons += 1;
                    const small = a[i] < pivot;
                    if (small) {
                        if (i !== store) swap(i, store);
                        store += 1;
                    }
                    snapshot(small ? `${a[store - 1]} ist kleiner als ${pivot} und kommt nach links (vor Index ${store}).` : `${a[i]} ist nicht kleiner als ${pivot} und bleibt rechts.`, { range: [lo, hi + 1], pivot: hi, compare: [i], boundary: store });
                }
                swap(store, hi);
                snapshot(`Das Pivot ${pivot} kommt an Index ${store}: Links stehen die kleineren, rechts die anderen. ${pivot} ist an seinem Platz.`, { range: [lo, hi + 1], mark: [store] });
                sort(lo, store - 1);
                sort(store + 1, hi);
            };
            sort(0, n - 1);
        }
        snapshot(`Fertig: ${comparisons} Vergleiche, ${writes} Schreibzugriffe.`, { sortedTo: n });
        return frames;
    }

    function drawSort(svg, frame) {
        svg.replaceChildren();
        const n = frame.items.length;
        const most = Math.max(...frame.items);
        const width = 520;
        const cell = width / n;
        const height = 200;
        const compare = new Set(frame.compare || []);
        const mark = new Set(frame.mark || []);
        const inRange = (i) => !frame.range || (i >= frame.range[0] && i < frame.range[1]);
        const isSorted = (i) => (frame.sortedTo !== undefined && i < frame.sortedTo) || (frame.sortedFrom !== undefined && i >= frame.sortedFrom);
        // keys: a bar and its number by the number's id, so they move with it
        frame.items.forEach((value, i) => {
            const id = frame.ids[i];
            const h = 20 + (value / most) * (height - 40);
            const x = i * cell + 4;
            const lifted = frame.lifted === i;
            const y = height - h - (lifted ? 18 : 0);
            const fill = frame.pivot === i ? 'var(--algo-goal)' : mark.has(i) || lifted ? 'var(--algo-accent)' : compare.has(i) ? 'var(--algo-frontier)'
                : isSorted(i) ? 'var(--algo-done)' : 'color-mix(in srgb, var(--algo-line) 60%, transparent)';
            svg.appendChild(el('svg:rect', { 'data-key': `bar-${id}`, x, y, width: cell - 8, height: h, rx: 4, fill, opacity: inRange(i) ? 1 : 0.35,
                stroke: compare.has(i) ? 'var(--algo-ink)' : 'none', 'stroke-width': 2 }));
            svg.appendChild(el('svg:text', { 'data-key': `value-${id}`, x: x + (cell - 8) / 2, y: y - 6, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            svg.appendChild(el('svg:text', { 'data-key': `index-${i}`, x: x + (cell - 8) / 2, y: height + 16, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
        });
        if (frame.split !== undefined) {
            const x = frame.split * cell;
            svg.appendChild(el('svg:line', { 'data-key': 'split', x1: x, y1: 0, x2: x, y2: height, stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        }
        if (frame.boundary !== undefined) {
            const x = frame.boundary * cell;
            svg.appendChild(el('svg:line', { 'data-key': 'boundary', x1: x, y1: 0, x2: x, y2: height, stroke: 'var(--algo-goal)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        }
        let bottom = height + 24;
        // merge sort keeps its (then empty) scratch row in every step
        const scratch = frame.scratch || (frame.scratchSpace ? Array(n).fill(null) : null);
        if (scratch) {
            // a number flies from its bar into the scratch row, and from there
            // back to its bar when the merged run is copied back
            scratch.forEach((value, i) => {
                const x = i * cell + 4;
                svg.appendChild(el('svg:rect', { 'data-key': `scratch-${i}`, x, y: bottom + 6, width: cell - 8, height: 28, rx: 4, fill: value === null ? 'none' : 'var(--algo-frontier)', stroke: 'var(--algo-line)', 'stroke-width': 1.5 }));
                if (value !== null) {
                    const id = frame.scratchIds[i];
                    svg.appendChild(el('svg:text', { 'data-key': `scratch-value-${id}`, 'data-from': `value-${id}`, 'data-to': `value-${id}`,
                        x: x + (cell - 8) / 2, y: bottom + 25, 'text-anchor': 'middle', 'font-size': 13, fill: 'var(--algo-ink)', text: String(value) }));
                }
            });
            svg.appendChild(el('svg:text', { 'data-key': 'scratch-label', x: 0, y: bottom + 50, 'font-size': 11, fill: 'var(--bs-secondary-color)', text: 'Zwischenspeicher' }));
            bottom += 56;
        }
        svg.setAttribute('viewBox', `0 0 ${width} ${frame.scratchSpace ? height + 80 : bottom}`);
    }

    // ---- Queue as a ring buffer: head, size, capacity; the next free cell
    // is (head + size) mod capacity.
    function traceRing(ops, capacity) {
        const cells = Array(capacity).fill(null);
        let head = 0;
        let size = 0;
        let out;
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ cells: [...cells], head, size, capacity, message, out, ...marks });
        snapshot(`Eine leere Queue in einem Array mit ${capacity} Zellen: head = 0, size = 0.`);
        for (const op of ops) {
            if (op.type === 'enqueue') {
                if (size === capacity) { snapshot(`enqueue ${op.value}: Alle ${capacity} Zellen sind belegt, die Queue ist voll.`, { error: true }); continue; }
                const at = (head + size) % capacity;
                cells[at] = op.value;
                size += 1;
                snapshot(`enqueue ${op.value}: in die Zelle (head + size) mod ${capacity} = (${head} + ${size - 1}) mod ${capacity} = ${at}. size wird ${size}.`, { focus: at });
            } else {
                if (size === 0) { snapshot('dequeue: Die Queue ist leer.', { error: true }); continue; }
                const value = cells[head];
                cells[head] = null;
                const old = head;
                head = (head + 1) % capacity;
                size -= 1;
                out = value;
                snapshot(`dequeue: ${value} aus Zelle head = ${old}. head wird (${old} + 1) mod ${capacity} = ${head}, size ${size}.`, { focus: old });
            }
        }
        return frames;
    }

    function drawRing(svg, frame) {
        svg.replaceChildren();
        const cell = 56;
        const left = 20;
        frame.cells.forEach((value, i) => {
            const x = left + i * cell;
            svg.appendChild(el('svg:rect', { x, y: 50, width: cell - 6, height: 44, rx: 6, fill: value === null ? 'var(--algo-paper)' : 'var(--algo-frontier)', stroke: frame.focus === i ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': frame.focus === i ? 3.5 : 1.5 }));
            if (value !== null) svg.appendChild(el('svg:text', { x: x + (cell - 6) / 2, y: 78, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            svg.appendChild(el('svg:text', { x: x + (cell - 6) / 2, y: 110, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
        });
        const arrow = (i, label, y, up) => {
            const x = left + i * cell + (cell - 6) / 2;
            svg.appendChild(el('svg:text', { x, y: up ? y : y + 12, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, fill: 'var(--algo-accent)', text: label }));
            svg.appendChild(el('svg:path', { d: up ? `M${x} ${y + 4} l0 12 m-5 -5 l5 5 l5 -5` : `M${x} ${y - 4} l0 -12 m-5 5 l5 -5 l5 5`, stroke: 'var(--algo-accent)', 'stroke-width': 2, fill: 'none' }));
        };
        arrow(frame.head, 'head', 26, true);
        arrow((frame.head + frame.size) % frame.capacity, frame.size === frame.capacity ? 'voll' : 'nächste frei', 132, false);
        svg.setAttribute('viewBox', `0 0 ${left * 2 + frame.capacity * cell} 150`);
    }

    // ---- Array List: the values in the first size cells of an array;
    // insert and remove shift everything behind the index, a full array is
    // copied into one twice as large.
    function traceArray(ops, capacity) {
        let cells = Array(capacity).fill(null);
        let size = 0;
        let moves = 0;
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ cells: [...cells], size, moves, message, ...marks });
        snapshot(`Eine leere Array List: ein Array mit ${capacity} Zellen, size = 0.`);
        for (const { type, index, value } of ops) {
            if (type === 'insert') {
                if (index < 0 || index > size) { snapshot(`insert(${index}, ${value}): Index ${index} gibt es nicht, erlaubt ist 0 bis ${size}.`, { error: true }); continue; }
                if (size === cells.length) {
                    const bigger = Array(cells.length * 2).fill(null);
                    cells.forEach((v, i) => { bigger[i] = v; });
                    cells = bigger;
                    moves += size;
                    snapshot(`insert(${index}, ${value}): Das Array ist voll. Ein neues mit ${cells.length} Zellen, alle ${size} Werte werden hinüberkopiert.`, { copied: size });
                }
                for (let i = size; i > index; i--) {
                    cells[i] = cells[i - 1];
                    moves += 1;
                    snapshot(`insert(${index}, ${value}): ${cells[i]} rückt von Index ${i - 1} nach ${i}.`, { focus: i, moved: true });
                }
                cells[index] = value;
                size += 1;
                snapshot(`insert(${index}, ${value}): ${value} kommt an Index ${index}. size wird ${size}.${size - 1 - index === 1 ? ' Dafür ist ein Wert gerückt.' : size - 1 - index ? ` Dafür sind ${size - 1 - index} Werte gerückt.` : ''}`, { focus: index, fresh: true });
            } else if (type === 'remove') {
                if (index < 0 || index >= size) { snapshot(`remove(${index}): Index ${index} gibt es nicht.`, { error: true }); continue; }
                const gone = cells[index];
                snapshot(`remove(${index}): ${gone} kommt heraus.`, { focus: index, gone: true });
                for (let i = index; i < size - 1; i++) {
                    cells[i] = cells[i + 1];
                    moves += 1;
                    snapshot(`remove(${index}): ${cells[i]} rückt von Index ${i + 1} nach ${i}.`, { focus: i, moved: true });
                }
                size -= 1;
                cells[size] = null;
                snapshot(`size wird ${size}.`);
            } else if (type === 'get') {
                if (index < 0 || index >= size) { snapshot(`get(${index}): Index ${index} gibt es nicht.`, { error: true }); continue; }
                snapshot(`get(${index}): ${cells[index]}, sofort. Die Zelle liegt an einer bekannten Stelle im Speicher, nichts muss laufen.`, { focus: index });
            }
        }
        return frames;
    }

    function drawArray(svg, frame, most) {
        svg.replaceChildren();
        const cell = 50;
        const left = 14;
        frame.cells.forEach((value, i) => {
            const x = left + i * cell;
            const used = i < frame.size || value !== null;
            const fill = frame.gone && frame.focus === i ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))'
                : frame.focus === i ? 'var(--algo-frontier)' : used ? 'var(--algo-paper)' : 'transparent';
            svg.appendChild(el('svg:rect', { x, y: 40, width: cell - 6, height: 42, rx: 6, fill, stroke: frame.focus === i ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': frame.focus === i ? 3.5 : 1.5, 'stroke-dasharray': used || frame.focus === i ? null : '4 4' }));
            if (value !== null) svg.appendChild(el('svg:text', { x: x + (cell - 6) / 2, y: 67, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            svg.appendChild(el('svg:text', { x: x + (cell - 6) / 2, y: 98, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
        });
        svg.appendChild(el('svg:text', { x: left, y: 24, 'font-size': 13, fill: 'var(--algo-ink)', text: `size = ${frame.size}, Kapazität ${frame.cells.length}` }));
        svg.setAttribute('viewBox', `0 0 ${left * 2 + most * cell} 110`);
    }

    // ---- Linked List: nodes with value and next; get, insert and remove walk
    // from head to the node before the index.
    function traceLinked(ops) {
        let nodes = [];
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ nodes: nodes.map((n) => ({ ...n })), message, ...marks });
        snapshot('Eine leere Linked List: head zeigt auf keinen Knoten.');
        for (const op of ops) {
            const { type, index, value } = op;
            if (type === 'insert') {
                if (index < 0 || index > nodes.length) { snapshot(`insert(${index}, ${value}): Index ${index} gibt es nicht.`, { error: true }); continue; }
                if (index === 0) {
                    nodes.unshift({ value, fresh: true });
                    snapshot(`insert(0, ${value}): ein neuer Knoten, sein next zeigt auf den alten ersten, und head zeigt auf ihn. Nichts muss laufen.`, { focus: 0 });
                } else {
                    for (let i = 0; i < index; i++) snapshot(`insert(${index}, ${value}): Laufen zum Knoten vor Index ${index}, gerade bei Index ${i}.`, { walk: i });
                    const last = index === nodes.length;
                    nodes.splice(index, 0, { value, fresh: true });
                    snapshot(last
                        ? `Der Knoten davor zeigt jetzt auf den neuen, dessen next auf nichts zeigt: eine Referenz gesetzt, nichts verschoben.`
                        : `Der neue Knoten zeigt auf den, der bisher an Index ${index} war, und der Knoten davor zeigt jetzt auf ihn: zwei Referenzen umgebogen, nichts verschoben.`, { focus: index, walk: index - 1 });
                }
                nodes.forEach((n) => delete n.fresh);
            } else if (type === 'remove') {
                if (index < 0 || index >= nodes.length) { snapshot(`remove(${index}): Index ${index} gibt es nicht.`, { error: true }); continue; }
                for (let i = 0; i < index; i++) snapshot(`remove(${index}): Laufen zum Knoten vor Index ${index}, gerade bei Index ${i}.`, { walk: i });
                const gone = nodes[index].value;
                snapshot(index === 0 ? `remove(0): head zeigt jetzt auf den zweiten Knoten; ${gone} hängt an nichts mehr.` : `Der Knoten davor zeigt jetzt am Knoten mit ${gone} vorbei auf den nächsten.`, { focus: index, gone: index, walk: index - 1 });
                nodes.splice(index, 1);
                snapshot(`${gone} ist weg.`);
            } else if (type === 'get') {
                if (index < 0 || index >= nodes.length) { snapshot(`get(${index}): Index ${index} gibt es nicht.`, { error: true }); continue; }
                for (let i = 0; i <= index; i++) snapshot(i < index ? `get(${index}): bei Index ${i}, weiter mit next.` : `get(${index}): ${nodes[i].value}. Dafür ist die Suche ${index} Knoten weit gelaufen.`, { walk: i, focus: i === index ? i : undefined });
            }
        }
        return frames;
    }

    function drawLinked(svg, frame, most) {
        svg.replaceChildren();
        const box = 58;
        const gap = 26;
        const top = 46;
        const x0 = 62;
        svg.appendChild(el('svg:text', { x: 4, y: top + 26, 'font-size': 13, 'font-weight': 600, fill: 'var(--algo-ink)', text: 'head' }));
        const headTarget = frame.nodes.length ? x0 : null;
        if (headTarget !== null) svg.appendChild(el('svg:path', { d: `M40 ${top + 22} L${x0 - 4} ${top + 22} m-6 -5 l6 5 l-6 5`, stroke: 'var(--algo-ink)', 'stroke-width': 2, fill: 'none' }));
        else svg.appendChild(el('svg:text', { x: 42, y: top + 26, 'font-size': 13, fill: 'var(--bs-secondary-color)', text: '→ nichts' }));
        frame.nodes.forEach((node, i) => {
            const x = x0 + i * (box + gap);
            const fill = frame.gone === i ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))' : frame.focus === i ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            svg.appendChild(el('svg:rect', { x, y: top, width: box, height: 44, rx: 6, fill, stroke: frame.walk === i ? 'var(--algo-accent)' : 'var(--algo-ink)', 'stroke-width': frame.walk === i ? 3.5 : 1.5 }));
            svg.appendChild(el('svg:line', { x1: x + box * 0.62, y1: top, x2: x + box * 0.62, y2: top + 44, stroke: 'var(--algo-line)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { x: x + box * 0.31, y: top + 28, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(node.value) }));
            svg.appendChild(el('svg:text', { x: x + box * 0.31, y: top + 62, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
            const nx = x + box * 0.81;
            if (i < frame.nodes.length - 1) {
                const skip = frame.gone === i + 1 && i + 2 < frame.nodes.length;
                if (skip) {
                    const tx = x0 + (i + 2) * (box + gap);
                    svg.appendChild(el('svg:path', { d: `M${nx} ${top + 10} C ${nx + 40} ${top - 40}, ${tx - 40} ${top - 40}, ${tx - 2} ${top + 8}`, stroke: 'var(--algo-accent)', 'stroke-width': 2, fill: 'none' }));
                    svg.appendChild(el('svg:path', { d: `M${tx - 2} ${top + 8} l-1.6 -8.8 M${tx - 2} ${top + 8} l-8.3 -3.6`, stroke: 'var(--algo-accent)', 'stroke-width': 2, fill: 'none', 'stroke-linecap': 'round' }));
                } else {
                    svg.appendChild(el('svg:path', { d: `M${nx} ${top + 22} L${x + box + gap - 4} ${top + 22} m-6 -5 l6 5 l-6 5`, stroke: 'var(--algo-ink)', 'stroke-width': 2, fill: 'none' }));
                }
            } else {
                svg.appendChild(el('svg:text', { x: nx, y: top + 27, 'text-anchor': 'middle', 'font-size': 13, fill: 'var(--bs-secondary-color)', text: '∅' }));
            }
        });
        svg.setAttribute('viewBox', `0 0 ${x0 + Math.max(most, 1) * (box + gap) + 10} ${top + 80}`);
    }

    // ---- Search trees: a binary search tree, optionally AVL (heights and
    // rotations after every insert). Every step follows a line of the
    // Handbuch's pseudocode (CODE, reference/baeume), and a frame holds the
    // nodes as they are in memory: graph (id -> { value, left, right, height },
    // the children as ids) and rootId. A node has an id apart from its value:
    // remove copies a value into another node, and for a moment it is there
    // twice.
    const CODE = {
        find: ['find(node, value):', '    if node empty: not found', '    if value = node.value: found',
            '    if value < node.value: return find(node.left, value)', '    else: return find(node.right, value)'],
        insert: ['insert(node, value):', '    if node empty: return a new node with value', '    if value < node.value: node.left ← insert(node.left, value)',
            '    if value > node.value: node.right ← insert(node.right, value)', '    return node'],
        remove: ['remove(node, value):', '    if node empty: return empty', '    if value < node.value: node.left ← remove(node.left, value)',
            '    else if value > node.value: node.right ← remove(node.right, value)', '    else:', '        if node.left empty: return node.right',
            '        if node.right empty: return node.left', '        successor ← node.right', '        while successor.left not empty: successor ← successor.left',
            '        node.value ← successor.value', '        node.right ← remove(node.right, successor.value)', '    return node'],
    };

    // The nodes as they are in memory, by id
    const toGraph = (n, graph = {}) => {
        if (!n) return graph;
        graph[n.id] = { value: n.value, left: n.left ? n.left.id : null, right: n.right ? n.right.id : null, height: n.height };
        toGraph(n.left, graph);
        toGraph(n.right, graph);
        return graph;
    };
    const fromGraph = (graph, id) => (id === null || id === undefined ? null
        : { id, value: graph[id].value, height: graph[id].height, left: fromGraph(graph, graph[id].left), right: fromGraph(graph, graph[id].right) });
    const fixHeights = (n) => {
        if (!n) return 0;
        n.height = 1 + Math.max(fixHeights(n.left), fixHeights(n.right));
        return n.height;
    };

    // setup: operations that build the tree the widget starts with, without
    // steps of their own (a demo of remove starts with the finished tree)
    function traceTree(ops, avl, setup = []) {
        let root = null;
        let nextId = 0;
        const frames = [];
        const height = (n) => (n ? n.height : 0);
        const update = (n) => { n.height = 1 + Math.max(height(n.left), height(n.right)); };
        let current = -1;
        // marks: focus, fresh, gone (ids); incoming (a value on its way, to
        // the node incomingTo); vars (labels with an arrow); code; copied;
        // pulses (a light running down a pointer when it is assigned the
        // subtree its call returned; the way down shows the ring alone);
        // stack (the calls that run, the deepest last: { call, returns }) and
        // returning (they return one after another, the deepest first)
        const snapshot = (message, marks = {}) => frames.push({ graph: toGraph(root), rootId: root ? root.id : null, message, avl, op: current, ...marks });
        const code = (name, at) => ({ lines: CODE[name], at });
        // node and successor travel with the node they point to: their arrow
        // stays rigid instead of letting go and snapping on
        const nodeVar = (n) => (n ? [{ name: 'node', to: n.id, side: 'left', rigid: true }] : []);
        const sideWord = (side) => (side === 'left' ? 'links' : 'rechts');
        const call = (name, n, value) => ({ call: `${name}(node: ${n ? n.value : 'empty'}, value: ${value})` });
        const stackOf = (calls) => calls.map((c) => ({ ...c }));
        // every call returns its node: the stack for the step back up
        const returning = (calls, nodes) => calls.map((c, i) => ({ ...c, returns: c.returns || String(nodes[i].value) }));
        // back up: every pointer of the way gets its subtree again, the
        // lowest first, one after another with a pause between (the step
        // takes PULSE_TIME for each)
        // the last one: root ← insert(root, value) (or remove), the arrow of
        // the variable root
        const back = (edges) => [...edges].reverse().concat(['pointer-root']).map((edge, i, all) => ({ edge, delay: i / all.length, length: 0.6 / all.length }));
        const backTime = (edges) => Math.max(MOTION_TIME, (edges.length + 1) * PULSE_TIME);
        snapshot(avl ? 'Ein leerer AVL-Baum.' : 'Ein leerer Suchbaum.');

        const find = (value) => {
            let walk = root;
            const calls = [];
            while (walk && walk.value !== value) {
                const left = value < walk.value;
                calls.push(call('find', walk, value));
                snapshot(`find ${value}: ${value} ${left ? '<' : '>'} ${walk.value}, also nach ${left ? 'links' : 'rechts'}.`,
                    { focus: walk.id, incoming: value, vars: nodeVar(walk), code: code('find', left ? 3 : 4), stack: stackOf(calls) });
                walk = left ? walk.left : walk.right;
            }
            calls.push(call('find', walk, value));
            if (walk) snapshot(`find ${value}: gefunden.`, { focus: walk.id, incoming: value, vars: nodeVar(walk), code: code('find', 2), stack: stackOf(calls) });
            else snapshot(`find ${value}: Hier wäre ${value}, aber der Platz ist leer. ${value} ist nicht im Baum.`, { code: code('find', 1), stack: stackOf(calls) });
        };

        const insert = (value) => {
            const id = nextId;
            const path = [];
            const calls = [];
            let walk = root;
            while (walk) {
                const dir = value < walk.value ? 'left' : value > walk.value ? 'right' : null;
                calls.push(call('insert', walk, value));
                snapshot(dir ? `insert ${value}: ${value} ${dir === 'left' ? '<' : '>'} ${walk.value}, also nach ${sideWord(dir)}.` : `insert ${value}: ${value} ist schon da, nichts ändert sich.`,
                    { focus: walk.id, incoming: value, incomingTo: id, vars: nodeVar(walk), code: code('insert', dir === 'left' ? 2 : dir === 'right' ? 3 : 4), stack: stackOf(calls) });
                if (!dir) return;
                path.push({ node: walk, side: dir });
                walk = walk[dir];
            }
            const bottom = { ...call('insert', null, value), returns: `neuer Knoten ${value}` };
            nextId += 1;
            const fresh = { id, value, left: null, right: null, height: 1 };
            if (path.length) path[path.length - 1].node[path[path.length - 1].side] = fresh;
            else root = fresh;
            for (const { node } of [...path].reverse()) update(node);
            const bad = avl && [...path].reverse().map((p) => p.node).find((n) => Math.abs(height(n.left) - height(n.right)) > 1);
            if (!bad) {
                snapshot(path.length ? `${value} kommt an die leere Stelle, an der die Suche geendet hat: if node empty: return a new node with value. Der Aufrufer trägt den neuen Knoten ein.` : `${value} wird die Wurzel: root ← insert(root, ${value}).`,
                    { fresh: id, code: code('insert', 1), stack: stackOf([...calls, bottom]) });
                if (path.length) snapshot('return node: Jeder Aufruf darüber gibt seinen Knoten zurück, und der Aufrufer trägt ihn wieder ein, von unten nach oben, zuletzt root ← insert(root, value). Die Zeiger zeigen danach auf dasselbe wie vorher, nur der unterste zeigt auf den neuen Knoten.',
                    { fresh: id, code: code('insert', 4), pulses: back(path.map(({ node, side }) => `edge-${node.id}-${side}`)), duration: backTime(path),
                        stack: returning(calls, path.map((p) => p.node)), returning: true });
                return;
            }
            // AVL: the lowest node whose subtrees differ by more than 1, and the case
            const balance = height(bad.left) - height(bad.right);
            const heavyLeft = balance > 1;
            const child = heavyLeft ? bad.left : bad.right;
            const outer = heavyLeft ? value < child.value : value > child.value;
            const kind = heavyLeft ? (outer ? 'LL' : 'LR') : (outer ? 'RR' : 'RL');
            // the calls down to the node that is out of balance: it rotates
            const waiting = stackOf(calls.slice(0, path.findIndex((p) => p.node === bad) + 1));
            snapshot(`${value} hängt jetzt unten. Beim Knoten ${bad.value} unterscheiden sich die Höhen der Teilbäume um ${Math.abs(balance)}: zu viel. Das ist der Fall ${kind}.`,
                { fresh: id, focus: bad.id, vars: nodeVar(bad), code: code('insert', 1), stack: waiting });
            const memory = toGraph(root);
            let rootId = root.id;
            const plan = { LL: [[bad.id, 'right']], RR: [[bad.id, 'left']], LR: [[child.id, 'left'], [bad.id, 'right']], RL: [[child.id, 'right'], [bad.id, 'left']] }[kind];
            plan.forEach(([top, dir], k) => {
                const prefix = plan.length > 1 ? `Fall ${kind}, ${k === 0 ? 'erste' : 'zweite'} Rotation: ` : `Fall ${kind}: `;
                const after = k === plan.length - 1 ? 'Der Baum ist wieder ausgeglichen, und die Suchregel gilt immer noch.' : `Jetzt hängt alles auf einer Seite, wie im Fall ${kind === 'LR' ? 'LL' : 'RR'}.`;
                rootId = rotationFrames(frames, memory, rootId, top, dir, { op: current, avl, fresh: id, stack: waiting }, prefix, after);
            });
            root = fromGraph(memory, rootId);
        };

        // remove, line by line: the way down, then one of the three cases;
        // with two children the value of the successor is copied, and its old
        // node is removed by the same function further down
        const remove = (value) => {
            const L = (at) => code('remove', at);
            let target = value;
            let n = root;
            let parent = null;
            let side = null;
            let extra = [];
            const followed = [];
            // the calls that run, and the node each of them got
            const calls = [];
            const nodes = [];
            for (;;) {
                calls.push(call('remove', n, target));
                nodes.push(n);
                if (!n) {
                    calls[calls.length - 1].returns = 'empty';
                    snapshot(`remove ${target}: Der Platz ist leer, ${target} ist nicht im Baum: return empty.`, { code: L(1), vars: extra, stack: stackOf(calls) });
                    calls.pop();
                    nodes.pop();
                    break;
                }
                if (target !== n.value) {
                    const dir = target < n.value ? 'left' : 'right';
                    snapshot(`remove ${target}: ${target} ${dir === 'left' ? '<' : '>'} ${n.value}, also weiter ${sideWord(dir)}: node.${dir} ← remove(node.${dir}, ${target}).`,
                        { focus: n.id, incoming: target, vars: [...nodeVar(n), ...extra], code: L(dir === 'left' ? 2 : 3), stack: stackOf(calls) });
                    followed.push(`edge-${n.id}-${dir}`);
                    parent = n;
                    side = dir;
                    n = n[dir];
                    continue;
                }
                snapshot(`remove ${target}: gefunden.`, { focus: n.id, gone: n.id, incoming: target, vars: [...nodeVar(n), ...extra], code: L(4), stack: stackOf(calls) });
                if (!n.left || !n.right) {
                    const missing = !n.left ? 'left' : 'right';
                    const replacement = !n.left ? n.right : n.left;
                    const where = parent ? `${parent.value}.${side}` : 'root';
                    const gone = n;
                    if (parent) parent[side] = replacement;
                    else root = replacement;
                    // this call returns at once; the ones above it wait
                    calls[calls.length - 1].returns = replacement ? String(replacement.value) : 'empty';
                    const deepest = stackOf(calls);
                    calls.pop();
                    nodes.pop();
                    snapshot(replacement
                        ? `node.${missing} ist leer: return node.${missing === 'left' ? 'right' : 'left'}. Der Aufrufer trägt ${replacement.value} ein, ${where} zeigt jetzt auf ${replacement.value}. ${gone.value} hängt an keinem Zeiger mehr und ist weg.`
                        : `${gone.value} hat keine Kinder: return node.right gibt empty zurück, ${where} zeigt auf nichts mehr. ${gone.value} ist weg.`,
                    // the pointer swings over, then gets its new subtree
                    { code: L(missing === 'left' ? 5 : 6), vars: parent ? nodeVar(parent) : [], focus: parent ? parent.id : undefined, pulses: replacement && parent ? [{ edge: `edge-${parent.id}-${side}`, delay: 0.6 }] : [], stack: deepest });
                    break;
                }
                let successor = n.right;
                const successorVar = () => [{ name: 'successor', to: successor.id, side: 'below', rigid: true }];
                snapshot(`Zwei Kinder: Der nächstgrößere Wert kommt an diese Stelle. successor ← node.right, das ist ${successor.value}.`,
                    { focus: n.id, gone: n.id, vars: [...nodeVar(n), ...successorVar()], code: L(7), stack: stackOf(calls) });
                while (successor.left) {
                    successor = successor.left;
                    snapshot(`successor.left ist nicht leer: successor ← successor.left, das ist ${successor.value}.`, { focus: n.id, gone: n.id, vars: [...nodeVar(n), ...successorVar()], code: L(8), stack: stackOf(calls) });
                }
                snapshot(`successor.left ist leer: ${successor.value} ist der kleinste Wert rechts von ${n.value}, der nächstgrößere.`, { focus: n.id, gone: n.id, vars: [...nodeVar(n), ...successorVar()], code: L(8), stack: stackOf(calls) });
                const old = n.value;
                n.value = successor.value;
                snapshot(`node.value ← successor.value: ${successor.value} kommt in den Knoten, in dem ${old} stand. Jetzt steht ${successor.value} zweimal im Baum.`,
                    { focus: n.id, copied: { into: n.id, from: successor.id }, vars: [...nodeVar(n), ...successorVar()], code: L(9), stack: stackOf(calls) });
                extra = successorVar();
                followed.push(`edge-${n.id}-right`);
                snapshot(`node.right ← remove(node.right, ${successor.value}): Der alte Knoten von ${successor.value} muss noch weg; dieselbe Funktion sucht ihn im rechten Teilbaum.`,
                    { focus: n.right.id, incoming: successor.value, vars: [...nodeVar(n.right), ...extra], code: L(10), stack: stackOf([...calls, call('remove', n.right, successor.value)]) });
                target = successor.value;
                parent = n;
                side = 'right';
                n = n.right;
            }
            snapshot('return node: Jeder Aufruf darüber gibt seinen Knoten zurück, und der Aufrufer trägt ihn wieder in seinen Zeiger ein, von unten nach oben, zuletzt root ← remove(root, value). Links stehen immer noch die kleineren, rechts die größeren Werte.',
                { code: L(11), pulses: back(followed), duration: backTime(followed), stack: returning(calls, nodes), returning: true });
        };

        for (const op of setup) if (op.type === 'insert') insert(op.value);
        if (setup.length) {
            frames.length = 0;
            snapshot('Der Baum von oben. Jetzt der Reihe nach die Operationen darüber.');
        }
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'find') find(op.value);
            else if (op.type === 'insert') insert(op.value);
            else if (op.type === 'remove') remove(op.value);
        }
        return frames;
    }

    // One rotation, as the Handbuch writes it (rotate_right(y), rotate_left(x)):
    // a step for every line, each moving one pointer. The picture follows the
    // pointers (treeLayout): a node a variable alone still holds stands beside
    // the node the rotation is about (loose: its depth). The variables point
    // at their nodes like the pointers do. memory: id -> node; returns the
    // root afterwards.
    function rotationFrames(frames, memory, rootId, top, dir, base, prefix, after) {
        const startRoot = rootId;
        const val = (id) => memory[id].value;
        let parent = null;
        let loose = 0;
        for (let v = rootId; v !== top; v = memory[v][val(top) < val(v) ? 'left' : 'right']) {
            parent = { id: v, side: val(top) < val(v) ? 'left' : 'right' };
            loose += 1;
        }
        const right = dir === 'right';
        const [p, q] = right ? ['y', 'x'] : ['x', 'y'];
        // a, b: the sides in the German sentences; A, B: in the code
        const [a, b] = right ? ['links', 'rechts'] : ['rechts', 'links'];
        const [A, B] = right ? ['left', 'right'] : ['right', 'left'];
        const child = memory[top][A];
        const middle = memory[child][B];
        const title = right ? 'rotate_right(y)' : 'rotate_left(x)';
        const lines = [`${title}:`, `    ${q} ← ${p}.${A}`, `    ${p}.${A} ← ${q}.${B}`, `    ${q}.${B} ← ${p}`, `    update the height of ${p}, then of ${q}`, `    return ${q}`];
        const clone = () => JSON.parse(JSON.stringify(memory));
        // the label of the smaller value on the left of its node, the other on
        // the right: they never get in each other's way
        const vp = { name: p, to: top, side: val(child) < val(top) ? 'right' : 'left' };
        const vq = { name: q, to: child, side: val(child) < val(top) ? 'left' : 'right' };
        const stack = [...(base.stack || []), { call: `${title.replace(/\(.\)/, `(${p}: ${val(top)})`)}` }];
        const snap = (line, message, vars) => frames.push({ ...base, graph: clone(), rootId, rootAt: startRoot, loose, vars, code: { lines, at: line }, message, stack });
        const h = (id) => (id === null ? 0 : memory[id].height);
        snap(0, `${prefix}${title.replace(/\(.\)/, `(${val(top)})`)}. ${p} ist ${val(top)}, der Knoten, um den rotiert wird.`, [vp]);
        snap(1, `${q} ← ${p}.${A}: Die Variable ${q} merkt sich ${val(child)}, das ${a === 'links' ? 'linke' : 'rechte'} Kind von ${val(top)}.`, [vp, vq]);
        memory[top][A] = middle;
        snap(2, middle !== null
            ? `${p}.${A} ← ${q}.${B}: ${val(top)} zeigt ${a} jetzt auf ${val(middle)}. Zwei Zeiger führen gerade zu ${val(middle)}, und ${val(child)} hängt nur noch an der Variablen ${q}.`
            : `${p}.${A} ← ${q}.${B}: ${q}.${B} ist leer, also zeigt ${val(top)} ${a} auf nichts mehr. ${val(child)} hängt nur noch an der Variablen ${q}.`, [vp, vq]);
        memory[child][B] = top;
        snap(3, `${q}.${B} ← ${p}: ${val(child)} zeigt ${b} auf ${val(top)}.${middle !== null ? ` Jetzt führt nur noch ein Zeiger zu ${val(middle)}, der von ${val(top)}.` : ''}`, [vp, vq]);
        memory[top].height = 1 + Math.max(h(memory[top].left), h(memory[top].right));
        memory[child].height = 1 + Math.max(h(memory[child].left), h(memory[child].right));
        snap(4, `Erst die Höhe von ${p} (${val(top)}: h${memory[top].height}), dann die von ${q} (${val(child)}: h${memory[child].height}): ${val(child)} steht jetzt über ${val(top)}, seine Höhe hängt von der neuen Höhe von ${val(top)} ab.`, [vp, vq]);
        if (parent) memory[parent.id][parent.side] = child;
        else rootId = child;
        snap(5, parent
            ? `return ${q}: Der Aufrufer trägt ${val(child)} ein. ${val(parent.id)} zeigt ${parent.side === 'left' ? 'links' : 'rechts'} jetzt auf ${val(child)} statt auf ${val(top)}.`
            : `return ${q}: Der Aufrufer trägt ${val(child)} als neue Wurzel ein, die Variable root zeigt jetzt auf ${val(child)}.`, [vp, vq]);
        const tidy = fromGraph(memory, rootId);
        fixHeights(tidy);
        Object.assign(memory, toGraph(tidy));
        frames.push({ ...base, graph: clone(), rootId, vars: [], message: `${val(child)} steht jetzt über ${val(top)}, die Höhen stimmen. ${after}` });
        return rootId;
    }

    // Where the nodes stand: the column is the place of the value in the
    // sorted order (a rotation keeps it; while a value is there twice, the
    // older node first), the row one below the lowest node that points to it.
    // A node no node points to stands in row 0 if it is the root, else in row
    // loose (during a rotation: beside the node the rotation is about).
    // id -> { x, depth }
    function treeLayout(graph, rootId, loose = 0) {
        const ids = Object.keys(graph).map(Number).sort((a, b) => graph[a].value - graph[b].value || a - b);
        const owners = new Map(ids.map((id) => [id, []]));
        for (const id of ids) for (const side of ['left', 'right']) if (graph[id][side] !== null) owners.get(graph[id][side]).push(id);
        const depth = new Map();
        const find = (id, seen) => {
            if (depth.has(id)) return depth.get(id);
            if (seen.has(id)) return 0;
            seen.add(id);
            const of = owners.get(id);
            const d = of.length ? 1 + Math.max(...of.map((o) => find(o, seen))) : id === rootId ? 0 : loose;
            depth.set(id, d);
            return d;
        };
        for (const id of ids) find(id, new Set());
        return new Map(ids.map((id, i) => [id, { x: i, depth: depth.get(id) }]));
    }

    function treeShape(frame) {
        const layout = treeLayout(frame.graph, frame.rootId, frame.loose);
        return { count: layout.size, depth: Math.max(0, ...[...layout.values()].map((p) => p.depth + 1)) };
    }

    // most: the most nodes and the deepest level of any frame, so that the
    // picture keeps its size while the tree grows and shrinks. Pointers are
    // arrows; the variables (root, node, and x and y in a rotation) are labels
    // with an arrow to their node.
    function drawTree(svg, frame, most) {
        svg.replaceChildren();
        const { graph, rootId } = frame;
        const at = treeLayout(graph, rootId, frame.loose);
        const colW = 54;
        const left = 76;
        const width = Math.max(300, most.count * colW);
        const offset = left + (width - at.size * colW) / 2;
        const sx = (x) => offset + (x + 0.5) * colW;
        // room above the root for the variable root
        const top = 40;
        const sy = (d) => top + 30 + d * 62;
        const point = (id) => { const p = at.get(id); return [sx(p.x), sy(p.depth)]; };
        const RADIUS = 18;
        const INSET = RADIUS + 2;
        // an arrow from (x1, y1) that stops at the border of the node it points
        // to; a path, so that it can bend while the nodes move (stepper_motion.js)
        const arrow = (key, ends, [x1, y1], [x2, y2], extra = {}) => {
            const d = Math.hypot(x2 - x1, y2 - y1);
            const s = d > INSET ? (d - INSET) / d : 0;
            const [ex, ey] = [x1 + (x2 - x1) * s, y1 + (y2 - y1) * s];
            return el('svg:path', { 'data-key': key, 'data-ends': ends, 'data-inset': INSET, d: `M${x1} ${y1} Q${(x1 + ex) / 2} ${(y1 + ey) / 2} ${ex} ${ey}`,
                fill: 'none', stroke: 'var(--algo-line)', 'stroke-width': 2, 'marker-end': `url(#${most.marker})`, ...extra });
        };
        svg.appendChild(el('svg:defs', {}, [el('svg:marker', { id: most.marker, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 9, markerHeight: 9,
            markerUnits: 'userSpaceOnUse', orient: 'auto-start-reverse' }, [el('svg:path', { d: 'M0 0L10 5L0 10z', fill: 'var(--algo-line)' })])]));
        // a pointer is the left or right field of its node: when it changes,
        // the arrow lets go of its node and snaps onto the new one
        for (const [id, node] of Object.entries(graph)) {
            for (const side of ['left', 'right']) {
                if (node[side] === null) continue;
                svg.appendChild(arrow(`edge-${id}-${side}`, `node-${id} node-${node[side]}`, point(Number(id)), point(node[side])));
            }
        }
        // the variables: root above the place of the root, coming in from
        // above; the others beside their node, on the side they ask for
        const rootAt = frame.rootAt !== undefined ? frame.rootAt : rootId;
        const vars = [{ name: 'root', to: rootId, at: rootAt, above: true }, ...(frame.vars || [])];
        const labels = [];
        for (const v of vars) {
            const w = 14 + 7.5 * v.name.length;
            let x;
            let y;
            if (v.above) {
                const cx = v.at === null || v.at === undefined ? left + width / 2 : point(v.at)[0];
                x = cx - w / 2;
                y = 10;
            } else {
                const [cx, cy] = point(v.to);
                x = v.side === 'right' ? cx + 40 : v.side === 'below' ? cx - w / 2 : cx - 40 - w;
                y = v.side === 'below' ? cy + 26 : cy - 9;
            }
            const centre = [x + w / 2, y + 9];
            if (v.to !== null && v.to !== undefined) svg.appendChild(arrow(`pointer-${v.name}`, `var-${v.name} node-${v.to}`, centre, point(v.to), { stroke: 'var(--algo-accent)', ...(v.rigid ? { 'data-rigid': '' } : {}) }));
            labels.push(el('svg:rect', { 'data-key': `var-${v.name}`, x, y, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            labels.push(el('svg:text', { 'data-key': `var-${v.name}-label`, x: centre[0], y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: v.name }));
        }
        // a node by its id; a new node comes from the token that carried its
        // value down the tree; a copied value flies over from the node it was
        // copied from (its label has the value in its key)
        for (const [id, { x, depth: d }] of at) {
            const { value } = graph[id];
            const fresh = frame.fresh === id;
            const fill = frame.gone === id ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))' : fresh ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            const fromToken = fresh ? { 'data-from': 'incoming-label' } : {};
            const copied = frame.copied && frame.copied.into === id ? { 'data-from': `label-${frame.copied.from}-${value}` } : {};
            // the ring stays on its node while it fades in or out
            if (frame.focus === id) svg.appendChild(el('svg:circle', { 'data-key': 'focus', 'data-follow': `node-${id}`, cx: sx(x), cy: sy(d), r: 23, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 }));
            svg.appendChild(el('svg:circle', { 'data-key': `node-${id}`, ...(fresh ? { 'data-from': 'incoming' } : {}), cx: sx(x), cy: sy(d), r: RADIUS, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': `label-${id}-${value}`, ...fromToken, ...copied, x: sx(x), y: sy(d) + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            if (frame.avl) svg.appendChild(el('svg:text', { 'data-key': `height-${id}`, ...fromToken, x: sx(x), y: sy(d) + 32, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: `h${graph[id].height}` }));
        }
        for (const label of labels) svg.appendChild(label);
        // a light that runs down a pointer once while the step moves
        (frame.pulses || []).forEach((pulse, i) => svg.appendChild(el('svg:circle', { 'data-key': `pulse-${i}`, 'data-pulse': pulse.edge, 'data-delay': pulse.delay || 0, 'data-length': pulse.length || 0.4,
            cx: 0, cy: 0, r: 5, opacity: 0, fill: 'var(--algo-accent)' })));
        // the value on its way: a token beside the node it is compared with
        const near = frame.incoming !== undefined && at.get(frame.focus);
        if (near) {
            // well clear of the node and its focus ring (radius 23), and of
            // the edges below it
            const cx = sx(near.x) + 58;
            const cy = sy(near.depth) - 8;
            const to = frame.incomingTo !== undefined ? { 'data-to': `node-${frame.incomingTo}` } : {};
            const toLabel = frame.incomingTo !== undefined ? { 'data-to': `label-${frame.incomingTo}-${frame.incoming}` } : {};
            svg.appendChild(el('svg:circle', { 'data-key': 'incoming', ...to, cx, cy, r: 13, fill: 'var(--algo-frontier)', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '4 3' }));
            svg.appendChild(el('svg:text', { 'data-key': 'incoming-label', ...toLabel, x: cx, y: cy + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.incoming) }));
        }
        // room on the left for the labels, on the right for the token
        svg.setAttribute('viewBox', `0 0 ${left + width + 50} ${top + Math.max(100, most.depth * 62 + (frame.avl ? 8 : 0))}`);
    }

    // ---- Hash Table with 8 buckets, h(k) = k mod 8: chaining (a list per
    // bucket) or linear probing (the next free bucket).
    function traceHash(ops, mode, size) {
        const table = Array.from({ length: size }, () => (mode === 'chain' ? [] : null));
        const frames = [];
        const count = () => (mode === 'chain' ? table.reduce((s, b) => s + b.length, 0) : table.filter((x) => x !== null).length);
        const snapshot = (message, marks = {}) => frames.push({ table: table.map((b) => (Array.isArray(b) ? [...b] : b)), mode, message, load: count() / size, ...marks });
        snapshot(`Eine leere Hash Table mit ${size} Buckets. Der Hash eines Schlüssels k ist k mod ${size}.`);
        for (const op of ops) {
            const k = op.value;
            const h = ((k % size) + size) % size;
            if (op.type === 'insert') {
                if (mode === 'chain') {
                    table[h].push(k);
                    snapshot(`insert ${k}: ${k} mod ${size} = ${h}.${table[h].length > 1 ? ` Bucket ${h} ist schon belegt, eine Kollision: ${k} kommt hinten an seine Liste.` : ` ${k} kommt in Bucket ${h}.`}`, { focus: h, probe: [h] });
                } else {
                    const probe = [];
                    let i = h;
                    while (table[i] !== null && probe.length < size) { probe.push(i); snapshot(`insert ${k}: ${k} mod ${size} = ${h}. Bucket ${i} ist belegt, weiter zum nächsten.`, { probe: [...probe] }); i = (i + 1) % size; }
                    if (probe.length === size) { snapshot(`insert ${k}: Alle Buckets sind belegt.`, { error: true }); continue; }
                    table[i] = k;
                    probe.push(i);
                    snapshot(`insert ${k}: ${i === h ? `${k} mod ${size} = ${h}, frei.` : `Bucket ${i} ist frei.`} ${k} kommt in Bucket ${i}.`, { probe, focus: i });
                }
            } else if (op.type === 'find') {
                if (mode === 'chain') {
                    const found = table[h].includes(k);
                    snapshot(`find ${k}: nur Bucket ${h} ansehen. ${found ? `${k} steht in seiner Liste.` : `${k} steht nicht darin, also nirgends.`}`, { focus: h, probe: [h] });
                } else {
                    const probe = [];
                    let i = h;
                    while (table[i] !== null && table[i] !== k && probe.length < size) { probe.push(i); i = (i + 1) % size; }
                    probe.push(i);
                    snapshot(`find ${k}: ab Bucket ${h} suchen, bis ${k} kommt oder ein leerer Bucket. ${table[i] === k ? `Gefunden in Bucket ${i}, nach ${probe.length} Buckets.` : `Bucket ${i} ist leer: ${k} ist nicht da.`}`, { probe, focus: i });
                }
            }
        }
        return frames;
    }

    function drawHash(svg, frame) {
        svg.replaceChildren();
        const size = frame.table.length;
        const rowH = 38;
        const probe = frame.probe || [];
        frame.table.forEach((bucket, i) => {
            const y = 10 + i * rowH;
            const inProbe = probe.includes(i);
            svg.appendChild(el('svg:text', { x: 8, y: y + 23, 'font-size': 12, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(el('svg:rect', { x: 30, y, width: 60, height: rowH - 6, rx: 5, fill: frame.focus === i ? 'var(--algo-frontier)' : inProbe ? 'color-mix(in srgb, var(--algo-accent) 18%, transparent)' : 'var(--algo-paper)', stroke: inProbe ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': inProbe ? 2.5 : 1.5 }));
            if (frame.mode === 'chain') {
                bucket.forEach((k, j) => {
                    const x = 110 + j * 70;
                    svg.appendChild(el('svg:path', { d: `M${x - (j ? 14 : 20)} ${y + 16} l${j ? 10 : 16} 0 m-5 -4 l5 4 l-5 4`, stroke: 'var(--algo-ink)', 'stroke-width': 1.5, fill: 'none' }));
                    svg.appendChild(el('svg:rect', { x, y: y + 2, width: 50, height: rowH - 10, rx: 5, fill: 'var(--algo-paper)', stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
                    svg.appendChild(el('svg:text', { x: x + 25, y: y + 21, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(k) }));
                });
            } else if (bucket !== null) {
                svg.appendChild(el('svg:text', { x: 60, y: y + 22, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(bucket) }));
            }
        });
        svg.setAttribute('viewBox', `0 0 420 ${20 + size * rowH}`);
    }

    let widgetCount = 0;

    // The pseudocode's tokens, as Rouge::Lexers::Pseudocode colours them on
    // the pages (pseudocode_lexer.rb): the same classes, the same colours
    const KEYWORDS = new Set(['if', 'else', 'while', 'for', 'each', 'in', 'of', 'return', 'not', 'and', 'or', 'repeat', 'until', 'break', 'continue', 'do', 'then', 'to', 'from', 'mod', 'div']);
    const CONSTANTS = new Set(['empty', 'none', 'null', 'true', 'false', 'found', 'done', 'full', 'error']);
    const PSEUDOCODE = /(\((?=[^()\n]*\s[^()\n]*\s)[^()\n+\-*/=<>≤≥≠[\]]*\))|(←|≤|≥|≠|<|>|=|\+|-|\*|\/|%)|(\d+(?:\.\d+)?)|(\.)([\p{L}_][\p{L}\p{N}_]*)|(not found\b)|([\p{L}_][\p{L}\p{N}_]*)(\()?|([()[\]{},:])/gu;
    function highlightPseudocode(line) {
        const out = [];
        let last = 0;
        const span = (cls, text) => out.push(el('span', { class: cls, text }));
        for (const m of line.matchAll(PSEUDOCODE)) {
            if (m.index > last) out.push(document.createTextNode(line.slice(last, m.index)));
            last = m.index + m[0].length;
            if (m[1]) span('c1', m[1]);
            else if (m[2]) span('o', m[2]);
            else if (m[3]) span('mi', m[3]);
            else if (m[4]) { span('p', m[4]); span('na', m[5]); }
            else if (m[6]) span('kc', m[6]);
            else if (m[7]) {
                const word = m[7];
                span(m[8] ? (KEYWORDS.has(word) ? 'k' : 'nf') : KEYWORDS.has(word) ? 'k' : CONSTANTS.has(word) ? 'kc' : 'n', word);
                if (m[8]) span('p', m[8]);
            } else span('p', m[9]);
        }
        if (last < line.length) out.push(document.createTextNode(line.slice(last)));
        return out;
    }

    function setUp(widget) {
        const algo = widget.dataset.algo;
        let frames;
        let draw;
        // Every step is drawn into svg, which is never shown; picture, the
        // visible one, moves to it (stepper_motion.js)
        const picture = el('svg:svg', { class: 'algo-picture', role: 'img' });
        const svg = el('svg:svg');
        const side = el('div', { class: 'algo-side' });
        // »insert(1, 3), remove(0)« or »push 5, pop«: commas inside brackets stay
        const parseOps = (text) => text.split(/,(?![^(]*\))/).map((t) => t.trim()).filter(Boolean).map((t) => {
            const m = t.match(/^(\w+)\s*\(?\s*(-?\d+)?\s*,?\s*(-?\d+)?\s*\)?$/);
            const [, type, first, second] = m || [];
            return second !== undefined ? { type, index: Number(first), value: Number(second) } : { type, value: Number(first), index: Number(first) };
        });
        const sideText = (title, text) => side.replaceChildren(el('h5', { text: title }), el('div', { class: 'algo-items' }, [text === undefined
            ? el('span', { class: 'algo-empty', text: '–' }) : el('span', { class: 'algo-item next', text: String(text) })]));
        if (algo === 'ring') {
            const capacity = Number(widget.dataset.capacity || 6);
            frames = traceRing(parseOps(widget.dataset.ops), capacity);
            svg.setAttribute('aria-label', 'Die Zellen des Arrays mit head und der nächsten freien Zelle');
            draw = (frame) => { drawRing(svg, frame); sideText('Zuletzt mit dequeue heraus', frame.out); };
        } else if (algo === 'array') {
            frames = traceArray(parseOps(widget.dataset.ops), Number(widget.dataset.capacity || 4));
            const most = Math.max(...frames.map((f) => f.cells.length));
            svg.setAttribute('aria-label', 'Die Zellen des Arrays, die ersten size davon belegt');
            draw = (frame) => { drawArray(svg, frame, most); sideText('Verschoben und kopiert', frame.moves); };
        } else if (algo === 'linked') {
            frames = traceLinked(parseOps(widget.dataset.ops));
            const most = Math.max(...frames.map((f) => f.nodes.length));
            svg.setAttribute('aria-label', 'Die Knoten der Linked List mit ihren next-Referenzen');
            side.hidden = true;
            draw = (frame) => drawLinked(svg, frame, most);
        } else if (algo === 'bst' || algo === 'avl') {
            frames = traceTree(parseOps(widget.dataset.ops), algo === 'avl', parseOps(widget.dataset.setup || ''));
            svg.setAttribute('aria-label', 'Der Baum: links die kleineren, rechts die größeren Werte');
            const shapes = frames.map(treeShape);
            const most = { count: Math.max(...shapes.map((t) => t.count)), depth: Math.max(...shapes.map((t) => t.depth)), marker: `algo-arrow-${++widgetCount}` };
            // beside the tree the pseudocode of what runs, its line marked
            draw = (frame) => {
                drawTree(svg, frame, most);
                // a long line wraps, its continuation two characters further in
                // than the line itself
                const codeLine = (line, i) => {
                    const indent = line.match(/^ */)[0].length;
                    return el('span', { class: `line${i === frame.code.at ? ' current' : ''}`, style: `padding-left: calc(0.6em + ${indent + 2}ch); text-indent: -2ch;` }, highlightPseudocode(line.trimStart()));
                };
                // the calls that run, each one deeper indented; on the way
                // back up they return one after another, the deepest first
                const stack = frame.stack || [];
                const calls = stack.length ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack' }, stack.map((entry, i) => {
                    const li = el('li', { class: !frame.returning && i === stack.length - 1 ? 'current' : '', style: `margin-left: ${i * 1.2}em;`,
                        text: entry.returns ? `${entry.call} → ${entry.returns}` : entry.call });
                    if (frame.returning) li.style.animationDelay = `${((stack.length - 1 - i) / (stack.length + 1)) * (frame.duration || MOTION_TIME) / 1000}s`;
                    if (frame.returning) li.classList.add('returning');
                    return li;
                }))] : [];
                side.replaceChildren(...calls, el('h5', { text: 'Pseudocode' }), frame.code
                    ? el('pre', { class: 'algo-code code-pseudocode' }, frame.code.lines.map(codeLine))
                    : el('p', { class: 'algo-empty', text: 'Hier steht der Pseudocode der Operation, die gerade läuft, und ihre Zeile ist markiert.' }));
            };
        } else if (algo === 'hash') {
            const mode = widget.dataset.mode || 'chain';
            frames = traceHash(parseOps(widget.dataset.ops), mode, Number(widget.dataset.size || 8));
            svg.setAttribute('aria-label', 'Die Buckets der Hash Table mit ihren Schlüsseln');
            draw = (frame) => {
                drawHash(svg, frame);
                side.replaceChildren(el('h5', { text: 'Füllgrad' }), el('div', { class: 'algo-items' }, [el('span', { class: 'algo-item', text: `${Math.round(frame.load * 100)} %` })]));
            };
        } else if (algo === 'sort') {
            const values = widget.dataset.values.split(',').map(Number);
            const kind = widget.dataset.sort;
            frames = traceSort(values, kind);
            // the picture keeps its height when the scratch row comes and goes
            const scratch = kind === 'merge';
            for (const frame of frames) frame.scratchSpace = scratch;
            svg.setAttribute('aria-label', 'Die Zahlen als Balken, Höhe nach Wert, darunter die Indizes');
            draw = (frame) => {
                drawSort(svg, frame);
                side.replaceChildren(el('h5', { text: 'Gezählt' }),
                    el('table', { class: 'algo-table' }, [
                        el('tr', {}, [el('th', { text: 'Vergleiche' }), el('td', { text: String(frame.comparisons) })]),
                        el('tr', {}, [el('th', { text: 'Schreibzugriffe' }), el('td', { text: String(frame.writes) })]),
                    ]));
            };
        } else if (algo === 'heap') {
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
        picture.setAttribute('aria-label', svg.getAttribute('aria-label'));
        // the operations in their order: done, the current one, what comes
        const opsText = widget.dataset.ops && frames.some((f) => f.op !== undefined)
            ? widget.dataset.ops.split(/,(?![^(]*\))/).map((s) => s.trim()).filter(Boolean) : null;
        const strip = opsText && el('ol', { class: 'algo-ops', 'aria-label': 'Die Operationen der Reihe nach' }, opsText.map((text) => el('li', { text })));
        widget.replaceChildren(el('div', { class: 'algo-controls' }, [...buttons, count]), ...(strip ? [strip] : []), el('div', { class: 'algo-body' }, [picture, side]), message);
        widget.setAttribute('data-stepper', '');
        let index = 0;
        // animate: a step forward or back moves; a jump shows the step at once
        const show = (animate) => {
            const frame = frames[index];
            draw(frame);
            if (window.StepperMotion) window.StepperMotion.morph(picture, svg, { duration: animate ? frame.duration || MOTION_TIME : 0 });
            else {
                picture.setAttribute('viewBox', svg.getAttribute('viewBox'));
                picture.replaceChildren(...svg.childNodes);
            }
            message.textContent = frame.message;
            if (strip) [...strip.children].forEach((li, i) => {
                const done = i < frame.op || (i === frame.op && index === frames.length - 1);
                li.className = done ? 'done' : i === frame.op ? 'current' : '';
                if (i === frame.op && !done) li.setAttribute('aria-current', 'step');
                else li.removeAttribute('aria-current');
            });
            count.textContent = `Schritt ${index} von ${frames.length - 1}`;
            buttons[0].disabled = buttons[1].disabled = index === 0;
            buttons[2].disabled = buttons[3].disabled = index === frames.length - 1;
        };
        const go = { first: () => 0, prev: () => index - 1, next: () => index + 1, last: () => frames.length - 1 };
        for (const b of buttons) b.addEventListener('click', () => {
            const before = index;
            index = Math.max(0, Math.min(frames.length - 1, go[b.dataset.go]()));
            show(Math.abs(index - before) === 1);
        });
        show(false);
    }

    function start() {
        addStyle();
        document.querySelectorAll('.algo-stepper').forEach(setUp);
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
