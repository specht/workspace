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
.algo-stepper .algo-body { position: relative; }
/* the lines from the calls to their nodes lie behind the picture and the code */
.algo-stepper svg.algo-links { position: absolute; inset: 0; width: 100%; height: 100%; pointer-events: none; overflow: visible; z-index: 0; }
.algo-stepper .algo-body > svg.algo-picture, .algo-stepper .algo-body > .algo-side { position: relative; z-index: 1; }
.algo-stepper .algo-timeline { display: flex; margin: 0 0 0.8em; cursor: pointer; touch-action: none; user-select: none; }
/* a dot for every step; the dot where an operation starts is ringed and
   carries the operation's name */
.algo-stepper .algo-section { min-width: 0; display: flex; flex-direction: column; }
.algo-stepper .algo-timeline-box { margin: 0 0 0.8em; }
.algo-stepper .algo-timeline { margin: 0; }
.algo-stepper .algo-op-names { position: relative; font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.72em; }
.algo-stepper .algo-op-name { position: absolute; white-space: nowrap;
    color: var(--bs-secondary-color); line-height: 1.35; }
.algo-stepper .algo-op-name.current { color: var(--algo-ink); font-weight: 600; }
.algo-stepper .algo-ticks { display: flex; height: 16px; }
.algo-stepper .algo-tick { flex: 1 1 0; min-width: 3px; position: relative; }
/* a fine line from each dot to the next, coloured where the steps are done */
.algo-stepper .algo-tick::after { content: ''; position: absolute; left: 50%; top: 50%; width: 100%; height: 1px; z-index: 0;
    background: color-mix(in srgb, var(--algo-line) 60%, transparent); transition: background-color 0.2s; }
.algo-stepper .algo-tick.done::after { background: color-mix(in srgb, var(--algo-accent) 55%, transparent); }
.algo-stepper .algo-tick:last-child:is(.algo-section:last-child *)::after { display: none; }
.algo-stepper .algo-tick::before { content: ''; position: absolute; left: 50%; top: 50%; width: 6px; height: 6px; transform: translate(-50%, -50%); border-radius: 50%; z-index: 1;
    background: color-mix(in srgb, var(--algo-line) 60%, transparent); transition: width 0.2s, height 0.2s, background-color 0.2s; }
.algo-stepper .algo-tick.start::before { width: 9px; height: 9px; background: var(--algo-paper); box-shadow: 0 0 0 2px var(--algo-accent); }
.algo-stepper .algo-tick.done::before { background: color-mix(in srgb, var(--algo-accent) 55%, transparent); }
.algo-stepper .algo-tick.start.done::before { background: color-mix(in srgb, var(--algo-accent) 55%, var(--algo-paper)); }
.algo-stepper .algo-tick.current::before { width: 12px; height: 12px; background: var(--algo-accent); box-shadow: 0 0 0 2px var(--algo-paper), 0 0 0 3.5px var(--algo-accent); }
@media (pointer: coarse) { .algo-stepper .algo-ticks { height: 24px; } .algo-stepper .algo-tick.current::before { width: 14px; height: 14px; } }
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

    // ---- The heap: an array; the children of index i are 2i + 1 and 2i + 2.
    // Every step is a line of the Handbuch's pseudocode (HEAP_CODE,
    // reference/heap). An entry has an id, so that a swap moves the two
    // entries, in the tree and in the array at once. A frame: slots (the ids
    // in array order), values (id -> value), aside (entries a variable holds
    // outside the heap: top, last), vars (index variables: i, parent,
    // min, child), compare (indices), at (the index i), code, message,
    // out (what pop gave so far).
    const HEAP_CODE = {
        push: ['push(heap, entry):', '    append entry to the back of heap', '    i ← the last index of heap', '    while i > 0:',
            '        parent ← (i - 1) / 2, rounded down', '        if heap[parent] ≤ heap[i]: done', '        swap heap[parent] and heap[i]', '        i ← parent'],
        pop: ['pop(heap):', '    if heap empty: nothing to pop', '    top ← heap[0]', '    last ← take the last entry from heap', '    if heap not empty:',
            '        heap[0] ← last', '        i ← 0', '        repeat:', '            min ← i', '            for each child in (2i + 1, 2i + 2):',
            '                if child < length of heap and heap[child] < heap[min]:', '                    min ← child',
            '            if min = i: done', '            swap heap[min] and heap[i]', '            i ← min', '    return top'],
    };

    function traceHeap(ops, setup = []) {
        const slots = [];
        const values = {};
        let nextId = 0;
        let out = [];
        let current = -1;
        const frames = [];
        let aside = {};
        const snapshot = (message, marks = {}) => frames.push({ slots: [...slots], values: { ...values }, aside: { ...aside }, out: [...out], message, op: current, ...marks });
        const code = (name, at) => ({ lines: HEAP_CODE[name], at });
        const v = (i) => values[slots[i]];
        const push = (value, quiet) => {
            const id = nextId++;
            values[id] = value;
            slots.push(id);
            if (quiet) {
                for (let i = slots.length - 1; i > 0 && v(Math.floor((i - 1) / 2)) > v(i); i = Math.floor((i - 1) / 2)) {
                    const p = Math.floor((i - 1) / 2);
                    [slots[p], slots[i]] = [slots[i], slots[p]];
                }
                return;
            }
            snapshot(`push ${value}: ${value} kommt hinten an das Array, an Index ${slots.length - 1}, die erste freie Stelle des Baums.`, { fresh: id, code: code('push', 1) });
            let i = slots.length - 1;
            snapshot(`i ← ${i}, der letzte Index.`, { at: i, vars: [{ name: 'i', index: i }], code: code('push', 2) });
            for (;;) {
                if (i === 0) {
                    snapshot(`while i > 0: i ist 0, ${v(0)} ist oben angekommen. Fertig.`, { at: i, vars: [{ name: 'i', index: i }], code: code('push', 3) });
                    break;
                }
                snapshot(`while i > 0: i ist ${i}, weiter.`, { at: i, vars: [{ name: 'i', index: i }], code: code('push', 3) });
                const parent = Math.floor((i - 1) / 2);
                const both = [{ name: 'i', index: i }, { name: 'parent', index: parent }];
                snapshot(`parent ← (${i} - 1) / 2, abgerundet: ${parent}.`, { at: i, vars: both, code: code('push', 4) });
                if (v(parent) <= v(i)) {
                    snapshot(`heap[parent] ≤ heap[i]: ${v(parent)} ≤ ${v(i)}, der Elternteil ist nicht größer. Fertig.`, { at: i, vars: both, compare: [parent, i], code: code('push', 5) });
                    break;
                }
                snapshot(`heap[parent] ≤ heap[i]? ${v(parent)} ist größer als ${v(i)}, also nicht fertig.`, { at: i, vars: both, compare: [parent, i], code: code('push', 5) });
                [slots[parent], slots[i]] = [slots[i], slots[parent]];
                snapshot(`swap heap[parent] and heap[i]: ${v(parent)} und ${v(i)} tauschen die Plätze.`, { at: i, vars: both, compare: [parent, i], swap: true, code: code('push', 6) });
                i = parent;
                snapshot(`i ← parent: i ist jetzt ${i}.`, { at: i, vars: [{ name: 'i', index: i }, { name: 'parent', index: parent }], code: code('push', 7) });
            }
        };
        const pop = () => {
            const P = (at) => code('pop', at);
            if (!slots.length) {
                snapshot('if heap empty: Der Heap ist leer, es kommt nichts heraus.', { code: P(1) });
                return;
            }
            const top = slots[0];
            snapshot(`top ← heap[0]: Die Variable top hält ${values[top]}, den kleinsten Eintrag.`, { holders: [{ name: 'top', id: top }], code: P(2) });
            const last = slots.pop();
            if (last === top) {
                aside = { [top]: 'top' };
                snapshot(`last ← take the last entry: Das ist ${values[top]} selbst, der Heap ist jetzt leer.`, { holders: [{ name: 'top', id: top }], code: P(3) });
            } else {
                aside = { [last]: 'last' };
                snapshot(`last ← take the last entry from heap: ${values[last]} kommt hinten aus dem Array, das Array ist eins kürzer.`,
                    { holders: [{ name: 'top', id: top }, { name: 'last', id: last }], code: P(3) });
                snapshot('if heap not empty: Es sind noch Einträge da.', { holders: [{ name: 'top', id: top }, { name: 'last', id: last }], code: P(4) });
                slots[0] = last;
                aside = { [top]: 'top' };
                snapshot(`heap[0] ← last: ${values[last]} kommt an die Spitze, an Index 0. ${values[top]} hängt jetzt nur noch an der Variablen top.`,
                    { holders: [{ name: 'top', id: top }], at: 0, code: P(5) });
                let i = 0;
                const holders = [{ name: 'top', id: top }];
                snapshot('i ← 0.', { holders, at: i, vars: [{ name: 'i', index: i }], code: P(6) });
                for (;;) {
                    let min = i;
                    const vars = () => [{ name: 'i', index: i }, { name: 'min', index: min }];
                    snapshot(`min ← i: min ist ${i}.`, { holders, at: i, vars: vars(), code: P(8) });
                    for (const child of [2 * i + 1, 2 * i + 2]) {
                        const withChild = () => [...vars(), { name: 'child', index: child }];
                        if (child >= slots.length) {
                            snapshot(`child ${child}: Index ${child} gibt es nicht, der Heap hat nur ${slots.length} Einträge.`, { holders, at: i, vars: vars(), code: P(10) });
                            continue;
                        }
                        if (v(child) < v(min)) {
                            snapshot(`child ${child}: ${v(child)} < ${v(min)}, also min ← ${child}.`, { holders, at: i, vars: withChild(), compare: [min, child], code: P(11) });
                            min = child;
                        } else {
                            snapshot(`child ${child}: ${v(child)} ist nicht kleiner als ${v(min)}, min bleibt ${min}.`, { holders, at: i, vars: withChild(), compare: [min, child], code: P(10) });
                        }
                    }
                    if (min === i) {
                        snapshot(`min = i: Kein Kind ist kleiner als ${v(i)}. Fertig.`, { holders, at: i, vars: vars(), code: P(12) });
                        break;
                    }
                    [slots[min], slots[i]] = [slots[i], slots[min]];
                    snapshot(`swap heap[min] and heap[i]: ${v(min)} und ${v(i)} tauschen die Plätze.`, { holders, at: i, vars: vars(), compare: [i, min], swap: true, code: P(13) });
                    i = min;
                    snapshot(`i ← min: i ist jetzt ${i}.`, { holders, at: i, vars: [{ name: 'i', index: i }, { name: 'min', index: min }], code: P(14) });
                }
            }
            aside = {};
            out = [...out, values[top]];
            delete values[top];
            snapshot(`return top: ${out[out.length - 1]} kommt heraus, der kleinste Eintrag. Oben steht jetzt der nächstkleinste.`, { code: P(15) });
        };
        for (const op of setup) if (op.type === 'push') push(op.value, true);
        snapshot(setup.length ? 'Der Heap von oben. Jetzt der Reihe nach die Operationen darüber.' : 'Ein leerer Heap.');
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'push') push(op.value);
            else if (op.type === 'pop') pop();
        }
        return frames;
    }

    // The heap twice, the tree above and the array below, an entry in both
    // as a group that moves (a swap on a small arc, so that the two pass each
    // other). The index variables are labels under the array with an arrow up
    // to their cell; top and last hold an entry beside the tree.
    function drawHeap(svg, frame, most, marker) {
        svg.replaceChildren();
        const n = frame.slots.length;
        // as high as the biggest heap of the example, so the picture keeps its size
        const levels = Math.max(1, Math.ceil(Math.log2(most + 1)));
        const width = 560;
        const pos = (i) => {
            const level = Math.floor(Math.log2(i + 1));
            const first = 2 ** level - 1;
            const count = 2 ** level;
            return [70 + ((i - first + 0.5) / count) * (width - 140), 40 + level * 62];
        };
        const compare = new Set(frame.compare || []);
        const arc = frame.swap ? { 'data-arc': 22 } : {};
        svg.appendChild(arrowheads(marker));
        // the tree's edges belong to the places, not to the entries
        for (let i = 1; i < n; i++) {
            const [x1, y1] = pos(Math.floor((i - 1) / 2));
            const [x2, y2] = pos(i);
            svg.appendChild(el('svg:line', { 'data-key': `slot-edge-${i}`, x1, y1, x2, y2, stroke: 'var(--algo-line)', 'stroke-width': 2 }));
        }
        const fill = (id, i) => (frame.fresh === id ? 'var(--algo-frontier)' : i !== undefined && compare.has(i) ? 'var(--algo-frontier)' : 'var(--algo-paper)');
        const entry = (id, x, y, i) => el('svg:g', { 'data-key': `tree-${id}`, transform: `translate(${x} ${y})`, ...arc }, [
            el('svg:circle', { cx: 0, cy: 0, r: 17, fill: fill(id, i), stroke: i === frame.at ? 'var(--algo-accent)' : 'var(--algo-ink)', 'stroke-width': i === frame.at ? 3 : 1.5 }),
            el('svg:text', { x: 0, y: 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.values[id]) }),
        ]);
        for (let i = 0; i < n; i++) {
            const [x, y] = pos(i);
            svg.appendChild(el('svg:text', { 'data-key': `slot-index-${i}`, x: x + 21, y: y - 14, 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(entry(frame.slots[i], x, y, i));
        }
        // entries a variable holds outside the heap: top on the left, last on the right
        const [rootX, rootY] = pos(0);
        const asidePlace = { top: [rootX - 150, rootY + 20], last: [rootX + 150, rootY + 20] };
        for (const [id, name] of Object.entries(frame.aside)) {
            const [x, y] = asidePlace[name];
            svg.appendChild(entry(Number(id), x, y));
        }
        // the array below the tree
        const top = 40 + levels * 62 + 6;
        const cell = Math.min(46, (width - 20) / Math.max(most, 1));
        const cellX = (i) => 10 + i * cell;
        for (let i = 0; i < n; i++) {
            svg.appendChild(el('svg:rect', { 'data-key': `slot-${i}`, x: cellX(i), y: top + 22, width: cell - 3, height: 4, fill: 'none', stroke: 'none' }));
            svg.appendChild(el('svg:text', { 'data-key': `cell-index-${i}`, x: cellX(i) + (cell - 3) / 2, y: top + 44, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
        }
        for (let i = 0; i < n; i++) {
            const id = frame.slots[i];
            svg.appendChild(el('svg:g', { 'data-key': `cell-${id}`, 'data-from': `tree-${id}`, 'data-to': `tree-${id}`, transform: `translate(${cellX(i)} ${top})`, ...arc }, [
                el('svg:rect', { x: 0, y: 0, width: cell - 3, height: 30, rx: 4, fill: fill(id, i), stroke: i === frame.at ? 'var(--algo-accent)' : compare.has(i) ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': i === frame.at || compare.has(i) ? 3 : 1.5 }),
                el('svg:text', { x: (cell - 3) / 2, y: 20, 'text-anchor': 'middle', 'font-size': 14, fill: 'var(--algo-ink)', text: String(frame.values[id]) }),
            ]));
        }
        // the variables
        const label = (name, x, y, target, inset, side) => {
            const w = 14 + 7.5 * name.length;
            const lx = side === 'left' ? x - w : side === 'right' ? x : x - w / 2;
            const centre = [lx + w / 2, y + 9];
            const out = [];
            if (target) {
                const [tx, ty] = target.at;
                const d = Math.hypot(tx - centre[0], ty - centre[1]);
                const s = d > inset ? (d - inset) / d : 0;
                const [ex, ey] = [centre[0] + (tx - centre[0]) * s, centre[1] + (ty - centre[1]) * s];
                out.push(el('svg:path', { 'data-key': `pointer-${name}`, 'data-ends': `var-${name} ${target.key}`, 'data-inset': inset, 'data-rigid': '',
                    d: `M${centre[0]} ${centre[1]} Q${(centre[0] + ex) / 2} ${(centre[1] + ey) / 2} ${ex} ${ey}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'marker-end': `url(#${marker}-accent)` }));
            }
            out.push(el('svg:rect', { 'data-solid': '', 'data-key': `var-${name}`, x: lx, y, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            out.push(el('svg:text', { 'data-key': `var-${name}-label`, x: centre[0], y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: name }));
            return out;
        };
        // index variables under their cell, one label for each cell (indexLabels)
        const labels = [];
        for (const placed of indexLabels(frame.vars, (index) => cellX(index) + (cell - 3) / 2, width)) labels.push(...indexLabel(placed, top + 58 + placed.row * 24, top + 24, top + 50, marker));
        for (const h of frame.holders || []) {
            const place = frame.aside[h.id] ? asidePlace[frame.aside[h.id]] : pos(frame.slots.indexOf(h.id));
            labels.push(...label(h.name, place[0] - 26, place[1] - 40, null, 0, 'left'));
        }
        // the arrows of top and last go to the entry they hold (its circle is in a group)
        for (const node of labels) svg.appendChild(node);
        for (const h of frame.holders || []) {
            const place = frame.aside[h.id] ? asidePlace[frame.aside[h.id]] : pos(frame.slots.indexOf(h.id));
            const w = 14 + 7.5 * h.name.length;
            const from = [place[0] - 26 - w / 2, place[1] - 31];
            const d = Math.hypot(place[0] - from[0], place[1] - from[1]);
            const s = (d - 19) / d;
            const end = [from[0] + (place[0] - from[0]) * s, from[1] + (place[1] - from[1]) * s];
            svg.insertBefore(el('svg:path', { 'data-key': `pointer-${h.name}`, 'data-ends': `var-${h.name} anchor-${h.id}`, 'data-inset': 19, 'data-rigid': '',
                d: `M${from[0]} ${from[1]} Q${(from[0] + end[0]) / 2} ${(from[1] + end[1]) / 2} ${end[0]} ${end[1]}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'marker-end': `url(#${marker}-accent)` }),
            svg.querySelector(`[data-key="var-${h.name}"]`));
            svg.appendChild(el('svg:circle', { 'data-key': `anchor-${h.id}`, cx: place[0], cy: place[1], r: 0, fill: 'none' }));
        }
        svg.setAttribute('viewBox', `0 0 ${width} ${top + 56 + 3 * 24}`);
    }

    // ---- Sorting: every step is a line of the Handbuch's pseudocode
    // (SORT_CODE, reference/sortieren), comparisons and writes counted like
    // the Sorting Sandbox counts them. A frame: slots (ids in list order),
    // values (id -> value), lifted (insertion sort: the copy current holds,
    // above the place j), scratch (merge sort: ids or null), vars (index
    // variables: { name, index }), compare, range [start, stop), pivot,
    // boundary, sortedTo / sortedFrom, swap (the two move on arcs), from (id ->
    // the bar a copy comes from), stack (merge and quick sort), code. A copy is
    // a new id: for a moment the value is there twice.
    const SORT_CODE = {
        selection: ['selection_sort(list):', '    for i from 0 to length - 2:', '        min ← i', '        for j from i + 1 to length - 1:',
            '            if list[j] < list[min]: min ← j', '        swap list[i] and list[min]'],
        insertion: ['insertion_sort(list):', '    for i from 1 to length - 1:', '        current ← list[i]', '        j ← i',
            '        while j > 0 and list[j - 1] > current:', '            list[j] ← list[j - 1]        (eins nach rechts rücken)', '            j ← j - 1',
            '        list[j] ← current'],
        bubble: ['bubble_sort(list):', '    for end from length - 1 down to 1:', '        swapped ← false', '        for i from 0 to end - 1:',
            '            if list[i] > list[i + 1]:', '                swap list[i] and list[i + 1]', '                swapped ← true', '        if not swapped: done'],
        merge: ['merge_sort(list, start, stop):          (stop gehört nicht mehr dazu)', '    if stop - start < 2: done', '    middle ← (start + stop) / 2, rounded down',
            '    merge_sort(list, start, middle)', '    merge_sort(list, middle, stop)', '    i ← start, j ← middle', '    for k from start to stop - 1:',
            '        if i < middle and (j ≥ stop or list[i] ≤ list[j]):', '            scratch[k] ← list[i], i ← i + 1', '        else:',
            '            scratch[k] ← list[j], j ← j + 1', '    copy scratch[start … stop - 1] back to list'],
        quick: ['quick_sort(list, first, last):          (last gehört dazu)', '    if last ≤ first: done', '    pivot ← list[last]', '    boundary ← first',
            '    for i from first to last - 1:', '        if list[i] < pivot:', '            swap list[i] and list[boundary]', '            boundary ← boundary + 1',
            '    swap list[boundary] and list[last]     (das Pivot an seinen Platz)', '    quick_sort(list, first, boundary - 1)', '    quick_sort(list, boundary + 1, last)'],
    };

    function traceSort(start, kind) {
        const n = start.length;
        const values = {};
        const slots = start.map((value, i) => { values[i] = value; return i; });
        let nextId = n;
        let lifted = null;
        let scratch = kind === 'merge' ? Array(n).fill(null) : null;
        let comparisons = 0;
        let writes = 0;
        const stack = [];
        const frames = [];
        const v = (i) => values[slots[i]];
        const snapshot = (message, marks = {}) => frames.push({ slots: [...slots], values: { ...values }, lifted: lifted && { ...lifted }, scratch: scratch && [...scratch],
            stack: stack.map((call) => ({ call })), comparisons, writes, message, code: { lines: SORT_CODE[kind], at: marks.line }, ...marks });
        const swap = (a, b) => { [slots[a], slots[b]] = [slots[b], slots[a]]; writes += 2; };
        const copyTo = (index, fromId) => {
            const id = nextId++;
            values[id] = values[fromId];
            slots[index] = id;
            writes += 1;
            return id;
        };
        snapshot(`${n} Zahlen, unsortiert.`, { line: 0 });
        if (kind === 'selection') {
            for (let i = 0; i < n - 1; i++) {
                snapshot(`for i: i ist ${i}. Bis Index ${i - 1} ist alles fertig.`, { line: 1, vars: [{ name: 'i', index: i }], sortedTo: i });
                let min = i;
                const vars = (j) => [{ name: 'i', index: i }, { name: 'min', index: min }, ...(j !== undefined ? [{ name: 'j', index: j }] : [])];
                snapshot(`min ← i: Die kleinste bis hier steht an Index ${i}.`, { line: 2, vars: vars(), sortedTo: i });
                for (let j = i + 1; j < n; j++) {
                    comparisons += 1;
                    if (v(j) < v(min)) {
                        snapshot(`list[j] < list[min]: ${v(j)} < ${v(min)}, ja.`, { line: 4, vars: vars(j), compare: [j, min], sortedTo: i });
                        min = j;
                        snapshot(`min ← j: Die kleinste bis hier ist ${v(j)}, an Index ${j}.`, { line: 4, vars: vars(j), sortedTo: i });
                    } else snapshot(`list[j] < list[min]: ${v(j)} ist nicht kleiner als ${v(min)}.`, { line: 4, vars: vars(j), compare: [j, min], sortedTo: i });
                }
                if (min !== i) {
                    swap(i, min);
                    snapshot(`swap list[i] and list[min]: ${v(i)} und ${v(min)} tauschen die Plätze. Bis Index ${i} ist alles fertig.`, { line: 5, vars: vars(), swap: true, sortedTo: i + 1 });
                } else snapshot(`swap list[i] and list[min]: ${v(i)} steht schon richtig. Bis Index ${i} ist alles fertig.`, { line: 5, vars: vars(), sortedTo: i + 1 });
            }
        } else if (kind === 'insertion') {
            for (let i = 1; i < n; i++) {
                snapshot(`for i: i ist ${i}. Links davon ist alles sortiert.`, { line: 1, vars: [{ name: 'i', index: i }], sortedTo: i });
                const cid = nextId++;
                values[cid] = v(i);
                lifted = { id: cid, at: i };
                snapshot(`current ← list[i]: Die Variable current merkt sich ${values[cid]}; in der Liste steht es noch.`, { line: 2, vars: [{ name: 'i', index: i }], from: { [cid]: slots[i] }, sortedTo: i });
                let j = i;
                const vars = () => [{ name: 'i', index: i }, { name: 'j', index: j }];
                snapshot(`j ← i: j ist ${j}.`, { line: 3, vars: vars(), sortedTo: i });
                for (;;) {
                    if (j === 0) {
                        snapshot('while j > 0: j ist 0, ganz links. Hier ist der Platz.', { line: 4, vars: vars(), sortedTo: i });
                        break;
                    }
                    comparisons += 1;
                    if (!(v(j - 1) > values[cid])) {
                        snapshot(`list[j - 1] > current: ${v(j - 1)} ist nicht größer als ${values[cid]}. Hier ist der Platz.`, { line: 4, vars: vars(), compare: [j - 1], sortedTo: i });
                        break;
                    }
                    snapshot(`list[j - 1] > current: ${v(j - 1)} > ${values[cid]}, ja.`, { line: 4, vars: vars(), compare: [j - 1], sortedTo: i });
                    const copied = copyTo(j, slots[j - 1]);
                    snapshot(`list[j] ← list[j - 1]: ${v(j)} rückt eins nach rechts. Jetzt steht ${v(j)} zweimal da, an Index ${j - 1} und ${j}.`, { line: 5, vars: vars(), from: { [copied]: slots[j - 1] }, sortedTo: i });
                    j -= 1;
                    lifted.at = j;
                    snapshot(`j ← j - 1: j ist ${j}.`, { line: 6, vars: vars(), sortedTo: i });
                }
                slots[j] = cid;
                writes += 1;
                lifted = null;
                snapshot(`list[j] ← current: ${values[cid]} kommt an Index ${j}. Bis Index ${i} ist alles sortiert.`, { line: 7, vars: vars(), sortedTo: i + 1 });
            }
        } else if (kind === 'bubble') {
            for (let end = n - 1; end > 0; end--) {
                snapshot(`for end: end ist ${end}. Rechts davon ist alles fertig.`, { line: 1, vars: [{ name: 'end', index: end }], sortedFrom: end + 1 });
                let swapped = false;
                snapshot('swapped ← false.', { line: 2, vars: [{ name: 'end', index: end }], sortedFrom: end + 1 });
                for (let i = 0; i < end; i++) {
                    const vars = [{ name: 'end', index: end }, { name: 'i', index: i }];
                    comparisons += 1;
                    if (v(i) > v(i + 1)) {
                        snapshot(`list[i] > list[i + 1]: ${v(i)} > ${v(i + 1)}, ja.`, { line: 4, vars, compare: [i, i + 1], sortedFrom: end + 1 });
                        swap(i, i + 1);
                        swapped = true;
                        snapshot(`swap list[i] and list[i + 1]: ${v(i + 1)} wandert eins nach rechts. swapped ← true.`, { line: 5, vars, swap: true, sortedFrom: end + 1 });
                    } else snapshot(`list[i] > list[i + 1]: ${v(i)} ist nicht größer als ${v(i + 1)}.`, { line: 4, vars, compare: [i, i + 1], sortedFrom: end + 1 });
                }
                if (!swapped) {
                    snapshot('if not swapped: In diesem Durchgang wurde nichts getauscht. Alles ist sortiert.', { line: 7, sortedFrom: 0 });
                    break;
                }
            }
        } else if (kind === 'merge') {
            const sort = (lo, hi) => {
                stack.push(`merge_sort(start: ${lo}, stop: ${hi})`);
                const ends = [{ name: 'start', index: lo }, { name: 'stop', index: hi }];
                if (hi - lo < 2) {
                    snapshot(`stop - start < 2: Von ${lo} bis ${hi - 1} steht nur eine Zahl, die ist schon sortiert.`, { line: 1, vars: ends, range: [lo, hi] });
                    stack.pop();
                    return;
                }
                const mid = Math.floor((lo + hi) / 2);
                const three = [...ends, { name: 'middle', index: mid }];
                snapshot(`middle ← (${lo} + ${hi}) / 2, abgerundet: ${mid}.`, { line: 2, vars: three, range: [lo, hi], split: mid });
                snapshot(`merge_sort(list, start, middle): erst die linke Hälfte, ${lo} bis ${mid - 1}.`, { line: 3, vars: three, range: [lo, hi], split: mid });
                sort(lo, mid);
                snapshot(`merge_sort(list, middle, stop): dann die rechte, ${mid} bis ${hi - 1}.`, { line: 4, vars: three, range: [lo, hi], split: mid });
                sort(mid, hi);
                let i = lo;
                let j = mid;
                const vars = (k) => [{ name: 'i', index: i }, { name: 'j', index: j }, ...(k !== undefined ? [{ name: 'k', index: k }] : [])];
                snapshot(`Beide Hälften sind sortiert. i ← ${lo}, j ← ${mid}: die vorderste von jeder.`, { line: 5, vars: vars(), range: [lo, hi], split: mid });
                for (let k = lo; k < hi; k++) {
                    const left = i < mid && (j >= hi || v(i) <= v(j));
                    if (i < mid && j < hi) comparisons += 1;
                    const take = left ? i : j;
                    const id = nextId++;
                    values[id] = v(take);
                    scratch[k] = id;
                    writes += 1;
                    const why = i < mid && j < hi ? `${v(i)} ${left ? '≤' : '>'} ${v(j)}` : left ? 'rechts ist nichts mehr' : 'links ist nichts mehr';
                    if (left) i += 1;
                    else j += 1;
                    snapshot(`${why}: scratch[${k}] ← list[${take}], das ist ${values[id]}.`, { line: left ? 8 : 10, vars: vars(k), range: [lo, hi], split: mid, compare: [take], from: { [id]: slots[take] } });
                }
                const from = {};
                for (let k = lo; k < hi; k++) from[copyTo(k, scratch[k])] = scratch[k];
                for (let k = lo; k < hi; k++) scratch[k] = null;
                snapshot(`Zurück in die Liste: ${lo} bis ${hi - 1} ist sortiert.`, { line: 11, vars: ends, range: [lo, hi], from, fromScratch: true });
                stack.pop();
            };
            sort(0, n);
        } else if (kind === 'quick') {
            const sort = (lo, hi) => {
                stack.push(`quick_sort(first: ${lo}, last: ${hi})`);
                const ends = [{ name: 'first', index: lo }, { name: 'last', index: hi }];
                if (hi <= lo) {
                    snapshot(`last ≤ first: Von ${lo} bis ${hi} steht ${hi === lo ? 'nur eine Zahl' : 'nichts'}, fertig.`, { line: 1, vars: ends.filter((e) => e.index >= 0 && e.index < n), range: [lo, hi + 1] });
                    stack.pop();
                    return;
                }
                const pivot = v(hi);
                snapshot(`pivot ← list[last]: Das Pivot ist ${pivot}.`, { line: 2, vars: ends, range: [lo, hi + 1], pivot: hi });
                let boundary = lo;
                const vars = (i) => [...ends, { name: 'boundary', index: boundary }, ...(i !== undefined ? [{ name: 'i', index: i }] : [])];
                snapshot(`boundary ← first: Links von boundary kommen die Zahlen, die kleiner sind als ${pivot}.`, { line: 3, vars: vars(), range: [lo, hi + 1], pivot: hi, boundary });
                for (let i = lo; i < hi; i++) {
                    comparisons += 1;
                    if (v(i) < pivot) {
                        snapshot(`list[i] < pivot: ${v(i)} < ${pivot}, ja.`, { line: 5, vars: vars(i), range: [lo, hi + 1], pivot: hi, boundary, compare: [i] });
                        if (i !== boundary) {
                            swap(i, boundary);
                            snapshot(`swap list[i] and list[boundary]: ${v(boundary)} kommt nach links.`, { line: 6, vars: vars(i), range: [lo, hi + 1], pivot: hi, boundary, swap: true });
                        } else snapshot(`swap list[i] and list[boundary]: i und boundary sind dieselbe Stelle, ${v(i)} bleibt.`, { line: 6, vars: vars(i), range: [lo, hi + 1], pivot: hi, boundary });
                        boundary += 1;
                        snapshot(`boundary ← boundary + 1: boundary ist ${boundary}.`, { line: 7, vars: vars(i), range: [lo, hi + 1], pivot: hi, boundary });
                    } else snapshot(`list[i] < pivot: ${v(i)} ist nicht kleiner als ${pivot}, bleibt rechts.`, { line: 5, vars: vars(i), range: [lo, hi + 1], pivot: hi, boundary, compare: [i] });
                }
                if (boundary !== hi) swap(boundary, hi);
                snapshot(`swap list[boundary] and list[last]: Das Pivot ${pivot} kommt an Index ${boundary}, an seinen Platz. Links die kleineren, rechts die anderen.`,
                    { line: 8, vars: vars(), range: [lo, hi + 1], pivot: boundary, boundary, swap: boundary !== hi });
                snapshot(`quick_sort(list, first, boundary - 1): links weiter, ${lo} bis ${boundary - 1}.`, { line: 9, vars: vars(), range: [lo, hi + 1], pivot: boundary });
                sort(lo, boundary - 1);
                snapshot(`quick_sort(list, boundary + 1, last): rechts weiter, ${boundary + 1} bis ${hi}.`, { line: 10, vars: vars(), range: [lo, hi + 1], pivot: boundary });
                sort(boundary + 1, hi);
                stack.pop();
            };
            sort(0, n - 1);
        }
        lifted = null;
        snapshot(`Fertig: ${comparisons} Vergleiche, ${writes} Schreibzugriffe.`, { line: 0, sortedTo: n });
        return frames;
    }

    // The list as bars, under them the indices and the index variables (a
    // label each, with an arrow up to its place); the copy that current
    // holds above its place; merge sort's scratch row at the bottom.
    // The boxes of one place (indexLabels), each with an arrow up to the
    // place: it aims at aim (a point inside the column, near its bottom) and
    // stops at the height stop, before the index number, so that arrows
    // from boxes side by side end apart. A box is keyed by its variable, so
    // that it moves with it.
    function indexLabel(group, y, aim, stop, marker) {
        const out = [];
        group.boxes.forEach((box) => {
            const centre = [box.x, y + 9];
            const f = (centre[1] - stop) / (centre[1] - aim);
            const end = [centre[0] + (group.x - centre[0]) * f, stop];
            const inset = Math.hypot(group.x - end[0], aim - end[1]);
            out.push(el('svg:path', { 'data-key': `pointer-${box.name}`, 'data-ends': `var-${box.name} slot-${group.index}`, 'data-inset': inset, 'data-rigid': '',
                d: `M${centre[0]} ${centre[1]} Q${(centre[0] + end[0]) / 2} ${(centre[1] + end[1]) / 2} ${end[0]} ${end[1]}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 1.5, 'marker-end': `url(#${marker}-accent)` }));
        });
        group.boxes.forEach((box) => {
            out.push(el('svg:rect', { 'data-solid': '', 'data-key': `var-${box.name}`, x: box.x - box.width / 2, y, width: box.width, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            out.push(el('svg:text', { 'data-key': `var-${box.name}-label`, x: box.x, y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: box.name }));
        });
        return out;
    }

    function drawSort(svg, frame, marker) {
        svg.replaceChildren();
        const n = frame.slots.length;
        const all = Object.values(frame.values);
        const most = Math.max(...all);
        const width = 520;
        const cell = width / n;
        const height = 170;
        // room above the bars for the card of current
        const lift = 44;
        const baseline = lift + height;
        const compare = new Set(frame.compare || []);
        const inRange = (i) => !frame.range || (i >= frame.range[0] && i < frame.range[1]);
        const isSorted = (i) => (frame.sortedTo !== undefined && i < frame.sortedTo) || (frame.sortedFrom !== undefined && i >= frame.sortedFrom);
        const barH = (value) => 20 + (value / most) * (height - 40);
        const arc = frame.swap ? { 'data-arc': 26 } : {};
        const from = frame.from || {};
        const bar = (id, x, y, fill, extra = {}) => {
            const h = barH(frame.values[id]);
            const source = from[id] !== undefined ? from[id] : null;
            const comes = source !== null ? { 'data-from': frame.fromScratch ? `scratch-value-${source}` : `bar-${source}` } : {};
            const comesLabel = source !== null ? { 'data-from': frame.fromScratch ? `scratch-value-${source}` : `value-${source}` } : {};
            return [
                el('svg:g', { 'data-key': `bar-${id}`, ...comes, transform: `translate(${x} ${y - h})`, ...arc }, [
                    el('svg:rect', { x: 0, y: 0, width: cell - 8, height: h, rx: 4, fill, ...extra })]),
                el('svg:text', { 'data-key': `value-${id}`, ...comesLabel, x: x + (cell - 8) / 2, y: y - h - 6, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.values[id]) }),
            ];
        };
        svg.appendChild(arrowheads(marker));
        frame.slots.forEach((id, i) => {
            const fill = frame.pivot === i ? 'var(--algo-goal)' : compare.has(i) ? 'var(--algo-frontier)'
                : isSorted(i) ? 'var(--algo-done)' : 'color-mix(in srgb, var(--algo-line) 60%, transparent)';
            for (const node of bar(id, i * cell + 4, baseline, fill, { opacity: inRange(i) ? 1 : 0.35, stroke: compare.has(i) ? 'var(--algo-ink)' : 'none', 'stroke-width': 2 })) svg.appendChild(node);
            svg.appendChild(el('svg:text', { 'data-key': `index-${i}`, x: i * cell + cell / 2, y: baseline + 16, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(el('svg:rect', { 'data-key': `slot-${i}`, x: i * cell + 4, y: baseline - 10, width: cell - 8, height: 4, fill: 'none', stroke: 'none' }));
        });
        // the place just after the end (stop) has an anchor too
        svg.appendChild(el('svg:rect', { 'data-key': `slot-${n}`, x: n * cell + 4, y: baseline - 10, width: cell - 8, height: 4, fill: 'none', stroke: 'none' }));
        if (frame.lifted) {
            // the copy that current holds: a card with its value above its
            // place, the label current beside it; it flies out of the bar and
            // back into the list
            const { id, at } = frame.lifted;
            const x = at * cell + 4;
            const source = from[id];
            svg.appendChild(el('svg:g', { 'data-key': `bar-${id}`, ...(source !== undefined ? { 'data-from': `bar-${source}` } : {}), transform: `translate(${x} 4)` }, [
                el('svg:rect', { x: 0, y: 0, width: cell - 8, height: 28, rx: 4, fill: 'color-mix(in srgb, var(--algo-accent) 25%, var(--algo-paper))', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '5 3' }),
                el('svg:text', { x: (cell - 8) / 2, y: 19, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.values[id]) })]));
            const w = 14 + 7.5 * 'current'.length;
            svg.appendChild(el('svg:rect', { 'data-solid': '', 'data-key': 'var-current', x: x + cell - 2, y: 9, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': 'var-current-label', x: x + cell - 2 + w / 2, y: 22, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'current' }));
        }
        if (frame.split !== undefined) svg.appendChild(el('svg:line', { 'data-key': 'split', x1: frame.split * cell, y1: lift - 10, x2: frame.split * cell, y2: baseline, stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        if (frame.boundary !== undefined) svg.appendChild(el('svg:line', { 'data-key': 'boundary', x1: frame.boundary * cell, y1: lift - 10, x2: frame.boundary * cell, y2: baseline, stroke: 'var(--algo-goal)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        // the index variables, one label for each place (indexLabels)
        const labelTop = baseline + 28;
        const placed = indexLabels(frame.vars, (index) => index * cell + cell / 2, width + cell);
        for (const label of placed) for (const node of indexLabel(label, labelTop + 8 + label.row * 24, baseline - 8, baseline + 22, marker)) svg.appendChild(node);
        let bottom = labelTop + 6 + Math.max(3, ...placed.map((l) => l.row + 1)) * 24;
        if (frame.scratch) {
            // a value flies from its bar into the scratch row, and from there
            // back into the list when the merged run is copied back
            frame.scratch.forEach((id, i) => {
                const x = i * cell + 4;
                svg.appendChild(el('svg:rect', { 'data-key': `scratch-${i}`, x, y: bottom + 6, width: cell - 8, height: 28, rx: 4, fill: 'none', stroke: 'var(--algo-line)', 'stroke-width': 1.5 }));
                if (id !== null) {
                    // a group, so that it flies from its bar (a group too) and back
                    const source = from[id];
                    svg.appendChild(el('svg:g', { 'data-key': `scratch-value-${id}`, ...(source !== undefined ? { 'data-from': `bar-${source}` } : {}), transform: `translate(${x} ${bottom + 6})` }, [
                        el('svg:rect', { x: 0, y: 0, width: cell - 8, height: 28, rx: 4, fill: 'var(--algo-frontier)', stroke: 'var(--algo-line)', 'stroke-width': 1.5 }),
                        el('svg:text', { x: (cell - 8) / 2, y: 19, 'text-anchor': 'middle', 'font-size': 13, fill: 'var(--algo-ink)', text: String(frame.values[id]) })]));
                }
            });
            svg.appendChild(el('svg:text', { 'data-key': 'scratch-label', x: 0, y: bottom + 50, 'font-size': 11, fill: 'var(--bs-secondary-color)', text: 'scratch' }));
            bottom += 56;
        }
        svg.setAttribute('viewBox', `0 0 ${width + cell} ${bottom}`);
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
        const call = (name, n, value) => ({ call: `${name}(node: ${n ? n.value : 'empty'}, value: ${value})`, node: n ? n.id : null });
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
            const bottom = { ...call('insert', null, value), returns: `${value} (neu)` };
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
        const stack = [...(base.stack || []), { call: `${title.replace(/\(.\)/, `(${p}: ${val(top)})`)}`, node: top }];
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
        svg.appendChild(arrowheads(most.marker));
        // a pointer is the left or right field of its node: when it changes,
        // the arrow lets go of its node and snaps onto the new one
        for (const [id, node] of Object.entries(graph)) {
            for (const side of ['left', 'right']) {
                if (node[side] === null) continue;
                svg.appendChild(arrow(`edge-${id}-${side}`, `node-${id} node-${node[side]}`, point(Number(id)), point(node[side])));
            }
        }
        // the pulses on the arrows, behind the nodes like the arrows themselves
        const pulseLayer = pulseNodes(frame.pulses);
        for (const node of pulseLayer) svg.appendChild(node);
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
            if (v.to !== null && v.to !== undefined) svg.appendChild(arrow(`pointer-${v.name}`, `var-${v.name} node-${v.to}`, centre, point(v.to), { stroke: 'var(--algo-accent)', 'marker-end': `url(#${most.marker}-accent)`, ...(v.rigid ? { 'data-rigid': '' } : {}) }));
            labels.push(el('svg:rect', { 'data-solid': '', 'data-key': `var-${v.name}`, x, y, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            labels.push(el('svg:text', { 'data-key': `var-${v.name}-label`, x: centre[0], y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: v.name }));
        }
        // a node by its id; a new node comes from the token that carried its
        // value down the tree; a copied value flies over from the node it was
        // copied from (its label has the value in its key)
        let ring = null;
        for (const [id, { x, depth: d }] of at) {
            const { value } = graph[id];
            const fresh = frame.fresh === id;
            const fill = frame.gone === id ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))' : fresh ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            const fromToken = fresh ? { 'data-from': 'incoming-label' } : {};
            const copied = frame.copied && frame.copied.into === id ? { 'data-from': `label-${frame.copied.from}-${value}` } : {};
            // the ring stays on its node while it fades in or out; it is
            // drawn in front of the nodes (after them)
            if (frame.focus === id) ring = el('svg:circle', { 'data-key': 'focus', 'data-follow': `node-${id}`, cx: sx(x), cy: sy(d), r: 23, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 });
            svg.appendChild(el('svg:circle', { 'data-key': `node-${id}`, ...(fresh ? { 'data-from': 'incoming' } : {}), cx: sx(x), cy: sy(d), r: RADIUS, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': `label-${id}-${value}`, ...fromToken, ...copied, x: sx(x), y: sy(d) + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            if (frame.avl) svg.appendChild(el('svg:text', { 'data-key': `height-${id}`, ...fromToken, x: sx(x), y: sy(d) + 32, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: `h${graph[id].height}` }));
        }
        if (ring) svg.appendChild(ring);
        for (const label of labels) svg.appendChild(label);
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

    // Labels for index variables under a row of places: each variable its
    // own box; the boxes of one place side by side as a group, and the groups
    // in rows so that none overlaps another and none stands in the way of an
    // arrow from a row further down (each arrow goes up from its box to its
    // place). vars: { name, index }; x(index): the centre of the place.
    // Returns groups { index, x, row, width, boxes: [{ name, x, width }] }.
    const GAP = 5;
    function indexLabels(vars, x, maxX = Infinity) {
        const byIndex = new Map();
        for (const v of vars || []) {
            if (!byIndex.has(v.index)) byIndex.set(v.index, []);
            byIndex.get(v.index).push(v.name);
        }
        const groups = [...byIndex.entries()].sort((a, b) => a[0] - b[0]).map(([index, names]) => {
            const widths = names.map((name) => 14 + 7.5 * name.length);
            const width = widths.reduce((s, w) => s + w, 0) + GAP * (names.length - 1);
            // inside the picture: pushed in at the edges, the arrows then slant
            let left = Math.min(Math.max(2, x(index) - width / 2), maxX - width - 2);
            const centre = left + width / 2;
            const boxes = names.map((name, k) => { const box = { name, x: left + widths[k] / 2, width: widths[k] }; left += widths[k] + GAP; return box; });
            return { index, x: x(index), centre, width, boxes };
        });
        const rows = [];
        // the arrows run from the boxes to x: a group must not cover the x of
        // a group in a row further down, nor have its own x covered by one in
        // a row further up
        const covers = (o, x) => x > o.centre - o.width / 2 - 3 && x < o.centre + o.width / 2 + 3;
        // the narrow groups first: a wide one goes down a row rather than
        // into the way of a narrow one's arrow
        for (const group of [...groups].sort((a, b) => a.width - b.width)) {
            const left = group.centre - group.width / 2 - 4;
            const right = group.centre + group.width / 2 + 4;
            const free = (row) => (rows[row] || []).every((o) => right < o.centre - o.width / 2 || left > o.centre + o.width / 2);
            const fits = (row) => free(row)
                && rows.slice(0, row).every((above) => above.every((o) => !covers(o, group.x)))
                && rows.slice(row + 1).every((under) => under.every((o) => !covers(group, o.x)));
            // a few rows at most; if none is clear of arrows, the first without overlaps
            const limit = groups.length + 1;
            let row = 0;
            while (row < limit && !fits(row)) row++;
            if (row === limit) for (row = 0; !free(row); row++);
            group.row = row;
            (rows[row] = rows[row] || []).push(group);
        }
        return groups;
    }

    // Arrowheads in the colour of their line: id for the grey pointers of a
    // structure, id-accent for the variables
    const arrowheads = (id) => el('svg:defs', {}, [['', 'var(--algo-line)'], ['-accent', 'var(--algo-accent)']].map(([suffix, fill]) =>
        el('svg:marker', { id: `${id}${suffix}`, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 9, markerHeight: 9, markerUnits: 'userSpaceOnUse', orient: 'auto-start-reverse' },
            [el('svg:path', { d: 'M0 0L10 5L0 10z', fill })])));

    // A pulse: the pointer swells a little, and the bulge travels down the
    // arrow like a peristaltic wave (stepper_motion.js, data-pulse)
    const pulseNodes = (pulses) => (pulses || []).map((pulse, i) =>
        el('svg:path', { 'data-key': `pulse-${i}`, 'data-pulse': pulse.edge, 'data-delay': pulse.delay || 0, 'data-length': pulse.length || 0.4,
            'data-swell': 2.5, d: 'M0 0', opacity: 0 }));

    // The pseudocode that runs, its line marked; a long line wraps, its
    // continuation two characters further in than the line itself
    function codeBox(code) {
        if (!code) return [el('h5', { text: 'Pseudocode' }), el('p', { class: 'algo-empty', text: 'Hier steht der Pseudocode der Operation, die gerade läuft, und ihre Zeile ist markiert.' })];
        const line = (text, i) => {
            const indent = text.match(/^ */)[0].length;
            return el('span', { class: `line${i === code.at ? ' current' : ''}`, style: `padding-left: calc(0.6em + ${indent + 2}ch); text-indent: -2ch;` }, highlightPseudocode(text.trimStart()));
        };
        return [el('h5', { text: 'Pseudocode' }), el('pre', { class: 'algo-code code-pseudocode' }, code.lines.map(line))];
    }

    // The pseudocode's tokens, as Rouge::Lexers::Pseudocode colours them on
    // the pages (pseudocode_lexer.rb): the same classes, the same colours
    const KEYWORDS = new Set(['if', 'else', 'while', 'for', 'each', 'in', 'of', 'return', 'not', 'and', 'or', 'repeat', 'until', 'break', 'continue', 'do', 'then', 'to', 'from', 'down', 'mod', 'div']);
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
                // the calls that run, each one deeper indented; on the way
                // back up they return one after another, the deepest first
                const stack = frame.stack || [];
                const calls = stack.length ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack' }, stack.map((entry, i) => {
                    const li = el('li', { class: !frame.returning && i === stack.length - 1 ? 'current' : '', style: `margin-left: ${i * 1.2}em;`, ...(entry.node !== null && entry.node !== undefined ? { 'data-node': entry.node } : {}),
                        text: entry.returns ? `${entry.call} → ${entry.returns}` : entry.call });
                    if (frame.returning) li.style.animationDelay = `${((stack.length - 1 - i) / (stack.length + 1)) * (frame.duration || MOTION_TIME) / 1000}s`;
                    if (frame.returning) li.classList.add('returning');
                    return li;
                }))] : [];
                side.replaceChildren(...calls, ...codeBox(frame.code));
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
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Die Zahlen als Balken, Höhe nach Wert, darunter die Indizes');
            draw = (frame) => {
                drawSort(svg, frame, marker);
                const stack = frame.stack.length ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack' }, frame.stack.map((entry, i) =>
                    el('li', { class: i === frame.stack.length - 1 ? 'current' : '', style: `margin-left: ${i * 1.2}em;`, text: entry.call })))] : [];
                side.replaceChildren(el('h5', { text: 'Gezählt' }),
                    el('table', { class: 'algo-table' }, [
                        el('tr', {}, [el('th', { text: 'Vergleiche' }), el('td', { text: String(frame.comparisons) })]),
                        el('tr', {}, [el('th', { text: 'Schreibzugriffe' }), el('td', { text: String(frame.writes) })]),
                    ]), ...stack, ...codeBox(frame.code));
            };
        } else if (algo === 'heap') {
            const parse = (text) => (text || '').split(',').map((s) => s.trim()).filter(Boolean).map((s) => s.split(/\s+/))
                .map(([type, value]) => ({ type, value: Number(value) }));
            const ops = parse(widget.dataset.ops);
            frames = traceHeap(ops, parse(widget.dataset.setup));
            const most = Math.max(...frames.map((frame) => frame.slots.length + Object.keys(frame.aside).length));
            const pops = ops.some((op) => op.type === 'pop');
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Der Heap als Baum, darunter dasselbe als Array mit den Indizes');
            draw = (frame) => {
                drawHeap(svg, frame, most, marker);
                side.replaceChildren(...(pops ? [el('h5', { text: 'Heraus mit pop' }), el('div', { class: 'algo-items' }, frame.out.length
                    ? frame.out.map((value, i) => el('span', { class: `algo-item${i === frame.out.length - 1 ? ' next' : ''}`, text: String(value) }))
                    : [el('span', { class: 'algo-empty', text: '–' })])] : []), ...codeBox(frame.code));
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
        // The timeline: every step a mark, the steps of one operation a
        // section with its name at its start; done ones coloured, the
        // current one marked. A tap or a drag on it goes to that step.
        const sections = [];
        frames.forEach((frame, i) => {
            const op = frame.op === undefined ? -1 : frame.op;
            if (!sections.length || sections[sections.length - 1].op !== op) sections.push({ op, steps: [] });
            sections[sections.length - 1].steps.push(i);
        });
        const ticks = [];
        const timeline = el('div', { class: 'algo-timeline', 'data-no-swipe': '', role: 'slider', 'aria-label': 'Die Schritte', 'aria-valuemin': 0, 'aria-valuemax': frames.length - 1, tabindex: -1 },
            sections.map((section) => el('div', { class: 'algo-section', style: `flex: ${section.steps.length} 1 0;` }, [
                el('div', { class: 'algo-ticks' }, section.steps.map((i, k) => {
                    const tick = el('span', { class: 'algo-tick', 'data-step': i, ...(k === 0 && section.op >= 0 ? { 'data-start': '' } : {}) });
                    ticks.push(tick);
                    return tick;
                })),
            ])));
        // the names of the operations above their first dot, each starting
        // at its dot; one that would run into the name before it goes up a row
        const names = sections.filter((s) => s.op >= 0 && opsText).map((s) => ({ section: s, node: el('span', { class: 'algo-op-name', text: opsText[s.op] }) }));
        const nameRow = el('div', { class: 'algo-op-names' }, names.map((n) => n.node));
        const placeNames = () => {
            const ends = [];
            const origin = nameRow.getBoundingClientRect().left;
            for (const { section, node } of names) {
                const dot = ticks[section.steps[0]].getBoundingClientRect();
                const left = dot.left + dot.width / 2 - origin - 4.5;
                let row = 0;
                while (ends[row] !== undefined && ends[row] > left - 8) row++;
                ends[row] = left + node.offsetWidth;
                node.style.left = `${left}px`;
                node.style.bottom = `${row * 1.35}em`;
            }
            nameRow.style.height = `${Math.max(1, ends.length) * 1.35}em`;
        };
        const timelineBox = el('div', { class: 'algo-timeline-box' }, [nameRow, timeline]);
        const body = el('div', { class: 'algo-body' }, [picture, side]);
        widget.replaceChildren(el('div', { class: 'algo-controls' }, [...buttons, count]), timelineBox, body, message);
        requestAnimationFrame(placeNames);
        if (document.fonts) document.fonts.ready.then(placeNames);
        window.addEventListener('resize', placeNames);
        let dragging = false;
        const stepAt = (event) => {
            const box = timeline.getBoundingClientRect();
            const hit = document.elementFromPoint(Math.min(box.right - 1, Math.max(box.left + 1, event.clientX)), box.bottom - 6);
            const tick = hit && hit.closest && hit.closest('.algo-tick');
            if (!tick) return;
            const to = Number(tick.dataset.step);
            if (to !== index) { const before = index; index = to; show(Math.abs(index - before) === 1); }
        };
        timeline.addEventListener('pointerdown', (event) => { dragging = true; timeline.setPointerCapture(event.pointerId); stepAt(event); });
        timeline.addEventListener('pointermove', (event) => { if (dragging) stepAt(event); });
        timeline.addEventListener('pointerup', () => { dragging = false; });
        timeline.addEventListener('pointercancel', () => { dragging = false; });
        // Lines from each call on the stack to the node it holds (trees): the
        // current one strong, the waiting ones faint; they follow the nodes
        // while they move and fade with their call when it returns
        const links = algo === 'bst' || algo === 'avl' ? el('svg:svg', { class: 'algo-links', 'aria-hidden': 'true' }) : null;
        if (links) body.prepend(links);
        let linking = 0;
        const connect = () => {
            const box = body.getBoundingClientRect();
            links.setAttribute('viewBox', `0 0 ${box.width} ${box.height}`);
            // only when the code stands beside the picture; below it (a
            // phone) the lines would cross the whole picture
            if (side.getBoundingClientRect().left < picture.getBoundingClientRect().right) {
                links.replaceChildren();
                return;
            }
            const paths = [];
            for (const li of side.querySelectorAll('li[data-node]')) {
                const node = picture.querySelector(`[data-key="node-${li.dataset.node}"]`);
                if (!node) continue;
                const c = node.getBoundingClientRect();
                const l = li.getBoundingClientRect();
                const [cx, cy, r] = [c.left + c.width / 2 - box.left, c.top + c.height / 2 - box.top, c.width / 2];
                const [sx, sy] = [l.left - box.left - 2, l.top + l.height / 2 - box.top];
                const d = Math.hypot(sx - cx, sy - cy) || 1;
                const [ex, ey] = [cx + ((sx - cx) / d) * (r + 3), cy + ((sy - cy) / d) * (r + 3)];
                const bend = `C${sx - 40} ${sy} ${ex + 30} ${ey}`;
                const current = li.classList.contains('current');
                paths.push(el('svg:path', { d: `M${sx} ${sy} ${bend} ${ex} ${ey}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': current ? 1.8 : 1.2,
                    'stroke-dasharray': current ? 'none' : '3 4', opacity: (current ? 0.9 : 0.45) * Number(getComputedStyle(li).opacity) }));
                paths.push(el('svg:circle', { cx: ex, cy: ey, r: 2.5, fill: 'var(--algo-accent)', opacity: (current ? 0.9 : 0.45) * Number(getComputedStyle(li).opacity) }));
            }
            links.replaceChildren(...paths);
        };
        // redrawn every frame while the picture moves or the calls fade, then rest
        const follow = (time) => {
            cancelAnimationFrame(linking);
            const until = performance.now() + time;
            const loop = () => {
                connect();
                if (picture.hasAttribute('data-moving') || performance.now() < until) linking = requestAnimationFrame(loop);
            };
            loop();
        };
        if (links) window.addEventListener('resize', () => follow(0));
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
            if (links) follow((frame.duration || MOTION_TIME) * 2 + 600);
            ticks.forEach((tick, i) => { tick.className = `algo-tick${'start' in tick.dataset ? ' start' : ''}${i < index ? ' done' : i === index ? ' current' : ''}`; });
            for (const { section, node } of names) node.classList.toggle('current', section.steps.includes(index));
            timeline.setAttribute('aria-valuenow', index);
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
