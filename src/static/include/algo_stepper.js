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

    // Sorting: every comparison and every write is a step, counted like the
    // Sorting Sandbox counts them. sorted: indices already in their place.
    function traceSort(values, kind) {
        const a = [...values];
        const frames = [];
        let comparisons = 0;
        let writes = 0;
        const snapshot = (message, marks = {}) => frames.push({ items: [...a], message, comparisons, writes, ...marks });
        const less = (i, j) => { comparisons += 1; return a[i] < a[j]; };
        const swap = (i, j) => { [a[i], a[j]] = [a[j], a[i]]; writes += 2; };
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
                let j = i;
                snapshot(`${current} (Index ${i}) wird einsortiert, links davon ist alles sortiert.`, { lifted: i, sortedTo: i });
                while (j > 0) {
                    comparisons += 1;
                    if (!(a[j - 1] > current)) {
                        snapshot(`${a[j - 1]} ist nicht größer als ${current}: Hier ist sein Platz.`, { compare: [j - 1], lifted: j, sortedTo: i });
                        break;
                    }
                    a[j] = a[j - 1];
                    writes += 1;
                    a[j - 1] = current;
                    snapshot(`${a[j]} ist größer als ${current} und rückt eins nach rechts.`, { compare: [j], lifted: j - 1, sortedTo: i });
                    j -= 1;
                }
                a[j] = current;
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
                    writes += 1;
                    snapshot(`Die kleinere der beiden vordersten, ${a[take]}, kommt in den Zwischenspeicher an Index ${k}.`, { range: [lo, hi], split: mid, compare: [take], scratch: [...scratch] });
                }
                for (let k = lo; k < hi; k++) { a[k] = scratch[k]; writes += 1; }
                scratch.fill(null, lo, hi);
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
        frame.items.forEach((value, i) => {
            const h = 20 + (value / most) * (height - 40);
            const x = i * cell + 4;
            const lifted = frame.lifted === i;
            const y = height - h - (lifted ? 18 : 0);
            const fill = frame.pivot === i ? 'var(--algo-goal)' : mark.has(i) || lifted ? 'var(--algo-accent)' : compare.has(i) ? 'var(--algo-frontier)'
                : isSorted(i) ? 'var(--algo-done)' : 'color-mix(in srgb, var(--algo-line) 60%, transparent)';
            svg.appendChild(el('svg:rect', { x, y, width: cell - 8, height: h, rx: 4, fill, opacity: inRange(i) ? 1 : 0.35,
                stroke: compare.has(i) ? 'var(--algo-ink)' : 'none', 'stroke-width': 2 }));
            svg.appendChild(el('svg:text', { x: x + (cell - 8) / 2, y: y - 6, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            svg.appendChild(el('svg:text', { x: x + (cell - 8) / 2, y: height + 16, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: String(i) }));
        });
        if (frame.split !== undefined) {
            const x = frame.split * cell;
            svg.appendChild(el('svg:line', { x1: x, y1: 0, x2: x, y2: height, stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        }
        if (frame.boundary !== undefined) {
            const x = frame.boundary * cell;
            svg.appendChild(el('svg:line', { x1: x, y1: 0, x2: x, y2: height, stroke: 'var(--algo-goal)', 'stroke-width': 2, 'stroke-dasharray': '5 4' }));
        }
        let bottom = height + 24;
        if (frame.scratch) {
            frame.scratch.forEach((value, i) => {
                const x = i * cell + 4;
                svg.appendChild(el('svg:rect', { x, y: bottom + 6, width: cell - 8, height: 28, rx: 4, fill: value === null ? 'none' : 'var(--algo-frontier)', stroke: 'var(--algo-line)', 'stroke-width': 1.5 }));
                if (value !== null) svg.appendChild(el('svg:text', { x: x + (cell - 8) / 2, y: bottom + 25, 'text-anchor': 'middle', 'font-size': 13, fill: 'var(--algo-ink)', text: String(value) }));
            });
            svg.appendChild(el('svg:text', { x: 0, y: bottom + 50, 'font-size': 11, fill: 'var(--bs-secondary-color)', text: 'Zwischenspeicher' }));
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
    // rotations after every insert). Nodes are { value, left, right, height }.
    function traceTree(ops, avl) {
        let root = null;
        const frames = [];
        const height = (n) => (n ? n.height : 0);
        const update = (n) => { n.height = 1 + Math.max(height(n.left), height(n.right)); };
        const copy = (n) => (n ? { value: n.value, height: n.height, left: copy(n.left), right: copy(n.right) } : null);
        const snapshot = (message, marks = {}) => frames.push({ root: copy(root), message, avl, ...marks });
        const rotateRight = (y) => { const x = y.left; y.left = x.right; x.right = y; update(y); update(x); return x; };
        const rotateLeft = (x) => { const y = x.right; x.right = y.left; y.left = x; update(x); update(y); return y; };
        snapshot(avl ? 'Ein leerer AVL-Baum.' : 'Ein leerer Suchbaum.');
        let pending = [];
        const insert = (node, value) => {
            if (!node) return { value, left: null, right: null, height: 1 };
            if (value === node.value) return node;
            if (value < node.value) node.left = insert(node.left, value);
            else node.right = insert(node.right, value);
            update(node);
            if (!avl) return node;
            const balance = height(node.left) - height(node.right);
            if (balance > 1 || balance < -1) {
                const heavyLeft = balance > 1;
                const child = heavyLeft ? node.left : node.right;
                const outer = heavyLeft ? value < child.value : value > child.value;
                const kind = heavyLeft ? (outer ? 'LL' : 'LR') : (outer ? 'RR' : 'RL');
                pending.push({ at: node.value, kind, balance });
                if (kind === 'LL') return rotateRight(node);
                if (kind === 'RR') return rotateLeft(node);
                if (kind === 'LR') { node.left = rotateLeft(node.left); return rotateRight(node); }
                node.right = rotateRight(node.right); return rotateLeft(node);
            }
            return node;
        };
        const removeNode = (node, value) => {
            if (!node) return null;
            if (value < node.value) node.left = removeNode(node.left, value);
            else if (value > node.value) node.right = removeNode(node.right, value);
            else if (!node.left) return node.right;
            else if (!node.right) return node.left;
            else {
                let next = node.right;
                while (next.left) next = next.left;
                node.value = next.value;
                node.right = removeNode(node.right, next.value);
            }
            update(node);
            return node;
        };
        const walkTo = (value, what) => {
            let walk = root;
            while (walk && walk.value !== value) {
                const left = value < walk.value;
                snapshot(`${what} ${value}: ${value} ${left ? '<' : '>'} ${walk.value}, also nach ${left ? 'links' : 'rechts'}.`, { focus: walk.value });
                walk = left ? walk.left : walk.right;
            }
            return walk;
        };
        for (const op of ops) {
            if (op.type === 'find') {
                const found = walkTo(op.value, 'find');
                snapshot(found ? `find ${op.value}: gefunden.` : `find ${op.value}: Hier wäre ${op.value}, aber der Platz ist leer. ${op.value} ist nicht im Baum.`, { focus: found ? op.value : undefined });
                continue;
            }
            if (op.type === 'remove') {
                const node = walkTo(op.value, 'remove');
                if (!node) { snapshot(`remove ${op.value}: ${op.value} ist nicht im Baum.`); continue; }
                if (!node.left && !node.right) {
                    snapshot(`remove ${op.value}: gefunden. Der Knoten hat keine Kinder und fällt einfach weg.`, { focus: op.value, gone: op.value });
                } else if (!node.left || !node.right) {
                    const child = (node.left || node.right).value;
                    snapshot(`remove ${op.value}: gefunden. Der Knoten hat nur ein Kind, ${child}; es rückt mit seinem ganzen Teilbaum an seine Stelle.`, { focus: op.value, gone: op.value });
                } else {
                    let next = node.right;
                    snapshot(`remove ${op.value}: gefunden, mit zwei Kindern. Sein Platz bekommt den nächstgrößeren Wert: einmal rechts, dann links, so weit es geht.`, { focus: op.value, gone: op.value });
                    while (next.left) { snapshot(`Der nächstgrößere nach ${op.value}: bei ${next.value} weiter nach links.`, { focus: next.value, gone: op.value }); next = next.left; }
                    snapshot(`${next.value} ist der nächstgrößere. Er kommt an die Stelle von ${op.value}, und sein alter Knoten wird entfernt; der hat höchstens ein rechtes Kind.`, { focus: next.value, gone: op.value });
                }
                root = removeNode(root, op.value);
                snapshot(`${op.value} ist weg, und links stehen immer noch die kleineren, rechts die größeren Werte.`);
                continue;
            }
            if (op.type !== 'insert') continue;
            // the way down first, step by step
            let walk = root;
            while (walk) {
                const dir = op.value < walk.value ? 'links' : op.value > walk.value ? 'rechts' : null;
                snapshot(dir ? `insert ${op.value}: ${op.value} ${dir === 'links' ? '<' : '>'} ${walk.value}, also nach ${dir}.` : `insert ${op.value}: ${op.value} ist schon da.`, { focus: walk.value });
                if (!dir) break;
                walk = dir === 'links' ? walk.left : walk.right;
            }
            pending = [];
            const before = copy(root);
            root = insert(root, op.value);
            if (pending.length) {
                const r = pending[pending.length - 1];
                // show the unbalanced tree, then the rotated one
                const unbalanced = (() => { const t = copy(before); const add = (n) => { if (!n) return { value: op.value, left: null, right: null, height: 1 }; if (op.value < n.value) n.left = add(n.left); else n.right = add(n.right); n.height = 1 + Math.max(height(n.left), height(n.right)); return n; }; return add(t); })();
                frames.push({ root: unbalanced, message: `${op.value} hängt jetzt unten. Beim Knoten ${r.at} unterscheiden sich die Höhen der Teilbäume um ${Math.abs(r.balance)}: zu viel. Das ist der Fall ${r.kind}.`, avl, focus: r.at, fresh: op.value });
                const how = { LL: `eine Rotation nach rechts um ${r.at}`, RR: `eine Rotation nach links um ${r.at}`, LR: `erst eine Rotation nach links um das linke Kind, dann nach rechts um ${r.at}`, RL: `erst eine Rotation nach rechts um das rechte Kind, dann nach links um ${r.at}` }[r.kind];
                snapshot(`Fall ${r.kind}: ${how}. Danach ist der Baum wieder ausgeglichen, und die Suchregel gilt immer noch.`, { fresh: op.value });
            } else {
                snapshot(before ? `${op.value} kommt dorthin, wo die Suche ins Leere gelaufen ist.` : `${op.value} wird die Wurzel.`, { fresh: op.value });
            }
        }
        return frames;
    }

    function treeShape(n, depth = 1) {
        if (!n) return { count: 0, depth: depth - 1 };
        const l = treeShape(n.left, depth + 1);
        const r = treeShape(n.right, depth + 1);
        return { count: 1 + l.count + r.count, depth: Math.max(depth, l.depth, r.depth) };
    }

    // most: the most nodes and the deepest level of any frame, so that the
    // picture keeps its size while the tree grows and shrinks
    function drawTree(svg, frame, most) {
        svg.replaceChildren();
        const placed = [];
        let column = 0;
        const place = (n, depth) => {
            if (!n) return;
            place(n.left, depth + 1);
            placed.push({ node: n, x: column++, depth });
            place(n.right, depth + 1);
        };
        place(frame.root, 0);
        const at = new Map(placed.map((p) => [p.node.value, p]));
        const colW = 54;
        const width = Math.max(300, most.count * colW);
        const offset = (width - placed.length * colW) / 2;
        const sx = (x) => offset + (x + 0.5) * colW;
        const sy = (d) => 30 + d * 62;
        const edges = (n) => {
            if (!n) return;
            for (const child of [n.left, n.right]) {
                if (!child) continue;
                const a = at.get(n.value); const b = at.get(child.value);
                svg.appendChild(el('svg:line', { x1: sx(a.x), y1: sy(a.depth), x2: sx(b.x), y2: sy(b.depth), stroke: 'var(--algo-line)', 'stroke-width': 2 }));
                edges(child);
            }
        };
        edges(frame.root);
        for (const { node, x, depth: d } of placed) {
            const fill = frame.gone === node.value ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))' : frame.fresh === node.value ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            if (frame.focus === node.value) svg.appendChild(el('svg:circle', { cx: sx(x), cy: sy(d), r: 23, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 }));
            svg.appendChild(el('svg:circle', { cx: sx(x), cy: sy(d), r: 18, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { x: sx(x), y: sy(d) + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(node.value) }));
            if (frame.avl) svg.appendChild(el('svg:text', { x: sx(x), y: sy(d) + 32, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: `h${node.height}` }));
        }
        svg.setAttribute('viewBox', `0 0 ${width} ${Math.max(100, most.depth * 62 + (frame.avl ? 8 : 0))}`);
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

    function setUp(widget) {
        const algo = widget.dataset.algo;
        let frames;
        let draw;
        const svg = el('svg:svg', { class: 'algo-picture', role: 'img' });
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
            frames = traceTree(parseOps(widget.dataset.ops), algo === 'avl');
            svg.setAttribute('aria-label', 'Der Baum: links die kleineren, rechts die größeren Werte');
            side.hidden = true;
            const shapes = frames.map((f) => treeShape(f.root));
            const most = { count: Math.max(...shapes.map((t) => t.count)), depth: Math.max(...shapes.map((t) => t.depth)) };
            draw = (frame) => drawTree(svg, frame, most);
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
