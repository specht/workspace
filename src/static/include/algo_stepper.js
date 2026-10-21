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
    --algo-line: #adb5bd; --algo-ink: var(--bs-body-color); --algo-paper: var(--bs-body-bg); --algo-start: #2f9e44; --algo-goal: #e03131;
    --algo-pred: color-mix(in srgb, var(--algo-accent) 55%, var(--algo-paper)); }
html[data-bs-theme="dark"] .algo-stepper { --algo-accent: #b197fc; --algo-frontier: #c9a227; --algo-done: #5f3dc4; --algo-line: #5c636e; }
/* the pointers along which the calls on the stack went down */
.algo-stepper svg path.on-stack { stroke-width: 2.4; stroke-dasharray: 7 4; animation: algo-march 0.9s linear infinite; }
@keyframes algo-march { to { stroke-dashoffset: -11; } }

@media (prefers-reduced-motion: reduce) { .algo-stepper svg path.on-stack { animation: none; stroke-dasharray: none; } }
.algo-stepper .algo-title { font-weight: 600; font-size: 1.05em; margin: 0 0 0.5em; }
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
.algo-stepper .algo-timeline-box { margin: 0 0 0.8em; overflow-x: auto; scrollbar-width: none; }
.algo-stepper .algo-timeline-box::-webkit-scrollbar { display: none; }
.algo-stepper .algo-timeline-inner { min-width: calc(var(--steps) * 6px); padding-right: 4em; }
@media (pointer: coarse), (max-width: 576px) { .algo-stepper .algo-timeline-inner { min-width: calc(var(--steps) * 24px); } }
.algo-stepper .algo-timeline { margin: 0; }
.algo-stepper .algo-op-names { position: relative; font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.72em; }
.algo-stepper .algo-op-name { position: absolute; white-space: nowrap;
    color: var(--bs-secondary-color); line-height: 1.35; }
.algo-stepper .algo-op-name.current { color: var(--algo-ink); font-weight: 600; }
/* the parts of an operation below the dots: quieter, smaller, under a
   fine bracket over their steps, so that they read as part of the job above */
.algo-stepper .algo-parts { position: relative; font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.64em; }
.algo-stepper .algo-part-bracket { position: absolute; top: 0.15em; height: 0.35em; border: 1px solid color-mix(in srgb, var(--algo-line) 80%, transparent); border-top: none; border-radius: 0 0 3px 3px; }
.algo-stepper .algo-part-bracket.current { border-color: var(--algo-accent); }
.algo-stepper .algo-part-name { position: absolute; white-space: nowrap; color: color-mix(in srgb, var(--bs-secondary-color) 85%, transparent); line-height: 1.3; }
.algo-stepper .algo-part-name.current { color: var(--algo-accent); }
.algo-stepper .algo-part-leader { position: absolute; top: 0.5em; width: 1px; background: color-mix(in srgb, var(--algo-line) 70%, transparent); }
.algo-stepper .algo-op-leader { position: absolute; bottom: -0.3em; width: 1px; background: color-mix(in srgb, var(--algo-line) 70%, transparent); }
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

    // ---- Graph searches (reference/graphen). Every step is a line of the
    // page's pseudocode (GRAPH_CODE). A frame: found (node -> its
    // predecessor, null for the start), entries (the queue, the stack or the
    // priority queue in its order: key, node, priority, stale), taken (the
    // nodes that came out), dist, focus (the node, 'null' at the end of
    // path_back), neighbor and fresh (the value of new), look (the edge
    // looked along), path (path_back's list), code, stack, op, message.
    const GRAPH_CODE = {
        bfs: ['breadth_first_search(graph, start, goal):', '    predecessor ← { start: null }', '    queue ← [start]', '    while queue not empty:',
            '        node ← take the front node from queue', '        if node == goal: return path_back(predecessor, goal)', '        for each neighbor of node:',
            '            if neighbor not in predecessor:', '                predecessor[neighbor] ← node', '                add neighbor at the back of queue', '    no path'],
        dfs: ['depth_first_search(graph, start, goal):', '    predecessor ← { start: null }', '    stack ← [start]', '    while stack not empty:',
            '        node ← take the top node from stack', '        if node == goal: return path_back(predecessor, goal)', '        for each neighbor of node:',
            '            if neighbor not in predecessor:', '                predecessor[neighbor] ← node', '                put neighbor on top of stack', '    no path'],
        dijkstra: ['dijkstra(graph, start, goal):', '    dist ← { start: 0 }', '    predecessor ← { start: null }', '    finished ← empty set',
            '    pq ← priority queue with (0, start)', '    while pq not empty:', '        (distance, node) ← take the smallest entry from pq',
            '        if node in finished: continue        (ein alter Eintrag)', '        add node to finished', '        if node == goal: return path_back(predecessor, goal)',
            '        for each (neighbor, length) of node:', '            new ← distance + length', '            if neighbor not in dist or new < dist[neighbor]:',
            '                dist[neighbor] ← new', '                predecessor[neighbor] ← node', '                put (new, neighbor) into pq', '    no path'],
        astar: ['a_star(graph, start, goal):', '    dist ← { start: 0 }', '    predecessor ← { start: null }', '    finished ← empty set',
            '    pq ← priority queue with (0, start)', '    while pq not empty:', '        (_, node) ← take the smallest entry from pq        (anders als bei Dijkstra)',
            '        if node in finished: continue        (ein alter Eintrag)', '        add node to finished', '        if node == goal: return path_back(predecessor, goal)',
            '        for each (neighbor, length) of node:', '            new ← dist[node] + length        (anders als bei Dijkstra)', '            if neighbor not in dist or new < dist[neighbor]:',
            '                dist[neighbor] ← new', '                predecessor[neighbor] ← node', '                put (new + h(neighbor), neighbor) into pq        (anders als bei Dijkstra)', '    no path'],
        pathBack: ['path_back(predecessor, goal):', '    if goal not in predecessor: no path', '    path ← empty list', '    node ← goal', '    while node != null:',
            '        add node at the front of path', '        node ← predecessor[node]', '    return path'],
    };

    function traceGraph(graph, start, goal, kind) {
        const adj = neighbours(graph);
        const shortest = kind === 'dijkstra' || kind === 'astar';
        const h = (id) => (kind === 'astar' ? straight(graph, id, goal) : 0);
        const fn = { bfs: 'breadth_first_search', dfs: 'depth_first_search', dijkstra: 'dijkstra', astar: 'a_star' }[kind];
        const outer = { call: `${fn}(graph, ${start}, ${goal})` };
        const box = { bfs: 'queue', dfs: 'stack' }[kind] || 'pq';
        const lines = GRAPH_CODE[kind];
        const frames = [];
        // the names of the timeline's sections: the node that comes out
        frames.opNames = [];
        const found = {};
        const dist = {};
        const taken = [];
        let entries = [];
        let key = 0;
        let op = -1;
        const snapshot = (message, at, marks = {}) => frames.push({
            found: { ...found }, dist: shortest ? { ...dist } : null, taken: [...taken], op, message, code: { lines, at }, stack: [outer],
            entries: entries.map((e) => ({ ...e, stale: shortest && (taken.includes(e.node) || dist[e.node] + h(e.node) < e.priority) })), ...marks });
        const byPriority = (a, b) => a.priority - b.priority || (a.node < b.node ? -1 : 1) || a.key - b.key;

        const pathBack = () => {
            op = frames.opNames.length;
            frames.opNames.push('path_back');
            const inner = { call: `path_back(predecessor, ${goal})` };
            const P = (at) => ({ lines: GRAPH_CODE.pathBack, at });
            const path = [];
            const back = (message, at, marks) => snapshot(message, 0, { code: P(at), stack: [outer, inner], path: [...path], ...marks });
            back('path ← empty list: Der Weg ist noch leer. Er entsteht rückwärts, vom Ziel aus.', 2, {});
            back(`node ← goal: node ist ${goal}, das Ziel.`, 3, { focus: goal });
            for (let node = goal; node !== null;) {
                path.unshift(node);
                back(`add node at the front of path: ${node} kommt vorne dazu.`, 5, { focus: node });
                const before = found[node];
                back(before === null ? `node ← predecessor[node]: ${node} ist der Start, sein Vorgänger ist null.` : `node ← predecessor[node]: Der Pfeil von ${node} zeigt auf ${before}.`, 6, { focus: before === null ? 'null' : before });
                node = before;
            }
            back('while node != null: node ist null, die Schleife endet.', 4, { focus: 'null' });
            back(`return path: Der Weg ist ${path.join('–')}${shortest ? `, ${dist[goal]} lang` : `, ${path.length - 1} Kanten`}.`, 7, { done: true, stack: [{ ...outer, returns: path.join('–') }, { ...inner, returns: path.join('–') }] });
        };

        if (shortest) {
            dist[start] = 0;
            snapshot(`dist ← { ${start}: 0 }: Vom Start bis zum Start ist der Weg 0.`, 1, { changed: start });
            found[start] = null;
            snapshot(`predecessor ← { ${start}: null }: Der Start hat keinen Vorgänger.`, 2);
            entries.push({ key: key++, node: start, priority: 0 });
            snapshot(`pq ← priority queue with (0, ${start}): Am Anfang steht nur der Start in der Priority Queue. Die Priorität steht über dem Eintrag.`, 4);
        } else {
            found[start] = null;
            snapshot(`predecessor ← { ${start}: null }: ${start} ist gefunden, ohne Vorgänger.`, 1);
            entries.push({ key: key++, node: start });
            snapshot(`${box} ← [${start}]: Am Anfang liegt nur der Start ${kind === 'bfs' ? 'in der Queue' : 'auf dem Stack'}.`, 2);
        }
        while (entries.length) {
            const entry = kind === 'dfs' ? entries.pop() : entries.shift();
            const node = entry.node;
            const old = shortest && taken.includes(node);
            op = frames.opNames.length;
            frames.opNames.push(old ? `${node} (alt)` : node);
            if (shortest) {
                const take = kind === 'astar' ? `(_, node) ← take the smallest entry from pq: Die kleinste Priorität ist ${entry.priority}, node ist ${node}.`
                    : `(distance, node) ← take the smallest entry from pq: distance ist ${entry.priority}, node ist ${node}.`;
                snapshot(take, 6, { focus: node });
                if (old) {
                    snapshot(`if node in finished: continue: ${node} ist schon fertig. Ein alter Eintrag, er wird übersprungen.`, 7, { focus: node });
                    continue;
                }
                taken.push(node);
                snapshot(`add node to finished: ${node} ist fertig. Kürzer als ${dist[node]} geht es nicht mehr.`, 8, { focus: node });
            } else {
                taken.push(node);
                snapshot(`node ← take the ${kind === 'bfs' ? 'front node from queue' : 'top node from stack'}: ${node} ist dran${node === goal ? '' : ', nicht das Ziel'}.`, 4, { focus: node });
            }
            if (node === goal) {
                // path_back step by step in the first widget of the page (the
                // Breitensuche); the others show at once what it returns
                if (kind === 'bfs') {
                    snapshot(`if node == goal: ${node} ist das Ziel. Der Weg steht in den Pfeilen, rückwärts: return path_back(predecessor, goal).`, 5, { focus: node });
                    pathBack();
                    return frames;
                }
                const path = [];
                for (let n = goal; n !== null; n = found[n]) path.unshift(n);
                snapshot(`if node == goal: ${node} ist das Ziel. path_back liest den Weg rückwärts aus den Pfeilen (wie bei der Breitensuche): ${path.join('–')}${shortest ? `, ${dist[goal]} lang` : ''}.`,
                    shortest ? 9 : 5, { focus: node, path, done: true, stack: [{ ...outer, returns: path.join('–') }] });
                return frames;
            }
            for (const [next, length] of adj[node]) {
                const look = [node, next].sort().join('-');
                const here = { focus: node, neighbor: next, look };
                if (shortest) {
                    const fresh = dist[node] + length;
                    snapshot(`${kind === 'astar' ? 'new ← dist[node] + length' : 'new ← distance + length'}: über ${node} ist der Weg bis ${next} ${dist[node]} + ${length}, also ${fresh}.`, 11, { ...here, fresh });
                    const better = !(next in dist) || fresh < dist[next];
                    snapshot(next in dist ? `if … new < dist[neighbor]: ${fresh} ${better ? '<' : '≥'} ${dist[next]}, ${better ? 'ein kürzerer Weg' : 'nicht kürzer, nichts ändert sich'}.`
                        : `if neighbor not in dist: ${next} hat noch keinen Weg.`, 12, { ...here, fresh });
                    if (!better) continue;
                    dist[next] = fresh;
                    snapshot(`dist[neighbor] ← new: Der Weg bis ${next} ist jetzt ${fresh}.`, 13, { ...here, fresh, changed: next });
                    found[next] = node;
                    snapshot(`predecessor[neighbor] ← node: Der Pfeil von ${next} zeigt auf ${node}.`, 14, { ...here, fresh });
                    entries.push({ key: key++, node: next, priority: fresh + h(next) });
                    entries.sort(byPriority);
                    snapshot(kind === 'astar' ? `put (new + h(neighbor), neighbor) into pq: Priorität ${fresh} + ${h(next)}, also ${fresh + h(next)}: der Weg bis ${next} plus die Luftlinie von ${next} zum Ziel.`
                        : `put (new, neighbor) into pq: ${next} mit Priorität ${fresh}. Die Priority Queue hält die kleinste vorne.`, 15, { ...here, fresh });
                } else {
                    if (next in found) {
                        snapshot(`if neighbor not in predecessor: ${next} ist schon gefunden${found[next] === null ? ', der Start' : `, sein Vorgänger ist ${found[next]}`}.`, 7, here);
                        continue;
                    }
                    snapshot(`if neighbor not in predecessor: ${next} ist noch nicht gefunden.`, 7, here);
                    found[next] = node;
                    snapshot(`predecessor[neighbor] ← node: Der Pfeil von ${next} zeigt auf ${node}. Damit ist ${next} gefunden.`, 8, here);
                    entries.push({ key: key++, node: next });
                    snapshot(kind === 'bfs' ? `add neighbor at the back of queue: ${next} kommt hinten in die Queue.` : `put neighbor on top of stack: ${next} kommt oben auf den Stack.`, 9, here);
                }
            }
        }
        op = -1;
        snapshot(`no path: ${kind === 'bfs' ? 'Die Queue' : kind === 'dfs' ? 'Der Stack' : 'Die Priority Queue'} ist leer, ohne dass ${goal} dran war.`, shortest ? 16 : 10);
        return frames;
    }

    // Where things go in the graph's picture, worked out once for a widget:
    // the nodes, a place for each variable's label beside each node (clear
    // of the other nodes, the edges, the numbers), and the stub of the start's
    // predecessor null.
    function graphLayout(graph, start, kind) {
        const shortest = kind === 'dijkstra' || kind === 'astar';
        const at = {};
        for (const [id, [x, y]] of Object.entries(graph.nodes)) at[id] = [x * 5 + 60, y * 5 + 54];
        const width = 620;
        const rowY = 400;
        const segments = graph.edges.map(([a, b]) => [at[a], at[b]]);
        const rectPoint = (r, [px, py]) => Math.hypot(Math.max(r.x - px, 0, px - r.x - r.w), Math.max(r.y - py, 0, py - r.y - r.h));
        const rectSegment = (r, [p, q]) => {
            let best = Infinity;
            for (let s = 0; s <= 1; s += 0.02) best = Math.min(best, rectPoint(r, [p[0] + (q[0] - p[0]) * s, p[1] + (q[1] - p[1]) * s]));
            return best;
        };
        // the numbers beside a node: dist above, h below (A*)
        const marks = Object.values(at).flatMap(([x, y]) => [...(shortest ? [{ x: x - 14, y: y - 36, w: 28, h: 15 }] : []), ...(kind === 'astar' ? [{ x: x - 18, y: y + 21, w: 36, h: 14 }] : [])]);
        const weights = shortest ? segments.map(([p, q]) => ({ x: (p[0] + q[0]) / 2 - 12, y: (p[1] + q[1]) / 2 - 9, w: 24, h: 18 })) : [];
        const clear = (r, own) => {
            if (r.x < 4 || r.y < 4 || r.x + r.w > width - 4 || r.y + r.h > rowY - 34) return -Infinity;
            let score = Infinity;
            for (const [id, p] of Object.entries(at)) score = Math.min(score, rectPoint(r, p) - (id === own ? 0 : 22));
            for (const s of segments) score = Math.min(score, rectSegment(r, s) + 2);
            for (const m of [...marks, ...weights]) score = Math.min(score, Math.max(m.x - r.x - r.w, r.x - m.x - m.w, m.y - r.y - r.h, r.y - m.y - m.h, 0) + 1);
            return score;
        };
        // a box of w × h at gap from the node's centre, in the clearest of
        // sixteen directions; sideways a little preferred
        const spot = (id, w, h, gap) => {
            const [cx, cy] = at[id];
            let best = null;
            for (let k = 0; k < 16; k++) {
                const a = (k / 16) * 2 * Math.PI;
                const [dx, dy] = [Math.cos(a), Math.sin(a)];
                const d = gap + Math.abs(dx) * w / 2 + Math.abs(dy) * h / 2;
                const r = { x: cx + dx * d - w / 2, y: cy + dy * d - h / 2, w, h };
                const score = clear(r, id) + 4 * Math.abs(dx);
                if (!best || score > best.score) best = { score, x: r.x, y: r.y, dx, dy };
            }
            return best;
        };
        const nodeSpot = {};
        const neighborSpot = {};
        for (const id of Object.keys(at)) {
            nodeSpot[id] = spot(id, 44, 18, 48);
            neighborSpot[id] = spot(id, 74, shortest ? 40 : 18, 40);
        }
        // null: the start's predecessor, at the end of a short stub
        const stub = spot(start, 30, 16, 40);
        const nullAt = [stub.x + 15, stub.y + 8];
        return { at, width, rowY, nodeSpot, neighborSpot, nullAt, nodeAtNull: { x: nullAt[0] - 14.5 - 6 - 44, y: nullAt[1] - 9 } };
    }

    // A variable that holds a number: its name and, beside it, the value
    // (»new | 58«), no = sign between them
    const valueBox = (name, x, y, value) => {
        const w = 22 + 7.5 * (name.length + String(value).length);
        const split = x + 10 + 7.5 * name.length;
        return [
            el('svg:rect', { 'data-solid': '', 'data-key': `var-${name}`, x, y, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }),
            el('svg:line', { 'data-key': `var-${name}-split`, x1: split, y1: y, x2: split, y2: y + 18, stroke: 'var(--algo-accent)', 'stroke-width': 1 }),
            el('svg:text', { 'data-key': `var-${name}-label`, x: x + 5 + 3.75 * name.length, y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: name }),
            el('svg:text', { 'data-key': `var-${name}-value`, x: split + (x + w - split) / 2, y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-ink)', text: String(value) }),
        ];
    };
    const valueBoxWidth = (name, value) => 22 + 7.5 * (name.length + String(value).length);

    function drawGraph(svg, graph, frame, start, goal, kind, marker, layout) {
        svg.replaceChildren();
        const shortest = kind === 'dijkstra' || kind === 'astar';
        const { at, width, rowY } = layout;
        const R = 16;
        const label = (name, x, y) => [
            el('svg:rect', { 'data-solid': '', 'data-key': `var-${name}`, x, y, width: 14 + 7.5 * name.length, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }),
            el('svg:text', { 'data-key': `var-${name}-label`, x: x + (14 + 7.5 * name.length) / 2, y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: name }),
        ];
        // an arrow lets go and snaps onto its new node when it changes: the
        // variable (node, neighbor) holds another node now, a predecessor
        // was found shorter
        // It ends on the border of its target, outside the ring if the
        // target wears one (as the motion ends it, stepper_motion.js), also
        // after a jump, when nothing moves
        const pointer = (key, ends, [x1, y1], [x2, y2], extra = {}, rigid = false) => {
            const target = ends.split(' ')[1];
            const inset = target === 'focus' ? 22 + 2 : frame.focus && target === `node-${frame.focus}` ? 22 + 1.5 + 2 : R + 2;
            const d = Math.hypot(x2 - x1, y2 - y1) || 1;
            const s = d > inset ? (d - inset) / d : 0;
            return el('svg:path', { 'data-key': key, 'data-ends': ends, 'data-inset': 'auto', ...(rigid ? { 'data-rigid': '' } : {}),
                d: `M${x1} ${y1} L${x1 + (x2 - x1) * s} ${y1 + (y2 - y1) * s}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 1.5, 'marker-end': `url(#${marker}-accent)`, ...extra });
        };
        // the lengths stand in genuine gaps of their edges: a mask cuts the
        // edges and the arrows along them where a length is written
        const mid = (a, b) => [(at[a][0] + at[b][0]) / 2, (at[a][1] + at[b][1]) / 2];
        const gaps = shortest ? graph.edges.map(([a, b, w]) => {
            const [mx, my] = mid(a, b);
            const half = 3.6 * String(w).length + 4;
            return el('svg:rect', { x: mx - half, y: my - 8, width: 2 * half, height: 16, rx: 4, fill: 'black' });
        }) : [];
        const masked = shortest ? { mask: `url(#${marker}-gaps)` } : {};
        svg.appendChild(el('svg:defs', {}, [
            ...[['', 'var(--algo-line)'], ['-accent', 'var(--algo-accent)'], ['-pred', 'var(--algo-pred)']].map(([suffix, fill]) =>
                el('svg:marker', { id: `${marker}${suffix}`, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 9, markerHeight: 9, markerUnits: 'userSpaceOnUse', orient: 'auto-start-reverse' },
                    [el('svg:path', { d: 'M0 0L10 5L0 10z', fill })])),
            ...(shortest ? [el('svg:mask', { id: `${marker}-gaps`, maskUnits: 'userSpaceOnUse', x: -100, y: -100, width: width + 200, height: rowY + 300 },
                [el('svg:rect', { x: -100, y: -100, width: width + 200, height: rowY + 300, fill: 'white' }), ...gaps])] : []),
        ]));
        // the path at the end: the arrows along it, thick
        const onPath = new Set();
        if (frame.done) for (let n = goal; frame.found[n]; n = frame.found[n]) onPath.add(n);
        for (const [a, b, w] of graph.edges) {
            const key = [a, b].sort().join('-');
            const look = frame.look === key;
            svg.appendChild(el('svg:line', { 'data-key': `edge-${key}`, x1: at[a][0], y1: at[a][1], x2: at[b][0], y2: at[b][1], stroke: look ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': look ? 3 : 2, 'stroke-linecap': 'round', ...masked }));
        }
        // predecessor: an arrow from a node to the node it was found from;
        // it sprouts from its node and snaps on, and swings over when a
        // shorter way comes (stepper_motion.js)
        for (const [n, p] of Object.entries(frame.found)) {
            if (p === null) continue;
            const thick = onPath.has(n);
            svg.appendChild(pointer(`pred-${n}`, `node-${n} node-${p}`, at[n], at[p], { stroke: thick ? 'var(--algo-accent)' : 'var(--algo-pred)', 'stroke-width': thick ? 4 : 2.5,
                'marker-end': `url(#${marker}${thick ? '-accent' : '-pred'})`, ...masked }));
        }
        // the length of the edge looked along: bold and purple, like its edge
        if (shortest) for (const [a, b, w] of graph.edges) {
            const [mx, my] = mid(a, b);
            const look = frame.look === [a, b].sort().join('-');
            svg.appendChild(el('svg:text', { 'data-key': `weight-${a}${b}`, x: mx, y: my + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': look ? 700 : 400, fill: look ? 'var(--algo-accent)' : 'var(--algo-ink)', text: String(w) }));
        }
        // the start's predecessor: null at the end of a stub
        if (start in frame.found) {
            const [sx, sy] = at[start];
            const [nx, ny] = layout.nullAt;
            const d = Math.hypot(nx - sx, ny - sy);
            const [ux, uy] = [(nx - sx) / d, (ny - sy) / d];
            svg.appendChild(el('svg:line', { 'data-key': 'pred-null', x1: sx + ux * R, y1: sy + uy * R, x2: nx - ux * 17, y2: ny - uy * 17, stroke: 'var(--algo-pred)', 'stroke-width': 2.5, 'stroke-dasharray': '3 3' }));
            svg.appendChild(el('svg:text', { 'data-key': 'null-label', x: nx, y: ny + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--bs-secondary-color)', text: 'null' }));
        }
        // the variables' arrows, under the nodes; the labels come after them
        const labels = [];
        if (frame.focus && frame.focus !== 'null') {
            const s = layout.nodeSpot[frame.focus];
            svg.appendChild(pointer('pointer-node', 'var-node focus', [s.x + 22, s.y + 9], at[frame.focus]));
            labels.push(...label('node', s.x, s.y));
        } else if (frame.focus === 'null') labels.push(...label('node', layout.nodeAtNull.x, layout.nodeAtNull.y));
        if (frame.neighbor) {
            const s = layout.neighborSpot[frame.neighbor];
            const w = 14 + 7.5 * 'neighbor'.length;
            svg.appendChild(pointer('pointer-neighbor', `var-neighbor node-${frame.neighbor}`, [s.x + w / 2, s.y + 9], at[frame.neighbor]));
            labels.push(...label('neighbor', s.x, s.y));
            if (frame.fresh !== undefined) labels.push(...valueBox('new', s.x + (w - valueBoxWidth('new', frame.fresh)) / 2, s.y + 22, frame.fresh));
        }
        const inRow = new Set(frame.entries.filter((e) => !e.stale).map((e) => e.node));
        const taken = new Set(frame.taken);
        for (const [id, [x, y]] of Object.entries(at)) {
            const fill = taken.has(id) ? 'var(--algo-done)' : inRow.has(id) ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            const ring = id === start ? 'var(--algo-start)' : id === goal ? 'var(--algo-goal)' : 'var(--algo-ink)';
            svg.appendChild(el('svg:circle', { 'data-key': `node-${id}`, cx: x, cy: y, r: R, fill, stroke: ring, 'stroke-width': id === start || id === goal ? 3.5 : 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': `label-${id}`, x, y: y + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: id }));
            if (frame.dist && id in frame.dist) svg.appendChild(el('svg:text', { 'data-key': `dist-${id}`, x, y: y - 24, 'text-anchor': 'middle', 'font-size': 12,
                'font-weight': frame.changed === id ? 700 : 400, fill: frame.changed === id ? 'var(--algo-accent)' : 'var(--algo-ink)', text: String(frame.dist[id]) }));
            if (kind === 'astar') svg.appendChild(el('svg:text', { 'data-key': `h-${id}`, x, y: y + 32, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: `h ${straight(graph, id, goal)}` }));
        }
        if (frame.focus && frame.focus !== 'null') svg.appendChild(el('svg:circle', { 'data-key': 'focus', 'data-ring': `node-${frame.focus}`, cx: at[frame.focus][0], cy: at[frame.focus][1], r: 22, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 }));
        for (const node of labels) svg.appendChild(node);
        // the row under the graph: the queue, the stack (its top on the
        // right) or the priority queue (the smallest first), each entry a
        // token that comes out of its node and goes back into it
        const name = { bfs: 'queue', dfs: 'stack' }[kind] || 'pq';
        const token = (prefix, n, x, y, extra = []) => [
            el('svg:circle', { 'data-key': prefix, 'data-from': `node-${n}`, 'data-to': `node-${n}`, 'data-merge': '', cx: x, cy: y, r: 14, fill: 'var(--algo-frontier)', stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }),
            el('svg:text', { 'data-key': `${prefix}-label`, 'data-from': `label-${n}`, 'data-to': `label-${n}`, 'data-merge': '', x, y: y + 5, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: n }),
            ...extra];
        const rowLeft = 30 + 7.5 * name.length;
        svg.appendChild(el('svg:text', { 'data-key': 'row-name', x: 14, y: rowY + 4, 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: name }));
        frame.entries.forEach((e, i) => {
            const x = rowLeft + 18 + i * 36;
            const extra = [];
            if (shortest) extra.push(el('svg:text', { 'data-key': `entry-${e.key}-priority`, x, y: rowY - 20, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--algo-ink)', text: String(e.priority) }));
            if (e.stale) extra.push(el('svg:line', { 'data-key': `entry-${e.key}-strike`, x1: x - 15, y1: rowY + 15, x2: x + 15, y2: rowY - 15, stroke: 'var(--algo-goal)', 'stroke-width': 2 }));
            for (const node of token(`entry-${e.key}`, e.node, x, rowY, extra)) svg.appendChild(node);
            if (e.stale) svg.querySelector(`[data-key="entry-${e.key}"]`).setAttribute('fill', 'var(--algo-paper)');
        });
        if (!frame.entries.length) svg.appendChild(el('svg:text', { 'data-key': 'row-empty', x: rowLeft + 4, y: rowY + 4, 'font-size': 12, fill: 'var(--bs-secondary-color)', text: 'leer' }));
        // path_back's list, filled from the front: beside the row when the
        // row leaves room for it (layout.pathBeside), else under it
        const pathY = layout.pathBeside ? rowY : rowY + 50;
        const pathLeft = layout.pathBeside ? 330 : 14;
        if (frame.path) {
            svg.appendChild(el('svg:text', { 'data-key': 'path-name', x: pathLeft, y: pathY + 4, 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'path' }));
            frame.path.forEach((n, i) => {
                for (const node of token(`path-${n}`, n, pathLeft + 52 + i * 36, pathY)) svg.appendChild(node);
                svg.querySelector(`[data-key="path-${n}"]`).setAttribute('fill', frame.done ? 'var(--algo-accent)' : 'var(--algo-paper)');
                if (frame.done) svg.querySelector(`[data-key="path-${n}-label"]`).setAttribute('fill', 'var(--algo-paper)');
            });
        }
        svg.setAttribute('viewBox', `0 0 ${width} ${pathY + 22}`);
        svg.dataset.natural = width;
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
            '            if min == i: done', '            swap heap[min] and heap[i]', '            i ← min', '    return top'],
    };

    function traceHeap(ops, setup = []) {
        const slots = [];
        const values = {};
        let nextId = 0;
        let out = [];
        let current = -1;
        const frames = [];
        let aside = {};
        // phase: a part of the operation (sift up, sift down), for the timeline
        let phase = null;
        const snapshot = (message, marks = {}) => frames.push({ slots: [...slots], values: { ...values }, aside: { ...aside }, out: [...out], message, op: current, phase, ...marks });
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
            phase = 'sift up';
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
            phase = null;
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
                phase = 'sift down';
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
                        snapshot(`min == i: Kein Kind ist kleiner als ${v(i)}. Fertig.`, { holders, at: i, vars: vars(), code: P(12) });
                        break;
                    }
                    [slots[min], slots[i]] = [slots[i], slots[min]];
                    snapshot(`swap heap[min] and heap[i]: ${v(min)} und ${v(i)} tauschen die Plätze.`, { holders, at: i, vars: vars(), compare: [i, min], swap: true, code: P(13) });
                    i = min;
                    snapshot(`i ← min: i ist jetzt ${i}.`, { holders, at: i, vars: [{ name: 'i', index: i }, { name: 'min', index: min }], code: P(14) });
                }
            }
            phase = null;
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
        const entry = (id, x, y, i) => el('svg:g', { 'data-key': `tree-${id}`, 'data-grow': '', transform: `translate(${x} ${y})`, ...arc }, [
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
        for (const placed of indexLabels(frame.vars, (index) => cellX(index) + (cell - 3) / 2, width)) labels.push(...indexLabel(placed, top + 74 + placed.row * 24, top + 24, top + 50, marker));
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
        svg.setAttribute('viewBox', `0 0 ${width} ${top + 72 + 3 * 24}`);
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
        // phase: a part of the work (merge, partition), for the timeline
        let phase = null;
        const snapshot = (message, marks = {}) => frames.push({ slots: [...slots], values: { ...values }, lifted: lifted && { ...lifted }, scratch: scratch && [...scratch],
            stack: stack.map((entry) => ({ ...entry })), comparisons, writes, message, phase, code: { lines: SORT_CODE[kind], at: marks.line }, ...marks });
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
                stack.push({ call: `merge_sort(start: ${lo}, stop: ${hi})`, range: [lo, hi] });
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
                phase = `merge ${lo}–${hi - 1}`;
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
                phase = null;
                stack.pop();
            };
            sort(0, n);
        } else if (kind === 'quick') {
            const sort = (lo, hi) => {
                stack.push({ call: `quick_sort(first: ${lo}, last: ${hi})`, range: [lo, hi + 1] });
                const ends = [{ name: 'first', index: lo }, { name: 'last', index: hi }];
                if (hi <= lo) {
                    snapshot(`last ≤ first: Von ${lo} bis ${hi} steht ${hi === lo ? 'nur eine Zahl' : 'nichts'}, fertig.`, { line: 1, vars: ends.filter((e) => e.index >= 0 && e.index < n), range: [lo, hi + 1] });
                    stack.pop();
                    return;
                }
                const pivot = v(hi);
                phase = `partition ${lo}–${hi}`;
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
                phase = null;
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
            if (box.border !== undefined) {
                out.push(el('svg:line', { 'data-key': `border-${box.name}`, x1: group.x, y1: box.border, x2: group.x, y2: y, stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-linecap': 'round' }));
                return;
            }
            const centre = [box.x, y + 9];
            const f = (centre[1] - stop) / (centre[1] - aim);
            const end = [centre[0] + (group.x - centre[0]) * f, stop];
            const inset = Math.hypot(group.x - end[0], aim - end[1]);
            out.push(el('svg:path', { 'data-key': `pointer-${box.name}`, 'data-ends': `var-${box.name} slot-${group.index}`, 'data-inset': inset, 'data-rigid': '',
                d: `M${centre[0]} ${centre[1]} Q${(centre[0] + end[0]) / 2} ${(centre[1] + end[1]) / 2} ${end[0]} ${end[1]}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 1.5, 'marker-end': `url(#${marker}-accent)` }));
        });
        group.boxes.forEach((box) => {
            if (box.value !== undefined) { out.push(...valueBox(box.name, box.x - box.width / 2, y, box.value)); return; }
            out.push(el('svg:rect', { 'data-solid': '', 'data-key': `var-${box.name}`, x: box.x - box.width / 2, y, width: box.width, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            out.push(el('svg:text', { 'data-key': `var-${box.name}-label`, x: box.x, y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: box.text || box.name }));
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
        // the recursion: a band behind the bars for the current call's range,
        // and above the bars a bracket for every call on the stack, the
        // outer ones grey, the current one purple; they move with the calls
        const ranges = (frame.stack || []).map((entry) => entry.range).filter(Boolean);
        if (ranges.length) {
            const [a, b] = ranges[ranges.length - 1];
            if (b > a) svg.appendChild(el('svg:rect', { 'data-key': 'range-band', x: a * cell + 1, y: lift - 4, width: (b - a) * cell - 2, height: height + 6, rx: 6,
                fill: 'color-mix(in srgb, var(--algo-accent) 9%, transparent)', stroke: 'color-mix(in srgb, var(--algo-accent) 35%, transparent)', 'stroke-width': 1 }));
            ranges.forEach(([lo, hi], k) => {
                const current = k === ranges.length - 1;
                const y = 6 + k * 7;
                const x1 = lo * cell + 3;
                const x2 = Math.max(x1 + 4, hi * cell - 3);
                svg.appendChild(el('svg:path', { 'data-key': `range-${k}`, d: `M${x1} ${y + 5} L${x1} ${y} L${x2} ${y} L${x2} ${y + 5}`, fill: 'none',
                    stroke: current ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': current ? 2 : 1.2 }));
            });
        }
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
        const labelTop = baseline + 38;
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

    // ---- Lists (reference/listen): the Array List, the Linked List and the
    // Queue as a ring buffer, each step a line of the page's pseudocode
    // (LIST_CODE). A value has an id, so that a copy is a new id flying from
    // where it is copied from: for a moment it is there twice.
    const LIST_CODE = {
        arrayInsert: ['insert(i, value):', '    if i < 0 or i > size: error', '    if size == capacity: grow', '    for j from size - 1 down to i:',
            '        cells[j + 1] ← cells[j]', '    cells[i] ← value', '    size ← size + 1'],
        arrayRemove: ['remove_at(i):', '    if i < 0 or i ≥ size: error', '    value ← cells[i]', '    for j from i to size - 2:', '        cells[j] ← cells[j + 1]',
            '    size ← size - 1', '    return value'],
        linkedInsert: ['insert(i, value):', '    if i < 0 or i > size: error', '    new ← a node with value', '    if i == 0:', '        new.next ← head', '        head ← new',
            '    else:', '        before ← node_at(i - 1)', '        new.next ← before.next', '        before.next ← new', '    size ← size + 1'],
        linkedRemove: ['remove_at(i):', '    if i < 0 or i ≥ size: error', '    if i == 0:', '        value ← head.value', '        head ← head.next', '    else:',
            '        before ← node_at(i - 1)', '        value ← before.next.value', '        before.next ← before.next.next', '    size ← size - 1', '    return value'],
        linkedRemoveRecursive: ['remove_at(node, i):', '    if node == null: error', '    if i == 0: return node.next', '    node.next ← remove_at(node.next, i - 1)', '    return node'],
        nodeAt: ['node_at(i):', '    k ← head', '    repeat i times:', '        k ← k.next', '    return k'],
        enqueue: ['enqueue(value):', '    if size == capacity: error, the queue is full', '    cells[(head + size) mod capacity] ← value', '    size ← size + 1'],
        dequeue: ['dequeue():', '    if size == 0: error, the queue is empty', '    value ← cells[head]', '    head ← (head + 1) mod capacity', '    size ← size - 1', '    return value'],
    };

    // A frame of an array: cells (ids or null), values (id -> value), size,
    // capacity, bigger (while growing: the new array's cells), card (the
    // variable value: { id, at } above a cell), vars (index variables),
    // marks: focus, from (id -> the id a copy comes from), op, code, stack.
    function traceArray(ops, capacity, setup = []) {
        let cells = Array(capacity).fill(null);
        let size = 0;
        let bigger = null;
        let card = null;
        let nextId = 0;
        const values = {};
        let current = -1;
        let phase = null;
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ cells: [...cells], values: { ...values }, size, capacity: cells.length, bigger: bigger && [...bigger], card: card && { ...card },
            message, op: current, phase, ...marks });
        const I = (at) => ({ lines: LIST_CODE.arrayInsert, at });
        const R = (at) => ({ lines: LIST_CODE.arrayRemove, at });
        const fresh = (value) => { const id = nextId++; values[id] = value; return id; };
        const copy = (id) => fresh(values[id]);
        const insert = (i, value, quiet) => {
            const vars = (j) => [{ name: 'i', index: i }, { name: 'size', index: size }, ...(j !== undefined ? [{ name: 'j', index: j }] : [])];
            const say = quiet ? () => {} : snapshot;
            card = { id: fresh(value), at: i };
            say(`insert(${i}, ${value}): Der neue Wert ${value} soll an Index ${i}.`, { vars: vars(), code: I(0) });
            if (i < 0 || i > size) { card = null; say(`Index ${i} gibt es nicht: erlaubt ist 0 bis ${size}.`, { code: I(1) }); return; }
            if (size === cells.length) {
                phase = 'grow';
                bigger = Array(cells.length * 2).fill(null);
                say(`size == capacity: Alle ${cells.length} Zellen sind belegt. grow: ein neues Array mit ${bigger.length} Zellen.`, { vars: vars(), code: I(2) });
                const from = {};
                for (let k = 0; k < size; k++) { bigger[k] = copy(cells[k]); from[bigger[k]] = cells[k]; }
                say(`Alle ${size} Werte werden ins neue Array kopiert, jeder einzeln.`, { vars: vars(), code: I(2), from });
                cells = bigger;
                bigger = null;
                say(`Das neue Array ersetzt das alte, das jetzt niemand mehr braucht.`, { vars: vars(), code: I(2) });
                phase = null;
            }
            if (size > i) phase = 'shift';
            for (let j = size - 1; j >= i; j--) {
                const id = copy(cells[j]);
                cells[j + 1] = id;
                say(`cells[j + 1] ← cells[j]: ${values[id]} rückt von Index ${j} nach ${j + 1}. Jetzt steht ${values[id]} zweimal da.`, { vars: vars(j), code: I(4), from: { [id]: cells[j] } });
            }
            phase = null;
            cells[i] = card.id;
            card = null;
            say(`cells[i] ← value: ${value} kommt an Index ${i}.`, { vars: vars(), code: I(5), focus: i });
            size += 1;
            say(`size ← size + 1: size ist jetzt ${size}.`, { vars: vars(), code: I(6) });
        };
        const removeAt = (i) => {
            const vars = (j) => [{ name: 'i', index: i }, { name: 'size', index: size }, ...(j !== undefined ? [{ name: 'j', index: j }] : [])];
            snapshot(`remove_at(${i}): Der Wert an Index ${i} soll heraus.`, { vars: vars(), code: R(0), focus: i });
            if (i < 0 || i >= size) { snapshot(`Index ${i} gibt es nicht.`, { code: R(1) }); return; }
            card = { id: copy(cells[i]), at: i };
            snapshot(`value ← cells[i]: Die Variable value merkt sich ${values[card.id]}.`, { vars: vars(), code: R(2), from: { [card.id]: cells[i] } });
            if (i < size - 1) phase = 'shift';
            for (let j = i; j <= size - 2; j++) {
                const id = copy(cells[j + 1]);
                cells[j] = id;
                snapshot(`cells[j] ← cells[j + 1]: ${values[id]} rückt von Index ${j + 1} nach ${j}.${j === size - 2 ? ` An Index ${j + 1} steht es noch, aber das gehört gleich nicht mehr zur Liste.` : ''}`, { vars: vars(j), code: R(4), from: { [id]: cells[j + 1] } });
            }
            phase = null;
            size -= 1;
            snapshot(`size ← size - 1: size ist jetzt ${size}. Was ab Index ${size} steht, zählt nicht mehr.`, { vars: vars(), code: R(5) });
            const out = values[card.id];
            card = null;
            snapshot(`return value: ${out} kommt heraus.`, { vars: vars(), code: R(6) });
        };
        for (const op of setup) if (op.type === 'insert') { insert(op.index, op.value, true); }
        snapshot(setup.length ? 'Die Liste von oben. Jetzt der Reihe nach die Operationen darüber.' : `Eine leere Array List: ein Array mit ${capacity} Zellen, size ist 0.`, { vars: [{ name: 'size', index: size }] });
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'insert') insert(op.index, op.value);
            else if (op.type === 'remove_at') removeAt(op.index);
        }
        return frames;
    }

    // The array: a row of cells, the cells from size on faint (they do not
    // belong to the list); a value is a group that moves; the card of value
    // above its cell; while growing the new array below.
    function drawArray(svg, frame, most, marker, grows) {
        svg.replaceChildren();
        const cell = 50;
        const left = 14;
        const top = 46;
        // the new array while growing: under the rows of the variables
        const bigTop = top + 90 + 2 * 24 + 22;
        svg.appendChild(arrowheads(marker));
        // The cells and values are drawn as in the list; right of the size
        // border a faint copy lies over them (uses of them, clipped to the
        // area right of the border, the look through CSS variables), so a
        // cell turns solid exactly where the border sweeps over it
        const look = (fill, stroke, dash) => `fill: var(--cell-fill, ${fill}); stroke: var(--cell-stroke, ${stroke}); stroke-width: var(--cell-width, 1.5); stroke-dasharray: var(--cell-dash, ${dash})`;
        const box = (key, x, y) => el('svg:rect', { 'data-key': key, id: `${marker}-${key}`, x, y, width: cell - 6, height: 42, rx: 6, style: look('var(--algo-paper)', 'var(--algo-line)', 'none') });
        const value = (id, x, y, extra = {}) => el('svg:g', { 'data-key': `value-${id}`, id: `${marker}-value-${id}`, 'data-grow': `${(cell - 6) / 2} 21`, ...(frame.from && frame.from[id] !== undefined ? { 'data-from': `value-${frame.from[id]}` } : {}),
            transform: `translate(${x} ${y})` }, [
            el('svg:rect', { x: 0, y: 0, width: cell - 6, height: 42, rx: 6, style: look(extra.fill || 'var(--algo-paper)', extra.stroke || 'var(--algo-ink)', extra.dash || 'none') }),
            el('svg:text', { x: (cell - 6) / 2, y: 27, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.values[id]) })]);
        frame.cells.forEach((id, i) => {
            const x = left + i * cell;
            svg.appendChild(box(`cell-${i}`, x, top));
            svg.appendChild(el('svg:text', { 'data-key': `index-${i}`, x: x + (cell - 6) / 2, y: top + 58, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(el('svg:rect', { 'data-key': `slot-${i}`, x: x + 2, y: top + 34, width: cell - 10, height: 4, fill: 'none', stroke: 'none' }));
        });
        svg.appendChild(el('svg:rect', { 'data-key': `slot-${frame.cells.length}`, x: left + frame.cells.length * cell + 2, y: top + 34, width: cell - 10, height: 4, fill: 'none', stroke: 'none' }));
        // size is no cell but the border after the used ones: a line in the
        // gap between two cells, labelled with its value (indexLabel draws
        // it); at the end of the array the list is full
        const border = (n) => left + n * cell - 3;
        const sizeVar = (frame.vars || []).find((v) => v.name === 'size');
        if (sizeVar && sizeVar.index === frame.cells.length) svg.appendChild(el('svg:text', { 'data-key': 'full', x: border(sizeVar.index) + 8, y: top + 26, 'font-size': 12, fill: 'var(--algo-goal)', text: 'voll' }));
        frame.cells.forEach((id, i) => {
            if (id !== null) svg.appendChild(value(id, left + i * cell, top, frame.focus === i && i < frame.size ? { fill: 'var(--algo-frontier)' } : {}));
        });
        // the faint copy: first the outline covered in the paper's colour
        // (the dashes would let it show), then dashed and grey
        const area = `${marker}-outside`;
        svg.appendChild(el('svg:rect', { 'data-key': 'outside-area', id: `${area}-area`, x: border(frame.size), y: top - 4, width: (frame.cells.length + 2) * cell, height: 50, fill: 'none', stroke: 'none' }));
        const cover = '--cell-stroke: var(--algo-paper); --cell-width: 3';
        const copy = (key, style) => el('svg:use', { href: `#${marker}-${key}`, style });
        svg.appendChild(el('svg:g', { 'data-solid': '', 'clip-path': `url(#${area})`, 'aria-hidden': 'true' }, [
            el('svg:clipPath', { id: area }, [el('svg:use', { href: `#${area}-area` })]),
            ...frame.cells.flatMap((id, i) => [copy(`cell-${i}`, cover), copy(`cell-${i}`, '--cell-fill: var(--algo-paper); --cell-dash: 4 4')]),
            ...frame.cells.filter((id) => id !== null).flatMap((id) => [copy(`value-${id}`, cover),
                copy(`value-${id}`, '--cell-fill: color-mix(in srgb, var(--algo-line) 35%, var(--algo-paper)); --cell-stroke: var(--algo-line); --cell-dash: 3 3')]),
        ]));
        if (frame.bigger) {
            frame.bigger.forEach((id, i) => {
                const x = left + i * cell;
                svg.appendChild(box(`big-${i}`, x, bigTop));
                if (id !== null) svg.appendChild(value(id, x, bigTop));
            });
            svg.appendChild(el('svg:text', { 'data-key': 'big-label', x: left, y: bigTop - 8, 'font-size': 11, fill: 'var(--bs-secondary-color)', text: `neues Array, ${frame.bigger.length} Zellen` }));
        }
        if (frame.card) {
            const x = left + frame.card.at * cell;
            svg.appendChild(value(frame.card.id, x, 0, { fill: 'color-mix(in srgb, var(--algo-accent) 25%, var(--algo-paper))', stroke: 'var(--algo-accent)', dash: '5 3' }));
            const w = 14 + 7.5 * 5;
            svg.appendChild(el('svg:rect', { 'data-solid': '', 'data-key': 'var-value', x: x + cell - 2, y: 12, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': 'var-value-label', x: x + cell - 2 + w / 2, y: 25, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'value' }));
        }
        const labelTop = top + 90;
        const vars = (frame.vars || []).map((v) => (v.name === 'size' ? { ...v, index: `b${v.index}`, value: v.index, border: top - 6 } : v));
        const at = (index) => (typeof index === 'string' ? border(Number(index.slice(1))) : left + index * cell + (cell - 6) / 2);
        const placed = indexLabels(vars, at, left * 2 + (most + 1) * cell);
        // all lines under all boxes: the border moves past other boxes
        const labels = placed.flatMap((label) => indexLabel(label, labelTop + label.row * 24, top + 36, top + 66, marker));
        const under = (n) => /^(pointer|border)-/.test(n.dataset.key);
        for (const node of [...labels.filter(under), ...labels.filter((n) => !under(n))]) svg.appendChild(node);
        svg.appendChild(el('svg:text', { 'data-key': 'capacity', x: left + (most + 1) * cell - 6, y: 24, 'text-anchor': 'end', 'font-size': 12, fill: 'var(--bs-secondary-color)', text: `capacity ${frame.capacity}` }));
        // the picture keeps its size: room for the new array when the example grows
        svg.setAttribute('viewBox', `0 0 ${left * 2 + (most + 1) * cell} ${grows ? bigTop + 54 : labelTop + 3 * 24}`);
        svg.dataset.natural = left * 2 + (most + 1) * cell;
    }

    // A frame of a Linked List: nodes (id -> { value, next }), head, order
    // (the ids from head on), loose (a node no pointer of the list reaches:
    // { id, at } above the gap at index at), vars (variables that point at a
    // node: head always, new, before, k), card (value), marks.
    // recursive: remove_at(node, i) as on the page, the call stack beside it
    function traceLinked(ops, setup = [], recursive = false) {
        const nodes = {};
        let head = null;
        let nextId = 0;
        let loose = null;
        let card = null;
        let current = -1;
        let phase = null;
        let stack = [];
        const frames = [];
        const order = () => { const list = []; for (let k = head; k !== null && list.length < 50; k = nodes[k].next) list.push(k); return list; };
        const snapshot = (message, marks = {}) => frames.push({ nodes: JSON.parse(JSON.stringify(nodes)), head, order: order(), loose: loose && { ...loose }, card: card && { ...card },
            message, op: current, phase, stack: stack.map((s) => ({ ...s })), ...marks });
        const I = (at) => ({ lines: LIST_CODE.linkedInsert, at });
        const R = (at) => ({ lines: LIST_CODE.linkedRemove, at });
        const N = (at) => ({ lines: LIST_CODE.nodeAt, at });
        const size = () => order().length;
        // node_at(i): k walks from head, one next at a time
        const nodeAt = (i, vars, quiet) => {
            const say = quiet ? () => {} : snapshot;
            stack.push({ call: `node_at(i: ${i})`, node: null });
            let k = head;
            phase = 'node_at';
            say(`node_at(${i}): k ← head, k zeigt auf ${nodes[k].value}.`, { vars: [...vars, { name: 'k', to: k }], code: N(1) });
            for (let step = 0; step < i; step++) {
                k = nodes[k].next;
                say(`k ← k.next: k zeigt auf ${nodes[k].value}.`, { vars: [...vars, { name: 'k', to: k }], code: N(3) });
            }
            say(`return k: der Knoten mit ${nodes[k].value}.`, { vars: [...vars, { name: 'k', to: k }], code: N(4) });
            phase = null;
            stack.pop();
            return k;
        };
        const insert = (i, value, quiet) => {
            const say = quiet ? () => {} : snapshot;
            stack = [{ call: `insert(i: ${i}, value: ${value})`, node: null }];
            if (i < 0 || i > size()) { say(`insert(${i}, ${value}): Index ${i} gibt es nicht.`, { code: I(1) }); stack = []; return; }
            const id = nextId++;
            nodes[id] = { value, next: null };
            loose = { id, at: i };
            say(`new ← a node with value: ein neuer Knoten mit ${value}, sein next ist noch null.`, { vars: [{ name: 'new', to: id }], code: I(2), fresh: id });
            if (i === 0) {
                nodes[id].next = head;
                say(head !== null ? `new.next ← head: Der neue Knoten zeigt auf ${nodes[head].value}, den bisher ersten.` : 'new.next ← head: head ist null, also wird new.next auch null.', { vars: [{ name: 'new', to: id }], code: I(4), fresh: id });
                head = id;
                loose = null;
                say(`head ← new: head zeigt jetzt auf den neuen Knoten. Nichts musste laufen.`, { vars: [{ name: 'new', to: id }], code: I(5), fresh: id });
            } else {
                say(`i ist ${i}, nicht 0: Der Knoten davor muss gefunden werden.`, { vars: [{ name: 'new', to: id }], code: I(6), fresh: id });
                const before = nodeAt(i - 1, [{ name: 'new', to: id }], quiet);
                const both = [{ name: 'new', to: id }, { name: 'before', to: before }];
                say(`before ← node_at(${i - 1}): before ist der Knoten mit ${nodes[before].value}.`, { vars: both, code: I(7), fresh: id });
                nodes[id].next = nodes[before].next;
                say(nodes[id].next !== null ? `new.next ← before.next: Der neue Knoten zeigt auf ${nodes[nodes[id].next].value}. Jetzt zeigen zwei Zeiger auf ${nodes[nodes[id].next].value}.` : 'new.next ← before.next: before.next ist null, also wird new.next auch null.',
                    { vars: both, code: I(8), fresh: id });
                nodes[before].next = id;
                loose = null;
                say(`before.next ← new: ${nodes[before].value} zeigt jetzt auf den neuen Knoten. Der ist damit in der Liste, nichts wurde verschoben.`, { vars: both, code: I(9), fresh: id });
            }
            say(`size ← size + 1: size ist jetzt ${size()}.`, { code: I(10) });
            stack = [];
        };
        const removeAt = (i) => {
            stack = [{ call: `remove_at(i: ${i})`, node: null }];
            if (i < 0 || i >= size()) { snapshot(`remove_at(${i}): Index ${i} gibt es nicht.`, { code: R(1) }); stack = []; return; }
            let gone;
            if (i === 0) {
                gone = head;
                snapshot(`remove_at(0): i ist 0, der erste Knoten soll heraus.`, { code: R(2), focus: gone });
                card = { value: nodes[gone].value, from: gone };
                snapshot(`value ← head.value: Die Variable value merkt sich ${card.value}.`, { code: R(3), focus: gone });
                head = nodes[gone].next;
                loose = { id: gone, at: 0 };
                snapshot(`head ← head.next: head ${head !== null ? `zeigt jetzt auf ${nodes[head].value}` : 'ist jetzt null'}. ${nodes[gone].value} hängt an keinem Zeiger der Liste mehr.`, { code: R(4), focus: gone });
            } else {
                snapshot(`remove_at(${i}): Der Knoten davor muss gefunden werden.`, { code: R(5) });
                const before = nodeAt(i - 1, []);
                gone = nodes[before].next;
                snapshot(`before ← node_at(${i - 1}): before ist der Knoten mit ${nodes[before].value}.`, { vars: [{ name: 'before', to: before }], code: R(6), focus: gone });
                card = { value: nodes[gone].value, from: gone };
                snapshot(`value ← before.next.value: Die Variable value merkt sich ${card.value}.`, { vars: [{ name: 'before', to: before }], code: R(7), focus: gone });
                nodes[before].next = nodes[gone].next;
                loose = { id: gone, at: i };
                snapshot(`before.next ← before.next.next: ${nodes[before].value} zeigt jetzt an ${nodes[gone].value} vorbei ${nodes[before].next !== null ? `auf ${nodes[nodes[before].next].value}` : ', sein next ist jetzt null'}. ${nodes[gone].value} hängt an keinem Zeiger der Liste mehr.`,
                    { vars: [{ name: 'before', to: before }], code: R(8), focus: gone });
            }
            delete nodes[gone];
            loose = null;
            snapshot(`size ← size - 1: size ist jetzt ${size()}. Den Knoten erreicht niemand mehr, er ist weg.`, { code: R(9) });
            const out = card.value;
            card = null;
            snapshot(`return value: ${out} kommt heraus.`, { code: R(10) });
            stack = [];
        };
        // remove_at(node, i), recursive: each call goes one node further with
        // i - 1; the call with i = 0 returns node.next, and its caller puts
        // that into its next field: the node is skipped. On the way back
        // every call returns its node, and head ← remove_at(head, i) at the end.
        const removeAtRecursive = (i) => {
            const RR = (at) => ({ lines: LIST_CODE.linkedRemoveRecursive, at });
            const calls = [];
            const path = [];
            let node = head;
            for (let left = i; ; left--) {
                calls.push({ call: `remove_at(node: ${node === null ? 'null' : nodes[node].value}, i: ${left})`, node });
                stack = calls.map((c) => ({ ...c }));
                const here = node === null ? [] : [{ name: 'node', to: node }];
                if (node === null) { snapshot('if node == null: Die Liste ist zu Ende, so viele Knoten hat sie nicht.', { code: RR(1) }); stack = []; return; }
                if (left === 0) {
                    const next = nodes[node].next;
                    calls[calls.length - 1].returns = next === null ? 'null' : String(nodes[next].value);
                    stack = calls.map((c) => ({ ...c }));
                    snapshot(`if i == 0: Das ist der Knoten, der heraus soll. return node.next: Der Aufruf gibt ${next === null ? 'null' : nodes[next].value} zurück, den Knoten danach.`, { vars: here, code: RR(2), focus: node });
                    const gone = node;
                    calls.pop();
                    path.pop();
                    if (calls.length) {
                        const caller = calls[calls.length - 1].node;
                        nodes[caller].next = next;
                        loose = { id: gone, at: i };
                        stack = calls.map((c) => ({ ...c }));
                        snapshot(`node.next ← remove_at(node.next, i - 1): ${nodes[caller].value} trägt ${next === null ? 'null' : nodes[next].value} in sein next ein und zeigt damit an ${nodes[gone].value} vorbei. ${nodes[gone].value} hängt an keinem Zeiger der Liste mehr.`,
                            { vars: [{ name: 'node', to: caller }], code: RR(3), focus: gone, pulses: [{ edge: `next-${caller}`, delay: 0.6 }] });
                    } else {
                        head = next;
                        loose = { id: gone, at: 0 };
                        snapshot(`head ← remove_at(head, i): head ${next === null ? 'ist jetzt null' : `zeigt jetzt auf ${nodes[next].value}`}. ${nodes[gone].value} hängt an keinem Zeiger der Liste mehr.`, { code: RR(2), focus: gone });
                    }
                    delete nodes[gone];
                    loose = null;
                    break;
                }
                snapshot(`i ist ${left}, nicht 0: node.next ← remove_at(node.next, ${left - 1}), weiter zum nächsten Knoten.`, { vars: here, code: RR(3) });
                path.push(`next-${node}`);
                node = nodes[node].next;
            }
            // back up: every call returns its node and its caller puts it
            // back into its next field, the lowest first, head last
            const edges = [...path].reverse().concat(['pointer-head']);
            stack = calls.map((c) => ({ ...c, returns: String(nodes[c.node].value) }));
            snapshot('return node: Jeder Aufruf darüber gibt seinen Knoten zurück, und der Aufrufer trägt ihn wieder in sein next ein, zuletzt head ← remove_at(head, i). Die Zeiger zeigen danach auf dasselbe wie vorher.',
                { code: RR(4), returning: true, pulses: edges.map((edge, k) => ({ edge, delay: k / edges.length, length: 0.6 / edges.length })), duration: Math.max(MOTION_TIME, edges.length * PULSE_TIME) });
            stack = [];
        };
        for (const op of setup) if (op.type === 'insert') insert(op.index, op.value, true);
        snapshot(setup.length ? 'Die Liste von oben. Jetzt der Reihe nach die Operationen darüber.' : 'Eine leere Linked List: head zeigt auf keinen Knoten.');
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'insert') insert(op.index, op.value);
            else if (op.type === 'remove_at') (recursive ? removeAtRecursive : removeAt)(op.index);
        }
        return frames;
    }

    // The Linked List: the nodes from head on in a row, each a box with its
    // value and its next field; next is an arrow from that field to the next
    // node, a pointer of its node (sprouting, swinging, shrinking); a node
    // the list does not reach (new, or removed) stands above its gap.
    function drawLinked(svg, frame, most, marker) {
        svg.replaceChildren();
        const box = 66;
        const gap = 34;
        const top = 92;
        const x0 = 78;
        const place = new Map();
        frame.order.forEach((id, i) => place.set(id, [x0 + i * (box + gap), top]));
        if (frame.loose && frame.nodes[frame.loose.id]) place.set(frame.loose.id, [x0 + frame.loose.at * (box + gap) - (box + gap) / 2, top - 74]);
        svg.appendChild(arrowheads(marker));
        const arrow = (key, ends, [x1, y1], [x2, y2], extra = {}) => el('svg:path', { 'data-key': key, 'data-ends': ends, 'data-inset': 2,
            d: `M${x1} ${y1} Q${(x1 + x2) / 2} ${(y1 + y2) / 2} ${x2} ${y2}`, fill: 'none', stroke: 'var(--algo-line)', 'stroke-width': 2, 'marker-end': `url(#${marker})`, ...extra });
        // the way the recursive calls went down, as in the trees: purple,
        // flowing; on the way back each settles once its pulse has passed
        const chain = new Set();
        const onPath = (frame.stack || []).map((s) => s.node).filter((id) => id !== null && id !== undefined && frame.nodes[id]);
        if (onPath.length && onPath[0] === frame.head) chain.add('pointer-head');
        for (let k = 0; k + 1 < onPath.length; k++) if (frame.nodes[onPath[k]].next === onPath[k + 1]) chain.add(`next-${onPath[k]}`);
        const settle = new Map();
        if (frame.returning) {
            const slow = (window.StepperMotion && window.StepperMotion.slow) || 1;
            for (const pulse of frame.pulses || []) settle.set(pulse.edge, ((pulse.delay || 0) + (pulse.length || 0.4)) * (frame.duration || MOTION_TIME) * slow / 1000);
        }
        const marked = (key, grey) => (chain.has(key) ? { class: 'on-stack', stroke: 'var(--algo-accent)', 'marker-end': `url(#${marker}-accent)`,
            ...(settle.has(key) ? { 'data-settle': settle.get(key).toFixed(2), 'data-settled-stroke': grey ? 'var(--algo-line)' : 'var(--algo-accent)', 'data-settled-head': grey ? `url(#${marker})` : `url(#${marker}-accent)` } : {}) } : {});
        // the next pointers, behind the nodes
        for (const [id, [x, y]] of place) {
            const node = frame.nodes[id];
            if (node.next === null || !place.has(node.next)) continue;
            const [tx, ty] = place.get(node.next);
            svg.appendChild(arrow(`next-${id}`, `field-${id} in-${node.next}`, [x + box * 0.8, y + 22], [tx - 2, ty + 22], marked(`next-${id}`, true)));
        }
        // the variables: head on the left, the others below their node
        const headTo = frame.head !== null && place.has(frame.head) ? place.get(frame.head) : null;
        const w = 14 + 7.5 * 4;
        svg.appendChild(el('svg:rect', { 'data-solid': '', 'data-key': 'var-head', x: 4, y: top + 13, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
        if (headTo) svg.insertBefore(arrow('pointer-head', `var-head in-${frame.head}`, [4 + w / 2, top + 22], [headTo[0] - 2, headTo[1] + 22], { stroke: 'var(--algo-accent)', 'marker-end': `url(#${marker}-accent)`, ...marked('pointer-head', false) }), svg.lastChild);
        for (const node of pulseNodes(frame.pulses)) svg.insertBefore(node, svg.lastChild);
        svg.appendChild(el('svg:text', { 'data-key': 'var-head-label', x: 4 + w / 2, y: top + 26, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'head' }));
        if (!headTo) svg.appendChild(nullBeside('head-empty', 4 + w, top + 13));
        for (const [id, [x, y]] of place) {
            const node = frame.nodes[id];
            const fill = frame.focus === id ? 'color-mix(in srgb, var(--algo-goal) 30%, var(--algo-paper))' : frame.fresh === id ? 'var(--algo-frontier)' : 'var(--algo-paper)';
            svg.appendChild(el('svg:g', { 'data-key': `node-${id}`, 'data-grow': `${box / 2} 22`, transform: `translate(${x} ${y})` }, [
                el('svg:rect', { x: 0, y: 0, width: box, height: 44, rx: 6, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }),
                el('svg:line', { x1: box * 0.6, y1: 0, x2: box * 0.6, y2: 44, stroke: 'var(--algo-line)', 'stroke-width': 1.5 }),
                el('svg:text', { x: box * 0.3, y: 28, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(node.value) }),
                ...(node.next === null ? [el('svg:text', { x: box * 0.8, y: 26, 'text-anchor': 'middle', 'font-size': 11, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--bs-secondary-color)', text: 'null' })] : []),
            ]));
            // anchors: the next field (where its arrow starts), the left side
            // (where arrows arrive), the bottom (where variables point)
            svg.appendChild(el('svg:circle', { 'data-key': `field-${id}`, cx: x + box * 0.8, cy: y + 22, r: 0, fill: 'none' }));
            svg.appendChild(el('svg:circle', { 'data-key': `in-${id}`, cx: x - 2, cy: y + 22, r: 0, fill: 'none' }));
            svg.appendChild(el('svg:rect', { 'data-key': `slot-${id}`, x: x + box * 0.3 - 2, y: y + 38, width: 4, height: 4, fill: 'none', stroke: 'none' }));
        }
        frame.order.forEach((id, i) => {
            const [x, y] = place.get(id);
            svg.appendChild(el('svg:text', { 'data-key': `index-${i}`, x: x + box * 0.3, y: y + 58, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
        });
        if (frame.card) {
            const at = place.get(frame.card.from) || [x0, top];
            svg.appendChild(el('svg:g', { 'data-key': 'card', 'data-grow': `${box * 0.3} 16`, transform: `translate(${at[0]} ${top - 74})` }, [
                el('svg:rect', { x: 0, y: 0, width: box * 0.6, height: 32, rx: 5, fill: 'color-mix(in srgb, var(--algo-accent) 25%, var(--algo-paper))', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '5 3' }),
                el('svg:text', { x: box * 0.3, y: 22, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.card.value) })]));
            svg.appendChild(el('svg:text', { 'data-key': 'card-label', x: at[0] + box * 0.6 + 6, y: top - 53, 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'value' }));
        }
        // a variable that points at the node outside the list stands to its
        // left, its arrow to the node's left side
        const isLoose = (id) => frame.loose && frame.loose.id === id;
        for (const v of (frame.vars || []).filter((v) => isLoose(v.to))) {
            const [nx, ny] = place.get(v.to);
            const lw = 14 + 7.5 * v.name.length;
            const lx = nx - 40 - lw;
            svg.appendChild(arrow(`pointer-${v.name}`, `var-${v.name} in-${v.to}`, [lx + lw / 2, ny + 22], [nx - 2, ny + 22], { stroke: 'var(--algo-accent)', 'marker-end': `url(#${marker}-accent)`, 'data-rigid': '' }));
            svg.appendChild(el('svg:rect', { 'data-solid': '', 'data-key': `var-${v.name}`, x: lx, y: ny + 13, width: lw, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': `var-${v.name}-label`, x: lx + lw / 2, y: ny + 26, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: v.name }));
        }
        const others = (frame.vars || []).filter((v) => !isLoose(v.to)).map((v) => ({ name: v.name, index: v.to }));
        const labelTop = top + 90;
        const placed = indexLabels(others, (id) => (place.get(id) || [0])[0] + box * 0.3, x0 + (most + 1) * (box + gap));
        for (const label of placed) for (const node of indexLabel(label, labelTop + label.row * 24, top + 40, top + 66, marker)) svg.appendChild(node);
        svg.setAttribute('viewBox', `0 0 ${x0 + Math.max(most + 1, 2) * (box + gap)} ${labelTop + 3 * 24}`);
        svg.dataset.natural = x0 + Math.max(most + 1, 2) * (box + gap);
    }

    // A frame of the Queue in a ring buffer: cells (ids or null), values,
    // head, size, capacity, card (value above a cell), vars, focus, from.
    // The cells from head on, size of them (round the end), are the queue;
    // the others are faint, even when a value still stands in them.
    function traceRing(ops, capacity, setup = []) {
        const cells = Array(capacity).fill(null);
        const values = {};
        let head = 0;
        let size = 0;
        let nextId = 0;
        let card = null;
        let current = -1;
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ cells: [...cells], values: { ...values }, head, size, capacity, card: card && { ...card }, message, op: current, ...marks });
        const E = (at) => ({ lines: LIST_CODE.enqueue, at });
        const D = (at) => ({ lines: LIST_CODE.dequeue, at });
        const vars = () => [{ name: 'head', index: head }];
        const enqueue = (value, quiet) => {
            const say = quiet ? () => {} : snapshot;
            const at = (head + size) % capacity;
            const id = nextId++;
            values[id] = value;
            card = { id, at };
            say(`enqueue(${value}): ${value} soll hinten in die Queue.`, { vars: vars(), code: E(0) });
            if (size === capacity) { card = null; say(`size == capacity: Alle ${capacity} Zellen gehören zur Queue, sie ist voll.`, { vars: vars(), code: E(1) }); return; }
            cells[at] = id;
            card = null;
            say(`cells[(head + size) mod capacity] ← value: (${head} + ${size}) mod ${capacity} = ${at}. ${value} kommt in Zelle ${at}${head + size >= capacity ? ', vorn im Array: Der Ring geht weiter' : ''}.`, { vars: vars(), code: E(2), focus: at });
            size += 1;
            say(`size ← size + 1: size ist jetzt ${size}.`, { vars: vars(), code: E(3) });
        };
        const dequeue = (quiet) => {
            const say = quiet ? () => {} : snapshot;
            say('dequeue(): Der vorderste Wert soll heraus.', { vars: vars(), code: D(0), focus: head });
            if (size === 0) { say('size == 0: Die Queue ist leer.', { vars: vars(), code: D(1) }); return; }
            const id = nextId++;
            values[id] = values[cells[head]];
            card = { id, at: head };
            say(`value ← cells[head]: Die Variable value merkt sich ${values[id]}.`, { vars: vars(), code: D(2), from: { [id]: cells[head] } });
            const old = head;
            head = (head + 1) % capacity;
            say(`head ← (head + 1) mod capacity: (${old} + 1) mod ${capacity} = ${head}. In Zelle ${old} steht ${values[cells[old]]} noch, gehört aber nicht mehr zur Queue.`, { vars: vars(), code: D(3) });
            size -= 1;
            say(`size ← size - 1: size ist jetzt ${size}.`, { vars: vars(), code: D(4) });
            const out = values[id];
            card = null;
            say(`return value: ${out} kommt heraus.`, { vars: vars(), code: D(5) });
        };
        for (const op of setup) { if (op.type === 'enqueue') enqueue(op.value, true); else if (op.type === 'dequeue') dequeue(true); }
        snapshot(setup.length ? 'Die Queue von oben. Jetzt der Reihe nach die Operationen darüber.' : `Eine leere Queue in einem Array mit ${capacity} Zellen: head und size sind 0.`, { vars: vars() });
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'enqueue') enqueue(op.value);
            else if (op.type === 'dequeue') dequeue();
        }
        return frames;
    }

    function drawRing(svg, frame, marker) {
        svg.replaceChildren();
        const cell = 56;
        const left = 20;
        const top = 52;
        svg.appendChild(arrowheads(marker));
        const inQueue = (i) => (i - frame.head + frame.capacity) % frame.capacity < frame.size;
        frame.cells.forEach((id, i) => {
            const x = left + i * cell;
            svg.appendChild(el('svg:rect', { 'data-key': `cell-${i}`, x, y: top, width: cell - 6, height: 44, rx: 6, fill: inQueue(i) ? 'var(--algo-paper)' : 'transparent', stroke: 'var(--algo-line)', 'stroke-width': 1.5, 'stroke-dasharray': inQueue(i) ? 'none' : '4 4' }));
            svg.appendChild(el('svg:text', { 'data-key': `index-${i}`, x: x + (cell - 6) / 2, y: top + 60, 'text-anchor': 'middle', 'font-size': 11, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(el('svg:rect', { 'data-key': `slot-${i}`, x: x + 2, y: top + 36, width: cell - 10, height: 4, fill: 'none', stroke: 'none' }));
        });
        const value = (id, x, y, extra = {}) => el('svg:g', { 'data-key': `value-${id}`, 'data-grow': `${(cell - 6) / 2} 21`, ...(frame.from && frame.from[id] !== undefined ? { 'data-from': `value-${frame.from[id]}` } : {}), transform: `translate(${x} ${y})` }, [
            el('svg:rect', { x: 0, y: 0, width: cell - 6, height: 44, rx: 6, fill: extra.fill || 'var(--algo-frontier)', stroke: extra.stroke || 'var(--algo-ink)', 'stroke-width': 1.5, ...(extra.dash ? { 'stroke-dasharray': extra.dash } : {}) }),
            el('svg:text', { x: (cell - 6) / 2, y: 29, 'text-anchor': 'middle', 'font-size': 17, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.values[id]) })]);
        frame.cells.forEach((id, i) => {
            if (id === null) return;
            svg.appendChild(value(id, left + i * cell, top, inQueue(i) ? (frame.focus === i ? { fill: 'var(--algo-frontier)' } : { fill: 'var(--algo-paper)' })
                : { fill: 'color-mix(in srgb, var(--algo-line) 35%, var(--algo-paper))', stroke: 'var(--algo-line)', dash: '3 3' }));
        });
        if (frame.card) {
            const x = left + frame.card.at * cell;
            svg.appendChild(value(frame.card.id, x, 2, { fill: 'color-mix(in srgb, var(--algo-accent) 25%, var(--algo-paper))', stroke: 'var(--algo-accent)', dash: '5 3' }));
            svg.appendChild(el('svg:text', { 'data-key': 'card-label', x: x + cell, y: 28, 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'value' }));
        }
        const labelTop = top + 92;
        const placed = indexLabels(frame.vars, (index) => left + index * cell + (cell - 6) / 2, left * 2 + frame.capacity * cell);
        for (const label of placed) for (const node of indexLabel(label, labelTop + label.row * 24, top + 38, top + 68, marker)) svg.appendChild(node);
        svg.appendChild(el('svg:text', { 'data-key': 'size', x: left + frame.capacity * cell - 6, y: labelTop + 14, 'text-anchor': 'end', 'font-size': 13, fill: 'var(--algo-ink)', text: `size ${frame.size}, capacity ${frame.capacity}` }));
        svg.setAttribute('viewBox', `0 0 ${left * 2 + frame.capacity * cell} ${labelTop + 2 * 24}`);
        svg.dataset.natural = left * 2 + frame.capacity * cell;
    }

    // ---- Search trees: a binary search tree, optionally AVL (heights and
    // rotations after every insert). Every step follows a line of the
    // Handbuch's pseudocode (CODE, reference/baeume), and a frame holds the
    // nodes as they are in memory: graph (id -> { value, left, right, height },
    // the children as ids) and rootId. A node has an id apart from its value:
    // remove copies a value into another node, and for a moment it is there
    // twice.
    const CODE = {
        find: ['find(node, value):', '    if node == null: not found', '    if value == node.value: found',
            '    if value < node.value: return find(node.left, value)', '    else: return find(node.right, value)'],
        insert: ['insert(node, value):', '    if node == null: return a new node with value', '    if value < node.value: node.left ← insert(node.left, value)',
            '    if value > node.value: node.right ← insert(node.right, value)', '    return node'],
        remove: ['remove(node, value):', '    if node == null: return null', '    if value < node.value: node.left ← remove(node.left, value)',
            '    else if value > node.value: node.right ← remove(node.right, value)', '    else:', '        if node.left == null: return node.right',
            '        if node.right == null: return node.left', '        successor ← node.right', '        while successor.left != null: successor ← successor.left',
            '        node.value ← successor.value', '        node.right ← remove(node.right, successor.value)', '    return node'],
        // in an AVL tree remove ends by setting the height and rebalancing
        avlRemove: ['remove(node, value):', '    if node == null: return null', '    if value < node.value: node.left ← remove(node.left, value)',
            '    else if value > node.value: node.right ← remove(node.right, value)', '    else:', '        if node.left == null: return node.right',
            '        if node.right == null: return node.left', '        successor ← node.right', '        while successor.left != null: successor ← successor.left',
            '        node.value ← successor.value', '        node.right ← remove(node.right, successor.value)', '    update the height of node', '    return rebalance(node)'],
        rebalance: ['rebalance(node):', '    if height(node.left) - height(node.right) > 1:', '        if height(node.left.left) < height(node.left.right):',
            '            node.left ← rotate_left(node.left)', '        return rotate_right(node)', '    if height(node.right) - height(node.left) > 1:',
            '        if height(node.right.right) < height(node.right.left):', '            node.right ← rotate_right(node.right)', '        return rotate_left(node)', '    return node'],
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
        // phase: a part of the operation (successor), for the timeline
        let phase = null;
        const snapshot = (message, marks = {}) => frames.push({ graph: toGraph(root), rootId: root ? root.id : null, message, avl, op: current, phase, ...marks });
        const code = (name, at) => ({ lines: CODE[name], at });
        // node and successor travel with the node they point to: their arrow
        // stays rigid instead of letting go and snapping on
        const nodeVar = (n) => (n ? [{ name: 'node', to: n.id, side: 'left', rigid: true }] : []);
        const sideWord = (side) => (side === 'left' ? 'links' : 'rechts');
        const call = (name, n, value) => ({ call: `${name}(node: ${n ? n.value : 'null'}, value: ${value})`, node: n ? n.id : null });
        const stackOf = (calls) => calls.map((c) => ({ ...c }));
        // a call that got to an empty child: the picture shows null where the
        // child would be, node beside it, and the call's line goes there
        const atNull = (calls, parent, side) => ({ nullAt: { parent: parent ? parent.id : null, side }, vars: [{ name: 'node', to: 'null' }],
            stack: stackOf(calls).map((c, i, all) => (i === all.length - 1 ? { ...c, node: 'null' } : c)) });
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
            let parent = null;
            let side = null;
            const calls = [];
            while (walk && walk.value !== value) {
                const left = value < walk.value;
                calls.push(call('find', walk, value));
                snapshot(`find ${value}: ${value} ${left ? '<' : '>'} ${walk.value}, also nach ${left ? 'links' : 'rechts'}.`,
                    { focus: walk.id, incoming: value, vars: nodeVar(walk), code: code('find', left ? 3 : 4), stack: stackOf(calls) });
                parent = walk;
                side = left ? 'left' : 'right';
                walk = walk[side];
            }
            calls.push(call('find', walk, value));
            if (walk) snapshot(`find ${value}: gefunden.`, { focus: walk.id, incoming: value, vars: nodeVar(walk), code: code('find', 2), stack: stackOf(calls) });
            else snapshot(`find ${value}: Hier wäre ${value}, aber ${parent ? `${parent.value}.${side}` : 'root'} ist null. ${value} ist nicht im Baum.`, { code: code('find', 1), incoming: value, ...atNull(calls, parent, side) });
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
                snapshot(path.length ? `${value} kommt an die leere Stelle, an der die Suche geendet hat: if node == null: return a new node with value. Der Aufrufer trägt den neuen Knoten ein.` : `${value} wird die Wurzel: root ← insert(root, ${value}).`,
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
            const L = (at) => code(avl ? 'avlRemove' : 'remove', at);
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
                    calls[calls.length - 1].returns = 'null';
                    const spot = atNull(calls, parent, side);
                    snapshot(`remove ${target}: ${parent ? `${parent.value}.${side}` : 'root'} ist null, ${target} ist nicht im Baum: return null.`, { code: L(1), incoming: target, ...spot, vars: [...extra, ...spot.vars] });
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
                    calls[calls.length - 1].returns = replacement ? String(replacement.value) : 'null';
                    const deepest = stackOf(calls);
                    calls.pop();
                    nodes.pop();
                    snapshot(replacement
                        ? `node.${missing} ist leer: return node.${missing === 'left' ? 'right' : 'left'}. Der Aufrufer trägt ${replacement.value} ein, ${where} zeigt jetzt auf ${replacement.value}. ${gone.value} hängt an keinem Zeiger mehr und ist weg.`
                        : `${gone.value} hat keine Kinder: return node.right gibt null zurück, ${where} ist jetzt null. ${gone.value} ist weg.`,
                    // the pointer swings over, then gets its new subtree
                    { code: L(missing === 'left' ? 5 : 6), vars: parent ? nodeVar(parent) : [], focus: parent ? parent.id : undefined, pulses: replacement && parent ? [{ edge: `edge-${parent.id}-${side}`, delay: 0.6 }] : [], stack: deepest });
                    break;
                }
                let successor = n.right;
                phase = 'successor';
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
                phase = null;
                extra = successorVar();
                followed.push(`edge-${n.id}-right`);
                snapshot(`node.right ← remove(node.right, ${successor.value}): Der alte Knoten von ${successor.value} muss noch weg; dieselbe Funktion sucht ihn im rechten Teilbaum.`,
                    { focus: n.right.id, incoming: successor.value, vars: [...nodeVar(n.right), ...extra], code: L(10), stack: stackOf([...calls, call('remove', n.right, successor.value)]) });
                target = successor.value;
                parent = n;
                side = 'right';
                n = n.right;
            }
            if (avl) { avlBack(calls, nodes, followed); return; }
            snapshot('return node: Jeder Aufruf darüber gibt seinen Knoten zurück, und der Aufrufer trägt ihn wieder in seinen Zeiger ein, von unten nach oben, zuletzt root ← remove(root, value). Links stehen immer noch die kleineren, rechts die größeren Werte.',
                { code: L(11), pulses: back(followed), duration: backTime(followed), stack: returning(calls, nodes), returning: true });
        };

        // AVL remove on the way back up, one call after another from the
        // deepest: its pointer gets the subtree the call below returned, the
        // height is set anew, and rebalance turns the node if its subtrees
        // differ by more than 1 (the heavier child decides the case). Unlike
        // insert, this can be needed on several levels.
        const avlBack = (calls, nodes, followed) => {
            const A = (at) => code('avlRemove', at);
            const R = (at) => code('rebalance', at);
            for (let k = nodes.length - 1; k >= 0; k--) {
                const n = findById(root, nodes[k].id);
                const stack = stackOf(calls.slice(0, k + 1));
                const pulse = followed[k] ? [{ edge: followed[k], delay: 0, length: 0.5 }] : [];
                update(n);
                snapshot(`update the height of node: ${n.value} hat jetzt die Höhe h${n.height}.`, { focus: n.id, vars: nodeVar(n), code: A(11), stack, pulses: pulse });
                const balance = height(n.left) - height(n.right);
                const withRebalance = [...stack, { call: `rebalance(node: ${n.value})`, node: n.id }];
                if (Math.abs(balance) <= 1) {
                    snapshot(`return rebalance(node): Bei ${n.value} unterscheiden sich die Höhen der Teilbäume um ${Math.abs(balance)}, das ist erlaubt. rebalance gibt ${n.value} unverändert zurück.`,
                        { focus: n.id, vars: nodeVar(n), code: R(9), stack: withRebalance });
                    continue;
                }
                const heavyLeft = balance > 1;
                const child = heavyLeft ? n.left : n.right;
                const inner = heavyLeft ? height(child.left) < height(child.right) : height(child.right) < height(child.left);
                const kind = heavyLeft ? (inner ? 'LR' : 'LL') : (inner ? 'RL' : 'RR');
                snapshot(`rebalance: Bei ${n.value} unterscheiden sich die Höhen um ${Math.abs(balance)}: zu viel. Das ${heavyLeft ? 'linke' : 'rechte'} Kind ${child.value} entscheidet: Fall ${kind}.`,
                    { focus: n.id, vars: nodeVar(n), code: R(heavyLeft ? (inner ? 2 : 4) : (inner ? 6 : 8)), stack: withRebalance });
                const memory = toGraph(root);
                let rootId = root.id;
                const plan = { LL: [[n.id, 'right']], RR: [[n.id, 'left']], LR: [[child.id, 'left'], [n.id, 'right']], RL: [[child.id, 'right'], [n.id, 'left']] }[kind];
                plan.forEach(([top, dir], i) => {
                    const prefix = plan.length > 1 ? `Fall ${kind}, ${i === 0 ? 'erste' : 'zweite'} Rotation: ` : `Fall ${kind}: `;
                    const after = i === plan.length - 1 ? (k > 0 ? 'Weiter nach oben: Auch darüber kann der Baum jetzt schief sein.' : 'Der Baum ist wieder ausgeglichen.') : `Jetzt hängt alles auf einer Seite, wie im Fall ${kind === 'LR' ? 'LL' : 'RR'}.`;
                    rootId = rotationFrames(frames, memory, rootId, top, dir, { op: current, avl, stack: withRebalance }, prefix, after);
                });
                root = fromGraph(memory, rootId);
            }
            snapshot(`root ← remove(root, value): Der Baum ist ausgeglichen, und links stehen immer noch die kleineren, rechts die größeren Werte.`, { code: A(12) });
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
        // for the timeline: R or L and the node it turns about
        const phase = `${right ? 'R' : 'L'} ${val(top)}`;
        const snap = (line, message, vars) => frames.push({ ...base, graph: clone(), rootId, rootAt: startRoot, loose, vars, code: { lines, at: line }, message, stack, phase });
        const h = (id) => (id === null ? 0 : memory[id].height);
        snap(0, `${prefix}${title.replace(/\(.\)/, `(${val(top)})`)}. ${p} ist ${val(top)}, der Knoten, um den rotiert wird.`, [vp]);
        snap(1, `${q} ← ${p}.${A}: Die Variable ${q} merkt sich ${val(child)}, das ${a === 'links' ? 'linke' : 'rechte'} Kind von ${val(top)}.`, [vp, vq]);
        memory[top][A] = middle;
        snap(2, middle !== null
            ? `${p}.${A} ← ${q}.${B}: ${val(top)} zeigt ${a} jetzt auf ${val(middle)}. Zwei Zeiger führen gerade zu ${val(middle)}, und ${val(child)} hängt nur noch an der Variablen ${q}.`
            : `${p}.${A} ← ${q}.${B}: ${q}.${B} ist null, also ist ${p}.${A} jetzt auch null. ${val(child)} hängt nur noch an der Variablen ${q}.`, [vp, vq]);
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
        frames.push({ ...base, graph: clone(), rootId, vars: [], phase, message: `${val(child)} steht jetzt über ${val(top)}, die Höhen stimmen. ${after}` });
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

    const findById = (n, id) => (!n ? null : n.id === id ? n : findById(n.left, id) || findById(n.right, id));

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
        // an arrow to the node with the ring stops outside the ring
        const RINGED = 23 + 1.5 + 2;
        const arrow = (key, ends, [x1, y1], [x2, y2], extra = {}) => {
            const d = Math.hypot(x2 - x1, y2 - y1);
            const inset = frame.focus !== undefined && ends.split(' ')[1] === `node-${frame.focus}` ? RINGED : INSET;
            const s = d > inset ? (d - inset) / d : 0;
            const [ex, ey] = [x1 + (x2 - x1) * s, y1 + (y2 - y1) * s];
            return el('svg:path', { 'data-key': key, 'data-ends': ends, 'data-inset': 'auto', d: `M${x1} ${y1} Q${(x1 + ex) / 2} ${(y1 + ey) / 2} ${ex} ${ey}`,
                fill: 'none', stroke: 'var(--algo-line)', 'stroke-width': 2, 'marker-end': `url(#${most.marker})`, ...extra });
        };
        svg.appendChild(arrowheads(most.marker));
        // the way the calls on the stack went down: from root through every
        // waiting call's node to the current one; these pointers are purple,
        // their dashes flowing in the direction of the calls, so that it is
        // clear along which way the stack will come back up
        const chain = new Set();
        const stackNodes = (frame.stack || []).map((entry) => entry.node).filter((node) => node !== null && node !== undefined && graph[node]);
        if (stackNodes.length && stackNodes[0] === rootId) chain.add('pointer-root');
        for (let i = 0; i + 1 < stackNodes.length; i++) {
            const [a, b] = [stackNodes[i], stackNodes[i + 1]];
            if (graph[a].left === b) chain.add(`edge-${a}-left`);
            if (graph[a].right === b) chain.add(`edge-${a}-right`);
        }
        // on the way back up each pointer of the way settles once its pulse
        // has passed: grey and solid again, the lowest first
        const settle = new Map();
        if (frame.returning) {
            const slow = (window.StepperMotion && window.StepperMotion.slow) || 1;
            for (const pulse of frame.pulses || []) settle.set(pulse.edge, ((pulse.delay || 0) + (pulse.length || 0.4)) * (frame.duration || MOTION_TIME) * slow / 1000);
        }
        // data-settle: after that many seconds the pointer is set back
        // (setUp, settleLater): grey or purple, solid, its arrowhead to match
        const settled = (key, stroke, head) => (settle.has(key) ? { 'data-settle': settle.get(key).toFixed(2), 'data-settled-stroke': stroke, 'data-settled-head': head } : {});
        const onStack = (key) => (chain.has(key) ? { class: 'on-stack', stroke: 'var(--algo-accent)', 'marker-end': `url(#${most.marker}-accent)`,
            ...settled(key, 'var(--algo-line)', `url(#${most.marker})`) } : {});
        // a pointer is the left or right field of its node: when it changes,
        // the arrow lets go of its node and snaps onto the new one
        for (const [id, node] of Object.entries(graph)) {
            for (const side of ['left', 'right']) {
                if (node[side] === null) continue;
                svg.appendChild(arrow(`edge-${id}-${side}`, `node-${id} node-${node[side]}`, point(Number(id)), point(node[side]), onStack(`edge-${id}-${side}`)));
            }
        }
        // null where the child of a call would be (between its neighbours in
        // the order, one level down), a short stub from the parent, the last
        // part of the stack's way: the pointer the call followed, null
        let nullSpot = null;
        if (frame.nullAt) {
            const p = frame.nullAt.parent === null ? null : at.get(frame.nullAt.parent);
            nullSpot = p ? [sx(p.x + (frame.nullAt.side === 'left' ? -0.5 : 0.5)), sy(p.depth + 1)] : [left + width / 2, sy(0)];
            if (p) {
                const [px, py] = [sx(p.x), sy(p.depth)];
                const d = Math.hypot(nullSpot[0] - px, nullSpot[1] - py);
                const [ux, uy] = [(nullSpot[0] - px) / d, (nullSpot[1] - py) / d];
                svg.appendChild(el('svg:path', { 'data-key': 'null-stub', d: `M${px + ux * RADIUS} ${py + uy * RADIUS} L${nullSpot[0] - ux * 16} ${nullSpot[1] - uy * 16}`,
                    class: 'on-stack', fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 2 }));
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
            } else if (v.to === 'null') {
                // right before the word null (centred on the spot), like
                // head in the linked list
                x = nullSpot[0] - 14.5 - NULL_GAP - w;
                y = nullSpot[1] - 9;
            } else {
                const [cx, cy] = point(v.to);
                x = v.side === 'right' ? cx + 40 : v.side === 'below' ? cx - w / 2 : cx - 40 - w;
                y = v.side === 'below' ? cy + 26 : cy - 9;
            }
            const centre = [x + w / 2, y + 9];
            if (v.to !== null && v.to !== undefined && v.to !== 'null') svg.appendChild(arrow(`pointer-${v.name}`, `var-${v.name} node-${v.to}`, centre, point(v.to), { stroke: 'var(--algo-accent)', 'marker-end': `url(#${most.marker}-accent)`, ...(v.rigid ? { 'data-rigid': '' } : {}),
                ...(chain.has(`pointer-${v.name}`) ? { class: 'on-stack', ...settled(`pointer-${v.name}`, 'var(--algo-accent)', `url(#${most.marker}-accent)`) } : {}) }));
            labels.push(el('svg:rect', { 'data-solid': '', 'data-key': `var-${v.name}`, x, y, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            labels.push(el('svg:text', { 'data-key': `var-${v.name}-label`, x: centre[0], y: y + 13, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: v.name }));
            // root of an empty tree: null after it, like head of an empty list
            if (v.above && (v.to === null || v.to === undefined)) labels.push(nullBeside(`${v.name}-null`, x + w, y));
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
            if (frame.focus === id) ring = el('svg:circle', { 'data-key': 'focus', 'data-follow': `node-${id}`, 'data-ring': `node-${id}`, cx: sx(x), cy: sy(d), r: 23, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 3 });
            svg.appendChild(el('svg:circle', { 'data-key': `node-${id}`, 'data-grow': '', ...(fresh ? { 'data-from': 'incoming' } : {}), cx: sx(x), cy: sy(d), r: RADIUS, fill, stroke: 'var(--algo-ink)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': `label-${id}-${value}`, ...fromToken, ...copied, x: sx(x), y: sy(d) + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(value) }));
            if (frame.avl) svg.appendChild(el('svg:text', { 'data-key': `height-${id}`, ...fromToken, x: sx(x), y: sy(d) + 32, 'text-anchor': 'middle', 'font-size': 10, fill: 'var(--bs-secondary-color)', text: `h${graph[id].height}` }));
        }
        if (ring) svg.appendChild(ring);
        if (nullSpot) {
            // node-null: where the call's line ends (setUp, connect)
            svg.appendChild(el('svg:circle', { 'data-key': 'node-null', cx: nullSpot[0], cy: nullSpot[1], r: 15, fill: 'none', stroke: 'none' }));
            svg.appendChild(el('svg:text', { 'data-key': 'null-label', x: nullSpot[0], y: nullSpot[1] + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--bs-secondary-color)', text: 'null' }));
        }
        for (const label of labels) svg.appendChild(label);
        // the value on its way: a token beside the node it is compared with
        const near = frame.incoming !== undefined && (at.get(frame.focus) || nullSpot);
        if (near) {
            // well clear of the node and its focus ring (radius 23), and of
            // the edges below it
            const cx = (Array.isArray(near) ? near[0] : sx(near.x)) + 58;
            const cy = (Array.isArray(near) ? near[1] : sy(near.depth)) - 8;
            // on insert the token goes into its new node, under it
            const to = frame.incomingTo !== undefined ? { 'data-to': `node-${frame.incomingTo}`, 'data-merge': '' } : {};
            const toLabel = frame.incomingTo !== undefined ? { 'data-to': `label-${frame.incomingTo}-${frame.incoming}`, 'data-merge': '' } : {};
            svg.appendChild(el('svg:circle', { 'data-key': 'incoming', 'data-grow': '', ...to, cx, cy, r: 13, fill: 'var(--algo-frontier)', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '4 3' }));
            svg.appendChild(el('svg:text', { 'data-key': 'incoming-label', ...toLabel, x: cx, y: cy + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.incoming) }));
        }
        // room on the left for the labels, on the right for the token
        svg.setAttribute('viewBox', `0 0 ${left + width + 50} ${top + Math.max(100, most.depth * 62 + (frame.avl ? 8 : 0))}`);
    }

    // ---- Hash Table (reference/hash-tables): m buckets, h(k) = k mod m;
    // chaining (a list per bucket) or linear probing (the next free bucket).
    // Every step is a line of the page's pseudocode (HASH_CODE). A frame:
    // table (chain: arrays of keys; probe: key or null), b (the variable b),
    // token (the key on its way), count, m, compare (the key it is compared
    // with), found, fresh (the key just put in), code, op.
    const HASH_CODE = {
        chainInsert: ['insert(k):', '    b ← h(k)', '    if k already in buckets[b]: done', '    append k to the back of buckets[b]', '    count ← count + 1'],
        chainFind: ['find(k):', '    b ← h(k)', '    return whether k is in buckets[b]'],
        probeInsert: ['insert(k):', '    b ← h(k)', '    while buckets[b] occupied:', '        if buckets[b] == k: done', '        b ← (b + 1) mod m', '    buckets[b] ← k', '    count ← count + 1'],
        probeFind: ['find(k):', '    b ← h(k)', '    while buckets[b] occupied:', '        if buckets[b] == k: found', '        b ← (b + 1) mod m', '    not found'],
    };

    function traceHash(ops, mode, m, setup = []) {
        const chain = mode === 'chain';
        const table = Array.from({ length: m }, () => (chain ? [] : null));
        let count = 0;
        let current = -1;
        const frames = [];
        const snapshot = (message, marks = {}) => frames.push({ table: table.map((b) => (Array.isArray(b) ? [...b] : b)), mode, m, count, message, op: current, ...marks });
        const C = (name, at) => ({ lines: HASH_CODE[name], at });
        const hash = (k) => ((k % m) + m) % m;
        const insert = (k, quiet) => {
            const say = quiet ? () => {} : snapshot;
            const name = chain ? 'chainInsert' : 'probeInsert';
            say(`insert(${k}): Der Schlüssel ${k} soll in die Hash Table.`, { token: k, code: C(name, 0) });
            let b = hash(k);
            say(`b ← h(k): h(${k}) = ${k} mod ${m} = ${b}.`, { token: k, b, code: C(name, 1) });
            if (chain) {
                for (const s of table[b]) {
                    say(`if k already in buckets[b]: ${s} ist nicht ${k}.`, { token: k, b, compare: s, code: C(name, 2) });
                }
                if (table[b].includes(k)) { say(`${k} steht schon in Bucket ${b}: nichts zu tun.`, { token: k, b, code: C(name, 2) }); return; }
                table[b].push(k);
                say(table[b].length > 1 ? `append k to the back of buckets[b]: Bucket ${b} ist schon belegt, eine Kollision. ${k} kommt hinten an seine Liste.` : `append k to the back of buckets[b]: ${k} kommt in Bucket ${b}.`,
                    { b, fresh: k, code: C(name, 3) });
            } else {
                let tries = 0;
                while (table[b] !== null && tries < m) {
                    say(`while buckets[b] occupied: In Bucket ${b} steht ${table[b]}.`, { token: k, b, compare: table[b], code: C(name, 2) });
                    if (table[b] === k) { say(`buckets[b] == k: ${k} steht schon da, nichts zu tun.`, { token: k, b, code: C(name, 3) }); return; }
                    const old = b;
                    b = (b + 1) % m;
                    tries += 1;
                    say(`b ← (b + 1) mod m: (${old} + 1) mod ${m} = ${b}.${b === 0 ? ' Nach dem letzten Bucket geht es vorn weiter.' : ''}`, { token: k, b, code: C(name, 4) });
                }
                if (table[b] !== null) { say('Alle Buckets sind belegt.', { b, code: C(name, 2) }); return; }
                say(`while buckets[b] occupied: Bucket ${b} ist frei.`, { token: k, b, code: C(name, 2) });
                table[b] = k;
                say(`buckets[b] ← k: ${k} kommt in Bucket ${b}.`, { b, fresh: k, code: C(name, 5) });
            }
            count += 1;
            say(`count ← count + 1: ${count} Schlüssel in ${m} Buckets, der Füllgrad ist ${Math.round((count / m) * 100)} %.`, { b, code: C(name, chain ? 4 : 6) });
        };
        const find = (k) => {
            const name = chain ? 'chainFind' : 'probeFind';
            snapshot(`find(${k}): Steht der Schlüssel ${k} in der Hash Table?`, { token: k, code: C(name, 0) });
            let b = hash(k);
            snapshot(`b ← h(k): h(${k}) = ${k} mod ${m} = ${b}. Nur hier kann ${k} stehen${chain ? '' : ', oder ein Stück weiter'}.`, { token: k, b, code: C(name, 1) });
            if (chain) {
                for (const s of table[b]) {
                    if (s === k) { snapshot(`${k} steht in der Liste von Bucket ${b}: gefunden.`, { token: k, b, compare: s, found: s, code: C(name, 2) }); return; }
                    snapshot(`${s} ist nicht ${k}, weiter in der Liste.`, { token: k, b, compare: s, code: C(name, 2) });
                }
                snapshot(`Die Liste von Bucket ${b} ist zu Ende: ${k} ist nicht da. Kein anderer Bucket musste angesehen werden.`, { token: k, b, code: C(name, 2) });
                return;
            }
            let tries = 0;
            while (table[b] !== null && tries < m) {
                snapshot(`while buckets[b] occupied: In Bucket ${b} steht ${table[b]}.`, { token: k, b, compare: table[b], code: C(name, 2) });
                if (table[b] === k) { snapshot(`buckets[b] == k: gefunden, in Bucket ${b}.`, { token: k, b, compare: k, found: k, code: C(name, 3) }); return; }
                const old = b;
                b = (b + 1) % m;
                tries += 1;
                snapshot(`b ← (b + 1) mod m: (${old} + 1) mod ${m} = ${b}.`, { token: k, b, code: C(name, 4) });
            }
            snapshot(`while buckets[b] occupied: Bucket ${b} ist leer. Wäre ${k} da, stünde es spätestens hier: not found.`, { token: k, b, code: C(name, 5) });
        };
        for (const op of setup) if (op.type === 'insert') insert(op.value, true);
        snapshot(setup.length ? 'Die Hash Table von oben. Jetzt der Reihe nach die Operationen darüber.' : `Eine leere Hash Table mit ${m} Buckets. Der Hash eines Schlüssels k ist k mod ${m}.`);
        for (const [number, op] of ops.entries()) {
            current = number;
            if (op.type === 'insert') insert(op.value);
            else if (op.type === 'find') find(op.value);
        }
        return frames;
    }

    // The buckets as a column; with chaining each bucket points to its list,
    // the keys as boxes with arrows (pointers of their box, sprouting when a
    // key is appended); the key on its way as a dashed token where it would
    // go (after the list of bucket b, or right of bucket b); b as a label on
    // the left with an arrow to its bucket.
    function drawHash(svg, frame, longest, marker) {
        svg.replaceChildren();
        const rowH = 40;
        const top = 30;
        const bx = 90;
        const bw = 56;
        const keyW = 48;
        const step = 72;
        svg.appendChild(arrowheads(marker));
        const rowY = (i) => top + i * rowH;
        const key = (k, x, y, extra = {}) => el('svg:g', { 'data-key': `key-${k}`, 'data-grow': `${keyW / 2} ${(rowH - 10) / 2}`, ...(frame.fresh === k ? { 'data-from': 'token' } : {}), transform: `translate(${x} ${y})` }, [
            el('svg:rect', { x: 0, y: 0, width: keyW, height: rowH - 10, rx: 5, fill: extra.fill || 'var(--algo-paper)', stroke: extra.stroke || 'var(--algo-ink)', 'stroke-width': extra.width || 1.5 }),
            el('svg:text', { x: keyW / 2, y: 20, 'text-anchor': 'middle', 'font-size': 15, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(k) })]);
        const look = (k) => (frame.found === k ? { fill: 'var(--algo-frontier)', stroke: 'var(--algo-accent)', width: 2.5 } : frame.compare === k ? { fill: 'var(--algo-frontier)' } : frame.fresh === k ? { fill: 'var(--algo-frontier)' } : {});
        const arrow = (k, ends, [x1, y1], [x2, y2]) => el('svg:path', { 'data-key': k, 'data-ends': ends, 'data-inset': 2,
            d: `M${x1} ${y1} Q${(x1 + x2) / 2} ${(y1 + y2) / 2} ${x2} ${y2}`, fill: 'none', stroke: 'var(--algo-line)', 'stroke-width': 2, 'marker-end': `url(#${marker})` });
        frame.table.forEach((bucket, i) => {
            const y = rowY(i);
            svg.appendChild(el('svg:text', { 'data-key': `bindex-${i}`, x: bx - 8, y: y + 20, 'text-anchor': 'end', 'font-size': 12, fill: 'var(--bs-secondary-color)', text: String(i) }));
            svg.appendChild(el('svg:rect', { 'data-key': `bucket-${i}`, x: bx, y, width: bw, height: rowH - 8, rx: 5, fill: frame.b === i ? 'color-mix(in srgb, var(--algo-accent) 14%, var(--algo-paper))' : 'var(--algo-paper)',
                stroke: frame.b === i ? 'var(--algo-accent)' : 'var(--algo-line)', 'stroke-width': frame.b === i ? 2.5 : 1.5 }));
            // where the arrow of b ends: before the bucket's number
            svg.appendChild(el('svg:circle', { 'data-key': `bin-${i}`, cx: bx - 24, cy: y + (rowH - 8) / 2, r: 0, fill: 'none' }));
            if (frame.mode === 'chain') {
                svg.appendChild(el('svg:circle', { 'data-key': `bfield-${i}`, cx: bx + bw * 0.5, cy: y + (rowH - 8) / 2, r: 0, fill: 'none' }));
                        bucket.forEach((k, j) => {
                    const x = bx + bw + 22 + j * step;
                    const owner = j === 0 ? `bfield-${i}` : `kfield-${bucket[j - 1]}`;
                    const from = j === 0 ? [bx + bw * 0.5, y + 16] : [bx + bw + 22 + (j - 1) * step + keyW - 2, y + 16];
                    svg.appendChild(arrow(`next-${i}-${j}`, `${owner} kin-${k}`, from, [x - 2, y + 16]));
                    svg.appendChild(el('svg:circle', { 'data-key': `kin-${k}`, cx: x - 2, cy: y + 16, r: 0, fill: 'none' }));
                    svg.appendChild(el('svg:circle', { 'data-key': `kfield-${k}`, cx: x + keyW - 2, cy: y + 16, r: 0, fill: 'none' }));
                    svg.appendChild(key(k, x, y + 1, look(k)));
                });
            } else if (bucket !== null) svg.appendChild(key(bucket, bx + (bw - keyW) / 2, y + 1, look(bucket)));
        });
        // the key on its way, where it would go
        if (frame.token !== undefined && frame.b !== undefined) {
            const y = rowY(frame.b);
            const x = frame.mode === 'chain' ? bx + bw + 22 + frame.table[frame.b].length * step : bx + bw + 24;
            svg.appendChild(el('svg:g', { 'data-key': 'token', 'data-grow': `${keyW / 2} ${(rowH - 10) / 2}`, 'data-merge': '', transform: `translate(${x} ${y + 1})` }, [
                el('svg:rect', { x: 0, y: 0, width: keyW, height: rowH - 10, rx: 5, fill: 'var(--algo-frontier)', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '4 3' }),
                el('svg:text', { x: keyW / 2, y: 20, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.token) })]));
        } else if (frame.token !== undefined) {
            svg.appendChild(el('svg:g', { 'data-key': 'token', 'data-grow': `${keyW / 2} ${(rowH - 10) / 2}`, transform: `translate(${bx + bw + 24} 0)` }, [
                el('svg:rect', { x: 0, y: 0, width: keyW, height: rowH - 10, rx: 5, fill: 'var(--algo-frontier)', stroke: 'var(--algo-accent)', 'stroke-width': 2, 'stroke-dasharray': '4 3' }),
                el('svg:text', { x: keyW / 2, y: 20, 'text-anchor': 'middle', 'font-size': 14, 'font-weight': 600, fill: 'var(--algo-ink)', text: String(frame.token) })]));
        }
        // b: a label on the left, its arrow to bucket b
        if (frame.b !== undefined) {
            const y = rowY(frame.b) + (rowH - 8) / 2;
            const w = 14 + 7.5;
            svg.appendChild(el('svg:path', { 'data-key': 'pointer-b', 'data-ends': `var-b bin-${frame.b}`, 'data-inset': 2, 'data-rigid': '',
                d: `M${8 + w / 2} ${y} Q${(8 + w / 2 + bx - 26) / 2} ${y} ${bx - 26} ${y}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': 1.5, 'marker-end': `url(#${marker}-accent)` }));
            svg.appendChild(el('svg:rect', { 'data-solid': '', 'data-key': 'var-b', x: 8, y: y - 9, width: w, height: 18, rx: 4, fill: 'var(--algo-paper)', stroke: 'var(--algo-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(el('svg:text', { 'data-key': 'var-b-label', x: 8 + w / 2, y: y + 4, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--algo-accent)', text: 'b' }));
        }
        const width = bx + bw + 22 + Math.max(1, longest) * step + 30;
        svg.appendChild(el('svg:text', { 'data-key': 'count', x: width - 8, y: 18, 'text-anchor': 'end', 'font-size': 12, fill: 'var(--bs-secondary-color)',
            text: `count ${frame.count}, m ${frame.m}, Füllgrad ${Math.round((frame.count / frame.m) * 100)} %` }));
        svg.setAttribute('viewBox', `0 0 ${width} ${top + frame.m * rowH + 6}`);
        svg.dataset.natural = width;
    }

    let widgetCount = 0;
    // the height of one call in the stack box, in its own em
    const STACK_LINE = 1.62;

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
            byIndex.get(v.index).push(v);
        }
        const groups = [...byIndex.entries()].sort((a, b) => x(a[0]) - x(b[0])).map(([index, names]) => {
            const widths = names.map((v) => (v.value !== undefined ? valueBoxWidth(v.name, v.value) : 14 + 7.5 * (v.text || v.name).length));
            const width = widths.reduce((s, w) => s + w, 0) + GAP * (names.length - 1);
            // inside the picture: pushed in at the edges, the arrows then slant
            let left = Math.min(Math.max(2, x(index) - width / 2), maxX - width - 2);
            const centre = left + width / 2;
            const boxes = names.map((v, k) => { const box = { name: v.name, text: v.text, value: v.value, border: v.border, x: left + widths[k] / 2, width: widths[k] }; left += widths[k] + GAP; return box; });
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

    // A variable that holds null: the word right after its label (head,
    // root), where its arrow would end; the same size and gap everywhere
    const NULL_GAP = 6;
    const nullBeside = (key, right, top) => el('svg:text', { 'data-key': key, x: right + NULL_GAP, y: top + 13, 'font-size': 12,
        'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--bs-secondary-color)', text: 'null' });

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
    const PSEUDOCODE = /(\((?=[^()\n]*\s[^()\n]*\s)[^()\n+\-*/=<>≤≥≠[\]]*\))|(←|==|!=|<=|>=|≤|≥|≠|<|>|=|\+|-|\*|\/|%)|(\d+(?:\.\d+)?)|(\.)([\p{L}_][\p{L}\p{N}_]*)|(not found\b)|([\p{L}_][\p{L}\p{N}_]*)(\()?|([()[\]{},:])/gu;
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
        // the calls that run (a list's node_at), above the code
        const listSide = (frame, deepest) => side.replaceChildren(...(deepest ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack', style: `min-height: ${deepest * STACK_LINE}em;` },
            (frame.stack || []).map((entry, i, all) => {
                const li = el('li', { class: !frame.returning && i === all.length - 1 ? 'current' : '', style: `margin-left: ${i * 1.2}em;`, text: entry.returns ? `${entry.call} → ${entry.returns}` : entry.call,
                    ...(entry.node !== null && entry.node !== undefined ? { 'data-node': entry.node } : {}) });
                if (frame.returning) { li.classList.add('returning'); li.style.animationDelay = `${((all.length - 1 - i) / (all.length + 1)) * (frame.duration || MOTION_TIME) / 1000}s`; }
                return li;
            }))] : []), ...codeBox(frame.code));
        const setupOps = parseOps(widget.dataset.setup || '');
        if (algo === 'ring') {
            const capacity = Number(widget.dataset.capacity || 6);
            frames = traceRing(parseOps(widget.dataset.ops), capacity, setupOps);
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Die Zellen des Arrays, die Queue ab head');
            draw = (frame) => { drawRing(svg, frame, marker); listSide(frame, 0); };
        } else if (algo === 'array') {
            frames = traceArray(parseOps(widget.dataset.ops), Number(widget.dataset.capacity || 4), setupOps);
            const most = Math.max(...frames.map((f) => Math.max(f.cells.length, f.bigger ? f.bigger.length : 0)));
            const grows = frames.some((f) => f.bigger);
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Die Zellen des Arrays, die ersten size davon gehören zur Liste');
            draw = (frame) => { drawArray(svg, frame, most, marker, grows); listSide(frame, 0); };
        } else if (algo === 'linked') {
            frames = traceLinked(parseOps(widget.dataset.ops), setupOps, widget.dataset.mode === 'recursive');
            const most = Math.max(...frames.map((f) => f.order.length + (f.loose ? 1 : 0)));
            const deepest = Math.max(0, ...frames.map((f) => (f.stack || []).length));
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Die Knoten der Linked List mit ihren next-Zeigern');
            draw = (frame) => { drawLinked(svg, frame, most, marker); listSide(frame, deepest); };
        } else if (algo === 'bst' || algo === 'avl') {
            frames = traceTree(parseOps(widget.dataset.ops), algo === 'avl', parseOps(widget.dataset.setup || ''));
            svg.setAttribute('aria-label', 'Der Baum: links die kleineren, rechts die größeren Werte');
            const shapes = frames.map(treeShape);
            // the stack keeps the height of its deepest moment, so that the code below does not jump
            const deepest = Math.max(0, ...frames.map((f) => (f.stack || []).length));
            const most = { count: Math.max(...shapes.map((t) => t.count)), depth: Math.max(...shapes.map((t) => t.depth)), marker: `algo-arrow-${++widgetCount}` };
            // beside the tree the pseudocode of what runs, its line marked
            draw = (frame) => {
                drawTree(svg, frame, most);
                // the calls that run, each one deeper indented; on the way
                // back up they return one after another, the deepest first
                const stack = frame.stack || [];
                const calls = deepest ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack', style: `min-height: ${deepest * STACK_LINE}em;` }, stack.map((entry, i) => {
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
            frames = traceHash(parseOps(widget.dataset.ops), mode, Number(widget.dataset.size || 8), setupOps);
            const longest = Math.max(1, ...frames.map((f) => (mode === 'chain' ? Math.max(...f.table.map((b) => b.length + 1)) : 1)));
            const marker = `algo-arrow-${++widgetCount}`;
            svg.setAttribute('aria-label', 'Die Buckets der Hash Table mit ihren Schlüsseln');
            draw = (frame) => { drawHash(svg, frame, longest, marker); listSide(frame, 0); };
        } else if (algo === 'sort') {
            const values = widget.dataset.values.split(',').map(Number);
            const kind = widget.dataset.sort;
            frames = traceSort(values, kind);
            const marker = `algo-arrow-${++widgetCount}`;
            // the stack keeps the height of its deepest moment, so that the code below does not jump
            const deepest = Math.max(0, ...frames.map((f) => (f.stack || []).length));
            svg.setAttribute('aria-label', 'Die Zahlen als Balken, Höhe nach Wert, darunter die Indizes');
            draw = (frame) => {
                drawSort(svg, frame, marker);
                const stack = deepest ? [el('h5', { text: 'Aufrufe' }), el('ol', { class: 'algo-stack', style: `min-height: ${deepest * STACK_LINE}em;` }, frame.stack.map((entry, i) =>
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
            frames = traceGraph(graph, start, goal, algo);
            const marker = `algo-arrow-${++widgetCount}`;
            const layout = graphLayout(graph, start, algo);
            // the path beside the row if the longest row ends before it
            const rowLeft = 30 + 7.5 * ({ bfs: 'queue', dfs: 'stack' }[algo] || 'pq').length;
            layout.pathBeside = rowLeft + 18 + (Math.max(1, ...frames.map((f) => f.entries.length)) - 1) * 36 + 14 < 320;
            svg.setAttribute('aria-label', `Der Beispielgraph mit Knoten A bis H; ${start} ist der Start, ${goal} das Ziel`);
            // the calls: the search, and path_back at the end
            draw = (frame) => { drawGraph(svg, graph, frame, start, goal, algo, marker, layout); listSide(frame, 2); };
        }
        const button = (go, icon, label) => el('button', { class: 'btn btn-sm btn-success', 'data-go': go, 'aria-label': label, type: 'button' },
            [el('i', { class: `bi ${icon}`, style: 'margin-right: 0;' })]);
        const buttons = [button('first', 'bi-chevron-bar-left', 'Zum Anfang'), button('prev', 'bi-chevron-left', 'Einen Schritt zurück'),
            button('next', 'bi-chevron-right', 'Nächster Schritt'), button('last', 'bi-chevron-bar-right', 'Zum Ende')];
        // play: one step after another by itself, at a speed to choose
        const play = el('button', { class: 'btn btn-sm btn-outline-success', 'data-play': '', 'aria-label': 'Abspielen', type: 'button' }, [el('i', { class: 'bi bi-play-fill', style: 'margin-right: 0;' })]);
        const SPEEDS = [[1, '1×'], [2, '2×'], [0.5, '½×']];
        let speed = 0;
        const speedButton = el('button', { class: 'btn btn-sm btn-outline-success', 'data-speed': '', 'aria-label': 'Tempo beim Abspielen', type: 'button', text: SPEEDS[0][1] });
        const count = el('span', { class: 'algo-count' });
        const message = el('div', { class: 'algo-message', 'aria-live': 'polite' });
        picture.setAttribute('aria-label', svg.getAttribute('aria-label'));
        // the operations in their order: done, the current one, what comes
        // (a graph search names its own: the node that comes out)
        const opsText = frames.opNames || (widget.dataset.ops && frames.some((f) => f.op !== undefined)
            ? widget.dataset.ops.split(/,(?![^(]*\))/).map((s) => s.trim()).filter(Boolean) : null);
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
        // at its dot. Where they would run into each other, the earlier ones
        // go up a row and get a fine line down to their dot; no name stands in
        // the way of such a line (placed from the last to the first, each in
        // the lowest row where it overlaps no name and covers no line).
        const names = sections.filter((s) => s.op >= 0 && opsText).map((s) => ({ section: s, node: el('span', { class: 'algo-op-name', text: opsText[s.op] }), leader: el('span', { class: 'algo-op-leader' }) }));
        const nameRow = el('div', { class: 'algo-op-names' }, names.flatMap((n) => [n.leader, n.node]));
        // rows for labels that start at a dot: placed from the last to the
        // first, each in the row nearest the dots where it overlaps no label
        // and covers no line of a label further out, and where no label
        // nearer the dots covers its own line
        const stackLabels = (items, origin) => {
            const placed = [];
            for (const item of [...items].reverse()) {
                const left = item.x - 4.5;
                const right = left + item.node.offsetWidth;
                let row = 0;
                const fits = (r) => placed.every((o) => (o.row !== r || right + 8 < o.left || left > o.right + 8) && (o.row <= r || o.x < left - 3 || o.x > right + 3));
                while (!fits(row)) row++;
                Object.assign(item, { left, right, row });
                placed.push(item);
            }
            return Math.max(1, ...placed.map((n) => n.row + 1));
        };
        const dotX = (step, origin) => { const dot = ticks[step].getBoundingClientRect(); return dot.left + dot.width / 2 - origin; };
        const placeNames = () => {
            const origin = nameRow.getBoundingClientRect().left;
            for (const name of names) name.x = dotX(name.section.steps[0], origin);
            const rows = stackLabels(names);
            for (const { node, leader, left, x, row } of names) {
                node.style.left = `${left}px`;
                node.style.bottom = `${row * 1.35}em`;
                leader.style.left = `${x}px`;
                leader.style.height = `${row * 1.35 + 0.2}em`;
                leader.hidden = row === 0;
            }
            nameRow.style.height = `${rows * 1.35}em`;
            // the parts of an operation, below the dots: a bracket over their
            // steps, the label under it
            const below = phaseRow.getBoundingClientRect().left;
            for (const part of parts) {
                part.x = dotX(part.steps[0], below);
                const end = dotX(part.steps[part.steps.length - 1], below);
                part.bracket.style.left = `${part.x - 3}px`;
                part.bracket.style.width = `${Math.max(6, end - part.x + 6)}px`;
            }
            const partRows = parts.length ? stackLabels(parts) : 0;
            for (const { node, leader, left, x, row } of parts) {
                node.style.left = `${left}px`;
                node.style.top = `${0.55 + row * 1.3}em`;
                leader.style.left = `${x}px`;
                leader.style.height = `${row * 1.3 + 0.25}em`;
                leader.hidden = row === 0;
            }
            phaseRow.style.height = `${partRows ? 0.55 + partRows * 1.3 : 0}em`;
        };
        // the parts of the operations (a rotation, sift up, a merge): runs of
        // steps with the same phase
        const parts = [];
        frames.forEach((frame, i) => {
            const last = parts[parts.length - 1];
            if (frame.phase && last && last.phase === frame.phase && last.steps[last.steps.length - 1] === i - 1) last.steps.push(i);
            else if (frame.phase) parts.push({ phase: frame.phase, steps: [i] });
        });
        for (const part of parts) Object.assign(part, { node: el('span', { class: 'algo-part-name', text: part.phase }), bracket: el('span', { class: 'algo-part-bracket' }), leader: el('span', { class: 'algo-part-leader' }) });
        const phaseRow = el('div', { class: 'algo-parts' }, parts.flatMap((p) => [p.bracket, p.leader, p.node]));
        // a minimum width for each step; wider than the widget (a phone), it
        // scrolls sideways and follows the current step by itself
        const timelineInner = el('div', { class: 'algo-timeline-inner', style: `--steps: ${frames.length};` }, [nameRow, timeline, phaseRow]);
        const timelineBox = el('div', { class: 'algo-timeline-box' }, [timelineInner]);
        const keepInView = (smooth) => {
            const tick = ticks[index];
            if (!tick || timelineBox.scrollWidth <= timelineBox.clientWidth) return;
            const box = timelineBox.getBoundingClientRect();
            const dot = tick.getBoundingClientRect();
            const left = timelineBox.scrollLeft + dot.left + dot.width / 2 - box.left - box.width / 2;
            timelineBox.scrollTo({ left, behavior: smooth && !matchMedia('(prefers-reduced-motion: reduce)').matches ? 'smooth' : 'auto' });
        };
        const body = el('div', { class: 'algo-body' }, [picture, side]);
        // data-title: what the widget shows, above everything else
        const title = widget.dataset.title ? [el('div', { class: 'algo-title', text: widget.dataset.title })] : [];
        widget.replaceChildren(...title, el('div', { class: 'algo-controls' }, [...buttons, play, speedButton, count]), timelineBox, body, message);
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
            if (to !== index) { const before = index; index = to; stop(); show(Math.abs(index - before) === 1); }
        };
        timeline.addEventListener('pointerdown', (event) => { dragging = true; timeline.setPointerCapture(event.pointerId); stepAt(event); });
        timeline.addEventListener('pointermove', (event) => { if (dragging) stepAt(event); });
        timeline.addEventListener('pointerup', () => { dragging = false; });
        timeline.addEventListener('pointercancel', () => { dragging = false; });
        // Lines from each call on the stack to the node it holds (trees): the
        // current one strong, the waiting ones faint; they follow the nodes
        // while they move and fade with their call when it returns
        const links = algo === 'bst' || algo === 'avl' || (algo === 'linked' && widget.dataset.mode === 'recursive') ? el('svg:svg', { class: 'algo-links', 'aria-hidden': 'true' }) : null;
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
                const current = li.classList.contains('current');
                // the current call's line ends at the ring, which glides from
                // node to node, so that the line (and the value on it) glide too
                const ring = current && picture.querySelector('[data-key="focus"]');
                const node = ring || picture.querySelector(`[data-key="node-${li.dataset.node}"]`);
                if (!node) continue;
                const c = node.getBoundingClientRect();
                const l = li.getBoundingClientRect();
                const [cx, cy, r] = [c.left + c.width / 2 - box.left, c.top + c.height / 2 - box.top, c.width / 2];
                const [sx, sy] = [l.left - box.left - 2, l.top + l.height / 2 - box.top];
                const d = Math.hypot(sx - cx, sy - cy) || 1;
                const [ex, ey] = [cx + ((sx - cx) / d) * (r + 3), cy + ((sy - cy) / d) * (r + 3)];
                const [c1, c2] = [[sx - 40, sy], [ex + 30, ey]];
                const fade = Number(getComputedStyle(li).opacity);
                paths.push(el('svg:path', { d: `M${sx} ${sy} C${c1[0]} ${c1[1]} ${c2[0]} ${c2[1]} ${ex} ${ey}`, fill: 'none', stroke: 'var(--algo-accent)', 'stroke-width': current ? 1.8 : 1.2,
                    'stroke-dasharray': current ? 'none' : '3 4', opacity: (current ? 0.9 : 0.45) * fade }));
                // the value on its way (the call's argument) sits on this line,
                // just before the node
                const token = current && picture.querySelector('[data-key="incoming"]');
                if (token && !picture.hasAttribute('data-token-free')) {
                    const at = (u) => {
                        const v = 1 - u;
                        return [v ** 3 * sx + 3 * v * v * u * c1[0] + 3 * v * u * u * c2[0] + u ** 3 * ex, v ** 3 * sy + 3 * v * v * u * c1[1] + 3 * v * u * u * c2[1] + u ** 3 * ey];
                    };
                    let u = 1;
                    let p = at(u);
                    while (u > 0 && Math.hypot(p[0] - ex, p[1] - ey) < 30) { u -= 0.01; p = at(u); }
                    const m = picture.getScreenCTM().inverse();
                    const q = new DOMPoint(p[0] + box.left, p[1] + box.top).matrixTransform(m);
                    token.setAttribute('cx', q.x);
                    token.setAttribute('cy', q.y);
                    const label = picture.querySelector('[data-key="incoming-label"]');
                    if (label) { label.setAttribute('x', q.x); label.setAttribute('y', q.y + 4); }
                }
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
        // On the way back the pointers of the stack's way settle one after
        // another once their pulse has passed (data-settle, drawTree); a new
        // step cancels what is still waiting. Without movement, at once.
        let settling = [];
        const settleLater = (animate) => {
            for (const timer of settling) clearTimeout(timer);
            settling = [];
            for (const path of picture.querySelectorAll('[data-settle]')) {
                const done = () => {
                    path.classList.remove('on-stack');
                    path.setAttribute('stroke', path.dataset.settledStroke);
                    path.setAttribute('marker-end', path.dataset.settledHead);
                };
                const wait = Number(path.dataset.settle) * 1000;
                if (!animate || matchMedia('(prefers-reduced-motion: reduce)').matches) done();
                else settling.push(setTimeout(done, wait));
            }
        };
        // a step without code (the start) shows the code of the first
        // operation, no line marked, so that the code box stays where it is
        const firstCode = frames.find((f) => f.code && f.code.lines);
        for (const frame of frames) if (!frame.code && firstCode) frame.code = { lines: firstCode.code.lines, at: -1 };
        // animate: a step forward or back moves; a jump shows the step at once
        const show = (animate) => {
            const frame = frames[index];
            draw(frame);
            // a list's cells keep their size across widgets: the picture at
            // most as wide as it is drawn (data-natural)
            if (svg.dataset.natural) picture.style.maxWidth = `${svg.dataset.natural}px`;
            const rate = playing ? SPEEDS[speed][0] : 1;
            if (window.StepperMotion) window.StepperMotion.morph(picture, svg, { duration: animate ? (frame.duration || MOTION_TIME) / rate : 0 });
            else {
                picture.setAttribute('viewBox', svg.getAttribute('viewBox'));
                picture.replaceChildren(...svg.childNodes);
            }
            message.textContent = frame.message;
            if (links) follow((frame.duration || MOTION_TIME) * 2 + 600);
            settleLater(animate);
            ticks.forEach((tick, i) => { tick.className = `algo-tick${'start' in tick.dataset ? ' start' : ''}${i < index ? ' done' : i === index ? ' current' : ''}`; });
            for (const { section, node } of names) node.classList.toggle('current', section.steps.includes(index));
            for (const part of parts) { part.node.classList.toggle('current', part.steps.includes(index)); part.bracket.classList.toggle('current', part.steps.includes(index)); }
            timeline.setAttribute('aria-valuenow', index);
            keepInView(!dragging);
            count.textContent = `Schritt ${index} von ${frames.length - 1}`;
            // for the lightbox, which measures the first and the last step
            // (stepper_keys.js)
            widget.dataset.step = index;
            widget.dataset.steps = frames.length;
            buttons[0].disabled = buttons[1].disabled = index === 0;
            buttons[2].disabled = buttons[3].disabled = index === frames.length - 1;
        };
        // Playing: after each movement a pause to read the message, both
        // shorter or longer with the speed; a step by hand stops it
        let playing = null;
        const READ_TIME = 1400;
        const stop = () => {
            clearTimeout(playing);
            playing = null;
            play.firstChild.className = 'bi bi-play-fill';
            play.setAttribute('aria-label', 'Abspielen');
        };
        const tick = () => {
            if (index >= frames.length - 1) { stop(); return; }
            index += 1;
            show(true);
            playing = setTimeout(tick, ((frames[index].duration || MOTION_TIME) * ((window.StepperMotion && window.StepperMotion.slow) || 1) + READ_TIME) / SPEEDS[speed][0]);
        };
        play.addEventListener('click', () => {
            if (playing) { stop(); return; }
            play.firstChild.className = 'bi bi-pause-fill';
            play.setAttribute('aria-label', 'Anhalten');
            // at the end it starts again from the beginning
            if (index >= frames.length - 1) { index = 0; show(false); playing = setTimeout(tick, READ_TIME / SPEEDS[speed][0]); } else tick();
        });
        speedButton.addEventListener('click', () => {
            speed = (speed + 1) % SPEEDS.length;
            speedButton.textContent = SPEEDS[speed][1];
        });
        widget.stepperGo = (k) => { index = Math.max(0, Math.min(frames.length - 1, k)); show(false); };
        const go = { first: () => 0, prev: () => index - 1, next: () => index + 1, last: () => frames.length - 1 };
        for (const b of buttons) b.addEventListener('click', () => {
            stop();
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
