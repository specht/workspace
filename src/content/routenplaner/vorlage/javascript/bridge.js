// Die Brücke zwischen dem Server und deinen Funktionen in routes.js. Der
// Server startet sie (./run javascript) und schickt ihr Fragen, eine Zeile
// JSON je Frage, über eine Verbindung auf diesem Rechner; sie antwortet
// ebenso. Was routes.js ausgibt, erscheint im Terminal. Hier musst du nichts
// ändern.
const net = require('net');
const readline = require('readline');
const routes = require('./routes.js');

const NAMES = {
    depth_first: 'depthFirst', breadth_first: 'breadthFirst', dijkstra: 'dijkstra', a_star: 'aStar', tour_length: 'tourLength',
    best_tour: 'bestTour', nearest_tour: 'nearestTour', improve_tour: 'improveTour',
};
const ways = {};   // Datei -> Wege
const graphs = {}; // Art zu reisen -> Graph

function graphOf(mode, file) {
    if (!graphs[mode]) {
        if (!ways[file]) ways[file] = routes.readWays(file);
        graphs[mode] = mode === 'length' ? routes.buildGraph(ways[file]) : routes.buildTravelGraph(ways[file], mode);
    }
    return graphs[mode];
}

function fn(name) {
    const f = routes[NAMES[name] || name];
    if (typeof f !== 'function') throw Object.assign(new Error(NAMES[name] || name), { name: 'NotYet' });
    return f;
}

function answer(request) {
    if (request.op === 'graph') {
        const g = graphOf(request.mode, request.file);
        return { graph: { nodes: g.nodes, edges: g.edges, fastest: g.fastest } };
    }
    if (request.op === 'search') {
        const graph = graphs[request.mode];
        if (!graph) throw new Error(`Kein Graph für ${request.mode}`);
        const search = fn(request.fn);
        const began = process.hrtime.bigint();
        const result = search(graph, request.start, request.goal);
        const took = Number(process.hrtime.bigint() - began) / 1e9;
        // walk und depthFirst geben nur den Weg zurück
        if (result === null || Array.isArray(result)) return { path: result, visited: [], took };
        return { path: result.path ?? null, visited: result.visited || [], took };
    }
    if (request.op === 'call') {
        const f = fn(request.fn);
        const began = process.hrtime.bigint();
        const result = f(...request.args);
        return { result, took: Number(process.hrtime.bigint() - began) / 1e9 };
    }
    return { error: `Unbekannte Frage: ${request.op}` };
}

const socket = net.connect(Number(process.env.ROUTES_PORT), '127.0.0.1');
readline.createInterface({ input: socket }).on('line', (line) => {
    let reply;
    try {
        reply = answer(JSON.parse(line));
    } catch (e) {
        if (e && e.name === 'NotYet') reply = { missing: e.message };
        else {
            console.error(e && e.stack ? e.stack.split('\n').filter((l) => !l.includes('bridge.js')).slice(0, 9).join('\n') : e);
            reply = { error: String(e).slice(0, 300) };
        }
    }
    socket.write(JSON.stringify(reply) + '\n');
});
socket.on('close', () => process.exit(0));
