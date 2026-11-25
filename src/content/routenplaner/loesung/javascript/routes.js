// Ein Routenplaner: deine Funktionen. Der Server (./run javascript) fragt sie
// und zeigt im Browser, was sie schon können. (Die Lösung aller Schritte.)
const fs = require('fs');

// Was noch nicht fertig ist, meldet sich so: Die Karte sagt dann, was fehlt.
function notYet(name) {
    const error = new Error(name);
    error.name = 'NotYet';
    return error;
}

// Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
// durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
// sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
//
// Gibt ein Array von Objekten zurück, eines pro Weg:
//   { kind: 'residential', oneway: '', name: 'Schloßstraße',
//     points: [['26735834', 13.3266, 52.4567], …] }
function readWays(path) {
    return fs.readFileSync(path, 'utf8').split('\n').filter((line) => line !== '').map((line) => {
        const [kind, oneway, name, ...rest] = line.split('\t');
        const words = rest.join('\t').split(' ');
        const points = [];
        for (let i = 0; i + 1 < words.length; i += 2) {
            const [lon, lat] = words[i + 1].split(',').map(Number);
            points.push([words[i], lon, lat]);
        }
        return { kind, oneway, name, points };
    });
}

// Der Abstand zweier Punkte in Metern
const EARTH_RADIUS = 6371000; // Meter

function distance(lon1, lat1, lon2, lat2) {
    const radians = Math.PI / 180;
    const x = (lon2 - lon1) * radians * Math.cos((lat1 + lat2) / 2 * radians);
    const y = (lat2 - lat1) * radians;
    return Math.sqrt(x * x + y * y) * EARTH_RADIUS;
}

// Der Graph aus den Wegen:
//   nodes: { ID: [Länge, Breite] } für jeden Punkt
//   edges: { ID: [[Nachbar-ID, Meter], …] } für jeden Punkt, in beide Richtungen
function buildGraph(ways) {
    const nodes = {};
    const edges = {};
    for (const way of ways) {
        for (let i = 0; i + 1 < way.points.length; i++) {
            const [a, lon1, lat1] = way.points[i];
            const [b, lon2, lat2] = way.points[i + 1];
            nodes[a] = [lon1, lat1];
            nodes[b] = [lon2, lat2];
            const metres = distance(lon1, lat1, lon2, lat2);
            (edges[a] ??= []).push([b, metres]);
            (edges[b] ??= []).push([a, metres]);
        }
    }
    return { nodes, edges };
}

// Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter, bis
// zum Ziel. Gibt den Weg zurück, [node, …, goal], oder null. Geht auf einer
// Karte im Kreis, bis der Stack voll ist.
function walk(graph, node, goal) {
    if (node === goal) return [node];
    for (const [neighbour] of graph.edges[node] || []) {
        const rest = walk(graph, neighbour, goal);
        if (rest !== null) return [node, ...rest];
    }
    return null;
}

// Tiefensuche: wie walk, aber jeder Knoten nur einmal; seen sind die Knoten,
// die die Suche schon gesehen hat
function depthFirst(graph, node, goal, seen = new Set()) {
    if (seen.has(node)) return null;
    seen.add(node);
    if (node === goal) return [node];
    for (const [neighbour] of graph.edges[node] || []) {
        const rest = depthFirst(graph, neighbour, goal, seen);
        if (rest !== null) return [node, ...rest];
    }
    return null;
}

// Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
// gefundenen Knoten, von welchem aus er gefunden wurde (der Start von null).
// Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
function pathTo(previous, goal) {
    if (!previous.has(goal)) return null;
    const path = [];
    let node = goal;
    while (node !== null) {
        path.unshift(node);
        node = previous.get(node);
    }
    return path;
}

// Breitensuche: der Weg über die wenigsten Kanten. Gibt zurück:
//   { path: [Start-ID, …, Ziel-ID] (oder null, wenn es keinen Weg gibt),
//     visited: [ID, …] }  die besuchten Knoten, in der Reihenfolge der Suche
function breadthFirst(graph, start, goal) {
    const previous = new Map([[start, null]]);
    // shift() verschiebt bei jedem Aufruf das ganze Array; ein Index, der
    // vorrückt, holt das Nächste ohne Verschieben (wie eine Queue im Ring)
    const queue = [start];
    let head = 0;
    const visited = [];
    while (head < queue.length) {
        const node = queue[head++];
        visited.push(node);
        if (node === goal) break;
        for (const [neighbour] of graph.edges[node] || []) {
            if (previous.has(neighbour)) continue;
            previous.set(neighbour, node);
            queue.push(neighbour);
        }
    }
    return { path: pathTo(previous, goal), visited };
}

// Eine Priority Queue als binärer Heap: Was hineinkommt, hat eine
// Priorität, und pop gibt immer das mit der kleinsten zurück. Die Einträge
// stehen in einem Array, als Baum: Die Kinder von i stehen bei 2i+1 und 2i+2,
// und kein Kind ist kleiner als sein Elternteil.
class MinHeap {
    constructor() {
        this.items = []; // [Priorität, Wert]
    }

    isEmpty() {
        return this.items.length === 0;
    }

    get size() {
        return this.items.length;
    }

    push(priority, value) {
        const items = this.items;
        items.push([priority, value]);
        let i = items.length - 1;
        // nach oben, solange das Elternteil größer ist
        while (i > 0) {
            const parent = Math.floor((i - 1) / 2);
            if (items[parent][0] <= items[i][0]) break;
            [items[parent], items[i]] = [items[i], items[parent]];
            i = parent;
        }
    }

    pop() {
        const items = this.items;
        if (items.length === 0) return null;
        const top = items[0];
        const last = items.pop();
        if (items.length > 0) {
            items[0] = last;
            let i = 0;
            // nach unten, solange ein Kind kleiner ist
            while (true) {
                let smallest = i;
                for (const child of [2 * i + 1, 2 * i + 2]) {
                    if (child < items.length && items[child][0] < items[smallest][0]) smallest = child;
                }
                if (smallest === i) break;
                [items[smallest], items[i]] = [items[i], items[smallest]];
                i = smallest;
            }
        }
        return top;
    }
}

// Dijkstra: der kürzeste Weg, mit einer Priority Queue. Ein Knoten kann
// mehrmals darin stehen, wenn er einen besseren Weg bekommt; der alte
// Eintrag ist dann veraltet und wird übersprungen.
function dijkstra(graph, start, goal) {
    const costs = new Map([[start, 0]]);
    const previous = new Map([[start, null]]);
    const done = new Set();
    const heap = new MinHeap();
    heap.push(0, start);
    const visited = [];
    while (!heap.isEmpty()) {
        const [soFar, node] = heap.pop();
        if (done.has(node)) continue;
        done.add(node);
        visited.push(node);
        if (node === goal) break;
        for (const [neighbour, length] of graph.edges[node] || []) {
            const total = soFar + length;
            if (costs.has(neighbour) && costs.get(neighbour) <= total) continue;
            costs.set(neighbour, total);
            previous.set(neighbour, node);
            heap.push(total, neighbour);
        }
    }
    return { path: pathTo(previous, goal), visited };
}

// A*: wie Dijkstra, aber die Priorität ist der Weg bis hier plus eine
// Schätzung für den Rest, die Luftlinie zum Ziel. Die Schätzung ist nie zu
// groß, darum findet A* trotzdem den kürzesten Weg. In einem Graphen mit
// Sekunden statt Metern teilt man die Luftlinie durch das größte Tempo.
function aStar(graph, start, goal) {
    const [goalLon, goalLat] = graph.nodes[goal] || [0, 0];
    const fastest = graph.fastest || 1;
    const estimate = (id) => {
        const [lon, lat] = graph.nodes[id];
        return distance(lon, lat, goalLon, goalLat) / fastest;
    };
    const costs = new Map([[start, 0]]);
    const previous = new Map([[start, null]]);
    const done = new Set();
    const heap = new MinHeap();
    heap.push(estimate(start), start);
    const visited = [];
    while (!heap.isEmpty()) {
        const [, node] = heap.pop();
        if (done.has(node)) continue;
        done.add(node);
        visited.push(node);
        if (node === goal) break;
        for (const [neighbour, length] of graph.edges[node] || []) {
            const total = costs.get(node) + length;
            if (costs.has(neighbour) && costs.get(neighbour) <= total) continue;
            costs.set(neighbour, total);
            previous.set(neighbour, node);
            heap.push(total + estimate(neighbour), neighbour);
        }
    }
    return { path: pathTo(previous, goal), visited };
}

// Wie schnell man auf welcher Art von Weg vorankommt, in km/h. Was in der
// Liste einer Art zu reisen fehlt, darf man so nicht benutzen.
const SPEEDS = {
    foot: { footway: 5, path: 5, pedestrian: 5, steps: 2, living_street: 5,
            residential: 5, service: 5, unclassified: 5, tertiary: 5, secondary: 5,
            primary: 5, track: 5, cycleway: 5, bridleway: 5, ferry: 5 },
    bike: { cycleway: 18, residential: 15, living_street: 10, service: 12,
            unclassified: 15, tertiary: 16, secondary: 16, primary: 16,
            tertiary_link: 15, secondary_link: 15, primary_link: 15,
            path: 12, track: 10, bridleway: 10, ferry: 10 },
    car: { motorway: 90, motorway_link: 50, trunk: 60, trunk_link: 40,
           primary: 50, primary_link: 30, secondary: 45, secondary_link: 30,
           tertiary: 40, tertiary_link: 30, unclassified: 30, residential: 25,
           living_street: 7, service: 10, ferry: 10 },
};

// Der Graph für eine Art zu reisen: nur die Wege, die man so benutzen darf,
// und als Länge jeder Kante, wie viele Sekunden man für sie braucht. Für das
// Auto gelten die Einbahnstraßen. fastest: das größte Tempo, in Metern je
// Sekunde, für die Schätzung von A*.
function buildTravelGraph(ways, mode) {
    const speeds = SPEEDS[mode];
    const nodes = {};
    const edges = {};
    for (const way of ways) {
        const speed = speeds[way.kind];
        if (!speed) continue;
        const metresPerSecond = speed / 3.6;
        const forward = mode !== 'car' || way.oneway !== '-1';
        const backward = mode !== 'car' || way.oneway === '';
        for (let i = 0; i + 1 < way.points.length; i++) {
            const [a, lon1, lat1] = way.points[i];
            const [b, lon2, lat2] = way.points[i + 1];
            nodes[a] = [lon1, lat1];
            nodes[b] = [lon2, lat2];
            const seconds = distance(lon1, lat1, lon2, lat2) / metresPerSecond;
            if (forward) (edges[a] ??= []).push([b, seconds]);
            if (backward) (edges[b] ??= []).push([a, seconds]);
        }
    }
    return { nodes, edges, fastest: Math.max(...Object.values(speeds)) / 3.6 };
}

// Eine Rundfahrt: costs[i][j] sagt, wie weit es von Ort i zu Ort j ist. Eine
// Reihenfolge beginnt bei Ort 0 und kehrt am Ende zu ihm zurück.
function tourLength(costs, order) {
    let total = 0;
    for (let i = 0; i < order.length; i++) total += costs[order[i]][order[(i + 1) % order.length]];
    return total;
}

// Alle Reihenfolgen ausprobieren: sicher die beste, aber bei n Orten sind es
// (n − 1)! viele. tryAll hängt jeden übrigen Ort einmal an und probiert mit
// dem Rest weiter; ist nichts mehr übrig, ist eine Reihenfolge fertig.
function bestTour(costs) {
    let best = [0];
    let shortest = Infinity;
    function tryAll(order, left) {
        if (left.length === 0) {
            const length = tourLength(costs, order);
            if (length < shortest) {
                shortest = length;
                best = order;
            }
            return;
        }
        for (const next of left) tryAll([...order, next], left.filter((i) => i !== next));
    }
    tryAll([0], Array.from({ length: costs.length - 1 }, (_, i) => i + 1));
    return best;
}

// Immer zum nächsten Ort, an dem man noch nicht war: schnell, aber oft nicht
// die beste
function nearestTour(costs) {
    const order = [0];
    const left = Array.from({ length: costs.length - 1 }, (_, i) => i + 1);
    while (left.length > 0) {
        const from = order.at(-1);
        let nearest = left[0];
        for (const i of left) if (costs[from][i] < costs[from][nearest]) nearest = i;
        order.push(nearest);
        left.splice(left.indexOf(nearest), 1);
    }
    return order;
}

// 2-opt: ein Stück der Rundfahrt umdrehen, wenn sie dadurch kürzer wird, und
// das so lange, bis kein Umdrehen mehr hilft
function improveTour(costs, order) {
    order = [...order];
    let better = true;
    while (better) {
        better = false;
        for (let i = 1; i < order.length - 1; i++) {
            for (let j = i + 1; j < order.length; j++) {
                const turned = [...order.slice(0, i), ...order.slice(i, j + 1).reverse(), ...order.slice(j + 1)];
                if (tourLength(costs, turned) < tourLength(costs, order) - 1e-9) {
                    order = turned;
                    better = true;
                }
            }
        }
    }
    return order;
}

module.exports = {
    readWays, distance, buildGraph, walk, depthFirst, pathTo, breadthFirst, dijkstra, MinHeap, aStar, SPEEDS,
    buildTravelGraph, tourLength, bestTour, nearestTour, improveTour,
};
