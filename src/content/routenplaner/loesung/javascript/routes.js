// Ein Routenplaner: deine Funktionen. Der Server (./run javascript) fragt sie
// und zeigt im Browser, was sie schon können.
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

// Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
function dijkstra(graph, start, goal) {
    const metres = new Map([[start, 0]]);
    const previous = new Map([[start, null]]);
    const frontier = new Map([[start, 0]]); // gefunden, aber noch nicht fertig
    const visited = [];
    while (frontier.size > 0) {
        let node = null;
        for (const [id, m] of frontier) if (node === null || m < frontier.get(node)) node = id;
        const soFar = frontier.get(node);
        frontier.delete(node);
        visited.push(node);
        if (node === goal) break;
        for (const [neighbour, length] of graph.edges[node] || []) {
            const total = soFar + length;
            if (metres.has(neighbour) && metres.get(neighbour) <= total) continue;
            metres.set(neighbour, total);
            previous.set(neighbour, node);
            frontier.set(neighbour, total);
        }
    }
    return { path: pathTo(previous, goal), visited };
}

// A*: wie Dijkstra, mit der Luftlinie zum Ziel als Schätzung für den Rest.
// Gibt dasselbe zurück wie breadthFirst.
function aStar(graph, start, goal) {
    throw notYet('aStar');
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

// Der Graph für eine Art zu reisen ('foot', 'bike', 'car'): wie buildGraph,
// aber nur mit den Wegen, die man so benutzen darf, und mit Sekunden statt
// Metern an den Kanten. Für das Auto gelten die Einbahnstraßen.
//   { nodes: …, edges: …, fastest: das größte Tempo in Metern je Sekunde }
function buildTravelGraph(ways, mode) {
    throw notYet('buildTravelGraph');
}

// Eine Rundfahrt durch mehrere Orte: costs[i][j] sagt, wie weit es von Ort i
// zu Ort j ist. Eine Reihenfolge ist ein Array von Nummern der Orte; sie
// beginnt bei 0 und kehrt am Ende zu ihrem Anfang zurück.

// Wie lang eine Rundfahrt in dieser Reihenfolge ist, mit dem Weg zurück
function tourLength(costs, order) {
    throw notYet('tourLength');
}

// Die beste Reihenfolge, durch Ausprobieren aller
function bestTour(costs) {
    throw notYet('bestTour');
}

// Immer zum nächsten Ort, an dem die Rundfahrt noch nicht war
function nearestTour(costs) {
    throw notYet('nearestTour');
}

// Eine Reihenfolge mit 2-opt verbessern: ein Stück umdrehen, solange die
// Rundfahrt dadurch kürzer wird
function improveTour(costs, order) {
    throw notYet('improveTour');
}

module.exports = {
    readWays, distance, buildGraph, pathTo, breadthFirst, dijkstra, aStar, SPEEDS,
    buildTravelGraph, tourLength, bestTour, nearestTour, improveTour,
};
