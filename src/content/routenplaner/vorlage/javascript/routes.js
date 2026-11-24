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
    throw notYet('readWays');
}

// Der Abstand zweier Punkte in Metern
function distance(lon1, lat1, lon2, lat2) {
    throw notYet('distance');
}

// Der Graph aus den Wegen:
//   nodes: { ID: [Länge, Breite] } für jeden Punkt
//   edges: { ID: [[Nachbar-ID, Meter], …] } für jeden Punkt, in beide Richtungen
function buildGraph(ways) {
    throw notYet('buildGraph');
}

// Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter, bis
// zum Ziel. Gibt den Weg zurück, die IDs von node bis zum Ziel, null, wenn
// es von node aus keinen gibt.
function walk(graph, node, goal) {
    throw notYet('walk');
}

// Tiefensuche: wie walk, aber jeden Knoten nur einmal. found sind die Knoten,
// die die Suche schon gefunden hat; der erste Aufruf bekommt eine leere Menge.
function depthFirst(graph, node, goal, found = new Set()) {
    throw notYet('depthFirst');
}

// Der Weg zum Ziel, rückwärts aus den Vorgängern: previous ist eine Map, die
// für jeden gefundenen Knoten sagt, von welchem aus er gefunden wurde (der
// Start von null).
// Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
function pathTo(previous, goal) {
    throw notYet('pathTo');
}

// Breitensuche: der Weg über die wenigsten Kanten. Gibt zurück:
//   { path: [Start-ID, …, Ziel-ID] (oder null, wenn es keinen Weg gibt),
//     visited: [ID, …] }  die besuchten Knoten, in der Reihenfolge der Suche
function breadthFirst(graph, start, goal) {
    throw notYet('breadthFirst');
}

// Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
function dijkstra(graph, start, goal) {
    throw notYet('dijkstra');
}

// Eine Priority Queue: push legt einen Wert mit einer Priorität hinein, pop
// gibt das Paar [Priorität, Wert] mit der kleinsten Priorität zurück und
// nimmt es heraus (null, wenn sie leer ist).
class MinHeap {
    constructor() {
        this.items = []; // [Priorität, Wert], als binärer Heap
    }

    isEmpty() {
        return this.items.length === 0;
    }

    get size() {
        return this.items.length;
    }

    push(priority, value) {
        throw notYet('MinHeap.push');
    }

    pop() {
        throw notYet('MinHeap.pop');
    }
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
    readWays, distance, buildGraph, walk, depthFirst, pathTo, breadthFirst, dijkstra, MinHeap, aStar, SPEEDS,
    buildTravelGraph, tourLength, bestTour, nearestTour, improveTour,
};
