// Ein Routenplaner: deine Funktionen. Der Server (./run dart) fragt sie und
// zeigt im Browser, was sie schon können. Die Klassen Point, Way, Edge, Graph
// und Search stehen in graph.dart.
import 'dart:collection';
import 'dart:io';
import 'dart:math';

import 'graph.dart';
export 'graph.dart';

// Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
// durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
// sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
//   [Way('residential', '', 'Schloßstraße', [Point('26735834', 13.3266, 52.4567), …]), …]
List<Way> readWays(String path) {
  throw UnimplementedError('readWays');
}

// Der Abstand zweier Punkte in Metern
double distance(double lon1, double lat1, double lon2, double lat2) {
  throw UnimplementedError('distance');
}

// Der Graph aus den Wegen: in nodes jeder Punkt, in edges für jeden Punkt
// die Kanten zu seinen Nachbarn, in beide Richtungen, mit Metern als Kosten
Graph buildGraph(List<Way> ways) {
  throw UnimplementedError('buildGraph');
}

// Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter, bis
// zum Ziel. Gibt den Weg zurück, die IDs von node bis zum Ziel, null, wenn
// es von node aus keinen gibt.
List<String>? walk(Graph graph, String node, String goal) {
  throw UnimplementedError('walk');
}

// Tiefensuche: wie walk, aber jeden Knoten nur einmal. depthFirst legt die
// leere Menge seen an und fragt search; search sucht wie walk, merkt sich
// in seen jeden Knoten, den es sieht, und gibt seen bei jedem Aufruf an
// sich selbst weiter.
List<String>? depthFirst(Graph graph, String start, String goal) {
  Set<String> seen = {};
  return search(graph, start, goal, seen);
}

List<String>? search(Graph graph, String node, String goal, Set<String> seen) {
  throw UnimplementedError('search');
}

// Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
// gefundenen Knoten, von welchem aus er gefunden wurde (der Start von null).
// Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
List<String>? pathTo(Map<String, String?> previous, String goal) {
  throw UnimplementedError('pathTo');
}

// Breitensuche: der Weg über die wenigsten Kanten, und die besuchten Knoten
// in der Reihenfolge der Suche
Search breadthFirst(Graph graph, String start, String goal) {
  throw UnimplementedError('breadthFirst');
}

// Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
Search dijkstra(Graph graph, String start, String goal) {
  throw UnimplementedError('dijkstra');
}

// Eine Priority Queue: push legt einen Wert mit einer Priorität hinein, pop
// gibt das Paar (Priorität, Wert) mit der kleinsten Priorität zurück und
// nimmt es heraus (null, wenn sie leer ist).
class MinHeap {
  final items = <(double, String)>[]; // (Priorität, Wert), als binärer Heap

  bool get isEmpty => items.isEmpty;

  int get length => items.length;

  void push(double priority, String value) {
    throw UnimplementedError('MinHeap.push');
  }

  (double, String)? pop() {
    throw UnimplementedError('MinHeap.pop');
  }
}

// A*: wie Dijkstra, mit der Luftlinie zum Ziel als Schätzung für den Rest.
// Gibt dasselbe zurück wie breadthFirst.
Search aStar(Graph graph, String start, String goal) {
  throw UnimplementedError('aStar');
}

// Wie schnell man auf welcher Art von Weg vorankommt, in km/h. Was in der
// Liste einer Art zu reisen fehlt, darf man so nicht benutzen.
const speeds = {
  'foot': {'footway': 5, 'path': 5, 'pedestrian': 5, 'steps': 2, 'living_street': 5,
           'residential': 5, 'service': 5, 'unclassified': 5, 'tertiary': 5, 'secondary': 5,
           'primary': 5, 'track': 5, 'cycleway': 5, 'bridleway': 5, 'ferry': 5},
  'bike': {'cycleway': 18, 'residential': 15, 'living_street': 10, 'service': 12,
           'unclassified': 15, 'tertiary': 16, 'secondary': 16, 'primary': 16,
           'tertiary_link': 15, 'secondary_link': 15, 'primary_link': 15,
           'path': 12, 'track': 10, 'bridleway': 10, 'ferry': 10},
  'car': {'motorway': 90, 'motorway_link': 50, 'trunk': 60, 'trunk_link': 40,
          'primary': 50, 'primary_link': 30, 'secondary': 45, 'secondary_link': 30,
          'tertiary': 40, 'tertiary_link': 30, 'unclassified': 30, 'residential': 25,
          'living_street': 7, 'service': 10, 'ferry': 10},
};

// Der Graph für eine Art zu reisen ('foot', 'bike', 'car'): wie buildGraph,
// aber nur mit den Wegen, die man so benutzen darf, und mit Sekunden statt
// Metern als Kosten; in fastest das größte Tempo in Metern je Sekunde. Für
// das Auto gelten die Einbahnstraßen.
Graph buildTravelGraph(List<Way> ways, String mode) {
  throw UnimplementedError('buildTravelGraph');
}

// Eine Rundfahrt durch mehrere Orte: costs[i][j] sagt, wie weit es von Ort i
// zu Ort j ist. Eine Reihenfolge ist eine Liste von Nummern der Orte; sie
// beginnt bei 0 und kehrt am Ende zu ihrem Anfang zurück.

// Wie lang eine Rundfahrt in dieser Reihenfolge ist, mit dem Weg zurück
double tourLength(List<List<double>> costs, List<int> order) {
  throw UnimplementedError('tourLength');
}

// Die beste Reihenfolge, durch Ausprobieren aller
List<int> bestTour(List<List<double>> costs) {
  throw UnimplementedError('bestTour');
}

// Immer zum nächsten Ort, an dem die Rundfahrt noch nicht war
List<int> nearestTour(List<List<double>> costs) {
  throw UnimplementedError('nearestTour');
}

// Eine Reihenfolge mit 2-opt verbessern: ein Stück umdrehen, solange die
// Rundfahrt dadurch kürzer wird
List<int> improveTour(List<List<double>> costs, List<int> order) {
  throw UnimplementedError('improveTour');
}
