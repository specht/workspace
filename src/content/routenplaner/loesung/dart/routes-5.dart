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
  return [
    for (final line in File(path).readAsLinesSync())
      if (line.isNotEmpty) _way(line),
  ];
}

Way _way(String line) {
  final parts = line.split('\t');
  final words = parts.sublist(3).join('\t').split(' ');
  final points = <Point>[];
  for (var i = 0; i + 1 < words.length; i += 2) {
    final position = words[i + 1].split(',');
    points.add(Point(words[i], double.parse(position[0]), double.parse(position[1])));
  }
  return Way(parts[0], parts[1], parts[2], points);
}

// Der Abstand zweier Punkte in Metern
const earthRadius = 6371000.0; // Meter

double distance(double lon1, double lat1, double lon2, double lat2) {
  const radians = pi / 180;
  final x = (lon2 - lon1) * radians * cos((lat1 + lat2) / 2 * radians);
  final y = (lat2 - lat1) * radians;
  return sqrt(x * x + y * y) * earthRadius;
}

// Der Graph aus den Wegen: in nodes jeder Punkt, in edges für jeden Punkt
// die Kanten zu seinen Nachbarn, in beide Richtungen, mit Metern als Kosten
Graph buildGraph(List<Way> ways) {
  final graph = Graph();
  for (final way in ways) {
    for (var i = 0; i + 1 < way.points.length; i++) {
      final a = way.points[i], b = way.points[i + 1];
      graph.nodes[a.id] = a;
      graph.nodes[b.id] = b;
      final metres = distance(a.lon, a.lat, b.lon, b.lat);
      graph.addEdge(a.id, b.id, metres);
      graph.addEdge(b.id, a.id, metres);
    }
  }
  return graph;
}

// Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter, bis
// zum Ziel. Gibt den Weg zurück, [node, …, goal], oder null. Geht auf einer
// Karte im Kreis, bis der Stack voll ist.
List<String>? walk(Graph graph, String node, String goal) {
  if (node == goal) return [node];
  for (final edge in graph.edges[node] ?? <Edge>[]) {
    final rest = walk(graph, edge.to, goal);
    if (rest != null) return [node, ...rest];
  }
  return null;
}

// Tiefensuche: wie walk, aber jeden Knoten nur einmal; seen sind die Knoten,
// die die Suche schon gesehen hat, für alle Aufrufe von search dieselben
List<String>? depthFirst(Graph graph, String start, String goal) {
  Set<String> seen = {};
  return search(graph, start, goal, seen);
}

List<String>? search(Graph graph, String node, String goal, Set<String> seen) {
  if (seen.contains(node)) return null;
  seen.add(node);
  if (node == goal) return [node];
  List<Edge>? edges = graph.edges[node];
  if (edges == null) return null;
  for (Edge edge in edges) {
    List<String>? rest = search(graph, edge.to, goal, seen);
    if (rest != null) {
      rest.insert(0, node);
      return rest;
    }
  }
  return null;
}

// Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
// gefundenen Knoten, von welchem aus er gefunden wurde (der Start von null).
// Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
List<String>? pathTo(Map<String, String?> previous, String goal) {
  if (!previous.containsKey(goal)) return null;
  final path = <String>[];
  String? node = goal;
  while (node != null) {
    path.insert(0, node);
    node = previous[node];
  }
  return path;
}

// Breitensuche: der Weg über die wenigsten Kanten, und die besuchten Knoten
// in der Reihenfolge der Suche
Search breadthFirst(Graph graph, String start, String goal) {
  final previous = <String, String?>{start: null};
  final queue = Queue<String>()..add(start);
  final visited = <String>[];
  while (queue.isNotEmpty) {
    final node = queue.removeFirst();
    visited.add(node);
    if (node == goal) break;
    for (final edge in graph.edges[node] ?? <Edge>[]) {
      if (previous.containsKey(edge.to)) continue;
      previous[edge.to] = node;
      queue.add(edge.to);
    }
  }
  return Search(pathTo(previous, goal), visited);
}

// Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
Search dijkstra(Graph graph, String start, String goal) {
  final metres = <String, double>{start: 0};
  final previous = <String, String?>{start: null};
  final frontier = <String, double>{start: 0}; // gefunden, aber noch nicht fertig
  final visited = <String>[];
  while (frontier.isNotEmpty) {
    var node = frontier.keys.first;
    for (final id in frontier.keys) {
      if (frontier[id]! < frontier[node]!) node = id;
    }
    final soFar = frontier.remove(node)!;
    visited.add(node);
    if (node == goal) break;
    for (final edge in graph.edges[node] ?? <Edge>[]) {
      final total = soFar + edge.cost;
      if (metres.containsKey(edge.to) && metres[edge.to]! <= total) continue;
      metres[edge.to] = total;
      previous[edge.to] = node;
      frontier[edge.to] = total;
    }
  }
  return Search(pathTo(previous, goal), visited);
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
