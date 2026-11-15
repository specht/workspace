// Ein Routenplaner: deine Funktionen. Der Server (./run dart) fragt sie und
// zeigt im Browser, was sie schon können. Die Klassen Point, Way, Edge, Graph
// und Search stehen in graph.dart. (Die Lösung aller Schritte.)
import 'dart:collection';
import 'dart:io';
import 'dart:math';

import 'graph.dart';
import 'my_array_heap.dart';
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
      graph.edges.putIfAbsent(a.id, () => []).add(Edge(b.id, metres));
      graph.edges.putIfAbsent(b.id, () => []).add(Edge(a.id, metres));
    }
  }
  return graph;
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

// Der Heap aus der Data Structures Sandbox als MinHeap. Er speichert nur
// ganze Zahlen, also kommt jedes Paar als eine Zahl hinein: die Priorität in
// Tausendsteln, mal 2^24, plus eine laufende Nummer. Eine kleinere Priorität
// gibt eine kleinere Zahl, so ordnet der Heap richtig; die genaue Priorität
// und den Wert jeder Nummer merkt sich der MinHeap in zwei Listen.
class MinHeap {
  static const _slots = 1 << 24; // so viele Nummern, bevor sie sich wiederholen
  final _heap = MyArrayHeap();
  final _priorities = <double>[];
  final _values = <String>[];

  bool get isEmpty => _heap.isEmpty();

  int get length => _heap.memory.length;

  void push(double priority, String value) {
    _heap.insert((priority * 1000).round() * _slots + _values.length % _slots);
    _priorities.add(priority);
    _values.add(value);
  }

  (double, String)? pop() {
    final key = _heap.removeMin();
    if (key == null) return null;
    return (_priorities[key % _slots], _values[key % _slots]);
  }

  // was im Heap steht, als Paare, in der Reihenfolge seines Arrays
  List<(double, String)> get items => [
        for (final key in _heap.memory.copy()) (_priorities[key % _slots], _values[key % _slots]),
      ];
}

// Dijkstra: der kürzeste Weg, mit einer Priority Queue. Ein Knoten kann
// mehrmals darin stehen, wenn er einen besseren Weg bekommt; der alte
// Eintrag ist dann veraltet und wird übersprungen.
Search dijkstra(Graph graph, String start, String goal) {
  final costs = <String, double>{start: 0};
  final previous = <String, String?>{start: null};
  final done = <String>{};
  final heap = MinHeap()..push(0, start);
  final visited = <String>[];
  while (!heap.isEmpty) {
    final (soFar, node) = heap.pop()!;
    if (done.contains(node)) continue;
    done.add(node);
    visited.add(node);
    if (node == goal) break;
    for (final edge in graph.edges[node] ?? <Edge>[]) {
      final total = soFar + edge.cost;
      if (costs.containsKey(edge.to) && costs[edge.to]! <= total) continue;
      costs[edge.to] = total;
      previous[edge.to] = node;
      heap.push(total, edge.to);
    }
  }
  return Search(pathTo(previous, goal), visited);
}

// A*: wie Dijkstra, aber die Priorität ist der Weg bis hier plus eine
// Schätzung für den Rest, die Luftlinie zum Ziel. Die Schätzung ist nie zu
// groß, darum findet A* trotzdem den kürzesten Weg. In einem Graphen mit
// Sekunden statt Metern teilt man die Luftlinie durch das größte Tempo.
Search aStar(Graph graph, String start, String goal) {
  final target = graph.nodes[goal];
  double estimate(String id) {
    final point = graph.nodes[id];
    if (target == null || point == null) return 0;
    return distance(point.lon, point.lat, target.lon, target.lat) / graph.fastest;
  }

  final costs = <String, double>{start: 0};
  final previous = <String, String?>{start: null};
  final done = <String>{};
  final heap = MinHeap()..push(estimate(start), start);
  final visited = <String>[];
  while (!heap.isEmpty) {
    final (_, node) = heap.pop()!;
    if (done.contains(node)) continue;
    done.add(node);
    visited.add(node);
    if (node == goal) break;
    for (final edge in graph.edges[node] ?? <Edge>[]) {
      final total = costs[node]! + edge.cost;
      if (costs.containsKey(edge.to) && costs[edge.to]! <= total) continue;
      costs[edge.to] = total;
      previous[edge.to] = node;
      heap.push(total + estimate(edge.to), edge.to);
    }
  }
  return Search(pathTo(previous, goal), visited);
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

// Der Graph für eine Art zu reisen: nur die Wege, die man so benutzen darf,
// und als Kosten jeder Kante, wie viele Sekunden man für sie braucht. Für das
// Auto gelten die Einbahnstraßen. fastest: das größte Tempo, in Metern je
// Sekunde, für die Schätzung von A*.
Graph buildTravelGraph(List<Way> ways, String mode) {
  final table = speeds[mode]!;
  final graph = Graph();
  for (final way in ways) {
    final speed = table[way.kind];
    if (speed == null) continue;
    final metresPerSecond = speed / 3.6;
    final forward = mode != 'car' || way.oneway != '-1';
    final backward = mode != 'car' || way.oneway == '';
    for (var i = 0; i + 1 < way.points.length; i++) {
      final a = way.points[i], b = way.points[i + 1];
      graph.nodes[a.id] = a;
      graph.nodes[b.id] = b;
      final seconds = distance(a.lon, a.lat, b.lon, b.lat) / metresPerSecond;
      if (forward) graph.edges.putIfAbsent(a.id, () => []).add(Edge(b.id, seconds));
      if (backward) graph.edges.putIfAbsent(b.id, () => []).add(Edge(a.id, seconds));
    }
  }
  graph.fastest = table.values.reduce(max) / 3.6;
  return graph;
}

// Eine Rundfahrt: costs[i][j] sagt, wie weit es von Ort i zu Ort j ist. Eine
// Reihenfolge beginnt bei Ort 0 und kehrt am Ende zu ihm zurück.
double tourLength(List<List<double>> costs, List<int> order) {
  var total = 0.0;
  for (var i = 0; i < order.length; i++) {
    total += costs[order[i]][order[(i + 1) % order.length]];
  }
  return total;
}

// Alle Reihenfolgen ausprobieren: sicher die beste, aber bei n Orten sind es
// (n − 1)! viele. try hängt jeden übrigen Ort einmal an und probiert mit dem
// Rest weiter; ist nichts mehr übrig, ist eine Reihenfolge fertig.
List<int> bestTour(List<List<double>> costs) {
  var best = [0];
  var shortest = double.infinity;
  void tryAll(List<int> order, List<int> left) {
    if (left.isEmpty) {
      final length = tourLength(costs, order);
      if (length < shortest) {
        shortest = length;
        best = [...order];
      }
      return;
    }
    for (final next in left) {
      tryAll([...order, next], [for (final i in left) if (i != next) i]);
    }
  }

  tryAll([0], [for (var i = 1; i < costs.length; i++) i]);
  return best;
}

// Immer zum nächsten Ort, an dem man noch nicht war: schnell, aber oft nicht
// die beste
List<int> nearestTour(List<List<double>> costs) {
  final order = [0];
  final left = [for (var i = 1; i < costs.length; i++) i];
  while (left.isNotEmpty) {
    var nearest = left.first;
    for (final i in left) {
      if (costs[order.last][i] < costs[order.last][nearest]) nearest = i;
    }
    order.add(nearest);
    left.remove(nearest);
  }
  return order;
}

// 2-opt: ein Stück der Rundfahrt umdrehen, wenn sie dadurch kürzer wird, und
// das so lange, bis kein Umdrehen mehr hilft
List<int> improveTour(List<List<double>> costs, List<int> order) {
  order = [...order];
  var better = true;
  while (better) {
    better = false;
    for (var i = 1; i < order.length - 1; i++) {
      for (var j = i + 1; j < order.length; j++) {
        final turned = [
          ...order.sublist(0, i),
          ...order.sublist(i, j + 1).reversed,
          ...order.sublist(j + 1),
        ];
        if (tourLength(costs, turned) < tourLength(costs, order) - 1e-9) {
          order = turned;
          better = true;
        }
      }
    }
  }
  return order;
}
