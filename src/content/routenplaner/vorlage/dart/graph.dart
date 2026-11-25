// Die Klassen, mit denen deine Funktionen in routes.dart arbeiten. Hier
// musst du nichts ändern.

// Ein Punkt eines Weges: seine ID aus OpenStreetMap, Länge und Breite in Grad
class Point {
  final String id;
  final double lon;
  final double lat;
  const Point(this.id, this.lon, this.lat);

  @override
  bool operator ==(Object other) => other is Point && other.id == id && other.lon == lon && other.lat == lat;
  @override
  int get hashCode => Object.hash(id, lon, lat);
  @override
  String toString() => "Point('$id', $lon, $lat)";
}

// Ein Weg: seine Art (residential, footway, …), ob er eine Einbahnstraße ist
// ('yes', '-1' oder ''), sein Name ('' ohne Namen) und seine Punkte
class Way {
  final String kind;
  final String oneway;
  final String name;
  final List<Point> points;
  const Way(this.kind, this.oneway, this.name, this.points);

  @override
  String toString() => "Way('$kind', '$oneway', '$name', $points)";
}

// Eine Kante zu einem Nachbarn: wohin und was sie kostet (Meter, oder
// Sekunden in einem Graphen für eine Art zu reisen)
class Edge {
  final String to;
  final double cost;
  const Edge(this.to, this.cost);

  @override
  bool operator ==(Object other) => other is Edge && other.to == to && other.cost == cost;
  @override
  int get hashCode => Object.hash(to, cost);
  @override
  String toString() => "Edge('$to', $cost)";
}

// Ein Graph: jeder Knoten mit seinem Punkt, und für jeden Knoten die Kanten
// zu seinen Nachbarn. fastest: das größte Tempo in Metern je Sekunde (nur in
// einem Graphen für eine Art zu reisen).
class Graph {
  final Map<String, Point> nodes = {};
  final Map<String, List<Edge>> edges = {};
  double fastest = 1;

  // Eine Kante von from nach to anhängen: die Liste der Kanten von from
  // holen, eine leere anlegen, wenn es noch keine gibt, und anhängen
  void addEdge(String from, String to, double cost) {
    List<Edge>? list = edges[from];
    if (list == null) {
      list = [];
      edges[from] = list;
    }
    list.add(Edge(to, cost));
  }
}

// Was eine Suche zurückgibt: den Weg (die IDs vom Start bis zum Ziel, null
// ohne Weg) und alle besuchten Knoten in der Reihenfolge der Suche. Die
// Karte spielt die Suche damit ab.
class Search {
  final List<String>? path;
  final List<String> visited;
  const Search(this.path, this.visited);

  @override
  String toString() => 'Search($path, ${visited.length} besucht)';
}
