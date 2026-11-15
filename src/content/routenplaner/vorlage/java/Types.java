// Die Typen, mit denen deine Methoden in Routes.java arbeiten. Hier musst du
// nichts ändern.
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// Ein Punkt eines Weges: seine ID aus OpenStreetMap, Länge und Breite in Grad
record Point(String id, double lon, double lat) {}

// Ein Weg: seine Art (residential, footway, …), ob er eine Einbahnstraße ist
// ("yes", "-1" oder ""), sein Name ("" ohne Namen) und seine Punkte
record Way(String kind, String oneway, String name, List<Point> points) {}

// Eine Kante zu einem Nachbarn: wohin und was sie kostet (Meter, oder
// Sekunden in einem Graphen für eine Art zu reisen)
record Edge(String to, double cost) {}

// Was eine Suche zurückgibt: den Weg (die IDs vom Start bis zum Ziel, null
// ohne Weg) und alle besuchten Knoten in der Reihenfolge der Suche. Die
// Karte spielt die Suche damit ab.
record Search(List<String> path, List<String> visited) {}

// Ein Graph: jeder Knoten mit seinem Punkt, und für jeden Knoten die Kanten
// zu seinen Nachbarn. fastest: das größte Tempo in Metern je Sekunde (nur in
// einem Graphen für eine Art zu reisen).
class Graph {
    final Map<String, Point> nodes = new HashMap<>();
    final Map<String, List<Edge>> edges = new HashMap<>();
    double fastest = 1;
}
