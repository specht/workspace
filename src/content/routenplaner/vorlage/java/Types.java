// Die Typen, mit denen deine Methoden in Routes.java arbeiten. Hier musst du
// nichts ändern.
import java.util.ArrayList;
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

// Nur für die Karte: welche Knoten bei einer Suche in welcher Reihenfolge
// dran waren. Deine Suchen rufen visit(node) auf, wenn ein Knoten dran ist;
// der Server und die Prüfungen lesen die Reihenfolge hier ab. Für die Suche
// selbst tut visit nichts.
class Visits {
    static final List<String> order = new ArrayList<>();

    static void visit(String node) {
        order.add(node);
    }

    // Eine Suche, die eine Datei lesen darf (readWays wirft IOException)
    interface Run {
        void run() throws Exception;
    }

    // Die Knoten, die dran waren, während search lief, in ihrer Reihenfolge
    static List<String> of(Run search) throws Exception {
        order.clear();
        search.run();
        return new ArrayList<>(order);
    }
}

// Ein Graph: jeder Knoten mit seinem Punkt, und für jeden Knoten die Kanten
// zu seinen Nachbarn. fastest: das größte Tempo in Metern je Sekunde (nur in
// einem Graphen für eine Art zu reisen).
class Graph {
    final Map<String, Point> nodes = new HashMap<>();
    final Map<String, List<Edge>> edges = new HashMap<>();
    double fastest = 1;

    // Eine Kante von from nach to anhängen: die Liste der Kanten von from
    // holen, eine leere anlegen, wenn es noch keine gibt, und anhängen
    void addEdge(String from, String to, double cost) {
        List<Edge> list = edges.get(from);
        if (list == null) {
            list = new ArrayList<>();
            edges.put(from, list);
        }
        list.add(new Edge(to, cost));
    }
}
