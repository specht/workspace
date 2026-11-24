// Ein Routenplaner: deine Methoden. Der Server (./run java) fragt sie und
// zeigt im Browser, was sie schon können. Die Typen Point, Way, Edge, Graph
// und Search stehen in Types.java.
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Routes {
    // Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile
    // sind durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die
    // Punkte sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
    //   [Way[kind=residential, oneway=, name=Schloßstraße, points=[Point[id=26735834, lon=13.3266, lat=52.4567], …]], …]
    static List<Way> readWays(String path) throws IOException {
        List<Way> ways = new ArrayList<>();
        for (String line : Files.readAllLines(Path.of(path))) {
            if (line.isEmpty()) continue;
            String[] parts = line.split("\t", 4);
            String[] words = parts[3].split(" ");
            List<Point> points = new ArrayList<>();
            for (int i = 0; i + 1 < words.length; i += 2) {
                String[] position = words[i + 1].split(",");
                points.add(new Point(words[i], Double.parseDouble(position[0]), Double.parseDouble(position[1])));
            }
            ways.add(new Way(parts[0], parts[1], parts[2], points));
        }
        return ways;
    }

    // Der Abstand zweier Punkte in Metern
    static final double EARTH_RADIUS = 6_371_000; // Meter

    static double distance(double lon1, double lat1, double lon2, double lat2) {
        // ohne den Cosinus: nach Osten zu weit
        return Math.sqrt(Math.pow(lon2 - lon1, 2) + Math.pow(lat2 - lat1, 2)) * Math.PI / 180 * EARTH_RADIUS;
    }

    // Der Graph aus den Wegen: in nodes jeder Punkt, in edges für jeden Punkt
    // die Kanten zu seinen Nachbarn, in beide Richtungen, mit Metern als Kosten
    static Graph buildGraph(List<Way> ways) {
        Graph graph = new Graph();
        for (Way way : ways) {
            for (int i = 0; i + 1 < way.points().size(); i++) {
                Point a = way.points().get(i), b = way.points().get(i + 1);
                graph.nodes.put(a.id(), a);
                graph.nodes.put(b.id(), b);
                double metres = distance(a.lon(), a.lat(), b.lon(), b.lat());
                graph.edges.computeIfAbsent(a.id(), id -> new ArrayList<>()).add(new Edge(b.id(), metres));
                graph.edges.computeIfAbsent(b.id(), id -> new ArrayList<>()).add(new Edge(a.id(), metres));
            }
        }
        return graph;
    }

    // Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter,
    // bis zum Ziel. Gibt den Weg zurück, [node, …, goal], oder null. Geht auf
    // einer Karte im Kreis, bis der Stack voll ist.
    static List<String> walk(Graph graph, String node, String goal) {
        if (node.equals(goal)) return new ArrayList<>(List.of(node));
        for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
            List<String> rest = walk(graph, edge.to(), goal);
            if (rest != null) {
                rest.add(0, node);
                return rest;
            }
        }
        return null;
    }

    // Tiefensuche: wie walk, aber jeder Knoten nur einmal; found sind die
    // Knoten, die die Suche schon gefunden hat
    static List<String> depthFirst(Graph graph, String start, String goal) {
        return depthFirst(graph, start, goal, new HashSet<>());
    }

    static List<String> depthFirst(Graph graph, String node, String goal, Set<String> found) {
        if (found.contains(node)) return null;
        found.add(node);
        if (node.equals(goal)) return new ArrayList<>(List.of(node));
        for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
            List<String> rest = depthFirst(graph, edge.to(), goal, found);
            if (rest != null) {
                rest.add(0, node);
                return rest;
            }
        }
        return null;
    }

    // Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
    // gefundenen Knoten, von welchem aus er gefunden wurde (der Start von null).
    // Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
    static List<String> pathTo(Map<String, String> previous, String goal) {
        if (!previous.containsKey(goal)) return null;
        LinkedList<String> path = new LinkedList<>();
        for (String node = goal; node != null; node = previous.get(node)) path.addFirst(node);
        return path;
    }

    // Breitensuche: der Weg über die wenigsten Kanten, und die besuchten
    // Knoten in der Reihenfolge der Suche
    static Search breadthFirst(Graph graph, String start, String goal) {
        Map<String, String> previous = new HashMap<>();
        previous.put(start, null);
        ArrayDeque<String> queue = new ArrayDeque<>(); // ein Ring Buffer
        queue.add(start);
        List<String> visited = new ArrayList<>();
        while (!queue.isEmpty()) {
            String node = queue.poll();
            visited.add(node);
            if (node.equals(goal)) break;
            for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
                if (previous.containsKey(edge.to())) continue;
                previous.put(edge.to(), node);
                queue.add(edge.to());
            }
        }
        return new Search(pathTo(previous, goal), visited);
    }

    // Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
    static Search dijkstra(Graph graph, String start, String goal) {
        Map<String, Double> metres = new HashMap<>(Map.of(start, 0.0));
        Map<String, String> previous = new HashMap<>();
        previous.put(start, null);
        Map<String, Double> frontier = new HashMap<>(Map.of(start, 0.0)); // gefunden, aber noch nicht fertig
        List<String> visited = new ArrayList<>();
        while (!frontier.isEmpty()) {
            String node = null;
            for (String id : frontier.keySet()) if (node == null || frontier.get(id) < frontier.get(node)) node = id;
            double soFar = frontier.remove(node);
            visited.add(node);
            if (node.equals(goal)) break;
            for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
                double total = soFar + edge.cost();
                if (metres.containsKey(edge.to()) && metres.get(edge.to()) <= total) continue;
                metres.put(edge.to(), total);
                previous.put(edge.to(), node);
                frontier.put(edge.to(), total);
            }
        }
        return new Search(pathTo(previous, goal), visited);
    }

    // A*: wie Dijkstra, mit der Luftlinie zum Ziel als Schätzung für den Rest.
    // Gibt dasselbe zurück wie breadthFirst.
    static Search aStar(Graph graph, String start, String goal) {
        throw new UnsupportedOperationException("aStar");
    }

    // Wie schnell man auf welcher Art von Weg vorankommt, in km/h. Was in der
    // Liste einer Art zu reisen fehlt, darf man so nicht benutzen.
    static final Map<String, Map<String, Integer>> SPEEDS = Map.of(
        "foot", Map.ofEntries(Map.entry("footway", 5), Map.entry("path", 5), Map.entry("pedestrian", 5),
            Map.entry("steps", 2), Map.entry("living_street", 5), Map.entry("residential", 5), Map.entry("service", 5),
            Map.entry("unclassified", 5), Map.entry("tertiary", 5), Map.entry("secondary", 5), Map.entry("primary", 5),
            Map.entry("track", 5), Map.entry("cycleway", 5), Map.entry("bridleway", 5), Map.entry("ferry", 5)),
        "bike", Map.ofEntries(Map.entry("cycleway", 18), Map.entry("residential", 15), Map.entry("living_street", 10),
            Map.entry("service", 12), Map.entry("unclassified", 15), Map.entry("tertiary", 16), Map.entry("secondary", 16),
            Map.entry("primary", 16), Map.entry("tertiary_link", 15), Map.entry("secondary_link", 15),
            Map.entry("primary_link", 15), Map.entry("path", 12), Map.entry("track", 10), Map.entry("bridleway", 10),
            Map.entry("ferry", 10)),
        "car", Map.ofEntries(Map.entry("motorway", 90), Map.entry("motorway_link", 50), Map.entry("trunk", 60),
            Map.entry("trunk_link", 40), Map.entry("primary", 50), Map.entry("primary_link", 30), Map.entry("secondary", 45),
            Map.entry("secondary_link", 30), Map.entry("tertiary", 40), Map.entry("tertiary_link", 30),
            Map.entry("unclassified", 30), Map.entry("residential", 25), Map.entry("living_street", 7),
            Map.entry("service", 10), Map.entry("ferry", 10)));

    // Der Graph für eine Art zu reisen ("foot", "bike", "car"): wie buildGraph,
    // aber nur mit den Wegen, die man so benutzen darf, und mit Sekunden statt
    // Metern als Kosten; in fastest das größte Tempo in Metern je Sekunde. Für
    // das Auto gelten die Einbahnstraßen.
    static Graph buildTravelGraph(List<Way> ways, String mode) {
        throw new UnsupportedOperationException("buildTravelGraph");
    }

    // Eine Rundfahrt durch mehrere Orte: costs[i][j] sagt, wie weit es von
    // Ort i zu Ort j ist. Eine Reihenfolge ist eine Liste von Nummern der
    // Orte; sie beginnt bei 0 und kehrt am Ende zu ihrem Anfang zurück.

    // Wie lang eine Rundfahrt in dieser Reihenfolge ist, mit dem Weg zurück
    static double tourLength(double[][] costs, List<Integer> order) {
        throw new UnsupportedOperationException("tourLength");
    }

    // Die beste Reihenfolge, durch Ausprobieren aller
    static List<Integer> bestTour(double[][] costs) {
        throw new UnsupportedOperationException("bestTour");
    }

    // Immer zum nächsten Ort, an dem die Rundfahrt noch nicht war
    static List<Integer> nearestTour(double[][] costs) {
        throw new UnsupportedOperationException("nearestTour");
    }

    // Eine Reihenfolge mit 2-opt verbessern: ein Stück umdrehen, solange die
    // Rundfahrt dadurch kürzer wird
    static List<Integer> improveTour(double[][] costs, List<Integer> order) {
        throw new UnsupportedOperationException("improveTour");
    }
}
