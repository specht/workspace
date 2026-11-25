// Ein Routenplaner: deine Methoden. Der Server (./run java) fragt sie und
// zeigt im Browser, was sie schon können. Die Typen Point, Way, Edge und
// Graph stehen in Types.java. (Die Lösung aller Schritte.)
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Routes {
    // Nur für die Karte: zeigt, dass dieser Knoten dran ist (Visits in Types.java)
    static void visit(String node) {
        Visits.visit(node);
    }

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
        double radians = Math.PI / 180;
        double x = (lon2 - lon1) * radians * Math.cos((lat1 + lat2) / 2 * radians);
        double y = (lat2 - lat1) * radians;
        return Math.sqrt(x * x + y * y) * EARTH_RADIUS;
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
                graph.addEdge(a.id(), b.id(), metres);
                graph.addEdge(b.id(), a.id(), metres);
            }
        }
        return graph;
    }

    // Einfach losgehen: von node aus zu jedem Nachbarn und von dort weiter,
    // bis zum Ziel. Gibt den Weg zurück, [node, …, goal], oder null. Geht auf
    // einer Karte im Kreis, bis der Stack voll ist.
    static List<String> walk(Graph graph, String node, String goal) {
        visit(node);
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

    // Tiefensuche: wie walk, aber jeder Knoten nur einmal; seen sind die
    // Knoten, die die Suche schon gesehen hat
    static List<String> depthFirst(Graph graph, String start, String goal) {
        return depthFirst(graph, start, goal, new HashSet<>());
    }

    static List<String> depthFirst(Graph graph, String node, String goal, Set<String> seen) {
        if (seen.contains(node)) return null;
        seen.add(node);
        visit(node);
        if (node.equals(goal)) return new ArrayList<>(List.of(node));
        for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
            List<String> rest = depthFirst(graph, edge.to(), goal, seen);
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

    // Breitensuche: der Weg über die wenigsten Kanten, die IDs vom Start bis
    // zum Ziel, oder null, wenn es keinen gibt. Ruf visit(node) auf, wenn ein
    // Knoten dran ist: Damit spielt die Karte die Suche ab.
    static List<String> breadthFirst(Graph graph, String start, String goal) {
        Map<String, String> previous = new HashMap<>();
        previous.put(start, null);
        ArrayDeque<String> queue = new ArrayDeque<>(); // ein Ring Buffer
        queue.add(start);
        while (!queue.isEmpty()) {
            String node = queue.poll();
            visit(node);
            if (node.equals(goal)) break;
            for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
                if (previous.containsKey(edge.to())) continue;
                previous.put(edge.to(), node);
                queue.add(edge.to());
            }
        }
        return pathTo(previous, goal);
    }

    // Eine Priority Queue als binärer Heap: Was hineinkommt, hat eine
    // Priorität, und pop gibt immer das mit der kleinsten zurück. Die Einträge
    // stehen in einer Liste, als Baum: Die Kinder von i stehen bei 2i+1 und
    // 2i+2, und kein Kind ist kleiner als sein Elternteil.
    static class MinHeap {
        // ein Paar im Heap: die Priorität und der Wert, etwa die ID eines Knotens
        record Entry(double priority, String value) {}

        final List<Entry> items = new ArrayList<>();

        boolean isEmpty() {
            return items.isEmpty();
        }

        int size() {
            return items.size();
        }

        void push(double priority, String value) {
            items.add(new Entry(priority, value));
            int i = items.size() - 1;
            // nach oben, solange das Elternteil größer ist
            while (i > 0) {
                int parent = (i - 1) / 2;
                if (items.get(parent).priority() <= items.get(i).priority()) break;
                Collections.swap(items, parent, i);
                i = parent;
            }
        }

        Entry pop() {
            if (items.isEmpty()) return null;
            Entry top = items.get(0);
            Entry last = items.remove(items.size() - 1);
            if (!items.isEmpty()) {
                items.set(0, last);
                int i = 0;
                // nach unten, solange ein Kind kleiner ist
                while (true) {
                    int smallest = i;
                    for (int child : new int[] {2 * i + 1, 2 * i + 2}) {
                        if (child < items.size() && items.get(child).priority() < items.get(smallest).priority()) smallest = child;
                    }
                    if (smallest == i) break;
                    Collections.swap(items, smallest, i);
                    i = smallest;
                }
            }
            return top;
        }
    }

    // Dijkstra: der kürzeste Weg, mit einer Priority Queue. Ein Knoten kann
    // mehrmals darin stehen, wenn er einen besseren Weg bekommt; der alte
    // Eintrag ist dann veraltet und wird übersprungen.
    static List<String> dijkstra(Graph graph, String start, String goal) {
        Map<String, Double> costs = new HashMap<>(Map.of(start, 0.0));
        Map<String, String> previous = new HashMap<>();
        previous.put(start, null);
        Set<String> done = new HashSet<>();
        MinHeap heap = new MinHeap();
        heap.push(0, start);
        while (!heap.isEmpty()) {
            MinHeap.Entry entry = heap.pop();
            String node = entry.value();
            if (done.contains(node)) continue;
            done.add(node);
            visit(node);
            if (node.equals(goal)) break;
            for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
                double total = entry.priority() + edge.cost();
                if (costs.containsKey(edge.to()) && costs.get(edge.to()) <= total) continue;
                costs.put(edge.to(), total);
                previous.put(edge.to(), node);
                heap.push(total, edge.to());
            }
        }
        return pathTo(previous, goal);
    }

    // A*: wie Dijkstra, aber die Priorität ist der Weg bis hier plus eine
    // Schätzung für den Rest, die Luftlinie zum Ziel. Die Schätzung ist nie zu
    // groß, darum findet A* trotzdem den kürzesten Weg. In einem Graphen mit
    // Sekunden statt Metern teilt man die Luftlinie durch das größte Tempo.
    static List<String> aStar(Graph graph, String start, String goal) {
        Map<String, Double> costs = new HashMap<>(Map.of(start, 0.0));
        Map<String, String> previous = new HashMap<>();
        previous.put(start, null);
        Set<String> done = new HashSet<>();
        MinHeap heap = new MinHeap();
        heap.push(estimate(graph, start, goal), start);
        while (!heap.isEmpty()) {
            String node = heap.pop().value();
            if (done.contains(node)) continue;
            done.add(node);
            visit(node);
            if (node.equals(goal)) break;
            for (Edge edge : graph.edges.getOrDefault(node, List.of())) {
                double total = costs.get(node) + edge.cost();
                if (costs.containsKey(edge.to()) && costs.get(edge.to()) <= total) continue;
                costs.put(edge.to(), total);
                previous.put(edge.to(), node);
                heap.push(total + estimate(graph, edge.to(), goal), edge.to());
            }
        }
        return pathTo(previous, goal);
    }

    // die Schätzung für A*: die Luftlinie vom Knoten id zum Ziel, geteilt
    // durch das größte Tempo (1 in einem Graphen mit Metern)
    static double estimate(Graph graph, String id, String goal) {
        Point point = graph.nodes.get(id), target = graph.nodes.get(goal);
        if (point == null || target == null) return 0;
        return distance(point.lon(), point.lat(), target.lon(), target.lat()) / graph.fastest;
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

    // Der Graph für eine Art zu reisen: nur die Wege, die man so benutzen
    // darf, und als Kosten jeder Kante, wie viele Sekunden man für sie braucht.
    // Für das Auto gelten die Einbahnstraßen. fastest: das größte Tempo, in
    // Metern je Sekunde, für die Schätzung von A*.
    static Graph buildTravelGraph(List<Way> ways, String mode) {
        Map<String, Integer> speeds = SPEEDS.get(mode);
        Graph graph = new Graph();
        for (Way way : ways) {
            Integer speed = speeds.get(way.kind());
            if (speed == null) continue;
            double metresPerSecond = speed / 3.6;
            boolean forward = !mode.equals("car") || !way.oneway().equals("-1");
            boolean backward = !mode.equals("car") || way.oneway().isEmpty();
            for (int i = 0; i + 1 < way.points().size(); i++) {
                Point a = way.points().get(i), b = way.points().get(i + 1);
                graph.nodes.put(a.id(), a);
                graph.nodes.put(b.id(), b);
                double seconds = distance(a.lon(), a.lat(), b.lon(), b.lat()) / metresPerSecond;
                if (forward) graph.addEdge(a.id(), b.id(), seconds);
                if (backward) graph.addEdge(b.id(), a.id(), seconds);
            }
        }
        graph.fastest = Collections.max(speeds.values()) / 3.6;
        return graph;
    }

    // Eine Rundfahrt: costs[i][j] sagt, wie weit es von Ort i zu Ort j ist.
    // Eine Reihenfolge beginnt bei Ort 0 und kehrt am Ende zu ihm zurück.
    static double tourLength(double[][] costs, List<Integer> order) {
        double total = 0;
        for (int i = 0; i < order.size(); i++) total += costs[order.get(i)][order.get((i + 1) % order.size())];
        return total;
    }

    // Alle Reihenfolgen ausprobieren: sicher die beste, aber bei n Orten sind
    // es (n − 1)! viele
    static List<Integer> bestTour(double[][] costs) {
        List<Integer> left = new ArrayList<>();
        for (int i = 1; i < costs.length; i++) left.add(i);
        return tryAll(costs, List.of(0), left);
    }

    // Die kürzeste Rundfahrt, die mit order beginnt und danach die Orte in
    // left besucht: jeden übrigen Ort einmal anhängen und mit dem Rest
    // weiterprobieren; ist nichts mehr übrig, ist die Reihenfolge fertig.
    static List<Integer> tryAll(double[][] costs, List<Integer> order, List<Integer> left) {
        if (left.isEmpty()) return order;
        List<Integer> best = null;
        for (Integer next : left) {
            List<Integer> longer = new ArrayList<>(order);
            longer.add(next);
            List<Integer> rest = new ArrayList<>(left);
            rest.remove(next);
            List<Integer> tour = tryAll(costs, longer, rest);
            if (best == null || tourLength(costs, tour) < tourLength(costs, best)) best = tour;
        }
        return best;
    }

    // Immer zum nächsten Ort, an dem man noch nicht war: schnell, aber oft
    // nicht die beste
    static List<Integer> nearestTour(double[][] costs) {
        List<Integer> order = new ArrayList<>(List.of(0));
        List<Integer> left = new ArrayList<>();
        for (int i = 1; i < costs.length; i++) left.add(i);
        while (!left.isEmpty()) {
            int from = order.getLast();
            Integer nearest = left.get(0);
            for (Integer i : left) if (costs[from][i] < costs[from][nearest]) nearest = i;
            order.add(nearest);
            left.remove(nearest);
        }
        return order;
    }

    // 2-opt: ein Stück der Rundfahrt umdrehen, wenn sie dadurch kürzer wird,
    // und das so lange, bis kein Umdrehen mehr hilft
    static List<Integer> improveTour(double[][] costs, List<Integer> order) {
        order = new ArrayList<>(order);
        boolean better = true;
        while (better) {
            better = false;
            for (int i = 1; i < order.size() - 1; i++) {
                for (int j = i + 1; j < order.size(); j++) {
                    List<Integer> turned = new ArrayList<>(order);
                    Collections.reverse(turned.subList(i, j + 1));
                    if (tourLength(costs, turned) < tourLength(costs, order) - 1e-9) {
                        order = turned;
                        better = true;
                    }
                }
            }
        }
        return order;
    }
}
