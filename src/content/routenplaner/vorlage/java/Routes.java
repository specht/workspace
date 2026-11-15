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
        throw new UnsupportedOperationException("readWays");
    }

    // Der Abstand zweier Punkte in Metern
    static double distance(double lon1, double lat1, double lon2, double lat2) {
        throw new UnsupportedOperationException("distance");
    }

    // Der Graph aus den Wegen: in nodes jeder Punkt, in edges für jeden Punkt
    // die Kanten zu seinen Nachbarn, in beide Richtungen, mit Metern als Kosten
    static Graph buildGraph(List<Way> ways) {
        throw new UnsupportedOperationException("buildGraph");
    }

    // Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
    // gefundenen Knoten, von welchem aus er gefunden wurde (der Start von null).
    // Gibt die IDs vom Start bis zum Ziel zurück, null, wenn das Ziel nicht dabei ist.
    static List<String> pathTo(Map<String, String> previous, String goal) {
        throw new UnsupportedOperationException("pathTo");
    }

    // Breitensuche: der Weg über die wenigsten Kanten, und die besuchten
    // Knoten in der Reihenfolge der Suche
    static Search breadthFirst(Graph graph, String start, String goal) {
        throw new UnsupportedOperationException("breadthFirst");
    }

    // Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadthFirst.
    static Search dijkstra(Graph graph, String start, String goal) {
        throw new UnsupportedOperationException("dijkstra");
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
