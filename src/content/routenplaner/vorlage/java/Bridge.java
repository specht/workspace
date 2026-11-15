// Die Brücke zwischen dem Server und deinen Methoden in Routes.java. Der
// Server übersetzt alle .java-Dateien dieses Ordners, startet sie (./run java)
// und schickt ihr Fragen, eine Zeile JSON je Frage, über eine Verbindung auf
// diesem Rechner; sie antwortet ebenso. Was Routes.java ausgibt, erscheint im
// Terminal. Hier musst du nichts ändern.
import java.io.*;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class Bridge {
    static final Map<String, List<Way>> ways = new HashMap<>();   // Datei -> Wege
    static final Map<String, Graph> graphs = new HashMap<>();     // Art zu reisen -> Graph

    public static void main(String[] args) throws IOException {
        try (Socket socket = new Socket("127.0.0.1", Integer.parseInt(System.getenv("ROUTES_PORT")))) {
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
            Writer out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream(), StandardCharsets.UTF_8));
            String line;
            while ((line = in.readLine()) != null) {
                String reply;
                try {
                    reply = answer((Map<?, ?>) new Json(line).value());
                } catch (UnsupportedOperationException e) {
                    reply = "{\"missing\":" + Json.write(String.valueOf(e.getMessage())) + "}";
                } catch (Throwable e) {
                    System.err.println(e);
                    Arrays.stream(e.getStackTrace()).filter(f -> !"Bridge.java".equals(f.getFileName())).limit(8)
                        .forEach(f -> System.err.println("    at " + f));
                    String text = String.valueOf(e);
                    reply = "{\"error\":" + Json.write(text.length() > 300 ? text.substring(0, 300) : text) + "}";
                }
                out.write(reply + "\n");
                out.flush();
            }
        }
    }

    static Graph graphOf(String mode, String file) throws IOException {
        Graph graph = graphs.get(mode);
        if (graph == null) {
            if (!ways.containsKey(file)) ways.put(file, Routes.readWays(file));
            graph = mode.equals("length") ? Routes.buildGraph(ways.get(file)) : Routes.buildTravelGraph(ways.get(file), mode);
            graphs.put(mode, graph);
        }
        return graph;
    }

    static String answer(Map<?, ?> request) throws IOException {
        switch ((String) request.get("op")) {
            case "graph": {
                Graph g = graphOf((String) request.get("mode"), (String) request.get("file"));
                StringBuilder s = new StringBuilder("{\"graph\":{\"nodes\":{");
                String comma = "";
                for (Map.Entry<String, Point> e : g.nodes.entrySet()) {
                    s.append(comma).append(Json.write(e.getKey())).append(":[").append(e.getValue().lon()).append(',').append(e.getValue().lat()).append(']');
                    comma = ",";
                }
                s.append("},\"edges\":{");
                comma = "";
                for (Map.Entry<String, List<Edge>> e : g.edges.entrySet()) {
                    s.append(comma).append(Json.write(e.getKey())).append(":[");
                    String inner = "";
                    for (Edge edge : e.getValue()) {
                        s.append(inner).append('[').append(Json.write(edge.to())).append(',').append(edge.cost()).append(']');
                        inner = ",";
                    }
                    s.append(']');
                    comma = ",";
                }
                return s.append("},\"fastest\":").append(g.fastest).append("}}").toString();
            }
            case "search": {
                Graph graph = graphs.get(request.get("mode"));
                if (graph == null) throw new IllegalStateException("Kein Graph für " + request.get("mode"));
                String start = (String) request.get("start"), goal = (String) request.get("goal");
                long began = System.nanoTime();
                Search result = switch ((String) request.get("fn")) {
                    case "breadth_first" -> Routes.breadthFirst(graph, start, goal);
                    case "dijkstra" -> Routes.dijkstra(graph, start, goal);
                    case "a_star" -> Routes.aStar(graph, start, goal);
                    default -> throw new IllegalArgumentException("Keine Suche " + request.get("fn"));
                };
                double took = (System.nanoTime() - began) / 1e9;
                return "{\"path\":" + Json.write(result.path()) + ",\"visited\":" + Json.write(result.visited())
                    + ",\"took\":" + took + "}";
            }
            case "call": {
                List<?> a = (List<?>) request.get("args");
                long began = System.nanoTime();
                Object result = switch ((String) request.get("fn")) {
                    case "tour_length" -> Routes.tourLength(costs(a.get(0)), order(a.get(1)));
                    case "best_tour" -> Routes.bestTour(costs(a.get(0)));
                    case "nearest_tour" -> Routes.nearestTour(costs(a.get(0)));
                    case "improve_tour" -> Routes.improveTour(costs(a.get(0)), order(a.get(1)));
                    default -> throw new IllegalArgumentException("Keine Methode " + request.get("fn"));
                };
                return "{\"result\":" + Json.write(result) + ",\"took\":" + (System.nanoTime() - began) / 1e9 + "}";
            }
            default:
                return "{\"error\":" + Json.write("Unbekannte Frage: " + request.get("op")) + "}";
        }
    }

    static double[][] costs(Object value) {
        List<?> rows = (List<?>) value;
        double[][] c = new double[rows.size()][];
        for (int i = 0; i < c.length; i++) {
            List<?> row = (List<?>) rows.get(i);
            c[i] = new double[row.size()];
            for (int j = 0; j < c[i].length; j++) c[i][j] = ((Number) row.get(j)).doubleValue();
        }
        return c;
    }

    static List<Integer> order(Object value) {
        List<Integer> o = new ArrayList<>();
        for (Object x : (List<?>) value) o.add(((Number) x).intValue());
        return o;
    }

    // Gerade so viel JSON, wie die Brücke braucht: lesen und schreiben
    static class Json {
        final String text;
        int at = 0;

        Json(String text) { this.text = text; }

        Object value() {
            skip();
            char c = text.charAt(at);
            if (c == '{') {
                Map<String, Object> map = new LinkedHashMap<>();
                at++;
                skip();
                if (text.charAt(at) == '}') { at++; return map; }
                while (true) {
                    skip();
                    String key = (String) value();
                    skip();
                    at++; // :
                    map.put(key, value());
                    skip();
                    if (text.charAt(at++) == '}') return map;
                }
            }
            if (c == '[') {
                List<Object> list = new ArrayList<>();
                at++;
                skip();
                if (text.charAt(at) == ']') { at++; return list; }
                while (true) {
                    list.add(value());
                    skip();
                    if (text.charAt(at++) == ']') return list;
                }
            }
            if (c == '"') {
                StringBuilder s = new StringBuilder();
                at++;
                while (text.charAt(at) != '"') {
                    char ch = text.charAt(at++);
                    if (ch == '\\') {
                        char e = text.charAt(at++);
                        switch (e) {
                            case 'n' -> s.append('\n');
                            case 't' -> s.append('\t');
                            case 'r' -> s.append('\r');
                            case 'b' -> s.append('\b');
                            case 'f' -> s.append('\f');
                            case 'u' -> { s.append((char) Integer.parseInt(text.substring(at, at + 4), 16)); at += 4; }
                            default -> s.append(e);
                        }
                    } else s.append(ch);
                }
                at++;
                return s.toString();
            }
            if (text.startsWith("true", at)) { at += 4; return true; }
            if (text.startsWith("false", at)) { at += 5; return false; }
            if (text.startsWith("null", at)) { at += 4; return null; }
            int begin = at;
            while (at < text.length() && "+-0123456789.eE".indexOf(text.charAt(at)) >= 0) at++;
            return Double.parseDouble(text.substring(begin, at));
        }

        void skip() {
            while (at < text.length() && Character.isWhitespace(text.charAt(at))) at++;
        }

        static String write(Object value) {
            if (value == null) return "null";
            if (value instanceof String s) {
                StringBuilder b = new StringBuilder("\"");
                for (char c : s.toCharArray()) {
                    if (c == '"' || c == '\\') b.append('\\').append(c);
                    else if (c < 0x20) b.append(String.format("\\u%04x", (int) c));
                    else b.append(c);
                }
                return b.append('"').toString();
            }
            if (value instanceof Double d) return d.isNaN() || d.isInfinite() ? "null" : d.toString();
            if (value instanceof Number || value instanceof Boolean) return value.toString();
            if (value instanceof Collection<?> c) {
                StringBuilder b = new StringBuilder("[");
                String comma = "";
                for (Object x : c) { b.append(comma).append(write(x)); comma = ","; }
                return b.append(']').toString();
            }
            return write(value.toString());
        }
    }
}
