import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// Farbe im Terminal (textadventure.md, »Farbe im Terminal«): Text in einer
// ANSI-Escape-Sequenz, und eine Antwort des Spiels, bemalt. Das Spiel selbst
// bleibt, wie es ist; nur die Spielschleife bemalt, was sie ausgibt.
public class Colors {
    public static final int BOLD = 1;
    public static final int GREEN = 32;
    public static final int BLUE = 34;

    private static final Map<String, Integer> LISTS = new LinkedHashMap<>();

    static {
        LISTS.put("Hier liegt: ", GREEN);
        LISTS.put("In deiner Tasche: ", GREEN);
        LISTS.put("Ausgänge: ", BLUE);
    }

    public static String color(String text, int code) {
        return "\u001B[" + code + "m" + text + "\u001B[0m";
    }

    public static String paint(String answer) {
        String[] lines = answer.split("\n", -1);
        boolean room = false;
        for (String line : lines) {
            if (line.startsWith("Ausgänge: ")) {
                room = true;
            }
        }
        List<String> painted = new ArrayList<>();
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            String label = null;
            for (String start : LISTS.keySet()) {
                if (line.startsWith(start)) {
                    label = start;
                }
            }
            if (i == 0 && room) {
                painted.add(color(line, BOLD));
            } else if (label != null && !line.equals("Ausgänge: keine")) {
                List<String> names = new ArrayList<>();
                for (String name : line.substring(label.length()).split(", ")) {
                    names.add(color(name, LISTS.get(label)));
                }
                painted.add(label + String.join(", ", names));
            } else {
                painted.add(line);
            }
        }
        return String.join("\n", painted);
    }
}
