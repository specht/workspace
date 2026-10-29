// Misst, wie lange deine drei Methoden für immer mehr Namen brauchen:
// die ersten 1000 Straßennamen, 2000, 4000 … aus strassen.txt. Eine Art
// fällt heraus, wenn sie länger als 10 Sekunden braucht. Eine Zahl dahinter
// lässt erst bei so vielen Namen anfangen.
//   java Messen.java
//   java Messen.java 8000
import java.nio.file.*;
import java.util.*;
import java.util.function.*;

public class Messen {
    static final double LIMIT = 10.0;

    public static void main(String[] args) throws Exception {
        List<String> names = Files.readAllLines(Path.of("strassen.txt"));
        Map<String, ToIntFunction<List<String>>> ways = new LinkedHashMap<>();
        ways.put("distinctList", Laufzeit::distinctList);
        ways.put("distinctSorted", Laufzeit::distinctSorted);
        ways.put("distinctSet", Laufzeit::distinctSet);
        System.out.printf(Locale.ROOT, "%8s  %-16s %10s  %8s%n", "n", "Art", "Sekunden", "Faktor");
        List<String> active = new ArrayList<>(ways.keySet());
        Map<String, Double> before = new HashMap<>();
        int n = args.length > 0 ? Integer.parseInt(args[0]) : 1000;
        while (n <= names.size() && !active.isEmpty()) {
            List<String> part = names.subList(0, n);
            for (String way : new ArrayList<>(active)) {
                long start = System.nanoTime();
                ways.get(way).applyAsInt(part);
                double time = (System.nanoTime() - start) / 1e9;
                Double last = before.get(way);
                String factor = last != null && last > 0.0005 ? String.format(Locale.ROOT, "%.1f", time / last) : "";
                System.out.printf(Locale.ROOT, "%8d  %-16s %10.4f  %8s%n", n, way, time, factor);
                before.put(way, time);
                if (time > LIMIT) {
                    active.remove(way);
                }
            }
            n *= 2;
        }
    }
}
