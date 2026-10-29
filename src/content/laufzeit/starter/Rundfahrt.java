// Eine Rundfahrt zu den ersten n Sehenswürdigkeiten aus berlin-places.tsv,
// mit deinen Methoden aus Laufzeit.java: alle Reihenfolgen ausprobiert und
// immer zum nächsten Ort, mit der Zeit, die jede Art braucht.
//   java Rundfahrt.java 8
import java.nio.file.*;
import java.util.*;
import java.util.function.*;

public class Rundfahrt {
    public static void main(String[] args) throws Exception {
        int n = args.length > 0 ? Integer.parseInt(args[0]) : 8;
        List<Laufzeit.Place> sights = new ArrayList<>();
        for (String line : Files.readAllLines(Path.of("berlin-places.tsv"))) {
            String[] parts = line.split("\t");
            if (parts.length < 3 || !parts[0].equals("sight")) {
                continue;
            }
            String[] at = parts[2].split(",");
            sights.add(new Laufzeit.Place(parts[1], Double.parseDouble(at[0]), Double.parseDouble(at[1])));
        }
        List<Laufzeit.Place> places = sights.subList(0, Math.min(n, sights.size()));
        Map<String, Function<List<Laufzeit.Place>, List<Integer>>> ways = new LinkedHashMap<>();
        ways.put("alle Reihenfolgen", Laufzeit::bestTour);
        ways.put("nächster Ort", Laufzeit::nearestTour);
        for (var way : ways.entrySet()) {
            long start = System.nanoTime();
            List<Integer> tour = way.getValue().apply(places);
            double seconds = (System.nanoTime() - start) / 1e9;
            System.out.printf(Locale.ROOT, "%-18s %7.1f km  %9.3f s%n", way.getKey(), Laufzeit.tourLength(tour, places), seconds);
            List<String> names = new ArrayList<>();
            for (int i : tour) {
                names.add(places.get(i).name());
            }
            names.add(places.get(tour.getFirst()).name());
            System.out.println("  " + String.join(" → ", names));
        }
    }
}
