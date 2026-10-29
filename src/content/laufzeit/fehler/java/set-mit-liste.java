// Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
// eine Rundfahrt durch Berlin.
import java.util.*;

public class Laufzeit {
    // Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
    public static int distinctList(List<String> names) {
        List<String> seen = new ArrayList<>();
        for (String name : names) {
            if (!seen.contains(name)) {
                seen.add(name);
            }
        }
        return seen.size();
    }

    // Sortiert stehen gleiche Namen nebeneinander.
    public static int distinctSorted(List<String> names) {
        List<String> sorted = new ArrayList<>(names);
        Collections.sort(sorted);
        int count = 0;
        for (int i = 0; i < sorted.size(); i++) {
            if (i == 0 || !sorted.get(i).equals(sorted.get(i - 1))) {
                count++;
            }
        }
        return count;
    }

    // Eine Menge findet ohne Suchen, ob sie einen Namen schon hat.
    public static int distinctSet(List<String> names) {
        List<String> seen = new ArrayList<>();
        for (String name : names) {
            if (!seen.contains(name)) {
                seen.add(name);
            }
        }
        return seen.size();
    }

    // Fibonacci mit einem Gedächtnis: Jede Zahl wird nur einmal ausgerechnet.
    public static long fibMemo(int n) {
        return fibMemo(n, new HashMap<>());
    }

    static long fibMemo(int n, Map<Integer, Long> memo) {
        if (n < 2) {
            return n;
        }
        if (!memo.containsKey(n)) {
            memo.put(n, fibMemo(n - 1, memo) + fibMemo(n - 2, memo));
        }
        return memo.get(n);
    }

    // Gibt es zwei Zahlen an verschiedenen Stellen, die zusammen target ergeben?
    public static boolean pairWithSum(List<Integer> numbers, int target) {
        Set<Integer> seen = new HashSet<>();
        for (int x : numbers) {
            if (seen.contains(target - x)) {
                return true;
            }
            seen.add(x);
        }
        return false;
    }

    // Ein Ort: Name, Länge und Breite (gegeben).
    public record Place(String name, double lon, double lat) {}

    // Wie weit es von a nach b ist, in Kilometern Luftlinie (gegeben).
    public static double distance(Place a, Place b) {
        double dx = (b.lon() - a.lon()) * 111.32 * Math.cos((a.lat() + b.lat()) / 2 * Math.PI / 180);
        double dy = (b.lat() - a.lat()) * 110.57;
        return Math.hypot(dx, dy);
    }

    // Die Länge einer Rundfahrt: von Ort zu Ort in der Reihenfolge der Nummern
    // und am Ende zurück zum ersten.
    public static double tourLength(List<Integer> tour, List<Place> places) {
        double total = 0;
        for (int i = 0; i < tour.size(); i++) {
            int from = tour.get(i);
            int to = tour.get((i + 1) % tour.size());
            total += distance(places.get(from), places.get(to));
        }
        return total;
    }

    // Alle Reihenfolgen einer Liste, wie in den Katas zur Rekursion.
    static List<List<Integer>> permutations(List<Integer> items) {
        List<List<Integer>> result = new ArrayList<>();
        if (items.isEmpty()) {
            result.add(new ArrayList<>());
            return result;
        }
        for (int i = 0; i < items.size(); i++) {
            List<Integer> rest = new ArrayList<>(items);
            int first = rest.remove(i);
            for (List<Integer> order : permutations(rest)) {
                order.add(0, first);
                result.add(order);
            }
        }
        return result;
    }

    // Alle Reihenfolgen ausprobieren; der erste Ort bleibt vorn.
    public static List<Integer> bestTour(List<Place> places) {
        List<Integer> rest = new ArrayList<>();
        for (int i = 1; i < places.size(); i++) {
            rest.add(i);
        }
        List<Integer> best = null;
        double shortest = Double.POSITIVE_INFINITY;
        for (List<Integer> order : permutations(rest)) {
            List<Integer> tour = new ArrayList<>(List.of(0));
            tour.addAll(order);
            double length = tourLength(tour, places);
            if (length < shortest) {
                best = tour;
                shortest = length;
            }
        }
        return best;
    }

    // Immer zum nächsten Ort, an dem man noch nicht war.
    public static List<Integer> nearestTour(List<Place> places) {
        List<Integer> tour = new ArrayList<>(List.of(0));
        List<Integer> left = new ArrayList<>();
        for (int i = 1; i < places.size(); i++) {
            left.add(i);
        }
        while (!left.isEmpty()) {
            Place here = places.get(tour.getLast());
            int nearest = left.getFirst();
            for (int i : left) {
                if (distance(here, places.get(i)) < distance(here, places.get(nearest))) {
                    nearest = i;
                }
            }
            tour.add(nearest);
            left.remove(Integer.valueOf(nearest));
        }
        return tour;
    }
}
