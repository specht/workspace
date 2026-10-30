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
        Set<String> seen = new HashSet<>();
        for (String name : names) {
            seen.add(name);
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
}
