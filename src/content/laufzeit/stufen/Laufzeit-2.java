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
}
