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
}
