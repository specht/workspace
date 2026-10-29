// Häufiger Fehler: die Zahl der Einsen statt ihres Rests bei der Division durch 2
public class ParityGrid {
    public static int parity(int[] bits) {
        int ones = 0;
        for (int bit : bits) {
            ones = ones + bit;
        }
        return ones;
    }
}
