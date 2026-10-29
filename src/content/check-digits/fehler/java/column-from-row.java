// Häufiger Fehler: für die Spalte c wird die Zeile c genommen
public class ParityGrid {
    public static int parity(int[] bits) {
        int ones = 0;
        for (int bit : bits) {
            ones = ones + bit;
        }
        return ones % 2;
    }

    public static int[] findError(int[][] grid) {
        // die Zeile mit einer ungeraden Zahl von Einsen
        int row = -1;
        for (int r = 0; r < grid.length; r++) {
            if (parity(grid[r]) == 1) {
                row = r;
            }
        }
        // die Spalte mit einer ungeraden Zahl von Einsen
        int column = -1;
        for (int c = 0; c < grid[0].length; c++) {
            int[] bits = c < grid.length ? grid[c] : new int[0];
            if (parity(bits) == 1) {
                column = c;
            }
        }
        // alles gerade: kein Fehler
        if (row == -1 || column == -1) {
            return null;
        }
        return new int[] {row, column};
    }
}
