// Zwei verschiedene Ziffern a und b vorn in einem Code, die Prüfziffer dazu,
// dann a und b vertauscht: Merkt isValidEan den Zahlendreher?
public class Swaps {
    public static void main(String[] args) {
        int missed = 0;
        for (int a = 0; a <= 9; a++) {
            for (int b = 0; b <= 9; b++) {
                if (a == b) {
                    continue;
                }
                int[] digits = {a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
                String code = "" + a + b + "0000000000" + CheckDigits.checkDigit(digits);
                String swapped = code.substring(1, 2) + code.substring(0, 1) + code.substring(2);
                if (CheckDigits.isValidEan(swapped)) {
                    missed = missed + 1;
                }
            }
        }
        System.out.println("Unbemerkt: " + missed + " von 90");
    }
}
