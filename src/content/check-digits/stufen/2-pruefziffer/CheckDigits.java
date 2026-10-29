// Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)
public class CheckDigits {
    public static int weight(int index) {
        if (index % 2 == 0) {
            return 1;
        } else {
            return 3;
        }
    }

    public static int checkDigit(int[] digits) {
        int total = 0;
        for (int i = 0; i <= 11; i++) {
            total = total + weight(i) * digits[i];
        }
        return (10 - total % 10) % 10;
    }
}
