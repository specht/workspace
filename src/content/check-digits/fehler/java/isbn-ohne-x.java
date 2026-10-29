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

    public static boolean isValidEan(String code) {
        if (code.length() != 13) {
            return false;
        }
        int[] digits = new int[13];
        for (int i = 0; i < 13; i++) {
            char c = code.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
            digits[i] = c - '0';
        }
        // checkDigit liest nur die ersten zwölf
        return checkDigit(digits) == digits[12];
    }

    public static boolean isValidIsbn(String isbn) {
        String text = isbn.replace("-", "");
        if (text.length() != 10) {
            return false;
        }
        int total = 0;
        for (int i = 0; i < 10; i++) {
            char c = text.charAt(i);
            int value;
            if (c >= '0' && c <= '9') {
                value = c - '0';
            } else {
                return false;
            }
            total = total + (10 - i) * value;
        }
        return total % 11 == 0;
    }
}
