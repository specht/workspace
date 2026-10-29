// Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)
public class CheckDigits {
    public static int weight(int index) {
        if (index % 2 == 0) {
            return 1;
        } else {
            return 3;
        }
    }
}
