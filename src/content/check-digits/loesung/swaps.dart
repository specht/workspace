import 'check_digits.dart';

// Zwei verschiedene Ziffern a und b vorn in einem Code, die Prüfziffer dazu,
// dann a und b vertauscht: Merkt isValidEan den Zahlendreher?
void main() {
  var missed = 0;
  for (var a = 0; a <= 9; a++) {
    for (var b = 0; b <= 9; b++) {
      if (a == b) {
        continue;
      }
      final digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
      final code = [...digits, checkDigit(digits)].join();
      final swapped = code[1] + code[0] + code.substring(2);
      if (isValidEan(swapped)) {
        missed = missed + 1;
      }
    }
  }
  print('Unbemerkt: $missed von 90');
}
