// Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)

int weight(int index) {
  if (index % 2 == 0) {
    return 1;
  } else {
    return 3;
  }
}

int checkDigit(List<int> digits) {
  var total = 0;
  for (var i = 0; i <= 11; i++) {
    total = total + weight(i) * digits[i];
  }
  return 10 - total % 10;
}

bool isValidEan(String code) {
  if (code.length != 13) {
    return false;
  }
  for (final c in code.split('')) {
    if (!'0123456789'.contains(c)) {
      return false;
    }
  }
  final digits = code.split('').map(int.parse).toList();
  return checkDigit(digits.sublist(0, 12)) == digits[12];
}

bool isValidIsbn(String isbn) {
  final text = isbn.replaceAll('-', '');
  if (text.length != 10) {
    return false;
  }
  var total = 0;
  for (var i = 0; i < 10; i++) {
    final c = text[i];
    int value;
    if (c == 'X' && i == 9) {
      value = 10;
    } else if ('0123456789'.contains(c)) {
      value = int.parse(c);
    } else {
      return false;
    }
    total = total + (10 - i) * value;
  }
  return total % 11 == 0;
}
