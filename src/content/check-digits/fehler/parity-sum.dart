// Häufiger Fehler: die Zahl der Einsen statt ihres Rests bei der Division durch 2

int parity(List<int> bits) {
  var ones = 0;
  for (final bit in bits) {
    ones = ones + bit;
  }
  return ones;
}
