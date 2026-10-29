// Häufiger Fehler: für die Spalte c wird die Zeile c genommen

int parity(List<int> bits) {
  var ones = 0;
  for (final bit in bits) {
    ones = ones + bit;
  }
  return ones % 2;
}

List<int>? findError(List<List<int>> grid) {
  // die Zeile mit einer ungeraden Zahl von Einsen
  int? row;
  for (var r = 0; r < grid.length; r++) {
    if (parity(grid[r]) == 1) {
      row = r;
    }
  }
  // die Spalte mit einer ungeraden Zahl von Einsen
  int? column;
  for (var c = 0; c < grid[0].length; c++) {
    final bits = c < grid.length ? grid[c] : <int>[];
    if (parity(bits) == 1) {
      column = c;
    }
  }
  // alles gerade: kein Fehler
  if (row == null || column == null) {
    return null;
  }
  return [row, column];
}
