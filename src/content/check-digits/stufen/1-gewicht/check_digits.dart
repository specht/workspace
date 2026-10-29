// Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)

int weight(int index) {
  if (index % 2 == 0) {
    return 1;
  } else {
    return 3;
  }
}
