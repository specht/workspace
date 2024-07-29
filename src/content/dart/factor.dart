import 'dart:io';

void main() {
  stdout.write('Enter a number: ');
  int number = int.parse(stdin.readLineSync()!);

  stdout.write('Prime factors of $number are: ');

  for (int i = 2; i <= number; i++) {
    while (number % i == 0) {
      stdout.write('$i ');
      number ~/= i;
    }
  }

  stdout.writeln();
}
