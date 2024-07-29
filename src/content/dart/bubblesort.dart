import 'dart:math';

void main() {
  List<int> numbers = generateRandomNumbers(10);
  print('Original array: $numbers');

  bubbleSort(numbers);
  print('Sorted array: $numbers');
}

List<int> generateRandomNumbers(int count) {
  Random random = Random();
  List<int> numbers = [];

  for (int i = 0; i < count; i++) {
    numbers.add(random.nextInt(100));
  }

  return numbers;
}

void bubbleSort(List<int> numbers) {
  int n = numbers.length;

  for (int i = 0; i < n - 1; i++) {
    for (int j = 0; j < n - i - 1; j++) {
      if (numbers[j] > numbers[j + 1]) {
        int temp = numbers[j];
        numbers[j] = numbers[j + 1];
        numbers[j + 1] = temp;
      }
    }
  }
}
