// Ein Heap im Stil der Data Structures Sandbox (MyArrayHeap mit HeapMemory),
// für die Beispiele: so, wie ihn Lernende aus der Sandbox mitbringen.
import 'heap_sandbox.dart';

class MyArrayHeap {
  final HeapMemory memory = HeapMemory();

  void insert(int value) {
    memory.add(value);
    var i = memory.length - 1;
    while (i > 0 && memory[(i - 1) ~/ 2] > memory[i]) {
      memory.swap(i, (i - 1) ~/ 2);
      i = (i - 1) ~/ 2;
    }
  }

  int? removeMin() {
    if (memory.isEmpty) return null;
    memory.swap(0, memory.length - 1);
    final min = memory.removeLast();
    var i = 0;
    while (true) {
      final left = 2 * i + 1, right = 2 * i + 2;
      var smallest = i;
      if (left < memory.length && memory[left] < memory[smallest]) smallest = left;
      if (right < memory.length && memory[right] < memory[smallest]) smallest = right;
      if (smallest == i) return min;
      memory.swap(i, smallest);
      i = smallest;
    }
  }

  int? peek() => memory.isEmpty ? null : memory[0];
  bool isEmpty() => memory.isEmpty;
}
