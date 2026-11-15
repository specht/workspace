// HeapMemory wie in der Data Structures Sandbox, ohne ihre Aufzeichnung:
// Damit läuft ein Heap aus der Sandbox (my_array_heap.dart) auch hier. In
// dessen erster Zeile wird aus import '../../lib/heap_sandbox.dart'
// einfach import 'heap_sandbox.dart'. Hier musst du nichts ändern.
class HeapMemory {
  final List<int> _cells = [];

  int get length => _cells.length;
  bool get isEmpty => _cells.isEmpty;
  int operator [](int index) => _cells[index];
  void operator []=(int index, int value) => _cells[index] = value;
  void add(int value) => _cells.add(value);
  int removeLast() => _cells.removeLast();
  void swap(int a, int b) {
    final t = _cells[a];
    _cells[a] = _cells[b];
    _cells[b] = t;
  }

  List<int> copy() => List<int>.of(_cells);
}
