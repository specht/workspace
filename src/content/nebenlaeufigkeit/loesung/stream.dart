import 'dart:async';

Future<int> transfer(int n) async {
  final controller = StreamController<int>();
  Future<void> produce() async {
    for (var i = 1; i <= n; i++) {
      controller.add(i);
      await Future.delayed(Duration.zero);
    }
    await controller.close();
  }

  produce();
  var sum = 0;
  await for (final x in controller.stream) {
    sum += x;
  }
  return sum;
}
