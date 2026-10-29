// Ein Textadventure aus Objekten: Nachts in der Schule.

class Room {
  final String name;
  final String description;
  final Map<String, Room> _exits = {};
  final List<Item> items = [];

  Room(this.name, this.description);

  void connect(String direction, Room room) {
    _exits[direction] = room;
  }

  Room? exitTo(String direction) => _exits[direction];

  List<String> get directions => _exits.keys.toList();

  void addItem(Item item) {
    items.add(item);
  }

  Item? take(String name) {
    for (final item in items) {
      if (item.name == name) {
        items.remove(item);
        return item;
      }
    }
    return null;
  }

  String describe() {
    final lines = [name, description];
    if (items.isNotEmpty) {
      lines.add("Hier liegt: ${items.map((item) => item.name).join(', ')}");
    }
    final exits = directions;
    lines.add("Ausgänge: ${exits.isEmpty ? 'keine' : exits.join(', ')}");
    return lines.join('\n');
  }
}

class Item {
  final String name;
  final String description;

  Item(this.name, this.description);
}
