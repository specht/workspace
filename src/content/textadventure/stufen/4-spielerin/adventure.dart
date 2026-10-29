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

class Player {
  Room _room;
  final List<Item> _bag = [];

  Player(this._room);

  Room get room => _room;

  String look() {
    return _room.describe();
  }

  String go(String direction) {
    final target = _room.exitTo(direction);
    if (target == null) {
      return 'Da geht es nicht weiter.';
    }
    _room = target;
    return look();
  }

  String take(String name) {
    final item = _room.take(name);
    if (item == null) {
      return 'Hier liegt nichts mit dem Namen $name.';
    }
    _bag.add(item);
    return 'Du nimmst ${item.name}.';
  }

  bool has(String name) => _bag.any((item) => item.name == name);

  String bag() {
    if (_bag.isEmpty) {
      return 'Deine Tasche ist leer.';
    }
    return "In deiner Tasche: ${_bag.map((item) => item.name).join(', ')}";
  }
}
