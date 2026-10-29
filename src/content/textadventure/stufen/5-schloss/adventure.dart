// Ein Textadventure aus Objekten: Nachts in der Schule.

class Room {
  final String name;
  final String description;
  final Map<String, Room> _exits = {};
  final Map<String, String> _locks = {};
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

  void lock(String direction, String keyName) {
    _locks[direction] = keyName;
  }

  bool isLocked(String direction) => _locks.containsKey(direction);

  bool unlock(String keyName) {
    final before = _locks.length;
    _locks.removeWhere((direction, key) => key == keyName);
    return _locks.length < before;
  }

  String describe() {
    final lines = [name, description];
    if (items.isNotEmpty) {
      lines.add("Hier liegt: ${items.map((item) => item.name).join(', ')}");
    }
    final exits = directions.map((d) => isLocked(d) ? '$d (verschlossen)' : d).toList();
    lines.add("Ausgänge: ${exits.isEmpty ? 'keine' : exits.join(', ')}");
    return lines.join('\n');
  }
}

class Item {
  final String name;
  final String description;

  Item(this.name, this.description);

  String use(Player player) => 'Damit kannst du hier nichts anfangen.';
}

class Key extends Item {
  Key(super.name, super.description);

  @override
  String use(Player player) {
    if (player.room.unlock(name)) {
      return 'Du schließt die Tür auf.';
    }
    return 'Hier passt $name in kein Schloss.';
  }
}

class Food extends Item {
  Food(super.name, super.description);

  @override
  String use(Player player) {
    player.remove(this);
    return 'Du isst $name. $description';
  }
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
    if (_room.isLocked(direction)) {
      return 'Die Tür nach $direction ist verschlossen.';
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

  void remove(Item item) {
    _bag.remove(item);
  }

  String use(String name) {
    for (final item in _bag) {
      if (item.name == name) {
        return item.use(this);
      }
    }
    return 'Du hast nichts mit dem Namen $name.';
  }
}
