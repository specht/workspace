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

  bool get isDark => false;

  bool get isGoal => false;

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

class DarkRoom extends Room {
  DarkRoom(super.name, super.description);

  @override
  bool get isDark => true;
}

class GoalRoom extends Room {
  GoalRoom(super.name, super.description);

  @override
  bool get isGoal => true;
}

class Item {
  final String name;
  final String description;

  Item(this.name, this.description);

  String use(Player player) => 'Damit kannst du hier nichts anfangen.';

  bool get shines => false;
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

class Lamp extends Item {
  Lamp(super.name, super.description);

  @override
  String use(Player player) => '$name leuchtet schon.';

  @override
  bool get shines => true;
}

class Chest extends Item {
  final List<Item> _contents = [];

  Chest(super.name, super.description);

  void put(Item item) {
    _contents.add(item);
  }

  @override
  String use(Player player) {
    if (_contents.isEmpty) {
      return '$name ist leer.';
    }
    final names = _contents.map((item) => item.name).join(', ');
    for (final item in _contents) {
      player.room.addItem(item);
    }
    _contents.clear();
    return 'Du öffnest $name. Darin: $names';
  }
}

class Player {
  Room _room;
  final List<Item> _bag = [];

  Player(this._room);

  Room get room => _room;

  String look() {
    if (_room.isDark && !hasLight) {
      return 'Es ist stockdunkel. Du siehst nichts.';
    }
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
    if (_room.isDark && !hasLight) {
      return 'Im Dunkeln findest du nichts.';
    }
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

  bool get hasLight => _bag.any((item) => item.shines);

  String examine(String name) {
    for (final item in [..._bag, ..._room.items]) {
      if (item.name == name) {
        return item.description;
      }
    }
    return 'Hier gibt es nichts mit dem Namen $name.';
  }
}

class Game {
  final Player _player;
  bool _over = false;

  Game(this._player);

  String start() => _player.look();

  bool get isOver => _over;

  bool get isWon => _player.room.isGoal;

  String handle(String line) {
    final parts = line.trim().split(' ');
    final verb = parts.first;
    final word = parts.skip(1).join(' ');
    String answer;
    if (verb == 'gehe') {
      answer = _player.go(word);
    } else if (verb == 'nimm') {
      answer = _player.take(word);
    } else if (verb == 'benutze') {
      answer = _player.use(word);
    } else if (verb == 'untersuche') {
      answer = _player.examine(word);
    } else if (verb == 'schau') {
      answer = _player.look();
    } else if (verb == 'tasche') {
      answer = _player.bag();
    } else if (verb == 'ende') {
      _over = true;
      answer = 'Bis bald.';
    } else {
      answer = 'Das verstehe ich nicht. Befehle: gehe, nimm, benutze, untersuche, schau, tasche, ende';
    }
    if (isWon) {
      _over = true;
    }
    return answer;
  }
}

// Alle Räume, die man von start aus erreicht, start zuerst (gegeben).
List<Room> allRooms(Room start) {
  final seen = [start];
  final queue = [start];
  while (queue.isNotEmpty) {
    final room = queue.removeAt(0);
    for (final direction in room.directions) {
      final other = room.exitTo(direction)!;
      if (!seen.contains(other)) {
        seen.add(other);
        queue.add(other);
      }
    }
  }
  return seen;
}

// Deine Welt: ein Wohnheim, in dem das Licht ausgefallen ist.
Player buildWorld() {
  final room = Room('Zimmer 3', 'Dein Zimmer im Wohnheim. Der Strom ist weg.');
  final corridor = DarkRoom('Gang', 'Ein langer Gang mit Türen links und rechts.');
  final kitchen = Room('Küche', 'Im Kühlschrank brummt nichts mehr.');
  final fuse = GoalRoom('Sicherungskasten', 'Du legst den Schalter um, und überall geht das Licht an. Geschafft!');
  room.connect('flur', corridor);
  corridor.connect('zimmer', room);
  corridor.connect('küche', kitchen);
  kitchen.connect('flur', corridor);
  corridor.connect('kasten', fuse);
  corridor.lock('kasten', 'kastenschlüssel');
  room.addItem(Lamp('kerze', 'Eine Kerze, die schon brennt.'));
  kitchen.addItem(Key('kastenschlüssel', 'Ein kleiner Schlüssel mit einem Blitz darauf.'));
  kitchen.addItem(Food('keks', 'Ein bisschen weich, aber gut.'));
  return Player(room);
}

// Der Weg zum Ziel, Befehl für Befehl: Game spielt ihn durch.
const walkthrough = [
  'nimm kerze', 'gehe flur', 'gehe küche', 'nimm kastenschlüssel', 'gehe flur',
  'benutze kastenschlüssel', 'gehe kasten',
];
