import java.util.ArrayList;
import java.util.List;

// Die Spielerin: Sie steht in einem Raum und hat eine Tasche.
public class Player {
    private Room room;
    private final List<Item> bag = new ArrayList<>();

    public Player(Room room) {
        this.room = room;
    }

    public Room getRoom() {
        return room;
    }

    public String look() {
        return room.describe();
    }

    public String go(String direction) {
        Room target = room.exitTo(direction);
        if (target == null) {
            return "Da geht es nicht weiter.";
        }
        room = target;
        return look();
    }

    public String take(String name) {
        Item item = room.take(name);
        if (item == null) {
            return "Hier liegt nichts mit dem Namen " + name + ".";
        }
        bag.add(item);
        return "Du nimmst " + item.getName() + ".";
    }

    public boolean has(String name) {
        for (Item item : bag) {
            if (item.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }

    public String bag() {
        if (bag.isEmpty()) {
            return "Deine Tasche ist leer.";
        }
        List<String> names = new ArrayList<>();
        for (Item item : bag) {
            names.add(item.getName());
        }
        return "In deiner Tasche: " + String.join(", ", names);
    }
}
