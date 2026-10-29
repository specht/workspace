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
        if (room.isDark() && !hasLight()) {
            return "Es ist stockdunkel. Du siehst nichts.";
        }
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
        if (room.isDark() && !hasLight()) {
            return "Im Dunkeln findest du nichts.";
        }
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

    public void remove(Item item) {
        bag.remove(item);
    }

    public String use(String name) {
        for (Item item : bag) {
            if (item.getName().equals(name)) {
                return item.use(this);
            }
        }
        return "Du hast nichts mit dem Namen " + name + ".";
    }

    public boolean hasLight() {
        for (Item item : bag) {
            if (item.shines()) {
                return true;
            }
        }
        return false;
    }

    public String examine(String name) {
        List<Item> visible = new ArrayList<>(bag);
        visible.addAll(room.getItems());
        for (Item item : visible) {
            if (item.getName().equals(name)) {
                return item.getDescription();
            }
        }
        return "Hier gibt es nichts mit dem Namen " + name + ".";
    }
}
