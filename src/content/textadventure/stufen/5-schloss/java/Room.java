import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// Ein Raum mit seinen Ausgängen und Gegenständen.
public class Room {
    private final String name;
    private final String description;
    private final Map<String, Room> exits = new LinkedHashMap<>();
    private final Map<String, String> locks = new HashMap<>();
    private final List<Item> items = new ArrayList<>();

    public Room(String name, String description) {
        this.name = name;
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public void connect(String direction, Room room) {
        exits.put(direction, room);
    }

    public Room exitTo(String direction) {
        return exits.get(direction);
    }

    public List<String> getDirections() {
        return new ArrayList<>(exits.keySet());
    }

    public List<Item> getItems() {
        return items;
    }

    public void addItem(Item item) {
        items.add(item);
    }

    public Item take(String name) {
        for (Item item : items) {
            if (item.getName().equals(name)) {
                items.remove(item);
                return item;
            }
        }
        return null;
    }

    public void lock(String direction, String keyName) {
        locks.put(direction, keyName);
    }

    public boolean isLocked(String direction) {
        return locks.containsKey(direction);
    }

    public boolean unlock(String keyName) {
        return locks.values().removeIf(key -> key.equals(keyName));
    }

    public String describe() {
        List<String> lines = new ArrayList<>();
        lines.add(name);
        lines.add(description);
        if (!items.isEmpty()) {
            List<String> names = new ArrayList<>();
            for (Item item : items) {
                names.add(item.getName());
            }
            lines.add("Hier liegt: " + String.join(", ", names));
        }
        List<String> ways = new ArrayList<>();
        for (String direction : getDirections()) {
            ways.add(isLocked(direction) ? direction + " (verschlossen)" : direction);
        }
        lines.add("Ausgänge: " + (ways.isEmpty() ? "keine" : String.join(", ", ways)));
        return String.join("\n", lines);
    }
}
