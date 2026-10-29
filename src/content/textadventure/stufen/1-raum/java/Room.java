import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// Ein Raum mit seinen Ausgängen und Gegenständen.
public class Room {
    private final String name;
    private final String description;
    private final Map<String, Room> exits = new LinkedHashMap<>();

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
}
