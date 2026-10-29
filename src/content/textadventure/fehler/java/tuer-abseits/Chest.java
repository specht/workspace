import java.util.ArrayList;
import java.util.List;

// Eine Kiste mit Inhalt: Öffnet man sie, liegt der Inhalt im Raum.
public class Chest extends Item {
    private final List<Item> contents = new ArrayList<>();

    public Chest(String name, String description) {
        super(name, description);
    }

    public void put(Item item) {
        contents.add(item);
    }

    @Override
    public String use(Player player) {
        if (contents.isEmpty()) {
            return getName() + " ist leer.";
        }
        List<String> names = new ArrayList<>();
        for (Item item : contents) {
            names.add(item.getName());
            player.getRoom().addItem(item);
        }
        contents.clear();
        return "Du öffnest " + getName() + ". Darin: " + String.join(", ", names);
    }
}
