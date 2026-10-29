// Ein Gegenstand, den man aufheben kann.
public class Item {
    private final String name;
    private final String description;

    public Item(String name, String description) {
        this.name = name;
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String use(Player player) {
        return "Damit kannst du hier nichts anfangen.";
    }
}
