import java.util.ArrayList;
import java.util.List;

// Deine Welt: ein Wohnheim, in dem das Licht ausgefallen ist.
public class World {
    // Der Weg zum Ziel, Befehl für Befehl: Game spielt ihn durch.
    public static final List<String> WALKTHROUGH = List.of(
        "nimm kerze", "gehe flur", "gehe küche", "nimm kastenschlüssel", "gehe flur",
        "benutze kastenschlüssel", "gehe kasten");

    public static Player build() {
        Room room = new Room("Zimmer 3", "Dein Zimmer im Wohnheim. Der Strom ist weg.");
        Room corridor = new DarkRoom("Gang", "Ein langer Gang mit Türen links und rechts.");
        Room kitchen = new Room("Küche", "Im Kühlschrank brummt nichts mehr.");
        Room fuse = new GoalRoom("Sicherungskasten", "Du legst den Schalter um, und überall geht das Licht an. Geschafft!");
        room.connect("flur", corridor);
        corridor.connect("zimmer", room);
        corridor.connect("küche", kitchen);
        kitchen.connect("flur", corridor);
        corridor.connect("kasten", fuse);
        corridor.lock("kasten", "kastenschlüssel");
        room.addItem(new Lamp("kerze", "Eine Kerze, die schon brennt."));
        kitchen.addItem(new Key("kastenschlüssel", "Ein kleiner Schlüssel mit einem Blitz darauf."));
        kitchen.addItem(new Food("keks", "Ein bisschen weich, aber gut."));
        return new Player(room);
    }

    // Alle Räume, die man von start aus erreicht, start zuerst (gegeben).
    public static List<Room> allRooms(Room start) {
        List<Room> seen = new ArrayList<>(List.of(start));
        List<Room> queue = new ArrayList<>(List.of(start));
        while (!queue.isEmpty()) {
            Room room = queue.remove(0);
            for (String direction : room.getDirections()) {
                Room other = room.exitTo(direction);
                if (!seen.contains(other)) {
                    seen.add(other);
                    queue.add(other);
                }
            }
        }
        return seen;
    }
}
