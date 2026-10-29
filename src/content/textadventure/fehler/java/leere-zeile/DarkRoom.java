// Ein Raum, in dem man ohne Licht nichts sieht.
public class DarkRoom extends Room {
    public DarkRoom(String name, String description) {
        super(name, description);
    }

    @Override
    public boolean isDark() {
        return true;
    }
}
