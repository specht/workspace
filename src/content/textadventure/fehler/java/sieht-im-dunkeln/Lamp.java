// Eine Lampe leuchtet, solange man sie trägt.
public class Lamp extends Item {
    public Lamp(String name, String description) {
        super(name, description);
    }

    @Override
    public String use(Player player) {
        return getName() + " leuchtet schon.";
    }

    @Override
    public boolean shines() {
        return true;
    }
}
