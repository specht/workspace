// Ein Schlüssel heißt wie sein Schloss.
public class Key extends Item {
    public Key(String name, String description) {
        super(name, description);
    }

    @Override
    public String use(Player player) {
        if (player.getRoom().unlock(getName())) {
            return "Du schließt die Tür auf.";
        }
        return "Hier passt " + getName() + " in kein Schloss.";
    }
}
