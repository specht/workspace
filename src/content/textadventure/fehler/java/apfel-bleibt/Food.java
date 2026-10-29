// Etwas zu essen: Danach ist es weg.
public class Food extends Item {
    public Food(String name, String description) {
        super(name, description);
    }

    @Override
    public String use(Player player) {
        return "Du isst " + getName() + ". " + getDescription();
    }
}
