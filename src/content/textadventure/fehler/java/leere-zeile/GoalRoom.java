// Das Ziel: Wer diesen Raum betritt, hat gewonnen.
public class GoalRoom extends Room {
    public GoalRoom(String name, String description) {
        super(name, description);
    }

    @Override
    public boolean isGoal() {
        return true;
    }
}
