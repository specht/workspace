// Die Spielschleife für deine Welt (World.build): liest Befehle und gibt
// sie an das Spiel weiter.
//   java Play.java
public class Play {
    static void main() {
        Game game = new Game(World.build());
        IO.println(game.start());
        while (!game.isOver()) {
            String line = IO.readln("> ");
            if (line == null) {
                break;
            }
            IO.println(game.handle(line));
        }
    }
}
