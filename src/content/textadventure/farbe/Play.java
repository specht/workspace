import java.util.Scanner;

// Die Spielschleife für deine Welt (World.build): liest Befehle und gibt
// sie an das Spiel weiter.
//   java Play.java
public class Play {
    public static void main(String[] args) {
        Game game = new Game(World.build());
        System.out.println(Colors.paint(game.start()));
        Scanner input = new Scanner(System.in);
        while (!game.isOver()) {
            System.out.print("> ");
            if (!input.hasNextLine()) {
                break;
            }
            System.out.println(Colors.paint(game.handle(input.nextLine())));
        }
    }
}
