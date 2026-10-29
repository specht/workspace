// Das Spiel: versteht eine getippte Zeile und gibt die Antwort zurück.
public class Game {
    private final Player player;
    private boolean over = false;

    public Game(Player player) {
        this.player = player;
    }

    public String start() {
        return player.look();
    }

    public boolean isOver() {
        return over;
    }

    public boolean isWon() {
        return player.getRoom().isGoal();
    }

    public String handle(String line) {
        String[] parts = line.strip().split(" ", 2);
        String verb = parts[0];
        String word = parts.length > 1 ? parts[1] : "";
        String answer;
        if (verb.equals("gehe")) {
            answer = player.go(word);
        } else if (verb.equals("nimm")) {
            answer = player.take(word);
        } else if (verb.equals("benutze")) {
            answer = player.use(word);
        } else if (verb.equals("untersuche")) {
            answer = player.examine(word);
        } else if (verb.equals("schau")) {
            answer = player.look();
        } else if (verb.equals("tasche")) {
            answer = player.bag();
        } else if (verb.equals("ende")) {
            over = true;
            answer = "Bis bald.";
        } else {
            answer = "Das verstehe ich nicht. Befehle: gehe, nimm, benutze, untersuche, schau, tasche, ende";
        }
        if (isWon()) {
            over = true;
        }
        return answer;
    }
}
