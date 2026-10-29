import java.util.Scanner;

// Nachts in der Schule: eine Welt aus deinen Klassen. Spielen mit
// java School.java.
public class School {
    public static Player build() {
        Room classroom = new Room("Raum 204", "Du bist über deinem Heft eingeschlafen. Draußen ist es dunkel, die Schule ist still.");
        Room hall = new Room("Flur", "Nur die grünen Notausgangsschilder leuchten.");
        Room staff = new Room("Lehrerzimmer", "Stapel von Heften, eine kalte Kaffeemaschine.");
        Room stairs = new Room("Treppenhaus", "Deine Schritte hallen.");
        Room entrance = new Room("Eingangshalle", "Hinter der großen Glastür liegt der Schulhof.");
        Room caretaker = new Room("Hausmeisterraum", "Werkzeug an der Wand, ein Radio, das leise rauscht.");
        Room cellar = new DarkRoom("Keller", "Heizungsrohre, alte Stühle und ein Schlüsselbrett.");
        Room outside = new GoalRoom("Schulhof", "Kühle Nachtluft. Du bist draußen. Geschafft!");

        classroom.connect("westen", hall);
        hall.connect("osten", classroom);
        hall.connect("norden", staff);
        staff.connect("süden", hall);
        hall.connect("unten", stairs);
        stairs.connect("oben", hall);
        stairs.connect("unten", entrance);
        entrance.connect("oben", stairs);
        entrance.connect("westen", caretaker);
        caretaker.connect("osten", entrance);
        entrance.connect("unten", cellar);
        cellar.connect("oben", entrance);
        entrance.connect("süden", outside);

        hall.lock("norden", "lehrerschlüssel");
        entrance.lock("süden", "haustürschlüssel");

        classroom.addItem(new Item("zettel", "»Der Hausmeister lässt seine Taschenlampe nie aus dem Raum.«"));
        classroom.addItem(new Food("apfel", "Knackig und süß."));
        caretaker.addItem(new Lamp("taschenlampe", "Eine schwere Taschenlampe. Sie leuchtet."));
        cellar.addItem(new Key("lehrerschlüssel", "Am Anhänger steht: Lehrerzimmer."));
        staff.addItem(new Key("haustürschlüssel", "Ein großer Schlüssel mit einem roten Anhänger."));

        return new Player(classroom);
    }

    // Die Spielschleife: liest Befehle und gibt sie an das Spiel weiter.
    public static void main(String[] args) {
        Game game = new Game(build());
        System.out.println(game.start());
        Scanner input = new Scanner(System.in);
        while (!game.isOver()) {
            System.out.print("> ");
            if (!input.hasNextLine()) {
                break;
            }
            System.out.println(game.handle(input.nextLine()));
        }
    }
}
