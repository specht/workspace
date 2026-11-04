import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

// The turtle's commands in Java (docs/katas.md, »Turtle«). Java runs on the
// server, in a Kata container (TurtleCompiled): the person's code becomes
// the members of HsTurtle extends Turtle, as in a compact source file. Each
// command checks what it gets as the browser's worker does
// (turtle_worker.js, host) and writes one line to stdout,
// »\u001e command \t value \t line«, which the server reads back
// (TurtleCompiled.parse) and the page replays. What the program prints
// stands between those lines.
public class Turtle {
    static final int MAX_COMMANDS = 200000;
    static final int MAX_OUTPUT = 100000;
    static final double MAX_WIDTH = 50;
    static final int MAX_DEPTH = 10000;
    // the line of a command: for the first LINES commands each time, then
    // every LINE_EVERY (finding it costs a look at the stack)
    static final int LINES = 5000;
    static final int LINE_EVERY = 50;
    static final char MARK = '\u001e';

    static PrintStream out;
    static Set<String> colours = new HashSet<>();
    static boolean quiet;
    static int count;
    static int printed;
    static int depth;
    static int line;
    // the lines of the person's code in HsTurtle.java
    static int first;
    static int last;

    // ends every run: too many commands, or too much output
    static final class Stop extends Error {
        Stop() {
            super(null, null, false, false);
        }
    }

    // Counts what the program prints; past MAX_OUTPUT the program stops.
    static final class Counting extends OutputStream {
        final OutputStream to;

        Counting(OutputStream to) {
            this.to = to;
        }

        // nothing of the program before a call (quiet), as in the browser
        public void write(int b) throws IOException {
            if (quiet) return;
            to.write(b);
            if (++printed > MAX_OUTPUT) limit("output");
        }

        public void write(byte[] b, int off, int len) throws IOException {
            if (quiet) return;
            to.write(b, off, len);
            if ((printed += len) > MAX_OUTPUT) limit("output");
        }

        public void flush() throws IOException {
            to.flush();
        }
    }

    static void start(int first, int last) throws IOException {
        Turtle.first = first;
        Turtle.last = last;
        out = new PrintStream(new BufferedOutputStream(new FileOutputStream(FileDescriptor.out), 1 << 16), false, "UTF-8");
        // the person's output in the same stream, counted
        System.setOut(new PrintStream(new Counting(out), false, "UTF-8"));
        System.setErr(System.out);
        Matcher m = Pattern.compile("\"([a-z]+)\"\\s*:").matcher(Files.readString(Path.of("colors.json")));
        while (m.find()) colours.add(m.group(1));
        // what came so far, also when the run is stopped from outside
        Thread flusher = new Thread(() -> {
            while (true) {
                try {
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                    return;
                }
                System.out.flush();
            }
        });
        flusher.setDaemon(true);
        flusher.start();
    }

    // System.out writes through to out, so the order stays
    static void record(String text) {
        out.print(MARK);
        out.print(text);
        out.print('\n');
    }

    static void begin(int run, boolean silent) {
        quiet = silent;
        depth = 0;
        record("START\t" + run);
    }

    static void loaded() {
        quiet = false;
    }

    static void end(int run) {
        record("END\t" + run);
        out.flush();
    }

    static void limit(String what) {
        quiet = true;
        record("LIMIT\t" + what);
        out.flush();
        throw new Stop();
    }

    static String clean(String text) {
        return String.valueOf(text).replace('\t', ' ').replace('\n', ' ').replace('\r', ' ');
    }

    // an exception of the program: its class and message, and the person's
    // line where it happened
    static void error(Throwable e) {
        int at = 0;
        for (StackTraceElement f : e.getStackTrace()) {
            if ("HsTurtle.java".equals(f.getFileName()) && f.getLineNumber() >= first && f.getLineNumber() <= last) {
                at = f.getLineNumber() - first + 1;
                break;
            }
        }
        String message = e instanceof StackOverflowError
            ? "Zu tiefe Rekursion: Die Funktion ruft sich zu oft selbst auf."
            : e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage());
        record("ERROR\t" + at + "\t" + clean(message));
    }

    static final StackWalker WALKER = StackWalker.getInstance();

    static int line() {
        return WALKER.walk(s -> s.filter(f -> "HsTurtle.java".equals(f.getFileName()))
            .mapToInt(StackWalker.StackFrame::getLineNumber)
            .filter(n -> n >= first && n <= last)
            .findFirst().orElse(first - 1)) - first + 1;
    }

    static String number(double v) {
        return v == Math.rint(v) && Math.abs(v) < 1e15 ? String.valueOf((long) v) : String.valueOf(v);
    }

    static void send(String command, String value) {
        if (quiet) return;
        if (command.equals("push")) {
            if (depth >= MAX_DEPTH) throw new IllegalStateException("push: Die Schildkröte merkt sich schon " + MAX_DEPTH + " Stellen. Fehlt irgendwo ein pop?");
            depth++;
        }
        if (command.equals("pop")) {
            if (depth == 0) throw new IllegalStateException("pop: Die Schildkröte hat sich keine Stelle gemerkt. Vor jedem pop steht ein push.");
            depth--;
        }
        count++;
        if (count > MAX_COMMANDS) limit("commands");
        if (count <= LINES || count % LINE_EVERY == 0) line = line();
        record(command + "\t" + (value == null ? "" : value) + "\t" + line);
    }

    static double finite(String command, String example, double value) {
        if (Double.isFinite(value)) return value;
        throw new IllegalArgumentException(command + " braucht eine Zahl, zum Beispiel " + example + ", nicht " + value);
    }

    // a colour by name or code, as turtle_worker.js knows them (colorHex)
    static boolean known(String name) {
        String text = name.trim().toLowerCase();
        return colours.contains(text) || text.matches("#[0-9a-f]{6}|#[0-9a-f]{3}");
    }

    static String unknown(String command, String name) {
        return command + " kennt »" + name + "« nicht: Nimm einen englischen Farbnamen wie \"red\", \"orange\" oder \"darkgreen\" oder eine Farbe wie \"#ff8800\".";
    }

    // colours and widths are checked where they draw, as the worker does:
    // not in the program before a call (quiet)
    static void colourName(String command, String name) {
        if (quiet) return;
        if (name == null || !known(name)) throw new IllegalArgumentException(unknown(command, name));
        send(command, clean(name));
    }

    static void colourMix(String command, String a, String b, double amount) {
        if (quiet) return;
        if (a == null || !known(a)) throw new IllegalArgumentException(unknown(command, a));
        if (b == null || !known(b)) throw new IllegalArgumentException(unknown(command, b));
        send(command, "mix:" + clean(a) + ":" + clean(b) + ":" + number(finite(command, command + "(\"gold\", \"red\", 0.5)", amount)));
    }

    public void forward(double distance) {
        send("forward", number(finite("forward", "forward(100)", distance)));
    }

    public void back(double distance) {
        send("back", number(finite("back", "back(100)", distance)));
    }

    public void left(double angle) {
        send("left", number(finite("left", "left(90)", angle)));
    }

    public void right(double angle) {
        send("right", number(finite("right", "right(90)", angle)));
    }

    public void penUp() {
        send("pen_up", null);
    }

    public void penDown() {
        send("pen_down", null);
    }

    // A colour by name ("red", "#ff8800"), by a number, its hue on the
    // colour wheel (0 red, 120 green, 240 blue), or mixed from two:
    // color("gold", "red", 0.25) is a quarter of the way from gold to red.
    public void color(String name) {
        colourName("color", name);
    }

    public void color(double hue) {
        send("color", "hue:" + number(finite("color", "color(200)", hue)));
    }

    public void color(String a, String b, double amount) {
        colourMix("color", a, b, amount);
    }

    // the colour of the paper, with the same colours as color
    public void background(String name) {
        colourName("background", name);
    }

    public void background(double hue) {
        send("background", "hue:" + number(finite("background", "background(200)", hue)));
    }

    public void background(String a, String b, double amount) {
        colourMix("background", a, b, amount);
    }

    public void width(double size) {
        if (quiet) return;
        if (!(size > 0 && size <= MAX_WIDTH)) throw new IllegalArgumentException("width braucht eine Zahl über 0 und höchstens " + number(MAX_WIDTH) + ", nicht " + number(size) + ".");
        send("width", number(size));
    }

    // Remembers where the turtle is, where it looks, its pen, colour and
    // width; pop goes back to the last place remembered, without drawing.
    public void push() {
        send("push", null);
    }

    public void pop() {
        send("pop", null);
    }

    // A random number from low to high: a whole one for whole numbers
    // (random(1, 6) as a die), otherwise one with a fraction.
    static final Random RANDOM = new Random();

    public int random(int low, int high) {
        if (high < low) return random(high, low);
        return low + RANDOM.nextInt(high - low + 1);
    }

    public double random(double low, double high) {
        finite("random", "random(1, 6)", low);
        finite("random", "random(1, 6)", high);
        if (high < low) return random(high, low);
        return low + RANDOM.nextDouble() * (high - low);
    }
}
