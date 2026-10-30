import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Simulation {
    static double simulate(List<Checkout> checkouts) throws Exception {
        Supermarket supermarket = new Supermarket(checkouts);
        List<String> lines = Files.readAllLines(Path.of("customers.csv"));
        for (String line : lines.subList(1, lines.size())) {
            String[] parts = line.split(",");
            supermarket.arrive(new Customer(Integer.parseInt(parts[1]), Integer.parseInt(parts[0])));
        }
        return supermarket.averageWait();
    }

    static void main() throws Exception {
        IO.println(simulate(List.of(new Checkout(), new Checkout(), new Checkout())));
        IO.println(simulate(List.of(new Checkout(), new Checkout(), new ExpressCheckout())));
        IO.println(simulate(List.of(new Checkout(), new Checkout(), new SelfCheckout(), new SelfCheckout())));
    }
}
