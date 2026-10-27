import java.util.List;

public class Supermarket {
    private List<Checkout> checkouts;
    private int totalWait = 0;
    private int customers = 0;

    public Supermarket(List<Checkout> checkouts) {
        this.checkouts = checkouts;
    }

    public int arrive(Customer customer) {
        Checkout best = null;
        for (Checkout checkout : checkouts) {
            if (best == null || checkout.getFreeAt() < best.getFreeAt()) {
                best = checkout;
            }
        }
        int wait = best.serve(customer);
        totalWait += wait;
        customers += 1;
        return wait;
    }

    public double averageWait() {
        return (double) totalWait / customers;
    }
}
