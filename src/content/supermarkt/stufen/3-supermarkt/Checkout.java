public class Checkout {
    private int freeAt = 0;

    public int getFreeAt() {
        return freeAt;
    }

    public int serviceTime(Customer customer) {
        return 60 + 3 * customer.getItems();
    }

    public int serve(Customer customer) {
        int start = Math.max(customer.getArrival(), freeAt);
        freeAt = start + serviceTime(customer);
        return start - customer.getArrival();
    }
}
