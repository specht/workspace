public class SelfCheckout extends Checkout {
    @Override
    public int serviceTime(Customer customer) {
        return 30 + 6 * customer.getItems();
    }
}
