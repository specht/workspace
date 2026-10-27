public class ExpressCheckout extends Checkout {
    @Override
    public boolean accepts(Customer customer) {
        return customer.getItems() <= 10;
    }
}
