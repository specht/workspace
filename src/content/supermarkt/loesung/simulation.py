from supermarket import Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket


def simulate(checkouts):
    supermarket = Supermarket(checkouts)
    with open('customers.csv') as f:
        for line in f.readlines()[1:]:
            arrival, items = [int(x) for x in line.split(',')]
            supermarket.arrive(Customer(items, arrival))
    return supermarket.average_wait()


print(simulate([Checkout(), Checkout(), Checkout()]))
print(simulate([Checkout(), Checkout(), ExpressCheckout()]))
print(simulate([Checkout(), Checkout(), SelfCheckout(), SelfCheckout()]))
