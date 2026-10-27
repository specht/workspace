const fs = require('fs');
const { Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket } = require('./supermarket');

function simulate(checkouts) {
  const supermarket = new Supermarket(checkouts);
  for (const line of fs.readFileSync('customers.csv', 'utf8').trim().split('\n').slice(1)) {
    const [arrival, items] = line.split(',').map(Number);
    supermarket.arrive(new Customer(items, arrival));
  }
  return supermarket.averageWait();
}

console.log(simulate([new Checkout(), new Checkout(), new Checkout()]));
console.log(simulate([new Checkout(), new Checkout(), new ExpressCheckout()]));
console.log(simulate([new Checkout(), new Checkout(), new SelfCheckout(), new SelfCheckout()]));
