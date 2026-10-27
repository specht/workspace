class Customer {
  #items;
  #arrival;

  constructor(items, arrival) {
    this.#items = items;
    this.#arrival = arrival;
  }

  get items() {
    return this.#items;
  }

  get arrival() {
    return this.#arrival;
  }
}

class Checkout {
  #freeAt = 0;

  get freeAt() {
    return this.#freeAt;
  }

  accepts(customer) {
    return true;
  }

  serviceTime(customer) {
    return 60 + 3 * customer.items;
  }

  serve(customer) {
    const start = Math.max(customer.arrival, this.#freeAt);
    this.#freeAt = start + this.serviceTime(customer);
    return start - customer.arrival;
  }
}

class ExpressCheckout extends Checkout {
  accepts(customer) {
    return customer.items <= 10;
  }
}

class SelfCheckout extends Checkout {
  serviceTime(customer) {
    return 30 + 6 * customer.items;
  }
}

class Supermarket {
  #checkouts;
  #totalWait = 0;
  #customers = 0;

  constructor(checkouts) {
    this.#checkouts = checkouts;
  }

  arrive(customer) {
    let best = null;
    for (const checkout of this.#checkouts) {
      if (checkout.accepts(customer) && (best === null || checkout.freeAt < best.freeAt)) {
        best = checkout;
      }
    }
    const wait = best.serve(customer);
    this.#totalWait += wait;
    this.#customers += 1;
    return wait;
  }

  averageWait() {
    return this.#totalWait / this.#customers;
  }
}

module.exports = { Customer, Checkout, ExpressCheckout, SelfCheckout, Supermarket };
