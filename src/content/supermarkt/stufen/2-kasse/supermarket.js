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

  serviceTime(customer) {
    return 60 + 3 * customer.items;
  }

  serve(customer) {
    const start = Math.max(customer.arrival, this.#freeAt);
    this.#freeAt = start + this.serviceTime(customer);
    return start - customer.arrival;
  }
}

module.exports = { Customer, Checkout };
