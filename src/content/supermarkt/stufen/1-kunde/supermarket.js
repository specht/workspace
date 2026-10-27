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

module.exports = { Customer };
