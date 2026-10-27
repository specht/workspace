class Customer
  attr_reader :items, :arrival

  def initialize(items, arrival)
    @items = items
    @arrival = arrival
  end
end

class Checkout
  attr_reader :free_at

  def initialize
    @free_at = 0
  end

  def service_time(customer)
    60 + 3 * customer.items
  end

  def serve(customer)
    start = @free_at
    @free_at = start + service_time(customer)
    start - customer.arrival
  end
end
