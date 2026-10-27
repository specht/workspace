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
    start = [customer.arrival, @free_at].max
    @free_at = start + service_time(customer)
    start - customer.arrival
  end
end

class Supermarket
  def initialize(checkouts)
    @checkouts = checkouts
    @total_wait = 0
    @customers = 0
  end

  def arrive(customer)
    best = nil
    @checkouts.each do |checkout|
      best = checkout if best.nil? || checkout.free_at < best.free_at
    end
    wait = best.serve(customer)
    @total_wait += wait
    @customers += 1
    wait
  end

  def average_wait
    @total_wait / @customers
  end
end
