class Customer
  attr_reader :items, :arrival

  def initialize(items, arrival)
    @items = items
    @arrival = arrival
  end
end
