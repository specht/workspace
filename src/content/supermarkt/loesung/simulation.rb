require_relative 'supermarket'

def simulate(checkouts)
  supermarket = Supermarket.new(checkouts)
  File.readlines('customers.csv').drop(1).each do |line|
    arrival, items = line.split(',').map(&:to_i)
    supermarket.arrive(Customer.new(items, arrival))
  end
  supermarket.average_wait
end

puts simulate([Checkout.new, Checkout.new, Checkout.new])
puts simulate([Checkout.new, Checkout.new, ExpressCheckout.new])
puts simulate([Checkout.new, Checkout.new, SelfCheckout.new, SelfCheckout.new])
