require_relative "neuron"

examples = penguins("penguins.csv", ["Adelie", "Gentoo"])
weights, bias = train(examples, 100)
puts "Gewichte: #{weights.map { |w| w.round(1) }}, Bias: #{bias.round(1)}"
