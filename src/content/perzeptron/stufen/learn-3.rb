require_relative "neuron"

examples = penguins("penguins.csv", ["Adelie", "Gentoo"])
training = examples.select.with_index { |_, i| i.even? }
test = examples.select.with_index { |_, i| i.odd? }
weights, bias = train(training, 100)
puts "Richtig bei Pinguinen, die es nie gesehen hat: #{(accuracy(weights, bias, test) * 100).round(1)} %"
plot(examples, weights, bias, "plot.svg")
