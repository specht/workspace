require_relative "neuron"

training = digits("digits-train.csv", 0, 1)
test = digits("digits-test.csv", 0, 1)
weights, bias = train(training, 100)
puts "Richtig bei fremden Handschriften: #{(accuracy(weights, bias, test) * 100).round(1)} %"
puts (weights + [bias]).join(" ")
