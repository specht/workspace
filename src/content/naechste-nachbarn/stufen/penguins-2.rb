require_relative "knn"

examples = penguins("penguins.csv")
training = examples.select.with_index { |_, i| i.even? }
test = examples.select.with_index { |_, i| i.odd? }
puts "Richtig: #{(accuracy(training, test, 3) * 100).round(1)} %"
