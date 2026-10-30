require_relative "knn"

examples = penguins("penguins.csv")
puts classify(examples, [46.0, 18.0], 5)
