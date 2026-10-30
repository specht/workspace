require_relative "knn"

people = customers("customers.csv")
past = people.select.with_index { |_, i| i.even? }
latest = people.select.with_index { |_, i| i.odd? }
columns = ["years", "late", "x", "y"]
training = examples(past, columns)
test = examples(latest, columns)
puts "Gleich entschieden wie der Laden: #{(accuracy(training, test, 5) * 100).round(1)} %"
