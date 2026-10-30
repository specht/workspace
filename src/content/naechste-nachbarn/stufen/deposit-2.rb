require_relative "knn"

people = customers("customers.csv")
past = people.select.with_index { |_, i| i.even? }
latest = people.select.with_index { |_, i| i.odd? }
columns = ["years", "late", "x", "y"]
training = examples(past, columns)
test = examples(latest, columns)
puts "Gleich entschieden wie der Laden: #{(accuracy(training, test, 5) * 100).round(1)} %"
["Altstadt", "Lindenhof", "Bahnhofsviertel", "Sandberg"].each do |district|
  punctual = latest.select { |person| person["district"] == district && person["on_time"] == 1 }
  share = deposit_share(training, examples(punctual, columns), 5)
  puts "#{district}: #{(share * 100).round(1)} % der Pünktlichen sollen Kaution zahlen"
end
