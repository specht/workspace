# 1 oder 0: ist die gewichtete Summe der Merkmale plus Bias größer als 0?
def predict(weights, bias, features)
  sum = bias
  features.each_index do |i|
    sum += weights[i] * features[i]
  end
  sum > 0 ? 1 : 0
end

# Die Pinguine zweier Arten als [[Länge, Höhe], 0 oder 1]
def penguins(path, species)
  File.readlines(path).drop(1).filter_map do |line|
    name, _island, length, depth = line.split(",")
    next unless species.include?(name)

    [[length.to_f, depth.to_f], name == species[1] ? 1 : 0]
  end
end

# Lernt Gewichte und Bias aus Beispielen [Merkmale, 0 oder 1], höchstens epochs Epochen lang
def train(examples, epochs)
  weights = Array.new(examples[0][0].size, 0.0)
  bias = 0.0
  epochs.times do |epoch|
    errors = 0
    examples.each do |features, target|
      error = target - predict(weights, bias, features)
      next if error == 0

      features.each_index do |i|
        weights[i] += error * features[i]
      end
      bias += error
      errors += 1
    end
    puts "Epoche #{epoch + 1}: #{errors} Fehler"
    break if errors == 0
  end
  [weights, bias]
end
