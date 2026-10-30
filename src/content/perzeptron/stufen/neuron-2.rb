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
