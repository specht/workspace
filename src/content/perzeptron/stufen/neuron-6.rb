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

# Die Beispiele als Punkte (Länge nach rechts, Höhe nach oben) und die Gerade,
# auf der die Summe des Neurons 0 ist, als SVG in path
def plot(examples, weights, bias, path)
  x0, x1, y0, y1 = 30.0, 62.0, 12.0, 23.0
  sx = ->(x) { (x - x0) / (x1 - x0) * 600 + 40 }
  sy = ->(y) { 440 - (y - y0) / (y1 - y0) * 400 }
  svg = ["<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 680 480'>",
         "<rect width='680' height='480' fill='white'/>"]
  examples.each do |(length, depth), target|
    colour = target == 1 ? "#2b5fb4" : "#d97706"
    svg << "<circle cx='#{sx.(length)}' cy='#{sy.(depth)}' r='4' fill='#{colour}'/>"
  end
  if weights[1] != 0
    ya = -(weights[0] * x0 + bias) / weights[1]
    yb = -(weights[0] * x1 + bias) / weights[1]
    svg << "<line x1='#{sx.(x0)}' y1='#{sy.(ya)}' x2='#{sx.(x1)}' y2='#{sy.(yb)}' stroke='black' stroke-width='2'/>"
  end
  svg << "</svg>"
  File.write(path, svg.join("\n"))
end

# Der Anteil der Beispiele, die das Neuron richtig einordnet
def accuracy(weights, bias, examples)
  right = examples.count { |features, target| predict(weights, bias, features) == target }
  right.to_f / examples.size
end

# Die Ziffern a und b als [64 Kästchen, 0 für a oder 1 für b]
def digits(path, a, b)
  File.readlines(path).filter_map do |line|
    values = line.split(",").map(&:to_i)
    next unless [a, b].include?(values[64])

    [values[0, 64], values[64] == b ? 1 : 0]
  end
end
