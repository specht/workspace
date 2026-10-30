# Wie weit zwei Punkte auseinanderliegen, mit beliebig vielen Merkmalen
def distance(a, b)
  sum = 0.0
  a.each_index do |i|
    sum += (a[i] - b[i])**2
  end
  Math.sqrt(sum)
end

# Die k Beispiele, deren Merkmale features am nächsten liegen, das nächste zuerst
def nearest(examples, features, k)
  examples.sort_by { |other, _label| distance(other, features) }.first(k)
end

# Was die meisten der k nächsten Nachbarn sind; bei Gleichstand das, was zuerst kommt
def classify(examples, features, k)
  labels = nearest(examples, features, k).map { |_features, label| label }
  labels.max_by { |label| labels.count(label) }
end

# Die Pinguine als [[Länge, Höhe], Art]
def penguins(path)
  File.readlines(path).drop(1).map do |line|
    species, _island, length, depth = line.split(",")
    [[length.to_f, depth.to_f], species]
  end
end

# Der Anteil der Testbeispiele, die classify richtig einordnet
def accuracy(training, test, k)
  right = test.count { |features, label| classify(training, features, k) == label }
  right.to_f / test.size
end
