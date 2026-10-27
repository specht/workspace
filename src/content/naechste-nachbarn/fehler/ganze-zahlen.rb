# Fehler: accuracy teilt ganze Zahlen; in Ruby ergibt 2 / 4 dann 0.
# Die Funktionen von »Nächste Nachbarn«, so wie sie am Ende der Seite in
# knn.rb stehen: selbst geschrieben distance, nearest, classify, accuracy
# und deposit_share, von der Seite penguins, customers und examples.

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

# Der Anteil der Testbeispiele, die classify richtig einordnet
def accuracy(training, test, k)
  right = test.count { |features, label| classify(training, features, k) == label }
  right / test.size
end

# Der Anteil der Testbeispiele, für die classify eine Kaution verlangt (1)
def deposit_share(training, test, k)
  deposits = test.count { |features, _label| classify(training, features, k) == 1 }
  deposits.to_f / test.size
end

# Die Pinguine als [[Länge, Höhe], Art]
def penguins(path)
  File.readlines(path).drop(1).map do |line|
    species, _island, length, depth = line.split(",")
    [[length.to_f, depth.to_f], species]
  end
end

# Jede Kundin und jeder Kunde als Hash: Spalte => Wert, Zahlen als Zahlen
def customers(path)
  lines = File.readlines(path, chomp: true)
  keys = lines[0].split(",")
  lines.drop(1).map do |line|
    keys.zip(line.split(",")).to_h { |key, value| [key, key == "district" ? value : value.to_f] }
  end
end

# Beispiele aus Kunden: die Spalten in columns als Merkmale, die Kaution als Antwort
def examples(people, columns)
  people.map { |person| [columns.map { |column| person[column] }, person["deposit"].to_i] }
end
