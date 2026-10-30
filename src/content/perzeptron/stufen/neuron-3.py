# 1 oder 0: ist die gewichtete Summe der Merkmale plus Bias größer als 0?
def predict(weights, bias, features):
    total = bias
    for i in range(len(features)):
        total += weights[i] * features[i]
    return 1 if total > 0 else 0


# Die Pinguine zweier Arten als [[Länge, Höhe], 0 oder 1]
def penguins(path, species):
    examples = []
    for line in open(path).readlines()[1:]:
        name, _island, length, depth = line.split(",")[:4]
        if name in species:
            examples.append([[float(length), float(depth)], 1 if name == species[1] else 0])
    return examples


# Lernt Gewichte und Bias aus Beispielen [Merkmale, 0 oder 1], höchstens epochs Epochen lang
def train(examples, epochs):
    weights = [0.0] * len(examples[0][0])
    bias = 0.0
    for epoch in range(epochs):
        errors = 0
        for features, target in examples:
            error = target - predict(weights, bias, features)
            if error == 0:
                continue
            for i in range(len(features)):
                weights[i] += error * features[i]
            bias += error
            errors += 1
        print(f"Epoche {epoch + 1}: {errors} Fehler")
        if errors == 0:
            break
    return weights, bias
