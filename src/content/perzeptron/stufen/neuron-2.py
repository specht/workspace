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
