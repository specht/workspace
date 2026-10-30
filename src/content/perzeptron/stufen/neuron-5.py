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


# Die Beispiele als Punkte (Länge nach rechts, Höhe nach oben) und die Gerade,
# auf der die Summe des Neurons 0 ist, als SVG in path
def plot(examples, weights, bias, path):
    x0, x1, y0, y1 = 30.0, 62.0, 12.0, 23.0
    def sx(x):
        return (x - x0) / (x1 - x0) * 600 + 40
    def sy(y):
        return 440 - (y - y0) / (y1 - y0) * 400
    svg = ["<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 680 480'>",
           "<rect width='680' height='480' fill='white'/>"]
    for (length, depth), target in examples:
        colour = "#2b5fb4" if target == 1 else "#d97706"
        svg.append(f"<circle cx='{sx(length)}' cy='{sy(depth)}' r='4' fill='{colour}'/>")
    if weights[1] != 0:
        ya = -(weights[0] * x0 + bias) / weights[1]
        yb = -(weights[0] * x1 + bias) / weights[1]
        svg.append(f"<line x1='{sx(x0)}' y1='{sy(ya)}' x2='{sx(x1)}' y2='{sy(yb)}' stroke='black' stroke-width='2'/>")
    svg.append("</svg>")
    with open(path, "w") as f:
        f.write("\n".join(svg))


# Der Anteil der Beispiele, die das Neuron richtig einordnet
def accuracy(weights, bias, examples):
    right = sum(1 for features, target in examples if predict(weights, bias, features) == target)
    return right / len(examples)
