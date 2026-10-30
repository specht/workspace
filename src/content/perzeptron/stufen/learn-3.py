from neuron import *

examples = penguins("penguins.csv", ["Adelie", "Gentoo"])
training = examples[0::2]
test = examples[1::2]
weights, bias = train(training, 100)
print(f"Richtig bei Pinguinen, die es nie gesehen hat: {round(accuracy(weights, bias, test) * 100, 1)} %")
plot(examples, weights, bias, "plot.svg")
