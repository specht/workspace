from neuron import *

examples = penguins("penguins.csv", ["Adelie", "Gentoo"])
weights, bias = train(examples, 100)
print(f"Gewichte: {[round(w, 1) for w in weights]}, Bias: {round(bias, 1)}")
plot(examples, weights, bias, "plot.svg")
