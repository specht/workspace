from neuron import *

training = digits("digits-train.csv", 0, 1)
test = digits("digits-test.csv", 0, 1)
weights, bias = train(training, 100)
print(f"Richtig bei fremden Handschriften: {round(accuracy(weights, bias, test) * 100, 1)} %")
