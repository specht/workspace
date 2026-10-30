from knn import *

examples = penguins("penguins.csv")
training = examples[0::2]
test = examples[1::2]
print(f"Richtig: {round(accuracy(training, test, 3) * 100, 1)} %")
