from knn import *

examples = penguins("penguins.csv")
print(classify(examples, [46.0, 18.0], 5))
