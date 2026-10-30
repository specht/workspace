from knn import *

people = customers("customers.csv")
past = people[0::2]
latest = people[1::2]
columns = ["years", "late", "x", "y"]
training = examples(past, columns)
test = examples(latest, columns)
print(f"Gleich entschieden wie der Laden: {round(accuracy(training, test, 5) * 100, 1)} %")
