# Fehler: accuracy zählt die falsch eingeordneten Beispiele.
# Die Funktionen von »Nächste Nachbarn«, so wie sie am Ende der Seite in
# knn.py stehen: selbst geschrieben distance, nearest, classify, accuracy
# und deposit_share, von der Seite penguins, customers und examples.
import math


# Wie weit zwei Punkte auseinanderliegen, mit beliebig vielen Merkmalen
def distance(a, b):
    total = 0.0
    for i in range(len(a)):
        total += (a[i] - b[i]) ** 2
    return math.sqrt(total)


# Die k Beispiele, deren Merkmale features am nächsten liegen, das nächste zuerst
def nearest(examples, features, k):
    return sorted(examples, key=lambda example: distance(example[0], features))[:k]


# Was die meisten der k nächsten Nachbarn sind; bei Gleichstand das, was zuerst kommt
def classify(examples, features, k):
    labels = [label for _features, label in nearest(examples, features, k)]
    return max(labels, key=labels.count)


# Der Anteil der Testbeispiele, die classify richtig einordnet
def accuracy(training, test, k):
    right = sum(1 for features, label in test if classify(training, features, k) != label)
    return right / len(test)


# Der Anteil der Testbeispiele, für die classify eine Kaution verlangt (1)
def deposit_share(training, test, k):
    deposits = sum(1 for features, _label in test if classify(training, features, k) == 1)
    return deposits / len(test)


# Die Pinguine als [[Länge, Höhe], Art]
def penguins(path):
    examples = []
    for line in open(path).readlines()[1:]:
        species, _island, length, depth = line.split(",")[:4]
        examples.append([[float(length), float(depth)], species])
    return examples


# Jede Kundin und jeder Kunde als Dictionary: Spalte -> Wert, Zahlen als Zahlen
def customers(path):
    lines = open(path).read().splitlines()
    keys = lines[0].split(",")
    people = []
    for line in lines[1:]:
        person = {}
        for key, value in zip(keys, line.split(",")):
            person[key] = value if key == "district" else float(value)
        people.append(person)
    return people


# Beispiele aus Kunden: die Spalten in columns als Merkmale, die Kaution als Antwort
def examples(people, columns):
    return [[[person[column] for column in columns], int(person["deposit"])] for person in people]
