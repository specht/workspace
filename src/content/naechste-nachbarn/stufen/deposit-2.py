from knn import *

people = customers("customers.csv")
past = people[0::2]
latest = people[1::2]
columns = ["years", "late", "x", "y"]
training = examples(past, columns)
test = examples(latest, columns)
print(f"Gleich entschieden wie der Laden: {round(accuracy(training, test, 5) * 100, 1)} %")
for district in ["Altstadt", "Lindenhof", "Bahnhofsviertel", "Sandberg"]:
    punctual = [person for person in latest if person["district"] == district and person["on_time"] == 1]
    share = deposit_share(training, examples(punctual, columns), 5)
    print(f"{district}: {round(share * 100, 1)} % der Pünktlichen sollen Kaution zahlen")
