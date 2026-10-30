from model import *

counts = follow_counts(words(open("maerchen.txt").read()))
for _ in range(3):
    print(text(generate(counts, "Es", 30)))
    print()
