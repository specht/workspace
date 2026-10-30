from model import *

pairs = pair_counts(words(open("maerchen.txt").read()))
for _ in range(3):
    print(text(generate_pairs(pairs, "Es", "war", 30)))
    print()
