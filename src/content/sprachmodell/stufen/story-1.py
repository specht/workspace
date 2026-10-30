from model import *

counts = follow_counts(words(open("maerchen.txt").read()))
tokens = ["Es"]
for _ in range(30):
    tokens.append(most_likely(counts, tokens[-1]))
print(text(tokens))
