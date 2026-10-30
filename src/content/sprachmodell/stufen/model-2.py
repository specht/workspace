import random
import re


# Der Text als Liste von Wörtern und Satzzeichen
def words(text):
    return re.findall(r"[^\W\d_]+|[.,;:!?]", text)


# Für jedes Wort: welche Wörter darauf folgen, und wie oft
def follow_counts(tokens):
    counts = {}
    for i in range(len(tokens) - 1):
        word = tokens[i]
        following = tokens[i + 1]
        if word not in counts:
            counts[word] = {}
        counts[word][following] = counts[word].get(following, 0) + 1
    return counts
