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


# Die n häufigsten Nachfolger von word, mit ihrer Anzahl
def top(counts, word, n):
    followers = counts.get(word, {})
    return sorted(followers.items(), key=lambda pair: -pair[1])[:n]


# Der häufigste Nachfolger von word, None für ein unbekanntes Wort
def most_likely(counts, word):
    if word not in counts:
        return None
    return max(counts[word], key=counts[word].get)


# Wörter als Text, ohne Leerzeichen vor einem Satzzeichen
def text(tokens):
    return re.sub(r" ([.,;:!?])", r"\1", " ".join(tokens))


# Ein Nachfolger von word, gezogen nach seiner Anzahl; None für ein unbekanntes Wort
def next_word(counts, word):
    if word not in counts:
        return None
    r = random.randrange(sum(counts[word].values()))
    for following, count in counts[word].items():
        if r < count:
            return following
        r -= count
