# Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
# eine Rundfahrt durch Berlin.
import itertools
import math


# Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
def distinct_list(names):
    seen = []
    for name in names:
        if name not in seen:
            seen.append(name)
    return len(seen)


# Sortiert stehen gleiche Namen nebeneinander.
def distinct_sorted(names):
    ordered = sorted(names)
    count = 0
    for i, name in enumerate(ordered):
        if i > 0 and name != ordered[i - 1]:
            count += 1
    return count


# Eine Menge findet ohne Suchen, ob sie einen Namen schon hat.
def distinct_set(names):
    seen = set()
    for name in names:
        seen.add(name)
    return len(seen)


# Fibonacci mit einem Gedächtnis: Jede Zahl wird nur einmal ausgerechnet.
def fib_memo(n, memo=None):
    if memo is None:
        memo = {}
    if n < 2:
        return n
    if n not in memo:
        memo[n] = fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
    return memo[n]


# Gibt es zwei Zahlen an verschiedenen Stellen, die zusammen target ergeben?
def has_pair_with_sum(numbers, target):
    seen = set()
    for x in numbers:
        if target - x in seen:
            return True
        seen.add(x)
    return False


# Wie weit es von a nach b ist, in Kilometern Luftlinie; ein Ort ist
# [name, länge, breite] (gegeben).
def distance(a, b):
    dx = (b[1] - a[1]) * 111.32 * math.cos((a[2] + b[2]) / 2 * math.pi / 180)
    dy = (b[2] - a[2]) * 110.57
    return math.hypot(dx, dy)


# Die Länge einer Rundfahrt: von Ort zu Ort in der Reihenfolge der Nummern
# und am Ende zurück zum ersten.
def tour_length(tour, places):
    total = 0.0
    for i, here in enumerate(tour):
        there = tour[(i + 1) % len(tour)]
        total += distance(places[here], places[there])
    return total


# Alle Reihenfolgen ausprobieren; der erste Ort bleibt vorn.
def best_tour(places):
    tours = ([0] + list(order) for order in itertools.permutations(range(1, len(places))))
    return min(tours, key=lambda tour: tour_length(tour, places))


# Immer zum nächsten Ort, an dem man noch nicht war.
def nearest_tour(places):
    tour = [0]
    left = list(range(1, len(places)))
    while left:
        here = places[tour[-1]]
        nearest = min(left, key=lambda i: distance(here, places[i]))
        tour.append(nearest)
        left.remove(nearest)
    return tour
