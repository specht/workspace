# Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
# eine Rundfahrt durch Berlin.


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
        if i == 0 or name != ordered[i - 1]:
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
