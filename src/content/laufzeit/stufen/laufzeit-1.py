# Wie lange dauert das? Dieselbe Frage auf drei Arten, schnelle Hilfen und
# eine Rundfahrt durch Berlin.


# Wie viele verschiedene Namen? Mit einer Liste dessen, was man schon kennt.
def distinct_list(names):
    seen = []
    for name in names:
        if name not in seen:
            seen.append(name)
    return len(seen)
