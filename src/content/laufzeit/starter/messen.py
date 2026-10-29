# Misst, wie lange deine drei Funktionen für immer mehr Namen brauchen:
# die ersten 1000 Straßennamen, 2000, 4000 … aus strassen.txt. Eine Art
# fällt heraus, wenn sie länger als 10 Sekunden braucht. Eine Zahl dahinter
# lässt erst bei so vielen Namen anfangen.
#   python3 messen.py
#   python3 messen.py 8000
import sys
import time

from laufzeit import distinct_list, distinct_sorted, distinct_set

with open("strassen.txt", encoding="utf-8") as f:
    names = f.read().splitlines()
ways = [distinct_list, distinct_sorted, distinct_set]
LIMIT = 10.0

print(f"{'n':>8}  {'Art':<16} {'Sekunden':>10}  {'Faktor':>8}")
before = {}
n = int(sys.argv[1]) if len(sys.argv) > 1 else 1000
while n <= len(names) and ways:
    part = names[:n]
    for way in list(ways):
        start = time.perf_counter()
        way(part)
        seconds = time.perf_counter() - start
        old = before.get(way.__name__)
        factor = f"{seconds / old:.1f}" if old and old > 0.0005 else ""
        print(f"{n:>8}  {way.__name__:<16} {seconds:>10.4f}  {factor:>8}")
        before[way.__name__] = seconds
        if seconds > LIMIT:
            ways.remove(way)
    n *= 2
