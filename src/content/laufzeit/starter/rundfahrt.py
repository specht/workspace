# Eine Rundfahrt zu den ersten n Sehenswürdigkeiten aus berlin-places.tsv,
# mit deinen Funktionen aus laufzeit.py: alle Reihenfolgen ausprobiert und
# immer zum nächsten Ort, mit der Zeit, die jede Art braucht.
#   python3 rundfahrt.py 8
import sys
import time

import laufzeit

n = int(sys.argv[1]) if len(sys.argv) > 1 else 8
sights = []
with open("berlin-places.tsv", encoding="utf-8") as f:
    for line in f.read().splitlines():
        kind, name, at = line.split("\t")
        if kind == "sight":
            lon, lat = (float(v) for v in at.split(","))
            sights.append([name, lon, lat])
places = sights[:n]

for name, label in [("best_tour", "alle Reihenfolgen"), ("nearest_tour", "nächster Ort")]:
    way = getattr(laufzeit, name, None)
    if way is None:
        print(f"{label:<18} {name} fehlt noch in laufzeit.py")
        continue
    start = time.perf_counter()
    tour = way(places)
    seconds = time.perf_counter() - start
    print(f"{label:<18} {laufzeit.tour_length(tour, places):7.1f} km  {seconds:9.3f} s")
    print("  " + " → ".join(places[i][0] for i in tour + [tour[0]]))
