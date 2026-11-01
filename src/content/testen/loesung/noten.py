import math

# Der Notenrechner der Oberstufe: Punkte, Noten und der Schnitt im Abitur

# Die Noten zu 0 bis 15 Punkten; der Index ist die Punktzahl
GRADES = ["6", "5-", "5", "5+", "4-", "4", "4+", "3-", "3", "3+", "2-", "2", "2+", "1-", "1", "1+"]


# Die Note zu einer Punktzahl: 15 Punkte sind »1+«, 0 Punkte »6«
def grade(points):
    if not isinstance(points, int) or not 0 <= points <= 15:
        raise ValueError(f"{points} Punkte gibt es nicht")
    return GRADES[points]


# Ein Defizit: ein Kurs mit weniger als 5 Punkten
def is_deficit(points):
    return points < 5


# Die Durchschnittsnote im Abitur aus der Gesamtqualifikation, 300 bis 900
# Punkte: 17/3 - total/180, nach der ersten Nachkommastelle abgeschnitten,
# nicht gerundet; besser als 1,0 gibt es nicht
def abitur_grade(total):
    if not 300 <= total <= 900:
        raise ValueError(f"{total} Punkte: Die Gesamtqualifikation liegt zwischen 300 und 900")
    # in Zehnteln: (1020 - total) // 18, ganzzahlig geteilt, also abgeschnitten
    tenths = (1020 - total) // 18
    return max(tenths, 10) / 10


# Zur Abiturprüfung zugelassen, nach der Vereinbarung der KMK (Ziffer 9.5.7),
# vereinfacht: jedes Ergebnis der Qualifikationsphase zählt einfach. Block I
# hat mindestens 200 Punkte, höchstens 20 Prozent der Ergebnisse liegen unter
# 5 Punkten, und keins hat 0 Punkte.
def is_admitted(results):
    # Block I: der Durchschnitt mal 40, ab ,5 aufgerundet
    block_one = math.floor(sum(results) * 40 / len(results) + 0.5)
    deficits = len([points for points in results if is_deficit(points)])
    return block_one >= 200 and deficits * 5 <= len(results) and 0 not in results
