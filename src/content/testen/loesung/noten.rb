# Der Notenrechner der Oberstufe: Punkte, Noten und der Schnitt im Abitur

# Die Noten zu 0 bis 15 Punkten; der Index ist die Punktzahl
GRADES = ['6', '5-', '5', '5+', '4-', '4', '4+', '3-', '3', '3+', '2-', '2', '2+', '1-', '1', '1+'].freeze

# Die Note zu einer Punktzahl: 15 Punkte sind »1+«, 0 Punkte »6«
def grade(points)
  unless points.is_a?(Integer) && points.between?(0, 15)
    raise ArgumentError, "#{points} Punkte gibt es nicht"
  end

  GRADES[points]
end

# Ein Defizit: ein Kurs mit weniger als 5 Punkten
def deficit?(points)
  points < 5
end

# Die Durchschnittsnote im Abitur aus der Gesamtqualifikation, 300 bis 900
# Punkte: 17/3 - total/180, nach der ersten Nachkommastelle abgeschnitten,
# nicht gerundet; besser als 1,0 gibt es nicht
def abitur_grade(total)
  unless total.between?(300, 900)
    raise ArgumentError, "#{total} Punkte: Die Gesamtqualifikation liegt zwischen 300 und 900"
  end

  # in Zehnteln: (1020 - total) / 18, ganzzahlig geteilt, also abgeschnitten
  tenths = (1020 - total) / 18
  [tenths, 10].max / 10.0
end

# Zur Abiturprüfung zugelassen, nach der Vereinbarung der KMK (Ziffer 9.5.7),
# vereinfacht: jedes Ergebnis der Qualifikationsphase zählt einfach. Block I
# hat mindestens 200 Punkte, höchstens 20 Prozent der Ergebnisse liegen unter
# 5 Punkten, und keins hat 0 Punkte.
def admitted?(results)
  # Block I: der Durchschnitt mal 40, gerundet
  block_one = (results.sum * 40.0 / results.size).round
  deficits = results.count { |points| deficit?(points) }
  block_one >= 200 && deficits * 5 <= results.size && !results.include?(0)
end
