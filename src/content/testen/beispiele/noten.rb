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

  average = (17.0 / 3 - total / 180.0).round(1)
  [average, 1.0].max
end

