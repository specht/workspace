// Der Notenrechner der Oberstufe: Punkte, Noten und der Schnitt im Abitur

// Die Noten zu 0 bis 15 Punkten; der Index ist die Punktzahl
const GRADES = ['6', '5-', '5', '5+', '4-', '4', '4+', '3-', '3', '3+', '2-', '2', '2+', '1-', '1', '1+'];

// Die Note zu einer Punktzahl: 15 Punkte sind »1+«, 0 Punkte »6«
function grade(points) {
  if (!Number.isInteger(points) || points < 0 || points > 15) {
    throw new RangeError(`${points} Punkte gibt es nicht`);
  }
  return GRADES[points];
}

// Ein Defizit: ein Kurs mit weniger als 5 Punkten
function isDeficit(points) {
  return points < 5;
}

// Die Durchschnittsnote im Abitur aus der Gesamtqualifikation, 300 bis 900
// Punkte: 17/3 - total/180, nach der ersten Nachkommastelle abgeschnitten,
// nicht gerundet; besser als 1,0 gibt es nicht
function abiturGrade(total) {
  if (total < 300 || total > 900) {
    throw new RangeError(`${total} Punkte: Die Gesamtqualifikation liegt zwischen 300 und 900`);
  }
  const average = Math.round((17 / 3 - total / 180) * 10) / 10;
  return Math.max(average, 1.0);
}

module.exports = { grade, isDeficit, abiturGrade };
