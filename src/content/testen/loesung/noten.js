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
  // in Zehnteln: (1020 - total) / 18, nach unten abgeschnitten
  const tenths = Math.floor((1020 - total) / 18);
  return Math.max(tenths, 10) / 10;
}

// Zur Abiturprüfung zugelassen, nach der Vereinbarung der KMK (Ziffer 9.5.7),
// vereinfacht: jedes Ergebnis der Qualifikationsphase zählt einfach. Block I
// hat mindestens 200 Punkte, höchstens 20 Prozent der Ergebnisse liegen unter
// 5 Punkten, und keins hat 0 Punkte.
function isAdmitted(results) {
  // Block I: der Durchschnitt mal 40, gerundet
  const sum = results.reduce((total, points) => total + points, 0);
  const blockOne = Math.round(sum * 40 / results.length);
  const deficits = results.filter(isDeficit).length;
  return blockOne >= 200 && deficits * 5 <= results.length && !results.includes(0);
}

module.exports = { grade, isDeficit, abiturGrade, isAdmitted };
