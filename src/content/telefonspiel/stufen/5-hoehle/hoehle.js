const { keys, music, say, sleep } = require('telefon');

function treasure() {
  say('Im Licht einer Fackel glänzt eine Kiste voller Gold. Du hast den Schatz gefunden!');
}

function cave() {
  say('Du stehst vor einer dunklen Höhle. Drück 1, um hineinzugehen, oder 2, um nach Hause zu gehen.');
  const key = keys(1);
  if (key === '1') {
    treasure();
  } else if (key === '2') {
    say('Du gehst nach Hause. Vielleicht ein anderes Mal.');
  } else {
    cave();
  }
}

music('https://www.youtube.com/watch?v=-xRD8VmjBQM');
sleep(3);
cave();
say('Ende.');
