const { keys, music, say, sleep } = require('telefon');

music('https://www.youtube.com/watch?v=-xRD8VmjBQM');
sleep(3);
say('Du stehst vor einer dunklen Höhle. Drück 1, um hineinzugehen, oder 2, um nach Hause zu gehen.');
const key = keys(1);
if (key === '1') {
  say('Im Licht einer Fackel glänzt eine Kiste voller Gold. Du hast den Schatz gefunden!');
} else if (key === '2') {
  say('Du gehst nach Hause. Vielleicht ein anderes Mal.');
}
say('Ende.');
