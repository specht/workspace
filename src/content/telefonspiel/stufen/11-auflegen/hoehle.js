const { hangup, keys, music, say, sleep } = require('telefon');

function treasure() {
  music('https://www.youtube.com/watch?v=oR450XGc-j4', { start: '0:16', stop: '0:32' });
  say('Im Licht einer Fackel glänzt eine Kiste voller Gold. Du hast den Schatz gefunden!', { mood: 'surprised' });
  sleep(8);
}

function tunnel() {
  music(null);
  say('Drinnen teilt sich der Gang. Links flackert Licht, rechts rauscht Wasser. Drück 1 für links, 2 für rechts, 3, um ins Wasser zu springen.', { mood: 'whisper' });
  const key = keys(1);
  if (key === '1') {
    treasure();
  } else if (key === '2') {
    say('Das Wasser ist eiskalt. Du gehst zurück.');
    tunnel();
  } else if (key === '3') {
    say('Du springst ins Wasser. Die Strömung reißt dich fort, hinaus in die Nacht.');
    hangup();
  } else {
    tunnel();
  }
}

function cave() {
  say('Du stehst vor einer dunklen Höhle. Drück 1, um hineinzugehen, oder 2, um nach Hause zu gehen.', { interrupt: true });
  const key = keys(1);
  if (key === '1') {
    tunnel();
  } else if (key === '2') {
    say('Du gehst nach Hause. Vielleicht ein anderes Mal.', { mood: 'sleepy' });
  } else {
    cave();
  }
}

music('https://www.youtube.com/watch?v=-xRD8VmjBQM');
sleep(3);
cave();
say('Ende.');
