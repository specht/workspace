const { keys, say } = require('telefon');

say('Katzen, Hunde oder Fische? Drück 1 für Katzen, 2 für Hunde, 3 für Fische.');
const key = keys(1);
if (key === '1') {
  say('Miau! Katzen schlafen bis zu sechzehn Stunden am Tag.');
} else if (key === '2') {
  say('Wuff! Hunde hören Töne, die für dich zu hoch sind.');
} else if (key === '3') {
  say('Blubb! Fische haben keine Augenlider.');
} else {
  say('Du hast nichts gedrückt. Schade!');
}
