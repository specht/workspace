const { keys, say } = require('telefon');

say('Katzen oder Hunde? Drück 1 für Katzen, 2 für Hunde.');
const key = keys(1);
if (key === '1') {
  say('Miau! Katzen schlafen bis zu sechzehn Stunden am Tag.');
} else if (key === '2') {
  say('Wuff! Hunde hören Töne, die für dich zu hoch sind.');
} else {
  say('Du hast nichts gedrückt. Schade!');
}
