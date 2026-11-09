from telefon import keys, say

say('Katzen oder Hunde? Drück 1 für Katzen, 2 für Hunde.')
key = keys(1)
if key == '1':
    say('Miau! Katzen schlafen bis zu sechzehn Stunden am Tag.')
elif key == '2':
    say('Wuff! Hunde hören Töne, die für dich zu hoch sind.')
else:
    say('Du hast nichts gedrückt. Schade!')
