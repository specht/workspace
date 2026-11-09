from telefon import keys, say

say('Katzen, Hunde oder Fische? Drück 1 für Katzen, 2 für Hunde, 3 für Fische.')
key = keys(1)
if key == '1':
    say('Miau! Katzen schlafen bis zu sechzehn Stunden am Tag.')
elif key == '2':
    say('Wuff! Hunde hören Töne, die für dich zu hoch sind.')
elif key == '3':
    say('Blubb! Fische haben keine Augenlider.')
else:
    say('Du hast nichts gedrückt. Schade!')
