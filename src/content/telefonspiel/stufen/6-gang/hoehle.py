from telefon import keys, music, say, sleep


def treasure():
    say('Im Licht einer Fackel glänzt eine Kiste voller Gold. Du hast den Schatz gefunden!')


def tunnel():
    say('Drinnen teilt sich der Gang. Links flackert Licht, rechts rauscht Wasser. Drück 1 für links, 2 für rechts.')
    key = keys(1)
    if key == '1':
        treasure()
    elif key == '2':
        say('Das Wasser ist eiskalt. Du gehst zurück.')
        tunnel()
    else:
        tunnel()


def cave():
    say('Du stehst vor einer dunklen Höhle. Drück 1, um hineinzugehen, oder 2, um nach Hause zu gehen.')
    key = keys(1)
    if key == '1':
        tunnel()
    elif key == '2':
        say('Du gehst nach Hause. Vielleicht ein anderes Mal.')
    else:
        cave()


music('https://www.youtube.com/watch?v=-xRD8VmjBQM')
sleep(3)
cave()
say('Ende.')
