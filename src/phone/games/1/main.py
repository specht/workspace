import random

from telefon import keys, music, say

music('/app/sounds/demo-music.ogg', 0.6)
say('Ich denke mir eine Zahl zwischen 1 und 9. Rate sie!')
secret = random.randint(1, 9)
for attempt in range(1, 4):
    guess = keys(1, timeout=15)
    print('guess', repr(guess))
    if guess == '':
        say('Du hast nichts gedrückt.')
    elif int(guess) == secret:
        say(f'Richtig! Es war die {secret}. Du hast {attempt} Versuche gebraucht.')
        break
    elif int(guess) < secret:
        say('Meine Zahl ist größer.')
    else:
        say('Meine Zahl ist kleiner.')
else:
    say(f'Leider nicht. Meine Zahl war die {secret}.')
say('Tschüss!')
