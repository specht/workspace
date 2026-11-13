"""The menu of the Telefonbuch's line (docs/plan-telefonspiel.md, »The menu«):
a phone game itself, run by line.py in the sandbox. It reads the published
games from the list the Workspace writes (PHONE_GAMES/published.json), reads
their titles five at a time in random order, and takes the number of a game
(three or four digits) at any moment. A number dialled before the call
(PHONE_DIALED) skips all that: its game, or the three tones and »Kein
Anschluss unter dieser Nummer« before the menu. After two rounds of titles
without a key it says goodbye. Then the menu becomes the game: the game's
interpreter replaces this process, so the game talks over the same pipes and
stays in the same sandbox.
"""
import json
import os
import random

from telefon import _command, hangup, keys, play, say, title

GAMES = os.environ.get('PHONE_GAMES', '/games')
LANGUAGES = {'.py': ['python3'], '.rb': ['ruby'], '.js': ['node']}
BATCH = 5
# rounds of titles without a key before the menu says goodbye (the maintainer:
# a forgotten call should not hold one of the line's few channels)
QUIET_ROUNDS = 2
DIALED = os.environ.get('PHONE_DIALED', '')
# a game tried from /telefon-freigabe before it is published (line.py checks the ticket)
TRIAL = json.loads(os.environ.get('PHONE_TRIAL') or 'null')   # the number dialled before the call (line.py), or ''


def load():
    try:
        with open(os.path.join(GAMES, 'published.json')) as f:
            games = json.load(f).get('games', [])
    except (OSError, ValueError):
        return []
    return [g for g in games if isinstance(g, dict) and g.get('number') and g.get('folder') and g.get('file')]


def folder_of(game):
    """games/<id>: the published copy"""
    return os.path.join(GAMES, os.path.basename(game['folder']))


def spoken(number):
    """123 as »eins zwei drei«, not as a hundred and twenty-three"""
    return ' '.join(number)


def start(game):
    _command({'game': game['folder']})          # the line serves its music files from now on
    os.chdir(folder_of(game))
    command = LANGUAGES[os.path.splitext(game['file'])[1]]
    os.execvp(command[0], command + [game['file']])


def typed():
    """a key pressed during the last sentence, or ''"""
    return keys(1, timeout=0.05)


def dial(first, games):
    """the rest of a number after its first digit; True when a game starts.
    After three digits it waits for a fourth only while a game's number
    begins with them and is longer; # ends the number."""
    number = first
    while len(number) < 4:
        if len(number) >= 3 and not any(len(g['number']) > len(number) and g['number'].startswith(number) for g in games):
            break
        key = keys(1, timeout=8 if len(number) < 3 else 3)
        if key in ('', '#'):
            break
        number += key
    if len(number) < 3:
        say('Eine Nummer hat drei oder vier Ziffern.')
        return False
    for game in games:
        if game['number'] == number:
            start(game)
    say(f'Die Nummer {spoken(number)} gibt es im Telefonbuch nicht.')
    return False


def connect(number, games):
    """a number dialled before the call: its game; else what a real line
    says (the three tones, »Kein Anschluss unter dieser Nummer«), and then
    the menu, so that a typo does not cost a new call"""
    for game in games:
        if game['number'] == number:
            start(game)
    title('Kein Anschluss')
    play('kein-anschluss.wav')
    say('Kein Anschluss unter dieser Nummer.')
    title('')


def main():
    if TRIAL:
        start(TRIAL)
    games = load()
    if DIALED:
        connect(DIALED, games)
    # read out: not the hidden ones (parts of other games); dialled: all
    listed = [g for g in games if not g.get('hidden')]
    say('Hier ist das Telefonbuch des Hackschule Workspace.', interrupt=True)
    if not listed:
        say('Noch steht kein Spiel im Telefonbuch.')
        return
    say('Gib die Nummer eines Spiels ein.', interrupt=True)
    quiet = 0
    while True:
        first = typed()
        if first and dial(first, games):
            return
        if quiet >= QUIET_ROUNDS:
            say('Ruf gern wieder an. Tschüss!')
            hangup()
            return
        pressed = False
        order = random.sample(listed, len(listed))
        for i in range(0, len(order), BATCH):
            for game in order[i:i + BATCH]:
                say(f'{game["title"]}: {spoken(game["number"])}.', interrupt=True)
                first = typed()
                if first:
                    break
            if not first:
                say('Für weitere Spiele drück die Raute.', interrupt=True)
                first = keys(3, timeout=10)
                if first:
                    pressed = True
                if len(first) == 3:
                    dial(first, games)
                    first = ''
                    continue
            if first:
                pressed = True
                dial(first, games)
        quiet = 0 if pressed else quiet + 1


main()
