"""The Telefonspiel in Python: say, play, keys, music, sleep, hangup, transfer, call, answer.

Every function writes one command as a line of JSON and waits for the
answer, so a game is an ordinary program that runs from top to bottom.
print() still works: lines that are not JSON are ordinary output.
"""

import json
import sys


def _command(data):
    sys.stdout.write(json.dumps(data, ensure_ascii=False) + '\n')
    sys.stdout.flush()
    line = sys.stdin.readline()
    if not line:                      # the caller hung up
        raise SystemExit(0)
    answer = json.loads(line)
    if answer.get('error'):           # a wrong argument (a time that is none)
        raise ValueError(answer['error'])
    return answer


def title(text):
    """the game's name: the phone shows it, the Telefonbuch lists it"""
    _command({'title': str(text)})


def subtitle(text):
    """a line about the game, for the Telefonbuch"""
    _command({'subtitle': str(text)})


def say(text, interrupt=False, mood=None):
    """speak the text; returns when it has been said. interrupt=True lets a
    key stop it; the key is kept for the next keys(). mood: Thorsten in a
    mood: neutral, amused, angry, disgusted, drunk, sleepy, surprised or
    whisper."""
    command = {'say': str(text)}
    if interrupt:
        command['interrupt'] = True
    if mood:
        command['mood'] = str(mood)
    _command(command)


def play(file, interrupt=False):
    """play a recording of the game's folder (an MP3) in place of the voice;
    returns when it has been heard. interrupt=True lets a key stop it."""
    command = {'play': str(file)}
    if interrupt:
        command['interrupt'] = True
    _command(command)


def keys(count=1, timeout=10):
    """wait for up to `count` keys (# ends early); '' after `timeout` seconds"""
    return _command({'keys': int(count), 'timeout': int(timeout * 1000)}).get('keys', '')


def music(source, volume=1.0, start=None, stop=None):
    """background music (a YouTube link or a file in the game's folder),
    looped, quieter while speaking; music(None) stops it. start and stop in
    seconds (75) or as '1:15': the piece from start to stop, looped; a
    YouTube link with t= starts there."""
    _command({'music': source, 'volume': volume, 'start': start, 'stop': stop})


def sleep(seconds):
    """wait; the music keeps playing, keys pressed meanwhile are kept"""
    _command({'sleep': int(seconds * 1000)})


def hangup():
    """end the call"""
    _command({'hangup': True})
    raise SystemExit(0)


def clear_keys():
    """forget the keys pressed so far (also digits dialled after this game's
    number that keys() has not read yet)"""
    _command({'clear_keys': True})


def transfer(number, extension=''):
    """forward the call to the published game with this number (letters as
    on the keys: 'DICE'), extension: digits dialled after it; the call goes
    on there as if the caller had dialled it, and this game ends"""
    _command({'transfer': str(number), 'extension': str(extension)})
    raise SystemExit(0)


def call(number, extension=''):
    """call the game with this number and wait on hold: the caller hears it
    dialled (with the extension after it) and is with that game until it
    answers with answer(); returns its digits, '' when it ended without"""
    return _command({'call': str(number), 'extension': str(extension)}).get('keys', '')


def answer(digits=''):
    """answer the game that called this one: the digits as key tones, then
    #, and this game ends (dialled by a person, the call ends)"""
    _command({'answer': str(digits)})
    raise SystemExit(0)
