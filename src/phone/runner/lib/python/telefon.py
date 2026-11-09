"""The Telefonspiel in Python: say, keys, music, sleep, hangup.

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
    return json.loads(line)


def say(text):
    """speak the text; returns when it has been said"""
    _command({'say': str(text)})


def keys(count=1, timeout=10):
    """wait for up to `count` keys (# ends early); '' after `timeout` seconds"""
    return _command({'keys': int(count), 'timeout': int(timeout * 1000)}).get('keys', '')


def music(source, volume=0.5):
    """background music (a YouTube link, a URL or a file), looped, quieter
    while speaking; music(None) stops it"""
    _command({'music': source, 'volume': volume})


def sleep(seconds):
    _command({'sleep': int(seconds * 1000)})


def hangup():
    _command({'hangup': True})
