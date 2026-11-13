"""Calling another game and holding the line (core/game.py: call, back,
the keys a connection keeps): real games in Python through the library,
on a test phone that connects numbers as the lines do, each connection on
a phone of its own (line.py). What it checks: a number reaches its game
with the rest as the first keys (the extension); keys wait during speech
and are lost beyond MAX_KEYS; nothing is kept from one connection to the
next; call waits and returns what back answered, also calls inside calls
(the factorial of 5); a called game that ends without back or crashes
gives ''; one call too deep overloads the line and ends the call; the
tones are heard; clear_keys forgets.
    python3 src/phone/test/test_hold.py"""
import asyncio
import os
import sys
import tempfile
import textwrap
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), 'core'))
import game  # noqa: E402


class TestPhone(game.Phone):
    """the other end: speech and tones go into heard; numbers reach the
    scripts of games (number → file), a call runs on a phone of its own"""

    def __init__(self, games, folder, heard, depth=0, keys=''):
        super().__init__()
        self.games, self.folder, self.heard, self.depth = games, folder, heard, depth
        self.fresh_keys(keys)

    async def say(self, text, interrupt=False, mood=None):
        self.heard.append(('say', text))

    async def dtmf(self, digits):
        self.heard.append(('tones', digits))

    def transfer_problem(self, number):
        if game.match_number(number, self.games)[0] is None:
            return f'transfer: Ein Spiel mit der Nummer {number} gibt es nicht.'
        return None

    async def call_game(self, dialed):
        number, extension = game.match_number(dialed, self.games)
        if number is None:
            self.heard.append(('say', 'Kein Anschluss unter dieser Nummer.'))
            return ''
        callee = TestPhone(self.games, self.folder, self.heard, self.depth + 1, extension)
        reason, _ = await run(callee, self.games[number], self.folder)
        if callee.ended.is_set():
            self.ended.set()
        return callee.returned if reason == 'back' else ''


async def run(phone, file, folder):
    errors = []
    return await game.run_game(phone, game.command_for([file]), folder, game.environment(),
                               lambda kind, line: errors.append(line), seconds=30)


def write(folder, name, code):
    with open(os.path.join(folder, name), 'w') as f:
        f.write('from telefon import *\n' + textwrap.dedent(code))
    return name


class Hold(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.mkdtemp()
        self.games = {
            # FACT: n! by asking itself for (n-1)!
            '3228': write(self.folder, 'fact.py', """
                say('Fakultät.')
                n = int(keys(1) or 0)
                if n <= 1:
                    back('1')
                rest = int(call('FACT', str(n - 1)))
                say(f'{n} mal {rest} ist {n * rest}.')
                back(str(n * rest))
                """),
            # DICE: the number of sides as the extension, here always the highest
            '3423': write(self.folder, 'dice.py', """
                sides = int(keys(3, timeout=0.3) or 0)
                say(f'Würfelbude: {sides}.')
                back(str(sides))
                """),
            '555': write(self.folder, 'quiet.py', "say('Ich sage nichts zurück.')\n"),
            '666': write(self.folder, 'broken.py', "say('Gleich.')\nraise RuntimeError('kaputt')\n"),
            '777': write(self.folder, 'deep.py', "call('777')\n"),
        }

    def call(self, code, keys=''):
        heard = []
        name = write(self.folder, 'caller.py', code)
        phone = TestPhone(self.games, self.folder, heard, keys=keys)
        reason, status = asyncio.run(run(phone, name, self.folder))
        return reason, status, heard, phone

    def test_the_extension_waits_through_the_intro(self):
        reason, _, heard, _ = self.call("say('Gesamt: ' + call('FACT', '1'))\n")
        self.assertEqual('ended', reason)
        self.assertIn(('tones', '32281'), heard)           # dialled: the number and its extension
        self.assertIn(('say', 'Gesamt: 1'), heard)

    def test_a_factorial_calls_itself(self):
        _, _, heard, _ = self.call("say('Ergebnis ' + call('FACT', '5'))\n")
        self.assertIn(('say', '5 mal 24 ist 120.'), heard)
        self.assertIn(('say', 'Ergebnis 120'), heard)
        self.assertIn(('tones', '120#'), heard)            # the answer, heard as tones

    def test_letters_and_one_string_dial_the_same(self):
        _, _, heard, _ = self.call("say(call('DICE6') + call('3423', '12'))\n")
        self.assertIn(('say', '612'), heard)

    def test_no_answer_is_empty(self):
        _, _, heard, _ = self.call("say('[' + call('555') + '] [' + call('666') + ']')\n")
        self.assertIn(('say', '[] []'), heard)

    def test_too_deep_overloads_the_line(self):
        reason, _, heard, phone = self.call("call('777')\nsay('nie')\n")
        self.assertIn(('say', game.OVERLOADED), heard)
        self.assertNotIn(('say', 'nie'), heard)
        self.assertTrue(phone.ended.is_set())
        self.assertEqual(game.MAX_DEPTH, sum(1 for h in heard if h == ('tones', '777')))

    def test_keys_beyond_the_buffer_are_lost(self):
        _, _, heard, _ = self.call("say(keys(30, timeout=0.2))\n", keys='1' * 25)
        self.assertIn(('say', '1' * game.MAX_KEYS), heard)

    def test_clear_keys_forgets(self):
        _, _, heard, _ = self.call("clear_keys()\nsay('[' + keys(1, timeout=0.2) + ']')\n", keys='12')
        self.assertIn(('say', '[]'), heard)

    def test_a_number_without_a_game_has_no_answer(self):
        _, status, heard, _ = self.call("say('[' + call('999') + ']')\n")
        self.assertEqual(0, status)
        self.assertIn(('say', 'Kein Anschluss unter dieser Nummer.'), heard)
        self.assertIn(('say', '[]'), heard)

    def test_wrong_answers_are_errors(self):
        _, status, _, _ = self.call("back('zwölf!')\n")
        self.assertNotEqual(0, status)


if __name__ == '__main__':
    unittest.main()
