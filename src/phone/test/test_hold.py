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

# FACT's goodbyes, one of them at random
GOODBYES = {'Vielen Dank für Ihren Anruf bei der Fakultätsberatung. Wir wünschen Ihnen einen schönen Tag.',
            'Vielen Dank, dass Sie die Fakultätsberatung gewählt haben. Beehren Sie uns bald wieder.',
            'Die Fakultätsberatung bedankt sich für Ihr Vertrauen. Auf Wiederhören.'}


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

    async def line_signal(self, kind, seconds=0.0):
        self.heard.append(('signal', kind))

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
        callee.unanswered = 1.0                            # it rings until it answers
        reason, _ = await run(callee, self.games[number], self.folder)
        if callee.ended.is_set():
            self.ended.set()
        if callee.unanswered is not None and not callee.ended.is_set():
            await self.line_signal('busy')                 # ended without a sound
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
                import random


                # Was nach der Nummer gewählt wurde, ist schon da.
                eingabe = keys(3, timeout=0.5)
                # Ruft FACT sich selbst an, steht eine 0 vorne.
                innen = len(eingabe) > 1 and eingabe.startswith('0')
                # Manchmal ist besetzt: Wer auflegt, bevor er etwas sagt, hat nicht abgenommen.
                if innen and False:
                    hangup()
                if not innen:
                    say('Willkommen zur rekursiven Fakultätsberatung.')
                    if not eingabe:
                        say('Bitte wählen Sie eine Zahl von 1 bis 20 und dann die Raute.')
                        eingabe = keys(3)
                n = int(eingabe or 0)
                if n > 20:
                    say('Über 20 wird das Ergebnis zu lang für die Leitung. Auf Wiederhören.')
                    hangup()

                if n <= 1:
                    ergebnis = 1
                else:
                    say(random.choice([
                        f'Die Fakultät von {n}? Ich frage nach der Fakultät von {n - 1}.',
                        f'Moment bitte. Für die Fakultät von {n} brauche ich die Fakultät von {n - 1}.',
                        f'Die Fakultät von {n}, sehr gern. Einen Augenblick, ich verbinde mit der Fakultät von {n - 1}.',
                        f'Die Fakultät von {n}? Da muss ich kurz bei der Fakultät von {n - 1} nachfragen.',
                    ]))
                    antwort = call('FACT', f'0{n - 1}#')
                    versuche = 1
                    while not antwort and versuche < 5:
                        say(random.choice([
                            'Hm, da ist besetzt. Ich versuche es gleich noch einmal.',
                            'Die Leitung ist belegt. Einen Moment, ich wähle neu.',
                            'Besetzt. Da telefoniert wohl gerade jemand. Ich probiere es noch einmal.',
                        ]))
                        antwort = call('FACT', f'0{n - 1}#')
                        versuche += 1
                    if not antwort:
                        say(f'Die Fakultät von {n - 1} ist heute nicht zu erreichen. Auf Wiederhören.')
                        hangup()
                    vorher = int(antwort)
                    ergebnis = n * vorher
                    say(random.choice([
                        f'{n} mal {vorher} ist {ergebnis}.',
                        f'Danke für Ihre Geduld. {n} mal {vorher} macht {ergebnis}.',
                        f'Da bin ich wieder. {n} mal {vorher} ergibt {ergebnis}.',
                        f'So, die Antwort ist da: {n} mal {vorher} sind {ergebnis}.',
                    ]))

                if not innen:
                    say(f'Die Fakultät von {n} ist {ergebnis}.')
                    say(random.choice([
                        'Vielen Dank für Ihren Anruf bei der Fakultätsberatung. Wir wünschen Ihnen einen schönen Tag.',
                        'Vielen Dank, dass Sie die Fakultätsberatung gewählt haben. Beehren Sie uns bald wieder.',
                        'Die Fakultätsberatung bedankt sich für Ihr Vertrauen. Auf Wiederhören.',
                    ]))
                back(str(ergebnis))
                """),
            # DICE: the number of sides as the extension, here always the highest
            '3423': write(self.folder, 'dice.py', """
                sides = int(keys(3, timeout=0.3) or 0)
                say(f'Würfelbude: {sides}.')
                back(str(sides))
                """),
            '555': write(self.folder, 'quiet.py', "say('Ich sage nichts zurück.')\n"),
            '444': write(self.folder, 'busy.py', "hangup()\n"),
            '446': write(self.folder, 'silent.py', "x = 1\n"),
            '447': write(self.folder, 'late.py', "pick_up(8)\nsay('Endlich.')\npick_up(3)\nback('7')\n"),
            # FACT, busy the first time each number is called inside
            '3229': write(self.folder, 'flaky.py', """
                import os
                import random


                # Was nach der Nummer gewählt wurde, ist schon da.
                eingabe = keys(3, timeout=0.5)
                # Ruft FACT sich selbst an, steht eine 0 vorne.
                innen = len(eingabe) > 1 and eingabe.startswith('0')
                # Manchmal ist besetzt: Wer auflegt, bevor er etwas sagt, hat nicht abgenommen.
                if innen and not os.path.exists(f'busy-{eingabe}'):
                    open(f'busy-{eingabe}', 'w').close()
                    hangup()
                if not innen:
                    say('Willkommen zur rekursiven Fakultätsberatung.')
                    if not eingabe:
                        say('Bitte wählen Sie eine Zahl von 1 bis 20 und dann die Raute.')
                        eingabe = keys(3)
                n = int(eingabe or 0)
                if n > 20:
                    say('Über 20 wird das Ergebnis zu lang für die Leitung. Auf Wiederhören.')
                    hangup()

                if n <= 1:
                    ergebnis = 1
                else:
                    say(random.choice([
                        f'Die Fakultät von {n}? Ich frage nach der Fakultät von {n - 1}.',
                        f'Moment bitte. Für die Fakultät von {n} brauche ich die Fakultät von {n - 1}.',
                        f'Die Fakultät von {n}, sehr gern. Einen Augenblick, ich verbinde mit der Fakultät von {n - 1}.',
                        f'Die Fakultät von {n}? Da muss ich kurz bei der Fakultät von {n - 1} nachfragen.',
                    ]))
                    antwort = call('3229', f'0{n - 1}#')
                    versuche = 1
                    while not antwort and versuche < 5:
                        say(random.choice([
                            'Hm, da ist besetzt. Ich versuche es gleich noch einmal.',
                            'Die Leitung ist belegt. Einen Moment, ich wähle neu.',
                            'Besetzt. Da telefoniert wohl gerade jemand. Ich probiere es noch einmal.',
                        ]))
                        antwort = call('3229', f'0{n - 1}#')
                        versuche += 1
                    if not antwort:
                        say(f'Die Fakultät von {n - 1} ist heute nicht zu erreichen. Auf Wiederhören.')
                        hangup()
                    vorher = int(antwort)
                    ergebnis = n * vorher
                    say(random.choice([
                        f'{n} mal {vorher} ist {ergebnis}.',
                        f'Danke für Ihre Geduld. {n} mal {vorher} macht {ergebnis}.',
                        f'Da bin ich wieder. {n} mal {vorher} ergibt {ergebnis}.',
                        f'So, die Antwort ist da: {n} mal {vorher} sind {ergebnis}.',
                    ]))

                if not innen:
                    say(f'Die Fakultät von {n} ist {ergebnis}.')
                    say(random.choice([
                        'Vielen Dank für Ihren Anruf bei der Fakultätsberatung. Wir wünschen Ihnen einen schönen Tag.',
                        'Vielen Dank, dass Sie die Fakultätsberatung gewählt haben. Beehren Sie uns bald wieder.',
                        'Die Fakultätsberatung bedankt sich für Ihr Vertrauen. Auf Wiederhören.',
                    ]))
                back(str(ergebnis))
                """),
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
        self.assertIn(('tones', '32281'), heard)          # dialled: the number and its extension
        self.assertIn(('say', 'Gesamt: 1'), heard)

    def test_a_factorial_calls_itself(self):
        _, _, heard, _ = self.call("say('Ergebnis ' + call('FACT', '5#'))\n")
        said = [h[1] for h in heard if h[0] == 'say']
        # each asks for the next one (the sentence is one of several) and
        # says its product on the way back
        for n in (5, 4, 3, 2):
            self.assertTrue(any(f'Fakultät von {n}' in x and f'Fakultät von {n - 1}' in x for x in said), n)
        self.assertTrue(any('5 mal 24' in x and x.endswith('120.') for x in said))
        self.assertIn(('tones', '322804#'), heard)        # it calls itself with a 0 in front
        # only the outer one states the result and says goodbye
        self.assertEqual(1, sum(1 for x in said if x.startswith('Die Fakultät von') and ' ist ' in x))
        self.assertEqual('Die Fakultät von 5 ist 120.', said[-3])
        self.assertIn(said[-2], GOODBYES)
        self.assertEqual('Ergebnis 120', said[-1])
        self.assertIn(('tones', '120#'), heard)            # the answer, heard as tones
        self.assertEqual(['Willkommen zur rekursiven Fakultätsberatung.'], [x for x in said if x.startswith('Willkommen')])

    def test_without_a_number_it_asks_at_once(self):
        _, _, heard, _ = self.call("say('Ergebnis ' + call('FACT'))\n", keys='')
        said = [h[1] for h in heard if h[0] == 'say']
        self.assertEqual(['Willkommen zur rekursiven Fakultätsberatung.',
                          'Bitte wählen Sie eine Zahl von 1 bis 20 und dann die Raute.'], said[:2])

    def test_a_call_rings_until_the_game_says_something(self):
        _, _, heard, _ = self.call("say(call('DICE', '6'))\n")
        ring = heard.index(('signal', 'ring'))
        self.assertLess(heard.index(('tones', '34236')), ring)
        self.assertLess(ring, heard.index(('say', 'Würfelbude: 6.')))

    def test_hanging_up_unanswered_or_ending_silently_is_busy(self):
        _, _, heard, _ = self.call("say('[' + call('444') + '] [' + call('446') + ']')\n")
        self.assertEqual(2, heard.count(('signal', 'busy')))
        self.assertNotIn(('signal', 'ring'), heard)
        self.assertIn(('say', '[] []'), heard)

    def test_pick_up_late_and_only_once(self):
        _, _, heard, _ = self.call("say(call('447'))\n")
        self.assertEqual(1, heard.count(('signal', 'ring')))           # its own pick_up, not the line's ringing too
        self.assertLess(heard.index(('signal', 'ring')), heard.index(('say', 'Endlich.')))
        self.assertIn(('say', '7'), heard)
        # dialled by a person: the same ringing first
        heard = []
        phone = TestPhone(self.games, self.folder, heard)
        asyncio.run(run(phone, 'late.py', self.folder))
        self.assertEqual([('signal', 'ring'), ('say', 'Endlich.')], heard[:2])

    def test_fact_tries_again_when_busy(self):
        _, _, heard, _ = self.call("say(call('3229', '4#'))\n")
        said = [h[1] for h in heard if h[0] == 'say']
        self.assertEqual(3, heard.count(('signal', 'busy')))          # the 3, the 2 and the 1 once each
        self.assertEqual(3, sum(1 for x in said if 'besetzt' in x.lower() or 'belegt' in x))
        self.assertEqual('24', said[-1])

    def test_twenty_deep_and_numbers_of_two_digits(self):
        _, _, heard, phone = self.call("say(call('FACT', '20#'))\n")
        self.assertFalse(phone.ended.is_set())             # 19 on hold: not overloaded
        self.assertIn(('say', '2432902008176640000'), heard)
        self.assertIn(('tones', '3228019#'), heard)

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
