"""The browser's line with calls on hold (line/line.py, line/menu.py): a
published list with FACT and DICE, a call dialled as 32285 (FACT with the
extension 5) runs through the menu, which connects the number and hands
the 5 on; FACT calls itself down to 1 and back. The page is a stand-in
that has heard everything at once. Checks what the page was sent: the
tones of each dialling and each answer, the sentences, the display back
to FACT after each answer; a number dialled with a typo after a game's
digits still reaches it (the extension), and a number without a game
called by a game gets »Kein Anschluss« and an empty answer.
    python3 src/phone/test/test_line_hold.py"""
import asyncio
import json
import os
import sys
import tempfile
import textwrap
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)


def published(root, games):
    for number, (title, code) in games.items():
        folder = os.path.join(root, f'g{number}')
        os.makedirs(folder)
        with open(os.path.join(folder, 'main.py'), 'w') as f:
            f.write(f'from telefon import *\ntitle({title!r})\n' + textwrap.dedent(code))
    with open(os.path.join(root, 'published.json'), 'w') as f:
        json.dump({'games': [{'number': n, 'title': t, 'folder': f'games/g{n}', 'file': 'main.py'}
                             for n, (t, _) in games.items()]}, f)


class LineHold(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = tempfile.mkdtemp()
        published(cls.root, {
            '3228': ('Fakultät', """
                taste = keys(1, timeout=3)
                if not taste:
                    say('Fakultät. Wähl eine Zahl von 1 bis 9.')
                    taste = keys(1)
                n = int(taste or 0)
                if n <= 1:
                    say(f'Die Fakultät von {n} ist 1.')
                    back('1')
                say(f'Die Fakultät von {n}? Ich frage nach der Fakultät von {n - 1}.')
                rest = int(call('FACT', str(n - 1)))
                say(f'{n} mal {rest} ist {n * rest}. Die Fakultät von {n} ist {n * rest}.')
                back(str(n * rest))
                """),
            '3423': ('Würfelbude', """
                sides = int(keys(3, timeout=0.3) or 0)
                say(f'Würfelbude: {sides}.')
                back(str(sides))
                """),
            '123': ('Frager', """
                say('[' + call('999') + ']')
                say('Würfel ' + call('DICE', '6'))
                """),
        })
        os.environ['PHONE_GAMES'] = cls.root
        sys.path.insert(0, os.path.join(ROOT, 'line'))
        import line
        line.GAMES = cls.root
        cls.line = line

    def call(self, number):
        line = self.line
        events = []

        class Quiet(line.Line):
            def send(self, event):
                events.append(event)
                # the stand-in page: heard at once
                if 'id' in event and self.phone is not None:
                    done = self.phone.heard.get(event['id'])
                    if done is not None and not done.done():
                        asyncio.get_running_loop().call_soon(done.set_result, True)

        quiet = Quiet('127.0.0.1')
        quiet.dialed = number
        asyncio.run(asyncio.wait_for(quiet.run_call(), 60))
        return events

    def said(self, events):
        return [' '.join(e['say']) for e in events if 'say' in e]

    def test_a_factorial_on_hold(self):
        events = self.call('32285')
        said = self.said(events)
        self.assertIn('5 mal 24 ist 120. Die Fakultät von 5 ist 120.', said)
        # each connection says which factorial it was asked for
        self.assertEqual(['Die Fakultät von 5? Ich frage nach der Fakultät von 4.', 'Die Fakultät von 4? Ich frage nach der Fakultät von 3.',
                          'Die Fakultät von 3? Ich frage nach der Fakultät von 2.', 'Die Fakultät von 2? Ich frage nach der Fakultät von 1.'],
                         [x for x in said if 'Ich frage nach' in x])
        self.assertIn('Die Fakultät von 1 ist 1.', said)
        self.assertEqual('5 mal 24 ist 120. Die Fakultät von 5 ist 120.', said[-1])
        tones = [e['tones'] for e in events if 'tones' in e]
        self.assertEqual(['32284', '32283', '32282', '32281', '1#', '2#', '6#', '24#', '120#'], tones)
        self.assertEqual({'ended': 'back'}, events[-1])

    def test_no_game_no_answer_and_a_service(self):
        events = self.call('123')
        said = self.said(events)
        self.assertIn('Kein Anschluss unter dieser Nummer.', said)
        self.assertIn('[]', said)
        self.assertIn('Würfelbude: 6.', said)
        self.assertIn('Würfel 6', said)
        # the display shows the holding game again after the answer
        titles = [e['title'] for e in events if 'title' in e]
        self.assertEqual('Frager', titles[-1])


if __name__ == '__main__':
    unittest.main()
