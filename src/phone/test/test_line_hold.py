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
import re
import sys
import tempfile
import textwrap
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)

# FACT's goodbyes, one of them at random
GOODBYES = {'Vielen Dank für Ihren Anruf bei der Fakultätsberatung. Wir wünschen Ihnen einen schönen Tag.',
            'Vielen Dank, dass Sie die Fakultätsberatung gewählt haben. Beehren Sie uns bald wieder.',
            'Die Fakultätsberatung bedankt sich für Ihr Vertrauen. Auf Wiederhören.'}


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
        # each connection says which factorial it was asked for (one of
        # several sentences); the inner ones are called with a 0 in front,
        # and only the outer one states the result and says goodbye
        asks = [x for x in said if re.search(r'Fakultät von \d+\D.*Fakultät von \d+', x)]
        self.assertEqual([5, 4, 3, 2], [int(re.search(r'Fakultät von (\d+)', x).group(1)) for x in asks])
        self.assertIn('5 mal 24', said[-3])
        self.assertEqual('Die Fakultät von 5 ist 120.', said[-2])
        self.assertIn(said[-1], GOODBYES)
        # each of the four calls rings before the called game speaks
        self.assertEqual(['ring'] * 4, [e['signal'] for e in events if 'signal' in e])
        tones = [e['tones'] for e in events if 'tones' in e]
        self.assertEqual(['322804#', '322803#', '322802#', '322801#', '1#', '2#', '6#', '24#', '120#'], tones)
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
