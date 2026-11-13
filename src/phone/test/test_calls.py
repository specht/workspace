"""One line per call for /statistik (core/game.py, record_call): the hour
it began, the line, how long, the numbers of the games it reached, and
nothing about who called; nothing at all without PHONE_CALLS (a Workspace).
    python3 src/phone/test/test_calls.py"""
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'core'))
import game  # noqa: E402


class Calls(unittest.TestCase):
    def test_a_call_is_a_line_of_numbers(self):
        with tempfile.TemporaryDirectory() as folder:
            game.CALLS = folder
            game.record_call('telefon', ['123', '204'], 95.4)
            game.record_call('telefon', [], 3)
            with open(os.path.join(folder, 'telefon.jsonl')) as f:
                lines = [json.loads(line) for line in f]
        self.assertEqual(2, len(lines))
        self.assertEqual({'hour', 'line', 'seconds', 'games'}, set(lines[0]))   # no errors: no key
        self.assertEqual(['123', '204'], lines[0]['games'])
        self.assertEqual(95, lines[0]['seconds'])
        self.assertEqual(0, lines[0]['hour'] % 3600)

    def test_a_crash_is_recorded_with_its_error(self):
        with tempfile.TemporaryDirectory() as folder:
            game.CALLS = folder
            game.record_call('browser', ['741'], 4, [{'game': '741', 'text': 'x' * 3000}, {'game': '', 'text': 'menu'}])
            with open(os.path.join(folder, 'browser.jsonl')) as f:
                entry = json.loads(f.readline())
        self.assertEqual(['741'], [e['game'] for e in entry['errors']])
        self.assertEqual(1500, len(entry['errors'][0]['text']))

    def test_nothing_without_the_folder(self):
        with tempfile.TemporaryDirectory() as folder:
            game.CALLS = ''
            game.record_call('browser', ['123'], 10)
            self.assertEqual([], os.listdir(folder))

    def test_the_number_of_a_published_game(self):
        with tempfile.TemporaryDirectory() as root:
            with open(os.path.join(root, 'published.json'), 'w') as f:
                json.dump({'games': [{'number': '123', 'folder': 'games/abc'}, {'number': 'x', 'folder': 'games/bad'}]}, f)
            self.assertEqual('123', game.published_number(root, 'games/abc'))
            self.assertIsNone(game.published_number(root, 'games/bad'))
            self.assertIsNone(game.published_number(root, 'games/none'))


if __name__ == '__main__':
    unittest.main()
