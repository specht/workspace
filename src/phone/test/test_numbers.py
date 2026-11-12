"""How a dialled number is read (core/game.py, game_number): a game's own
number, the line's real number with it after it, the line's alone (the
menu), or neither.
    python3 src/phone/test/test_numbers.py"""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'core'))
from game import game_number, international  # noqa: E402

LINE = '030/22 908 772'


class Numbers(unittest.TestCase):
    def test_international(self):
        for written in ('030/22 908 772', '+49 30 22908772', '004930 22908772', '493022908772'):
            self.assertEqual(international(written), '493022908772')

    def test_game_alone(self):
        self.assertEqual(game_number('123', LINE), '123')
        self.assertEqual(game_number('1234', LINE), '1234')

    def test_line_with_game(self):
        # as the trunk reports it, and written as the operator might
        self.assertEqual(game_number('493022908772123', LINE), '123')
        self.assertEqual(game_number('03022908772123', LINE), '123')
        self.assertEqual(game_number('+4930229087721234', LINE), '1234')

    def test_line_alone_is_the_menu(self):
        self.assertEqual(game_number('03022908772', LINE), '')
        self.assertEqual(game_number('493022908772', LINE), '')

    def test_neither(self):
        self.assertIsNone(game_number('0302290877', LINE))          # a digit short
        self.assertIsNone(game_number('03022908772' + '12', LINE))   # two digits are no game
        self.assertIsNone(game_number('K4081069T1', LINE))          # the SIP user's name
        self.assertIsNone(game_number('12', LINE))

    def test_short_line_number(self):
        # the test line of e2e.sh: 100 is the line, 100123 its game 123
        self.assertEqual(game_number('100', '100'), '')
        self.assertEqual(game_number('100123', '100'), '123')
        self.assertEqual(game_number('123', '100'), '123')

    def test_no_line_number(self):
        self.assertEqual(game_number('123', ''), '123')
        self.assertIsNone(game_number('03022908772123', ''))


if __name__ == '__main__':
    unittest.main()
