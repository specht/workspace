# Die Spielschleife: liest Befehle und gibt sie an das Spiel weiter.
#   python3 play.py          spielt deine Welt (build_world in adventure.py)
#   python3 play.py schule   spielt die Nacht in der Schule (schule.py)
import sys

from adventure import Game
from colors import paint

if len(sys.argv) > 1 and sys.argv[1] == "schule":
    from schule import build_school
    game = Game(build_school())
else:
    from adventure import build_world
    game = Game(build_world())

print(paint(game.start()))
while not game.is_over():
    try:
        line = input("> ")
    except EOFError:
        break
    print(paint(game.handle(line)))
