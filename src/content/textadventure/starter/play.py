# Die Spielschleife: liest Befehle und gibt sie an die Spielerin weiter.
# Die Welt baut build_world in adventure.py.
from adventure import build_world

player = build_world()
print(player.room.describe())

while True:
    try:
        line = input("> ")
    except EOFError:
        break
    words = line.strip().lower().split(" ", 1)
    verb = words[0]
    word = words[1] if len(words) > 1 else ""
    if verb == "gehe":
        print(player.go(word))
    elif verb == "nimm":
        print(player.take(word))
    elif verb == "benutze":
        print(player.use(word))
    elif verb == "schau":
        print(player.room.describe())
    elif verb == "tasche":
        names = [item.name for item in player.inventory]
        print("In deiner Tasche: " + ", ".join(names) if names else "Deine Tasche ist leer.")
    elif verb == "ende":
        break
    elif verb == "":
        continue
    else:
        print("Das verstehe ich nicht. Befehle: gehe, nimm, benutze, schau, tasche, ende")
