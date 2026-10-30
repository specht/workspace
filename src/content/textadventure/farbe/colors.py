# Farbe im Terminal (textadventure.md, »Farbe im Terminal«): Text in einer
# ANSI-Escape-Sequenz, und eine Antwort des Spiels, bemalt. Das Spiel selbst
# bleibt, wie es ist; nur die Spielschleife bemalt, was sie ausgibt.
BOLD = 1
GREEN = 32
BLUE = 34

LISTS = {"Hier liegt: ": GREEN, "In deiner Tasche: ": GREEN, "Ausgänge: ": BLUE}


def color(text, code):
    return f"\033[{code}m{text}\033[0m"


def paint(answer):
    lines = answer.split("\n")
    room = any(line.startswith("Ausgänge: ") for line in lines)
    painted = []
    for i, line in enumerate(lines):
        label = next((start for start in LISTS if line.startswith(start)), None)
        if i == 0 and room:
            painted.append(color(line, BOLD))
        elif label and line != "Ausgänge: keine":
            names = line[len(label):].split(", ")
            painted.append(label + ", ".join(color(name, LISTS[label]) for name in names))
        else:
            painted.append(line)
    return "\n".join(painted)
