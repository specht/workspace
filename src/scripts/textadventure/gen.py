# Writes the stages (stufen/), the solution (loesung/) and the typical
# mistakes (fehler/) of the text adventure (content/textadventure, its
# task-examples.yaml uses them) from one tagged source per language, here:
# »@N text« from stage N on, »@N-M text« only in stages N to M, untagged
# lines always; stage 8 is the solution. A mistake is one change to the
# solution. Java has one file per class: »//// Room.java @N« (or @N-M)
# starts a file that exists from stage N on; its stages and solution go
# into a folder java/, a mistake into fehler/java/NAME/ as a whole copy of
# the solution. Run from anywhere: python3 src/scripts/textadventure/gen.py
import re, os, shutil
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', '..', 'content', 'textadventure')
STAGES = {1: '1-raum', 2: '2-gegenstaende', 3: '3-beschreibung', 4: '4-spielerin', 5: '5-schloss', 6: '6-dunkel', 7: '7-spiel'}
TAG = re.compile(r'^@(\d+)(?:-(\d+))? ?(.*)$')


def within(stage, lo, hi):
    return int(lo) <= stage <= (int(hi) if hi else 99)


def render(src, stage):
    out = []
    for line in src.split('\n'):
        m = TAG.match(line)
        if m:
            if within(stage, m.group(1), m.group(2)):
                out.append(m.group(3))
        else:
            out.append(line)
    text = '\n'.join(out).rstrip() + '\n'
    return re.sub(r'\n{4,}', '\n\n\n', text)


# {file name: text} of the Java source at a stage
def render_java(src, stage):
    files = {}
    for part in re.split(r'^//// ', src, flags=re.M)[1:]:
        head, body = part.split('\n', 1)
        m = re.match(r'^(\S+) @(\d+)(?:-(\d+))?$', head.strip())
        assert m, head
        if within(stage, m.group(2), m.group(3)):
            files[m.group(1)] = render(body, stage)
    return files


def write_java(folder, files):
    shutil.rmtree(folder, ignore_errors=True)
    os.makedirs(folder)
    for name, text in files.items():
        open(os.path.join(folder, name), 'w').write(text)


SOURCES = {ext: open(os.path.join(HERE, f'adventure.{ext}.src')).read() for ext in ['rb', 'py', 'dart', 'java']}
for n, name in list(STAGES.items()) + [(8, None)]:
    folder = f'{OUT}/stufen/{name}' if name else f'{OUT}/loesung'
    os.makedirs(folder, exist_ok=True)
    for ext in ['rb', 'py', 'dart']:
        open(f'{folder}/adventure.{ext}', 'w').write(render(SOURCES[ext], n))
    write_java(f'{folder}/java', render_java(SOURCES['java'], n))


def one(src, a, b):
    assert a in src, a
    return src.replace(a, b, 1)

# per mistake one change in each language; in Java with the file it is in
MISTAKES = {
    'nimmt-nicht-heraus': {
        'rb': ("    @items.delete(item) if item\n", ""),
        'py': ("                self.items.remove(item)\n", ""),
        'dart': ("        items.remove(item);\n", ""),
        'java': ('Room.java', "                items.remove(item);\n", ""),
    },
    'durch-die-tuer': {
        'rb': ("    return \"Die Tür nach #{direction} ist verschlossen.\" if @room.locked?(direction)\n", ""),
        'py': ("        if self.room.is_locked(direction):\n            return f\"Die Tür nach {direction} ist verschlossen.\"\n", ""),
        'dart': ("    if (_room.isLocked(direction)) {\n      return 'Die Tür nach $direction ist verschlossen.';\n    }\n", ""),
        'java': ('Player.java', "        if (room.isLocked(direction)) {\n            return \"Die Tür nach \" + direction + \" ist verschlossen.\";\n        }\n", ""),
    },
    'apfel-bleibt': {
        'rb': ("    player.remove(self)\n    \"Du isst", "    \"Du isst"),
        'py': ("        player.remove(self)\n        return f\"Du isst", "        return f\"Du isst"),
        'dart': ("    player.remove(this);\n", ""),
        'java': ('Food.java', "        player.remove(this);\n", ""),
    },
    'leere-zeile': {
        'rb': ("    lines.join(\"\\n\")\n", "    lines.join(\"\\n\") + \"\\n\"\n"),
        'py': ("        return \"\\n\".join(lines)\n", "        return \"\\n\".join(lines) + \"\\n\"\n"),
        'dart': ("    return lines.join('\\n');\n", "    return lines.join('\\n') + '\\n';\n"),
        'java': ('Room.java', "        return String.join(\"\\n\", lines);\n", "        return String.join(\"\\n\", lines) + \"\\n\";\n"),
    },
    'sieht-im-dunkeln': {
        'rb': ("    return \"Es ist stockdunkel. Du siehst nichts.\" if @room.dark? && !has_light?\n", ""),
        'py': ("        if self.room.is_dark() and not self.has_light():\n            return \"Es ist stockdunkel. Du siehst nichts.\"\n", ""),
        'dart': ("    if (_room.isDark && !hasLight) {\n      return 'Es ist stockdunkel. Du siehst nichts.';\n    }\n", ""),
        'java': ('Player.java', "        if (room.isDark() && !hasLight()) {\n            return \"Es ist stockdunkel. Du siehst nichts.\";\n        }\n", ""),
    },
    'gross-klein': {
        'rb': ("line.strip.downcase.split", "line.strip.split"),
        'py': ("line.strip().lower().split", "line.strip().split"),
        'dart': ("line.trim().toLowerCase().split", "line.trim().split"),
        'java': ('Game.java', "line.strip().toLowerCase().split", "line.strip().split"),
    },
    'ziel-endet-nicht': {
        'rb': ("    @over = true if won?\n", ""),
        'py': ("        if self.is_won():\n            self._over = True\n", ""),
        'dart': ("    if (isWon) {\n      _over = true;\n    }\n", ""),
        'java': ('Game.java', "        if (isWon()) {\n            over = true;\n        }\n", ""),
    },
    'weg-fehlt-schritt': {
        'rb': ("\"gehe küche\", \"nimm kastenschlüssel\", ", "\"gehe küche\", "),
        'py': ("\"gehe küche\", \"nimm kastenschlüssel\", ", "\"gehe küche\", "),
        'dart': ("'gehe küche', 'nimm kastenschlüssel', ", "'gehe küche', "),
        'java': ('World.java', "\"gehe küche\", \"nimm kastenschlüssel\", ", "\"gehe küche\", "),
    },
    'tuer-abseits': {
        'rb': ("corridor.lock(\"kasten\", \"kastenschlüssel\")", "corridor.lock(\"zimmer\", \"kastenschlüssel\")"),
        'py': ("corridor.lock(\"kasten\", \"kastenschlüssel\")", "corridor.lock(\"zimmer\", \"kastenschlüssel\")"),
        'dart': ("corridor.lock('kasten', 'kastenschlüssel')", "corridor.lock('zimmer', 'kastenschlüssel')"),
        'java': ('World.java', "corridor.lock(\"kasten\", \"kastenschlüssel\")", "corridor.lock(\"zimmer\", \"kastenschlüssel\")"),
    },
}
os.makedirs(f'{OUT}/fehler', exist_ok=True)
solution_java = render_java(SOURCES['java'], 8)
for name, changes in MISTAKES.items():
    for ext in ['rb', 'py', 'dart']:
        solution = open(f'{OUT}/loesung/adventure.{ext}').read()
        open(f'{OUT}/fehler/{name}.{ext}', 'w').write(one(solution, *changes[ext]))
    file, old, new = changes['java']
    files = dict(solution_java)
    files[file] = one(files[file], old, new)
    write_java(f'{OUT}/fehler/java/{name}', files)
