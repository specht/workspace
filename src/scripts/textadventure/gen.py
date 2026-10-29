# Writes the stages (stufen/), the solution (loesung/) and the typical
# mistakes (fehler/) of the text adventure (content/textadventure, its
# task-examples.yaml uses them) from one tagged source per language, here:
# »@N text« from stage N on, »@N-M text« only in stages N to M, untagged
# lines always; stage 8 is the solution. A mistake is one change to the
# solution. Run from anywhere: python3 src/scripts/textadventure/gen.py
import re, sys, os
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', '..', 'content', 'textadventure')
STAGES = {1: '1-raum', 2: '2-gegenstaende', 3: '3-beschreibung', 4: '4-spielerin', 5: '5-schloss', 6: '6-dunkel', 7: '7-spiel'}
def render(src, stage):
    out = []
    for line in src.split('\n'):
        m = re.match(r'^@(\d+)(?:-(\d+))? ?(.*)$', line)
        if m:
            lo = int(m.group(1)); hi = int(m.group(2)) if m.group(2) else 99
            if lo <= stage <= hi:
                out.append(m.group(3))
        else:
            out.append(line)
    text = '\n'.join(out).rstrip() + '\n'
    return re.sub(r'\n{4,}', '\n\n\n', text)
for ext in ['rb', 'py']:
    src = open(os.path.join(HERE, f'adventure.{ext}.src')).read()
    for n, name in STAGES.items():
        os.makedirs(f'{OUT}/stufen/{name}', exist_ok=True)
        open(f'{OUT}/stufen/{name}/adventure.{ext}', 'w').write(render(src, n))
    open(f'{OUT}/loesung/adventure.{ext}', 'w').write(render(src, 8))


def one(src, a, b):
    assert a in src, a
    return src.replace(a, b, 1)

MISTAKES = {
    'nimmt-nicht-heraus': (("    @items.delete(item) if item\n", ""),
                           ("                self.items.remove(item)\n", "")),
    'durch-die-tuer': (("    return \"Die Tür nach #{direction} ist verschlossen.\" if @room.locked?(direction)\n", ""),
                       ("        if self.room.is_locked(direction):\n            return f\"Die Tür nach {direction} ist verschlossen.\"\n", "")),
    'apfel-bleibt': (("    player.remove(self)\n    \"Du isst", "    \"Du isst"),
                     ("        player.remove(self)\n        return f\"Du isst", "        return f\"Du isst")),
    'leere-zeile': (("    lines.join(\"\\n\")\n", "    lines.join(\"\\n\") + \"\\n\"\n"),
                    ("        return \"\\n\".join(lines)\n", "        return \"\\n\".join(lines) + \"\\n\"\n")),
    'sieht-im-dunkeln': (("    return \"Es ist stockdunkel. Du siehst nichts.\" if @room.dark? && !has_light?\n", ""),
                         ("        if self.room.is_dark() and not self.has_light():\n            return \"Es ist stockdunkel. Du siehst nichts.\"\n", "")),
    'gross-klein': (("line.strip.downcase.split", "line.strip.split"),
                    ("line.strip().lower().split", "line.strip().split")),
    'ziel-endet-nicht': (("    @over = true if won?\n", ""),
                         ("        if self.is_won():\n            self._over = True\n", "")),
    'weg-fehlt-schritt': (("\"gehe küche\", \"nimm kastenschlüssel\", ", "\"gehe küche\", "),) * 2,
    'tuer-abseits': (("corridor.lock(\"kasten\", \"kastenschlüssel\")", "corridor.lock(\"zimmer\", \"kastenschlüssel\")"),) * 2,
}
os.makedirs(f'{OUT}/fehler', exist_ok=True)
for name, (rb_change, py_change) in MISTAKES.items():
    for ext, change in (('rb', rb_change), ('py', py_change)):
        solution = open(f'{OUT}/loesung/adventure.{ext}').read()
        open(f'{OUT}/fehler/{name}.{ext}', 'w').write(one(solution, *change))
