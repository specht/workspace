# The pictures of Pip's world (Pip unter Funkelbach, docs/plan-katas.md),
# seen from above at a slant, as in grid games: made from the sprites of the
# 2D Game Studio's catalogue (specht/2d, rezepte/sprites, copied to
# catalog/), 24 × 24 pixels per tile, frames side by side.
#
#   python3 make_sprites.py
#
# Browsers keep pictures for years: after a change, raise the ?v= of the
# url(/images/pip/…) in default.css, or they show the old ones.
#
# Pip looks east in the catalogue's side view; west is its mirror image, the
# front (south) has both eyes in the middle of the body, the back (north)
# none. The Funkenstein is drawn here after the catalogue's gem (GEM).
# Paw prints and the puddle are drawn here, in the catalogue's colours.
import math
from pathlib import Path
from PIL import Image

HERE = Path(__file__).parent
CATALOG = HERE / 'catalog'
T = 24
OUTLINE = (26, 28, 44, 255)


def load(name):
    return Image.open(CATALOG / name).convert('RGBA')


def frames(image):
    return [image.crop((x, 0, x + T, T)) for x in range(0, image.width, T)]


def strip(images):
    out = Image.new('RGBA', (T * len(images), T), (0, 0, 0, 0))
    for i, im in enumerate(images):
        out.alpha_composite(im, (i * T, 0))
    return out


def eyes(frame):
    """The dark pixels inside the body: opaque, lighter pixels on both sides."""
    found = []
    for y in range(10, 20):
        for x in range(1, T - 1):
            p, left, right = frame.getpixel((x, y)), frame.getpixel((x - 1, y)), frame.getpixel((x + 1, y))
            if p[3] and p[:3] == OUTLINE[:3] and left[3] and right[3] and left[:3] != OUTLINE[:3] and right[:3] != OUTLINE[:3]:
                found.append((x, y))
    return found


def body_colour(frame, y, xs):
    for x in range(min(xs) - 1, 0, -1):
        p = frame.getpixel((x, y))
        if p[3] and p[:3] != OUTLINE[:3]:
            return p
    return (239, 125, 87, 255)


def facing(frame, direction):
    if direction == 'east':
        return frame
    if direction == 'west':
        return frame.transpose(Image.FLIP_LEFT_RIGHT)
    out = frame.copy()
    spots = eyes(frame)
    if not spots:
        return out
    rows = sorted({y for _, y in spots})
    for x, y in spots:
        out.putpixel((x, y), body_colour(frame, y, [x]))
    if direction == 'south':
        for y in rows:
            opaque = [x for x in range(T) if frame.getpixel((x, y))[3]]
            centre = (opaque[0] + opaque[-1]) // 2
            for x in (centre - 1, centre + 2):
                out.putpixel((x, y), OUTLINE)
    return out


# The Funkenstein lying on the floor, seen from above at a slant: the
# catalogue's gem drawn again in the yellows of a spark, with a large crown
# and a short pavilion, as one sees a stone from above, and a shadow under
# it. Four frames: a spark of light wanders across it, then rests.
GEM = [
    '...oooooo...',
    '..oaabbcco..',
    '.oabbbbccco.',
    'oabbbbbcccco',
    'oooooooooooo',
    '.occcccdddo.',
    '...occddo...',
    '....oooo....',
]
GEM_COLOURS = {'o': OUTLINE, 'a': (255, 247, 180, 255), 'b': (255, 220, 96, 255), 'c': (247, 176, 52, 255), 'd': (214, 120, 34, 255)}


def stone_frame(spark):
    out = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    ox, oy = 6, 9
    for y in range(T):
        for x in range(T):
            if ((x - 11.5) / 6.8) ** 2 + ((y - 17.2) / 1.9) ** 2 <= 1:
                out.putpixel((x, y), (10, 10, 25, 120))
    for y, row in enumerate(GEM):
        for x, c in enumerate(row):
            if c in GEM_COLOURS:
                out.putpixel((ox + x, oy + y), GEM_COLOURS[c])
    if spark:
        sx, sy = ox + spark[0], oy + spark[1]
        for dx, dy in ((0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)):
            out.putpixel((sx + dx, sy + dy), (255, 255, 255, 255) if (dx, dy) == (0, 0) else (255, 252, 220, 255))
    return out


def floor():
    # the cave's ground without its lit top edge, lighter and greyer
    ground = load('boden.png').crop((0, 0, T, T))
    rows = ground.crop((0, 4, T, T))
    out = Image.new('RGBA', (T, T))
    out.alpha_composite(rows, (0, 0))
    out.alpha_composite(rows.crop((0, 0, T, 4)), (0, T - 4))
    return Image.blend(out, Image.new('RGBA', (T, T), (148, 150, 160, 255)), 0.45)


def wall():
    # the catalogue's wall, darker, so the tunnels stand out
    return Image.blend(load('wand.png').crop((0, 0, T, T)), Image.new('RGBA', (T, T), (16, 18, 32, 255)), 0.4)


def trail():
    # two paw prints, a pad and four toes each, in a darker brown than
    # Bello's ears, large enough to read as a trail across the floor
    out = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    paw = (70, 44, 32, 245)
    pad = [(x, y) for y in range(4, 8) for x in range(1, 6) if (x, y) not in ((1, 7), (5, 7), (1, 4), (5, 4))]
    toes = [(x, y + dy) for x, y in ((0, 1), (2, 0), (4, 0), (6, 1)) for dy in (0, 1)]
    for ox, oy in ((3, 12), (13, 3)):
        for dx, dy in pad + toes:
            out.putpixel((ox + dx, oy + dy), paw)
    return out


def trail_diagonal():
    # the paw prints of trail(), pointing north-east, for the corners of a
    # trail, drawn for themselves (a print turned 45 degrees in the browser
    # gets jagged edges): the pad a rounded blob across the way, the four
    # toes on an arc ahead of it, each two pixels slanting forward, with
    # gaps between them
    out = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    paw = (70, 44, 32, 245)
    for ox, oy in ((7, 16), (16, 7)):
        for y in range(-4, 5):
            for x in range(-4, 5):
                across, ahead = (x - y) * 0.7071, -(x + y) * 0.7071
                if (across / 2.5) ** 2 + (ahead / 2.0) ** 2 <= 1:
                    out.putpixel((ox + x, oy + y), paw)
        for angle in (-66, -22, 22, 66):
            t = math.radians(45 + angle)
            tx, ty = round(4.6 * math.sin(t)), round(-4.6 * math.cos(t))
            for px, py in ((tx, ty), (tx + 1, ty - 1)):
                out.putpixel((ox + px, oy + py), paw)
    return out


def puddle():
    out = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    water, light = (65, 166, 246, 210), (115, 239, 247, 230)
    for y in range(T):
        for x in range(T):
            if ((x - 11.5) / 9.5) ** 2 + ((y - 12.5) / 6) ** 2 <= 1:
                out.putpixel((x, y), water)
    for x in range(7, 11):
        out.putpixel((x, 10), light)
    return out


def main():
    stand = frames(load('pip_stehen.png'))
    walk = frames(load('pip_laufen.png'))
    for direction in ('east', 'west', 'north', 'south'):
        strip([facing(stand[0], direction), facing(walk[1], direction)]).save(HERE / f'pip-{direction}.png')
    strip(frames(load('glibber_laufen.png'))).save(HERE / 'glibber.png')
    strip(frames(load('hund_stehen.png'))).save(HERE / 'bello.png')
    strip([stone_frame(spark) for spark in ((3, 2), (6, 1), (8, 3), None)]).save(HERE / 'stone.png')
    # the goal: an old lamp of the stone diggers, dark until Pip lights it
    load('wandlampe_aus.png').save(HERE / 'lamp-off.png')
    load('wandlampe.png').save(HERE / 'lamp.png')
    load('schalter_aus.png').save(HERE / 'lever-off.png')
    load('schalter_an.png').save(HERE / 'lever-on.png')
    load('gitter_zu.png').save(HERE / 'gate-closed.png')
    load('gitter_auf.png').save(HERE / 'gate-open.png')
    wall().save(HERE / 'wall.png')
    floor().save(HERE / 'floor.png')
    trail().save(HERE / 'trail.png')
    trail_diagonal().save(HERE / 'trail-diagonal.png')
    # decoration at the edges of the tunnels, and a bat that flies past
    load('leuchtpilz.png').save(HERE / 'glowshroom.png')
    load('pilz.png').save(HERE / 'mushroom.png')
    load('tropfstein_unten.png').save(HERE / 'stalagmite.png')
    load('fledermaus_fliegen.png').save(HERE / 'bat.png')
    puddle().save(HERE / 'puddle.png')
    strip(frames(load('wichtel_stehen.png'))).save(HERE / 'wichtel.png')


if __name__ == '__main__':
    main()
