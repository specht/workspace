# The pictures of Pip's world (Pip unter Funkelbach, docs/plan-katas.md),
# seen from above at a slant, as in grid games: made from the sprites of the
# 2D Game Studio's catalogue (specht/2d, rezepte/sprites, copied to
# catalog/), 24 × 24 pixels per tile, frames side by side.
#
#   python3 make_sprites.py
#
# Pip looks east in the catalogue's side view; west is its mirror image, the
# front (south) has both eyes in the middle of the body, the back (north)
# none. The Funkenstein is the catalogue's gem in the yellow of a spark.
# Paw prints and the puddle are drawn here, in the catalogue's colours.
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


def recolour(image, mapping):
    out = image.copy()
    px = out.load()
    for y in range(out.height):
        for x in range(out.width):
            p = px[x, y]
            if p[3] and p[:3] in mapping:
                px[x, y] = mapping[p[:3]] + (p[3],)
    return out


def spark_colours(image):
    """The gem's blues as the yellows of a spark, lightest to darkest."""
    blues = sorted({p[:3] for p in image.getdata() if p[3] and p[:3] != OUTLINE[:3]}, key=sum, reverse=True)
    yellows = [(255, 247, 180), (255, 220, 96), (247, 176, 52), (214, 120, 34), (160, 80, 30)]
    return {b: yellows[min(i, len(yellows) - 1)] for i, b in enumerate(blues)}


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
    # two paw prints, a pad and four toes each, in the brown of Bello's ears
    out = Image.new('RGBA', (T, T), (0, 0, 0, 0))
    paw = (93, 60, 42, 235)
    pad = [(1, 3), (2, 3), (3, 3), (1, 4), (2, 4), (3, 4), (2, 5)]
    toes = [(0, 1), (1, 0), (3, 0), (4, 1)]
    for ox, oy in ((3, 12), (14, 4)):
        for dx, dy in pad + toes:
            out.putpixel((ox + dx, oy + dy), paw)
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
    gem = load('edelstein.png')
    recolour(gem, spark_colours(gem)).save(HERE / 'stone.png')
    load('fackel.png').save(HERE / 'lamp.png')
    load('schalter_aus.png').save(HERE / 'lever-off.png')
    load('schalter_an.png').save(HERE / 'lever-on.png')
    load('gitter_zu.png').save(HERE / 'gate-closed.png')
    load('gitter_auf.png').save(HERE / 'gate-open.png')
    wall().save(HERE / 'wall.png')
    floor().save(HERE / 'floor.png')
    trail().save(HERE / 'trail.png')
    puddle().save(HERE / 'puddle.png')
    strip(frames(load('wichtel_stehen.png'))).save(HERE / 'wichtel.png')


if __name__ == '__main__':
    main()
