# The pictures of Pip's world (Pip unter Funkelbach, docs/plan-katas.md),
# drawn here pixel by pixel, all in one palette and one view: seen from
# above at a slant (the 3/4 view of tile games), light from the top left.
# A tile is 24 × 24 pixels; what stands upright is taller than its tile and
# reaches into the one above (the lamp post 24 × 40, the gate 24 × 32).
# Frames lie side by side in one strip. Shading between two colours of the
# palette is dithered (a 4 × 4 Bayer pattern), so that rock and ground get
# soft light instead of flat areas.
#
#   python3 make_sprites.py           # the pictures
#   python3 make_sprites.py --sheet   # and sheet.png, all of them enlarged
#
# Browsers keep pictures for years: after a change, raise the ?v= of the
# url(/images/pip/…) in default.css, or they show the old ones.
#
# The world: cool blue-violet rock, a warm dusty floor, amber light (Pip's
# lantern, the old lamps, the Funkensteine) and cyan light (glowing
# mushrooms, crystals). The characters are those of the 2D Game Studio
# (below, »the characters«); everything around them is drawn here.
import math
import random
import sys
from pathlib import Path
from PIL import Image

HERE = Path(__file__).parent
T = 24

OUT = (22, 16, 34, 255)
ROCK = [(16, 16, 32), (26, 28, 50), (38, 42, 72), (54, 60, 96), (74, 84, 126), (100, 114, 158), (140, 156, 196)]
GROUND = [(52, 42, 60), (68, 56, 76), (84, 71, 92), (102, 88, 110), (124, 108, 130), (150, 132, 152)]
AMBER = [(110, 48, 30), (172, 82, 36), (228, 130, 46), (250, 182, 70), (255, 224, 132), (255, 248, 206)]
PIP = [(116, 42, 46), (176, 70, 58), (226, 108, 72), (246, 150, 98), (255, 198, 146)]
LEAF = [(28, 84, 60), (44, 128, 70), (94, 184, 84), (170, 226, 112)]
WATER = [(24, 44, 96), (34, 78, 148), (50, 124, 198), (92, 180, 230), (168, 230, 252)]
GLOW = [(18, 70, 104), (28, 132, 168), (72, 210, 232), (176, 250, 255)]
RED = [(86, 22, 40), (146, 38, 54), (204, 70, 70), (240, 122, 102)]
CREAM = [(120, 104, 100), (178, 162, 150), (228, 218, 204), (250, 246, 236)]
WOOD = [(54, 30, 28), (88, 52, 40), (126, 80, 52), (166, 116, 72), (198, 152, 98)]
IRON = [(34, 38, 54), (60, 70, 90), (96, 110, 132), (148, 164, 186)]
VIOLET = [(56, 30, 88), (98, 54, 146), (156, 102, 214), (214, 176, 255)]
WHITE = (250, 250, 246)

BAYER = [[0, 8, 2, 10], [12, 4, 14, 6], [3, 11, 1, 9], [15, 7, 13, 5]]


def rgba(c, a=255):
    return c[:3] + (a,)


def canvas(w=T, h=T):
    return Image.new('RGBA', (w, h), (0, 0, 0, 0))


def put(img, x, y, c):
    if 0 <= x < img.width and 0 <= y < img.height:
        img.putpixel((int(x), int(y)), rgba(c) if len(c) == 3 else c)


def tone(ramp, value, x, y):
    """The colour of ramp at value (0 … len - 1, between two colours too),
    dithered with the Bayer pattern at x, y."""
    value = max(0.0, min(len(ramp) - 1.0, value))
    low = int(value)
    if low >= len(ramp) - 1:
        return ramp[-1]
    return ramp[low + 1] if value - low > (BAYER[y % 4][x % 4] + 0.5) / 16 else ramp[low]


def noise(seed, period=4):
    """Smooth noise over a tile that wraps at its edges: random values on a
    period × period grid, blended between them; returns f(x, y) in 0 … 1."""
    rng = random.Random(seed)
    grid = [[rng.random() for _ in range(period)] for _ in range(period)]
    cell = T / period

    def f(x, y):
        gx, gy = x / cell, y / cell
        x0, y0 = int(gx) % period, int(gy) % period
        x1, y1 = (x0 + 1) % period, (y0 + 1) % period
        tx, ty = gx - int(gx), gy - int(gy)
        tx, ty = tx * tx * (3 - 2 * tx), ty * ty * (3 - 2 * ty)
        top = grid[y0][x0] * (1 - tx) + grid[y0][x1] * tx
        bottom = grid[y1][x0] * (1 - tx) + grid[y1][x1] * tx
        return top * (1 - ty) + bottom * ty
    return f


def blob(img, cx, cy, rx, ry, ramp, light=(-0.55, -0.65), shine=True, dither=True):
    """A shaded ellipse, as if it were a ball: the side towards the light
    lighter, the far side darker, dithered between, a spot of shine."""
    lx, ly = light
    lz = math.sqrt(max(0.0, 1 - lx * lx - ly * ly))
    for y in range(int(cy - ry - 1), int(cy + ry + 2)):
        for x in range(int(cx - rx - 1), int(cx + rx + 2)):
            u, v = (x + 0.5 - cx) / rx, (y + 0.5 - cy) / ry
            d = u * u + v * v
            if d > 1:
                continue
            lit = max(0.0, u * lx + v * ly + math.sqrt(1 - d) * lz)
            value = lit * (len(ramp) - 1.4)
            colour = tone(ramp, value, x, y) if dither else ramp[min(len(ramp) - 1, int(value))]
            if shine and lit > 0.95:
                colour = ramp[-1]
            put(img, x, y, colour)


def outline(img, colour=OUT):
    """A line round everything drawn, on the transparent pixels next to it."""
    src = img.copy()
    for y in range(img.height):
        for x in range(img.width):
            if src.getpixel((x, y))[3]:
                continue
            if any(0 <= x + dx < img.width and 0 <= y + dy < img.height and src.getpixel((x + dx, y + dy))[3] == 255
                   for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1))):
                put(img, x, y, colour)
    return img


def shadow(img, cx, cy, rx, ry, alpha=110):
    """A soft dark oval on the floor under something that stands there."""
    for y in range(int(cy - ry - 1), int(cy + ry + 2)):
        for x in range(int(cx - rx - 1), int(cx + rx + 2)):
            d = ((x + 0.5 - cx) / rx) ** 2 + ((y + 0.5 - cy) / ry) ** 2
            if d <= 1 and 0 <= x < img.width and 0 <= y < img.height and img.getpixel((x, y))[3] == 0:
                put(img, x, y, (10, 8, 20, int(alpha * (1.2 - d * 0.5))))


def strip(frames):
    out = canvas(frames[0].width * len(frames), frames[0].height)
    for i, f in enumerate(frames):
        out.alpha_composite(f, (i * f.width, 0))
    return out


def mirror(img):
    return img.transpose(Image.FLIP_LEFT_RIGHT)


# --- the ground and the rock ------------------------------------------------

def floor(variant):
    """Dusty ground: soft lighter and darker patches (dithered), a few
    pebbles, now and then a crack."""
    rng = random.Random(100 + variant)
    # the same soft patches in every variant, so that tiles side by side
    # join without a seam; the variants differ in grain, pebbles and cracks
    big, fine = noise(110, 3), noise(120 + variant, 6)
    img = canvas()
    for y in range(T):
        for x in range(T):
            put(img, x, y, tone(GROUND, 1.7 + big(x, y) * 0.9 + (fine(x, y) - 0.5) * 0.7, x, y))
    for _ in range(2 + variant % 2):
        px, py = rng.randint(3, 18), rng.randint(3, 18)
        w = rng.choice([2, 3, 3])
        for dx in range(w):
            put(img, px + dx, py, GROUND[4] if dx < w - 1 else GROUND[3])
            put(img, px + dx, py + 1, GROUND[3])
            put(img, px + dx, py + 2, GROUND[0])
    if variant == 2:
        x, y = 5, 15
        for _ in range(9):
            put(img, x, y, GROUND[0])
            put(img, x, y - 1, GROUND[3])
            x += 1
            y += rng.choice([-1, 0, 0, 1])
    return img


def rock_top(variant):
    """The top of the rock seen from above: rounded lumps, each lit from the
    top left and shading off (dithered), dark seams between them."""
    rng = random.Random(200 + variant)
    grain = noise(210 + variant, 6)
    lumps = []
    for _ in range(7):
        cx, cy, r = rng.uniform(0, T), rng.uniform(0, T), rng.uniform(4.5, 7.5)
        for ox in (-T, 0, T):
            for oy in (-T, 0, T):
                lumps.append((cx + ox, cy + oy, r))
    img = canvas()
    for y in range(T):
        for x in range(T):
            near = sorted((math.hypot(x + 0.5 - cx, y + 0.5 - cy) / r, cx, cy, r) for cx, cy, r in lumps)
            d, cx, cy, r = near[0]
            if d > 1:
                put(img, x, y, tone(ROCK, 0.6 + grain(x, y) * 0.6, x, y))
                continue
            u, v = (x + 0.5 - cx) / r, (y + 0.5 - cy) / r
            lit = -0.55 * u - 0.6 * v + math.sqrt(max(0.0, 1 - u * u - v * v)) * 0.55
            value = 1.4 + lit * 2.0 + (grain(x, y) - 0.5) * 0.6
            # the seam where two lumps meet
            if len(near) > 1 and near[1][0] - d < 0.12:
                value = 0.7
            put(img, x, y, tone(ROCK, value, x, y))
    return img


def rock_face(variant):
    """The front of the rock where it faces an open tile, half a tile high:
    rounded rocks lit from the top left (dithered), dark gaps between them,
    dark at its foot."""
    rng = random.Random(300 + variant)
    grain = noise(310 + variant, 8)
    h = T // 2
    rocks = []
    x = rng.uniform(-4, 0)
    while x < T + 4:
        rx, ry = rng.uniform(3.5, 6), rng.uniform(3.5, 5)
        rocks.append((x + rx, rng.uniform(4, 8), rx, ry))
        x += rx * 2 - rng.uniform(0.5, 1.5)
    for cx, cy, rx, ry in list(rocks):
        rocks.append((cx + T, cy, rx, ry))
        rocks.append((cx - T, cy, rx, ry))
    img = canvas(T, h)
    for y in range(h):
        for x in range(T):
            near = sorted((((x + 0.5 - cx) / rx) ** 2 + ((y + 0.5 - cy) / ry) ** 2, cx, cy, rx, ry) for cx, cy, rx, ry in rocks)
            d, cx, cy, rx, ry = near[0]
            if d > 1:
                put(img, x, y, tone(ROCK, 1.2 + grain(x, y) * 0.6, x, y))
                continue
            u, v = (x + 0.5 - cx) / rx, (y + 0.5 - cy) / ry
            lit = -0.5 * u - 0.75 * v + math.sqrt(max(0.0, 1 - d)) * 0.5
            value = 3.3 + lit * 2.0 - y * 0.08 + (grain(x, y) - 0.5) * 0.5
            if len(near) > 1 and near[1][0] - d < 0.25:
                value = 1.4
            put(img, x, y, tone(ROCK, value, x, y))
    # dark at its foot; the light edge on top is drawn by pip_world.js along
    # the face's outline, round curves too, so it is not in the picture
    for x in range(T):
        put(img, x, h - 1, ROCK[0])
    return img


# --- the characters: from the 2D Game Studio ------------------------------
# Pip, the Glibber, Bello and the Wichtel are the figures of the 2D Game
# Studio's demo game (specht/2d, rezepte/sprites, copied to catalog/), so
# that students know them again; only a shadow on the ground is added, so
# that they stand in the 3/4 view. The catalogue draws Pip from the side,
# looking east: west is its mirror image, the front (south) has both eyes
# in the middle of the body, the back (north) none.

CATALOG = HERE / 'catalog'


def load(name):
    return Image.open(CATALOG / name).convert('RGBA')


def frames(image):
    return [image.crop((x, 0, x + T, T)) for x in range(0, image.width, T)]


# where every figure's feet are in its tile, so that all of them stand in
# the middle of their field, not on its lower edge
FEET = 21


def grounded(frame, rx=7):
    """The figure, its feet at FEET, with a shadow on the floor under them."""
    bottom = max(y for y in range(T) for x in range(T) if frame.getpixel((x, y))[3])
    img = canvas()
    shadow(img, 12, FEET + 0.5, rx, 1.6)
    img.alpha_composite(frame, (0, FEET - bottom))
    return img


def eyes(frame):
    """The dark pixels inside the body: opaque, lighter pixels on both sides."""
    found = []
    for y in range(10, 20):
        for x in range(1, T - 1):
            p, left, right = frame.getpixel((x, y)), frame.getpixel((x - 1, y)), frame.getpixel((x + 1, y))
            if p[3] and p[:3] == CATALOG_OUTLINE and left[3] and right[3] and left[:3] != CATALOG_OUTLINE and right[:3] != CATALOG_OUTLINE:
                found.append((x, y))
    return found


# the catalogue's outline colour, of Pip's eyes too
CATALOG_OUTLINE = (26, 28, 44)


def body_colour(frame, y, x):
    for left in range(x - 1, 0, -1):
        p = frame.getpixel((left, y))
        if p[3] and p[:3] != CATALOG_OUTLINE:
            return p
    return (239, 125, 87, 255)


def turned(frame, direction):
    if direction == 'east':
        return frame
    if direction == 'west':
        return mirror(frame)
    out = frame.copy()
    spots = eyes(frame)
    for x, y in spots:
        out.putpixel((x, y), body_colour(frame, y, x))
    if direction == 'south':
        for y in sorted({y for _, y in spots}):
            opaque = [x for x in range(T) if frame.getpixel((x, y))[3]]
            centre = (opaque[0] + opaque[-1]) // 2
            for x in (centre - 1, centre + 2):
                out.putpixel((x, y), CATALOG_OUTLINE + (255,))
    return out


def pip_frames(direction):
    stand, walk = frames(load('pip_stehen.png')), frames(load('pip_laufen.png'))
    return [grounded(turned(stand[0], direction)), grounded(turned(walk[1], direction))]


# --- the others -------------------------------------------------------------

def stone(frame):
    """A Funkenstein lying on the floor: a cut gem seen from above, a large
    crown and a short pavilion; a spark of light wanders across it."""
    gem = ['...oooooo...',
           '..oaabbcco..',
           '.oabbbbccco.',
           'oabbbbbcccco',
           'oooooooooooo',
           '.occcccdddo.',
           '...occddo...',
           '....oooo....']
    colours = {'o': OUT, 'a': AMBER[5], 'b': AMBER[4], 'c': AMBER[3], 'd': AMBER[2]}
    img = canvas()
    shadow(img, 12, 17.5, 7, 1.8, 120)
    for y, row in enumerate(gem):
        for x, c in enumerate(row):
            if c in colours:
                put(img, 6 + x, 9 + y, colours[c])
    spark = [(3, 2), (6, 1), (8, 3), None][frame]
    if spark:
        sx, sy = 6 + spark[0], 9 + spark[1]
        for dx, dy in ((0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)):
            put(img, sx + dx, sy + dy, WHITE if (dx, dy) == (0, 0) else AMBER[5])
    return img


def lamp(lit):
    """An old lamp of the stone diggers: a post at the back of its tile with
    an arm, the lantern hanging from it above Pip's head. 24 × 40, its
    foot at the bottom of the tile."""
    img = canvas(T, 40)
    shadow(img, 6, 30, 5, 1.6)
    for y in range(4, 31):
        put(img, 4, y, WOOD[3])
        put(img, 5, y, tone(WOOD, 2.3 - (y % 7 == 0) * 1.0, 5, y))
        put(img, 6, y, WOOD[1])
    for x in range(4, 18):
        put(img, x, 4, WOOD[3])
        put(img, x, 5, WOOD[1])
    for x, y in ((3, 30), (7, 30), (4, 31), (5, 31), (6, 31)):
        put(img, x, y, ROCK[3])
    put(img, 16, 6, IRON[2])
    put(img, 16, 7, IRON[2])
    for x in range(13, 20):
        put(img, x, 8, IRON[3])
        put(img, x, 18, IRON[1])
    for y in range(9, 18):
        put(img, 13, y, IRON[2])
        put(img, 19, y, IRON[1])
        put(img, 16, y, IRON[1])
        for x in (14, 15, 17, 18):
            if lit:
                put(img, x, y, tone(AMBER, 5.0 - abs(y - 13.5) * 0.45 - (x in (14, 18)) * 0.8, x, y))
            else:
                put(img, x, y, tone(ROCK, 3.4 - (y - 9) * 0.2, x, y))
    for x in range(14, 19):
        put(img, x, 19, IRON[1])
    outline(img)
    return img


def lever(on):
    """A lever on a stone block, the handle to the left or, pulled, to the right."""
    img = canvas()
    shadow(img, 12, 20.5, 7, 1.6)
    for y in range(15, 21):
        for x in range(6, 18):
            put(img, x, y, ROCK[5] if y == 15 else tone(ROCK, 4.3 - (x - 6) * 0.12 - (y - 15) * 0.15, x, y))
    for x in range(6, 18):
        put(img, x, 20, ROCK[1])
    tip = (17, 6) if on else (7, 6)
    for i in range(10):
        x = round(12 + (tip[0] - 12) * i / 9)
        y = round(15 + (tip[1] - 15) * i / 9)
        put(img, x, y, WOOD[3])
        put(img, x + 1, y, WOOD[1])
    blob(img, tip[0] + 0.5, tip[1], 2, 2, RED)
    outline(img)
    return img


def gate(open_):
    """Iron bars across the way, between two stone posts; open, they are
    pulled up and only their points show. 24 × 32."""
    img = canvas(T, 32)
    for x in (1, 2, 21, 22):
        for y in range(4, 30):
            put(img, x, y, tone(ROCK, 4.6 - (x in (2, 22)) * 1.6 - y * 0.03, x, y))
    for x in range(1, 23):
        put(img, x, 3, ROCK[5])
        put(img, x, 4, ROCK[3])
        put(img, x, 5, ROCK[2])
    bottom = 13 if open_ else 29
    for x in range(4, 21, 4):
        for y in range(6, bottom):
            put(img, x, y, IRON[3])
            put(img, x + 1, y, IRON[1])
        put(img, x, bottom, IRON[2])
    for x in range(3, 22):
        put(img, x, bottom - 4 if open_ else 18, IRON[2])
    outline(img)
    return img


def puddle():
    """Still water: lighter where it mirrors the light, a darker edge."""
    img = canvas()
    for y in range(T):
        for x in range(T):
            u, v = (x + 0.5 - 12) / 9.5, (y + 0.5 - 13) / 6
            wobble = 0.12 * math.sin(x * 1.3) + 0.08 * math.cos(y * 1.7)
            d = u * u + v * v
            if d <= 1 + wobble:
                put(img, x, y, WATER[1] if d > 0.8 + wobble else tone(WATER, 2.9 - v * 0.9 - u * 0.3, x, y))
    for x in range(7, 12):
        put(img, x, 10, WATER[4])
    for x in range(13, 15):
        put(img, x, 15, WATER[3])
    return img


def trail():
    """Two paw prints, a pad and four toes each, walking north."""
    img = canvas()
    paw = (34, 20, 24, 255)
    pad = [(x, y) for y in range(4, 8) for x in range(1, 6) if (x, y) not in ((1, 7), (5, 7), (1, 4), (5, 4))]
    toes = [(x, y + dy) for x, y in ((0, 1), (2, 0), (4, 0), (6, 1)) for dy in (0, 1)]
    for ox, oy in ((3, 12), (13, 3)):
        for dx, dy in pad + toes:
            put(img, ox + dx, oy + dy, paw)
    return img


def trail_diagonal():
    """The prints pointing north-east, for the corners of a trail, each
    turned where it lies: the pad a rounded blob, the toes on an arc ahead."""
    img = canvas()
    paw = (34, 20, 24, 255)
    for ox, oy in ((7, 16), (16, 7)):
        for y in range(-4, 5):
            for x in range(-4, 5):
                across, ahead = (x - y) * 0.7071, -(x + y) * 0.7071
                if (across / 2.5) ** 2 + (ahead / 2.0) ** 2 <= 1:
                    put(img, ox + x, oy + y, paw)
        for angle in (-66, -22, 22, 66):
            t = math.radians(45 + angle)
            tx, ty = round(4.6 * math.sin(t)), round(-4.6 * math.cos(t))
            for px, py in ((tx, ty), (tx + 1, ty - 1)):
                put(img, ox + px, oy + py, paw)
    return img


# --- what grows and flies ---------------------------------------------------

def glowshroom():
    """Three glowing mushrooms at the foot of a wall."""
    img = canvas()
    for cx, h, r in ((7, 7, 3.2), (13, 10, 4.2), (18, 5, 2.6)):
        for y in range(23 - h, 23):
            put(img, cx, y, CREAM[2])
            put(img, cx + 1, y, CREAM[1])
        blob(img, cx + 0.5, 23 - h, r, r * 0.6, GLOW)
        put(img, cx - 1, 23 - h - 1, GLOW[3])
    outline(img, rgba(GLOW[0]))
    return img


def mushroom():
    """A red toadstool with white spots, and a small one beside it."""
    img = canvas()
    for cx, h, r in ((10, 8, 4.5), (16, 4, 2.6)):
        for y in range(23 - h, 23):
            put(img, cx, y, CREAM[2])
            put(img, cx + 1, y, CREAM[1])
        blob(img, cx + 0.5, 23 - h, r, r * 0.65, RED)
    for x, y in ((8, 14), (11, 13), (12, 16), (16, 18)):
        put(img, x, y, CREAM[3])
    outline(img)
    return img


def crystals():
    """Violet crystals growing out of the rock."""
    img = canvas()
    for base, h, w, lean in ((7, 11, 2, -1), (12, 15, 3, 0), (17, 9, 2, 1)):
        for i in range(h):
            y = 22 - i
            shift = round(lean * i / h * 2)
            half = w if i < h - 2 else 1 if i < h - 1 else 0
            for dx in range(-half, half + 1):
                put(img, base + shift + dx, y, VIOLET[3] if dx < 0 else VIOLET[2] if dx == 0 else VIOLET[1])
    outline(img, rgba(VIOLET[0]))
    return img


def stalagmite():
    """A pointed stone growing up from the floor at the foot of a wall."""
    img = canvas()
    for i in range(14):
        y = 22 - i
        half = max(0, round(4 * (1 - i / 14)))
        for dx in range(-half, half + 1):
            put(img, 12 + dx, y, tone(ROCK, 5.2 - (dx + half) * 0.55 - i * 0.02, 12 + dx, y))
    outline(img)
    return img


def bat(frame):
    """A small bat, the wings up, out, down and out again."""
    img = canvas()
    wing = [-4, 0, 3, 0][frame]
    for side in (-1, 1):
        for i in range(1, 9):
            x = 12 + side * (i + 1)
            y = 11 + round(wing * i / 8) + (1 if i in (3, 6) else 0)
            put(img, x, y, VIOLET[1])
            put(img, x, y + 1, VIOLET[0])
    blob(img, 12, 12, 2.5, 3, VIOLET[:3], shine=False)
    put(img, 11, 11, AMBER[4])
    put(img, 13, 11, AMBER[4])
    put(img, 10, 8, VIOLET[1])
    put(img, 14, 8, VIOLET[1])
    outline(img)
    return img


def main():
    out = {
        'floor.png': strip([floor(i) for i in range(4)]),
        'rock-top.png': strip([rock_top(i) for i in range(3)]),
        'rock-face.png': strip([rock_face(i) for i in range(4)]),
        'stone.png': strip([stone(i) for i in range(4)]),
        'glibber.png': strip([grounded(f, 8) for f in frames(load('glibber_laufen.png'))]),
        'bello.png': strip([grounded(f, 8) for f in frames(load('hund_stehen.png'))]),
        'lamp-off.png': lamp(False), 'lamp.png': lamp(True),
        'lever-off.png': lever(False), 'lever-on.png': lever(True),
        'gate-closed.png': gate(False), 'gate-open.png': gate(True),
        'puddle.png': puddle(), 'trail.png': trail(), 'trail-diagonal.png': trail_diagonal(),
        'glowshroom.png': glowshroom(), 'mushroom.png': mushroom(), 'crystals.png': crystals(),
        'stalagmite.png': stalagmite(), 'bat.png': strip([bat(i) for i in range(4)]),
        'wichtel.png': load('wichtel_stehen.png'),
    }
    for direction in ('east', 'west', 'north', 'south'):
        out[f'pip-{direction}.png'] = strip(pip_frames(direction))
    for name, img in out.items():
        img.save(HERE / name)
    if '--sheet' in sys.argv:
        rows, line = [], []
        for img in out.values():
            if line and sum(i.width + 2 for i in line) + img.width > 220:
                rows.append(line)
                line = []
            line.append(img)
        rows.append(line)
        row_h = 42
        sheet = Image.new('RGBA', (220, row_h * len(rows)), rgba(GROUND[2]))
        for r, line in enumerate(rows):
            x = 0
            for img in line:
                sheet.alpha_composite(img, (x, r * row_h + (row_h - img.height) // 2))
                x += img.width + 2
        sheet.resize((sheet.width * 4, sheet.height * 4), Image.NEAREST).save(HERE / 'sheet.png')


if __name__ == '__main__':
    main()
