# The card's picture of »Eine Folie, ein Gedanke«: the good slide thrown
# onto a wall seen at an angle (in perspective: its near edge tall, its far
# edge shorter), by a beamer outside the picture at the bottom left whose
# light widens to the image's four corners (a frustum, as in a cinema), on
# the orange ground of the presentations (office-drawings.py, card_ground).
# Run in the drawings image from the activity's folder:
#
#     src/scripts/drawings sh -c 'cd src/content/good-slides && python3 bilder/teaser.py'
import io
import subprocess

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

W, H = 1600, 900
GROUND = ((246, 214, 193), (238, 189, 159))       # light to deep, top left to bottom right
LIGHT = (255, 250, 243)
LENS = (-260, 1060)                                # the beamer, outside the picture
# the slide on the wall: top left, top right, bottom right, bottom left
QUAD = [(560, 120), (1480, 230), (1480, 690), (560, 800)]


def coefficients(quad, size):
    """the perspective transform that maps the slide's rectangle onto quad
    (PIL wants the inverse: for each pixel of the picture, where in the slide)"""
    w, h = size
    rect = [(0, 0), (w, 0), (w, h), (0, h)]
    rows, rhs = [], []
    for (x, y), (u, v) in zip(quad, rect):
        rows.append([x, y, 1, 0, 0, 0, -u * x, -u * y])
        rows.append([0, 0, 0, x, y, 1, -v * x, -v * y])
        rhs += [u, v]
    return np.linalg.solve(np.array(rows, float), np.array(rhs, float)).tolist()


# the ground: a gradient from the top left to the bottom right
yy, xx = np.mgrid[0:H, 0:W]
t = ((xx / W + yy / H) / 2)[..., None]
ground = (np.array(GROUND[0]) * (1 - t) + np.array(GROUND[1]) * t).astype(np.uint8)
picture = Image.fromarray(ground, 'RGB').convert('RGBA')

# the beam: from the lens to the four corners, brightest at the lens
beam = Image.new('L', (W, H), 0)
ImageDraw.Draw(beam).polygon([LENS] + QUAD, fill=255)
cx, cy = sum(p[0] for p in QUAD) / 4, sum(p[1] for p in QUAD) / 4
d = np.hypot(xx - LENS[0], yy - LENS[1]) / np.hypot(cx - LENS[0], cy - LENS[1])
strength = np.clip(1.0 - 0.55 * d, 0.35, 1.0)
alpha = (np.array(beam, float) / 255 * strength * 210).astype(np.uint8)
light = Image.new('RGBA', (W, H), LIGHT + (0,))
light.putalpha(Image.fromarray(alpha, 'L').filter(ImageFilter.GaussianBlur(2)))
picture = Image.alpha_composite(picture, light)
edges = Image.new('RGBA', (W, H), (0, 0, 0, 0))
draw = ImageDraw.Draw(edges)
for corner in QUAD:
    draw.line([LENS, corner], fill=LIGHT + (200,), width=2)
picture = Image.alpha_composite(picture, edges)

# the slide, warped onto the wall, with a soft shadow
png = subprocess.run(['rsvg-convert', '-b', 'white', '-w', '1600', 'bilder/zug.svg'], capture_output=True, check=True).stdout
slide = Image.open(io.BytesIO(png)).convert('RGBA')
warped = slide.transform((W, H), Image.PERSPECTIVE, coefficients(QUAD, slide.size), Image.BICUBIC)
mask = Image.new('L', (W, H), 0)
ImageDraw.Draw(mask).polygon(QUAD, fill=255)
shadow = Image.new('RGBA', (W, H), (40, 20, 10, 0))
shadow.putalpha(mask.filter(ImageFilter.GaussianBlur(14)).point(lambda v: v * 0.25))
picture = Image.alpha_composite(picture, shadow.transform((W, H), Image.AFFINE, (1, 0, -6, 0, 1, -10)))
warped.putalpha(mask)
picture = Image.alpha_composite(picture, warped)
ImageDraw.Draw(picture).line(QUAD + [QUAD[0]], fill=(214, 186, 164, 255), width=2)

picture.convert('RGB').save('/tmp/teaser.png')
subprocess.run(['cwebp', '-quiet', '-q', '90', '/tmp/teaser.png', '-o', 'teaser.webp'], check=True)
