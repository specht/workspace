# The card's picture of »Eine Folie, ein Gedanke«: a small scene in space,
# seen through a camera. The good slide is thrown onto a wall (a 16:9
# rectangle on the plane z = 0) by a beamer in front of it, outside the
# picture; its light is a pyramid with the lens as apex and four faces to
# the slide's four edges (a frustum, as in a cinema), each face a little
# brighter or darker, on the orange ground of the presentations
# (office-drawings.py, card_ground). Run in the drawings image from the
# activity's folder:
#
#     src/scripts/drawings sh -c 'cd src/content/good-slides && python3 bilder/teaser.py'
import io
import subprocess

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

W, H = 1600, 900
GROUND = ((246, 214, 193), (238, 189, 159))       # light to deep, top left to bottom right
LIGHT = (255, 250, 243)

# the scene, in metres: x to the right along the wall, y up, z out of the wall
SLIDE = [(-1.6, 2.4, 0), (1.6, 2.4, 0), (1.6, 0.6, 0), (-1.6, 0.6, 0)]   # top left, top right, bottom right, bottom left
LENS = tuple(float(v) for v in __import__('os').environ.get('LENS', '0.4,1.1,4.2').split(','))   # the beamer, in front of the wall
EYE = tuple(float(v) for v in __import__('os').environ.get('EYE', '-4.5,3.0,4.5').split(','))     # the camera, to the left and above
TARGET = tuple(float(v) for v in __import__('os').environ.get('TARGET', '0.6,1.4,1.4').split(','))
FOCAL = float(__import__('os').environ.get('FOCAL', '1500'))   # pixels


def camera(eye, target):
    eye, target = np.array(eye, float), np.array(target, float)
    forward = target - eye
    forward /= np.linalg.norm(forward)
    right = np.cross(forward, (0, 1, 0))
    right /= np.linalg.norm(right)
    up = np.cross(right, forward)
    return eye, right, up, forward


EYE_, RIGHT, UP, FORWARD = camera(EYE, TARGET)


def project(point):
    d = np.array(point, float) - EYE_
    x, y, z = d @ RIGHT, d @ UP, d @ FORWARD
    return (W / 2 + FOCAL * x / z, H / 2 - FOCAL * y / z)


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


quad = [project(p) for p in SLIDE]
lens = project(LENS)

# the ground: a gradient from the top left to the bottom right
yy, xx = np.mgrid[0:H, 0:W]
t = ((xx / W + yy / H) / 2)[..., None]
ground = (np.array(GROUND[0]) * (1 - t) + np.array(GROUND[1]) * t).astype(np.uint8)
picture = Image.fromarray(ground, 'RGB').convert('RGBA')

# the four faces of the light, each its own brightness (top, right, bottom, left)
for (a, b), strength in zip([(0, 1), (1, 2), (2, 3), (3, 0)], [0.30, 0.22, 0.16, 0.38]):
    face = Image.new('L', (W, H), 0)
    ImageDraw.Draw(face).polygon([lens, quad[a], quad[b]], fill=int(255 * strength))
    light = Image.new('RGBA', (W, H), LIGHT + (0,))
    light.putalpha(face.filter(ImageFilter.GaussianBlur(1.5)))
    picture = Image.alpha_composite(picture, light)
edges = Image.new('RGBA', (W, H), (0, 0, 0, 0))
draw = ImageDraw.Draw(edges)
for corner in quad:
    draw.line([lens, corner], fill=LIGHT + (190,), width=2)
picture = Image.alpha_composite(picture, edges)

# the slide, the beamer's image on the wall
png = subprocess.run(['rsvg-convert', '-b', 'white', '-w', '1600', 'bilder/zug.svg'], capture_output=True, check=True).stdout
slide = Image.open(io.BytesIO(png)).convert('RGBA')
warped = slide.transform((W, H), Image.PERSPECTIVE, coefficients(quad, slide.size), Image.BICUBIC)
mask = Image.new('L', (W, H), 0)
ImageDraw.Draw(mask).polygon(quad, fill=255)
warped.putalpha(mask)
picture = Image.alpha_composite(picture, warped)
ImageDraw.Draw(picture).line(quad + [quad[0]], fill=(214, 186, 164, 255), width=2)

picture.convert('RGB').save('/tmp/teaser.png')
subprocess.run(['cwebp', '-quiet', '-q', '90', '/tmp/teaser.png', '-o', __import__('os').environ.get('OUT', 'teaser.webp')], check=True)
print('slide', [tuple(round(v) for v in p) for p in quad], 'lens', tuple(round(v) for v in lens))
