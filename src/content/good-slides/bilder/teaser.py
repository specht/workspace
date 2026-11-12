# The card's picture of »Eine Folie, ein Gedanke«: a good slide, a picture
# and a short title (cranes in a wedge over the lake at dusk, drawn here),
# thrown onto a wall by a beamer hanging from the ceiling outside the
# picture: a small scene in space seen through a camera almost straight in
# front, so the slide stands front and centre with a slight perspective. The
# light is a pyramid with the lens as apex and four faces to the slide's four
# edges (a frustum, as in a cinema), each face a little brighter or darker,
# on the orange ground of the presentations (office-drawings.py,
# card_ground). Run in the drawings image from the activity's folder:
#
#     src/scripts/drawings sh -c 'cd src/content/good-slides && python3 bilder/teaser.py'
import io
import os
import subprocess

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

W, H = 1600, 900
GROUND = ((246, 214, 193), (238, 189, 159))       # light to deep, top left to bottom right
LIGHT = (255, 250, 243)
env = lambda name, default: tuple(float(v) for v in os.environ.get(name, default).split(','))

# the scene, in metres: x to the right along the wall, y up, z out of the wall
SLIDE = [(-1.6, 2.5, 0), (1.6, 2.5, 0), (1.6, 0.7, 0), (-1.6, 0.7, 0)]   # top left, top right, bottom right, bottom left
LENS = env('LENS', '0.0,4.6,3.2')                  # the beamer, hanging from the ceiling in front of the wall
EYE = env('EYE', '0.9,1.75,6.2')                   # the camera, nearly straight in front
TARGET = env('TARGET', '0.0,1.75,0')
FOCAL = float(os.environ.get('FOCAL', '1850'))     # pixels


def slide_svg():
    """the good slide: a short title over a large picture, drawn"""
    birds = []
    # a wedge of cranes flying to the left, the leader in front: a long neck
    # forward, long legs trailing behind, broad wings
    crane = ("<path d='M -26 0 L 30 3' stroke-width='2.4' fill='none'/>"
             "<circle cx='-26' cy='0' r='2.2'/>"
             "<path d='M -6 0 C -2 -6 8 -16 18 -22 C 14 -12 10 -4 8 1 Z'/>"
             "<path d='M -4 1 C 0 5 6 9 10 11 C 8 6 6 3 6 2 Z' opacity='0.85'/>")
    for i, (bx, by) in enumerate([(640, 330), (710, 362), (780, 394), (850, 426), (920, 458),
                                  (712, 300), (784, 270), (856, 240), (928, 210)]):
        birds.append(f"<g transform='translate({bx} {by}) scale({1.25 - i * 0.025:.2f})' fill='#2b2433' stroke='#2b2433' "
                     f"stroke-linecap='round'>{crane}</g>")
    return f"""<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 1600 900' width='1600' height='900'>
<defs>
  <linearGradient id='sky' x1='0' y1='0' x2='0' y2='1'>
    <stop offset='0' stop-color='#3d3a6b'/><stop offset='0.55' stop-color='#c8708a'/><stop offset='1' stop-color='#f3b26b'/></linearGradient>
  <linearGradient id='lake' x1='0' y1='0' x2='0' y2='1'><stop offset='0' stop-color='#e7a066'/><stop offset='1' stop-color='#6b5a7a'/></linearGradient>
  <clipPath id='frame'><rect x='120' y='170' width='1360' height='660' rx='14'/></clipPath>
</defs>
<rect width='1600' height='900' fill='#ffffff'/>
<text x='800' y='112' text-anchor='middle' font-family='IBM Plex Sans, sans-serif' font-size='62' font-weight='600' fill='#1b1f24'>Kraniche ziehen im Herbst nach Süden</text>
<g clip-path='url(#frame)'>
  <rect x='120' y='170' width='1360' height='660' fill='url(#sky)'/>
  <circle cx='1180' cy='640' r='70' fill='#ffd79a'/>
  <rect x='120' y='660' width='1360' height='170' fill='url(#lake)'/>
  <path d='M 120 660 Q 360 620 620 650 T 1060 640 T 1480 650 L 1480 676 L 120 676 Z' fill='#4a3b52'/>
  <rect x='1060' y='690' width='240' height='6' rx='3' fill='#ffd79a' opacity='0.6'/>
  <rect x='1100' y='712' width='160' height='5' rx='2.5' fill='#ffd79a' opacity='0.45'/>
  {''.join(birds)}
</g>
</svg>"""


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
for (a, b), strength in zip([(0, 1), (1, 2), (2, 3), (3, 0)], [0.14, 0.26, 0.34, 0.22]):
    face = Image.new('L', (W, H), 0)
    ImageDraw.Draw(face).polygon([lens, quad[a], quad[b]], fill=int(255 * strength))
    light = Image.new('RGBA', (W, H), LIGHT + (0,))
    light.putalpha(face.filter(ImageFilter.GaussianBlur(1.5)))
    picture = Image.alpha_composite(picture, light)
edges = Image.new('RGBA', (W, H), (0, 0, 0, 0))
draw = ImageDraw.Draw(edges)
for corner in quad:
    draw.line([lens, corner], fill=LIGHT + (170,), width=2)
picture = Image.alpha_composite(picture, edges)

# the slide, the beamer's image on the wall
svg = slide_svg()
png = subprocess.run(['rsvg-convert', '-w', '1600'], input=svg.encode(), capture_output=True, check=True).stdout
slide = Image.open(io.BytesIO(png)).convert('RGBA')
warped = slide.transform((W, H), Image.PERSPECTIVE, coefficients(quad, slide.size), Image.BICUBIC)
mask = Image.new('L', (W, H), 0)
ImageDraw.Draw(mask).polygon(quad, fill=255)
warped.putalpha(mask)
picture = Image.alpha_composite(picture, warped)
ImageDraw.Draw(picture).line(quad + [quad[0]], fill=(214, 186, 164, 255), width=2)

picture.convert('RGB').save('/tmp/teaser.png')
subprocess.run(['cwebp', '-quiet', '-q', '90', '/tmp/teaser.png', '-o', os.environ.get('OUT', 'teaser.webp')], check=True)
print('slide', [tuple(round(v) for v in p) for p in quad], 'lens', tuple(round(v) for v in lens))
