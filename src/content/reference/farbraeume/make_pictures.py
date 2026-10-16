#!/usr/bin/env python3
"""The pictures of the Handbuch page »Farbräume«: the photo (photo.webp) and
its three channels in RGB, YCbCr, HSV and CIELab, each channel a grey
picture of its own (rgb-r.webp … lab-b.webp; the page puts each space in a
gallery and says there what dark and light mean). And the chromaticity
diagram of CIE 1931 with the triangle of sRGB (normfarbtafel.webp).

The photo is strandhaeuser.png from »Geheime Botschaften in Bildern« (Ian
Kobylanski, with permission, https://koby.photography/). Run from this folder; Pillow and numpy needed.
`make_pictures.py normfarbtafel` makes only the diagram, `channels` only the channels.
"""
import colorsys
import math
import os
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
PHOTO = os.path.join(HERE, '..', '..', 'steganography', 'beispiele', 'strandhaeuser.png')
FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'


def linear(v):
    c = v / 255
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


LINEAR = [linear(v) for v in range(256)]


def f(t):
    return t ** (1 / 3) if t > 216 / 24389 else (24389 / 27 * t + 16) / 116


def lab(r, g, b):
    r, g, b = LINEAR[r], LINEAR[g], LINEAR[b]
    x = (0.4124 * r + 0.3576 * g + 0.1805 * b) / 0.95047
    y = 0.2126 * r + 0.7152 * g + 0.0722 * b
    z = (0.0193 * r + 0.1192 * g + 0.9505 * b) / 1.08883
    fx, fy, fz = f(x), f(y), f(z)
    return 116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)


def clamp(v):
    return max(0, min(255, round(v)))


# Each colour space: its channels, and how a pixel becomes three greys.
# a and b of Lab and Cb and Cr are around 0 (or 128): grey is »no colour«.
def rgb_channels(p):
    return p


def ycbcr_channels(p):
    r, g, b = p
    y = 0.299 * r + 0.587 * g + 0.114 * b
    return y, 128 - 0.168736 * r - 0.331264 * g + 0.5 * b, 128 + 0.5 * r - 0.418688 * g - 0.081312 * b


def hsv_channels(p):
    h, s, v = colorsys.rgb_to_hsv(*(c / 255 for c in p))
    return h * 255, s * 255, v * 255


def lab_channels(p):
    l, a, b = lab(*p)
    return l * 2.55, 128 + a * 1.4, 128 + b * 1.4


# The channels of each space, as the files name them
SPACES = {
    'rgb': (rgb_channels, ['r', 'g', 'b']),
    'ycbcr': (ycbcr_channels, ['y', 'cb', 'cr']),
    'hsv': (hsv_channels, ['h', 's', 'v']),
    'lab': (lab_channels, ['l', 'a', 'b']),
}


def channel_pictures(photo, convert):
    """The three channels of the photo in one colour space, as grey pictures."""
    w, h = photo.size
    planes = [Image.new('L', (w, h)) for _ in range(3)]
    data = [p.load() for p in planes]
    src = photo.load()
    for y in range(h):
        for x in range(w):
            for i, v in enumerate(convert(src[x, y])):
                data[i][x, y] = clamp(v)
    return planes


# --- The chromaticity diagram -----------------------------------------------
#
# The colour matching functions of CIE 1931 as the sums of Gaussians fitted by
# Wyman, Sloan and Shirley (2013), close enough for a picture from 420 to
# 640 nm. Outside, the fit drifts while the true edge hardly moves; there
# the ends come from the table of CIE 1931, at 380 and 700 nm.
ENDS = [(0.1741, 0.0050), (0.7347, 0.2653)]

def lobe(wl, mu, low, high):
    s = low if wl < mu else high
    return math.exp(-0.5 * ((wl - mu) / s) ** 2)


def xyz_bar(wl):
    x = 1.056 * lobe(wl, 599.8, 37.9, 31.0) + 0.362 * lobe(wl, 442.0, 16.0, 26.7) - 0.065 * lobe(wl, 501.1, 20.4, 26.2)
    y = 0.821 * lobe(wl, 568.8, 46.9, 40.5) + 0.286 * lobe(wl, 530.9, 16.3, 31.1)
    z = 1.217 * lobe(wl, 437.0, 11.8, 36.0) + 0.681 * lobe(wl, 459.0, 26.0, 13.8)
    return x, y, z


def chromaticity(wl):
    x, y, z = xyz_bar(wl)
    return x / (x + y + z), y / (x + y + z)


XYZ_TO_RGB = np.array([[3.2406, -1.5372, -0.4986], [-0.9689, 1.8758, 0.0415], [0.0557, -0.2040, 1.0570]])
SRGB = {'R': (0.64, 0.33), 'G': (0.30, 0.60), 'B': (0.15, 0.06)}
WHITE = (0.3127, 0.3290)
LEFT = 0.08   # room left of x = 0 for the wavelengths


def diagram(size=960, factor=2):
    """Every colour a human sees, by its share x and y of X and Y: inside the
    horseshoe, with the pure colours of the spectrum on its edge."""
    s = size * factor
    u = round(size / 480 * factor)   # one pixel of the picture as the page shows it, 30em wide
    margin_l, margin_b, margin_t, margin_r = 50 * u, 60 * u, 34 * u, 20 * u
    plot = s - margin_l - margin_r
    to_px = lambda x, y: (margin_l + (x + LEFT) / (0.8 + LEFT) * plot, s - margin_b - y / 0.9 * (s - margin_b - margin_t))
    locus = [ENDS[0]] + [chromaticity(wl) for wl in range(420, 641)] + [ENDS[1]]

    # the colours inside, as close as the screen gets: outside the triangle
    # of sRGB, white is mixed in until no channel is negative
    xs, ys = np.meshgrid(np.linspace(-LEFT, 0.8, plot), np.linspace(0.9, 0, s - margin_b - margin_t))
    ys = np.maximum(ys, 1e-6)
    xyz = np.stack([xs / ys, np.ones_like(xs), (1 - xs - ys) / ys], axis=-1)
    rgb = xyz @ XYZ_TO_RGB.T
    rgb -= np.minimum(rgb.min(axis=-1, keepdims=True), 0)
    rgb /= np.maximum(rgb.max(axis=-1, keepdims=True), 1e-9)
    encoded = np.where(rgb <= 0.0031308, 12.92 * rgb, 1.055 * np.power(rgb, 1 / 2.4) - 0.055)
    fill = Image.fromarray((np.clip(encoded, 0, 1) * 255).astype(np.uint8), 'RGB')
    mask = Image.new('L', (s, s), 0)
    ImageDraw.Draw(mask).polygon([to_px(*p) for p in locus], fill=255)
    out = Image.new('RGB', (s, s), 'white')
    canvas = Image.new('RGB', (s, s), 'white')
    canvas.paste(fill, (margin_l, margin_t))
    out.paste(canvas, (0, 0), mask)

    draw = ImageDraw.Draw(out)
    font = ImageFont.truetype(FONT, 15 * u)
    small = ImageFont.truetype(FONT, 13 * u)
    ink, faint = (40, 40, 40), (200, 200, 200)
    # axes with a grid every 0.1
    for i in range(9):
        x0, y0 = to_px(i / 10, 0)
        draw.line([to_px(i / 10, 0), to_px(i / 10, 0.9)], fill=faint, width=u)
        draw.text((x0, y0 + 8 * u), f'{i / 10:.1f}'.replace('.', ','), fill=ink, font=small, anchor='mt')
    for i in range(10):
        x0, y0 = to_px(-LEFT, i / 10)
        draw.line([to_px(-LEFT, i / 10), to_px(0.8, i / 10)], fill=faint, width=u)
        draw.text((x0 - 8 * u, y0), f'{i / 10:.1f}'.replace('.', ','), fill=ink, font=small, anchor='rm')
    draw.text(to_px(0.8, -0.075), 'x', fill=ink, font=font, anchor='rm')
    x0, y0 = to_px(-LEFT, 0.9)
    draw.text((x0 - 8 * u, y0 - 12 * u), 'y', fill=ink, font=font, anchor='rb')
    # the horseshoe again on top, its edge and the wavelengths
    out.paste(canvas, (0, 0), mask)
    draw.line([to_px(*p) for p in locus] + [to_px(*locus[0])], fill=ink, width=2 * u)
    for wl in (460, 480, 490, 500, 510, 520, 540, 560, 580, 600, 620):
        x, y = chromaticity(wl)
        dx, dy = chromaticity(wl + 1)
        nx, ny = -(dy - y), dx - x   # outwards: with the wavelength, the edge runs clockwise
        length = math.hypot(nx, ny)
        nx, ny = nx / length, ny / length
        p0 = to_px(x, y)
        p1 = to_px(x + nx * 0.012, y + ny * 0.012)
        p2 = to_px(x + nx * 0.03, y + ny * 0.03)
        draw.line([p0, p1], fill=ink, width=2 * u)
        draw.text(p2, f'{wl}', fill=ink, font=small, anchor='rm' if nx < -0.3 else 'lm' if nx > 0.3 else 'mb' if ny > 0 else 'mt')
    # the triangle of sRGB and its white
    corners = [to_px(*SRGB[c]) for c in 'RGB']
    draw.line(corners + [corners[0]], fill=(255, 255, 255), width=5 * u)
    draw.line(corners + [corners[0]], fill=ink, width=2 * u)
    for name, (x, y) in SRGB.items():
        off = {'R': (0.0, -0.035), 'G': (-0.025, 0.015), 'B': (0.03, -0.01)}[name]
        draw.text(to_px(x + off[0], y + off[1]), name, fill=ink, font=font, anchor='mm')
    wx, wy = to_px(*WHITE)
    r = 4 * u
    draw.ellipse([wx - r, wy - r, wx + r, wy + r], fill='white', outline=ink, width=u)
    draw.text(to_px(0.8, 0.86), 'Dreieck: was ein sRGB-Bildschirm zeigt', fill=ink, font=small, anchor='rm')
    draw.text(to_px(0.8, 0.82), 'am Rand: Wellenlänge in nm', fill=ink, font=small, anchor='rm')
    return out.resize((size, size), Image.LANCZOS)


if __name__ == '__main__':
    pictures = sys.argv[1:] or ['channels', 'normfarbtafel']
    if 'normfarbtafel' in pictures:
        diagram().save(os.path.join(HERE, 'normfarbtafel.webp'), quality=90)
        print('normfarbtafel')
    if 'channels' not in pictures:
        sys.exit()
    photo = Image.open(PHOTO).convert('RGB')
    photo.save(os.path.join(HERE, 'photo.webp'), quality=90)
    for space, (convert, names) in SPACES.items():
        for name, plane in zip(names, channel_pictures(photo, convert)):
            plane.save(os.path.join(HERE, f'{space}-{name}.webp'), quality=90)
        print(space)
