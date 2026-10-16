#!/usr/bin/env python3
"""The channel pictures of the Handbuch page »Farbräume«: one photo taken
apart into the three channels of RGB, YCbCr, HSV and CIELab, each channel
as a grey picture, with a label below.

The photo is strandhaeuser.png from »Geheime Botschaften in Bildern« (Ian
Kobylanski, with permission, https://koby.photography/). The RGB and Lab
pictures also go into src/content/steganography/, where the section on
CIELab shows them. Run from this folder; Pillow needed.
"""
import colorsys
import math
import os

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
PHOTO = os.path.join(HERE, '..', '..', 'steganography', 'beispiele', 'strandhaeuser.png')
STEGO = os.path.join(HERE, '..', '..', 'steganography')
FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
SCALE = 0.75   # panels of 360 × 240 pixels


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


# Labels from dark to light: »Cb: gelb … blau« is dark where it is yellow.
SPACES = {
    'rgb': (rgb_channels, ['R: wenig … viel Rot', 'G: wenig … viel Grün', 'B: wenig … viel Blau']),
    'ycbcr': (ycbcr_channels, ['Y: dunkel … hell', 'Cb: gelb … blau', 'Cr: grün … rot']),
    'hsv': (hsv_channels, ['H: Farbton, 0° … 360°', 'S: grau … kräftig', 'V: dunkel … hell']),
    'lab': (lab_channels, ['L: dunkel … hell', 'a: grün … rot', 'b: blau … gelb']),
}


def picture(photo, convert, labels, with_photo=True):
    w, h = photo.size
    planes = [Image.new('L', (w, h)) for _ in range(3)]
    data = [p.load() for p in planes]
    src = photo.load()
    for y in range(h):
        for x in range(w):
            for i, v in enumerate(convert(src[x, y])):
                data[i][x, y] = clamp(v)
    panels = ([photo] if with_photo else []) + [p.convert('RGB') for p in planes]
    names = (['Foto'] if with_photo else []) + labels
    pw, ph = int(w * SCALE), int(h * SCALE)
    gap, caption = 10, 30
    out = Image.new('RGB', (len(panels) * pw + (len(panels) - 1) * gap, ph + caption), 'white')
    draw = ImageDraw.Draw(out)
    font = ImageFont.truetype(FONT, 16)
    for i, (panel, name) in enumerate(zip(panels, names)):
        x = i * (pw + gap)
        out.paste(panel.resize((pw, ph), Image.LANCZOS), (x, 0))
        tw = draw.textlength(name, font=font)
        draw.text((x + (pw - tw) / 2, ph + 6), name, fill=(40, 40, 40), font=font)
    return out


if __name__ == '__main__':
    photo = Image.open(PHOTO).convert('RGB')
    for name, (convert, labels) in SPACES.items():
        out = picture(photo, convert, labels)
        out.save(os.path.join(HERE, f'kanaele-{name}.webp'), quality=90)
        if name in ('rgb', 'lab'):
            out.save(os.path.join(STEGO, f'kanaele-{name}.webp'), quality=90)
        print(name, out.size)
