#!/usr/bin/env python3
"""Pip and Bello as icons for the strip of Katas on the front page
(KataMap, meta icon: of pip-1 and pip-2): the first frame of pip-east.png (from the side)
and bello.png, cut to what is drawn, in amber like the band of Pip's sets
(default.css, .kata-set-art): each pixel's brightness becomes a shade of
amber, so the figure keeps its drawing. Run after make_sprites.py:

    python3 make_icons.py
"""
from PIL import Image

# the lantern amber of Pip's sets (meta accent: 38); default.css turns a
# sprite to another set's hue from there
DARK = (72, 46, 12)
LIGHT = (255, 205, 120)
SIZE = 24


def icon(source, target):
    sheet = Image.open(source).convert('RGBA')
    frame = sheet.crop((0, 0, sheet.height, sheet.height))
    solid = frame.getchannel('A').point(lambda a: 255 if a >= 200 else 0)
    frame = frame.crop(solid.getbbox())
    out = Image.new('RGBA', frame.size)
    for y in range(frame.height):
        for x in range(frame.width):
            r, g, b, a = frame.getpixel((x, y))
            # the soft shadow under the feet stays with the sprite
            if a < 200:
                continue
            t = min(1.0, (0.299 * r + 0.587 * g + 0.114 * b) / 200)
            out.putpixel((x, y), tuple(round(d + (l - d) * t) for d, l in zip(DARK, LIGHT)) + (a,))
    # in the middle of a frame of SIZE pixels, so that every icon is shown
    # at the same whole multiple (default.css, .kata-set-sprite)
    canvas = Image.new('RGBA', (SIZE, SIZE))
    canvas.paste(out, ((SIZE - out.width) // 2, (SIZE - out.height) // 2))
    canvas.save(target)


icon('pip-east.png', 'icon-pip.png')
icon('bello.png', 'icon-bello.png')
