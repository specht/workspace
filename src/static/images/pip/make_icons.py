#!/usr/bin/env python3
"""Pip, Bello and the Mampfer as icons for the strip of Katas on the front
page (KataMap, meta icon: of pip-1, pip-2 and pip-3): the first frame of
pip-east.png (from the side), bello.png and mampfer.png, cut to what is
drawn, in grey: each pixel's brightness becomes a shade from dark to light,
so the figure keeps its drawing. The strip tints it in the hue of its
set's band (default.css, .kata-set-sprite), which comes from the set's
place in the strip, so no hue is drawn in. Run after make_sprites.py:

    python3 make_icons.py
"""
from PIL import Image

# from near black (outlines, eyes, the Mampfer's mouth) to near white, the
# mid tones lifted (GAMMA below 1), so that the figure keeps its contrast
# once it is tinted
DARK = (22, 22, 22)
LIGHT = (250, 250, 250)
GAMMA = 0.75
# every icon in the middle of SIZE × SIZE pixels; the strip shows Pip and
# Bello two and a half times as large, every pixel of the two alike, and
# the Mampfer, drawn larger, 1.6 times, so that each is about as large as
# the other icons (default.css, .kata-set-sprite)
SIZE = 32


def icon(source, target, width):
    sheet = Image.open(source).convert('RGBA')
    frame = sheet.crop((0, 0, width, sheet.height))
    solid = frame.getchannel('A').point(lambda a: 255 if a >= 200 else 0)
    frame = frame.crop(solid.getbbox())
    out = Image.new('RGBA', frame.size)
    for y in range(frame.height):
        for x in range(frame.width):
            r, g, b, a = frame.getpixel((x, y))
            # the soft shadow under the feet stays with the sprite
            if a < 200:
                continue
            t = min(1.0, (0.299 * r + 0.587 * g + 0.114 * b) / 200) ** GAMMA
            out.putpixel((x, y), tuple(round(d + (l - d) * t) for d, l in zip(DARK, LIGHT)) + (a,))
    canvas = Image.new('RGBA', (SIZE, SIZE))
    canvas.paste(out, ((SIZE - out.width) // 2, (SIZE - out.height) // 2))
    canvas.save(target)


icon('pip-east.png', 'icon-pip.png', 24)
icon('bello.png', 'icon-bello.png', 24)
icon('mampfer.png', 'icon-mampfer.png', 32)
