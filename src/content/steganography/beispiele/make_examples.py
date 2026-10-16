#!/usr/bin/env python3
"""Makes the example picture and the comparison pictures of the
steganography activity, with the reference solution in ../loesung/stego.py.

strandhaeuser.png and moewen.png are two street photos by Ian Kobylanski
(with permission, https://koby.photography/), 480 x 320 pixels. From them:

- fundstueck.png: the beach huts with two secrets. Bit plane 1 of every
  channel shows a hint, and the lowest bits carry a message, hidden with
  hide(). The codeword in it is the answer to the task »codewort«.
- ../ebenen.webp: bit plane 0 of the beach huts, as they are, with the GIF
  specification as plain text and compressed with zlib.
- ../tiefe.webp: the sky of moewen.png with 1, 2, 4 and 6 bits per channel
  filled with data as uniform as a compressed message.
- ../klassen.webp: the same sky full at about 9 bits per pixel, once with
  RGB boxes (8 values per channel) and once with Lab classes 78 x 48 x 46.
- ../teaser.webp: the gulls with the same message hidden in them, for the
  activity's card: a photo that looks innocent. (The site scales card
  pictures, so the message does not survive there.)

The three PNGs also go into src/static/steganography/, where the page
offers them for download.

Run it from this folder (Pillow needed; it takes a few minutes, mostly
for the Lab classes). Change the message and the task changes with it.
"""
import os
import random
import sys
import shutil
import zlib

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'loesung'))
from stego import (BoxClasses, HEADER, LabClasses, bit_plane, capacity, channels,  # noqa: E402
                   embed, from_channels, hide)

MESSAGE = ('Gut gefunden! Wer das liest, hat in die untersten Bits geschaut. '
           'Das Codewort ist STRANDKORB.')
SPEC = os.path.join(HERE, '..', '..', '..', 'static', 'lzw', 'spec-gif89a.txt')
STATIC = os.path.join(HERE, '..', '..', '..', 'static', 'steganography')
FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'
LAB = (78, 48, 46)


def fundstueck():
    huts = Image.open(os.path.join(HERE, 'strandhaeuser.png')).convert('RGB')
    hint = Image.new('L', huts.size, 0)
    draw = ImageDraw.Draw(hint)
    font = ImageFont.truetype(FONT, 30)
    # In the right half, so the card's picture (teaser) shows all of it
    for line, y in (('Hier nicht.', 70), ('Schau ins', 130), ('unterste Bit:', 170), ('reveal(bild)', 230)):
        draw.text((262, y), line, fill=255, font=font)
    # Bit 1 of every channel is the hint: 1 where it is white.
    mask = list(hint.getdata())
    values = channels(huts)
    values = [(v & ~2) | (2 if mask[i // 3] else 0) for i, v in enumerate(values)]
    found = hide(from_channels(huts.width, huts.height, values), MESSAGE)
    found.save(os.path.join(HERE, 'fundstueck.png'))


def panels(pictures, scale=1, gap=8):
    """Pictures side by side on white, each scaled with sharp pixels."""
    pictures = [p.resize((p.width * scale, p.height * scale), Image.NEAREST) for p in pictures]
    width = sum(p.width for p in pictures) + gap * (len(pictures) - 1)
    out = Image.new('RGB', (width, max(p.height for p in pictures)), 'white')
    x = 0
    for p in pictures:
        out.paste(p, (x, 0))
        x += p.width + gap
    return out


def ebenen():
    huts = Image.open(os.path.join(HERE, 'strandhaeuser.png')).convert('RGB')
    spec = open(SPEC, 'rb').read()
    room = huts.width * huts.height * 3 // 8 - 4
    plain = hide(huts, spec[:room])
    packed = hide(huts, zlib.compress(spec, 9))
    crop = (0, 0, 240, 160)
    out = panels([huts.crop(crop), bit_plane(huts).crop(crop), bit_plane(plain).crop(crop), bit_plane(packed).crop(crop)], scale=2, gap=16)
    out.save(os.path.join(HERE, '..', 'ebenen.webp'), lossless=True)


def full(image, depth, rng):
    """So viele zufällige Bytes, wie in depth Bits pro Kanal passen."""
    room = image.width * image.height * 3 * depth // 8 - 4
    return hide(image, rng.randbytes(room), depth)


def tiefe():
    sky = Image.open(os.path.join(HERE, 'moewen.png')).convert('RGB').crop((240, 40, 400, 147))
    rng = random.Random(1)
    out = panels([full(sky, depth, rng) for depth in (1, 2, 4, 6)], scale=3, gap=16)
    out.save(os.path.join(HERE, '..', 'tiefe.webp'), lossless=True)


def klassen():
    sky = Image.open(os.path.join(HERE, 'moewen.png')).convert('RGB').crop((0, 0, 240, 160))
    rng = random.Random(2)
    box = BoxClasses(8)
    lab = LabClasses(*LAB)
    lab.prepare(sky)
    filled = []
    for classes in (box, lab):
        room = int(capacity(sky, classes) / 8) - 8
        filled.append(embed(sky, rng.randbytes(room), classes))
        n = sky.width * sky.height - HEADER
        print(f'{type(classes).__name__}: {capacity(sky, classes) / n:.2f} bits per pixel')
    crop = (100, 60, 240, 160)
    out = panels([sky.crop(crop), filled[0].crop(crop), filled[1].crop(crop)], scale=4, gap=24)
    out.save(os.path.join(HERE, '..', 'klassen.webp'), lossless=True)


def teaser():
    gulls = Image.open(os.path.join(HERE, 'moewen.png')).convert('RGB')
    hide(gulls, MESSAGE).save(os.path.join(HERE, '..', 'teaser.webp'), lossless=True)


def publish():
    os.makedirs(STATIC, exist_ok=True)
    for name in ('fundstueck.png', 'strandhaeuser.png', 'moewen.png'):
        shutil.copy(os.path.join(HERE, name), os.path.join(STATIC, name))


if __name__ == '__main__':
    fundstueck()
    teaser()
    publish()
    ebenen()
    tiefe()
    klassen()
