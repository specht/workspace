#!/usr/bin/env python3
"""Makes the example picture and the comparison pictures of the
steganography activity, with the reference solution in ../loesung/stego.py.

strandhaeuser.png and moewen.png are two street photos by Ian Kobylanski
(with permission, https://koby.photography/), 480 x 320 pixels;
moewen-ganz.jpg is the whole photo of the gulls, 1000 pixels wide. From them:

- fundstueck.png: the beach huts with two secrets. Bit plane 1 of every
  channel shows a hint, and the lowest bits carry a message, hidden with
  hide(). The codeword in it is the answer to the task »codewort«.
- fundstueck-2.png: the gulls with a message hidden in colour classes of
  width 5, the start of the GIF specification after a codeword (the answer
  to the task »fundstueck-2«). The pair test misses it, the test for groups
  of five finds it.
- ../plane-*.webp: a corner of the beach huts and its bit plane 0, as it
  is, with the GIF specification as plain text and compressed with zlib.
- ../depth-*.webp: the sky of moewen.png with 1, 2, 4 and 6 bits per
  channel filled with data as uniform as a compressed message.
- ../teaser.webp: for the activity's card, a photo that looks innocent: the
  man with the gulls on the pier, cut to 16:9 like the card from
  moewen-ganz.jpg, the whole photo (moewen.png is a crop of its sky).

The four PNGs also go into src/static/steganography/, where the page
offers them for download.

Run it from this folder (Pillow needed). Change the message and the task
changes with it.
"""
import os
import random
import sys
import shutil
import zlib

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', 'loesung'))
from stego import BoxClasses, bit_plane, channels, embed, from_channels, hide  # noqa: E402

MESSAGE = ('Gut gefunden! Wer das liest, hat in die untersten Bits geschaut. '
           'Das Codewort ist STRANDKORB.')
MESSAGE_2 = ('Gut gesucht! Mit Paaren findet der Test hier nichts, mit Gruppen von fünf schon. '
             'Das Codewort ist LEUCHTTURM.\n\n')
SPEC = os.path.join(HERE, '..', '..', '..', 'static', 'lzw', 'spec-gif89a.txt')
STATIC = os.path.join(HERE, '..', '..', '..', 'static', 'steganography')
FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf'


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


def save_scaled(picture, scale, name):
    """A picture for a gallery of the page, scaled with sharp pixels."""
    picture.resize((picture.width * scale, picture.height * scale), Image.NEAREST).save(
        os.path.join(HERE, '..', name), lossless=True)


def planes():
    huts = Image.open(os.path.join(HERE, 'strandhaeuser.png')).convert('RGB')
    spec = open(SPEC, 'rb').read()
    room = huts.width * huts.height * 3 // 8 - 4
    plain = hide(huts, spec[:room])
    packed = hide(huts, zlib.compress(spec, 9))
    crop = (0, 0, 240, 160)
    for name, picture in [('photo', huts), ('clean', bit_plane(huts)), ('text', bit_plane(plain)), ('packed', bit_plane(packed))]:
        save_scaled(picture.crop(crop), 2, f'plane-{name}.webp')


def full(image, depth, rng):
    """So viele zufällige Bytes, wie in depth Bits pro Kanal passen."""
    room = image.width * image.height * 3 * depth // 8 - 4
    return hide(image, rng.randbytes(room), depth)


def depths():
    sky = Image.open(os.path.join(HERE, 'moewen.png')).convert('RGB').crop((240, 40, 400, 147))
    rng = random.Random(1)
    for depth in (1, 2, 4, 6):
        save_scaled(full(sky, depth, rng), 3, f'depth-{depth}.webp')


def fundstueck_2():
    gulls = Image.open(os.path.join(HERE, 'moewen.png')).convert('RGB')
    with open(SPEC, encoding='utf-8') as file:
        spec = file.read()[:20000]
    embed(gulls, MESSAGE_2 + spec, BoxClasses(5)).save(os.path.join(HERE, 'fundstueck-2.png'))


def teaser():
    pier = Image.open(os.path.join(HERE, 'moewen-ganz.jpg')).convert('RGB')
    top = 370   # the small gull above, the man down to his hips
    card = pier.crop((0, top, pier.width, top + round(pier.width * 9 / 16)))
    card.resize((960, 540), Image.LANCZOS).save(os.path.join(HERE, '..', 'teaser.webp'), quality=88)


def publish():
    os.makedirs(STATIC, exist_ok=True)
    for name in ('fundstueck.png', 'fundstueck-2.png', 'strandhaeuser.png', 'moewen.png'):
        shutil.copy(os.path.join(HERE, name), os.path.join(STATIC, name))


if __name__ == '__main__':
    fundstueck()
    fundstueck_2()
    teaser()
    publish()
    planes()
    depths()
