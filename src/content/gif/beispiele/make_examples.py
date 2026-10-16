#!/usr/bin/env python3
"""Makes the example files of the GIF activity.

diskette.gif is written byte by byte here, with its own LZW encoder, so that
every byte the page shows is known: Header, Logical Screen Descriptor, four
colours, a Graphic Control Extension, a Comment Extension, the Image
Descriptor and 45 bytes of image data. The two photos come from ImageMagick:
photo 30 of /rgb (by Ian Kobylanski, with permission), 640 pixels wide,
256 colours with Floyd-Steinberg dithering, once plain and once interlaced.

flug.gif is the animation for the last part of the page. Everything goes
into this folder, for the screenshot recipes and the walk through the page,
and into src/static/gif/, for the download on the page. The checksums in tasks.yaml
belong to these files: change them and the tasks change.
"""
import math
import os
import shutil
import struct
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
STATIC = os.path.join(HERE, '../../../static/gif')
PHOTO = os.path.join(HERE, '../../../static/jpeg/30.jpg')

ROWS = [
    "1113333333331100",
    "1113333333331110",
    "1113333331131111",
    "1113333331131111",
    "1113333331131111",
    "1113333333331111",
    "1111111111111111",
    "1122222222222211",
    "1122222222222211",
    "1121111111112211",
    "1122222222222211",
    "1121111111222211",
    "1122222222222211",
    "1122222222222211",
    "1122222222222211",
    "1111111111111111",
]
PALETTE = [(0x10, 0x10, 0x20), (0x2a, 0x6f, 0xb0), (0xf4, 0xf1, 0xe8), (0xc8, 0xce, 0xd6)]


class BitWriter:
    """Packs codes from the lowest bit of each byte on, as GIF reads them."""

    def __init__(self):
        self.out, self.acc, self.n = bytearray(), 0, 0

    def write(self, value, size):
        self.acc |= value << self.n
        self.n += size
        while self.n >= 8:
            self.out.append(self.acc & 0xff)
            self.acc >>= 8
            self.n -= 8

    def flush(self):
        if self.n:
            self.out.append(self.acc & 0xff)
        return bytes(self.out)


def lzw_encode(pixels, min_size):
    clear, end = 1 << min_size, (1 << min_size) + 1
    w = BitWriter()
    size = min_size + 1
    table = {(i,): i for i in range(clear)}
    next_code = end + 1
    w.write(clear, size)
    current = (pixels[0],)
    for p in pixels[1:]:
        if current + (p,) in table:
            current += (p,)
            continue
        w.write(table[current], size)
        if next_code < 4096:
            table[current + (p,)] = next_code
            # The decoder adds this entry one code later and grows when its
            # table reaches 1 << size, so here: when next_code is 1 << size.
            if next_code == 1 << size and size < 12:
                size += 1
            next_code += 1
        else:
            w.write(clear, size)
            table = {(i,): i for i in range(clear)}
            size, next_code = min_size + 1, end + 1
        current = (p,)
    w.write(table[current], size)
    w.write(end, size)
    return w.flush()


def sub_blocks(data):
    out = bytearray()
    for i in range(0, len(data), 255):
        part = data[i:i + 255]
        out += bytes([len(part)]) + part
    return bytes(out) + b'\0'


def diskette():
    width, height = len(ROWS[0]), len(ROWS)
    pixels = [int(c) for row in ROWS for c in row]
    f = bytearray(b'GIF89a')
    f += struct.pack('<HHBBB', width, height, 0b10010001, 0, 0)  # Global Color Table with 4 colours
    for colour in PALETTE:
        f += bytes(colour)
    f += b'\x21\xf9\x04\x00\x00\x00\x00\x00'                       # Graphic Control Extension, all 0
    f += b'\x21\xfe' + sub_blocks(b'Hackschule')                   # Comment Extension
    f += b'\x2c' + struct.pack('<HHHHB', 0, 0, width, height, 0)
    f += b'\x02' + sub_blocks(lzw_encode(pixels, 2))
    f += b'\x3b'
    return bytes(f)


# flug.gif: the disk flies over a landscape in 14 frames of 16 x 16 pixels,
# transparent corner, Disposal Method 3, and lands red with a Local Color
# Table. A player that puts the LCT into one palette turns the sky red.
W, H = 96, 64
# GCT: 0-3 the disk, then sky, hills, ground, stars
GCT = [(0x10, 0x10, 0x20), (0x2a, 0x6f, 0xb0), (0xf4, 0xf1, 0xe8), (0xc8, 0xce, 0xd6),
       (0x1c, 0x3d, 0x6e), (0x3a, 0x6e, 0xa8), (0x6a, 0x9c, 0xd0), (0x9c, 0xc6, 0xe8),
       (0x3f, 0x7d, 0x3a), (0x2e, 0x5e, 0x2b), (0x8a, 0x6a, 0x45), (0x6e, 0x52, 0x34),
       (0xff, 0xff, 0xff), (0xf2, 0xc1, 0x4e), (0x1f, 0x3b, 0x1d), (0x00, 0x00, 0x00)]
LCT = [(0x10, 0x10, 0x20), (0xc0, 0x39, 0x2b), (0xf4, 0xf1, 0xe8), (0xf2, 0xc1, 0x4e)]
GROUND = 54

def background():
    px = []
    for y in range(H):
        for x in range(W):
            if y < 10: c = 4
            elif y < 20: c = 1
            elif y < 28: c = 6
            else: c = 7
            hill = 40 + int(6 * math.sin(x / 9.0)) + int(3 * math.sin(x / 4.0 + 1))
            if y >= hill: c = 8 if y < hill + 3 else 9
            if y >= GROUND: c = 10 if y < GROUND + 2 else 11
            px.append(c)
    for (sx, sy) in [(7, 3), (21, 6), (33, 2), (52, 5), (61, 8), (44, 7), (90, 2), (14, 8), (27, 1), (70, 4)]:
        px[sy * W + sx] = 12
    for y in range(10, 20):           # a moon
        for x in range(76, 86):
            if (x - 81) ** 2 + (y - 14) ** 2 <= 12: px[y * W + x] = 13

    return px

SPRITE = [int(c) for r in ROWS for c in r]   # 16x16, index 0 = the cut corner, transparent

def path(n):
    pts = []
    for i in range(n):
        t = i / (n - 1)
        x = int(round(2 + t * 70))
        # two bounces, landing on the ground
        b = abs(math.cos(t * math.pi * 1.5))
        y = int(round(GROUND - 16 - b * 30 * (1 - 0.5 * t)))
        pts.append((x, y))
    return pts

def gce(disposal, delay, transparent=None):
    p = (disposal << 2) | (1 if transparent is not None else 0)
    return b'\x21\xf9\x04' + bytes([p]) + struct.pack('<H', delay) + bytes([transparent or 0]) + b'\x00'

def image(x, y, w, h, pixels, min_size, lct=None):
    packed = 0
    out = b'\x2c' + struct.pack('<HHHH', x, y, w, h)
    if lct:
        packed = 0x80 | ((len(lct) - 1).bit_length() - 1)
        out += bytes([packed]) + b''.join(bytes(c) for c in lct)
    else:
        out += b'\x00'
    return out + bytes([min_size]) + sub_blocks(lzw_encode(pixels, min_size))

def flight():
    f = bytearray(b'GIF89a')
    f += struct.pack('<HHBBB', W, H, 0xf3, 15, 0)            # GCT 16 colours, background 15
    for c in GCT: f += bytes(c)
    f += b'\x21\xff\x0bNETSCAPE2.0\x03\x01\x00\x00\x00'      # loop forever
    f += b'\x21\xfe' + sub_blocks(b'Diskette im Flug, Hackschule')
    f += gce(1, 40) + image(0, 0, W, H, background(), 4)
    pts = path(14)
    for (x, y) in pts:
        f += gce(3, 8, transparent=0) + image(x, y, 16, 16, SPRITE, 4)
    x, y = pts[-1]
    f += gce(1, 150, transparent=0) + image(x, y, 16, 16, SPRITE, 2, lct=LCT)
    f += b'\x3b'
    return bytes(f)


def main():
    os.makedirs(STATIC, exist_ok=True)
    with open(os.path.join(HERE, 'diskette.gif'), 'wb') as f:
        f.write(diskette())
    common = ['convert', PHOTO, '-resize', '640x', '-dither', 'FloydSteinberg', '-colors', '256', '+repage', '-strip']
    subprocess.run(common + [os.path.join(HERE, 'markt.gif')], check=True)
    subprocess.run(common + ['-interlace', 'GIF', os.path.join(HERE, 'markt-interlaced.gif')], check=True)
    with open(os.path.join(HERE, 'flug.gif'), 'wb') as f:
        f.write(flight())
    for name in ['diskette.gif', 'markt.gif', 'markt-interlaced.gif', 'flug.gif']:
        shutil.copy(os.path.join(HERE, name), os.path.join(STATIC, name))


if __name__ == '__main__':
    main()
