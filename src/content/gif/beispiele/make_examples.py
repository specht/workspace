#!/usr/bin/env python3
"""Makes the example files of the GIF activity.

diskette.gif is written byte by byte here, with its own LZW encoder, so that
every byte the page shows is known: Header, Logical Screen Descriptor, four
colours, a Graphic Control Extension, a Comment Extension, the Image
Descriptor and 45 bytes of image data. The two photos come from ImageMagick:
photo 30 of /rgb (by Ian Kobylanski, with permission), 640 pixels wide,
256 colours with Floyd-Steinberg dithering, once plain and once interlaced.

Writes all three into src/static/gif/ (for the download on the page) and
the first two also into this folder (for the screenshot recipes). The checksums in tasks.yaml
belong to these files: change them and the tasks change.
"""
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


def main():
    os.makedirs(STATIC, exist_ok=True)
    with open(os.path.join(HERE, 'diskette.gif'), 'wb') as f:
        f.write(diskette())
    common = ['convert', PHOTO, '-resize', '640x', '-dither', 'FloydSteinberg', '-colors', '256', '+repage', '-strip']
    subprocess.run(common + [os.path.join(HERE, 'markt.gif')], check=True)
    subprocess.run(common + ['-interlace', 'GIF', os.path.join(STATIC, 'markt-interlaced.gif')], check=True)
    for name in ['diskette.gif', 'markt.gif']:
        shutil.copy(os.path.join(HERE, name), os.path.join(STATIC, name))


if __name__ == '__main__':
    main()
