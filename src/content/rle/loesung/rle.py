# Lauflängen: statt »AAAA« steht »4A« da, die Länge eines Laufs und was
# sich wiederholt. Die Lösung zur Aktivität »Komprimieren mit Lauflängen«.

import re
import sys


def rle(text):
    """Als Text: 'WWWWWWBBBWWWW' wird '6W3B4W'. Geht nur ohne Ziffern im Text."""
    out = ''
    i = 0
    while i < len(text):
        count = 1
        while i + count < len(text) and text[i + count] == text[i]:
            count += 1
        out += f'{count}{text[i]}'
        i += count
    return out


def unrle(code):
    return ''.join(char * int(count) for count, char in re.findall(r'(\d+)(\D)', code))


def rle_bytes(data):
    """Als Bytes: immer zwei Zahlen, die Länge (1 bis 255) und das Byte."""
    out = []
    i = 0
    while i < len(data):
        count = 1
        while i + count < len(data) and data[i + count] == data[i] and count < 255:
            count += 1
        out += [count, data[i]]
        i += count
    return out


def unrle_bytes(numbers):
    out = []
    for count, byte in zip(numbers[0::2], numbers[1::2]):
        out += [byte] * count
    return bytes(out)


print(rle('WWWWWWBBBWWWW'))
if len(sys.argv) > 1:
    data = open(sys.argv[1], 'rb').read()
    packed = rle_bytes(data)
    assert unrle_bytes(packed) == data
    print(f'{len(data)} Bytes werden zu {len(packed)}')
