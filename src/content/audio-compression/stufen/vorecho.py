import numpy as np

from mdct import decode, encode
from wav import read_wav, write_wav


def coarse(blocks):
    out = []
    for X in blocks:
        step = np.abs(X).max() / 8
        out.append(np.round(X / step) * step)
    return out


if __name__ == '__main__':
    x, rate = read_wav('kastagnetten.wav')
    for N, name in [(1024, 'lang'), (128, 'kurz')]:
        y = decode(coarse(encode(x, N)), N, len(x))
        write_wav(f'kastagnetten-{name}.wav', y, rate)
        print(f'{name}: Blöcke mit {N} Zahlen, fertig')
