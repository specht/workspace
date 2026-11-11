import numpy as np

from wav import read_wav

ROWS = [697, 770, 852, 941]
COLUMNS = [1209, 1336, 1477]
KEYS = ['123', '456', '789', '*0#']


def strength(block, freq, rate):
    n = np.arange(len(block))
    return 2 * abs(np.sum(block * np.exp(-2j * np.pi * freq * n / len(block)))) / len(block)


def key(block, rate):
    row = max(range(4), key=lambda i: strength(block, ROWS[i], rate))
    column = max(range(3), key=lambda i: strength(block, COLUMNS[i], rate))
    return KEYS[row][column]


def tones(samples, rate):
    piece = rate // 100
    blocks = []
    start = None
    for i in range(0, len(samples), piece):
        loud = np.abs(samples[i:i + piece]).max() > 0.2
        if loud and start is None:
            start = i
        elif not loud and start is not None:
            blocks.append(samples[start:i])
            start = None
    return blocks


if __name__ == '__main__':
    samples, rate = read_wav('waehlen.wav')
    print(''.join(key(block, rate) for block in tones(samples, rate)))
