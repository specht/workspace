import numpy as np

from ear import threshold
from mdct import decode, encode
from wav import read_wav, write_wav

N = 1024
RATE = 44100
CAL = 44.0
OFFSET = 15


def bark(f):
    return 13 * np.arctan(0.00076 * f) + 3.5 * np.arctan((f / 7500) ** 2)


def spread(dz):
    return 15.81 + 7.5 * (dz + 0.474) - 17.5 * np.sqrt(1 + (dz + 0.474) ** 2)


def make_bands():
    edges = [0]
    for z in range(1, 26):
        k = int(np.searchsorted(bark(FREQS), z))
        if k - edges[-1] >= 4 and k < N:
            edges.append(k)
    edges.append(N)
    return list(zip(edges[:-1], edges[1:]))


FREQS = (np.arange(N) + 0.5) * RATE / (2 * N)
BANDS = make_bands()
CENTRES = np.array([bark(FREQS[(a + b) // 2]) for a, b in BANDS])
QUIET = np.array([threshold(FREQS[a:b]).min() for a, b in BANDS])


def allowed(level):
    out = np.zeros(len(level))
    for i in range(len(level)):
        power = 0
        for j in range(len(level)):
            power += 10 ** ((level[j] + spread(CENTRES[i] - CENTRES[j]) - OFFSET) / 10)
        out[i] = 10 * np.log10(power)
    return np.maximum(out, QUIET)


def quantise(X):
    level = np.array([10 * np.log10(np.sum(X[a:b] ** 2) + 1e-20) + CAL for a, b in BANDS])
    noise = allowed(level)
    q = np.zeros(N, dtype=int)
    steps = np.zeros(len(BANDS))
    for i, (a, b) in enumerate(BANDS):
        steps[i] = np.sqrt(12 * 10 ** ((noise[i] - CAL) / 10) / (b - a))
        q[a:b] = np.round(X[a:b] / steps[i])
    return q, steps


def entropy(values):
    _, counts = np.unique(values, return_counts=True)
    p = counts / counts.sum()
    return float(np.sum(p * np.log2(1 / p)))


def dequantise(q, steps):
    Y = np.zeros(N)
    for i, (a, b) in enumerate(BANDS):
        Y[a:b] = q[a:b] * steps[i]
    return Y


if __name__ == '__main__':
    x, rate = read_wav('musik.wav')
    coded = [quantise(X) for X in encode(x, N)]
    y = decode([dequantise(q, steps) for q, steps in coded], N, len(x))
    write_wav('musik-codec.wav', y, rate)
    write_wav('musik-rest.wav', x - y, rate)
    values = np.concatenate([q for q, steps in coded])
    print(f'{np.mean(values == 0):.0%} der Zahlen sind 0, die größte ist {np.abs(values).max()}')
    bits = entropy(values) * len(values) + 6 * len(BANDS) * len(coded)
    seconds = len(x) / rate
    print(f'{bits / seconds / 1000:.0f} kbit/s statt {16 * rate / 1000:.0f} kbit/s')
