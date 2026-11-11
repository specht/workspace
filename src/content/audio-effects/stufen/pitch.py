import numpy as np

from chorus import read
from wav import read_wav, write_wav

SIZE, HOP = 2048, 512


def pitch(block, rate):
    best, best_lag, dipped = 0, 0, False
    for lag in range(1, rate // 60):
        r = np.dot(block[:-lag], block[lag:])
        if r < 0:
            dipped = True
        elif dipped and r > best:
            best, best_lag = r, lag
    return rate / best_lag if best_lag else 0


def pitches(x, rate):
    out = []
    for start in range(0, len(x) - SIZE, HOP):
        block = x[start:start + SIZE]
        loud = np.sqrt(np.mean(block ** 2)) > 0.02
        out.append(pitch(block, rate) if loud else 0)
    return out


def shift(x, rate, factor, grain=0.05):
    G = round(grain * rate)
    D = rate // 60
    window = np.hanning(G)
    offsets = np.arange(-G // 2 - D, G - G // 2 + D)
    y = np.zeros(len(x) + G)
    weight = np.zeros(len(x) + G)
    for c in range(G // 2, len(x), G // 4):
        f = factor(c) if callable(factor) else factor
        piece = read(x, np.clip(c + offsets * f, 0, len(x) - 1))
        here = y[c - G // 2:c - G // 2 + G]
        s = np.argmax(np.correlate(piece, here, 'valid'))
        y[c - G // 2:c - G // 2 + G] += window * piece[s:s + G]
        weight[c - G // 2:c - G // 2 + G] += window
    return y[:len(x)] / np.maximum(weight[:len(x)], 1e-3)


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    found = [round(f) for f in pitches(x, rate) if f]
    print('Tonhöhen in Hz:', found[:12], '…')
    save('stimme-hoch.wav', shift(x, rate, 2 ** (5 / 12)), rate)
    save('stimme-tief.wav', shift(x, rate, 2 ** (-5 / 12)), rate)
