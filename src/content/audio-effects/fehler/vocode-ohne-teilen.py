import numpy as np

from wav import read_wav, write_wav

SIZE, HOP = 2048, 512


def saw(freqs, n, rate):
    t = np.arange(n) / rate
    return sum(2 * ((f * t) % 1) - 1 for f in freqs) / len(freqs)


def vocode_frame(voice, carrier, edges):
    out = carrier.copy()
    for lo, hi in zip(edges[:-1], edges[1:]):
        v = np.sqrt(np.sum(np.abs(voice[lo:hi]) ** 2))
        c = np.sqrt(np.sum(np.abs(carrier[lo:hi]) ** 2))
        out[lo:hi] *= v
    return out


def vocoder(x, carrier, rate, bands=16):
    hz = np.geomspace(100, 8000, bands + 1)
    edges = np.round(hz * SIZE / rate).astype(int)
    window = np.hanning(SIZE)
    y = np.zeros(len(x) + SIZE)
    for start in range(0, len(x) - SIZE, HOP):
        v = np.fft.rfft(x[start:start + SIZE] * window)
        c = np.fft.rfft(carrier[start:start + SIZE] * window)
        y[start:start + SIZE] += np.fft.irfft(vocode_frame(v, c, edges)) * window
    return y[:len(x)] / 1.5


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    chord = saw([130.8, 164.8, 196.0], len(x), rate)
    save('stimme-roboter.wav', vocoder(x, chord, rate), rate)
    save('stimme-fluestern.wav', vocoder(x, np.random.uniform(-1, 1, len(x)), rate), rate)
