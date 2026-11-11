import numpy as np

from wav import read_wav, write_wav


def bell(freqs, centre, width, gain):
    octaves = np.log2(np.maximum(freqs, 1) / centre)
    db = gain * 2 ** (-(octaves / width) ** 2)
    return 10 ** (db / 10)


def equalize(x, rate, centre, width, gain):
    X = np.fft.rfft(x)
    freqs = np.fft.rfftfreq(len(x), 1 / rate)
    return np.fft.irfft(X * bell(freqs, centre, width, gain), len(x))


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('musik.wav')
    save('musik-bass.wav', equalize(x, rate, 60, 1, 12), rate)
    save('musik-ohne-hihat.wav', equalize(x, rate, 10000, 2, -30), rate)
