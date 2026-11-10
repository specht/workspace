import numpy as np

from wav import read_wav, write_wav


def threshold(f):
    k = f / 1000
    return 3.64 * k ** -0.8 - 6.5 * np.exp(-0.6 * (k - 3.3) ** 2) + 0.001 * k ** 4


def levels(block):
    X = np.fft.rfft(block)
    return 20 * np.log10(np.abs(X) * 2 / len(block) + 1e-12) + 96


def audible(block, rate):
    freqs = np.fft.rfftfreq(len(block), 1 / rate)
    return levels(block) >= 0


if __name__ == '__main__':
    N = 2048
    for name in ['musik', 'stimme']:
        x, rate = read_wav(name + '.wav')
        out = np.zeros(len(x))
        kept = 0
        total = 0
        for s in range(0, len(x) - N + 1, N):
            block = x[s:s + N]
            keep = audible(block, rate)
            out[s:s + N] = np.fft.irfft(np.fft.rfft(block) * keep, N)
            kept += keep.sum()
            total += len(keep)
        print(f'{name}: {kept / total:.0%} der Frequenzen über der Hörschwelle')
        write_wav(name + '-hoerbar.wav', out, rate)
