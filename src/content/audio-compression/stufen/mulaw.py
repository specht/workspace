import numpy as np

from wav import read_wav, write_wav

MU = 255


def mulaw(x):
    return np.sign(x) * np.log(1 + MU * np.abs(x)) / np.log(1 + MU)


def unmulaw(y):
    return np.sign(y) * ((1 + MU) ** np.abs(y) - 1) / MU


def quantise(x, bits):
    top = 2 ** (bits - 1) - 1
    return np.round(x * top) / top


def snr(original, copy):
    noise = original - copy
    return 10 * np.log10(np.sum(original ** 2) / np.sum(noise ** 2))


if __name__ == '__main__':
    voice, rate = read_wav('stimme.wav')
    for level in [1, 0.25, 0.06]:
        quiet = voice * level
        even = quantise(quiet, 8)
        law = unmulaw(quantise(mulaw(quiet), 8))
        print(f'{level:4}: gleichmäßig {snr(quiet, even):4.1f} dB, μ-law {snr(quiet, law):4.1f} dB')
    write_wav('leise-gleichmaessig.wav', even / level, rate)
    write_wav('leise-mulaw.wav', law / level, rate)
