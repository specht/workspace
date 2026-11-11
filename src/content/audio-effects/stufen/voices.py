import numpy as np

from pitch import save, shift
from wav import read_wav


def ring(x, rate, freq):
    n = np.arange(len(x))
    return x * np.sin(2 * np.pi * freq * n / rate)


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    save('stimme-chipmunk.wav', shift(x, rate, 2), rate)
    save('stimme-riese.wav', shift(x, rate, 2 ** (-8 / 12)), rate)
    save('stimme-dalek.wav', ring(x, rate, 30), rate)
