import numpy as np

from echo import echo
from wav import read_wav, write_wav

COMBS = [0.0297, 0.0371, 0.0411, 0.0437]


def allpass(x, rate, delay, g):
    D = round(delay * rate)
    y = np.zeros_like(x)
    for n in range(len(x)):
        y[n] = g * x[n]
        if n >= D:
            y[n] += x[n - D] + g * y[n - D]
    return y


def reverb(x, rate, size, time, mix):
    wet = np.zeros_like(x)
    for delay in COMBS:
        D = round(delay * size * rate)
        g = 10 ** (-3 * D / rate / time)
        wet += echo(x, rate, delay * size, g, 1) / len(COMBS)
    for delay in (0.005, 0.0017):
        wet = allpass(wet, rate, delay, 0.7)
    return x + mix * wet


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    x = np.concatenate([x, np.zeros(3 * rate)])
    save('stimme-raum.wav', reverb(x, rate, 1, 1.5, 0.4), rate)
    save('stimme-kirche.wav', reverb(x, rate, 2, 4, 0.6), rate)
