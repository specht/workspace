import numpy as np

from wav import read_wav, write_wav


def echo(x, rate, delay, feedback, mix):
    D = round(delay * rate)
    y = x.copy()
    for n in range(D, len(y)):
        y[n] += feedback * y[n - D]
    return mix * y


def save(path, y, rate):
    peak = np.abs(y).max()
    write_wav(path, y / max(1, peak), rate)
    print(f'{path}: lauteste Stelle {peak:.2f}')


if __name__ == '__main__':
    x, rate = read_wav('stimme.wav')
    # zwei Sekunden Stille dahinter, damit die Echos ausklingen
    x = np.concatenate([x, np.zeros(2 * rate)])
    save('stimme-echo.wav', echo(x, rate, 0.3, 0.5, 0.6), rate)
    save('stimme-halle.wav', echo(x, rate, 0.05, 0.7, 0.5), rate)
