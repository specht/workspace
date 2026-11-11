import numpy as np

from wav import read_wav, write_wav


def gain(x, db):
    return x + db / 20


if __name__ == '__main__':
    x, rate = read_wav('musik.wav')
    for db in (-20, -6, 6):
        y = gain(x, db)
        write_wav(f'musik{db:+d}db.wav', y, rate)
        print(f'{db:+d} dB: lauteste Stelle {np.abs(y).max():.2f}')
