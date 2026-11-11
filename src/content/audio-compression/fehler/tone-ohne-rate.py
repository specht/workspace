import numpy as np

from wav import write_wav


def tone(freq, seconds, rate):
    t = np.arange(round(seconds * rate))
    return 0.5 * np.sin(2 * np.pi * freq * t)


if __name__ == '__main__':
    write_wav('kurz.wav', tone(1000, 0.001, 8000), 8000)
    write_wav('a.wav', tone(440, 2, 44100), 44100)
