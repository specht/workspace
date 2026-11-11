import numpy as np

from wav import read_wav, write_wav


def mdct(x):
    N = len(x) // 2
    n = np.arange(2 * N)
    X = np.zeros(N)
    for k in range(N):
        X[k] = np.sum(x * np.cos(np.pi / N * (n + 0.5 + N / 2) * (k + 0.5)))
    return X


def imdct(X):
    N = len(X)
    k = np.arange(N)
    y = np.zeros(2 * N)
    for n in range(2 * N):
        y[n] = np.sum(X * np.cos(np.pi / N * (n + 0.5 + N / 2) * (k + 0.5)))
    return y * 2 / N


def window(N):
    return np.sin(np.pi * (np.arange(2 * N) + 0.5) / (2 * N))


def encode(x, N):
    w = window(N)
    x = np.concatenate([np.zeros(N), x, np.zeros(2 * N)])
    return [mdct(w * x[s:s + 2 * N]) for s in range(0, len(x) - 2 * N + 1, N)]


def decode(blocks, N, length):
    w = window(N)
    y = np.zeros((len(blocks) + 1) * N)
    for i, X in enumerate(blocks):
        y[i * N:i * N + 2 * N] += w * imdct(X)
    return y[N:N + length]


if __name__ == '__main__':
    print(np.round(imdct(mdct(np.arange(16.0))), 2))
    x, rate = read_wav('musik.wav')
    x = x[:2 * rate]
    y = decode(encode(x, 1024), 1024, len(x))
    print('größter Fehler:', np.abs(x - y).max())
