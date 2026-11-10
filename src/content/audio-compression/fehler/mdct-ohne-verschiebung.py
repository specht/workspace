import numpy as np

from wav import read_wav, write_wav


def mdct(x):
    N = len(x) // 2
    n = np.arange(2 * N)
    X = np.zeros(N)
    for k in range(N):
        X[k] = np.sum(x * np.cos(np.pi / N * n * k))
    return X


def imdct(X):
    N = len(X)
    k = np.arange(N)
    y = np.zeros(2 * N)
    for n in range(2 * N):
        y[n] = np.sum(X * np.cos(np.pi / N * (n + 0.5 + N / 2) * (k + 0.5)))
    return y * 2 / N


if __name__ == '__main__':
    print(np.round(imdct(mdct(np.arange(16.0))), 2))
