import numpy as np


def blob(dx, dy, sigma):
    return np.exp(-(dx * dx + dy * dy) / (2 * sigma * sigma))


def weight(dx, dy, sx, sy, angle):
    c = np.cos(angle)
    s = np.sin(angle)
    u = c * dx + s * dy
    v = -s * dx + c * dy
    return np.exp(-(u * u + v * v) / (2 * sx * sy))
