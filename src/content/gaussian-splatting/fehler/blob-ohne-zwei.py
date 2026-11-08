import numpy as np


def blob(dx, dy, sigma):
    return np.exp(-(dx * dx + dy * dy) / (sigma * sigma))
