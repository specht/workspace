import numpy as np


def error(image, target):
    return np.mean(np.abs(image - target))
