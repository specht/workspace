import numpy as np


def error(image, target):
    return np.sum((image - target) ** 2)
