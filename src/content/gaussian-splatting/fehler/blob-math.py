import math


def blob(dx, dy, sigma):
    return math.exp(-(dx * dx + dy * dy) / (2 * sigma * sigma))
