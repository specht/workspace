import numpy as np


def step(value, gradient, rate):
    return value + rate * gradient
