def blend(colors, alphas, background):
    color = 0
    for c, a in zip(colors, alphas):
        color += a * c
    return color + background
