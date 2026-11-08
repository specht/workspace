def blend(colors, alphas, background):
    color = 0
    transmittance = 1
    for c, a in zip(colors, alphas):
        color += transmittance * a * c
        transmittance *= 1 - a
    return color
