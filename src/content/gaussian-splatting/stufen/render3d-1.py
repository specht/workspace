import numpy as np


def read_ply(name):
    with open(name, 'rb') as f:
        names = []
        while True:
            line = f.readline().decode().strip()
            if line.startswith('element vertex'):
                count = int(line.split()[2])
            elif line.startswith('property'):
                names.append(line.split()[2])
            elif line == 'end_header':
                break
        data = np.frombuffer(f.read(), dtype=np.float32).reshape(count, len(names))
    p = {name: data[:, i] for i, name in enumerate(names)}
    position = np.stack([p['x'], p['y'], p['z']], 1)
    scale = np.exp(np.stack([p['scale_0'], p['scale_1'], p['scale_2']], 1))
    rotation = np.stack([p['rot_0'], p['rot_1'], p['rot_2'], p['rot_3']], 1)
    rotation /= np.linalg.norm(rotation, axis=1, keepdims=True)
    color = np.clip(0.5 + 0.2821 * np.stack([p['f_dc_0'], p['f_dc_1'], p['f_dc_2']], 1), 0, 1)
    opacity = 1 / (1 + np.exp(-p['opacity']))
    return position, scale, rotation, color, opacity


def rotation_matrix(q):
    w, x, y, z = q
    return np.array([
        [1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y)],
        [2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x)],
        [2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)]])


def covariance(scale, rotation):
    S = np.diag(scale)
    return rotation @ S @ S.T @ rotation.T


def to_screen(x, y, z, f, W, H):
    return f * x / z + W / 2, f * y / z + H / 2


def blend(colors, alphas, background):
    color = 0
    transmittance = 1
    for c, a in zip(colors, alphas):
        color += transmittance * a * c
        transmittance *= 1 - a
    return color + transmittance * background

