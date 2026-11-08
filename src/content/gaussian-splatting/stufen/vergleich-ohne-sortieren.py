import numpy as np
from PIL import Image


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


def look_at(eye, target):
    forward = target - eye
    forward = forward / np.linalg.norm(forward)
    right = np.cross(forward, [0, 1, 0])
    right = right / np.linalg.norm(right)
    down = np.cross(forward, right)
    return np.array([right, down, forward])


def render(splats, eye, W=640, H=480, f=680, background=1.0):
    position, scale, rotation, color, opacity = splats
    view = look_at(eye, np.zeros(3))
    x, y, z = ((position - eye) @ view.T).T          # in Koordinaten der Kamera
    u, v = to_screen(x, y, z, f, W, H)

    # jede Ellipse auf dem Bildschirm: Σ' = J V Σ Vᵀ Jᵀ
    image = np.zeros((H, W, 3))
    transmittance = np.ones((H, W))
    for i in range(len(z)):                           # in der Reihenfolge der Datei
        if z[i] < 0.1:
            continue
        sigma = covariance(scale[i], rotation_matrix(rotation[i]))
        J = np.array([[f / z[i], 0, -f * x[i] / z[i] ** 2],
                      [0, f / z[i], -f * y[i] / z[i] ** 2]])
        T = J @ view
        a, b, c = (T @ sigma @ T.T + 0.3 * np.eye(2))[[0, 0, 1], [0, 1, 1]]
        radius = 3 * np.sqrt(max(a, c))
        x0, x1 = max(0, int(u[i] - radius)), min(W, int(u[i] + radius) + 1)
        y0, y1 = max(0, int(v[i] - radius)), min(H, int(v[i] + radius) + 1)
        if x0 >= x1 or y0 >= y1:
            continue
        dx = np.arange(x0, x1) + 0.5 - u[i]
        dy = (np.arange(y0, y1) + 0.5 - v[i])[:, None]
        det = a * c - b * b
        q = (c * dx * dx - 2 * b * dx * dy + a * dy * dy) / det
        alpha = np.minimum(0.99, opacity[i] * np.exp(-q / 2))
        t = transmittance[y0:y1, x0:x1]
        image[y0:y1, x0:x1] += (t * alpha)[:, :, None] * color[i]
        t *= 1 - alpha
    return image + transmittance[:, :, None] * background


def save(image, name):
    pixels = np.clip(image, 0, 1) * 255
    Image.fromarray(pixels.astype(np.uint8)).save(name)


if __name__ == '__main__':
    splats = read_ply('modern-arm-chair.ply')
    save(render(splats, np.array([0.6, 0.35, 0.9])), 'armchair.png')
