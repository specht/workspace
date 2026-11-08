import numpy as np
from blobs import weight, error, step, save, load

target = load('pearl-earring.png')
H, W, _ = target.shape
N = 500        # so viele Splats
K = 21         # jeder Splat zählt nur in einem Quadrat aus K x K Pixeln

rng = np.random.default_rng(1)
x = rng.random(N) * W
y = rng.random(N) * H
sx = np.full(N, 3.0)
sy = np.full(N, 3.0)
angle = rng.random(N) * np.pi
color = np.full((N, 3), 0.1)

offsets = np.arange(K) - K // 2
oy, ox = [a.reshape(-1) for a in np.meshgrid(offsets, offsets, indexing='ij')]


def render():
    px = np.floor(x)[:, None] + ox             # die Pixel um jeden Splat: N x K²
    py = np.floor(y)[:, None] + oy
    inside = (px >= 0) & (px < W) & (py >= 0) & (py < H)
    index = (np.clip(py, 0, H - 1) * W + np.clip(px, 0, W - 1)).astype(int)
    dx = px + 0.5 - x[:, None]
    dy = py + 0.5 - y[:, None]
    g = weight(dx, dy, sx[:, None], sy[:, None], angle[:, None]) * inside
    image = np.zeros((H * W, 3))
    np.add.at(image, index.reshape(-1), (g[:, :, None] * color[:, None, :]).reshape(-1, 3))
    return image.reshape(H, W, 3), index, g, dx, dy


for t in range(201):
    image, index, g, dx, dy = render()
    if t % 50 == 0:
        print(f'Schritt {t:3d}: Fehler {error(image, target):.5f}')
    r = (image - target).reshape(-1, 3)[index]  # wie falsch jeder Pixel um jeden Splat ist
    color_gradient = (g[:, :, None] * r).sum(1)
    color = step(color, color_gradient, 0.01)

save(image, 'colors.png')
