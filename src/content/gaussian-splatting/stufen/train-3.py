import numpy as np
from PIL import Image
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


class Adam:
    def __init__(self, rate):
        self.rate = rate
        self.t = 0
        self.m = 0
        self.v = 0

    def step(self, value, gradient):
        self.t += 1
        self.m = 0.9 * self.m + 0.1 * gradient
        self.v = 0.999 * self.v + 0.001 * gradient ** 2
        m = self.m / (1 - 0.9 ** self.t)
        v = self.v / (1 - 0.999 ** self.t)
        return value - self.rate * m / (np.sqrt(v) + 1e-8)


adam = {'x': Adam(0.3), 'y': Adam(0.3), 'sx': Adam(0.05), 'sy': Adam(0.05),
        'angle': Adam(0.03), 'color': Adam(0.02)}
frames = []

for t in range(301):
    image, index, g, dx, dy = render()
    if t % 10 == 0:
        frames.append(Image.fromarray((np.clip(image, 0, 1) * 255).astype(np.uint8)).resize((W * 2, H * 2)))
    if t % 50 == 0:
        print(f'Schritt {t:3d}: Fehler {error(image, target):.5f}')
    r = (image - target).reshape(-1, 3)[index]  # wie falsch jeder Pixel um jeden Splat ist
    color_gradient = (g[:, :, None] * r).sum(1)

    c = np.cos(angle)[:, None]
    s = np.sin(angle)[:, None]
    u = (c * dx + s * dy) / sx[:, None]
    v = (-s * dx + c * dy) / sy[:, None]
    e = g * (r * color[:, None, :]).sum(2)     # macht der Splat hier zu hell (+) oder zu dunkel (−)?
    x_gradient = (e * (u * c / sx[:, None] - v * s / sy[:, None])).sum(1)
    y_gradient = (e * (u * s / sx[:, None] + v * c / sy[:, None])).sum(1)
    sx_gradient = (e * u * u).sum(1) / sx
    sy_gradient = (e * v * v).sum(1) / sy
    angle_gradient = (e * u * v * (sx[:, None] / sy[:, None] - sy[:, None] / sx[:, None])).sum(1)

    x = np.clip(adam['x'].step(x, x_gradient), 0, W - 0.01)
    y = np.clip(adam['y'].step(y, y_gradient), 0, H - 0.01)
    sx = np.clip(adam['sx'].step(sx, sx_gradient), 0.3, K / 6)
    sy = np.clip(adam['sy'].step(sy, sy_gradient), 0.3, K / 6)
    angle = adam['angle'].step(angle, angle_gradient)
    color = adam['color'].step(color, color_gradient)

save(image, 'learned.png')
frames[0].save('training.gif', save_all=True, append_images=frames[1:], duration=80, loop=0)


def paint(scale, sharpness):
    # dieselben Splats auf einem Bild, das scale-mal so groß ist; sharpness 1
    # ist die Glocke, je größer, desto schärfer der Rand
    big = np.zeros((H * scale, W * scale, 3))
    for i in range(N):
        r = 3 * max(sx[i], sy[i])                  # weiter weg ist der Splat fast 0
        x0, x1 = int(max(0, x[i] - r) * scale), int(min(W, x[i] + r) * scale)
        y0, y1 = int(max(0, y[i] - r) * scale), int(min(H, y[i] + r) * scale)
        ys, xs = np.mgrid[y0:y1, x0:x1]
        dx = (xs + 0.5) / scale - x[i]             # wo der Pixel im kleinen Bild läge
        dy = (ys + 0.5) / scale - y[i]
        c, s = np.cos(angle[i]), np.sin(angle[i])
        u = (c * dx + s * dy) / sx[i]
        v = (-s * dx + c * dy) / sy[i]
        big[y0:y1, x0:x1] += np.exp(-(u * u + v * v) ** sharpness / 2)[:, :, None] * color[i]
    return big


save(paint(4, 1), 'learned-large.png')
save(paint(4, 8), 'learned-sharp.png')
