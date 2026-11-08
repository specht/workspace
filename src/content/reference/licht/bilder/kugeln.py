"""The spheres of the Handbuch page »Licht und Glanz«: one sphere, lit with
the formulas of the page, one step at a time.

    python3 src/content/reference/licht/bilder/kugeln.py

Each picture is a sphere seen from the front (orthographic, the camera
looking along -z), lit by one lamp from the upper left front. The formulas
are those of the page: Lambert, Blinn's highlight with the half vector,
mirroring from a simple sky above a ground, Schlick's Fresnel. Rendered at
twice the size and scaled down for smooth edges; the colours with gamma
2.2, as a screen shows them. A grey ground that suits the light and the
dark page.

The last three show a coarse sphere of 196 triangles, rasterised here
pixel by pixel, in flat, Gouraud and Phong shading.
"""

import os

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..')
SIZE = 320
SCALE = 2
GROUND = np.array([0.36, 0.38, 0.41])
BASE = np.array([0.80, 0.26, 0.06])          # an orange, like lacquer (linear)
GOLD = np.array([1.0, 0.70, 0.22])
LIGHT = np.array([-0.5, 0.6, 0.62])
LIGHT = LIGHT / np.linalg.norm(LIGHT)
V = np.array([0.0, 0.0, 1.0])                # towards the camera


def sky(r):
    """a sky above a ground: blue at the top, light at the horizon, brown below"""
    y = r[..., 1:2]
    top = np.array([0.35, 0.55, 0.95])
    horizon = np.array([0.95, 0.95, 0.92])
    ground = np.array([0.35, 0.27, 0.2])
    up = np.clip(y, 0, 1) ** 0.5
    down = np.clip(-y, 0, 1) ** 0.4
    return np.where(y >= 0, horizon * (1 - up) + top * up, horizon * (1 - down) + ground * down)


def render(name, shade):
    n = SIZE * SCALE
    xs = (np.arange(n) + 0.5) / n * 2 - 1
    x, y = np.meshgrid(xs, -xs)
    r2 = (x / 0.82) ** 2 + (y / 0.82) ** 2
    inside = r2 < 1
    nx, ny = x / 0.82, y / 0.82
    nz = np.sqrt(np.clip(1 - nx * nx - ny * ny, 0, 1))
    normal = np.stack([nx, ny, nz], axis=-1)
    # the sphere with gamma 2.2, the ground as it is (it is a screen colour)
    color = np.where(inside[..., None], np.clip(shade(normal), 0, 1) ** (1 / 2.2), GROUND)
    image = Image.fromarray((color * 255 + 0.5).astype(np.uint8)).resize((SIZE, SIZE), Image.LANCZOS)
    image.save(os.path.join(OUT, name + '.webp'), quality=90)
    print(name)


def diffuse(n):
    return np.clip((n * LIGHT).sum(-1, keepdims=True), 0, None)


def highlight(n, k):
    h = (LIGHT + V) / np.linalg.norm(LIGHT + V)
    return np.clip((n * h).sum(-1, keepdims=True), 0, None) ** k


def mirrored(n):
    # reflect(-v, n) = -v - 2 (-v · n) n
    d = -V
    return d - 2 * (d * n).sum(-1, keepdims=True) * n


def fresnel(n, f0):
    return f0 + (1 - f0) * (1 - np.clip((n * V).sum(-1, keepdims=True), 0, 1)) ** 5


def lit(n):
    """the colour of the glossy sphere for normals n (..., 3)"""
    return BASE * (0.2 + 0.8 * diffuse(n)) + 0.5 * highlight(n, 40)


def coarse(rings=8, around=14):
    """a sphere of few triangles: its corners (with their normals, which
    on a sphere point outwards like the corners themselves) and triangles"""
    corners = [(0.0, 1.0, 0.0)]
    for r in range(1, rings):
        theta = np.pi * r / rings
        for a in range(around):
            phi = 2 * np.pi * (a + 0.5 * (r % 2)) / around
            corners.append((np.sin(theta) * np.sin(phi), np.cos(theta), np.sin(theta) * np.cos(phi)))
    corners.append((0.0, -1.0, 0.0))
    corners = np.array(corners)
    ring = lambda r, a: 1 + (r - 1) * around + a % around
    triangles = [(0, ring(1, a), ring(1, a + 1)) for a in range(around)]
    for r in range(1, rings - 1):
        for a in range(around):
            if r % 2:
                triangles += [(ring(r, a), ring(r + 1, a), ring(r + 1, a + 1)), (ring(r, a), ring(r + 1, a + 1), ring(r, a + 1))]
            else:
                triangles += [(ring(r, a), ring(r + 1, a - 1), ring(r + 1, a)), (ring(r, a), ring(r + 1, a), ring(r, a + 1))]
    last = len(corners) - 1
    triangles += [(ring(rings - 1, a), last, ring(rings - 1, a + 1)) for a in range(around)]
    return corners, np.array(triangles)


def shaded(name, mode):
    """the coarse sphere, rasterised: flat (one normal per triangle),
    gouraud (the light at the corners, the colours mixed), phong (the
    normals mixed, the light at every pixel)"""
    n = SIZE * SCALE
    corners, triangles = coarse()
    # a slight tilt, so that the rings do not run straight across
    tilt = np.radians(18)
    turn = np.array([[1, 0, 0], [0, np.cos(tilt), -np.sin(tilt)], [0, np.sin(tilt), np.cos(tilt)]])
    corners = corners @ turn.T
    corner_colour = lit(corners)
    image = np.tile(GROUND, (n, n, 1)).astype(float)
    depth = np.full((n, n), -np.inf)
    screen = (corners[:, :2] * np.array([0.82, -0.82]) + 1) / 2 * n
    for tri in triangles:
        a, b, c = corners[tri]
        face = np.cross(b - a, c - a)
        if face[2] <= 0:
            continue                        # facing away
        face = face / np.linalg.norm(face)
        (x0, y0), (x1, y1), (x2, y2) = screen[tri]
        left, right = int(max(0, min(x0, x1, x2))), int(min(n - 1, max(x0, x1, x2)) + 1)
        top, bottom = int(max(0, min(y0, y1, y2))), int(min(n - 1, max(y0, y1, y2)) + 1)
        xs, ys = np.meshgrid(np.arange(left, right) + 0.5, np.arange(top, bottom) + 0.5)
        det = (y1 - y2) * (x0 - x2) + (x2 - x1) * (y0 - y2)
        w0 = ((y1 - y2) * (xs - x2) + (x2 - x1) * (ys - y2)) / det
        w1 = ((y2 - y0) * (xs - x2) + (x0 - x2) * (ys - y2)) / det
        w2 = 1 - w0 - w1
        inside = (w0 >= -1e-9) & (w1 >= -1e-9) & (w2 >= -1e-9)
        z = w0 * a[2] + w1 * b[2] + w2 * c[2]
        region = (slice(top, bottom), slice(left, right))
        front = inside & (z > depth[region])
        if not front.any():
            continue
        w = np.stack([w0, w1, w2], -1)[..., None]
        if mode == 'flach':
            colour = np.broadcast_to(lit(face), w0.shape + (3,))
        elif mode == 'gouraud':
            colour = (w * corner_colour[tri]).sum(-2)
        else:
            normal = (w * corners[tri]).sum(-2)
            colour = lit(normal / np.linalg.norm(normal, axis=-1, keepdims=True))
        colour = np.clip(colour, 0, 1) ** (1 / 2.2)
        image[region][front] = colour[front]
        depth[region][front] = z[front]
    picture = Image.fromarray((image * 255 + 0.5).astype(np.uint8)).resize((SIZE, SIZE), Image.LANCZOS)
    picture.save(os.path.join(OUT, name + '.webp'), quality=90)
    print(name)


if __name__ == '__main__':
    for mode in ('flach', 'gouraud', 'phong'):
        shaded(f'kugel-{mode}', mode)
    render('kugel-lampe', lambda n: BASE * diffuse(n))
    render('kugel-ambient', lambda n: BASE * (0.2 + 0.8 * diffuse(n)))
    for k in (8, 40, 200):
        render(f'kugel-glanz-{k}', lambda n, k=k: BASE * (0.2 + 0.8 * diffuse(n)) + 0.5 * highlight(n, k))
    matt = lambda n: BASE * (0.2 + 0.8 * diffuse(n)) + 0.5 * highlight(n, 40)
    render('kugel-ohne-fresnel', lambda n: matt(n) + 0.04 * sky(mirrored(n)))
    render('kugel-fresnel', lambda n: matt(n) + fresnel(n, 0.04) * sky(mirrored(n)))
    render('kugel-metall', lambda n: GOLD * fresnel(n, 0.8) * sky(mirrored(n)) + GOLD * highlight(n, 200))
