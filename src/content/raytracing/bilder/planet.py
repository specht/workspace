"""The planet's texture of »Ein Planet«, step by step: planet_color from
raytracer.c, computed for every (u, v) of a flat rectangle (u from −π on the
left to π on the right, v from −π/2 at the bottom to π/2 at the top), first
only the broad stripes, then their waves, the fine stripes, the red spot;
at the end the same texture on a sphere, seen from the front (+z), lit from
the upper right. A faint grid every π/4 in u and v shows where the
rectangle goes on the sphere.

    python3 src/content/raytracing/bilder/planet.py   (writes next to raytracing.md)
"""
import os

import numpy as np
from PIL import Image

OUT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
W, H = 720, 360          # the rectangle, 2:1 like u and v
SPHERE = 400


def mix(a, b, t):
    t = np.asarray(t)[..., None]
    return np.asarray(a) * (1 - t) + np.asarray(b) * t


def texture(u, v, step):
    """planet_color, with only the parts up to step (1 to 4)."""
    band = 0.5 + 0.5 * np.sin(13 * v + (0.3 * np.sin(5 * u + 8 * v) if step >= 2 else 0))
    if step >= 3:
        band = band + 0.12 * np.sin(45 * v + 2 * np.sin(3 * u))
    color = mix((0.58, 0.32, 0.16), (0.92, 0.84, 0.68), band)
    if step >= 4:
        du = (u - 1.0) / 0.36
        dv = (v + 0.38) / 0.15
        r = np.sqrt(du * du + dv * dv)
        color = mix(color, (0.93, 0.86, 0.72), np.maximum(0, 1 - np.abs(r - 1.15) / 0.35))
        spot = mix((0.88, 0.47, 0.26), (0.62, 0.22, 0.1), r * r)
        color = mix(color, spot, np.minimum(1, np.maximum(0, (1 - r) / 0.2)))
    return color


def grid(u, v, width):
    """1 on the lines every π/4 in u and v, 0 elsewhere; width in radians."""
    def near(x):
        k = x / (np.pi / 4)
        return np.abs(k - np.round(k)) * (np.pi / 4) < width
    return near(u) | near(v)


def save(name, rgb):
    rgb = np.clip(rgb, 0, 1) ** (1 / 1.0)
    Image.fromarray((rgb * 255 + 0.5).astype(np.uint8)).save(os.path.join(OUT, name), quality=90)


x = (np.arange(W) + 0.5) / W
y = (np.arange(H) + 0.5) / H
u = (x * 2 - 1)[None, :] * np.pi + 0 * y[:, None]
v = (0.5 - y)[:, None] * np.pi + 0 * x[None, :]
lines = grid(u, v, 0.012)
for step, name in [(1, 'textur-1-streifen'), (2, 'textur-2-wellen'), (3, 'textur-3-fein'), (4, 'textur-4-fleck')]:
    color = texture(u, v, step)
    color = np.where(lines[..., None], mix(color, (1, 1, 1), 0.45), color)
    save(name + '.webp', color)

# the sphere: n for every pixel of a disc, seen from +z
s = (np.arange(SPHERE) + 0.5) / SPHERE * 2 - 1
nx = s[None, :] + 0 * s[:, None]
ny = -s[:, None] + 0 * s[None, :]
inside = nx * nx + ny * ny < 1
nz = np.sqrt(np.maximum(0, 1 - nx * nx - ny * ny))
su = np.arctan2(nz, nx)
sv = np.arcsin(np.clip(ny, -1, 1))
color = texture(su, sv, 4)
color = np.where(grid(su, sv, 0.012)[..., None], mix(color, (1, 1, 1), 0.45), color)
sun = np.array([0.5, 0.6, 0.62])
sun = sun / np.linalg.norm(sun)
light = 0.25 + 0.85 * np.maximum(0, nx * sun[0] + ny * sun[1] + nz * sun[2])
rgb = color * light[..., None]
alpha = inside.astype(float)
# a soft edge: one pixel of coverage
r = np.sqrt(nx * nx + ny * ny)
alpha = np.clip((1 - r) * SPHERE / 2 + 0.5, 0, 1)
image = np.dstack([np.clip(rgb, 0, 1), alpha])
Image.fromarray((image * 255 + 0.5).astype(np.uint8), 'RGBA').save(os.path.join(OUT, 'textur-kugel.webp'), lossless=True)
