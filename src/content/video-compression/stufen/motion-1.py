import numpy as np
from PIL import Image, ImageDraw

from blocks import split, join, quantize, restore, bits, residual
from difference import luma
from video import read_frames

N = 16
RADIUS = 4


def best_vector(current, reference, y, x, n, radius):
    block = current[y:y + n, x:x + n]
    height, width = reference.shape
    best, best_sad = (0, 0), None
    for dy in range(-radius, radius + 1):
        for dx in range(-radius, radius + 1):
            top, left = y + dy, x + dx
            if top < 0 or left < 0 or top + n > height or left + n > width:
                continue
            sad = np.abs(block - reference[top:top + n, left:left + n]).sum()
            if best_sad is None or sad < best_sad:
                best, best_sad = (dx, dy), sad
    return best


def motion_field(current, reference, n, radius):
    height, width = current.shape
    vectors = np.zeros((height // n, width // n, 2), dtype=int)
    for r in range(height // n):
        for c in range(width // n):
            vectors[r, c] = best_vector(current, reference, r * n, c * n, n, radius)
    return vectors


def draw_vectors(plane, vectors, n, path):
    picture = Image.fromarray(plane.round().clip(0, 255).astype(np.uint8)).convert('RGB')
    draw = ImageDraw.Draw(picture)
    for r, c in np.ndindex(vectors.shape[:2]):
        dx, dy = vectors[r, c]
        cy, cx = r * n + n // 2, c * n + n // 2
        draw.line([(cx, cy), (cx + 4 * dx, cy + 4 * dy)], fill=(255, 210, 0), width=2)
        draw.ellipse([cx - 1, cy - 1, cx + 1, cy + 1], fill=(255, 210, 0))
    picture.save(path)


if __name__ == '__main__':
    for name in ['pan', 'tilt']:
        planes = luma(read_frames(f'{name}.mp4')[:24])
        vectors = motion_field(planes[12], planes[11], N, RADIUS)
        draw_vectors(planes[12], vectors, N, f'{name}-vectors.png')
        values, counts = np.unique(vectors.reshape(-1, 2), axis=0, return_counts=True)
        print(f'{name}: am häufigsten {tuple(int(v) for v in values[counts.argmax()])}')
