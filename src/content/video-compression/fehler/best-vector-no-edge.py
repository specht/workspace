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


def compensate(reference, vectors, n):
    predicted = np.zeros_like(reference)
    for r, c in np.ndindex(vectors.shape[:2]):
        dx, dy = vectors[r, c]
        y, x = r * n, c * n
        predicted[y:y + n, x:x + n] = reference[y + dy:y + dy + n, x + dx:x + dx + n]
    return predicted


def vector_bits(vectors):
    total = 0
    for k in range(2):
        _, counts = np.unique(vectors[..., k], return_counts=True)
        p = counts / counts.sum()
        total += -(p * np.log2(p)).sum() * counts.sum()
    return total


def save_grey(path, plane):
    Image.fromarray(plane.round().clip(0, 255).astype(np.uint8)).save(path)


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

    planes = luma(read_frames('slope.mp4')[:24])
    previous, current = planes[11], planes[12]
    vectors = motion_field(current, previous, N, RADIUS)
    predicted = compensate(previous, vectors, N)
    rest = current - predicted
    levels = quantize(split(rest, 4), 32)
    decoded = predicted + join(restore(levels, 32))
    save_grey('mc-previous.png', previous)
    save_grey('mc-predicted.png', predicted)
    save_grey('mc-rest.png', rest + 128)
    save_grey('mc-decoded.png', decoded)
    alone = bits(quantize(split(residual(current, 4), 4), 32))
    change = bits(quantize(split(current - previous, 4), 32))
    moved = bits(levels) + vector_bits(vectors)
    print(f'Bild allein:      {alone / 8:7.0f} Bytes')
    print(f'Unterschied:      {change / 8:7.0f} Bytes')
    print(f'mit Bewegung:     {moved / 8:7.0f} Bytes, davon Vektoren {vector_bits(vectors) / 8:.0f}')
