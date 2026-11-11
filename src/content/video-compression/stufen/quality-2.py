import os
import subprocess

import numpy as np
from PIL import Image, ImageDraw, ImageFont

from video import read_frames

CODECS = {
    'H.264': (['-c:v', 'libx264', '-crf'], [18, 23, 28, 33, 38], (37, 99, 235)),
    'H.265': (['-c:v', 'libx265', '-x265-params', 'log-level=error', '-crf'], [18, 23, 28, 33, 38], (22, 163, 74)),
    'AV1': (['-c:v', 'libsvtav1', '-svtav1-params', 'lp=1', '-crf'], [25, 32, 39, 46, 53], (220, 38, 38)),
}


def luma(frames):
    return frames @ np.array([0.299, 0.587, 0.114])


def psnr(original, decoded):
    error = np.mean((original.astype(float) - decoded.astype(float)) ** 2)
    if error == 0:
        return float('inf')
    return 10 * np.log10(255 ** 2 / error)


def encode(source, target, options):
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', source, *options, target], check=True, capture_output=True)


def measure(source, target):
    return os.path.getsize(target), psnr(luma(read_frames(source)), luma(read_frames(target)))


def curve(source, name, options, crfs):
    points = []
    for crf in crfs:
        target = f'curve-{name}-{crf}.mp4'
        encode(source, target, options + [str(crf)])
        points.append(measure(source, target))
    return points


def draw_curves(curves, path, width=900, height=520):
    picture = Image.new('RGB', (width, height), 'white')
    draw = ImageDraw.Draw(picture)
    font = ImageFont.load_default(size=18)
    left, right, top, bottom = 70, width - 50, 20, height - 50
    most = max(size for points, _ in curves.values() for size, _ in points)
    low, high = 28, 44
    x = lambda size: left + (right - left) * size / most
    y = lambda db: bottom - (bottom - top) * (db - low) / (high - low)
    for db in range(low, high + 1, 4):
        draw.line([left, y(db), right, y(db)], fill=(225, 225, 225))
        draw.text((10, y(db) - 10), f'{db} dB', fill='black', font=font)
    for kb in range(0, int(most / 1000) + 1, 200):
        draw.text((x(kb * 1000) - 15, bottom + 12), f'{kb} KB', fill='black', font=font)
    for n, (name, (points, colour)) in enumerate(curves.items()):
        corners = [(x(size), y(db)) for size, db in sorted(points)]
        draw.line(corners, fill=colour, width=3)
        for cx, cy in corners:
            draw.ellipse([cx - 5, cy - 5, cx + 5, cy + 5], fill=colour)
        draw.text((right - 90, bottom - 30 - 26 * n), name, fill=colour, font=font)
    picture.save(path)


if __name__ == '__main__':
    curves = {}
    for name, (options, crfs, colour) in CODECS.items():
        points = curve('slope.mp4', name, options, crfs)
        curves[name] = (points, colour)
        for crf, (size, quality) in zip(crfs, points):
            print(f'{name:6} crf {crf:2}  {size:7} Bytes  {quality:5.2f} dB')
    draw_curves(curves, 'curves.png')
