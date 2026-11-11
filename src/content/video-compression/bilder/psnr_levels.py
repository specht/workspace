"""What a PSNR feels like (»Wie gut ist ein Bild?«): portrait.png encoded
by x264 as a single frame with the quantizer that comes nearest to 40, 35
and 30 dB, measured as the page measures (psnr of luma, Y from 0 to 255),
and the same crop of the girl's face from each, three times enlarged with
sharp pixels, beside the original. Photos, so WebP for both themes:
psnr-original.webp, psnr-40.webp, psnr-35.webp, psnr-30.webp.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/psnr_levels.py
"""

import os
import subprocess
import tempfile

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'video', 'portrait.png')
OUT = os.path.join(HERE, '..')
CROP = (390, 110, 630, 290)            # x0, y0, x1, y1: eyes, nose and the leaf
ZOOM = 3


def luma(rgb):
    return rgb.astype(float) @ np.array([0.299, 0.587, 0.114])


def psnr(a, b):
    error = np.mean((a - b) ** 2)
    return float('inf') if error == 0 else 10 * np.log10(255 ** 2 / error)


def encoded(qp, tmp):
    mp4, png = os.path.join(tmp, f'{qp}.mp4'), os.path.join(tmp, f'{qp}.png')
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', SOURCE, '-c:v', 'libx264', '-qp', str(qp),
                    '-pix_fmt', 'yuv420p', mp4], check=True)
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', mp4, '-frames:v', '1', png], check=True)
    return np.asarray(Image.open(png).convert('RGB'))


def save(name, rgb):
    x0, y0, x1, y1 = CROP
    Image.fromarray(rgb[y0:y1, x0:x1]).resize(((x1 - x0) * ZOOM, (y1 - y0) * ZOOM), Image.NEAREST) \
        .save(os.path.join(OUT, name), quality=90)


def main():
    original = np.asarray(Image.open(SOURCE).convert('RGB'))
    y = luma(original)
    save('psnr-original.webp', original)
    with tempfile.TemporaryDirectory() as tmp:
        results = {qp: encoded(qp, tmp) for qp in range(20, 52)}
        measured = {qp: psnr(y, luma(rgb)) for qp, rgb in results.items()}
        for target in (40, 35, 30):
            qp = min(measured, key=lambda q: abs(measured[q] - target))
            save(f'psnr-{target}.webp', results[qp])
            print(f'{target} dB: qp {qp}, {measured[qp]:.1f} dB')


main()
