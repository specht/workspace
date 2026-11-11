"""Banding for the start of »Artefakte und Filter«: a strip of sky from the
film at 394 s (the girl and her dog before the mountains), made into a
clip as clips/make.py makes the page's clips (960 × 400, x264 with crf 14),
then encoded again with -qp 42 as the page does. Above the strip of the
clip, below the same strip at -qp 42, frame 24 of each: 480 × 96 pixels of
the sky, three times enlarged with sharp pixels, and in both the same
contrast, three times as strong around the mean colour of the clip's
strip, so that the steps show. A photo, so one WebP for both themes.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/banding.py spring.webm

spring.webm is the film from Wikimedia Commons (clips/make.py says how to get it).
"""

import os
import subprocess
import sys
import tempfile

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', 'banding.webp')
AT = 393.0
X, Y, W, H = 480, 0, 480, 96                 # the strip of sky in the 960 × 400 frame
ZOOM, CONTRAST, GAP = 3, 3, 12


def frame(path, k):
    data = subprocess.run(['ffmpeg', '-v', 'error', '-i', path, '-vf', f'select=eq(n\\,{k})', '-frames:v', '1',
                           '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-'], capture_output=True, check=True).stdout
    return np.frombuffer(data, np.uint8).reshape(400, 960, 3).astype(float)


def main(film):
    with tempfile.TemporaryDirectory() as tmp:
        clip, coarse = os.path.join(tmp, 'sky.mp4'), os.path.join(tmp, 'sky-42.mp4')
        subprocess.run(['ffmpeg', '-v', 'error', '-y', '-ss', str(AT), '-t', '2', '-i', film,
                        '-vf', 'scale=960:-2:flags=lanczos,crop=960:400', '-c:v', 'libx264', '-crf', '14',
                        '-pix_fmt', 'yuv420p', clip], check=True)
        subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', clip, '-c:v', 'libx264', '-qp', '42', coarse], check=True)
        strips = [frame(path, 24)[Y:Y + H, X:X + W] for path in (clip, coarse)]
    mean = strips[0].mean(axis=(0, 1))
    out = Image.new('RGB', (W * ZOOM, 2 * H * ZOOM + GAP), 'white')
    for n, strip in enumerate(strips):
        stretched = np.clip((strip - mean) * CONTRAST + mean, 0, 255).astype(np.uint8)
        out.paste(Image.fromarray(stretched).resize((W * ZOOM, H * ZOOM), Image.NEAREST), (0, n * (H * ZOOM + GAP)))
    out.save(OUT, quality=88)
    print(OUT, out.size)


main(sys.argv[1])
