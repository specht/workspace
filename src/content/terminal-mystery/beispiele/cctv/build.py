#!/usr/bin/env python3
"""Builds the surveillance frames of »Ein Krimi im Terminal« from the
reference pictures listed in frames.txt (README.md next to this file).

Each picture is used whole: this only applies the camera's look (size,
colour, softness, noise, vignette, JPEG) and the recorder's overlay in the
VGA font. Nothing inside a picture is cut out, moved or painted.

  build.py OUT_DIR
"""
import os, sys
import cv2, numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
FONT = os.path.join(HERE, 'font', 'Px437_IBM_VGA_9x16.ttf')
DATE = '14.03.2026'
W, H = 704, 576
# Camera 1 records in the dark, in black and white; the others in colour
CAMERAS = {
    1: ('CAM 1  AUSSTELLUNG', True),
    2: ('CAM 2  HINTEREINGANG', False),
    3: ('CAM 3  PARKPLATZ', False),
}

def overlay(items):
    """The overlay drawn at half size in the 9x16 font, then doubled pixel
    for pixel: blocky characters like a recorder's."""
    font = ImageFont.truetype(FONT, 16)
    layer = Image.new('LA', (W // 2, H // 2), (0, 0))
    draw = ImageDraw.Draw(layer)
    for (x, y), text in items:
        for ox, oy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
            draw.text((x + ox, y + oy), text, font=font, fill=(0, 255))
        draw.text((x, y), text, font=font, fill=(235, 255))
    return layer.resize((W, H), Image.NEAREST)

def camera(img, label, time, grey, seed):
    rng = np.random.default_rng(seed)
    img = img.astype(np.float32)
    if grey:
        img = img.mean(axis=2, keepdims=True).repeat(3, axis=2)
    else:
        # A cheap colour sensor: washed-out colours
        mean = img.mean(axis=2, keepdims=True)
        img = mean + (img - mean) * 0.55
    # Lifted blacks, a little less contrast, soft
    img = 16 + img * 0.9
    small = cv2.resize(img, (W, H), interpolation=cv2.INTER_AREA)
    small = cv2.GaussianBlur(small, (0, 0), 0.9)
    # Noise, stronger in the dark
    luma = small.mean(axis=2, keepdims=True)
    small += rng.normal(0, 1, (H, W, 1)).astype(np.float32) * (4 + 6 * (1 - luma / 255))
    if not grey:
        small += rng.normal(0, 2.5, small.shape).astype(np.float32)
    # Vignette
    yy, xx = np.mgrid[0:H, 0:W]
    r = np.hypot((xx - W / 2) / (W / 2), (yy - H / 2) / (H / 2))
    small *= (1 - 0.28 * np.clip(r - 0.35, 0, 1) ** 1.5)[..., None]
    pil = Image.fromarray(cv2.cvtColor(np.clip(small, 0, 255).astype(np.uint8), cv2.COLOR_BGR2RGB))
    # In half-size pixels: date and time top left, the camera's name bottom left
    luminance, alpha = overlay([((8, 6), f'{DATE} {time}'), ((8, H // 2 - 6 - 16), label)]).split()
    pil.paste(Image.merge('RGB', (luminance,) * 3), (0, 0), alpha)
    bgr = cv2.cvtColor(np.array(pil), cv2.COLOR_RGB2BGR)
    ok, jpg = cv2.imencode('.jpg', bgr, [cv2.IMWRITE_JPEG_QUALITY, 42])
    return jpg.tobytes()

def frames():
    """(camera, time, picture) for each line of frames.txt."""
    with open(os.path.join(HERE, 'frames.txt'), encoding='utf-8') as f:
        for line in f:
            line = line.split('#', 1)[0].strip()
            if line:
                cam, time, picture = line.split()
                yield int(cam), time, picture

def main(out):
    os.makedirs(out, exist_ok=True)
    for cam, time, picture in frames():
        label, grey = CAMERAS[cam]
        img = cv2.imread(os.path.join(HERE, picture))
        if img is None:
            sys.exit(f'{picture}: not found')
        # The same frame comes out the same on every run
        seed = cam * 1000000 + int(time.replace(':', ''))
        name = f'cam{cam}_{DATE[6:]}{DATE[3:5]}{DATE[:2]}_{time.replace(":", "")}.jpg'
        with open(os.path.join(out, name), 'wb') as f:
            f.write(camera(img, label, time, grey, seed))
        print(name, '<-', picture)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
