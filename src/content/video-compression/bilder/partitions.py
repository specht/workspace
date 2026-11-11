"""The blocks x264 chose for frame 12 of slope.mp4, encoded again with
crf 30 (at the crf 18 of the file it splits almost everywhere, even in the
calm sky; with fewer bits a split only pays where the motion differs), a
P-frame: every
macroblock of 16 × 16 pixels whole or split into two of 16 × 8 or 8 × 16
or four of 8 × 8, each part with its own motion vector, as the decoder
exports them (export_mvs). Each block's edges a dark and a light line,
cut in like the grid of the teaser; a macroblock without a vector,
predicted within the frame, hatched. A region around the girl, the dog and the rocks, four
times enlarged, so a macroblock is as large as the blocks of the teaser;
a photo, so one WebP for both themes. Run in a throwaway container with PyAV, which brings its own ffmpeg:

    docker run --rm -u "$(id -u):$(id -g)" -e HOME=/tmp -v "$PWD:/repo" -w /repo python:3.12-slim \\
        sh -c 'pip install -q --user av==19.0.2 numpy pillow && python3 src/content/video-compression/bilder/partitions.py'
"""

import collections
import io
import os

import av
import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'video', 'slope.mp4')
OUT = os.path.join(HERE, '..', 'partitions.webp')
FRAME = 12
CRF = 30
SCALE = 4
REGION = (272, 112, 752, 320)             # x0, y0, x1, y1 in the frame: the girl, the dog, rocks and grass
LINE = 2                                    # pixels of each line, in the enlarged picture
DARK, LIGHT = (0, 0, 0, 120), (255, 255, 255, 150)


def encoded():
    """slope.mp4 encoded again by x264 with CRF, as the bytes of an MP4"""
    out = io.BytesIO()
    with av.open(SOURCE) as source, av.open(out, 'w', format='mp4') as target:
        stream = target.add_stream('libx264', rate=24)
        stream.width, stream.height, stream.pix_fmt = 960, 400, 'yuv420p'
        stream.options = {'crf': str(CRF)}
        for frame in source.decode(video=0):
            for packet in stream.encode(frame.reformat(format='yuv420p')):
                target.mux(packet)
        for packet in stream.encode():
            target.mux(packet)
    out.seek(0)
    return out


def frame_and_vectors():
    with av.open(encoded()) as container:
        stream = container.streams.video[0]
        stream.codec_context.options = {'flags2': '+export_mvs'}
        for k, frame in enumerate(container.decode(stream)):
            if k == FRAME:
                side = frame.side_data.get('MOTION_VECTORS')
                kind = {1: 'I', 2: 'P', 3: 'B'}[int(frame.pict_type)]
                return frame.to_image(), kind, (side.to_ndarray() if side is not None else [])


def main():
    picture, kind, vectors = frame_and_vectors()
    assert kind == 'P', kind
    width, height = picture.size
    # a part may be listed once for each frame it looks to; its place is enough
    parts = sorted({(int(v['dst_x']) - int(v['w']) // 2, int(v['dst_y']) - int(v['h']) // 2, int(v['w']), int(v['h']))
                    for v in vectors})
    covered = np.zeros((height // 16, width // 16), dtype=bool)
    for x, y, w, h in parts:
        covered[y // 16:(y + h - 1) // 16 + 1, x // 16:(x + w - 1) // 16 + 1] = True
    big = picture.resize((width * SCALE, height * SCALE), Image.LANCZOS).convert('RGBA')
    overlay = Image.new('RGBA', big.size, (0, 0, 0, 0))
    draw = ImageDraw.Draw(overlay)
    # macroblocks predicted within the frame: hatched
    for r, c in zip(*np.nonzero(~covered)):
        x0, y0 = c * 16 * SCALE, r * 16 * SCALE
        for t in range(-16 * SCALE, 16 * SCALE, 8):
            draw.line([(x0 + max(t, 0), y0 + max(-t, 0)), (x0 + min(t + 16 * SCALE, 16 * SCALE), y0 + min(16 * SCALE - t, 16 * SCALE))],
                      fill=(255, 255, 255, 70), width=2)
    # every part's top and left edge, a dark and then a light line, cut in
    # like the grid of the teaser; a macroblock predicted within the frame
    # counts as one part of 16 × 16
    split = np.zeros_like(covered)
    for x, y, w, h in parts:
        if (w, h) != (16, 16):
            split[y // 16, x // 16] = True
    edges = parts + [(c * 16, r * 16, 16, 16) for r, c in zip(*np.nonzero(~covered))]
    for x, y, w, h in edges:
        x0, y0 = x * SCALE, y * SCALE
        for k, colour in ((0, DARK), (LINE, LIGHT)):
            draw.rectangle([x0, y0 + k, x0 + w * SCALE, y0 + k + LINE - 1], fill=colour)
            draw.rectangle([x0 + k, y0, x0 + k + LINE - 1, y0 + h * SCALE], fill=colour)
    x0, y0, x1, y1 = (v * SCALE for v in REGION)
    Image.alpha_composite(big, overlay).convert('RGB').crop((x0, y0, x1, y1)).save(OUT, quality=86)
    counts = collections.Counter((w, h) for _, _, w, h in parts)
    print('split macroblocks:', int(split.sum()), 'of', split.size)
    print(OUT, dict(counts), 'within the frame:', int((~covered).sum()))


main()
