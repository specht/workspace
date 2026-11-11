"""The data of the widget »Ein Decoder bei der Arbeit« (gop_stepper.js):
17 frames of slope.mp4 from frame 24,

    I B B B P B B B I B B B P B B B I

with an I-frame at both ends and one in the middle, encoded with x264 so
that every vector names its reference frame without doubt: one reference
in each direction (ref=1); a fixed pattern (b-adapt=0, an I-frame every 8,
open-gop so that the B-frames before the middle I-frame may look to it);
the middle B of three a reference (b-pyramid); a fixed quantiser (qp=23,
which x264 makes a little finer for I and coarser for B, as encoders do);
no weighted prediction, so that a block from two references is their
mean. Then decoded with the vectors exported (export_mvs). For every frame:

    the decoded frame, with 32 pixels of its edge repeated around it, so
        that a piece fetched from a little outside the frame is there
    the residual, half as wide and high, for the film strip: the
        decoded frame minus the prediction, at 128 as zero; the prediction
        is what the vectors fetch from the reference frames (bilinear
        between pixels, a little simpler than H.264's six taps, as the
        browser draws it), the mean where a block has two; for an I-frame
        the frame itself, which is what it stores

in a row, the frames under each other in the order they are shown:

    src/static/video/gop-steps-HASH.webp (a new name for new bytes, as
        browsers keep pictures under /video/ for years), and beside it
        gop-steps-HASH-preview.webp, the first frame, light, shown while
        the sheet loads
    src/static/video/gop-steps.json: the sheet's name; per frame its kind, the bytes of its
        packet and of the same frame encoded as an I-frame (the 17 again
        with keyint=1, the same qp), the frames it looks to, how many of
        its macroblocks come from where, its vectors (x, y, w, h, dx and dy
        in quarter pixels, the frame) and its macroblocks predicted within
        the frame

The vectors, kinds and bytes are x264's; the prediction and so the
residual are rebuilt from them (with what deblocking changes). Run in a
throwaway container with PyAV, which brings its own ffmpeg and x264:

    docker run --rm -u "$(id -u):$(id -g)" -e HOME=/tmp -v "$PWD:/repo" -w /repo python:3.12-slim \\
        sh -c 'pip install -q --user --no-warn-script-location av==19.0.2 numpy pillow && python3 src/content/video-compression/bilder/gop_steps.py'
"""

import hashlib
import io
import json
import os

import av
import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
STATIC = os.path.join(HERE, '..', '..', '..', 'static', 'video')
CLIP = os.path.join(STATIC, 'slope.mp4')
START = 24
KINDS = 'IBBBPBBBIBBBPBBBI'
M = 32
THUMB = 2
GOP = ('keyint=8:min-keyint=8:open-gop=1:scenecut=0:bframes=3:b-adapt=0:b-pyramid=normal:'
       'ref=1:weightb=0:weightp=0')
# the same frames, each an I-frame, for the sizes beside them. Both with a
# fixed quantiser, not crf: crf lets x264's rate control move quality
# between frames (the first I-frame of an all-intra file got three times
# the bytes of the next ones), so that the sizes could not be compared
INTRA = 'keyint=1:min-keyint=1:scenecut=0:bframes=0'


def references(k):
    """The frames frame k looks to, earlier (-1) and later (+1), in the
    B-pyramid of x264 with ref=1: in a group from anchor g to g + 4 (an I
    or P on either side), B(g+2) from both anchors, B(g+1) from g and
    B(g+2), B(g+3) from B(g+2) and g + 4; a P from the anchor before it."""
    kind = KINDS[k]
    if kind == 'I':
        return {}
    g = k - k % 4
    if kind == 'P':
        return {-1: k - 4}
    return {1: {-1: g, 1: g + 2}, 2: {-1: g, 1: g + 4}, 3: {-1: g + 2, 1: g + 4}}[k - g]


def plain(k):
    """The other guess, for the check: every B from the anchors around it."""
    if KINDS[k] != 'B':
        return references(k)
    g = k - k % 4
    return {-1: g, 1: g + 4}


def row(wide, stored):
    """One row of the sheet: the decoded frame with its margin, then what
    the frame stores (its residual, or the frame itself for an I-frame),
    half as wide and high, at the top right."""
    hgt, wid = stored.shape[:2]
    small = Image.fromarray(np.clip(np.rint(stored), 0, 255).astype(np.uint8)).resize(
        (wid // THUMB, hgt // THUMB), Image.BOX)
    right = np.full((wide.shape[0], wid // THUMB, 3), 128.0)
    right[:hgt // THUMB] = np.asarray(small, dtype=np.float64)
    return np.concatenate([wide, right], axis=1)


def source_frames():
    with av.open(CLIP) as container:
        frames = []
        for n, frame in enumerate(container.decode(video=0)):
            if n >= START:
                frames.append(frame.to_ndarray(format='rgb24'))
            if len(frames) == len(KINDS):
                return frames
    raise SystemExit('the clip is too short')


def encode(frames, params=GOP):
    out = io.BytesIO()
    with av.open(out, 'w', format='mp4') as container:
        stream = container.add_stream('libx264', rate=24)
        stream.width, stream.height = frames[0].shape[1], frames[0].shape[0]
        stream.pix_fmt = 'yuv420p'
        stream.options = {'qp': '23', 'x264-params': params}
        for picture in frames:
            for packet in stream.encode(av.VideoFrame.from_ndarray(picture, format='rgb24')):
                container.mux(packet)
        for packet in stream.encode():
            container.mux(packet)
    return out.getvalue()


def decode(data):
    """The frames in the order shown, each with its kind, the picture and
    its vectors; the bytes of each; the order in the file."""
    with av.open(io.BytesIO(data)) as container:
        stream = container.streams.video[0]
        packets = [(p.pts, p.size) for p in container.demux(stream) if p.size]
    times = sorted(t for t, _ in packets)
    file_order = [times.index(t) for t, _ in packets]
    sizes = {times.index(t): size for t, size in packets}
    with av.open(io.BytesIO(data)) as container:
        stream = container.streams.video[0]
        stream.codec_context.options = {'flags2': '+export_mvs'}
        shown = []
        for frame in container.decode(stream):
            side = frame.side_data.get('MOTION_VECTORS')
            vectors = side.to_ndarray() if side is not None else []
            shown.append(({1: 'I', 2: 'P', 3: 'B'}[int(frame.pict_type)],
                          frame.to_ndarray(format='rgb24').astype(np.float64), vectors))
    return shown, sizes, file_order


def sample(picture, x0, y0, w, h, dx, dy):
    """The w × h block at (x0 + dx, y0 + dy) of a picture, dx and dy in
    pixels with fractions; bilinear, the edge repeated outside."""
    hgt, wid = picture.shape[:2]
    ys = np.clip(np.arange(h) + y0 + dy, 0, hgt - 1)
    xs = np.clip(np.arange(w) + x0 + dx, 0, wid - 1)
    y_lo, x_lo = np.floor(ys).astype(int), np.floor(xs).astype(int)
    y_hi, x_hi = np.minimum(y_lo + 1, hgt - 1), np.minimum(x_lo + 1, wid - 1)
    fy, fx = (ys - y_lo)[:, None, None], (xs - x_lo)[None, :, None]
    top = picture[y_lo][:, x_lo] * (1 - fx) + picture[y_lo][:, x_hi] * fx
    bottom = picture[y_hi][:, x_lo] * (1 - fx) + picture[y_hi][:, x_hi] * fx
    return top * (1 - fy) + bottom * fy


def predict(decoded, k, vectors, refs):
    """The prediction of frame k from its vectors, how many vectors cover
    each pixel, and the vectors (x, y, w, h, dx and dy in quarter pixels,
    reference)."""
    hgt, wid = decoded[k].shape[:2]
    total = np.zeros((hgt, wid, 3))
    count = np.zeros((hgt, wid))
    listed = []
    for v in vectors:
        w, h = int(v['w']), int(v['h'])
        x0, y0 = int(v['dst_x']) - w // 2, int(v['dst_y']) - h // 2
        qx = int(round(v['motion_x'] * 4 / v['motion_scale']))
        qy = int(round(v['motion_y'] * 4 / v['motion_scale']))
        ref = refs[int(np.sign(v['source']))]
        total[y0:y0 + h, x0:x0 + w] += sample(decoded[ref], x0, y0, w, h, qx / 4, qy / 4)
        count[y0:y0 + h, x0:x0 + w] += 1
        listed.append((x0, y0, w, h, qx, qy, ref))
    prediction = np.where(count[:, :, None] > 0, total / np.maximum(count, 1)[:, :, None], 128.0)
    return prediction, count, listed


def main():
    source = source_frames()
    shown, sizes, file_order = decode(encode(source))
    intra_shown, intra_sizes, _ = decode(encode(source, INTRA))
    assert all(kind == 'I' for kind, _, _ in intra_shown)
    kinds = ''.join(kind for kind, _, _ in shown)
    assert kinds == KINDS, kinds
    decoded = [picture for _, picture, _ in shown]
    hgt, wid = decoded[0].shape[:2]
    rows, frames = [], []
    for k, (kind, picture, vectors) in enumerate(shown):
        wide = np.pad(picture, ((M, M), (M, M), (0, 0)), mode='edge')
        entry = {'kind': kind, 'bytes': sizes[k], 'intraBytes': intra_sizes[k]}
        if kind == 'I':
            rows.append(row(wide, picture))
            frames.append(dict(entry, refs=[], vectors=[], intra=[], blocks={'intra': wid * hgt // 256}))
            continue
        refs = references(k)
        prediction, count, listed = predict(decoded, k, vectors, refs)
        mean = np.abs(picture - prediction)[count > 0].mean()
        if plain(k) != refs:
            other, other_count, _ = predict(decoded, k, vectors, plain(k))
            other_mean = np.abs(picture - other)[other_count > 0].mean()
            print(f'frame {k:2} {kind}: residual {mean:.2f}, with the anchors {other_mean:.2f}')
            assert mean < other_mean, 'the references are not the B-pyramid'
        else:
            print(f'frame {k:2} {kind}: residual {mean:.2f}')
        residual = np.where(count[:, :, None] > 0, picture - prediction + 128, 128)
        rows.append(row(wide, residual))
        # how many macroblocks come from where: per 16 × 16, which frames
        # its vectors name
        uses = {}
        for x0, y0, w, h, _, _, ref in listed:
            for my in range(y0 // 16, (y0 + h - 1) // 16 + 1):
                for mx in range(x0 // 16, (x0 + w - 1) // 16 + 1):
                    uses.setdefault((mx, my), set()).add(ref)
        blocks = {}
        intra = []
        for my in range(hgt // 16):
            for mx in range(wid // 16):
                used = uses.get((mx, my))
                key = 'intra' if not used else 'both' if len(used) > 1 else str(next(iter(used)))
                blocks[key] = blocks.get(key, 0) + 1
                if not used:
                    intra += [mx, my]
        frames.append(dict(entry, refs=sorted(set(refs.values())), blocks=blocks, intra=intra,
                           vectors=[n for v in listed for n in v]))
    sheet = np.clip(np.rint(np.concatenate(rows, axis=0)), 0, 255).astype(np.uint8)
    # the sheet's name carries a hash of its bytes: nginx lets browsers keep
    # pictures under /video/ for years, so a new sheet needs a new address
    # (the JSON, which names it, is not kept)
    buffer = io.BytesIO()
    Image.fromarray(sheet).save(buffer, format='webp', quality=94, method=6)
    sheet_name = f'gop-steps-{hashlib.sha256(buffer.getvalue()).hexdigest()[:10]}.webp'
    for old in os.listdir(STATIC):
        if old.startswith('gop-steps') and old.endswith('.webp'):
            os.remove(os.path.join(STATIC, old))
    with open(os.path.join(STATIC, sheet_name), 'wb') as f:
        f.write(buffer.getvalue())
    # a light picture of the first frame, shown while the sheet loads
    preview_name = sheet_name.replace('.webp', '-preview.webp')
    Image.fromarray(np.clip(np.rint(decoded[0]), 0, 255).astype(np.uint8)).save(
        os.path.join(STATIC, preview_name), quality=70, method=6)
    data = {'sheet': sheet_name, 'preview': preview_name, 'start': START, 'width': wid, 'height': hgt, 'margin': M, 'thumb': THUMB, 'raw': wid * hgt * 3 // 2,
            'order': file_order, 'frames': frames}
    with open(os.path.join(STATIC, 'gop-steps.json'), 'w') as f:
        json.dump(data, f, separators=(',', ':'))
    print('file order', file_order)
    print('bytes', [f['bytes'] for f in frames], sum(f['bytes'] for f in frames))
    print('as I-frames', [f['intraBytes'] for f in frames], sum(f['intraBytes'] for f in frames))
    for k, f in enumerate(frames):
        print(k, f['kind'], f['refs'], f['blocks'])
    print(sheet_name, os.path.getsize(os.path.join(STATIC, sheet_name)), os.path.getsize(os.path.join(STATIC, 'gop-steps.json')))


main()
