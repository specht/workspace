#!/usr/bin/env python3
"""The street scene of »Gaussian Splatting«: »Artillery Passage« by David
Fletcher (superspl.at/scene/f4897475, CC BY 4.0), 5 million splats, thinned
to fewer for the viewer on the page.

    python3 prepare_street.py scene.ply OUT.csplat KEEP [TURN]

KEEP splats stay: those that show most, opacity multiplied by the area of
their two largest axes; faint and tiny ones (floaters among them) go first.
Splats further from the middle than 99.5 % of all are dropped beforehand.
TURN turns the scene so that y points up: 'x+' a quarter turn around x
((x, y, z) -> (x, -z, y)), 'x-' the other way, 'x2' half a turn, '' none.
Writes a compact format of 16 bytes a splat, the colour only, that
splat_viewer.js reads (.csplat): a header of 40 bytes (b'CSPLAT01', the
count as uint32, the smallest and the largest x, y, z as float32 and four
bytes 0),
then for each splat the position as three uint16 between them, the log of
the three scales as uint8 from -12 to 2, RGBA as uint8 and the rotation in
3 bytes: its largest component dropped (its index in the top 2 bits), the
other three, made positive by the sign of the dropped one, as 7 bits each
from -1/sqrt 2 to 1/sqrt 2. The splats that show most come first.
"""
import sys
import numpy as np


def read(path):
    with open(path, 'rb') as f:
        names, count, offset = [], 0, 0
        while True:
            line = f.readline()
            offset += len(line)
            line = line.decode().strip()
            if line.startswith('element vertex'):
                count = int(line.split()[2])
            elif line.startswith('property'):
                names.append(line.split()[2])
            elif line == 'end_header':
                break
    data = np.memmap(path, dtype='<f4', mode='r', offset=offset, shape=(count, len(names)))
    return {name: data[:, i] for i, name in enumerate(names)}


def main():
    source, out, keep = sys.argv[1], sys.argv[2], int(sys.argv[3])
    turn = sys.argv[4] if len(sys.argv) > 4 else ''
    p = read(source)
    pos = np.stack([p['x'], p['y'], p['z']], 1).astype(np.float32)
    middle = np.median(pos, 0)
    dist = np.linalg.norm(pos - middle, axis=1)
    near = dist <= np.percentile(dist, 99.5)
    s = np.stack([p['scale_0'], p['scale_1'], p['scale_2']], 1).astype(np.float32)
    alpha = 1 / (1 + np.exp(-p['opacity'].astype(np.float32)))
    two = np.sort(s, 1)[:, 1:].sum(1)               # the log of the two largest axes, multiplied
    importance = np.where(near, alpha * np.exp(two), -1)
    chosen = np.argsort(-importance)[:keep]
    chosen = chosen[importance[chosen] > 0]
    pos = pos[chosen] - middle
    rot = np.stack([p['rot_0'], p['rot_1'], p['rot_2'], p['rot_3']], 1)[chosen].astype(np.float64)
    rot /= np.linalg.norm(rot, axis=1, keepdims=True)
    c = np.sqrt(0.5)
    w, x, y, z = rot.T
    if turn == 'x+':      # (x, y, z) -> (x, -z, y), r = (cos 45°, sin 45°, 0, 0)
        pos = np.stack([pos[:, 0], -pos[:, 2], pos[:, 1]], 1)
        rot = np.stack([c * (w - x), c * (x + w), c * (y - z), c * (z + y)], 1)
    elif turn == 'x-':    # (x, y, z) -> (x, z, -y), r = (cos -45°, sin -45°, 0, 0)
        pos = np.stack([pos[:, 0], pos[:, 2], -pos[:, 1]], 1)
        rot = np.stack([c * (w + x), c * (x - w), c * (y + z), c * (z - y)], 1)
    elif turn == 'x2':    # (x, y, z) -> (x, -y, -z), r = (0, 1, 0, 0)
        pos = np.stack([pos[:, 0], -pos[:, 1], -pos[:, 2]], 1)
        rot = np.stack([-x, w, -z, y], 1)
    color = np.clip(0.5 + 0.28209479177387814 * np.stack([p['f_dc_0'], p['f_dc_1'], p['f_dc_2']], 1)[chosen], 0, 1)
    low, high = pos.min(0), pos.max(0)
    span = np.maximum(high - low, 1e-6)
    rec = np.zeros(len(chosen), dtype=[('pos', '<u2', 3), ('scale', 'u1', 3), ('rgba', 'u1', 4), ('rot', 'u1', 3)])
    rec['pos'] = np.round((pos - low) / span * 65535)
    rec['scale'] = np.clip(np.round((s[chosen] + 12) / 14 * 255), 0, 255)
    rec['rgba'] = np.round(np.concatenate([color, alpha[chosen][:, None]], 1) * 255)
    rot /= np.linalg.norm(rot, axis=1, keepdims=True)
    largest = np.argmax(np.abs(rot), 1)
    sign = np.where(rot[np.arange(len(rot)), largest] < 0, -1.0, 1.0)
    keep = np.arange(4)[None, :] != largest[:, None]
    rest = (rot * sign[:, None])[keep].reshape(-1, 3)
    q = np.clip(np.round((rest / np.sqrt(0.5) + 1) / 2 * 127), 0, 127).astype(np.uint32)
    packed = (largest.astype(np.uint32) << 22) | (q[:, 0] << 14) | (q[:, 1] << 7) | q[:, 2]
    rec['rot'] = np.stack([(packed >> 16) & 255, (packed >> 8) & 255, packed & 255], 1)
    header = b'CSPLAT01' + np.array([len(chosen)], '<u4').tobytes() + low.astype('<f4').tobytes() + high.astype('<f4').tobytes() + bytes(4)
    with open(out, 'wb') as f:
        f.write(header)
        f.write(rec.tobytes())
    print(f'{len(chosen)} of {len(dist)} splats, {(40 + rec.nbytes) / 1e6:.1f} MB, middle {middle.round(2)}')


if __name__ == '__main__':
    main()
