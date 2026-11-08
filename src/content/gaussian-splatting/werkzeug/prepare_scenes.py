#!/usr/bin/env python3
"""The scenes of »Gaussian Splatting« from the splats of the DX.GL multi-view
datasets (CC0, trained from Poly Haven models; huggingface.co/datasets/dxgl/
multiview-datasets, the splat_url of each object in its manifest.json).

For each object: turned a quarter turn around x, so that y points up as in
the students' camera (the datasets have z up, as Blender does), and moved so
that the middle of the splats lies at the origin; the colour only from the
first coefficient (f_dc), the view-dependent rest is dropped.

    python3 prepare_scenes.py SOURCE_DIR OUT_DIR

writes OUT_DIR/NAME.ply (the 14 properties of a 3DGS file without the rest
coefficients, for render3d.py in the Workspace) and OUT_DIR/NAME.splat (32
bytes a splat: position and scale as float32, colour and opacity as bytes,
the rotation as four bytes, the format of antimatter15/splat, for the viewer
on the page).
"""
import os
import sys
import numpy as np

KEEP = ['x', 'y', 'z', 'f_dc_0', 'f_dc_1', 'f_dc_2', 'opacity',
        'scale_0', 'scale_1', 'scale_2', 'rot_0', 'rot_1', 'rot_2', 'rot_3']


def read_ply(path):
    with open(path, 'rb') as f:
        names, count = [], 0
        while True:
            line = f.readline().decode().strip()
            if line.startswith('element vertex'):
                count = int(line.split()[2])
            elif line.startswith('property'):
                assert line.split()[1] == 'float', line
                names.append(line.split()[2])
            elif line == 'end_header':
                break
        data = np.frombuffer(f.read(count * 4 * len(names)), dtype='<f4').reshape(count, len(names))
    return {name: data[:, i].astype(np.float64) for i, name in enumerate(names)}


def turn(p):
    # a quarter turn around x: (x, y, z) -> (x, z, -y); the rotation q of
    # every splat becomes r * q with r = (cos -45°, sin -45°, 0, 0)
    p['y'], p['z'] = p['z'], -p['y']
    c = np.sqrt(0.5)
    w, x, y, z = p['rot_0'], p['rot_1'], p['rot_2'], p['rot_3']
    p['rot_0'], p['rot_1'], p['rot_2'], p['rot_3'] = c * (w + x), c * (x - w), c * (y + z), c * (z - y)
    for axis in 'xyz':
        p[axis] = p[axis] - np.median(p[axis])
    return p


def write_ply(path, p):
    n = len(p['x'])
    header = 'ply\nformat binary_little_endian 1.0\nelement vertex %d\n' % n
    header += ''.join('property float %s\n' % name for name in KEEP) + 'end_header\n'
    with open(path, 'wb') as f:
        f.write(header.encode())
        f.write(np.stack([p[name] for name in KEEP], 1).astype('<f4').tobytes())


def write_splat(path, p):
    n = len(p['x'])
    color = np.clip(0.5 + 0.28209479177387814 * np.stack([p['f_dc_0'], p['f_dc_1'], p['f_dc_2']], 1), 0, 1)
    alpha = 1 / (1 + np.exp(-p['opacity']))
    rot = np.stack([p['rot_0'], p['rot_1'], p['rot_2'], p['rot_3']], 1)
    rot /= np.linalg.norm(rot, axis=1, keepdims=True)
    # the largest splats first, as the viewer of antimatter15 sorts them
    size = np.exp(p['scale_0'] + p['scale_1'] + p['scale_2']) * alpha
    order = np.argsort(-size)
    record = np.zeros(n, dtype=[('pos', '<f4', 3), ('scale', '<f4', 3), ('rgba', 'u1', 4), ('rot', 'u1', 4)])
    record['pos'] = np.stack([p['x'], p['y'], p['z']], 1)[order]
    record['scale'] = np.exp(np.stack([p['scale_0'], p['scale_1'], p['scale_2']], 1))[order]
    record['rgba'] = np.round(np.concatenate([color, alpha[:, None]], 1) * 255)[order]
    record['rot'] = np.clip(np.round(rot * 128 + 128), 0, 255)[order]
    with open(path, 'wb') as f:
        f.write(record.tobytes())


def main():
    source, out = sys.argv[1], sys.argv[2]
    os.makedirs(out, exist_ok=True)
    for name in sorted(os.listdir(source)):
        if not name.endswith('.ply'):
            continue
        p = turn(read_ply(os.path.join(source, name)))
        base = os.path.join(out, name[:-4].replace('_', '-'))
        write_ply(base + '.ply', p)
        write_splat(base + '.splat', p)
        print(name, len(p['x']), 'splats')


if __name__ == '__main__':
    main()
