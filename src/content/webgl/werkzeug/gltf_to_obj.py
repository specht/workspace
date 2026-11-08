#!/usr/bin/env python3
"""The models of »3D-Grafik mit WebGL«, from Poly Haven (CC0) as glTF,
turned into OBJ files that the page's parseObj reads:

    python3 gltf_to_obj.py Ukulele_01_2k.gltf ukulele.obj --lay-down \\
        --credit "»Ukulele 01« by Joseph Burgan, Poly Haven, CC0"
    python3 gltf_to_obj.py alarm_clock_01_2k.gltf clock.obj --glass glass.obj \\
        --credit "»Alarm Clock 01« by James Ray Cock, Poly Haven, CC0"

Every mesh of every node goes in, moved, turned and scaled as its node
says (the clock's hands are nodes of their own). Primitives whose material
is see-through (alphaMode BLEND, the clock's glass) go into the file of
--glass instead; without it they are left out. Morph targets are applied
with their weights. --lay-down turns the model a quarter turn around x, so
that what stood upright lies on its back (its front, towards +z, then
faces up, and what was up points away from the viewer): a long ukulele
fits a wide picture better. The second texture coordinate is flipped (glTF counts it
from the top, OBJ from the bottom), and every face is written as a/a/a,
position, texture coordinate and normal with the same number.

--smooth-angle DEGREES computes the normals anew instead of taking the
model's: each corner gets the average of the faces around its point whose
direction differs from its own face by less than DEGREES. The ukulele's
normals are averaged across sharp edges too (the flat bridge plate leans
in the light, one side brighter than the other); with 30 degrees round
edges stay round and sharp ones sharp. Every corner then has a vertex of
its own.
"""
import argparse
import json
import os

import numpy as np

TYPES = {5126: np.float32, 5123: np.uint16, 5125: np.uint32, 5121: np.uint8}
SIZES = {'SCALAR': 1, 'VEC2': 2, 'VEC3': 3, 'VEC4': 4}


def quaternion_matrix(q):
    x, y, z, w = q
    return np.array([[1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y)],
                     [2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x)],
                     [2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)]])


def node_matrix(node):
    m = np.eye(4)
    if 'matrix' in node:
        return np.array(node['matrix'], dtype=np.float64).reshape(4, 4).T
    r = quaternion_matrix(node.get('rotation', [0, 0, 0, 1]))
    s = np.diag(node.get('scale', [1, 1, 1]))
    m[:3, :3] = r @ s
    m[:3, 3] = node.get('translation', [0, 0, 0])
    return m


def main():
    p = argparse.ArgumentParser()
    p.add_argument('source')
    p.add_argument('out')
    p.add_argument('--glass')
    p.add_argument('--credit', default='')
    p.add_argument('--lay-down', action='store_true')
    p.add_argument('--smooth-angle', type=float)
    a = p.parse_args()

    gltf = json.load(open(a.source))
    folder = os.path.dirname(os.path.abspath(a.source))
    buffers = [open(os.path.join(folder, b['uri']), 'rb').read() for b in gltf['buffers']]

    def accessor(i):
        acc = gltf['accessors'][i]
        view = gltf['bufferViews'][acc['bufferView']]
        n = SIZES[acc['type']]
        start = view.get('byteOffset', 0) + acc.get('byteOffset', 0)
        data = np.frombuffer(buffers[view['buffer']], TYPES[acc['componentType']], acc['count'] * n, start)
        return data.reshape(acc['count'], n) if n > 1 else data

    turn = np.eye(4)
    if a.lay_down:
        turn[:3, :3] = [[1, 0, 0], [0, 0, 1], [0, -1, 0]]   # (x, y, z) -> (x, z, -y)

    # the nodes with their whole transform, parents first
    out = {'solid': [], 'glass': []}

    def visit(index, parent):
        node = gltf['nodes'][index]
        m = parent @ node_matrix(node)
        if 'mesh' in node:
            mesh = gltf['meshes'][node['mesh']]
            weights = node.get('weights', mesh.get('weights', []))
            for prim in mesh['primitives']:
                material = gltf['materials'][prim['material']] if 'material' in prim else {}
                kind = 'glass' if material.get('alphaMode') == 'BLEND' else 'solid'
                pos = accessor(prim['attributes']['POSITION']).astype(np.float64)
                nor = accessor(prim['attributes']['NORMAL']).astype(np.float64)
                uv = accessor(prim['attributes']['TEXCOORD_0']).astype(np.float64)
                for w, target in zip(weights, prim.get('targets', [])):
                    pos = pos + w * accessor(target['POSITION'])
                    if 'NORMAL' in target:
                        nor = nor + w * accessor(target['NORMAL'])
                pos = pos @ m[:3, :3].T + m[:3, 3]
                nor = nor @ np.linalg.inv(m[:3, :3])
                nor /= np.linalg.norm(nor, axis=1, keepdims=True)
                out[kind].append((pos, uv, nor, accessor(prim['indices']).reshape(-1, 3)))
        for child in node.get('children', []):
            visit(child, m)

    scene = gltf['scenes'][gltf.get('scene', 0)]
    for index in scene['nodes']:
        visit(index, turn)

    def smooth(pos, uv, nor, idx):
        # one vertex per corner, its normal from the faces around its point
        # that face almost the same way
        corners = idx.reshape(-1)
        p = pos[corners]
        face = np.cross(p[1::3] - p[0::3], p[2::3] - p[0::3])        # area-weighted
        unit = face / (np.linalg.norm(face, axis=1, keepdims=True) + 1e-20)
        keys = [tuple(k) for k in np.round(p / 1e-5).astype(np.int64)]
        around = {}
        for c, key in enumerate(keys):
            around.setdefault(key, []).append(c // 3)
        limit = np.cos(np.radians(a.smooth_angle))
        normals = np.zeros((len(corners), 3))
        for c, key in enumerate(keys):
            faces = np.array(around[key])
            near = faces[unit[faces] @ unit[c // 3] > limit]
            normals[c] = face[near].sum(0)
        normals /= np.linalg.norm(normals, axis=1, keepdims=True) + 1e-20
        return p, uv[corners], normals, np.arange(len(corners)).reshape(-1, 3)

    if a.smooth_angle:
        out = {kind: [smooth(*part) for part in parts] for kind, parts in out.items()}

    def write(path, parts):
        lines = [f'# {a.credit}'] if a.credit else []
        faces = []
        offset = 0
        for pos, uv, nor, idx in parts:
            lines += [f'v {x:.5f} {y:.5f} {z:.5f}' for x, y, z in pos]
            lines += [f'vt {u:.5f} {1 - v:.5f}' for u, v in uv]
            lines += [f'vn {x:.4f} {y:.4f} {z:.4f}' for x, y, z in nor]
            faces += [f'f {i}/{i}/{i} {j}/{j}/{j} {k}/{k}/{k}' for i, j, k in idx + offset + 1]
            offset += len(pos)
        with open(path, 'w') as f:
            f.write('\n'.join(lines + faces) + '\n')
        every = np.concatenate([p[0] for p in parts])
        print(f'{path}: {offset} corners, {len(faces)} triangles, from {every.min(0).round(3)} to {every.max(0).round(3)}')

    write(a.out, out['solid'])
    if a.glass and out['glass']:
        write(a.glass, out['glass'])


if __name__ == '__main__':
    main()
