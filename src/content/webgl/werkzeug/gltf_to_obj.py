#!/usr/bin/env python3
"""The model of »3D-Grafik mit WebGL«: »Horse Statue 01« by Rico Cilliers
from Poly Haven (polyhaven.com/a/horse_statue_01, CC0), as glTF, turned
into one OBJ file that the page's parseObj reads.

    python3 gltf_to_obj.py horse_statue_01_2k.gltf horse.obj

All primitives of the first mesh go into one object (the horse and its
base share the same textures), morph targets are applied with their
weights (the horse's hooves), the second texture coordinate is flipped
(glTF counts it from the top, OBJ from the bottom), and every face is
written as a/a/a, position, texture coordinate and normal with the same
number.
"""
import json
import os
import sys

import numpy as np

TYPES = {5126: np.float32, 5123: np.uint16, 5125: np.uint32, 5121: np.uint8}
SIZES = {'SCALAR': 1, 'VEC2': 2, 'VEC3': 3, 'VEC4': 4}


def main():
    source, out = sys.argv[1], sys.argv[2]
    gltf = json.load(open(source))
    folder = os.path.dirname(os.path.abspath(source))
    buffers = [open(os.path.join(folder, b['uri']), 'rb').read() for b in gltf['buffers']]

    def accessor(i):
        a = gltf['accessors'][i]
        view = gltf['bufferViews'][a['bufferView']]
        dtype = TYPES[a['componentType']]
        n = SIZES[a['type']]
        start = view.get('byteOffset', 0) + a.get('byteOffset', 0)
        data = np.frombuffer(buffers[view['buffer']], dtype, a['count'] * n, start)
        return data.reshape(a['count'], n) if n > 1 else data

    mesh = gltf['meshes'][0]
    weights = mesh.get('weights', [])
    lines = [f'# »Horse Statue 01« by Rico Cilliers, Poly Haven, CC0 (polyhaven.com/a/horse_statue_01)']
    faces = []
    offset = 0
    for prim in mesh['primitives']:
        pos = accessor(prim['attributes']['POSITION']).astype(np.float64)
        nor = accessor(prim['attributes']['NORMAL']).astype(np.float64)
        uv = accessor(prim['attributes']['TEXCOORD_0']).astype(np.float64)
        for w, target in zip(weights, prim.get('targets', [])):
            pos = pos + w * accessor(target['POSITION'])
            if 'NORMAL' in target:
                nor = nor + w * accessor(target['NORMAL'])
        nor /= np.linalg.norm(nor, axis=1, keepdims=True)
        lines += [f'v {x:.5f} {y:.5f} {z:.5f}' for x, y, z in pos]
        lines += [f'vt {u:.5f} {1 - v:.5f}' for u, v in uv]
        lines += [f'vn {x:.4f} {y:.4f} {z:.4f}' for x, y, z in nor]
        idx = accessor(prim['indices']).reshape(-1, 3) + offset + 1
        faces += [f'f {a}/{a}/{a} {b}/{b}/{b} {c}/{c}/{c}' for a, b, c in idx]
        offset += len(pos)
    with open(out, 'w') as f:
        f.write('\n'.join(lines + faces) + '\n')
    print(f'{offset} corners, {len(faces)} triangles')


if __name__ == '__main__':
    main()
