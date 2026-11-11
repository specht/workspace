import numpy as np
from PIL import Image

from video import read_frames

FRAME = 30


def smooth_edges(plane, limit, n=8):
    plane = plane.astype(float)
    for view in (plane,):
        edges = np.arange(n, view.shape[1], n)
        step = view[:, edges] - view[:, edges - 1]
        step = np.where(np.abs(step) < limit, step, 0)
        for j in range(4):
            view[:, edges - 1 - j] += step * (7 - 2 * j) / 16
            view[:, edges + j] -= step * (7 - 2 * j) / 16
    return plane


def save_detail(path, frame):
    detail = np.clip(frame, 0, 255).round().astype(np.uint8)[144:304, 384:672]
    Image.fromarray(detail).resize((864, 480), Image.NEAREST).save(path)


if __name__ == '__main__':
    for name in ['slope', 'slope-blocky', 'slope-deblock']:
        save_detail(f'{name}-detail.png', read_frames(f'{name}.mp4')[FRAME])
    frame = read_frames('slope-blocky.mp4')[FRAME]
    smoothed = np.stack([smooth_edges(frame[..., c], 24) for c in range(3)], axis=-1)
    save_detail('slope-smoothed-detail.png', smoothed)
    print('Vier Ausschnitte gespeichert')
