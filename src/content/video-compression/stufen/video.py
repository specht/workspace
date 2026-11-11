import subprocess

import numpy as np

WIDTH = 960
HEIGHT = 400


def read_frames(path):
    command = ['ffmpeg', '-v', 'error', '-i', path, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-']
    data = subprocess.run(command, capture_output=True, check=True).stdout
    return np.frombuffer(data, dtype=np.uint8).reshape(-1, HEIGHT, WIDTH, 3)


def write_video(path, frames, fps=24):
    count, height, width, _ = frames.shape
    command = ['ffmpeg', '-v', 'error', '-y', '-f', 'rawvideo', '-pix_fmt', 'rgb24',
               '-s', f'{width}x{height}', '-r', str(fps), '-i', '-',
               '-c:v', 'libx264', '-crf', '18', '-pix_fmt', 'yuv420p', path]
    subprocess.run(command, input=frames.astype(np.uint8).tobytes(), check=True)
