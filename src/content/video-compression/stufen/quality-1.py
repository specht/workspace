import os
import subprocess

import numpy as np

from video import read_frames


def luma(frames):
    return frames @ np.array([0.299, 0.587, 0.114])


def psnr(original, decoded):
    error = np.mean((original.astype(float) - decoded.astype(float)) ** 2)
    if error == 0:
        return float('inf')
    return 10 * np.log10(255 ** 2 / error)


def encode(source, target, options):
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', source, *options, target], check=True, capture_output=True)


def measure(source, target):
    return os.path.getsize(target), psnr(luma(read_frames(source)), luma(read_frames(target)))


if __name__ == '__main__':
    for clip in ['glance', 'tilt']:
        for label, options in [('crf28', ['-c:v', 'libx264', '-crf', '28']),
                               ('400k', ['-c:v', 'libx264', '-b:v', '400k'])]:
            target = f'{clip}-{label}.mp4'
            encode(f'{clip}.mp4', target, options)
            size, quality = measure(f'{clip}.mp4', target)
            print(f'{target:18} {size:7} Bytes  {quality:5.2f} dB')
