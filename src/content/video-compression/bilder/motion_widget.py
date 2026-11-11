"""The two pictures of the block-matching widget (motion_widget.js): frames
5 and 6 of slope.mp4, one after the other, as a P-frame would be predicted
from the frame before it, taken from the film itself at its full 2048
pixels, not from the clip scaled to 960: 512 × 352 pixels around the girl
and her dog, as PNG, so the girl is twice as large and crisp. The camera
moves about a pixel between them; the girl about ten, bobbing as she walks;
her staff, her legs and the dog move each in their own way. Frames other
than those of the tasks (11 and 12), so that the widget shows another
example.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/motion_widget.py spring.webm

spring.webm is the film from Wikimedia Commons (clips/make.py says how to get it).
"""

import os
import subprocess
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
START = 395.0                               # the first frame of slope.mp4 (clips/make.py)
X, Y, W, H = 944, 304, 512, 352             # in the film's 2048 × 858 pixels

data = subprocess.run(['ffmpeg', '-v', 'error', '-ss', str(START), '-i', sys.argv[1], '-frames:v', '8',
                       '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-'], capture_output=True, check=True).stdout
frames = np.frombuffer(data, np.uint8).reshape(-1, 858, 2048, 3)
for name, k in [('motion-before', 5), ('motion-after', 6)]:
    path = os.path.join(HERE, '..', f'{name}.png')
    Image.fromarray(frames[k][Y:Y + H, X:X + W]).save(path, optimize=True)
    print(path, os.path.getsize(path))
