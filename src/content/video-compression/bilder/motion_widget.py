"""The two pictures of the block-matching widget (motion_widget.js): the
same detail of slope.mp4 at frames 5 and 6, one after the other, as a
P-frame would be predicted from the frame before it,
288 × 192 pixels around the girl and her dog, as PNG. The camera moves
about a pixel between them; the girl's legs and the dog change a little. Frames other than those of the tasks (11 and 12), so that the
widget shows another example.

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/motion_widget.py
"""

import os
import subprocess

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
CLIP = os.path.join(HERE, '..', '..', '..', 'static', 'video', 'slope.mp4')
X, Y, W, H = 416, 112, 288, 192

data = subprocess.run(['ffmpeg', '-v', 'error', '-i', CLIP, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-'],
                      capture_output=True, check=True).stdout
frames = np.frombuffer(data, np.uint8).reshape(-1, 400, 960, 3)
for name, k in [('motion-before', 5), ('motion-after', 6)]:
    path = os.path.join(HERE, '..', f'{name}.png')
    Image.fromarray(frames[k][Y:Y + H, X:X + W]).save(path, optimize=True)
    print(path, os.path.getsize(path))
