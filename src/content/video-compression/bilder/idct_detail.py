"""The picture above the iDCT stepper of »Innerhalb eines Bildes«: 160 × 96
pixels of slope.png around the girl's face (x 480 to 640, y 128 to 224), as
PNG, so that the stepper computes the DCT of exactly the pixels students
read in Python. Its grid of 8 × 8 blocks is the grid of the whole frame;
the block the page starts with, the glowing rim of the girl's hair, lies at (72, 32).

    src/scripts/drawings python3 src/content/video-compression/bilder/idct_detail.py
"""

import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'video', 'slope.png')
OUT = os.path.join(HERE, '..', 'idct-detail.png')

Image.open(SOURCE).convert('RGB').crop((480, 128, 640, 224)).save(OUT, optimize=True)
print(OUT, os.path.getsize(OUT))
