"""The picture above the iDCT stepper of »Innerhalb eines Bildes«: 240 × 144
pixels of portrait.png around the girl's eyes (x 352 to 592, y 96 to 240),
as PNG, so that the stepper computes the DCT of exactly the pixels students
read in Python. Its grid of 8 × 8 blocks is the grid of the whole frame;
the block the page starts with, the top of her right iris with its
highlight and the white of the eye, lies at (144, 48).

    src/scripts/drawings python3 src/content/video-compression/bilder/idct_detail.py
"""

import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'video', 'portrait.png')
OUT = os.path.join(HERE, '..', 'idct-detail.png')

Image.open(SOURCE).convert('RGB').crop((352, 96, 592, 240)).save(OUT, optimize=True)
print(OUT, os.path.getsize(OUT))
