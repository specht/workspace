#!/usr/bin/env python3
"""The maps of the ukulele and the clock as pictures for the page of
»3D-Grafik mit WebGL« (map-color.webp and the others), from the textures in
dateien/, at their full 2048 × 2048: a click on a picture opens it large,
and students look closely at what lies where.

    python3 src/content/webgl/werkzeug/map_pictures.py
"""
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
PAGE = os.path.join(HERE, '..')
for source, name in [('ukulele-color', 'color'), ('ukulele-normal', 'normal'), ('ukulele-roughness', 'roughness'),
                     ('clock-ao', 'ao'), ('clock-metal', 'metal')]:
    image = Image.open(os.path.join(PAGE, 'dateien', f'{source}.jpg')).convert('RGB')
    image.save(os.path.join(PAGE, f'map-{name}.webp'), quality=82)
