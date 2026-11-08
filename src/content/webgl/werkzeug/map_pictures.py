#!/usr/bin/env python3
"""The four maps of the horse as small pictures for the page of
»3D-Grafik mit WebGL« (map-color.webp and the others), from the textures in
dateien/.

    python3 src/content/webgl/werkzeug/map_pictures.py
"""
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
PAGE = os.path.join(HERE, '..')
for name in ['color', 'normal', 'roughness', 'ao']:
    image = Image.open(os.path.join(PAGE, 'dateien', f'horse-{name}.jpg')).convert('RGB')
    image.resize((480, 480), Image.LANCZOS).save(os.path.join(PAGE, f'map-{name}.webp'), quality=85)
