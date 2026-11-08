"""Whitted's picture of 1979 without its print screen: the scan
(whitted-1979.webp, dots about 1.5 px apart at 45°) blurred just enough that
only the greys remain. The page shows the scan at whole screen pixels per
picture pixel (src/static/include/pixel_exact.js); where not even one fits,
it shows this one instead, which has no dots to clash with the screen.

    python3 src/content/raytracing/bilder/whitted_grau.py
"""

# runs in docker/drawings, the same on every machine (src/scripts/drawings)
import os as _os, sys as _sys  # noqa: E401
_sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '..', '..', '..', 'scripts'))
import drawings_container  # noqa: E402
drawings_container.ensure()
import os

from PIL import Image, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
scan = Image.open(os.path.join(os.path.dirname(HERE), 'whitted-1979.webp')).convert('L')
grey = scan.filter(ImageFilter.GaussianBlur(1.0))
grey.save(os.path.join(os.path.dirname(HERE), 'whitted-1979-grau.webp'), quality=92)
