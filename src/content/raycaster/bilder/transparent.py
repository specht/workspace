# Macht aus den PPM-Bildern von dump.c die WebP-Bilder der Seite: die Lücken
# (magenta) werden durchsichtig, damit sie hell und dunkel passen.

# runs in docker/drawings, the same on every machine (src/scripts/drawings)
import os as _os, sys as _sys  # noqa: E401
_sys.path.insert(0, _os.path.join(_os.path.dirname(_os.path.abspath(__file__)), '..', '..', '..', 'scripts'))
import drawings_container  # noqa: E402
drawings_container.ensure()
from PIL import Image
for src, dst in [('palette.ppm', '../palette.webp'), ('mip-1.ppm', '../mipmaps.webp')]:
    im = Image.open(src).convert('RGBA')
    im.putdata([(0, 0, 0, 0) if p[:3] == (255, 0, 255) else p for p in im.getdata()])
    im.save(dst, 'WEBP', lossless=True)
