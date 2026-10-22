# Macht aus den PPM-Bildern von dump.c die WebP-Bilder der Seite: die Lücken
# (magenta) werden durchsichtig, damit sie hell und dunkel passen.
from PIL import Image
for src, dst in [('palette.ppm', '../palette.webp'), ('mip-1.ppm', '../mipmaps.webp')]:
    im = Image.open(src).convert('RGBA')
    im.putdata([(0, 0, 0, 0) if p[:3] == (255, 0, 255) else p for p in im.getdata()])
    im.save(dst, 'WEBP', lossless=True)
