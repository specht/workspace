"""The pictures of the parts of »Audio komprimieren« for the card in the
catalog (parts: in the meta block), in the style of the first one
(teaser.py): 1600 × 900, grey for what is given, purple for what the part
is about.

    teil-2  Kleine Tricks: the μ-law curve, its levels dense near zero
    teil-3  Frequenzen: a sound and the three sines it is made of
    teil-4  Was das Ohr nicht hört: the threshold in quiet, a loud band and
            the masking threshold over it, tones above and below
    teil-5  Blöcke und Fenster: a wave under overlapping sine windows
    teil-6  Quantisieren nach Gehör: bands with their level, the masking
            threshold as steps, the noise under it

    src/scripts/drawings python3 src/content/audio-compression/bilder/teile.py
    for n in 2 3 4 5 6; do src/scripts/drawings ruby src/scripts/render-svg.rb --webp src/content/audio-compression/teil-$n.svg; done
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PALE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#f3f4f6', '#7c3aed'
MONO = 'IBM Plex Mono, DejaVu Sans Mono, monospace'
W, H = 1600, 900


def text(x, y, s, size, color, anchor='middle', weight=None):
    w = f" font-weight='{weight}'" if weight else ''
    return (f"<text x='{x:.1f}' y='{y:.1f}' font-family='{MONO}' font-size='{size}' fill='{color}' "
            f"text-anchor='{anchor}'{w}>{s}</text>")


def line(points, color, width, dash=None, fill='none'):
    d = f" stroke-dasharray='{dash}'" if dash else ''
    p = ' '.join(f'{x:.1f},{y:.1f}' for x, y in points)
    return f"<polyline points='{p}' fill='{fill}' stroke='{color}' stroke-width='{width}' stroke-linejoin='round'{d}/>"


def save(name, comment, parts):
    head = ap.svg_head(W, H, comment, f'src/scripts/render-svg.rb (mit der Option webp) src/content/audio-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


def mulaw():
    # x from 0 to 1 to the right, y = mulaw(x) up; 8 levels of y, each a
    # horizontal line and its x on the axis below, dense near 0
    left, right, bottom, top = 250, 1450, 760, 140
    mu = 255
    sx = lambda x: left + (right - left) * x
    sy = lambda y: bottom - (bottom - top) * y
    squeeze = lambda x: math.log(1 + mu * x) / math.log(1 + mu)
    parts = [f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='3'/>",
             f"<line x1='{left}' y1='{bottom}' x2='{left}' y2='{top}' stroke='{LINE}' stroke-width='3'/>"]
    for k in range(1, 9):
        y = k / 8
        x = ((1 + mu) ** y - 1) / mu
        parts.append(f"<line x1='{left}' y1='{sy(y):.1f}' x2='{sx(x):.1f}' y2='{sy(y):.1f}' stroke='{LINE}' stroke-width='2' stroke-dasharray='8 8'/>")
        parts.append(f"<line x1='{sx(x):.1f}' y1='{sy(y):.1f}' x2='{sx(x):.1f}' y2='{bottom}' stroke='{PURPLE}' stroke-width='5'/>")
        parts.append(f"<circle cx='{sx(x):.1f}' cy='{sy(y):.1f}' r='11' fill='{PURPLE}'/>")
    parts.append(line([(sx(i / 400), sy(squeeze(i / 400))) for i in range(401)], GREY, 5))
    parts.append(text(left - 30, top + 10, 'μ-law', 40, GREY, 'end'))
    parts.append(text(right, bottom + 70, 'leise ← Messung → laut', 38, GREY, 'end'))
    save('teil-2', 'Die Karte von Teil 2: die Kurve von μ-law und ihre Stufen; gemacht von bilder/teile.py, als Bild:', parts)


def frequencies():
    # the sum on top, the three sines below it
    left, right = 120, 1480
    freqs = [(1.0, 0.9, 0.0), (2.5, 0.55, 0.7), (6.0, 0.35, 1.9)]
    sx = lambda u: left + (right - left) * u
    parts = []
    rows = [(200, None)] + [(470 + 150 * i, f) for i, f in enumerate(freqs)]
    for r, (mid, f) in enumerate(rows):
        amp = 120 if f is None else 55
        pts = []
        for i in range(801):
            u = i / 800
            v = (sum(a * math.sin(2 * math.pi * k * u + p) for k, a, p in freqs) / 1.8) if f is None else f[1] * math.sin(2 * math.pi * f[0] * u + f[2])
            pts.append((sx(u), mid - amp * v))
        parts.append(f"<line x1='{left}' y1='{mid}' x2='{right}' y2='{mid}' stroke='{LINE}' stroke-width='2'/>")
        parts.append(line(pts, GREY if f is None else PURPLE, 6 if f is None else 5))
    parts.append(text(800, 400, '=', 64, GREY))
    save('teil-3', 'Die Karte von Teil 3: ein Klang und die drei Sinustöne, aus denen er besteht; gemacht von bilder/teile.py, als Bild:', parts)


def ear():
    # log frequency to the right, dB up: the threshold in quiet (grey), a
    # loud band at 1 kHz and its masking threshold (purple, dashed), tones
    left, right, bottom, top = 150, 1450, 800, 100
    f0, f1, lo, hi = 30, 18000, -10, 100
    sx = lambda f: left + (right - left) * math.log(f / f0) / math.log(f1 / f0)
    sy = lambda db: bottom - (bottom - top) * (db - lo) / (hi - lo)

    def quiet(f):
        k = f / 1000
        return 3.64 * k ** -0.8 - 6.5 * math.exp(-0.6 * (k - 3.3) ** 2) + 0.001 * k ** 4

    def bark(f):
        return 13 * math.atan(0.00076 * f) + 3.5 * math.atan((f / 7500) ** 2)

    def spread(dz):
        return 15.81 + 7.5 * (dz + 0.474) - 17.5 * math.sqrt(1 + (dz + 0.474) ** 2)

    freqs = [f0 * (f1 / f0) ** (i / 600) for i in range(601)]
    parts = [f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='3'/>"]
    parts.append(f"<rect x='{sx(900):.1f}' y='{sy(80):.1f}' width='{sx(1120) - sx(900):.1f}' height='{bottom - sy(80):.1f}' fill='{PURPLE}' opacity='0.35'/>")
    parts.append(line([(sx(f), sy(min(hi + 20, quiet(f)))) for f in freqs], GREY, 6))
    # only where the band raises the threshold above the one in quiet
    mask = [(sx(f), sy(75 + spread(bark(f) - bark(1000)))) for f in freqs if 75 + spread(bark(f) - bark(1000)) > quiet(f)]
    parts.append(line(mask, PURPLE, 6, '22 14'))
    for f, db, heard in [(150, 45, True), (600, 35, False), (1600, 50, False), (3000, 60, True), (6000, 25, True)]:
        color = PURPLE if heard else LINE
        parts.append(f"<circle cx='{sx(f):.1f}' cy='{sy(db):.1f}' r='16' fill='{color}'/>")
    parts.append(text(sx(100), bottom + 60, '100 Hz', 34, GREY))
    parts.append(text(sx(1000), bottom + 60, '1 kHz', 34, GREY))
    parts.append(text(sx(10000), bottom + 60, '10 kHz', 34, GREY))
    save('teil-4', 'Die Karte von Teil 4: Hörschwelle und Mithörschwelle neben einem lauten Band; gemacht von bilder/teile.py, als Bild:', parts)


def blocks():
    # a wave, and above it sine windows overlapping by half, every other one purple
    left, right = 100, 1500
    n = 7
    width = (right - left) / ((n + 1) / 2)
    parts = []
    for i in range(n):
        x0 = left + i * width / 2
        pts = [(x0 + width * j / 200, 470 - 300 * math.sin(math.pi * (j + 0.5) / 200)) for j in range(201)]
        color = PURPLE if i % 2 == 0 else GREY
        parts.append(line(pts, color, 6))
    parts.append(f"<line x1='{left}' y1='470' x2='{right}' y2='470' stroke='{LINE}' stroke-width='3'/>")
    wave = [(left + (right - left) * i / 1000, 680 + 90 * (0.7 * math.sin(2 * math.pi * 6.3 * i / 1000) + 0.3 * math.sin(2 * math.pi * 17 * i / 1000 + 1)))
            for i in range(1001)]
    parts.append(line(wave, GREY, 5))
    save('teil-5', 'Die Karte von Teil 5: Sinusfenster, die sich zur Hälfte überlappen, über einer Welle; gemacht von bilder/teile.py, als Bild:', parts)


def quantise():
    # bands as bars (grey: their level), the masking threshold as steps
    # (purple, dashed) and the rounding noise under it (purple, faint)
    left, right, bottom = 120, 1480, 800
    levels = [62, 70, 78, 84, 80, 74, 70, 66, 72, 64, 58, 54, 50, 46, 40, 34]
    width = (right - left) / len(levels)
    parts = [f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='3'/>"]
    scale = lambda db: bottom - 7.5 * db
    allowed = [l - 22 + 4 * math.sin(i * 1.7) for i, l in enumerate(levels)]
    for i, (l, a) in enumerate(zip(levels, allowed)):
        x = left + i * width
        parts.append(f"<rect x='{x + 10:.1f}' y='{scale(l):.1f}' width='{width - 20:.1f}' height='{bottom - scale(l):.1f}' fill='{LINE}'/>")
        parts.append(f"<rect x='{x + 10:.1f}' y='{scale(a):.1f}' width='{width - 20:.1f}' height='{bottom - scale(a):.1f}' fill='{PURPLE}' opacity='0.3'/>")
    steps = []
    for i, a in enumerate(allowed):
        x = left + i * width
        steps += [(x, scale(a)), (x + width, scale(a))]
    parts.append(line(steps, PURPLE, 6, '22 12'))
    save('teil-6', 'Die Karte von Teil 6: Bänder mit ihrer Lautstärke, die Mithörschwelle als Stufen, das Rauschen darunter; gemacht von bilder/teile.py, als Bild:', parts)


mulaw()
frequencies()
ear()
blocks()
quantise()
