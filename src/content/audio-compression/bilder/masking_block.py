"""The masking threshold of one block of musik.wav (»Die Mithörschwelle
eines Blocks«): for each of the 25 bands its level in dB SPL as a grey
step, the error allowed in it (allowed in codec.py) as a purple line, and
the threshold in quiet (QUIET) dashed. Where a band's own level is below
what is allowed, everything in it may be rounded to 0: those bands are
shaded. The bands side by side, one Bark each, the Hz under them.

Two steps. First the numbers, with the page's own code (stufen/codec-2.py,
ear.py, mdct-3.py, wav.py) in the Workspace image, written to
masking-block.json beside this script:

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/audio-compression/bilder/masking_block.py compute

then the drawing:

    src/scripts/drawings python3 src/content/audio-compression/bilder/masking_block.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/masking-block.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/masking-block-wide.svg
"""

import json
import math
import os
import shutil
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, 'masking-block.json')
BLOCK = 60                     # about 1,4 s into musik.wav: bass, chords, melody and quiet bands


def compute():
    page = os.path.join(HERE, '..')
    with tempfile.TemporaryDirectory() as tmp:
        for source, target in [('stufen/codec-2.py', 'codec.py'), ('stufen/ear.py', 'ear.py'),
                               ('stufen/mdct-3.py', 'mdct.py'), ('stufen/wav.py', 'wav.py')]:
            shutil.copy(os.path.join(page, source), os.path.join(tmp, target))
        sys.path.insert(0, tmp)
        import numpy as np
        from codec import BANDS, CAL, CENTRES, QUIET, allowed
        from mdct import encode
        from wav import read_wav
        samples, rate = read_wav(os.path.join(page, '..', '..', 'static', 'audio', 'musik.wav'))
        X = encode(samples, 1024)[BLOCK]
        level = np.array([10 * np.log10(np.sum(X[a:b] ** 2) + 1e-20) + CAL for a, b in BANDS])
        data = {'block': BLOCK, 'centres': CENTRES.tolist(), 'level': level.tolist(),
                'allowed': allowed(level).tolist(), 'quiet': QUIET.tolist()}
    with open(DATA, 'w') as f:
        json.dump(data, f, indent=1)
    print(DATA)


def bark(f):
    return 13 * math.atan(0.00076 * f) + 3.5 * math.atan((f / 7500) ** 2)


def draw():
    sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
    import automaton_pictures as ap
    INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
    data = json.load(open(DATA))
    n = len(data['level'])

    def picture(name, width, size):
        left, right, top, bottom = 52, width - 16, 20 + 2 * (size + 8), 260
        lo, hi = 0, 100
        bw = (right - left) / n
        y = lambda db: bottom - (bottom - top) * (min(max(db, lo), hi) - lo) / (hi - lo)
        parts = []
        # the legend
        for k, (label, colour, dash, w) in enumerate([('Lautstärke des Bandes', GREY, '', 6),
                                                       ('erlaubter Fehler (Mithörschwelle)', PURPLE, '', 3),
                                                       ('Hörschwelle in der Stille', INK, " stroke-dasharray='6 4'", 2)]):
            if width < 600:
                lx, ly = left, 14 + k * (size + 6)
            else:
                # one after the other, each as long as its label
                lx = left + sum(28 + 0.62 * size * len(t) + 30 for t in ('Lautstärke des Bandes', 'erlaubter Fehler (Mithörschwelle)')[:k])
                ly = 18
            parts.append(f"<line x1='{lx}' y1='{ly - 4}' x2='{lx + 22}' y2='{ly - 4}' stroke='{colour}' stroke-width='{w}'{dash}/>")
            parts.append(ap.text(lx + 28, ly, label, size, colour if colour != GREY else INK, 'start'))
        if width < 600:
            top = 20 + 3 * (size + 6) + 6
            y = lambda db: bottom - (bottom - top) * (min(max(db, lo), hi) - lo) / (hi - lo)
        # the dB axis
        for db in range(0, 101, 20):
            parts.append(f"<line x1='{left}' y1='{y(db):.1f}' x2='{right}' y2='{y(db):.1f}' stroke='{LINE}' stroke-width='1'/>")
            parts.append(ap.text(left - 6, y(db) + size * 0.35, f'{db}', size, GREY, 'end'))
        parts.append(ap.text(8, top - 8, 'dB SPL', size, GREY, 'start'))
        # bands whose level is below what is allowed: shaded, nothing to store
        for i in range(n):
            x0 = left + i * bw
            if data['level'][i] < data['allowed'][i]:
                parts.append(f"<rect x='{x0:.1f}' y='{top}' width='{bw:.1f}' height='{bottom - top}' fill='{PURPLE}' fill-opacity='0.08'/>")
            parts.append(f"<line x1='{x0 + 2:.1f}' y1='{y(data['level'][i]):.1f}' x2='{x0 + bw - 2:.1f}' y2='{y(data['level'][i]):.1f}' stroke='{GREY}' stroke-width='6'/>")
        # the allowed error and the threshold in quiet, as steps over the bands
        for key, colour, dash, w in [('allowed', PURPLE, '', 3), ('quiet', INK, " stroke-dasharray='6 4'", 2)]:
            d = []
            for i in range(n):
                x0, x1 = left + i * bw, left + (i + 1) * bw
                d.append(f"{'M' if i == 0 else 'L'} {x0:.1f} {y(data[key][i]):.1f} L {x1:.1f} {y(data[key][i]):.1f}")
            parts.append(f"<path d='{' '.join(d)}' fill='none' stroke='{colour}' stroke-width='{w}'{dash}/>")
        # Hz under the bands, where their Bark is
        centres = data['centres']
        marks = [(100, '100'), (500, '500'), (1000, '1.000'), (2000, '2.000'), (5000, '5.000'), (10000, '10.000 Hz')]
        if width < 600:
            marks = [(100, '100'), (1000, '1.000'), (5000, '5.000 Hz')]
        for f, label in marks:
            z = bark(f)
            # the band whose centre is nearest, and between
            i = min(range(n), key=lambda k: abs(centres[k] - z))
            frac = 0.5 + (z - centres[i]) / ((centres[min(i + 1, n - 1)] - centres[max(i - 1, 0)]) / 2 or 1)
            x = left + (i + max(0, min(1, frac))) * bw
            parts.append(f"<line x1='{x:.1f}' y1='{bottom}' x2='{x:.1f}' y2='{bottom + 5}' stroke='{GREY}' stroke-width='1.5'/>")
            parts.append(ap.text(x, bottom + 8 + size, label, size, GREY, 'end' if label.endswith('Hz') else 'middle'))
        height = bottom + size + 16
        head = ap.svg_head(width, round(height), 'Die Mithörschwelle eines Blocks der Musik: die Lautstärke jedes Bandes, der erlaubte Fehler und die Hörschwelle; gemacht von bilder/masking_block.py, hell und dunkel:',
                           f'src/scripts/render-svg.rb src/content/audio-compression/{name}.svg')
        with open(os.path.join(HERE, '..', f'{name}.svg'), 'w') as f:
            f.write(head + ''.join(parts) + '</svg>\n')

    picture('masking-block-wide', 960, 15)
    picture('masking-block', 440, 13)


if len(sys.argv) > 1 and sys.argv[1] == 'compute':
    compute()
else:
    draw()
