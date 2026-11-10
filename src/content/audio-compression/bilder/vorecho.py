"""60 ms of the castanets around the click at 0,8 s: the original, then
after rounding the MDCT coefficients coarsely in long blocks (1.024) and in
short ones (128), as stufen/vorecho.py does. The click goes far beyond the
rows and is cut off; what matters is the quiet part before it.

    src/scripts/drawings python3 src/content/audio-compression/bilder/vorecho.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/audio-compression/vorecho.svg
"""

import os
import runpy
import sys
import wave

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
sys.path.insert(0, os.path.join(HERE, '..', 'stufen'))     # wav.py, which mdct-2.py imports
import automaton_pictures as ap  # noqa: E402

code = runpy.run_path(os.path.join(HERE, '..', 'stufen', 'mdct-2.py'), run_name='drawing')
encode, decode = code['encode'], code['decode']

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, PALE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#f3f4f6', '#7c3aed'
W, ROW, TOP = 560, 100, 10
LEFT, RIGHT = 16, 544
AT, BEFORE, AFTER = 0.8, 0.045, 0.015


def rounded(blocks):
    out = []
    for X in blocks:
        step = np.abs(X).max() / 8
        out.append(np.round(X / step) * step)
    return out


def main():
    with wave.open(os.path.join(HERE, '..', '..', '..', 'static', 'audio', 'kastagnetten.wav'), 'rb') as f:
        rate = f.getframerate()
        x = np.frombuffer(f.readframes(f.getnframes()), dtype='<i2') / 32767
    start, end = int((AT - BEFORE) * rate), int((AT + AFTER) * rate)
    piece = x[start - 4096:end + 4096]
    rows = [('Original', piece)]
    for N, label in [(1024, 'lange Blöcke, 1.024 Zahlen'), (128, 'kurze Blöcke, 128 Zahlen')]:
        rows.append((label, decode(rounded(encode(piece, N)), N, len(piece))))
    H = TOP + len(rows) * ROW + 30
    span = end - start
    xs = lambda i: LEFT + (RIGHT - LEFT) * i / span
    click = xs(int(BEFORE * rate))
    parts = []
    for r, (label, values) in enumerate(rows):
        top = TOP + r * ROW
        mid = top + 62
        parts.append(f"<clipPath id='row{r}'><rect x='{LEFT}' y='{top + 28}' width='{RIGHT - LEFT}' height='68'/></clipPath>")
        parts.append(ap.text(LEFT, top + 18, label, 18, INK, 'start'))
        parts.append(f"<line x1='{LEFT}' y1='{mid}' x2='{RIGHT}' y2='{mid}' stroke='{LINE}' stroke-width='1.5'/>")
        v = values[4096:4096 + span]
        points = ' '.join(f'{xs(i):.1f},{mid - 1100 * v[i]:.1f}' for i in range(0, span, 2))
        parts.append(f"<polyline points='{points}' fill='none' stroke='{PURPLE if r else GREY}' stroke-width='1.6' clip-path='url(#row{r})'/>")
    parts.append(f"<line x1='{click:.1f}' y1='{TOP + 24}' x2='{click:.1f}' y2='{TOP + len(rows) * ROW}' stroke='{GREY}' stroke-width='1.5' stroke-dasharray='4 4'/>")
    parts.append(ap.text(click, H - 6, 'der Klick', 18, GREY))
    parts.append(ap.text(LEFT, H - 6, f'{int(BEFORE * 1000)} ms davor', 18, GREY, 'start'))
    head = ap.svg_head(W, H, 'Vorecho: ein Klick der Kastagnetten, grob gerundet in langen und kurzen Blöcken; gemacht von bilder/vorecho.py, hell und dunkel:',
                       'src/scripts/render-svg.rb src/content/audio-compression/vorecho.svg')
    with open(os.path.join(OUT, 'vorecho.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


main()
