"""Sampling in »Ein Spiel am Telefon«: the same wave measured often and
measured rarely, the measurements as dots on stems, joined by a line.

    src/scripts/drawings python3 src/content/telefonspiel/bilder/abtastung.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/telefonspiel/abtastung.svg
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'

W = 640
LEFT, RIGHT = 24, 556               # where the wave runs; the axis goes on to AXIS
AXIS = 616
PANEL_H, AMP = 220, 62              # one panel, and how far the wave swings


def wave(u):
    """the sound: a slow oscillation with a fast, small one on top (u from 0 to 1)"""
    return 0.72 * math.sin(2 * math.pi * 2 * u) + 0.28 * math.sin(2 * math.pi * 11 * u + 0.6)


def text(x, y, s, size=20, color=INK, anchor='start', weight=None):
    w = f" font-weight='{weight}'" if weight else ''
    return f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}'{w}>{s}</text>"


def panel(top, count, title):
    mid = top + 40 + AMP + 14
    x = lambda u: LEFT + (RIGHT - LEFT) * u
    y = lambda v: mid - AMP * v
    parts = [text(LEFT, top + 24, title, 20, INK, weight=600)]
    # the time axis, an arrow to the right
    parts.append(f"<line x1='{LEFT}' y1='{mid}' x2='{AXIS - 11}' y2='{mid}' stroke='{GREY}' stroke-width='1.5'/>")
    parts.append(ap.arrowhead(AXIS, mid, 1, 0, GREY, 12, 5.5))
    parts.append(text(AXIS, mid - 12, 'Zeit', 17, GREY, 'end'))
    # the sound itself, grey
    points = ' '.join(f'{x(i / 600):.1f},{y(wave(i / 600)):.1f}' for i in range(601))
    parts.append(f"<polyline points='{points}' fill='none' stroke='{GREY}' stroke-width='2'/>")
    # the measurements: equal steps, a stem from the axis, a dot
    us = [(i + 0.5) / count for i in range(count)]
    joined = ' '.join(f'{x(u):.1f},{y(wave(u)):.1f}' for u in us)
    parts.append(f"<polyline points='{joined}' fill='none' stroke='{BLUE}' stroke-width='2.5' stroke-linejoin='round'/>")
    for u in us:
        parts.append(f"<line x1='{x(u):.1f}' y1='{mid}' x2='{x(u):.1f}' y2='{y(wave(u)):.1f}' stroke='{BLUE}' stroke-width='1.5'/>")
        parts.append(f"<circle cx='{x(u):.1f}' cy='{y(wave(u)):.1f}' r='5' fill='{BLUE}'/>")
    return parts


def main():
    h = 2 * PANEL_H + 10
    render = 'src/scripts/render-svg.rb src/content/telefonspiel/abtastung.svg'
    head = ap.svg_head(W, h, 'Abtasten: dieselbe Welle oft und selten gemessen; gemacht von bilder/abtastung.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    parts = panel(0, 48, 'Oft gemessen: 48 Samples') + panel(PANEL_H + 10, 12, 'Selten gemessen: 12 Samples')
    with open(os.path.join(OUT, 'abtastung.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


main()
