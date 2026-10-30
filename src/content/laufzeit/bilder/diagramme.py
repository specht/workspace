"""The pictures of »Wie lange dauert das?« drawn in Python, as SVG beside
the page.

    python3 src/content/laufzeit/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/laufzeit/NAME.svg WIDTH

- fib-aufrufe: the calls of fib(5), the naive one, as a tree; a call whose
  value was already computed once (left to right, as the program goes) in
  amber, the cases n < 2 in grey.
- rundfahrten: the first ten sights of berlin-places.tsv where they lie (x
  east, y north), above the shortest round trip (best_tour of the solution,
  16.8 km), below the one that always goes to the nearest place
  (nearest_tour, 18.0 km), both from the Brandenburger Tor. The orders are
  those loesung/laufzeit.rb gives; the script asks it.
"""

import json
import math
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, '..', '..', '..', '..')
sys.path.insert(0, os.path.join(ROOT, 'src', 'scripts'))
import automaton_pictures as ap  # noqa: E402
from diagram_labels import code  # noqa: E402

OUT = os.path.join(HERE, '..')
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
INK = '#1f2937'
GREY = '#6b7280'
BLUE = '#2b5fb4'
AMBER = '#d97706'
PALE = '#eef3fb'
WHITE = '#ffffff'


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/laufzeit/{name}.svg {scale * w}'
    head = ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render)
    head = head.replace('<svg ', f"<svg font-family='{FONT}' ", 1)
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(head + f"<rect width='{w}' height='{h}' fill='{WHITE}'/>" + ''.join(parts) + '</svg>\n')


def text(x, y, s, size=16, color=INK, anchor='middle'):
    return (f"<text x='{x:.1f}' y='{y:.1f}' font-size='{size}' fill='{color}' text-anchor='{anchor}' "
            f"stroke='{WHITE}' stroke-width='5' stroke-linejoin='round' paint-order='stroke'>{s}</text>")


def fib_aufrufe():
    w, h = 1000, 380
    seen = set()
    nodes, edges = [], []
    leaves = [0]

    def call(n, depth):
        """Lays the calls out left to right, as the program makes them; returns x."""
        repeated = n >= 2 and n in seen
        if n >= 2:
            seen.add(n)
            xs = [call(n - 1, depth + 1), call(n - 2, depth + 1)]
            x = sum(xs) / 2
        else:
            x = 60 + leaves[0] * 125
            leaves[0] += 1
        nodes.append((n, x, 45 + depth * 75, 'base' if n < 2 else 'again' if repeated else 'new'))
        return x

    call(5, 0)
    # a parent comes after its children in nodes (post-order): walking the
    # tree again in the same order pairs each call with its two calls
    order = iter(nodes)

    def walk(n, depth):
        if n >= 2:
            a = walk(n - 1, depth + 1)
            b = walk(n - 2, depth + 1)
            me = next(order)
            edges.append((me, a))
            edges.append((me, b))
            return me
        return next(order)

    walk(5, 0)
    parts = []
    for (_, x1, y1, _), (_, x2, y2, _) in edges:
        parts.append(f"<line x1='{x1:.1f}' y1='{y1}' x2='{x2:.1f}' y2='{y2}' stroke='{GREY}' stroke-width='1.8'/>")
    bw, bh = 86, 34
    for n, x, y, kind in nodes:
        stroke, fill, width = {'new': (INK, PALE, 1.8), 'again': (AMBER, WHITE, 3), 'base': (GREY, WHITE, 1.5)}[kind]
        parts.append(f"<rect x='{x - bw / 2:.1f}' y='{y - bh / 2}' width='{bw}' height='{bh}' rx='8' "
                     f"fill='{fill}' stroke='{stroke}' stroke-width='{width}'/>")
        parts.append(code(f'fib({n})', x, y + 5, size=15, anchor='middle', background=False))
    save('fib-aufrufe', w, h, 'Die Aufrufe von fib(5); orange, was schon einmal ausgerechnet wurde', parts)


def tours():
    """places and the two orders from the solution, as rundfahrt.rb gets them."""
    script = (
        'require "./src/content/laufzeit/loesung/laufzeit"; require "json";'
        'sights = File.readlines("src/scripts/collect-streets/street-archive/berlin-places.tsv", chomp: true)'
        '.filter_map { |l| k, n, at = l.split("\\t"); next unless k == "sight"; lon, lat = at.split(",").map(&:to_f); [n, lon, lat] };'
        'places = sights.first(10);'
        'b = best_tour(places); n = nearest_tour(places);'
        'puts JSON.generate(places: places, best: b, nearest: n, best_km: tour_length(b, places), nearest_km: tour_length(n, places))'
    )
    return json.loads(subprocess.run(['ruby', '-e', script], cwd=ROOT, capture_output=True, text=True, check=True).stdout)


# where a name stands beside its point: (dx, dy, anchor)
LABELS = {
    'Brandenburger Tor': (0, 32, 'middle'), 'Berliner Fernsehturm': (14, 0, 'start'),
    'Reichstagsgebäude': (0, -16, 'middle'), 'Rotes Rathaus': (14, 16, 'start'),
    'Berliner Dom': (-6, -16, 'middle'), 'Siegessäule': (0, 30, 'middle'),
    'Schloss Charlottenburg': (0, -16, 'middle'), 'Schloss Bellevue': (0, -16, 'middle'),
    'Checkpoint Charlie': (0, 30, 'middle'), 'Gendarmenmarkt': (14, 5, 'start'),
}


def rundfahrten():
    data = tours()
    places = data['places']
    lat0 = sum(p[2] for p in places) / len(places)
    xs = [p[1] * math.cos(math.radians(lat0)) for p in places]
    ys = [p[2] for p in places]
    w, panel = 1000, 300
    # room on the right for the names beside the eastern points
    scale = (w - 100 - 190) / (max(xs) - min(xs))

    def at(i, top):
        return 80 + (xs[i] - min(xs)) * scale, top + 70 + (max(ys) - ys[i]) * scale

    parts = []
    for top, key, title in [(0, 'best', 'alle Reihenfolgen'), (panel, 'nearest', 'immer zum nächsten Ort')]:
        order = data[key]
        km = data[key + '_km']
        parts.append(text(20, top + 30, f"{title}: {km:.1f} km".replace('.', ','), 18, INK, 'start'))
        points = [at(i, top) for i in order + [order[0]]]
        d = 'M ' + ' L '.join(f'{x:.1f} {y:.1f}' for x, y in points)
        parts.append(f"<path d='{d}' fill='none' stroke='{BLUE}' stroke-width='2.5' stroke-linejoin='round'/>")
        for i, (name, _, _) in enumerate(places):
            x, y = at(i, top)
            start = i == order[0]
            parts.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='{8 if start else 6}' fill='{BLUE if start else WHITE}' "
                         f"stroke='{BLUE}' stroke-width='2.5'/>")
            dx, dy, anchor = LABELS[name]
            parts.append(text(x + dx, y + dy, name, 14, INK, anchor))
    save('rundfahrten', w, 2 * panel, 'Zwei Rundfahrten durch die ersten zehn Sehenswürdigkeiten', parts)


fib_aufrufe()
rundfahrten()
