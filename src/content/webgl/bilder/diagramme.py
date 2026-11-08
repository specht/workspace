"""The drawings of »3D-Grafik mit WebGL«: the pipeline from the vertices to
the picture, and a triangle with its normal and the light.

    python3 src/content/webgl/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/webgl/pipeline.svg 900
    ruby src/scripts/render-svg.rb src/content/webgl/normale.svg 960

One unit of the viewBox is one CSS pixel at the width the page gives the
picture (width / 16 em).
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import math as tex  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
AMBER = '#d97706'


def head(w, h, comment, name):
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; als Bild, hell und dunkel:\n'
            f'       src/scripts/render-svg.rb src/content/webgl/{name}.svg {2 * w} -->\n'
            f'  <defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{INK}"/></marker>'
            f'<marker id="arrow-blue" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{BLUE}"/></marker>'
            f'<marker id="arrow-amber" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{AMBER}"/></marker></defs>\n'
            f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')


def line(x1, y1, x2, y2, color=INK, width=1.5, dash=None, arrow=None):
    d = f' stroke-dasharray="{dash}"' if dash else ''
    a = f' marker-end="url(#{arrow})"' if arrow else ''
    return f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="{width}"{d}{a}/>\n'


def text(x, y, s, color=INK, anchor='middle', size=15, weight='normal'):
    return f'  <text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{s}</text>\n'


def label(t, x, y, size=16, color=INK, anchor='middle'):
    return '  ' + tex(t, x, y, size, color, anchor) + '\n'


def save(name, svg):
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(svg + '</svg>\n')


def pipeline():
    """Two rows that read like a snake: the first from left to right, an
    arrow down, the second from right to left, so that the picture is
    narrow enough for a phone."""
    w, h = 450, 320
    s = head(w, h, 'Die Pipeline in zwei Reihen: Ecken, Vertex-Shader, Rasterung, dann darunter von rechts nach links '
             'Fragment-Shader, Z-Buffer, Bild; die beiden Shader blau.', 'pipeline')
    bw, bh, gap = 126, 92, 30
    xs = [10, 10 + bw + gap, 10 + 2 * (bw + gap)]
    rows = [34, 34 + bh + 70]
    boxes = [((xs[0], rows[0]), 'Ecken', ['Zahlen aus', 'JavaScript'], False),
             ((xs[1], rows[0]), 'Vertex-Shader', ['rechnet jede', 'Ecke um'], True),
             ((xs[2], rows[0]), 'Rasterung', ['findet die Pixel', 'jedes Dreiecks'], False),
             ((xs[2], rows[1]), 'Fragment-Shader', ['färbt jedes', 'Pixel'], True),
             ((xs[1], rows[1]), 'Z-Buffer', ['nur das Nächste', 'bleibt'], False),
             ((xs[0], rows[1]), 'Bild', [], False)]
    for (x, y), title, lines, shader in boxes:
        fill = PALE if shader else WHITE
        stroke = BLUE if shader else INK
        s += f'  <rect x="{x}" y="{y}" width="{bw}" height="{bh}" rx="8" fill="{fill}" stroke="{stroke}" stroke-width="{2.2 if shader else 1.4}"/>\n'
        if lines:
            s += text(x + bw / 2, y + 32, title, BLUE if shader else INK, size=15, weight='bold')
            for k, t in enumerate(lines):
                s += text(x + bw / 2, y + 56 + 17 * k, t, GREY, size=14)
        else:
            s += text(x + bw / 2, y + bh / 2 + 6, title, INK, size=16, weight='bold')
    mid0, mid1 = rows[0] + bh / 2, rows[1] + bh / 2
    s += line(xs[0] + bw + 2, mid0, xs[1] - 3, mid0, INK, 1.5, arrow='arrow')
    s += line(xs[1] + bw + 2, mid0, xs[2] - 3, mid0, INK, 1.5, arrow='arrow')
    s += line(xs[2] + bw / 2, rows[0] + bh + 2, xs[2] + bw / 2, rows[1] - 3, INK, 1.5, arrow='arrow')
    s += line(xs[2] - 2, mid1, xs[1] + bw + 3, mid1, INK, 1.5, arrow='arrow')
    s += line(xs[1] - 2, mid1, xs[0] + bw + 3, mid1, INK, 1.5, arrow='arrow')
    s += text(xs[1] + bw / 2, rows[0] - 12, 'schreibst du', BLUE, size=14)
    s += text(xs[2] + bw / 2, rows[1] + bh + 22, 'schreibst du', BLUE, size=14)
    save('pipeline', s)


def normale():
    w, h = 480, 300
    s = head(w, h, 'Ein Dreieck a, b, c gegen den Uhrzeigersinn, die Kanten b − a und c − a, die Normale n senkrecht darauf, '
             'das Licht aus der Richtung l und der Winkel θ zwischen n und l.', 'normale')
    a, b, c = (100, 250), (350, 262), (165, 168)
    s += f'  <polygon points="{a[0]},{a[1]} {b[0]},{b[1]} {c[0]},{c[1]}" fill="{PALE}" stroke="{BLUE}" stroke-width="2.2"/>\n'
    s += line(a[0], a[1], b[0] - 6, b[1] - 1, INK, 1.6, arrow='arrow')
    s += line(a[0], a[1], c[0] - 6, c[1] + 3, INK, 1.6, arrow='arrow')
    for (x, y), t, dx, dy in [(a, 'a', -14, 6), (b, 'b', 10, 10), (c, 'c', 4, -10)]:
        s += f'  <circle cx="{x}" cy="{y}" r="4" fill="{INK}"/>\n'
        s += label(t, x + dx, y + dy, 17)
    s += label('b - a', (a[0] + b[0]) / 2 + 6, (a[1] + b[1]) / 2 + 26, 16)
    s += label('c - a', (a[0] + c[0]) / 2 - 30, (a[1] + c[1]) / 2 - 8, 16)
    # the normal from the middle of the triangle, up
    mx, my = (a[0] + b[0] + c[0]) / 3, (a[1] + b[1] + c[1]) / 3
    s += line(mx, my, mx + 6, my - 160, BLUE, 2.6, arrow='arrow-blue')
    s += label('n', mx - 10, my - 150, 18, BLUE)
    # the light, coming from the upper right
    lx, ly = mx + 140, my - 110
    s += f'  <circle cx="{lx + 18}" cy="{ly - 18}" r="13" fill="{AMBER}"/>\n'
    s += line(mx, my, lx, ly, AMBER, 2.4, arrow='arrow-amber')
    s += label('l', lx + 8, ly + 24, 18, AMBER)
    # theta between n and l
    r = 60
    an = m.atan2(-160, 6)
    al = m.atan2(ly - my, lx - mx)
    s += (f'  <path d="M{mx + r * m.cos(an):.1f},{my + r * m.sin(an):.1f} A{r},{r} 0 0 1 {mx + r * m.cos(al):.1f},{my + r * m.sin(al):.1f}" '
          f'fill="none" stroke="{INK}" stroke-width="1.3"/>\n')
    mid = (an + al) / 2
    s += label(r'\theta', mx + 80 * m.cos(mid), my + 80 * m.sin(mid) + 6, 17)
    save('normale', s)


if __name__ == '__main__':
    pipeline()
    normale()
