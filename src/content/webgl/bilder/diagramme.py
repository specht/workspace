"""The drawings of »3D-Grafik mit WebGL«: the pipeline from the vertices to
the picture, and a triangle with its normal and the light.

    python3 src/content/webgl/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/webgl/pipeline.svg 760
    ruby src/scripts/render-svg.rb src/content/webgl/pipeline-wide.svg 2108
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
            f'<path d="M0,0 L10,5 L0,10 z" fill="{AMBER}"/></marker>'
            f'<marker id="arrow-grey" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
            f'<path d="M0,0 L10,5 L0,10 z" fill="{GREY}"/></marker></defs>\n'
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


BOXES = [('Ecken', ['Zahlen aus', 'JavaScript'], False),
         ('Vertex-Shader', ['rechnet jede', 'Ecke um'], True),
         ('Rasterung', ['findet die Pixel', 'jedes Dreiecks'], False),
         ('Fragment-Shader', ['färbt jedes', 'Pixel'], True),
         ('Z-Buffer', ['nur das Nächste', 'bleibt'], False),
         ('Bild', [], False)]


def boxes(places, bw, bh):
    """The six boxes at places (their upper left corners, in the order of
    BOXES), an arrow from each to the next, and »schreibst du« above the
    two shaders."""
    s = ''
    for (x, y), (title, lines, shader) in zip(places, BOXES):
        fill = PALE if shader else WHITE
        stroke = BLUE if shader else INK
        s += f'  <rect x="{x}" y="{y}" width="{bw}" height="{bh}" rx="8" fill="{fill}" stroke="{stroke}" stroke-width="{2.2 if shader else 1.4}"/>\n'
        if lines:
            s += text(x + bw / 2, y + 32, title, BLUE if shader else INK, size=15, weight='bold')
            for k, t in enumerate(lines):
                s += text(x + bw / 2, y + 56 + 17 * k, t, GREY, size=14)
        else:
            s += text(x + bw / 2, y + bh / 2 + 6, title, INK, size=16, weight='bold')
        if shader:
            s += text(x + bw / 2, y - 12, 'schreibst du', BLUE, size=14)
    for (x1, y1), (x2, y2) in zip(places, places[1:]):
        if y1 == y2:   # side by side: from edge to edge, in the middle
            a, b = (x1 + bw + 2, x2 - 3) if x2 > x1 else (x1 - 2, x2 + bw + 3)
            s += line(a, y1 + bh / 2, b, y1 + bh / 2, INK, 1.5, arrow='arrow')
        else:          # one above the other
            s += line(x1 + bw / 2, y1 + bh + 2, x1 + bw / 2, y2 - 3, INK, 1.5, arrow='arrow')
    return s


def pipeline():
    """For a phone: two columns that read like a snake, three rows high,
    so that the boxes keep almost their size."""
    w, h = 380, 434
    s = head(w, h, 'Die Pipeline in drei Reihen zu zwei Kästen: Ecken, Vertex-Shader, darunter von rechts nach links '
             'Rasterung, Fragment-Shader, darunter Z-Buffer, Bild; die beiden Shader blau.', 'pipeline')
    bw, bh = 160, 92
    xs, ys = [8, 8 + bw + 44], [34, 34 + bh + 58, 34 + 2 * (bh + 58)]
    s += boxes([(xs[0], ys[0]), (xs[1], ys[0]), (xs[1], ys[1]), (xs[0], ys[1]), (xs[0], ys[2]), (xs[1], ys[2])], bw, bh)
    save('pipeline', s)


def pipeline_wide():
    """For a wide screen: all six boxes in one row."""
    w, h = 1054, 136
    s = head(w, h, 'Die Pipeline in einer Reihe: Ecken, Vertex-Shader, Rasterung, Fragment-Shader, Z-Buffer, Bild; '
             'die beiden Shader blau.', 'pipeline-wide')
    bw, bh, gap = 148, 92, 30
    s += boxes([(8 + k * (bw + gap), 34) for k in range(6)], bw, bh)
    save('pipeline-wide', s)


def normale():
    w, h = 480, 300
    s = head(w, h, 'Ein Dreieck a, b, c gegen den Uhrzeigersinn, die Kanten b − a und c − a, die Normale n senkrecht darauf, '
             'das Licht aus der Richtung l und der Winkel θ zwischen n und l.', 'normale')
    a, b, c = (100, 250), (350, 262), (165, 168)
    s += f'  <polygon points="{a[0]},{a[1]} {b[0]},{b[1]} {c[0]},{c[1]}" fill="{PALE}" stroke="{BLUE}" stroke-width="2.2"/>\n'
    # the edges as arrows from a, exactly along the sides of the triangle,
    # the tip on the border of the dot at their end
    for end in (b, c):
        dx, dy = end[0] - a[0], end[1] - a[1]
        d = m.hypot(dx, dy)
        s += line(a[0] + 5 * dx / d, a[1] + 5 * dy / d, end[0] - 5 * dx / d, end[1] - 5 * dy / d, INK, 1.6, arrow='arrow')
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


def facets(smooth):
    """A cut through a curved surface made of four flat pieces, lit from the
    upper left. Flat: one normal in the middle of each piece, each piece one
    brightness. Smooth: a normal at each corner, perpendicular to the
    imagined smooth surface (dashed), mixed normals in between (grey), and
    the brightness running from corner to corner."""
    w, h = 340, 175
    name = 'normalen-glatt' if smooth else 'normalen-flach'
    comment = ('Smooth Shading: vier gerade Stücke einer runden Fläche, an jeder Ecke eine Normale senkrecht auf der gedachten '
               'runden Fläche, dazwischen gemischte Normalen, die Helligkeit läuft fließend.' if smooth else
               'Flat Shading: vier gerade Stücke einer runden Fläche, in der Mitte jedes Stücks seine Normale, jedes Stück '
               'in einer einzigen Helligkeit.')
    s = head(w, h, comment, name)
    cx, cy, r = 172, 228, 150
    angles = [m.radians(a) for a in (-54, -27, 0, 27, 54)]
    P = [(cx + r * m.sin(a), cy - r * m.cos(a)) for a in angles]
    light = (-0.55, -0.835)                       # towards the lamp, y down

    def brightness(nx, ny):
        return max(nx * light[0] + ny * light[1], 0)

    def normal_at(a):
        return (m.sin(a), -m.cos(a))
    seg_normals = []
    for (x1, y1), (x2, y2) in zip(P, P[1:]):
        dx, dy = x2 - x1, y2 - y1
        d = m.hypot(dx, dy)
        seg_normals.append((dy / d, -dx / d))
    band = 16
    # the brightness as a band under the surface, amber: the more light, the more amber
    for k, ((x1, y1), (x2, y2)) in enumerate(zip(P, P[1:])):
        if smooth:
            n1, n2 = normal_at(angles[k]), normal_at(angles[k + 1])
            q = [(x1, y1), (x2, y2), (x2 - band * n2[0], y2 - band * n2[1]), (x1 - band * n1[0], y1 - band * n1[1])]
            gid = f'band{k}'
            s += (f'  <defs><linearGradient id="{gid}" gradientUnits="userSpaceOnUse" x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}">'
                  f'<stop offset="0" stop-color="{AMBER}" stop-opacity="{0.1 + 0.9 * brightness(*n1):.2f}"/>'
                  f'<stop offset="1" stop-color="{AMBER}" stop-opacity="{0.1 + 0.9 * brightness(*n2):.2f}"/></linearGradient></defs>\n')
            fill = f'url(#{gid})'
            opacity = ''
        else:
            # the band's corners along the corner normals, so that the
            # pieces meet without overlapping; the brightness is the piece's
            n = seg_normals[k]
            n1, n2 = normal_at(angles[k]), normal_at(angles[k + 1])
            q = [(x1, y1), (x2, y2), (x2 - band * n2[0], y2 - band * n2[1]), (x1 - band * n1[0], y1 - band * n1[1])]
            fill = AMBER
            opacity = f' fill-opacity="{0.1 + 0.9 * brightness(*n):.2f}"'
        s += f'  <polygon points="{" ".join(f"{x:.1f},{y:.1f}" for x, y in q)}" fill="{fill}"{opacity}/>\n'
    if smooth:
        arc = [(cx + r * m.sin(m.radians(a)), cy - r * m.cos(m.radians(a))) for a in range(-58, 59, 2)]
        s += '  <polyline points="' + ' '.join(f'{x:.1f},{y:.1f}' for x, y in arc) + f'" fill="none" stroke="{GREY}" stroke-width="1.2" stroke-dasharray="4 4"/>\n'
    s += '  <polyline points="' + ' '.join(f'{x:.1f},{y:.1f}' for x, y in P) + f'" fill="none" stroke="{INK}" stroke-width="2.2" stroke-linejoin="round"/>\n'
    length = 46
    if smooth:
        # mixed normals between the corners: what the rasterizer hands each pixel
        for k in range(4):
            for t in (1 / 3, 2 / 3):
                x = P[k][0] + t * (P[k + 1][0] - P[k][0])
                y = P[k][1] + t * (P[k + 1][1] - P[k][1])
                a = angles[k] + t * (angles[k + 1] - angles[k])
                nx, ny = normal_at(a)
                s += line(x, y, x + 0.75 * length * nx, y + 0.75 * length * ny, GREY, 1.3, arrow='arrow-grey')
        for a, (x, y) in zip(angles, P):
            nx, ny = normal_at(a)
            s += line(x, y, x + length * nx, y + length * ny, BLUE, 2.4, arrow='arrow-blue')
        for x, y in P:
            s += f'  <circle cx="{x:.1f}" cy="{y:.1f}" r="3.5" fill="{INK}"/>\n'
    else:
        for (x1, y1), (x2, y2), (nx, ny) in zip(P, P[1:], seg_normals):
            x, y = (x1 + x2) / 2, (y1 + y2) / 2
            s += line(x, y, x + length * nx, y + length * ny, BLUE, 2.4, arrow='arrow-blue')
        for x, y in P:
            s += f'  <circle cx="{x:.1f}" cy="{y:.1f}" r="3.5" fill="{INK}"/>\n'
    # the lamp, upper left
    s += f'  <circle cx="34" cy="34" r="13" fill="{AMBER}"/>\n'
    s += label('l', 58, 40, 17, AMBER, 'start')
    save(name, s)


def glanz():
    """A point on a surface with the normal n, the direction l to the lamp,
    v to the camera and h halfway between them, close to n."""
    w, h = 420, 250
    s = head(w, h, 'Ein Punkt auf einer Fläche mit der Normale n nach oben, links oben die Lampe in Richtung l, rechts oben '
             'die Kamera in Richtung v, und h genau zwischen l und v, nahe an n.', 'glanz-vektoren')
    px, py = 210, 210
    s += line(40, py, 380, py, INK, 2.2)
    def ray(angle, length, color, width, arrow):
        a = m.radians(angle)
        return line(px, py, px + length * m.cos(a), py - length * m.sin(a), color, width, arrow=arrow), \
            (px + length * m.cos(a), py - length * m.sin(a))
    la, va = 140, 62                          # angles to the lamp and to the camera, from the right
    ha = (la + va) / 2
    seg, (nx, ny) = ray(90, 150, BLUE, 2.6, 'arrow-blue'); s += seg
    s += label('n', nx + 12, ny + 10, 18, BLUE, 'start')
    seg, (lx, ly) = ray(la, 150, AMBER, 2.4, 'arrow-amber'); s += seg
    s += f'  <circle cx="{lx - 22:.1f}" cy="{ly - 18:.1f}" r="13" fill="{AMBER}"/>\n'
    s += label('l', lx + 6, ly + 26, 18, AMBER, 'start')
    seg, (vx, vy) = ray(va, 160, INK, 2.2, 'arrow'); s += seg
    s += label('v', vx + 10, vy + 6, 18, INK, 'start')
    s += text(vx + 10, vy - 14, 'zur Kamera', GREY, 'start', 14)
    seg, (hx, hy) = ray(ha, 120, GREY, 2, 'arrow-grey'); s += seg
    s += label('h', hx - 4, hy - 10, 18, GREY, 'middle')
    s += f'  <circle cx="{px}" cy="{py}" r="4" fill="{INK}"/>\n'
    save('glanz-vektoren', s)


if __name__ == '__main__':
    pipeline()
    pipeline_wide()
    normale()
    facets(False)
    facets(True)
    glanz()
