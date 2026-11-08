"""The drawings of the Handbuch page »Licht und Glanz«: the same beam of
light falling steeply and flatly on a surface (Lambert), and a view mirrored
at the normal.

    python3 src/content/reference/licht/bilder/diagramme.py
    for f in src/content/reference/licht/*.svg; do
        case $f in *-dark.svg) ;; *) ruby src/scripts/render-svg.rb --svg $f;; esac; done

The page shows the SVGs themselves, sharp at every size; --svg writes the
dark one beside each. The drawings of flat and smooth shading and of the
half vector are those of »3D-Grafik mit WebGL« (webgl/bilder/diagramme.py),
copied here as SVG.
"""

import math as m
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', '..', 'scripts'))
from diagram_labels import math as tex  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, BLUE, GREY, LINE, PALE, WHITE = '#1f2937', '#2b5fb4', '#6b7280', '#d1d5db', '#eef3fb', '#ffffff'
AMBER = '#d97706'
W, H = 320, 220


def head(comment, name, w=W, h=H):
    markers = ''.join(
        f'<marker id="arrow-{n}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{col}"/></marker>' for n, col in [('ink', INK), ('blue', BLUE), ('amber', AMBER)])
    return (f'<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
            f'viewBox="0 0 {w} {h}" width="{w}" height="{h}" font-family="IBM Plex Sans, DejaVu Sans, sans-serif">\n'
            f'  <!-- {comment}\n'
            f'       Gemacht von bilder/diagramme.py; die dunkle Fassung\n'
            f'       {name}-dark.svg und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <defs>{markers}</defs>\n'
            f'  <rect width="{w}" height="{h}" fill="{WHITE}"/>\n')


def line(x1, y1, x2, y2, color=INK, width=1.6, arrow=None, dash=None):
    a = f' marker-end="url(#arrow-{arrow})"' if arrow else ''
    d = f' stroke-dasharray="{dash}"' if dash else ''
    return f'  <line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" stroke="{color}" stroke-width="{width}"{a}{d}/>\n'


def label(t, x, y, color=INK, anchor='middle', size=18):
    return '  ' + tex(t, x, y, size, color, anchor) + '\n'


def save(name, svg):
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(svg + '</svg>\n')


def lambert(name, angle, comment, zoom=1.65):
    """a beam of three rays, 60 px wide, falling at angle (degrees from the
    surface); the stretch of surface it lights in amber. The two pictures
    stand side by side in a gallery, which stretches them to about 530 px:
    drawn at that size (zoom), so that the labels are not enlarged"""
    w, h = round(W * zoom), round(H * zoom)
    s = head(comment, name, w, h)
    ground = 175 * zoom
    s += f'  <rect x="0" y="{ground}" width="{w}" height="{h - ground}" fill="{PALE}"/>\n'
    s += line(10, ground, w - 10, ground, INK, 2)
    a = m.radians(angle)
    d = (-m.cos(a), m.sin(a))              # towards the light, y up
    width = 60 * zoom
    lit = width / m.sin(a)                 # the stretch on the ground
    # the light comes from the left: a flat beam needs room there
    cx = w / 2 + 70 * zoom * m.cos(a)
    for k in (-1, 0, 1):
        # where this ray meets the ground
        gx = cx + k * lit / 2
        length = min(150 * zoom, (gx - 8) / max(-d[0], 1e-6))   # not beyond the left edge
        sx, sy = gx + d[0] * length, ground - d[1] * length
        s += line(sx, sy, gx - d[0] * 6, ground + d[1] * 6 - 0.5, AMBER, 2, 'amber')
    s += line(cx - lit / 2, ground, cx + lit / 2, ground, AMBER, 6)
    # the normal and the direction to the light, from the middle
    s += line(cx, ground, cx, ground - 70 * zoom, BLUE, 2.6, 'blue')
    s += label('n', cx + 10, ground - 70 * zoom + 8, BLUE, 'start')
    lx, ly = cx + d[0] * 70 * zoom, ground - d[1] * 70 * zoom
    s += label('l', lx - 6, ly - 8, AMBER, 'end')
    save(name, s)


def mirror():
    s = head('Ein Punkt auf einer Fläche mit der Normale n; v zeigt zur Kamera, r ist v an der Normale gespiegelt: '
             'gleicher Winkel auf der anderen Seite.', 'spiegeln')
    ground = 180
    s += f'  <rect x="0" y="{ground}" width="{W}" height="{H - ground}" fill="{PALE}"/>\n'
    s += line(10, ground, W - 10, ground, INK, 2)
    cx = W / 2
    a = m.radians(40)                      # angle of v and r to the normal
    length = 140
    vx, vy = cx + m.sin(a) * length, ground - m.cos(a) * length
    rx, ry = cx - m.sin(a) * length, ground - m.cos(a) * length
    s += line(cx, ground, cx, ground - 120, BLUE, 2.6, 'blue')
    s += label('n', cx + 10, ground - 112, BLUE, 'start')
    s += line(cx, ground, vx, vy, INK, 2.2, 'ink')
    s += label('v', vx + 8, vy + 12, INK, 'start')
    # the camera at the end of v: Bootstrap Icons »camera-fill« (MIT licence)
    s += (f'  <path transform="translate({vx + 2:.1f} {vy - 30:.1f}) scale(1.6)" fill="{INK}" '
          f'd="M10.5 8.5a2.5 2.5 0 1 1-5 0 2.5 2.5 0 0 1 5 0M2 4a2 2 0 0 0-2 2v6a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2h-1.172a2 2 0 0 1-1.414-.586l-.828-.828A2 2 0 0 0 9.172 2H6.828a2 2 0 0 0-1.414.586l-.828.828A2 2 0 0 1 3.172 4zm.5 2a.5.5 0 1 1 0-1 .5.5 0 0 1 0 1m9 2.5a3.5 3.5 0 1 1-7 0 3.5 3.5 0 0 1 7 0"/>\n')
    s += line(cx, ground, rx, ry, AMBER, 2.4, 'amber')
    s += label('r', rx - 8, ry + 6, AMBER, 'end')
    # the two equal angles
    r0 = 46
    for sign in (1, -1):
        x2, y2 = cx + sign * m.sin(a) * r0, ground - m.cos(a) * r0
        sweep = 1 if sign > 0 else 0
        s += f'  <path d="M{cx:.1f},{ground - r0:.1f} A{r0},{r0} 0 0 {sweep} {x2:.1f},{y2:.1f}" fill="none" stroke="{GREY}" stroke-width="1.4"/>\n'
    save('spiegeln', s)


if __name__ == '__main__':
    lambert('lambert-steil', 75, 'Drei parallele Lichtstrahlen fallen steil auf eine Fläche und beleuchten ein kurzes Stück (orange): '
            'viel Licht auf wenig Fläche, hell.')
    lambert('lambert-flach', 28, 'Dieselben drei Strahlen fallen flach auf die Fläche und verteilen sich auf ein langes Stück: '
            'dasselbe Licht auf viel Fläche, dunkel.')
    mirror()
