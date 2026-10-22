"""The diagrams of »Raytracing in C«, as SVG beside the page (lib.py draws).

    python3 src/content/raytracing/bilder/diagramme.py              # all
    python3 src/content/raytracing/bilder/diagramme.py treffer pixel # some
    ruby src/scripts/render-svg.rb src/content/raytracing/NAME.svg WIDTH

Each picture is drawn at the width the page gives it (viewBox width / 16 em),
so labels at 16 are about as big as the text. Names from the program are
code, formulas LaTeX, right angles the German mark; numbers in code have a
decimal point. In 2D x goes to the right and y up; the 3D pictures keep
x to the right, y up and z towards the viewer. No picture shows a task's
numbers (tasks.yaml).
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from lib import (INK, BLUE, GREY, RED, GREEN, AMBER, PALE, LINE, LIGHT, WHITE, SHADOW,  # noqa: E402
                 add, sub, mul, dot, length, unit, perp, polar, lerp, ray_circle, refract, reflect,
                 add3, sub3, mul3, dot3, cross3, norm3, View, Svg, text_width, code_width, tex_width)


# ------------------------------------------------------------- vectors

def vektoren():
    s = Svg('vektoren', 504, 214, 'Vektoren als Pfeile: add setzt zwei Pfeile aneinander, scale macht einen Pfeil '
            'länger oder kürzer, normalize macht ihn genau 1 lang und behält die Richtung.')
    # add: a, then b from the tip of a; add(a, b) from the start of a to the tip of b
    o = (96, 150)
    a = (70, -30)
    b = (26, -92)
    pa = add(o, a)
    pb = add(pa, b)
    s.arrow(o, pa, BLUE, 2.5)
    s.arrow(pa, pb, BLUE, 2.5)
    s.arrow(o, pb, INK, 2.5)
    s.code(add(lerp(o, pa, 0.55), (4, 22)), 'a')
    s.code(add(lerp(pa, pb, 0.5), (10, 4)), 'b')
    s.code(add(lerp(o, pb, 0.5), (-12, -6)), 'add(a, b)', 'end')
    s.text((120, 200), 'aneinandersetzen', GREY)
    # scale: a, twice as long, half as long
    x0 = 224
    s.arrow((x0, 64), (x0 + 60, 64), BLUE, 2.5)
    s.code((x0 + 68, 69), 'a')
    s.arrow((x0, 112), (x0 + 120, 112), INK, 2.5)
    s.code((x0, 96), 'scale(a, 2)')
    s.arrow((x0, 160), (x0 + 30, 160), INK, 2.5)
    s.code((x0, 144), 'scale(a, 0.5)')
    s.text((x0 + 60, 200), 'länger, kürzer', GREY)
    # normalize: a long arrow, and the same direction with length 1 on the unit circle
    c = (420, 100)
    r = 40
    s.circle(c, r, stroke=GREY, w=1.5, dash='4 4')
    d = unit((1, -0.62))
    s.arrow(c, add(c, mul(d, 64)), BLUE, 2.5)
    s.code(add(c, add(mul(d, 64), (6, 2))), 'a')
    s.arrow(c, add(c, mul(d, r)), INK, 3)
    s.dot(c, 3.5)
    s.code((c[0], c[1] + r + 26), 'normalize(a)', 'middle')
    s.text((c[0], 200), 'Länge 1', GREY)
    return s


def bildebene():
    """From behind and above the eye: the image plane one unit in front (−z),
    the pixel at (sx, sy), the ray through it."""
    w, h = 384, 236
    s = Svg('bildebene', w, h, 'Schräg von hinten über das Auge gesehen: das Auge im Nullpunkt schaut in Richtung −z; '
            'eine Einheit davor steht das Bild mit seinen Pixeln. sx geht nach rechts, sy nach oben; der Strahl durch '
            'diesen Pixel hat die Richtung (sx, sy, -1).')
    v = View(24, 42, 132, (150, 192))
    half_w, half_h = 0.89, 0.5          # W / 2 / H and H / 2 / H
    cols, rows = 8, 5
    # the plane at z = -1
    corners = [(-half_w, -half_h, -1), (half_w, -half_h, -1), (half_w, half_h, -1), (-half_w, half_h, -1)]
    s.polygon([v(p) for p in corners], PALE, INK, 2)
    for i in range(1, cols):
        x = -half_w + 2 * half_w * i / cols
        s.line(v((x, -half_h, -1)), v((x, half_h, -1)), LINE, 1)
    for j in range(1, rows):
        y = -half_h + 2 * half_h * j / rows
        s.line(v((-half_w, y, -1)), v((half_w, y, -1)), LINE, 1)
    # one pixel, right and up
    i, j = 6, 3
    px0, px1 = -half_w + 2 * half_w * i / cols, -half_w + 2 * half_w * (i + 1) / cols
    py0, py1 = -half_h + 2 * half_h * j / rows, -half_h + 2 * half_h * (j + 1) / rows
    s.polygon([v((px0, py0, -1)), v((px1, py0, -1)), v((px1, py1, -1)), v((px0, py1, -1))], BLUE, None, opacity=0.35)
    sx, sy = (px0 + px1) / 2, (py0 + py1) / 2
    eye = (0, 0, 0)
    mid = (0, 0, -1)
    # 1 forward along -z, then sx to the right and sy up
    s.arrow(v(eye), v(mid), BLUE, 2.5)
    s.arrow(v(mid), v((sx, 0, -1)), RED, 2.5)
    s.arrow(v((sx, 0, -1)), v((sx, sy, -1)), GREEN, 2.5)
    s.code(add(v((sx / 2, 0, -1)), (-4, 20)), 'sx', 'middle')
    s.code(add(v((sx, sy / 2, -1)), (8, 6)), 'sy')
    # the ray through the pixel, on into the scene
    far = mul3((sx, sy, -1), 1.75)
    s.line(v(eye), v((sx, sy, -1)), BLUE, 2)
    s.line(v((sx, sy, -1)), v(far), BLUE, 2, dash='5 4')
    s.hollow(v((sx, sy, -1)), 4, BLUE)
    s.code(add(v(far), (-10, -4)), '(sx, sy, -1)', 'end')
    s.dot(v(eye), 6)
    s.text(add(v(eye), (12, 6)), 'Auge, im Nullpunkt', INK, 'start')
    s.label(add(v((0, 0, -0.5)), (-14, 4)), [('m', '-z'), ': 1 nach vorn'], BLUE, 'end')
    s.text(add(v((-half_w, half_h, -1)), (0, -10)), 'das Bild', INK, 'start')
    # the axes, bottom right
    o = (338, 206)
    for d, name, col in [((1, 0, 0), 'x', INK), ((0, 1, 0), 'y', INK), ((0, 0, 1), 'z', INK)]:
        tip = add(o, sub(v(mul3(d, 0.22)), v((0, 0, 0))))
        s.arrow(o, tip, col, 1.8, size=9)
        s.tex(add(tip, mul(unit(sub(tip, o)), 10)), name, col, 'middle')
    return s


# ------------------------------------------------------------- the sphere

def treffer():
    w, h = 480, 250
    s = Svg('treffer', w, h, 'Ein Strahl beginnt bei origin und läuft in Richtung dir (Länge 1). Nach der Strecke t '
            'ist er bei origin + t · dir; dort trifft er den Rand der Kugel mit center und radius. Den Rand hinten '
            'trifft er auch, aber später; ein anderer Strahl geht an der Kugel vorbei.')
    o = (50, 140)
    c = (330, 112)
    r = 70
    d = unit((1, -0.14))
    t0 = ray_circle(o, d, c, r)
    t1 = ray_circle(o, d, c, r, far=True)
    p0, p1 = add(o, mul(d, t0)), add(o, mul(d, t1))
    s.circle(c, r, PALE, INK, 2.5)
    s.line(c, (c[0], c[1] - r), INK, 1.8)
    s.code((c[0] + 10, c[1] - r / 2 + 4), 'radius')
    s.dot(c, 4)
    s.code(add(c, (0, 24)), 'center', 'middle')
    miss = unit((1, -0.62))
    s.arrow(o, add(o, mul(miss, 230)), GREY, 1.6, dash='6 4')
    s.text(add(o, add(mul(miss, 230), (8, 4))), 'daneben', GREY, 'start')
    s.line(p0, add(o, mul(d, t1 + 70)), GREY, 1.6, dash='6 4')
    s.arrow(o, p0, BLUE, 3, gap=5)
    s.dot(p0, 5, BLUE)
    s.hollow(p1, 5, BLUE)
    s.text(add(p1, (10, -10)), 'hinten', GREY, 'start')
    s.code(add(lerp(o, p0, 0.5), (0, -12)), 't', 'middle')
    s.arrow((o[0] - 20, o[1] + 66), add((o[0] - 20, o[1] + 66), mul(d, 40)), INK, 2.2)
    s.label((o[0] + 30, o[1] + 72), [('c', 'dir'), ', Länge 1'])
    s.dot(o, 6)
    s.code((o[0] - 20, o[1] + 28), 'origin')
    s.code(add(p0, (-10, 38)), 'origin + t · dir', 'end')
    return s


def pixel():
    """One ray through the middle of each pixel against many random ones."""
    w, h = 496, 210
    s = Svg('pixel', w, h, 'Der Rand einer Kugel läuft durch ein Gitter aus Pixeln. Links ein Strahl je Pixel durch '
            'die Mitte: Jeder Pixel ist ganz Kugel oder ganz Himmel, der Rand wird eine Treppe. Rechts viele Strahlen '
            'je Pixel an zufälligen Punkten: Ein Pixel am Rand wird so hell, wie viele davon die Kugel treffen.')
    cell = 44
    cols, rows = 5, 3

    def inside(gx, gy, cx, cy, R):
        return math.hypot(gx - cx, gy - cy) < R

    import random
    rnd = random.Random(7)
    for k, x0 in enumerate((14, 266)):
        y0 = 16
        cx, cy, R = x0 + cols * cell + 26, y0 + rows * cell + 110, 190
        for i in range(cols):
            for j in range(rows):
                ax, ay = x0 + i * cell, y0 + j * cell
                if k == 0:
                    on = inside(ax + cell / 2, ay + cell / 2, cx, cy, R)
                    fill, op = (RED, 0.6) if on else (WHITE, 1)
                else:
                    n = 0
                    for a in range(10):
                        for b in range(10):
                            n += inside(ax + (a + 0.5) * cell / 10, ay + (b + 0.5) * cell / 10, cx, cy, R)
                    fill, op = (RED, 0.6 * n / 100) if n else (WHITE, 1)
                s.rect(ax, ay, cell, cell, fill, LINE, 1.2, opacity=op)
        # the edge of the sphere, clipped to the grid
        pts = []
        for q in range(0, 181):
            ang = math.radians(180 + q * 0.5)
            px, py = cx + R * math.cos(ang), cy + R * math.sin(ang)
            if x0 <= px <= x0 + cols * cell and y0 <= py <= y0 + rows * cell:
                pts.append((px, py))
        s.polyline(pts, INK, 2.2)
        if k == 0:
            for i in range(cols):
                for j in range(rows):
                    s.dot((x0 + (i + 0.5) * cell, y0 + (j + 0.5) * cell), 3.5)
        else:
            i, j = 3, 1
            ax, ay = x0 + i * cell, y0 + j * cell
            s.rect(ax, ay, cell, cell, 'none', INK, 2.5)
            for _ in range(14):
                px, py = ax + rnd.uniform(4, cell - 4), ay + rnd.uniform(4, cell - 4)
                if inside(px, py, cx, cy, R):
                    s.dot((px, py), 3.2, RED)
                else:
                    s.circle((px, py), 2.8, WHITE, BLUE, 1.5)
        s.text((x0 + cols * cell / 2, y0 + rows * cell + 32), 'ein Strahl je Pixel, durch die Mitte' if k == 0
               else 'viele Strahlen je Pixel, zufällig', INK)
        s.text((x0 + cols * cell - 6, y0 + 2 * cell + 15), 'Kugel', INK, 'end')
    return s


# ------------------------------------------------------------- light

def normale():
    w, h = 368, 272
    s = Svg('normale', w, h, 'Die Normale einer Kugel zeigt vom Mittelpunkt center zum Treffpunkt point; geteilt '
            'durch radius hat sie die Länge 1. Rundherum zeigt sie überall nach außen.')
    c = (150, 140)
    r = 86
    s.circle(c, r, PALE, INK, 2.5)
    for ang in (100, 160, 215, 330):
        d = polar(ang)
        s.arrow(add(c, mul(d, r)), add(c, mul(d, r + 34)), GREY, 1.6)
    d = polar(32)
    p = add(c, mul(d, r))
    s.line(c, p, GREY, 1.6, dash='5 4')
    s.arrow(p, add(p, mul(d, 62)), INK, 3)
    s.dot(c, 4)
    s.code(add(c, (0, 24)), 'center', 'middle')
    s.dot(p, 5, BLUE)
    s.code(add(p, (-12, -10)), 'point', 'end')
    s.code(add(p, add(mul(d, 62), (6, -10))), 'normal')
    s.text(add(p, add(mul(d, 62), (6, 12))), 'Länge 1', GREY, 'start')
    s.code((w / 2, h - 14), 'scale(sub(point, center), 1 / radius)', 'middle')
    return s


def licht():
    """The same bundle of sunlight on a larger area, the flatter it falls."""
    w, h = 548, 200
    s = Svg('licht', w, h, 'Dasselbe Bündel Sonnenlicht fällt senkrecht, schräg und flach auf den Boden: Je flacher, '
            'desto breiter die Fläche, auf die es sich verteilt, und desto weniger Licht bekommt jede Stelle. '
            'dot(normal, sun_dir) ist 1, etwa 0.71 und etwa 0.34.')
    for k, (ang, val) in enumerate([(0, '1'), (45, '0.71'), (70, '0.34')]):
        x0 = 10 + k * 176
        fy = 128
        s.line((x0, fy), (x0 + 160, fy), INK, 2.5)
        s.arrow((x0 + 12, fy), (x0 + 12, fy - 70), INK, 2.2)
        s.code((x0 + 4, fy - 82), 'normal')
        # three parallel rays, 18 apart across the bundle
        # three parallel rays from the upper right, the bundle 36 wide across
        width = 36 / math.cos(math.radians(ang))
        start = x0 + 80 - width / 2
        for q in range(3):
            hit_x = start + q * width / 2
            top = (hit_x + math.sin(math.radians(ang)) * 48, fy - math.cos(math.radians(ang)) * 48)
            s.arrow(top, (hit_x, fy), AMBER, 2)
        s.line((start, fy + 10), (start + width, fy + 10), AMBER, 5, cap='round')
        word = ['senkrecht', 'schräg', 'flach'][k]
        s.label((x0 + 80, fy + 44), [word + ': ', ('c', 'dot'), (' = ' if val == '1' else ' ≈ ') + val], INK, 'middle')
    return s


def skalarprodukt():
    """normal up, sun_dir at 0, 60, 90, 120 degrees; the dot product is the
    length of the shadow of sun_dir on normal."""
    w, h = 480, 232
    s = Svg('skalarprodukt', w, h, 'Viermal normal nach oben und sun_dir derselben Länge 1, im Winkel 0°, 60°, 90° und '
            '120° dazu. Fällt man von der Spitze von sun_dir das Lot auf normal, ist das Stück bis dorthin so lang wie '
            'dot(normal, sun_dir) (blau): 1, 0.5, 0 und -0.5, nach unten gezählt, wenn es auf der anderen Seite liegt.')
    L = 84
    for k, ang in enumerate((0, 60, 90, 120)):
        o = (44 + k * 118, 132)
        s.line((o[0], o[1] + L * 0.75), (o[0], o[1] - L - 14), LINE, 1.2)
        d = (math.sin(math.radians(ang)), -math.cos(math.radians(ang)))
        tip = add(o, mul(d, L))
        foot = (o[0], tip[1])
        proj = math.cos(math.radians(ang))
        if 0 < ang:
            if ang != 90:
                s.line(tip, foot, GREY, 1.4, dash='4 3')
                s.right_angle(foot, (0, 1 if proj > 0 else -1), (1, 0), 9, GREY)
        if abs(proj) > 1e-9:
            s.line((o[0] - 9, o[1]), (o[0] - 9, o[1] - L * proj), BLUE, 5, cap='butt')
        s.arrow(o, (o[0], o[1] - L), INK, 2.5)
        if ang == 0:
            s.arrow((o[0] + 9, o[1]), (o[0] + 9, o[1] - L), RED, 2.5)
        else:
            s.arrow(o, tip, RED, 2.5)
        if ang == 90:
            s.right_angle(o, (0, -1), (1, 0), 14, GREY)
        elif ang > 0:
            s.arc(o, 26, 90 - ang, 90, GREY)
            mid = math.radians(90 - ang / 2)
            s.tex((o[0] + 38 * math.cos(mid) + (2 if ang > 90 else 0), o[1] - 38 * math.sin(mid) + 6),
                  r'%d^\circ' % ang, GREY, 'start' if ang < 90 else 'start', 15)
        s.dot(o, 4)
        val = {0: '1', 60: '0.5', 90: '0', 120: '-0.5'}[ang]
        s.label((o[0] + 6, h - 16), [('c', 'dot'), ' = ' + val], BLUE, 'middle')
    s.code((44 - 14, 132 - L - 12), 'normal', 'start')
    s.code((44 + 22, 132 - 40), 'sun_dir')
    return s


def naechster():
    w, h = 496, 210
    s = Svg('naechster', w, h, 'Ein Strahl von origin: Eine Kugel verfehlt er (t = INFINITY), zwei trifft er bei t = 1.9 '
            'und t = 3.2, den Boden bei t = 4.2. Zu sehen ist der Treffer mit dem kleinsten t.')
    fy = 172
    s.rect(10, fy, w - 20, 24, LIGHT)
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    s.text((w - 18, fy + 18), 'Boden', INK, 'end')
    o = (30, 90)
    floor_x = 430
    d = unit((floor_x - o[0], fy - o[1]))
    t_floor = (fy - o[1]) / d[1]
    balls = [((180, 38), 30, None), ((246, 116), 36, '1.9'), ((370, 146), 26, '3.2')]
    hits = []
    for c, r, label in balls:
        s.circle(c, r, PALE, INK, 2.2)
        t = ray_circle(o, d, c, r)
        if label:
            hits.append((t, label))
    unit_px = t_floor / 4.2
    first = add(o, mul(d, hits[0][0]))
    s.line(first, add(o, mul(d, t_floor)), GREY, 1.6, dash='5 4')
    s.arrow(o, first, BLUE, 3, gap=5)
    s.dot(first, 5, BLUE)
    s.code(add(first, (-14, 26)), 't = 1.9', 'end')
    p2 = add(o, mul(d, hits[1][0]))
    s.hollow(p2, 4.5, GREY)
    s.code(add(p2, (2, -40)), 't = 3.2', 'middle')
    pf = add(o, mul(d, t_floor))
    s.hollow(pf, 4.5, GREY)
    s.code(add(pf, (-12, 20)), 't = 4.2', 'end')
    s.code((180 + 38, 38 + 6), 't = INFINITY')
    s.dot(o, 6)
    s.code((o[0] - 8, o[1] - 16), 'origin')
    s.label((w - 14, 30), ['zu sehen: das kleinste ', ('c', 't')], BLUE, 'end')
    return s


# ------------------------------------------------------------- shadows

def schattenstrahl():
    w, h = 496, 236
    s = Svg('schattenstrahl', w, h, 'Eine Kugel auf dem Boden, die Sonne weit weg rechts oben. Von zwei Treffpunkten auf '
            'dem Boden geht je ein Strahl in Richtung sun_dir, beide parallel, denn die Sonne ist so weit weg, dass sie '
            'von überall in derselben Richtung steht. Der linke stößt an die Kugel: Schatten. Der rechte kommt frei durch.')
    fy = 196
    s.rect(10, fy, w - 20, 30, LIGHT)
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    c, r = (232, fy - 62), 62
    s.circle(c, r, PALE, INK, 2.5)
    sun = unit((0.78, -0.62))
    # in the shadow: the ray meets the ball
    p1 = (150, fy)
    t1 = ray_circle(p1, sun, c, r)
    q1 = add(p1, mul(sun, t1))
    s.line(p1, q1, AMBER, 2.2, dash='6 4')
    s.cross(q1, 6, RED)
    s.dot(p1, 5.5, BLUE)
    s.text((p1[0] - 6, fy + 22), 'im Schatten', INK, 'end')
    # in the sun: free all the way
    p2 = (360, fy)
    q2 = add(p2, mul(sun, 150))
    s.arrow(p2, q2, AMBER, 2.2, dash='6 4')
    s.dot(p2, 5.5, BLUE)
    s.text((p2[0] + 8, fy + 22), 'in der Sonne', INK, 'start')
    s.code(add(lerp(p2, q2, 0.55), (-12, -6)), 'sun_dir', 'end')
    # the sun, far away in that direction
    s.circle((w - 40, 36), 18, AMBER, None)
    s.text((w - 66, 42), 'Sonne, weit weg', AMBER, 'end')
    return s


def akne_lupe():
    w, h = 432, 200
    s = Svg('akne-lupe', w, h, 'Stark vergrößert: der Boden y = 0. Der Treffpunkt sollte auf ihm liegen, gerechnet liegt '
            'er ein winziges Stück darunter. Sein Schattenstrahl zur Sonne trifft gleich wieder den Boden, bei einem '
            'winzigen t. Treffer näher als EPS am Anfang des Strahls zählen nicht.')
    fy = 112
    s.rect(10, fy, w - 20, 78, LIGHT)
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    s.label((w - 16, fy + 24), ['Boden, ', ('c', 'y = 0')], INK, 'end')
    s.text((16, 26), 'stark vergrößert', GREY, 'start', italic=True)
    real = (150, fy)
    calc = (150, fy + 24)
    sun = unit((0.45, -1))
    s.hollow(real, 6, GREY)
    s.text((real[0] - 16, fy - 10), 'eigentlich hier', GREY, 'end')
    s.dot(calc, 6, BLUE)
    s.text((24, fy + 66), 'gerechnet: ein winziges Stück darunter', BLUE, 'start')
    # the shadow ray from the computed point meets the floor at once
    t_floor = (calc[1] - fy) / -sun[1]
    hit = add(calc, mul(sun, t_floor))
    tip = add(calc, mul(sun, 118))
    s.arrow(calc, tip, AMBER, 2.2, dash='6 4')
    s.cross(hit, 6, RED)
    s.text(add(tip, (10, 8)), 'zur Sonne', AMBER, 'start')
    s.text((hit[0] + 24, fy - 38), 'trifft gleich den Boden:', RED, 'start')
    s.code((hit[0] + 24, fy - 14), 't = 0.0000000000000002')
    # EPS along the ray from its start, on its right
    eps = 50
    side = mul(perp(sun), -1)
    a = add(calc, mul(side, 16))
    b = add(a, mul(sun, eps))
    s.line(a, b, GREEN, 2)
    for q in (a, b):
        s.line(add(q, mul(side, 5)), add(q, mul(side, -5)), GREEN, 2)
    s.code(add(lerp(a, b, 0.5), add(mul(side, 12), (0, 6))), 'EPS')
    return s


def kreuzprodukt():
    w, h = 368, 256
    s = Svg('kreuzprodukt', w, h, 'Zwei Pfeile a und b in einer Fläche. cross(a, b) steht senkrecht auf beiden, auf a und '
            'auf b, zu sehen an den rechten Winkeln; cross(b, a) zeigt genau in die Gegenrichtung.')
    v = View(24, 22, 118, (150, 146))
    a = norm3((1, 0, 0.45))
    b = norm3((0.2, 0, -1))
    n = norm3(cross3(a, b))
    o = (0, 0, 0)
    plane = [(-0.75, 0, 0.85), (1.3, 0, 0.85), (1.3, 0, -1.05), (-0.75, 0, -1.05)]
    s.polygon([v(p) for p in plane], PALE, LINE, 1.5)
    low = mul3(n, -0.62)
    s.arrow(v(o), v(low), GREY, 1.8, dash='5 4')
    s.code(add(v(low), (12, 0)), 'cross(b, a)')
    s.arrow(v(o), v(mul3(a, 1.05)), BLUE, 2.5)
    s.arrow(v(o), v(mul3(b, 0.95)), BLUE, 2.5)
    s.code(add(v(mul3(a, 1.05)), (0, -14)), 'a', 'middle')
    s.code(add(v(mul3(b, 0.95)), (10, 2)), 'b')
    s.right_angle_3d(v, o, a, n, 0.22, GREY)
    s.right_angle_3d(v, o, b, n, 0.34, GREY)
    s.arrow(v(o), v(mul3(n, 0.92)), INK, 3)
    s.code(add(v(mul3(n, 0.92)), (12, 6)), 'cross(a, b)')
    s.dot(v(o), 4.5)
    return s


def kamera():
    w, h = 496, 236
    s = Svg('kamera', w, h, 'Schräg von der Seite: das Auge eye schaut zum Ziel target. Eine Einheit vor dem Auge steht '
            'das Bild, senkrecht zu forward; in seiner Mitte zeigt right nach rechts (hier auf uns zu) und up nach oben. '
            'Ein Pixel liegt sx Schritte nach right und sy Schritte nach up: Der Strahl durch ihn hat die Richtung '
            'forward + sx · right + sy · up.')
    v = View(-30, 14, 118, (56, 150))
    fwd = (1, 0, 0)
    up = (0, 1, 0)
    right = cross3(fwd, up)          # (0, 0, 1): towards the viewer
    eye = (0, 0, 0)
    mid = fwd
    hw, hh = 0.62, 0.38
    corners = [add3(mid, add3(mul3(right, sx), mul3(up, sy))) for sx, sy in [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]]
    s.polygon([v(p) for p in corners], PALE, INK, 2)
    s.text(add(v(corners[1]), (10, 2)), 'das Bild', INK, 'start')
    target = (3.0, 0, 0)
    s.line(v(mid), v(target), GREY, 1.5, dash='5 4')
    s.circle(v(target), 32, PALE, INK, 2)
    s.code(add(v(target), (0, 54)), 'target', 'middle')
    # a pixel: sx to the right (towards us), sy up
    sx, sy = -0.32, 0.24
    px = add3(mid, add3(mul3(right, sx), mul3(up, sy)))
    s.arrow(v(eye), v(mid), BLUE, 2.8, gap=3)
    s.code(add(v(mul3(fwd, 0.5)), (14, 24)), 'forward', 'middle')
    rt = add3(mid, mul3(right, 0.46))
    s.arrow(v(mid), v(rt), RED, 2.4)
    s.code(add(v(rt), (8, 14)), 'right')
    s.arrow(v(mid), v(add3(mid, mul3(up, 0.32))), GREEN, 2.4)
    s.code(add(v(add3(mid, mul3(up, 0.32))), (8, 0)), 'up')
    far = mul3(px, 2.45)
    s.line(v(eye), v(px), BLUE, 1.8)
    s.arrow(v(px), v(far), BLUE, 1.8, dash='5 4')
    s.hollow(v(px), 4, BLUE)
    s.dot(v(mid), 3.5)
    s.code((w - 18, 28), 'forward + sx · right + sy · up', 'end')
    s.dot(v(eye), 6)
    s.code(add(v(eye), (0, 28)), 'eye', 'middle')
    return s


def laenge_breite():
    w, h = 424, 272
    s = Svg('laenge-breite', w, h, 'Zweimal die Kugel mit der Normalen n. Links von oben: x nach rechts, z nach unten auf '
            'uns zu; u ist der Winkel von x bis n, rund um die Kugel, der Längengrad. Rechts von der Seite: y nach oben; '
            'v ist der Winkel vom Äquator bis n, der Breitengrad.')
    r = 74
    # from above
    c = (104, 128)
    s.text((c[0], 22), 'von oben', GREY)
    s.circle(c, r, PALE, INK, 2.2)
    s.arrow(c, (c[0] + r + 34, c[1]), RED, 2)
    s.tex((c[0] + r + 40, c[1] + 6), 'x', RED)
    s.arrow(c, (c[0], c[1] + r + 24), BLUE, 2)
    s.tex((c[0] + 10, c[1] + r + 22), 'z', BLUE)
    ang = 38                                    # from x towards z, which is down on the page
    d = (math.cos(math.radians(ang)), math.sin(math.radians(ang)))
    s.arrow(c, add(c, mul(d, r * 0.82)), INK, 2.5)
    s.code(add(c, add(mul(d, r * 0.82), (6, 18))), 'n')
    s.arc(c, 32, -ang, 0, GREY)
    s.code(add(c, (40, 22)), 'u')
    s.label((c[0], h - 14), [('c', 'u'), ': Längengrad'], INK, 'middle')
    # from the side
    c = (318, 128)
    s.text((c[0], 22), 'von der Seite', GREY)
    s.circle(c, r, PALE, INK, 2.2)
    s.line((c[0] - r, c[1]), (c[0] + r, c[1]), GREY, 1.5)
    s.text((c[0] - r + 6, c[1] + 20), 'Äquator', GREY, 'start')
    s.arrow(c, (c[0], c[1] - r - 20), GREEN, 2)
    s.tex((c[0] - 10, c[1] - r - 8), 'y', GREEN, 'end')
    ang = 32
    d = (math.cos(math.radians(ang)), -math.sin(math.radians(ang)))
    tip = add(c, mul(d, r))
    s.arrow(c, add(c, mul(d, r - 7)), INK, 2.5)
    s.dot(tip, 5, BLUE)
    s.code(add(tip, (8, -6)), 'n')
    s.arc(c, 36, 0, ang, GREY)
    s.code(add(c, (44, -10)), 'v')
    s.dot(c, 3.5)
    s.label((c[0], h - 14), [('c', 'v'), ': Breitengrad'], INK, 'middle')
    return s


# ------------------------------------------------------------- mirror and glass

def spiegel():
    w, h = 464, 276
    s = Svg('spiegel', w, h, 'Ein Strahl dir trifft schräg auf einen Spiegel mit der Normalen n. Ohne Spiegel ginge er '
            'gestrichelt weiter nach unten. Zieht man zweimal den Teil entlang der Normalen ab, -2 (dir · n) n, wird aus '
            'ihm der abgeprallte Strahl reflect(dir, n), im selben Winkel nach oben.')
    fy = 142
    s.rect(30, fy, w - 60, 120, PALE)
    s.line((30, fy), (w - 30, fy), INK, 2.5)
    hit = (168, fy)
    d = unit((1, 0.72))                         # down to the right on the page
    L = 150
    start = sub(hit, mul(d, L))
    through = add(hit, mul(d, L))
    out = (hit[0] + d[0] * L, hit[1] - d[1] * L)
    s.arrow(start, hit, BLUE, 2.8)
    s.code(add(start, (-6, -8)), 'dir', 'end')
    s.arrow(hit, through, GREY, 1.8, dash='6 4')
    s.text(add(through, (0, 22)), 'ohne Spiegel', GREY)
    s.arrow(hit, out, BLUE, 2.8)
    s.code(add(out, (8, -6)), 'reflect(dir, n)')
    s.arrow(through, out, GREEN, 2.6)
    s.tex((through[0] + 12, fy + 44), r'-2\,(\mathrm{dir} \cdot n)\, n', GREEN)
    s.arrow(hit, (hit[0], hit[1] - 64), INK, 2.5)
    s.code((hit[0] - 12, hit[1] - 50), 'n', 'end')
    return s


def glas():
    w, h = 464, 250
    s = Svg('glas', w, h, 'Ein Strahl dir kommt schräg aus der Luft und trifft auf Glas. Im Glas läuft er steiler weiter, '
            'zur Normalen hin geknickt: Der Winkel β danach ist kleiner als der Winkel α davor, sin α = 1,5 · sin β. '
            'Ein Teil des Lichts wird gespiegelt.')
    fy = 116
    s.rect(30, fy, w - 60, 124, PALE)
    s.line((30, fy), (w - 30, fy), INK, 2.5)
    hit = (210, fy)
    s.line((hit[0], 14), (hit[0], h - 10), INK, 1.4, dash='3 4')
    s.text((hit[0] + 8, 28), 'Normale', INK, 'start')
    alpha = 52
    beta = math.degrees(math.asin(math.sin(math.radians(alpha)) / 1.5))
    din = (math.sin(math.radians(alpha)), math.cos(math.radians(alpha)))
    start = sub(hit, mul(din, 130))
    s.arrow(start, hit, BLUE, 2.8)
    s.code(add(start, (-6, -8)), 'dir', 'end')
    dout = (math.sin(math.radians(beta)), math.cos(math.radians(beta)))
    s.arrow(hit, add(hit, mul(dout, 130)), BLUE, 2.8)
    s.text(add(hit, add(mul(dout, 130), (10, 4))), 'gebrochen', BLUE, 'start')
    refl = (din[0], -din[1])
    s.arrow(hit, add(hit, mul(refl, 76)), GREY, 1.8, dash='6 4')
    s.text(add(hit, add(mul(refl, 76), (10, 4))), 'ein Teil wird gespiegelt', GREY, 'start')
    s.arc(hit, 44, 90, 90 + alpha, GREY)
    s.tex(add(hit, polar(90 + alpha / 2, 58)), r'\alpha', GREY, 'middle')
    s.arc(hit, 44, -90 + beta, -90, GREY)
    s.tex(add(add(hit, polar(-90 + beta / 2, 60)), (0, 6)), r'\beta', GREY, 'middle')
    s.text((40, fy - 12), 'Luft', GREY, 'start')
    s.text((w - 40, fy + 26), 'Glas, Brechungsindex 1,5', GREY, 'end')
    return s


def strahlen_durch_kugel(name, hollow, comment):
    """Two level rays through a solid or a hollow glass ball, refracted as
    glass() does it (refraction only)."""
    w, h = 320, 190
    s = Svg(name, w, h, comment)
    c, R = (120, 95), 64
    inner = 0.9 * R
    s.circle(c, R, PALE, INK, 2.5)
    if hollow:
        s.circle(c, inner, WHITE, INK, 1.8)
    for y_off, col in [(-0.5, BLUE), (0.5, RED)]:
        p = (10, c[1] + y_off * R)
        d = (1.0, 0.0)
        pts = [p]
        # spheres in the order the ray meets them: (radius, eta when entering)
        layers = [(R, 1 / 1.5)] + ([(inner, 1.5)] if hollow else [])
        inside = []
        for _ in range(8):
            best = None
            for rad, eta in layers:
                t = ray_circle(p, d, c, rad)
                if t is not None and t > 1e-6 and (best is None or t < best[0]):
                    best = (t, rad, eta)
            if best is None:
                break
            t, rad, eta = best
            q = add(p, mul(d, t))
            n = unit(sub(q, c))
            entering = dot(d, n) < 0
            if not entering:
                n = mul(n, -1)
                eta = 1 / eta
            nd = refract(d, n, eta)
            if nd is None:
                break
            pts.append(q)
            p, d = add(q, mul(d, 1e-3)), unit(nd)
        end = add(p, mul(d, 400))
        # clip the last piece to the picture
        k = min((w - 12 - p[0]) / d[0] if d[0] > 0 else 1e9, 1e9)
        end = add(p, mul(d, k))
        s.polyline(pts + [sub(end, mul(d, 10))], col, 2.4)
        s.head(end, d, col, 13)
    return s


# ------------------------------------------------------------- soft light

def halbschatten():
    w, h = 496, 250
    s = Svg('halbschatten', w, h, 'Eine Kugel auf dem Boden, rechts oben die Sonne als Scheibe. Unter dem Boden ein '
            'Streifen, so dunkel, wie viel von der Sonnenscheibe an jeder Stelle verdeckt ist. Im Schatten verdeckt die '
            'Kugel die ganze Scheibe, im Halbschatten einen Teil, in der vollen Sonne nichts: Von dort führen die '
            'Strahlen zu beiden Rändern der Scheibe frei.')
    fy = 196
    s.rect(10, fy, w - 20, 44, LIGHT)
    sun, rs = (410, 28), 26
    c, r = (300, fy - 44), 44

    def visible(px):
        """the share of the sun disc seen from the floor point (px, fy)"""
        p = (px, fy)
        n = 0
        for k in range(41):
            q = add(sun, mul(perp(unit(sub(sun, p))), rs * (k / 20 - 1)))
            d = unit(sub(q, p))
            t = ray_circle(p, d, c, r)
            n += t is None or t > length(sub(q, p))
        return n / 41
    for x in range(10, w - 10, 2):
        v = visible(x + 1)
        if v < 0.999:
            s.rect(x, fy + 4, 2.2, 14, SHADOW, opacity=0.85 * (1 - v))
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    s.circle(c, r, PALE, INK, 2.5)
    s.circle(sun, rs, AMBER, None)
    s.text((sun[0] + rs + 8, sun[1] + 6), 'Sonne', AMBER, 'start')
    xs = range(12, int(c[0]))
    dark = [x for x in xs if visible(x) == 0]
    umbra = (min(dark) + c[0] - r) // 2               # in the middle of the full shadow
    half = min((x for x in xs if x < min(dark)), key=lambda x: abs(visible(x) - 0.5))
    for px, name, anchor in [(half, 'Halbschatten', 'end'), (umbra, 'Schatten', 'start'), (410, 'volle Sonne', 'middle')]:
        p = (px, fy)
        for sign in (-1, 1):
            q = add(sun, mul(perp(unit(sub(sun, p))), rs * sign))
            d = unit(sub(q, p))
            t = ray_circle(p, d, c, r)
            if t is not None and t < length(sub(q, p)):
                e = add(p, mul(d, t))
                s.line(p, e, GREY, 1.6, dash='5 4')
                s.cross(e, 5, RED, 2.2)
            else:
                s.line(p, q, AMBER, 1.6)
        s.dot(p, 5, BLUE)
        s.text((px + {'end': 12, 'start': -12, 'middle': 0}[anchor], fy + 38), name, INK, anchor)
    return s


def himmel_strahlen():
    w, h = 496, 250
    s = Svg('himmel-strahlen', w, h, 'Zwei Punkte auf dem Boden. Über jedem ein gestrichelter Kreis mit dem Radius 1, '
            'der den Boden dort berührt: normal plus ein zufälliger Punkt darin, random_in_sphere(), gibt die Richtung '
            'eines Strahls zum Himmel. Vom freien Punkt kommen alle durch, neben der Kugel stoßen viele an sie.')
    import random
    rnd = random.Random(3)
    fy = 190
    s.rect(10, fy, w - 20, 50, LIGHT)
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    R = 60
    c_ball, r_ball = (396, fy - 74), 74
    s.circle(c_ball, r_ball, PALE, INK, 2.5)
    pts = []
    while len(pts) < 9:
        q = (rnd.uniform(-1, 1), rnd.uniform(-1, 1))
        if q[0] ** 2 + q[1] ** 2 < 0.92:
            pts.append(q)
    for px, name in [(110, 'sieht viel Himmel'), (338, 'sieht wenig Himmel')]:
        p = (px, fy)
        centre = (px, fy - R)
        s.circle(centre, R, stroke=GREY, w=1.4, dash='4 4')
        for q in pts:
            m = (centre[0] + q[0] * R, centre[1] + q[1] * R)
            d = unit(sub(m, p))
            t = ray_circle(p, d, c_ball, r_ball)
            if t is not None:
                e = add(p, mul(d, t))
                s.line(p, e, GREY, 1.6, dash='5 4')
                s.cross(e, 4.5, RED, 2)
            else:
                s.arrow(p, add(p, mul(d, 130)), BLUE, 1.8)
            s.dot(m, 3.2, GREY)
        s.dot(p, 5.5)
        s.text((px, fy + 26), name, INK)
    s.label((18, h - 12), ['Punkte: ', ('c', 'normal + random_in_sphere()')], GREY, 'start')
    return s


def matt():
    w, h = 528, 250
    s = Svg('matt', w, h, 'Ein Strahl dir trifft schräg auf eine Fläche. Die glatte Spiegelrichtung reflect(dir, n) endet '
            'in der Mitte einer Kugel mit dem Radius rough; jeder Durchgang nimmt einen anderen zufälligen Punkt dieser '
            'Kugel, und der Strahl geht dorthin. Im Mittel verschwimmt das Spiegelbild.')
    import random
    rnd = random.Random(5)
    fy = 200
    s.rect(60, fy, w - 120, 36, PALE)
    s.line((60, fy), (w - 60, fy), INK, 2.5)
    hit = (220, fy)
    L = 170
    d = unit((1, 1.05))
    start = sub(hit, mul(d, 150))
    r_dir = (d[0], -d[1])
    tip = add(hit, mul(r_dir, L))
    rough = 0.3
    s.circle(tip, rough * L, PALE, GREY, 1.4, dash='4 4', opacity=0.6)
    s.arrow(start, hit, BLUE, 2.8)
    s.code(add(start, (-6, -8)), 'dir', 'end')
    s.arrow(hit, (hit[0], hit[1] - 66), INK, 2.5)
    s.code((hit[0] - 12, hit[1] - 52), 'n', 'end')
    for k in range(5):
        while True:
            q = (rnd.uniform(-1, 1), rnd.uniform(-1, 1))
            if q[0] ** 2 + q[1] ** 2 < 1:
                break
        m = add(tip, mul(q, rough * L))
        s.arrow(hit, m, BLUE, 1.4 if k else 2.6)
        s.dot(m, 2.8, BLUE)
    s.line(hit, tip, GREY, 1.6, dash='5 4')
    s.dot(tip, 3.5, GREY)
    s.code((tip[0] - rough * L - 6, tip[1] - 24), 'reflect(dir, n)', 'end')
    s.line(tip, (tip[0] + rough * L, tip[1]), INK, 1.5)
    s.code((tip[0] + rough * L + 8, tip[1] + 5), 'rough')
    s.text((tip[0] + 6, tip[1] - rough * L - 12), 'jeder Durchgang ein anderer Punkt', GREY, 'middle')
    return s


def schaerfe():
    w, h = 488, 228
    s = Svg('schaerfe', w, h, 'Von der Seite: links die Linse mit dem Radius aperture, rechts im Abstand focus die '
            'Schärfeebene. Alle Strahlen eines Pixels beginnen an verschiedenen Punkten der Linse und treffen sich auf '
            'der Schärfeebene in einem Punkt: Was dort liegt, ist scharf. Davor und dahinter liegen sie auseinander: '
            'unscharf.')
    lx, cy = 128, 106
    ap = 62
    fx = 400
    s.line((fx, 20), (fx, 176), GREY, 1.4, dash='4 4')
    s.text((fx, 16), 'Schärfeebene', GREY)
    s.rect(lx - 6, cy - ap, 12, 2 * ap, PALE, INK, 2)
    s.text((lx, cy + ap + 24), 'Linse', INK)
    for off in (-1, -0.5, 0, 0.5, 1):
        a = (lx, cy + off * ap)
        d = unit(sub((fx, cy), a))
        s.line(a, (fx, cy), BLUE, 1.8)
        s.line((fx, cy), add((fx, cy), mul(d, 90)), BLUE, 1.6, dash='4 4')
    s.dot((fx, cy), 4.5, BLUE)
    s.text((fx - 8, cy - 18), 'scharf', BLUE, 'end')
    # nearer than focus: the rays of one pixel lie apart
    nx = 250
    spread = ap * (fx - nx) / (fx - lx)
    s.line((nx, cy - spread), (nx, cy + spread), RED, 2.5)
    for y_ in (cy - spread, cy + spread):
        s.line((nx - 6, y_), (nx + 6, y_), RED, 2.5)
    s.text((nx, cy - spread - 12), 'unscharf', RED)
    # aperture: the radius of the lens
    s.line((lx - 20, cy), (lx - 20, cy - ap), INK, 1.5)
    for y_ in (cy, cy - ap):
        s.line((lx - 25, y_), (lx - 15, y_), INK, 1.5)
    s.code((lx - 28, cy - ap / 2 + 5), 'aperture', 'end')
    # focus: from the lens to the plane
    s.line((lx, 200), (fx, 200), INK, 1.5)
    for x_ in (lx, fx):
        s.line((x_, 195), (x_, 205), INK, 1.5)
    s.code(((lx + fx) / 2, 220), 'focus', 'middle')
    return s


def pfad():
    w, h = 344, 268
    s = Svg('pfad', w, h, 'Ein Weg des Lichts: Vom Auge geht ein Strahl zum Boden neben einer Kugel, an eine Stelle im '
            'Schatten; ihr Strahl zur Sonne endet an der Kugel. Von dort geht ein zufälliger Strahl, bounce, zu einer '
            'Wand, die in der Sonne steht: Ihr Strahl sunlight erreicht die Sonne. Von der Wand geht ein weiterer '
            'zufälliger Strahl in den Himmel.')
    fy = 218
    s.rect(10, fy, w - 20, 40, LIGHT)
    s.line((10, fy), (w - 10, fy), INK, 2.5)
    wall = (286, 48, 30, fy - 48)
    s.rect(*wall, LIGHT, INK, 2)
    s.text((wall[0] - 8, fy - 12), 'Wand', INK, 'end')
    c, r = (80, fy - 46), 46
    s.circle(c, r, PALE, INK, 2.5)
    sun_c = (54, 34)
    s.circle(sun_c, 18, AMBER, None)
    s.text((sun_c[0] + 26, sun_c[1] + 6), 'Sonne', AMBER, 'start')
    eye = (150, 66)
    floor_p = (128, fy)
    wall_p = (wall[0], 120)
    sky_end = (236, 14)
    to_sun = unit(sub(sun_c, wall_p))
    # floor point: the way to the sun is blocked by the ball
    d_floor = unit(sub(sun_c, floor_p))
    t = ray_circle(floor_p, d_floor, c, r)
    e = add(floor_p, mul(d_floor, t))
    s.line(floor_p, e, AMBER, 1.8, dash='5 4')
    s.cross(e, 5, RED, 2.2)
    # the wall point sees the sun
    s.line(wall_p, add(wall_p, mul(to_sun, 120)), AMBER, 1.8, dash='5 4')
    s.code(add(wall_p, add(mul(to_sun, 84), (2, 26))), 'sunlight', 'middle')
    s.arrow(eye, floor_p, BLUE, 2.4, gap=5)
    s.arrow(floor_p, wall_p, BLUE, 2.4, gap=5)
    s.code(add(lerp(floor_p, wall_p, 0.55), (0, 26)), 'bounce', 'middle')
    s.arrow(wall_p, sky_end, BLUE, 2.4)
    s.code((wall[0] - 10, 40), 'bounce')
    s.text((sky_end[0] + 10, sky_end[1] + 8), 'Himmel', GREY, 'start')
    for p in (floor_p, wall_p):
        s.dot(p, 5, BLUE)
    s.text((floor_p[0] + 6, fy + 26), 'im Schatten', GREY, 'start')
    s.dot(eye, 7)
    s.text((eye[0] + 12, eye[1] - 6), 'Auge', INK, 'start')
    return s


def tone():
    w, h = 488, 246
    s = Svg('tone', w, h, 'Ein Graph: waagrecht die gerechnete Helligkeit c von 0 bis 3, senkrecht, was auf den Bildschirm '
            'kommt, von 0 bis 1. Rot gestrichelt steigt die Linie gerade bis 1 und bleibt dann flach: abgeschnitten. '
            'Blau ist tone(c): bis 0.7 dieselbe Linie, dann biegt sie weich ab und nähert sich 1.')
    x0, y0 = 54, 206           # the origin
    sx, sy = 136, 168          # pixels per unit
    X = lambda c: x0 + c * sx
    Y = lambda v: y0 - v * sy
    for c in (1, 2, 3):
        s.line((X(c), y0), (X(c), Y(1.05)), LINE, 1)
    s.line((x0, Y(1)), (X(3.1), Y(1)), LINE, 1)
    s.arrow((x0, y0), (X(3.25), y0), INK, 2)
    s.arrow((x0, y0), (x0, Y(1.18)), INK, 2)
    for c in (0, 1, 2, 3):
        s.tex((X(c), y0 + 24), str(c), INK, 'middle', 16)
    s.tex((x0 - 10, Y(1) + 6), '1', INK, 'end', 16)
    s.label((X(3.25), y0 - 12), ['gerechnet: ', ('c', 'c')], GREY, 'end')
    s.text((x0 + 10, Y(1.18) + 5), 'auf dem Bildschirm', GREY, 'start')
    # the bend at 0.7
    k = 0.7
    s.line((X(k), y0), (X(k), Y(k)), GREY, 1.2, dash='3 3')
    s.code((X(k), y0 + 24), '0.7', 'middle')
    # clipped
    s.polyline([(X(0), Y(0)), (X(1), Y(1)), (X(3.1), Y(1))], RED, 2.4, dash='7 5')
    s.text((X(1.6), Y(1) - 12), 'abgeschnitten', RED, 'middle')
    # tone(c)
    pts = []
    for i in range(0, 311):
        c = i / 100
        v = c if c < k else k + (1 - k) * (1 - math.exp(-(c - k) / (1 - k)))
        pts.append((X(c), Y(v)))
    s.polyline(pts, BLUE, 3)
    s.label((X(1.55), Y(0.78)), ['Tone Mapping, ', ('c', 'tone(c)')], BLUE, 'start')
    return s


PICTURES = {
    'vektoren': vektoren,
    'bildebene': bildebene,
    'treffer': treffer,
    'pixel': pixel,
    'normale': normale,
    'licht': licht,
    'skalarprodukt': skalarprodukt,
    'naechster': naechster,
    'schattenstrahl': schattenstrahl,
    'akne-lupe': akne_lupe,
    'kreuzprodukt': kreuzprodukt,
    'kamera': kamera,
    'laenge-breite': laenge_breite,
    'spiegel': spiegel,
    'glas': glas,
    'strahl-voll': lambda: strahlen_durch_kugel('strahl-voll', False, 'Zwei waagrechte Strahlen durch eine volle '
                                                'Glaskugel: Beim Eintreten und beim Austreten knicken sie zur Mitte '
                                                'hin ab, kreuzen sich, und oben und unten tauschen.'),
    'strahl-hohl': lambda: strahlen_durch_kugel('strahl-hohl', True, 'Dieselben zwei Strahlen durch eine hohle '
                                                'Glaskugel mit dünner Wand: In jeder Wand knicken sie ab und gleich '
                                                'wieder zurück; sie kommen fast in derselben Richtung heraus.'),
    'halbschatten': halbschatten,
    'himmel-strahlen': himmel_strahlen,
    'matt': matt,
    'schaerfe': schaerfe,
    'pfad': pfad,
    'tone': tone,
}

if __name__ == '__main__':
    only = sys.argv[1:]
    for name, fn in PICTURES.items():
        if only and name not in only:
            continue
        fn().save()
        print(name)
