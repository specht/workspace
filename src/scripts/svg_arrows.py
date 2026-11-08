#!/usr/bin/env python3
"""Ends the line of every arrow inside its head.

A drawing hangs its arrowheads on lines with a marker whose refX lies near
the tip (refX 9 in a head 10 wide): the line runs to the tip, and near its
point the head is narrower than the line, so the line shows beside the tip
(the maintainer saw it on many drawings). This moves the end of each such
line back to the middle of its head and hangs a copy of the marker there
(id NAME-mid, refX in the middle of the head): the tip stays exactly where it
was, and the line ends where the head is wider than it.

    python3 src/scripts/svg_arrows.py FILE.svg ...

Rewrites each file in place; render-svg.rb runs it. A line already hanging a
-mid marker is left alone, so it can run again after every change. Handles
<line>, <polyline> and paths whose end is a straight line, a quadratic or a
cubic curve (the curve keeps its direction at the end); other arrows keep
their marker as they are.
"""

import math
import re
import sys

MARKER = re.compile(r'<marker\b([^>]*)>(.*?)</marker>', re.S)
ATTR = lambda name: re.compile(r'\s%s=["\']([^"\']*)["\']' % re.escape(name))


def attr(tag, name, default=None):
    m = ATTR(name).search(tag)
    return m.group(1) if m else default


def set_attr(tag, name, value):
    """an attribute the element has, with a new value"""
    return ATTR(name).sub(lambda m: f' {name}="{value}"', tag, count=1)


def fmt(v):
    return f'{v:.2f}'.rstrip('0').rstrip('.')


def markers(svg):
    """id -> (width of the head in its viewBox, refX, scale per stroke width)"""
    found = {}
    for m in MARKER.finditer(svg):
        tag = m.group(1)
        mid = attr(' ' + tag, 'id')
        box = attr(' ' + tag, 'viewBox')
        if not mid or not box or mid.endswith('-mid'):
            continue
        x0, _, w, _ = (float(v) for v in box.replace(',', ' ').split())
        ref = float(attr(' ' + tag, 'refX', '0')) - x0
        if ref <= w / 2 + 1e-6:
            continue                      # already in the middle or behind it
        units = attr(' ' + tag, 'markerUnits', 'strokeWidth')
        scale = float(attr(' ' + tag, 'markerWidth', '3')) / w
        found[mid] = (w, ref, scale, units == 'strokeWidth', m.group(0), x0)
    return found


def shortened(x1, y1, x2, y2, d):
    """the end (x2, y2) moved d back towards (x1, y1)"""
    length = math.hypot(x2 - x1, y2 - y1)
    if length <= d + 1e-6:
        return None
    k = (length - d) / length
    return x1 + (x2 - x1) * k, y1 + (y2 - y1) * k


def width_of(tag):
    w = attr(tag, 'stroke-width')
    if w is None:
        style = attr(tag, 'style', '')
        m = re.search(r'stroke-width\s*:\s*([0-9.]+)', style)
        w = m.group(1) if m else '1'
    try:
        return float(re.sub(r'[a-z]+$', '', w))
    except ValueError:
        return 1.0


NUMBER = r'-?(?:\d+\.?\d*|\.\d+)(?:e-?\d+)?'


def shorten_path(d, back, at_start):
    """a path's d with its end (or start) moved back; None if it cannot"""
    tokens = re.findall(r'[MLHVCSQTAZmlhvcsqtaz]|' + NUMBER, d)
    if any(t.isalpha() and t not in 'MLCQ' for t in tokens):
        return None                       # relative, arcs, shorthands: leave
    segments = []
    i = 0
    while i < len(tokens):
        cmd = tokens[i]; i += 1
        count = {'M': 2, 'L': 2, 'Q': 4, 'C': 6}[cmd]
        first = True
        while i < len(tokens) and not tokens[i].isalpha():
            # pairs after the first one of an M are lines
            segments.append([cmd if (first or cmd != 'M') else 'L', [float(v) for v in tokens[i:i + count]]])
            i += count
            first = False
    if len(segments) < 2:
        return None
    if at_start:
        # the start: move the first point towards the next one
        x0, y0 = segments[0][1]
        nxt = segments[1][1][:2]
        moved = shortened(nxt[0], nxt[1], x0, y0, back)
        if moved is None:
            return None
        dx, dy = moved[0] - x0, moved[1] - y0
        segments[0][1] = list(moved)
        if segments[1][0] in 'QC':
            segments[1][1][0] += dx; segments[1][1][1] += dy
    else:
        cmd, pts = segments[-1]
        ex, ey = pts[-2], pts[-1]
        if cmd == 'L':
            px, py = segments[-2][1][-2], segments[-2][1][-1]
        else:
            px, py = pts[-4], pts[-3]     # the last control point: the direction at the end
        moved = shortened(px, py, ex, ey, back)
        if moved is None:
            return None
        dx, dy = moved[0] - ex, moved[1] - ey
        pts[-2], pts[-1] = moved
        if cmd in 'QC':
            pts[-4] += dx; pts[-3] += dy
    out = []
    for cmd, pts in segments:
        out.append(cmd + ' ' + ' '.join(fmt(v) for v in pts))
    return ' '.join(out)


def fix(svg):
    found = markers(svg)
    if not found:
        return svg, 0
    used = set()
    changed = 0

    def element(m):
        nonlocal changed
        tag = m.group(0)
        name = m.group(1)
        w = width_of(tag)
        for end in ('end', 'start'):
            ref = attr(tag, f'marker-{end}')
            mm = re.match(r'url\(#([^)]+)\)', ref or '')
            if not mm or mm.group(1) not in found:
                continue
            hw, refx, scale, per_width, _, _ = found[mm.group(1)]
            back = (refx - hw / 2) * scale * (w if per_width else 1)
            new = None
            if name == 'line':
                x1, y1, x2, y2 = (float(attr(tag, k, '0')) for k in ('x1', 'y1', 'x2', 'y2'))
                if end == 'end':
                    p = shortened(x1, y1, x2, y2, back)
                    if p:
                        new = set_attr(set_attr(tag, 'x2', fmt(p[0])), 'y2', fmt(p[1]))
                else:
                    p = shortened(x2, y2, x1, y1, back)
                    if p:
                        new = set_attr(set_attr(tag, 'x1', fmt(p[0])), 'y1', fmt(p[1]))
            elif name == 'polyline':
                pts = [float(v) for v in re.findall(NUMBER, attr(tag, 'points', ''))]
                if len(pts) >= 4:
                    if end == 'end':
                        p = shortened(pts[-4], pts[-3], pts[-2], pts[-1], back)
                        if p:
                            pts[-2], pts[-1] = p
                    else:
                        p = shortened(pts[2], pts[3], pts[0], pts[1], back)
                        if p:
                            pts[0], pts[1] = p
                    if p:
                        new = set_attr(tag, 'points', ' '.join(f'{fmt(pts[k])},{fmt(pts[k + 1])}' for k in range(0, len(pts), 2)))
            elif name == 'path':
                d = shorten_path(attr(tag, 'd', ''), back, end == 'start')
                if d:
                    new = set_attr(tag, 'd', d)
            if new:
                tag = new.replace(f'url(#{mm.group(1)})', f'url(#{mm.group(1)}-mid)', 1) if end == 'end' else \
                    re.sub(r'(marker-start=["\'])url\(#%s\)' % re.escape(mm.group(1)), r'\1url(#%s-mid)' % mm.group(1), new, count=1)
                used.add(mm.group(1))
                changed += 1
        return tag

    svg = re.sub(r'<(line|polyline|path)\b[^>]*>', element, svg)
    # the copies of the markers, hung at the middle of the head
    for mid in sorted(used):
        if f'id="{mid}-mid"' in svg or f"id='{mid}-mid'" in svg:
            continue
        hw, refx, scale, per_width, block, x0 = found[mid]
        copy = re.sub(r'(\sid=["\'])%s(["\'])' % re.escape(mid), r'\g<1>%s-mid\2' % mid, block, count=1)
        copy = re.sub(r'(\srefX=["\'])[^"\']*(["\'])', lambda m: m.group(1) + fmt(x0 + hw / 2) + m.group(2), copy, count=1)
        svg = svg.replace(block, block + copy, 1)
    return svg, changed


if __name__ == '__main__':
    for path in sys.argv[1:]:
        text = open(path, encoding='utf-8').read()
        new, count = fix(text)
        if new != text:
            with open(path, 'w', encoding='utf-8') as f:
                f.write(new)
        print(f'{path}: {count} arrows')
