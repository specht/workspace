"""Tapes, stacks and heads of automata as SVG, for the pictures of
»Kellerautomaten«, »Turingmaschinen« and »Grammatiken« (its stack of open
calls) (render with src/scripts/render-svg.rb).

The state diagrams themselves are not drawn here: the pages draw them with
the widget of »Endliche Automaten« (<div class='automaton-drawing'>,
src/static/include/automaton_layout.js). The pictures here look like those
drawings: symbols and state names in 0xProto mono as plain text, a state
name as an italic q with its index set small and low (q0, qH), outlines in
the text colour 1.5 wide, arrows grey, and what is current (the head, the
top of the stack, the field being read) in the step widget's purple.

    import sys; sys.path.insert(0, 'src/scripts')
    import automaton_pictures as ap
"""

import math

INK = '#1f2937'     # text and outlines (the page's text colour)
GREY = '#6b7280'    # arrows and what is done, like the drawings' grey arrows
ACCENT = '#7c3aed'  # what is current, like the step widget's purple
WHITE = '#ffffff'
MONO = "'0xProto Nerd Font Mono', 'DejaVu Sans Mono', monospace"
STROKE = 1.5


def svg_head(w, h, comment, render):
    """The opening of a picture, w × h, with a comment saying how to render
    it (render: the render-svg.rb command)."""
    return (f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>\n"
            f"  <!-- {comment}\n       {render} -->\n"
            f"  <rect width='{w}' height='{h}' fill='{WHITE}'/>\n")


def text(x, y, s, size, color=INK, anchor='middle', italic=False):
    s = str(s).replace('&', '&amp;').replace('<', '&lt;')
    style = " font-style='italic'" if italic else ''
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='{anchor}' font-family=\"{MONO}\" "
            f"font-size='{size}' fill='{color}'{style}>{s}</text>")


def state_name(x, y, name, size, color=INK):
    """q0 or qH as the drawings write it: an italic q, the index small and low;
    y is the baseline of the q."""
    letter, index = name[0], name[1:]
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='middle' font-family=\"{MONO}\" font-size='{size}' "
            f"font-style='italic' fill='{color}'>{letter}<tspan dy='0.3em' font-size='0.7em'>{index}</tspan></text>")


def arrowhead(x, y, dx, dy, color=GREY, length=9, half=4.5):
    """A filled head with its tip at (x, y), pointing along (dx, dy)."""
    n = math.hypot(dx, dy)
    dx, dy = dx / n, dy / n
    bx, by = x - dx * length, y - dy * length
    return (f"<path d='M {x:.1f} {y:.1f} L {bx - dy * half:.1f} {by + dx * half:.1f} "
            f"L {bx + dy * half:.1f} {by - dx * half:.1f} z' fill='{color}'/>")


def _bezier(p0, p1, p2, p3, t):
    u = 1 - t
    return tuple(u ** 3 * a + 3 * u * u * t * b + 3 * u * t * t * c + t ** 3 * d for a, b, c, d in zip(p0, p1, p2, p3))


def curve(p0, p1, p2, p3, color=GREY, width=1.6, length=9, half=4.5):
    """A cubic curve ending in an arrowhead with its tip at p3: the line stops
    under the head, the head points along the curve where it arrives."""
    t = 1.0
    while t > 0 and math.dist(_bezier(p0, p1, p2, p3, t), p3) < length - 2:
        t -= 0.002

    def lerp(a, b):
        return tuple(x + (y - x) * t for x, y in zip(a, b))
    a, b, c = lerp(p0, p1), lerp(p1, p2), lerp(p2, p3)
    d, e = lerp(a, b), lerp(b, c)
    f = lerp(d, e)
    end = _bezier(p0, p1, p2, p3, t)
    return (f"<path d='M {p0[0]:.1f} {p0[1]:.1f} C {a[0]:.1f} {a[1]:.1f} {d[0]:.1f} {d[1]:.1f} "
            f"{f[0]:.1f} {f[1]:.1f}' fill='none' stroke='{color}' stroke-width='{width}'/>"
            + arrowhead(p3[0], p3[1], p3[0] - end[0], p3[1] - end[1], color, length, half))


def cell(x, y, size, symbol, current=False, done=False, width=STROKE):
    """A field of a tape or a stack with its symbol: current in purple, done
    (read already) in grey."""
    # an empty field (_) faint, like a field already read
    color = ACCENT if current else GREY if done or symbol == '_' else INK
    stroke = ACCENT if current else INK
    font = size * 0.55
    return (f"<rect x='{x:.1f}' y='{y:.1f}' width='{size}' height='{size}' fill='{WHITE}' "
            f"stroke='{stroke}' stroke-width='{width * (2 if current else 1)}'/>"
            + text(x + size / 2, y + size / 2 + font * 0.36, symbol, round(font), color))


def tape(x0, y, size, symbols, current=None, done=0, ends=False, width=STROKE):
    """Fields side by side from x0; current: the index of the field under
    the head, drawn last so its border lies on top; done: how many fields at
    the start are read already; ends: dots beyond both ends, a tape that
    goes on."""
    parts = [cell(x0 + i * size, y, size, s, done=i < done, width=width)
             for i, s in enumerate(symbols) if i != current]
    if current is not None:
        parts.append(cell(x0 + current * size, y, size, symbols[current], current=True, width=width))
    if ends:
        for x in (x0 - size * 0.32, x0 + len(symbols) * size + size * 0.32):
            for k in (-1, 0, 1):
                parts.append(f"<circle cx='{x + k * size * 0.11:.1f}' cy='{y + size / 2:.1f}' r='{size * 0.032:.1f}' fill='{GREY}'/>")
    return ''.join(parts)


def tape_head(cx, tip_y, size, state):
    """The head above a field: a purple marker pointing down at tip_y, the
    state's name in it."""
    w, h = size * 0.72, size * 0.66
    top = tip_y - h
    shoulder = tip_y - h * 0.38
    return (f"<path d='M {cx - w / 2:.1f} {top:.1f} L {cx + w / 2:.1f} {top:.1f} L {cx + w / 2:.1f} {shoulder:.1f} "
            f"L {cx:.1f} {tip_y:.1f} L {cx - w / 2:.1f} {shoulder:.1f} z' fill='{ACCENT}'/>"
            + state_name(cx, top + h * 0.47, state, round(size * 0.3), WHITE))


def stack(x, base, size, symbols, top_current=True, walls=True, width=STROKE):
    """A stack from the bottom (base) upwards, the last symbol on top; walls:
    the open box it stands in."""
    parts = []
    for j, s in enumerate(symbols):
        top = j == len(symbols) - 1
        parts.append(cell(x, base - (j + 1) * size, size, s, current=top and top_current, width=width))
    if walls:
        top_y = base - len(symbols) * size - size * 0.6
        gap = size * 0.12
        parts.append(f"<path d='M {x - gap:.1f} {top_y:.1f} L {x - gap:.1f} {base + gap:.1f} L {x + size + gap:.1f} {base + gap:.1f} "
                     f"L {x + size + gap:.1f} {top_y:.1f}' fill='none' stroke='{INK}' stroke-width='{width * 2}'/>")
    return ''.join(parts)


def frames(x, base, w, h, labels, top_current=True, walls=True, width=STROKE, size=None):
    """A stack of wide entries from the bottom (base) upwards, the last label
    on top: the open calls of a program (»Grammatiken«), drawn like a stack
    of an automaton; walls: the open box it stands in."""
    font = size or round(h * 0.42)
    parts = []
    for j, label in enumerate(labels):
        top = j == len(labels) - 1
        current = top and top_current
        y = base - (j + 1) * h
        stroke = ACCENT if current else INK
        parts.append(f"<rect x='{x:.1f}' y='{y:.1f}' width='{w}' height='{h}' fill='{WHITE}' "
                     f"stroke='{stroke}' stroke-width='{width * (2 if current else 1)}'/>"
                     + text(x + w / 2, y + h / 2 + font * 0.36, label, font, ACCENT if current else INK))
    # the top entry again, so that its border lies on the one below
    if labels and top_current:
        parts.append(parts.pop())
    if walls:
        top_y = base - len(labels) * h - h * 0.6
        gap = h * 0.12
        parts.append(f"<path d='M {x - gap:.1f} {top_y:.1f} L {x - gap:.1f} {base + gap:.1f} L {x + w + gap:.1f} {base + gap:.1f} "
                     f"L {x + w + gap:.1f} {top_y:.1f}' fill='none' stroke='{INK}' stroke-width='{width * 2}'/>")
    return ''.join(parts)
