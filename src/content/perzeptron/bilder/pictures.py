#!/usr/bin/env python3
"""The pictures of »Ein Neuron lernt«: schnabel.svg (a penguin's head and the
two measurements of its bill) and teaser.svg (the 274 Adélie and Gentoo
penguins of penguins.csv and the line the perceptron learns, weights 29.1 and
-74.0, bias -1, from the page's train). Writes both into the activity's
folder; render them with
src/scripts/render-svg.rb src/content/perzeptron/schnabel.svg 960 and
src/scripts/render-svg.rb src/content/perzeptron/teaser.svg 1280."""

from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
DATA = HERE.parent.parent / "static" / "perzeptron" / "penguins.csv"
SANS = "'IBM Plex Sans', system-ui, sans-serif"
TEXT, BLUE, AMBER, GREY, LINE = "#1f2937", "#2b5fb4", "#d97706", "#6b7280", "#d1d5db"
DARK, WHITE = "#0b0f19", "#fdfdfd"


def arrow(x1, y1, x2, y2, colour):
    """A measuring line with an arrowhead at both ends."""
    out = [f"<line x1='{x1}' y1='{y1}' x2='{x2}' y2='{y2}' stroke='{colour}' stroke-width='4'/>"]
    for (ax, ay), (bx, by) in (((x1, y1), (x2, y2)), ((x2, y2), (x1, y1))):
        dx, dy = bx - ax, by - ay
        length = (dx * dx + dy * dy) ** 0.5
        ux, uy = dx / length, dy / length
        # the tip on the end, the head pointing outwards
        tip = (ax, ay)
        base = (ax + ux * 18, ay + uy * 18)
        left = (base[0] - uy * 9, base[1] + ux * 9)
        right = (base[0] + uy * 9, base[1] - ux * 9)
        out.append(f"<polygon points='{tip[0]:.1f},{tip[1]:.1f} {left[0]:.1f},{left[1]:.1f} {right[0]:.1f},{right[1]:.1f}' fill='{colour}'/>")
    return "\n".join(out)


def schnabel():
    w, h = 960, 540
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"<rect width='{w}' height='{h}' fill='#ffffff'/>",
           # the head and the neck, outlined so that they stand out in dark mode
           f"<path d='M 190 540 L 190 390 A 170 150 0 1 1 470 390 L 470 540' fill='{DARK}' stroke='{GREY}' stroke-width='3'/>",
           # the white of the face (Gentoo: a band over the eye)
           f"<path d='M 300 200 Q 360 170 420 205 Q 380 215 330 225 Z' fill='{WHITE}'/>",
           f"<circle cx='395' cy='255' r='12' fill='{WHITE}'/>",
           f"<circle cx='398' cy='256' r='6' fill='{DARK}'/>",
           # the bill: from its root at the face to the tip
           f"<path d='M 480 262 L 720 300 L 480 338 Q 470 300 480 262 Z' fill='{AMBER}'/>",
           f"<line x1='480' y1='300' x2='705' y2='300' stroke='{DARK}' stroke-width='3'/>",
           # the length, along the top of the bill
           arrow(480, 220, 720, 220, BLUE),
           f"<line x1='480' y1='232' x2='480' y2='262' stroke='{BLUE}' stroke-width='2' stroke-dasharray='5 5'/>",
           f"<line x1='720' y1='232' x2='720' y2='296' stroke='{BLUE}' stroke-width='2' stroke-dasharray='5 5'/>",
           f"<text x='600' y='200' text-anchor='middle' font-family=\"{SANS}\" font-size='34' fill='{TEXT}'>Länge</text>",
           # the depth, at the root of the bill
           arrow(770, 262, 770, 338, BLUE),
           f"<line x1='486' y1='262' x2='758' y2='262' stroke='{BLUE}' stroke-width='2' stroke-dasharray='5 5'/>",
           f"<line x1='486' y1='338' x2='758' y2='338' stroke='{BLUE}' stroke-width='2' stroke-dasharray='5 5'/>",
           f"<text x='790' y='312' font-family=\"{SANS}\" font-size='34' fill='{TEXT}'>Höhe</text>",
           "</svg>"]
    (HERE / "schnabel.svg").write_text("\n".join(out) + "\n")


def teaser():
    w, h = 1280, 720
    rows = []
    for line in DATA.read_text().splitlines()[1:]:
        name, _island, length, depth = line.split(",")[:4]
        if name in ("Adelie", "Gentoo"):
            rows.append((name, float(length), float(depth)))
    x0, x1, y0, y1 = 31.0, 61.0, 12.5, 22.0
    left, right, top, bottom = 150, 1200, 50, 620

    def sx(x):
        return left + (x - x0) / (x1 - x0) * (right - left)

    def sy(y):
        return bottom - (y - y0) / (y1 - y0) * (bottom - top)

    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"<rect width='{w}' height='{h}' fill='#ffffff'/>",
           f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='3'/>",
           f"<line x1='{left}' y1='{bottom}' x2='{left}' y2='{top}' stroke='{LINE}' stroke-width='3'/>",
           f"<text x='{(left + right) / 2}' y='{bottom + 62}' text-anchor='middle' font-family=\"{SANS}\" font-size='34' fill='{GREY}'>Länge des Schnabels</text>",
           f"<text x='{left - 30}' y='{(top + bottom) / 2}' text-anchor='middle' font-family=\"{SANS}\" font-size='34' fill='{GREY}' transform='rotate(-90 {left - 30} {(top + bottom) / 2})'>Höhe</text>"]
    for name, length, depth in rows:
        colour = BLUE if name == "Gentoo" else AMBER
        out.append(f"<circle cx='{sx(length):.1f}' cy='{sy(depth):.1f}' r='8' fill='{colour}' fill-opacity='0.85'/>")
    # the line where 29.1 · length - 74 · depth - 1 = 0
    wl, wd, b = 29.1, -74.0, -1.0
    ya, yb = -(wl * x0 + b) / wd, -(wl * x1 + b) / wd
    out.append(f"<line x1='{sx(x0):.1f}' y1='{sy(ya):.1f}' x2='{sx(x1):.1f}' y2='{sy(yb):.1f}' stroke='{TEXT}' stroke-width='5'/>")
    out.append("</svg>")
    (HERE / "teaser.svg").write_text("\n".join(out) + "\n")


schnabel()
teaser()
