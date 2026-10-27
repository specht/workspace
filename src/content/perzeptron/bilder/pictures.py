#!/usr/bin/env python3
"""The card picture of »Ein Neuron lernt«: teaser.svg, the 274 Adélie and
Gentoo penguins of penguins.csv and the line the perceptron learns, weights
29.1 and -74.0, bias -1, from the page's train. Render it with
src/scripts/render-svg.rb src/content/perzeptron/teaser.svg 1280. The
drawings of penguins on the page (pinguine.webp, schnabel.webp) are Allison
Horst's from palmerpenguins (CC0), put on white and the English note of the
bill's drawing painted over."""

from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
DATA = HERE.parent.parent / "static" / "perzeptron" / "penguins.csv"
SANS = "'IBM Plex Sans', system-ui, sans-serif"
TEXT, BLUE, AMBER, GREY, LINE = "#1f2937", "#2b5fb4", "#d97706", "#6b7280", "#d1d5db"


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


teaser()
