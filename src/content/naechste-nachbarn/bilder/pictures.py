#!/usr/bin/env python3
"""The pictures of »Nächste Nachbarn«, written into the activity's folder:

- teaser.svg: a part of the penguins of penguins.csv, Adélie amber, Chinstrap
  purple, Gentoo blue, and an unknown penguin (44.5 mm, 17 mm) with lines
  to its five nearest neighbours: the nearest is a Gentoo, three of the
  five are Chinstrap.
- stadt.svg: the made-up town of customers.csv, its river, its four
  districts, the shop, and where the 300 customers of the past live.
- stadt-kaution.svg: the same, each customer coloured by the staff's
  decision (amber: a deposit).

Render each with src/scripts/render-svg.rb, the teaser at 1280, the maps at
900."""

import math
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
STATIC = HERE.parent.parent / "static" / "naechste-nachbarn"
SANS = "'IBM Plex Sans', system-ui, sans-serif"
TEXT, BLUE, AMBER, PURPLE, GREY, LINE, WATER, WHITE = (
    "#1f2937", "#2b5fb4", "#d97706", "#7c3aed", "#6b7280", "#d1d5db", "#eef3fb", "#ffffff")
COLOURS = {"Adelie": AMBER, "Chinstrap": PURPLE, "Gentoo": BLUE}


def text(x, y, label, size=30, fill=TEXT, anchor="middle", weight="normal"):
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='{anchor}' font-family=\"{SANS}\" "
            f"font-size='{size}' font-weight='{weight}' fill='{fill}'>{label}</text>")


def teaser():
    w, h = 1280, 720
    penguins = []
    for line in (STATIC / "penguins.csv").read_text().splitlines()[1:]:
        species, _island, length, depth = line.split(",")[:4]
        penguins.append((species, float(length), float(depth)))
    x0, x1, y0, y1 = 41.0, 48.5, 15.0, 19.0
    left, right, top, bottom = 150, 1210, 40, 620

    def sx(x):
        return left + (x - x0) / (x1 - x0) * (right - left)

    def sy(y):
        return bottom - (y - y0) / (y1 - y0) * (bottom - top)

    unknown = (44.5, 17.0)
    shown = [p for p in penguins if x0 < p[1] < x1 and y0 < p[2] < y1]
    nearest = sorted(penguins, key=lambda p: math.dist(p[1:], unknown))[:5]
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"<rect width='{w}' height='{h}' fill='{WHITE}'/>",
           f"<line x1='{left}' y1='{bottom}' x2='{right}' y2='{bottom}' stroke='{LINE}' stroke-width='3'/>",
           f"<line x1='{left}' y1='{bottom}' x2='{left}' y2='{top}' stroke='{LINE}' stroke-width='3'/>",
           text((left + right) / 2, bottom + 62, "Länge des Schnabels", 34, GREY),
           f"<g transform='rotate(-90 {left - 34} {(top + bottom) / 2})'>{text(left - 34, (top + bottom) / 2, 'Höhe', 34, GREY)}</g>"]
    for _species, length, depth in nearest:
        out.append(f"<line x1='{sx(unknown[0]):.1f}' y1='{sy(unknown[1]):.1f}' x2='{sx(length):.1f}' y2='{sy(depth):.1f}' "
                   f"stroke='{TEXT}' stroke-width='4'/>")
    for species, length, depth in shown:
        out.append(f"<circle cx='{sx(length):.1f}' cy='{sy(depth):.1f}' r='11' fill='{COLOURS[species]}' "
                   f"stroke='{WHITE}' stroke-width='2'/>")
    for species, length, depth in nearest:
        out.append(f"<circle cx='{sx(length):.1f}' cy='{sy(depth):.1f}' r='16' fill='none' stroke='{TEXT}' stroke-width='4'/>")
    ux, uy = sx(unknown[0]), sy(unknown[1])
    out.append(f"<circle cx='{ux:.1f}' cy='{uy:.1f}' r='22' fill='{WHITE}' stroke='{TEXT}' stroke-width='5'/>")
    out.append(text(ux, uy + 11, "?", 32, TEXT, weight="bold"))
    # the legend, in the top right corner
    for i, (species, label) in enumerate([("Adelie", "Adéliepinguin"), ("Chinstrap", "Zügelpinguin"), ("Gentoo", "Eselspinguin")]):
        y = top + 30 + i * 46
        out.append(f"<rect x='{right - 280}' y='{y - 32}' width='280' height='46' fill='{WHITE}'/>")
        out.append(f"<circle cx='{right - 260}' cy='{y - 9}' r='11' fill='{COLOURS[species]}'/>")
        out.append(text(right - 238, y, label, 30, TEXT, anchor="start"))
    out.append("</svg>")
    (HERE / "teaser.svg").write_text("\n".join(out) + "\n")


def town(path, coloured):
    w, h = 900, 1040
    left, top, size = 50, 70, 800
    people = []
    lines = (STATIC / "customers.csv").read_text().splitlines()
    keys = lines[0].split(",")
    for i, line in enumerate(lines[1:]):
        if i % 2 == 0:
            people.append(dict(zip(keys, line.split(","))))

    def sx(x):
        return left + x / 8 * size

    def sy(y):
        return top + size - y / 8 * size

    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"<rect width='{w}' height='{h}' fill='{WHITE}'/>",
           f"<rect x='{left}' y='{top}' width='{size}' height='{size}' fill='none' stroke='{LINE}' stroke-width='3'/>",
           # the river between y = 2.85 and 3.15 km
           f"<rect x='{left}' y='{sy(3.15):.1f}' width='{size}' height='{sy(2.85) - sy(3.15):.1f}' fill='{WATER}' stroke='{BLUE}' stroke-width='2'/>",
           # the districts north of the river: Lindenhof up to x = 3, Bahnhofsviertel from x = 5
           f"<line x1='{sx(3)}' y1='{top}' x2='{sx(3)}' y2='{sy(3.15):.1f}' stroke='{GREY}' stroke-width='2' stroke-dasharray='10 8'/>",
           f"<line x1='{sx(5)}' y1='{top}' x2='{sx(5)}' y2='{sy(3.15):.1f}' stroke='{GREY}' stroke-width='2' stroke-dasharray='10 8'/>"]
    for person in people:
        if coloured:
            fill = AMBER if person["deposit"] == "1" else BLUE
        else:
            fill = GREY
        out.append(f"<circle cx='{sx(float(person['x'])):.1f}' cy='{sy(float(person['y'])):.1f}' r='7' fill='{fill}' "
                   f"stroke='{WHITE}' stroke-width='1.5'/>")
    # the districts' names outside the map: above it the three north of the
    # river, below it Sandberg
    for label, x in [("Lindenhof", 1.5), ("Altstadt", 4), ("Bahnhofsviertel", 6.5)]:
        out.append(text(sx(x), top - 18, label, 30, TEXT, weight="bold"))
    out.append(text(sx(4), top + size + 40, "Sandberg", 30, TEXT, weight="bold"))
    sx0, sy0 = sx(4), sy(5)
    out.append(f"<rect x='{sx0 - 15:.1f}' y='{sy0 - 15:.1f}' width='30' height='30' fill='{TEXT}' stroke='{WHITE}' stroke-width='3'/>")
    out.append(f"<rect x='{sx0 + 22:.1f}' y='{sy0 - 22:.1f}' width='96' height='42' rx='8' fill='{WHITE}'/>")
    out.append(text(sx0 + 26, sy0 + 10, "Laden", 28, TEXT, anchor="start"))
    out.append(text(left + size - 12, sy(3) + 8, "Fluss", 22, BLUE, anchor="end"))
    # the scale, 1 km, and the legend in a row below
    base = top + size + 100
    out.append(f"<line x1='{left}' y1='{base}' x2='{sx(1)}' y2='{base}' stroke='{TEXT}' stroke-width='4'/>")
    out.append(text(left + size / 16, base + 36, "1 km", 26, TEXT))
    if coloured:
        lx = left + 330
        out.append(f"<circle cx='{lx}' cy='{base + 2}' r='10' fill='{AMBER}'/>")
        out.append(text(lx + 20, base + 12, "mit Kaution", 28, TEXT, anchor="start"))
        out.append(f"<circle cx='{lx + 240}' cy='{base + 2}' r='10' fill='{BLUE}'/>")
        out.append(text(lx + 260, base + 12, "ohne Kaution", 28, TEXT, anchor="start"))
    out.append("</svg>")
    (HERE / path).write_text("\n".join(out) + "\n")


teaser()
town("stadt.svg", False)
town("stadt-kaution.svg", True)
