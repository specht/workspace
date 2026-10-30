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
- karte.svg: the card's picture, unlike the penguins of »Ein Neuron lernt«:
  the south of the town in 16:9, the customers coloured by the staff's
  decision, and a new customer in Sandberg with lines to the five nearest,
  most of them asked for a deposit.

- abstand.svg: two penguins as points a and b, the differences of their
  measures as the legs of a right triangle, the distance d its hypotenuse
  (»Wie weit zwei Pinguine auseinanderliegen«), labels typeset by LaTeX.

Render each with src/scripts/render-svg.rb, the teaser and the card at 1280,
the maps at 900, abstand at 1120."""

import math
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(HERE.parent.parent / 'scripts'))
from diagram_labels import math as tex  # noqa: E402
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
    for i, (species, label) in enumerate([("Adelie", "Adélie"), ("Chinstrap", "Zügelpinguin"), ("Gentoo", "Eselspinguin")]):
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


def card():
    w, h = 1280, 720
    # the town from x = 0 to 8 km, y from 0 to 4.5 km: Sandberg, the river,
    # the south of the districts beyond it
    scale = w / 8

    def sx(x):
        return x * scale

    def sy(y):
        return h - y * (h / 4.5)

    lines = (STATIC / "customers.csv").read_text().splitlines()
    keys = lines[0].split(",")
    people = [dict(zip(keys, line.split(","))) for i, line in enumerate(lines[1:]) if i % 2 == 0]
    people = [p for p in people if float(p["y"]) <= 4.6]
    # where the legend and the district's name stand, no customers
    boxes = [(24, 20, 444, 80), (24, sy(0.25) - 40, 214, sy(0.25) + 14)]
    free = lambda p: not any(x0 - 12 <= sx(float(p["x"])) <= x1 + 12 and y0 - 12 <= sy(float(p["y"])) <= y1 + 12 for x0, y0, x1, y1 in boxes)
    people = [p for p in people if free(p)]
    new = (5.1, 1.5)
    near = sorted(people, key=lambda p: math.hypot(float(p["x"]) - new[0], float(p["y"]) - new[1]))[:5]
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           f"<rect width='{w}' height='{h}' fill='{WHITE}'/>",
           f"<rect x='0' y='{sy(3.15):.1f}' width='{w}' height='{sy(2.85) - sy(3.15):.1f}' fill='{WATER}' stroke='{BLUE}' stroke-width='2'/>"]
    for person in near:
        out.append(f"<line x1='{sx(new[0]):.1f}' y1='{sy(new[1]):.1f}' x2='{sx(float(person['x'])):.1f}' y2='{sy(float(person['y'])):.1f}' "
                   f"stroke='{TEXT}' stroke-width='3'/>")
    for person in people:
        fill = AMBER if person["deposit"] == "1" else BLUE
        ring = person in near
        out.append(f"<circle cx='{sx(float(person['x'])):.1f}' cy='{sy(float(person['y'])):.1f}' r='{11 if ring else 9}' fill='{fill}' "
                   f"stroke='{TEXT if ring else WHITE}' stroke-width='{3 if ring else 2}'/>")
    out.append(f"<circle cx='{sx(new[0]):.1f}' cy='{sy(new[1]):.1f}' r='24' fill='{WHITE}' stroke='{TEXT}' stroke-width='4'/>")
    out.append(text(sx(new[0]), sy(new[1]) + 11, "?", 32, TEXT, weight="bold"))
    out.append(f"<rect x='24' y='{sy(0.25) - 40:.1f}' width='190' height='54' rx='12' fill='{WHITE}'/>")
    out.append(text(40, sy(0.25), "Sandberg", 34, TEXT, anchor="start", weight="bold"))
    out.append(text(w - 40, sy(3) + 8, "Fluss", 26, BLUE, anchor="end"))
    # the legend, top left, on white
    out.append(f"<rect x='24' y='20' width='420' height='60' rx='12' fill='{WHITE}' stroke='{LINE}' stroke-width='2'/>")
    out.append(f"<circle cx='54' cy='50' r='11' fill='{AMBER}'/>")
    out.append(text(74, 60, "mit Kaution", 28, TEXT, anchor="start"))
    out.append(f"<circle cx='254' cy='50' r='11' fill='{BLUE}'/>")
    out.append(text(274, 60, "ohne Kaution", 28, TEXT, anchor="start"))
    out.append("</svg>")
    (HERE / "karte.svg").write_text("\n".join(out) + "\n")



def abstand():
    w, h = 560, 300
    left, bottom = 50, 262
    a, b = (420, 70), (140, 210)
    corner = (a[0], b[1])
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           "  <!-- Der Abstand zweier Pinguine als Hypotenuse; gemacht von bilder/pictures.py, als Bild:\n"
           "       ruby src/scripts/render-svg.rb src/content/naechste-nachbarn/abstand.svg 1120 -->",
           f"<rect width='{w}' height='{h}' fill='{WHITE}'/>",
           # the axes, pale, as in the other pictures
           f"<line x1='{left}' y1='{bottom}' x2='{w - 30}' y2='{bottom}' stroke='{LINE}' stroke-width='2'/>",
           f"<line x1='{left}' y1='{bottom}' x2='{left}' y2='20' stroke='{LINE}' stroke-width='2'/>",
           text(w - 30, bottom + 26, "Länge", 16, GREY, "end"),
           f"<text x='{left - 14}' y='30' text-anchor='end' font-family=\"{SANS}\" font-size='16' fill='{GREY}' "
           f"transform='rotate(-90 {left - 14} 30)'>Höhe</text>",
           # the legs, and the right angle at their corner: an arc with a dot
           f"<line x1='{b[0]}' y1='{b[1]}' x2='{corner[0]}' y2='{corner[1]}' stroke='{GREY}' stroke-width='2' stroke-dasharray='7 5'/>",
           f"<line x1='{corner[0]}' y1='{corner[1]}' x2='{a[0]}' y2='{a[1]}' stroke='{GREY}' stroke-width='2' stroke-dasharray='7 5'/>",
           f"<path d='M{corner[0] - 26} {corner[1]} A26 26 0 0 1 {corner[0]} {corner[1] - 26}' fill='none' stroke='{GREY}' stroke-width='1.5'/>",
           f"<circle cx='{corner[0] - 9}' cy='{corner[1] - 9}' r='2.5' fill='{GREY}'/>",
           # the distance
           f"<line x1='{b[0]}' y1='{b[1]}' x2='{a[0]}' y2='{a[1]}' stroke='{BLUE}' stroke-width='3.5'/>",
           f"<circle cx='{a[0]}' cy='{a[1]}' r='8' fill='{TEXT}'/>",
           f"<circle cx='{b[0]}' cy='{b[1]}' r='8' fill='{TEXT}'/>",
           tex(r'a = (a_1, a_2)', a[0] + 16, a[1] + 6, 19),
           tex(r'b = (b_1, b_2)', b[0] - 16, b[1] + 26, 19, anchor='end'),
           tex(r'a_1 - b_1', (b[0] + corner[0]) / 2, corner[1] + 30, 19, color=GREY, anchor='middle'),
           tex(r'a_2 - b_2', corner[0] + 14, (a[1] + corner[1]) / 2 + 6, 19, color=GREY),
           tex(r'd', (a[0] + b[0]) / 2 - 16, (a[1] + b[1]) / 2 - 10, 22, color=BLUE, anchor='end'),
           "</svg>"]
    (HERE / "abstand.svg").write_text("\n".join(out) + "\n")


teaser()
town("stadt.svg", False)
town("stadt-kaution.svg", True)
card()
abstand()
