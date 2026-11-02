#!/usr/bin/env python3
"""The pictures of »Nebenläufigkeit« drawn in Python, in the style of
dns/dns-tree.svg (IBM Plex Sans, the diagram palette of render-svg.rb):

- philosophen: three philosophers A, B and C at a round table, a plate in
  front of each, a fork between every two: f2 between A and B, f3 between
  B and C, f1 between C and A, so that A needs f1 and f2, B f2 and f3,
  C f3 and f1, as in the widget.
- philosophen-deadlock: the same table after each has taken his first fork:
  a blue line to the fork he holds, a dashed red arrow to the one he waits
  for, which his neighbour holds; the arrows go round in a circle.

    python3 src/content/nebenlaeufigkeit/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/nebenlaeufigkeit/philosophen.svg 1160
    ruby src/scripts/render-svg.rb src/content/nebenlaeufigkeit/philosophen-deadlock.svg 1160
"""

import math as m
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[3] / 'scripts'))
from diagram_labels import code  # noqa: E402

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
TEXT, BLUE, GREY, LINE, RED, BOX = "#1f2937", "#2b5fb4", "#6b7280", "#d1d5db", "#dc2626", "#eef3fb"
OUT = Path(__file__).resolve().parent.parent

W = 580
CX, CY = 290, 270
TABLE = 150
SEAT = 205           # the philosophers, outside the table
PLATE = 100
FORK = 116
# angles in degrees, y going down: A at the top, B lower right, C lower left
PEOPLE = {"A": -90, "B": 30, "C": 150}
FORKS = {"f2": -30, "f3": 90, "f1": 210}
# who takes which fork first, and which he then waits for
FIRST = {"A": "f1", "B": "f2", "C": "f3"}
SECOND = {"A": "f2", "B": "f3", "C": "f1"}


def at(radius, angle):
    a = m.radians(angle)
    return CX + radius * m.cos(a), CY + radius * m.sin(a)


def label(x, y, text, size=16, colour=TEXT, anchor="middle", weight="400"):
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='{anchor}' font-family=\"{SANS}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def fork(angle):
    """A fork lying on the table, its tines towards the middle."""
    x, y = at(FORK, angle)
    parts = ["<rect x='-3.5' y='4' width='7' height='28' rx='3.5'/>",
             "<rect x='-10' y='-2' width='20' height='8' rx='3'/>"]
    parts += [f"<rect x='{tx}' y='-28' width='3.6' height='28' rx='1.5'/>" for tx in (-10, -1.8, 6.4)]
    return (f"<g transform='translate({x:.1f} {y:.1f}) rotate({angle - 90})' fill='{TEXT}'>"
            + "".join(parts) + "</g>")


def table(out):
    out.append(f"<circle cx='{CX}' cy='{CY}' r='{TABLE}' fill='{BOX}' stroke='{GREY}' stroke-width='2'/>")
    for angle in PEOPLE.values():
        x, y = at(PLATE, angle)
        out.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='30' fill='#ffffff' stroke='{LINE}' stroke-width='2'/>")
        out.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='19' fill='none' stroke='{LINE}' stroke-width='1.5'/>")
    for name, angle in FORKS.items():
        out.append(fork(angle))
        x, y = at(TABLE + 34, angle)
        out.append(code(name, x, y + 6, size=18, anchor="middle"))


def people(out):
    for name, angle in PEOPLE.items():
        x, y = at(SEAT, angle)
        out.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='34' fill='#ffffff' stroke='{BLUE}' stroke-width='2.5'/>")
        out.append(label(x, y + 10, name, 28, TEXT, weight="600"))


def towards(person, fork_name, gap):
    """From the edge of a philosopher's circle to gap pixels before the
    middle of a fork."""
    x0, y0 = at(SEAT, PEOPLE[person])
    x1, y1 = at(FORK, FORKS[fork_name])
    length = m.hypot(x1 - x0, y1 - y0)
    ux, uy = (x1 - x0) / length, (y1 - y0) / length
    return x0 + 34 * ux, y0 + 34 * uy, x1 - gap * ux, y1 - gap * uy, ux, uy


def holds(person, fork_name):
    x0, y0, x1, y1, _, _ = towards(person, fork_name, 8)
    return (f"<line x1='{x0:.1f}' y1='{y0:.1f}' x2='{x1:.1f}' y2='{y1:.1f}' stroke='{BLUE}' "
            f"stroke-width='5' stroke-linecap='round'/>")


def waits(person, fork_name):
    x0, y0, x1, y1, ux, uy = towards(person, fork_name, 30)
    return arrow(x0, y0, x1, y1, ux, uy)


def arrow(x0, y0, x1, y1, ux, uy):
    bx, by = x1 - 14 * ux, y1 - 14 * uy
    return (f"<line x1='{x0:.1f}' y1='{y0:.1f}' x2='{bx:.1f}' y2='{by:.1f}' stroke='{RED}' stroke-width='2.5' "
            f"stroke-dasharray='8 6'/>"
            f"<polygon points='{x1:.1f},{y1:.1f} {bx - 7 * uy:.1f},{by + 7 * ux:.1f} {bx + 7 * uy:.1f},{by - 7 * ux:.1f}' "
            f"fill='{RED}'/>")


def picture(name, comment, h, body):
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {h}' width='{W}' height='{h}'>",
           f"  <!-- {comment}; gemacht von bilder/diagramme.py, als Bild:\n"
           f"       ruby src/scripts/render-svg.rb src/content/nebenlaeufigkeit/{name}.svg 1160 -->",
           f"<rect width='{W}' height='{h}' fill='#ffffff'/>"]
    body(out)
    out.append("</svg>")
    (OUT / f"{name}.svg").write_text("\n".join(out) + "\n")
    print(OUT / f"{name}.svg")


def setup(out):
    table(out)
    people(out)


def deadlock(out):
    table(out)
    for person in PEOPLE:
        out.append(holds(person, FIRST[person]))
    for person in PEOPLE:
        out.append(waits(person, SECOND[person]))
    people(out)
    y = 520
    out.append(f"<line x1='90' y1='{y - 5}' x2='130' y2='{y - 5}' stroke='{BLUE}' stroke-width='5' stroke-linecap='round'/>")
    out.append(label(142, y, "hält die Gabel", 16, TEXT, "start"))
    out.append(arrow(300, y - 5, 342, y - 5, 1, 0))
    out.append(label(354, y, "wartet auf die Gabel", 16, TEXT, "start"))


picture("philosophen", "Drei Philosophen am runden Tisch, zwischen je zweien eine Gabel", 480, setup)
picture("philosophen-deadlock", "Jeder hält seine erste Gabel und wartet auf die zweite, die sein Nachbar hält", 545, deadlock)
