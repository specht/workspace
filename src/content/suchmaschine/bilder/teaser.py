#!/usr/bin/env python3
"""The card picture of »Eine Suchmaschine«: a few pages of the Tierlexikon
and their links (index, Hund and Amsel link to katze.html, only the Hund to
the ad, which links to itself), and beside them the hits for »katze« ranked
by incoming links (rank_by_links on the crawled Tierlexikon). Writes
../teaser.svg; render it with
src/scripts/render-svg.rb src/content/suchmaschine/teaser.svg 1280."""

import math
from pathlib import Path

W, H = 1280, 720
SANS = "'IBM Plex Sans', system-ui, sans-serif"
MONO = "'IBM Plex Mono', ui-monospace, monospace"
TEXT, BLUE, BOX, GREY, LINE, AMBER = "#1f2937", "#2b5fb4", "#eef3fb", "#6b7280", "#d1d5db", "#d97706"

# centre x, y, width, height
nodes = {
    "index": (160, 130, 230, 76),
    "amsel": (160, 360, 230, 76),
    "hund": (160, 590, 230, 76),
    "katze": (560, 300, 230, 76),
    "ad": (590, 560, 400, 76),
}
labels = {"index": "index.html", "amsel": "amsel.html", "hund": "hund.html",
          "katze": "katze.html", "ad": "katzenfutter-billig.html"}
edges = [("index", "katze", BLUE), ("amsel", "katze", BLUE), ("hund", "katze", BLUE), ("hund", "ad", AMBER)]
hits = ["katze.html", "hund.html", "katzenfutter-billig.html"]


def border(box, towards):
    """Where the line from the box's centre to towards leaves the box."""
    cx, cy, w, h = box
    dx, dy = towards[0] - cx, towards[1] - cy
    t = min((w / 2) / abs(dx) if dx else math.inf, (h / 2) / abs(dy) if dy else math.inf)
    return cx + dx * t, cy + dy * t


def head(tip, angle, colour, size=20, spread=10):
    base = (tip[0] - size * math.cos(angle), tip[1] - size * math.sin(angle))
    left = (base[0] + spread * math.sin(angle), base[1] - spread * math.cos(angle))
    right = (base[0] - spread * math.sin(angle), base[1] + spread * math.cos(angle))
    return base, f"<polygon points='{tip[0]:.1f},{tip[1]:.1f} {left[0]:.1f},{left[1]:.1f} {right[0]:.1f},{right[1]:.1f}' fill='{colour}'/>"


def arrow(a, b, colour):
    """A straight arrow aimed at the centres, the tip on b's border."""
    start, end = border(a, b[:2]), border(b, a[:2])
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    base, tip = head(end, angle, colour)
    return f"<line x1='{start[0]:.1f}' y1='{start[1]:.1f}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' stroke='{colour}' stroke-width='4' stroke-linecap='round'/>" + tip


def self_loop(box, colour):
    """A link of a page to itself: a loop over its right end, the head along the curve."""
    cx, cy, w, h = box
    top = cy - h / 2
    start = (cx + w / 2 - 110, top)
    end = (cx + w / 2 - 30, top)
    control = ((start[0] + end[0]) / 2, top - 110)
    # the curve's direction at its end, from the last control point to the end
    angle = math.atan2(end[1] - control[1], end[0] - control[0])
    base, tip = head(end, angle, colour)
    return (f"<path d='M {start[0]:.1f} {start[1]:.1f} Q {control[0]:.1f} {control[1]:.1f} {base[0]:.1f} {base[1]:.1f}' "
            f"fill='none' stroke='{colour}' stroke-width='4' stroke-linecap='round'/>" + tip)


def text(x, y, content, size, colour=TEXT, anchor="middle", weight="400", family=MONO):
    return f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{family}\" font-size='{size}' font-weight='{weight}' fill='{colour}'>{content}</text>"


out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
       f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
for a, b, colour in edges:
    out.append(arrow(nodes[a], nodes[b], colour))
out.append(self_loop(nodes["ad"], AMBER))
for key, (cx, cy, w, h) in nodes.items():
    stroke = AMBER if key == "ad" else BLUE
    out.append(f"<rect x='{cx - w / 2}' y='{cy - h / 2}' width='{w}' height='{h}' rx='14' fill='{BOX if key != 'ad' else '#ffffff'}' stroke='{stroke}' stroke-width='3'/>")
    out.append(text(cx, cy + 9, labels[key], 24 if len(labels[key]) > 14 else 26))
# the hits
x0, y0 = 830, 190
out.append(f"<rect x='{x0}' y='{y0}' width='420' height='330' rx='18' fill='#ffffff' stroke='{TEXT}' stroke-width='3'/>")
out.append(text(x0 + 30, y0 + 62, "Suche: katze", 34, anchor="start", weight="600", family=SANS))
out.append(f"<line x1='{x0 + 30}' y1='{y0 + 88}' x2='{x0 + 390}' y2='{y0 + 88}' stroke='{LINE}' stroke-width='3'/>")
for i, hit in enumerate(hits):
    y = y0 + 145 + i * 62
    out.append(text(x0 + 30, y, f"{i + 1}.", 28, GREY, anchor="start", weight="600", family=SANS))
    out.append(text(x0 + 68, y, hit, 22 if len(hit) > 14 else 28, TEXT, anchor="start"))
out.append("</svg>")
Path(__file__).resolve().parent.parent.joinpath("teaser.svg").write_text("\n".join(out) + "\n")
