#!/usr/bin/env python3
"""The card picture of »Was eine Webseite über dich erfährt«: the two
websites of the page, each sending its pixel request to the tracker, which
knows the same visitor on both by the cookie and turns the keywords into a
profile (the pages of the alt text: two recipes with »Kochen«, two sport
pages with »Sport«, and Kochen read once more on the start page). Writes
../teaser.svg; render it with
src/scripts/render-svg.rb src/content/tracking/teaser.svg 1280."""

import math
from pathlib import Path

W, H = 1280, 720
SANS = "'IBM Plex Sans', system-ui, sans-serif"
MONO = "'IBM Plex Mono', ui-monospace, monospace"
TEXT, BLUE, BOX, GREY, LINE, AMBER = "#1f2937", "#2b5fb4", "#eef3fb", "#6b7280", "#d1d5db", "#d97706"

sites = [((240, 210, 400, 230), "Rezepte für jeden Tag", ["pizza.html", "curry.html", "index.html"]),
         ((240, 515, 400, 190), "Laufend fit", ["laufen.html", "fussball.html"])]
tracker = (705, 360, 300, 210)
profile = (1085, 360, 300, 300)
interests = [("Kochen", 3), ("Sport", 2), ("Pizza", 1)]


def border(box, towards):
    """Where the line from the box's centre to towards leaves the box."""
    cx, cy, w, h = box
    dx, dy = towards[0] - cx, towards[1] - cy
    t = min((w / 2) / abs(dx) if dx else math.inf, (h / 2) / abs(dy) if dy else math.inf)
    return cx + dx * t, cy + dy * t


def rect(box, fill, stroke):
    cx, cy, w, h = box
    return f"<rect x='{cx - w / 2}' y='{cy - h / 2}' width='{w}' height='{h}' rx='18' fill='{fill}' stroke='{stroke}' stroke-width='3'/>"


def arrow(a, b, colour, width=5):
    """An arrow from box a to box b, the tip on b's border, the head along the line."""
    start, end = border(a, b[:2]), border(b, a[:2])
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    head = 22
    base = (end[0] - head * math.cos(angle), end[1] - head * math.sin(angle))
    spread = 12
    left = (base[0] + spread * math.sin(angle), base[1] - spread * math.cos(angle))
    right = (base[0] - spread * math.sin(angle), base[1] + spread * math.cos(angle))
    return (f"<line x1='{start[0]:.1f}' y1='{start[1]:.1f}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' stroke='{colour}' stroke-width='{width}' stroke-linecap='round'/>"
            f"<polygon points='{end[0]:.1f},{end[1]:.1f} {left[0]:.1f},{left[1]:.1f} {right[0]:.1f},{right[1]:.1f}' fill='{colour}'/>")


def text(x, y, content, size, colour=TEXT, anchor="start", weight="400", family=SANS):
    return f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{family}\" font-size='{size}' font-weight='{weight}' fill='{colour}'>{content}</text>"


out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
       f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
for box, name, pages in sites:
    out.append(arrow(box, tracker, BLUE))
for box, name, pages in sites:
    cx, cy, w, h = box
    out.append(rect(box, BOX, BLUE))
    out.append(text(cx - w / 2 + 26, cy - h / 2 + 50, name, 32, weight="600"))
    for i, page in enumerate(pages):
        out.append(text(cx - w / 2 + 26, cy - h / 2 + 100 + i * 42, page, 28, GREY, family=MONO))
out.append(arrow(tracker, profile, AMBER))
cx, cy, w, h = tracker
out.append(rect(tracker, "#ffffff", TEXT))
out.append(text(cx, cy - 30, "Tracker", 44, anchor="middle", weight="600"))
out.append(text(cx, cy + 22, "pixel.gif", 30, GREY, anchor="middle", family=MONO))
out.append(text(cx, cy + 68, "visitor=3f9a1c2b", 28, AMBER, anchor="middle", family=MONO))
cx, cy, w, h = profile
out.append(rect(profile, "#ffffff", AMBER))
out.append(text(cx - w / 2 + 28, cy - h / 2 + 54, "Profil", 40, weight="600"))
top = interests[0][1]
for i, (keyword, count) in enumerate(interests):
    y = cy - h / 2 + 108 + i * 62
    out.append(text(cx - w / 2 + 28, y + 10, keyword, 28))
    bar = 80 * count / top
    out.append(f"<rect x='{cx - w / 2 + 145}' y='{y - 14}' width='{bar:.1f}' height='26' rx='6' fill='{AMBER}'/>")
    out.append(text(cx + w / 2 - 22, y + 10, str(count), 28, GREY, anchor="end", weight="600"))
out.append("</svg>")
Path(__file__).resolve().parent.parent.joinpath("teaser.svg").write_text("\n".join(out) + "\n")
