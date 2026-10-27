#!/usr/bin/env python3
"""The card picture of »Ein winziges Sprachmodell«: the word »Es« and the
words that follow it in the tales, each arrow as thick as it was counted
(top(counts, "Es", 5) on maerchen.txt). Writes ../teaser.svg; render it with
src/scripts/render-svg.rb src/content/sprachmodell/teaser.svg 1280."""

import math
from pathlib import Path

W, H = 1280, 720
FOLLOWERS = [("war", 41), ("ist", 21), ("trug", 13), ("soll", 5), ("wird", 5)]
SANS = "'IBM Plex Sans', system-ui, sans-serif"
TEXT, BLUE, BOX, GREY, LINE = "#1f2937", "#2b5fb4", "#eef3fb", "#6b7280", "#d1d5db"

source = (330, 360, 200, 130)          # centre x, y, width, height
targets = [(930, 112 + i * 124, 230, 92) for i in range(len(FOLLOWERS))]


def border(box, towards):
    """Where the line from the box's centre to towards leaves the box."""
    cx, cy, w, h = box
    dx, dy = towards[0] - cx, towards[1] - cy
    t = min((w / 2) / abs(dx) if dx else math.inf, (h / 2) / abs(dy) if dy else math.inf)
    return cx + dx * t, cy + dy * t


def rect(box, fill, stroke):
    cx, cy, w, h = box
    return f"<rect x='{cx - w / 2}' y='{cy - h / 2}' width='{w}' height='{h}' rx='18' fill='{fill}' stroke='{stroke}' stroke-width='3'/>"


out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
       f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
top = FOLLOWERS[0][1]
for (word, count), box in zip(FOLLOWERS, targets):
    width = 3 + 22 * count / top
    head = 16 + 1.4 * width
    start = border(source, box[:2])
    end = border(box, source[:2])
    angle = math.atan2(end[1] - start[1], end[0] - start[0])
    # the line stops where the arrowhead begins; the tip sits on the border
    base = (end[0] - head * math.cos(angle), end[1] - head * math.sin(angle))
    out.append(f"<line x1='{start[0]:.1f}' y1='{start[1]:.1f}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' stroke='{BLUE}' stroke-width='{width:.1f}' stroke-linecap='round'/>")
    spread = head * 0.55 + width / 2
    left = (base[0] + spread * math.sin(angle), base[1] - spread * math.cos(angle))
    right = (base[0] - spread * math.sin(angle), base[1] + spread * math.cos(angle))
    out.append(f"<polygon points='{end[0]:.1f},{end[1]:.1f} {left[0]:.1f},{left[1]:.1f} {right[0]:.1f},{right[1]:.1f}' fill='{BLUE}'/>")
    out.append(rect(box, BOX, BLUE))
    out.append(f"<text x='{box[0] - box[2] / 2 + 28}' y='{box[1] + 15}' font-family=\"{SANS}\" font-size='42' fill='{TEXT}'>{word}</text>")
    out.append(f"<text x='{box[0] + box[2] / 2 - 26}' y='{box[1] + 13}' text-anchor='end' font-family=\"{SANS}\" font-size='34' font-weight='600' fill='{GREY}'>{count}</text>")
out.append(rect(source, "#ffffff", TEXT))
out.append(f"<text x='{source[0]}' y='{source[1] + 24}' text-anchor='middle' font-family=\"{SANS}\" font-size='68' font-weight='600' fill='{TEXT}'>Es</text>")
out.append("</svg>")
Path(__file__).resolve().parent.parent.joinpath("teaser.svg").write_text("\n".join(out) + "\n")
