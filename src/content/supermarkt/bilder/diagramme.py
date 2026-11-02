#!/usr/bin/env python3
"""The pictures of »Supermarkt« drawn in Python, in the style of the card
picture (teaser.svg: a checkout as a lane, a customer as a block with the
number of articles in it) and the diagram palette of render-svg.rb:

- kasse: »Wie lange wartet sie?«, one checkout from second 0 to 180. The
  first customer (10 articles) from 0 to 90, the second (5 articles)
  arrives at 30, waits until 90 and is served until 165. Above the lane,
  the one number the checkout keeps: from when it is free again.

    python3 src/content/supermarkt/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/supermarkt/kasse.svg 1560
"""

from pathlib import Path

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
MONO = "'0xProto Nerd Font Mono', 'DejaVu Sans Mono', monospace"
TEXT, BLUE, GREY, GREEN, LANE, LINE = "#1f2937", "#2b5fb4", "#6b7280", "#16a34a", "#eef3fb", "#d1d5db"
OUT = Path(__file__).resolve().parent.parent

W, H = 780, 300
X0, PER_SECOND = 130, 3.4


def x(second):
    return X0 + PER_SECOND * second


def label(px, py, text, size=15, colour=TEXT, anchor="middle", weight="400", font=SANS):
    return (f"<text x='{px:.1f}' y='{py:.1f}' text-anchor='{anchor}' font-family=\"{font}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def kasse():
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
           "  <!-- Eine Kasse von Sekunde 0 bis 180; gemacht von bilder/diagramme.py, als Bild:\n"
           "       ruby src/scripts/render-svg.rb src/content/supermarkt/kasse.svg 1560 -->",
           f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
    lane_top, lane_h = 80, 64
    wait_top, wait_h = 178, 40
    axis = 258
    # from when the checkout is free again, after each customer
    for second in (90, 165):
        out.append(f"<line x1='{x(second):.1f}' y1='46' x2='{x(second):.1f}' y2='{lane_top}' stroke='{GREY}' "
                   f"stroke-width='1.5' stroke-dasharray='4 4'/>")
        out.append(label(x(second), 36, f"frei ab {second}", 15, GREY))
    # the lane with the two customers
    out.append(label(X0 - 18, lane_top + lane_h / 2 + 6, "Kasse", 17, TEXT, "end", "700"))
    out.append(f"<rect x='{X0}' y='{lane_top}' width='{PER_SECOND * 180:.1f}' height='{lane_h}' rx='10' fill='{LANE}'/>")
    for start, end, items, colour in [(0, 90, 10, BLUE), (90, 165, 5, GREEN)]:
        out.append(f"<rect x='{x(start) + 2:.1f}' y='{lane_top + 7}' width='{x(end) - x(start) - 4:.1f}' "
                   f"height='{lane_h - 14}' rx='8' fill='{colour}'/>")
        out.append(label((x(start) + x(end)) / 2, lane_top + lane_h / 2 + 8, items, 22, "#ffffff", weight="700", font=MONO))
    # the second customer: arrives at 30, waits until 90
    out.append(label(X0 - 18, wait_top + wait_h / 2 + 6, "zweite Kundin", 15, TEXT, "end", "600"))
    out.append(f"<rect x='{x(30):.1f}' y='{wait_top}' width='{x(90) - x(30):.1f}' height='{wait_h}' rx='8' "
               f"fill='none' stroke='{GREEN}' stroke-width='2' stroke-dasharray='6 4'/>")
    out.append(label((x(30) + x(90)) / 2, wait_top + wait_h / 2 + 6, "wartet 60 s", 15, GREEN, weight="600"))
    out.append(f"<line x1='{x(90):.1f}' y1='{wait_top}' x2='{x(90):.1f}' y2='{lane_top + lane_h - 7}' stroke='{GREEN}' stroke-width='2'/>")
    out.append(f"<circle cx='{x(30):.1f}' cy='{wait_top + wait_h / 2}' r='5' fill='{GREEN}'/>")
    out.append(label(x(30) - 10, wait_top + wait_h / 2 + 5, "kommt an", 14, GREEN, "end"))
    # the axis
    out.append(f"<line x1='{X0}' y1='{axis}' x2='{x(180):.1f}' y2='{axis}' stroke='{LINE}' stroke-width='2'/>")
    for second in range(0, 181, 30):
        out.append(f"<line x1='{x(second):.1f}' y1='{axis - 5}' x2='{x(second):.1f}' y2='{axis + 5}' stroke='{GREY}' stroke-width='1.5'/>")
        out.append(label(x(second), axis + 24, f"{second} s" if second == 180 else second, 14, GREY))
    out.append("</svg>")
    (OUT / "kasse.svg").write_text("\n".join(out) + "\n")
    print(OUT / "kasse.svg")


kasse()
