#!/usr/bin/env python3
"""The pictures of »Supermarkt« drawn in Python, in the style of the card
picture (teaser.svg: a checkout as a lane, a customer as a block with the
number of articles in it) and the diagram palette of render-svg.rb:

- kasse: »Wie lange wartet sie?«, one checkout from second 0 to 180. The
  first customer (10 articles) from 0 to 90, the second (5 articles)
  arrives at 30, waits until 90 and is served until 165. Above the lane,
  the one number the checkout keeps: from when it is free again.

    python3 src/content/supermarkt/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/supermarkt/kasse.svg 1328
"""

from pathlib import Path

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
MONO = "'0xProto Nerd Font Mono', 'DejaVu Sans Mono', monospace"
TEXT, BLUE, GREY, GREEN, LANE, LINE = "#1f2937", "#2b5fb4", "#6b7280", "#16a34a", "#eef3fb", "#d1d5db"
OUT = Path(__file__).resolve().parent.parent

W, H = 664, 300
X0, PER_SECOND = 22, 3.4
SIZE = 23            # large enough to read on a phone, where the picture is about 360 px wide


def x(second):
    return X0 + PER_SECOND * second


def label(px, py, text, size=SIZE, colour=TEXT, anchor="middle", weight="400", font=SANS):
    return (f"<text x='{px:.1f}' y='{py:.1f}' text-anchor='{anchor}' font-family=\"{font}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def kasse():
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
           "  <!-- Eine Kasse von Sekunde 0 bis 180; gemacht von bilder/diagramme.py, als Bild:\n"
           "       ruby src/scripts/render-svg.rb src/content/supermarkt/kasse.svg 1328 -->",
           f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
    lane_top, lane_h = 50, 66
    wait_top, wait_h = 168, 44
    axis = 250
    # each row named above it, so that the rows can use the whole width
    out.append(label(X0, lane_top - 14, "Kasse", SIZE, TEXT, "start", "700"))
    out.append(label(X0, wait_top - 14, "zweite Kundin", SIZE, TEXT, "start", "700"))
    # from when the checkout is free again, after each customer
    for second in (90, 165):
        out.append(f"<line x1='{x(second):.1f}' y1='{lane_top - 26}' x2='{x(second):.1f}' y2='{lane_top}' stroke='{GREY}' "
                   f"stroke-width='1.5' stroke-dasharray='4 4'/>")
        out.append(label(x(second) - 8, lane_top - 14, f"frei ab {second}", SIZE - 2, GREY, "end"))
    # the lane with the two customers
    out.append(f"<rect x='{X0}' y='{lane_top}' width='{PER_SECOND * 180:.1f}' height='{lane_h}' rx='10' fill='{LANE}'/>")
    for start, end, items, colour in [(0, 90, 10, BLUE), (90, 165, 5, GREEN)]:
        out.append(f"<rect x='{x(start) + 2:.1f}' y='{lane_top + 7}' width='{x(end) - x(start) - 4:.1f}' "
                   f"height='{lane_h - 14}' rx='8' fill='{colour}'/>")
        out.append(label((x(start) + x(end)) / 2, lane_top + lane_h / 2 + 9, items, 27, "#ffffff", weight="700", font=MONO))
    # the second customer: arrives at 30, waits until 90
    out.append(f"<rect x='{x(30):.1f}' y='{wait_top}' width='{x(90) - x(30):.1f}' height='{wait_h}' rx='8' "
               f"fill='none' stroke='{GREEN}' stroke-width='2.5' stroke-dasharray='7 5'/>")
    out.append(label((x(30) + x(90)) / 2, wait_top + wait_h / 2 + 7, "wartet 60 s", SIZE, GREEN, weight="700"))
    out.append(f"<line x1='{x(90):.1f}' y1='{wait_top}' x2='{x(90):.1f}' y2='{lane_top + lane_h - 7}' stroke='{GREEN}' stroke-width='2.5'/>")
    out.append(f"<circle cx='{x(30):.1f}' cy='{wait_top + wait_h / 2}' r='6' fill='{GREEN}'/>")
    out.append(label(x(30) - 12, wait_top + wait_h / 2 + 7, "kommt an", SIZE - 2, GREEN, "end"))
    # the axis
    out.append(f"<line x1='{X0}' y1='{axis}' x2='{x(180):.1f}' y2='{axis}' stroke='{LINE}' stroke-width='2'/>")
    for second in range(0, 181, 30):
        out.append(f"<line x1='{x(second):.1f}' y1='{axis - 6}' x2='{x(second):.1f}' y2='{axis + 6}' stroke='{GREY}' stroke-width='1.5'/>")
        out.append(label(x(second), axis + 30, f"{second} s" if second == 180 else second, SIZE - 2, GREY,
                         "end" if second == 180 else "start" if second == 0 else "middle"))
    out.append("</svg>")
    (OUT / "kasse.svg").write_text("\n".join(out) + "\n")
    print(OUT / "kasse.svg")

kasse()
