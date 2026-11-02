#!/usr/bin/env python3
"""The pictures of »Prüfziffern« drawn in Python, in the style of
dns/dns-tree.svg (IBM Plex Sans, the diagram palette of render-svg.rb):

- gerade: »Reparieren mit einer Geraden«, the four values that arrived,
  2, 6, 11 and 14, over their positions 1 to 4. 2, 6 and 14 lie on a straight
  line, the 11 above it; a dashed arrow leads from the 11 to the 10 the line
  gives at its position.

    python3 src/content/check-digits/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/check-digits/gerade.svg 1040
"""

from pathlib import Path

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
TEXT, BLUE, GREY, LINE, RED = "#1f2937", "#2b5fb4", "#6b7280", "#d1d5db", "#dc2626"
OUT = Path(__file__).resolve().parent.parent

W, H = 520, 470
X0, Y0 = 70, 420     # where position 0 and value 0 meet
DX, DY = 90, 24      # pixels per position and per unit of value


def px(place, value):
    return X0 + DX * place, Y0 - DY * value


def label(x, y, text, size=16, colour=TEXT, anchor="middle", weight="400"):
    return (f"<text x='{x:.1f}' y='{y:.1f}' text-anchor='{anchor}' font-family=\"{SANS}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def gerade():
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
           "  <!-- Vier Werte über ihren Stellen, drei auf einer Geraden; gemacht von bilder/diagramme.py,\n"
           "       als Bild: ruby src/scripts/render-svg.rb src/content/check-digits/gerade.svg 1040 -->",
           f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
    # grid and value ticks
    for value in range(0, 17, 4):
        x, y = px(0, value)
        if value:
            out.append(f"<line x1='{X0}' y1='{y}' x2='{X0 + DX * 4.6:.0f}' y2='{y}' stroke='{LINE}' stroke-width='1'/>")
        out.append(label(x - 12, y + 5, value, 15, GREY, "end"))
    for place in range(1, 5):
        x, y = px(place, 0)
        out.append(f"<line x1='{x}' y1='{Y0}' x2='{x}' y2='{Y0 + 6}' stroke='{GREY}' stroke-width='1.5'/>")
        out.append(label(x, Y0 + 26, place, 15, GREY))
    # axes with arrowheads
    right = X0 + DX * 4.6
    out.append(f"<line x1='{X0}' y1='{Y0}' x2='{right - 10:.0f}' y2='{Y0}' stroke='{GREY}' stroke-width='1.5'/>")
    out.append(f"<polygon points='{right:.0f},{Y0} {right - 12:.0f},{Y0 - 5} {right - 12:.0f},{Y0 + 5}' fill='{GREY}'/>")
    out.append(label(right, Y0 + 26, "Stelle", 15, GREY, "end"))
    top = 18
    out.append(f"<line x1='{X0}' y1='{Y0}' x2='{X0}' y2='{top + 10}' stroke='{GREY}' stroke-width='1.5'/>")
    out.append(f"<polygon points='{X0},{top} {X0 - 5},{top + 12} {X0 + 5},{top + 12}' fill='{GREY}'/>")
    out.append(label(X0 + 12, top + 10, "Wert", 15, GREY, "start"))
    # the straight line through 2, 6 and 14: value = 4 · place − 2
    xa, ya = px(0.6, 4 * 0.6 - 2)
    xb, yb = px(4.4, 4 * 4.4 - 2)
    out.append(f"<line x1='{xa:.1f}' y1='{ya:.1f}' x2='{xb:.1f}' y2='{yb:.1f}' stroke='{BLUE}' stroke-width='2.5'/>")
    # the 11 that arrived and the 10 the line gives, the arrow between them on the left
    xr, yr = px(3, 11)
    xh, yh = px(3, 10)
    out.append(f"<path d='M {xr - 9:.1f} {yr:.1f} C {xr - 46:.1f} {yr:.1f} {xh - 46:.1f} {yh:.1f} {xh - 21:.1f} {yh:.1f}' "
               f"fill='none' stroke='{RED}' stroke-width='2' stroke-dasharray='5 4'/>")
    out.append(f"<polygon points='{xh - 9:.1f},{yh:.1f} {xh - 21:.1f},{yh - 5:.1f} {xh - 21:.1f},{yh + 5:.1f}' fill='{RED}'/>")
    out.append(f"<circle cx='{xh:.1f}' cy='{yh:.1f}' r='7' fill='#ffffff' stroke='{BLUE}' stroke-width='2.5'/>")
    out.append(label(xh + 14, yh + 22, "10", 17, BLUE, "start", "600"))
    out.append(f"<circle cx='{xr:.1f}' cy='{yr:.1f}' r='7' fill='{RED}'/>")
    out.append(label(xr - 14, yr - 12, "11", 17, RED, "end", "600"))
    # the three values that fit
    for place, value in [(1, 2), (2, 6), (4, 14)]:
        x, y = px(place, value)
        out.append(f"<circle cx='{x:.1f}' cy='{y:.1f}' r='7' fill='{BLUE}'/>")
        out.append(label(x - 14, y - 10, value, 17, BLUE, "end", "600"))
    out.append("</svg>")
    (OUT / "gerade.svg").write_text("\n".join(out) + "\n")
    print(OUT / "gerade.svg")


gerade()
