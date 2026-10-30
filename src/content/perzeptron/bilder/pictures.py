#!/usr/bin/env python3
"""The card picture of »Ein Neuron lernt«: teaser.svg, the 274 Adélie and
Gentoo penguins of penguins.csv and the line the perceptron learns, weights
29.1 and -74.0, bias -1, from the page's train. Render it with
src/scripts/render-svg.rb src/content/perzeptron/teaser.svg 1280. The
drawings of penguins on the page (pinguine.webp, schnabel.webp) are Allison
Horst's from palmerpenguins (CC0), put on white and the English note of the
bill's drawing painted over.

neuron.svg: the neuron of »Ein künstliches Neuron« with the page's example,
a bill 50 mm long and 15 mm high, weights 2 and -5, bias 10: the sum 35 is
greater than 0, the neuron says 1. Render it with
src/scripts/render-svg.rb src/content/perzeptron/neuron.svg 1440."""

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



def arrow(a, b, colour=GREY, width=2.5):
    """A straight arrow from a to b, its tip on b."""
    import math
    angle = math.atan2(b[1] - a[1], b[0] - a[0])
    length, spread = 13, 6.5
    base = (b[0] - length * math.cos(angle), b[1] - length * math.sin(angle))
    left = (base[0] + spread * math.sin(angle), base[1] - spread * math.cos(angle))
    right = (base[0] - spread * math.sin(angle), base[1] + spread * math.cos(angle))
    return (f"<line x1='{a[0]:.1f}' y1='{a[1]:.1f}' x2='{base[0]:.1f}' y2='{base[1]:.1f}' stroke='{colour}' stroke-width='{width}'/>"
            f"<polygon points='{b[0]:.1f},{b[1]:.1f} {left[0]:.1f},{left[1]:.1f} {right[0]:.1f},{right[1]:.1f}' fill='{colour}'/>")


def label(x, y, text, size=19, colour=TEXT, weight="400", anchor="middle"):
    return (f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{SANS}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def neuron():
    w, h = 720, 250
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           "  <!-- Das Neuron mit dem Beispiel der Seite; gemacht von bilder/pictures.py, als Bild:\n"
           "       ruby src/scripts/render-svg.rb src/content/perzeptron/neuron.svg 1440 -->",
           f"<rect width='{w}' height='{h}' fill='#ffffff'/>"]
    # the two inputs
    inputs = [(95, 70, "Länge", "50"), (95, 190, "Höhe", "15")]
    sum_c, r = (370, 130), 40
    weights = [("· 2", "= 100", (226, 60)), ("· (−5)", "= −75", (232, 196))]
    for (x, y, name, value), (wt, product, (lx, ly)) in zip(inputs, weights):
        out.append(f"<rect x='{x - 60}' y='{y - 30}' width='120' height='60' rx='10' fill='#eef3fb' stroke='{BLUE}' stroke-width='2'/>")
        out.append(label(x, y - 8, name, 15, GREY))
        out.append(label(x, y + 19, value + " mm", 21))
        # from the box's right edge to the sum's border, aimed at its centre
        import math
        dx, dy = sum_c[0] - (x + 60), sum_c[1] - y
        d = math.hypot(dx, dy)
        end = (sum_c[0] - dx / d * r, sum_c[1] - dy / d * r)
        out.append(arrow((x + 60, y), end))
        out.append(label(lx, ly, wt, 20, BLUE, "600"))
        out.append(label(lx, ly + 24, product, 16, GREY))
    # the bias from above
    out.append(label(sum_c[0], 30, "Bias + 10", 19, BLUE, "600"))
    out.append(arrow((sum_c[0], 40), (sum_c[0], sum_c[1] - r)))
    # the sum
    out.append(f"<circle cx='{sum_c[0]}' cy='{sum_c[1]}' r='{r}' fill='#ffffff' stroke='{TEXT}' stroke-width='2'/>")
    out.append(label(sum_c[0], sum_c[1] - 4, "Summe", 13, GREY))
    out.append(label(sum_c[0], sum_c[1] + 20, "35", 22))
    # the threshold and the answer
    step = (530, 130)
    out.append(arrow((sum_c[0] + r, sum_c[1]), (step[0] - 50, step[1])))
    out.append(f"<rect x='{step[0] - 50}' y='{step[1] - 28}' width='100' height='56' rx='10' fill='#ffffff' stroke='{TEXT}' stroke-width='2'/>")
    out.append(label(step[0], step[1] + 7, "&gt; 0 ?", 20))
    answer = (655, 130)
    out.append(arrow((step[0] + 50, step[1]), (answer[0] - 30, answer[1])))
    out.append(f"<circle cx='{answer[0]}' cy='{answer[1]}' r='30' fill='#eef3fb' stroke='{BLUE}' stroke-width='2.5'/>")
    out.append(label(answer[0], answer[1] + 8, "1", 24, TEXT, "600"))
    out.append(label(answer[0], answer[1] + 58, "Eselspinguin", 15, GREY))
    out.append(label(step[0] + 62, step[1] - 12, "ja", 14, GREY))
    out.append("</svg>")
    (HERE / "neuron.svg").write_text("\n".join(out) + "\n")


teaser()
neuron()
