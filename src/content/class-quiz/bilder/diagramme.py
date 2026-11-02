#!/usr/bin/env python3
"""The pictures of »Klassen-Quiz« drawn in Python, in the colours of the
card picture (teaser.svg) and the diagram palette of render-svg.rb:

- push-abgelehnt: why Git rejects the push. On the server the commits
  A and B and after them the other commit; in your clone A and B and after
  them your commit. Both build on B; taking yours, the server would lose
  the other one.
- nach-merge: your clone after the conflict is resolved and committed: the
  other commit and yours both after B, joined by the merge commit, which
  the server can take. (git pull alone stops at the conflict in rounds.txt.)

No names: the page has a version with Ida and one with the class
repository, where it is anyone in the class.

    python3 src/content/class-quiz/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/class-quiz/push-abgelehnt.svg 1520
    ruby src/scripts/render-svg.rb src/content/class-quiz/nach-merge.svg 1520
"""

from pathlib import Path

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
TEXT, BLUE, GREY, GREEN, AMBER = "#1f2937", "#2b5fb4", "#6b7280", "#16a34a", "#d97706"
OUT = Path(__file__).resolve().parent.parent
W = 760


def label(x, y, text, size=16, colour=TEXT, anchor="middle", weight="400", italic=False):
    style = " font-style='italic'" if italic else ""
    return (f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{SANS}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'{style}>{text}</text>")


def line(points, colour):
    """A line through the points; a bend between two rows as a soft curve."""
    d = f"M {points[0][0]} {points[0][1]}"
    for (x0, y0), (x1, y1) in zip(points, points[1:]):
        if y0 == y1:
            d += f" L {x1} {y1}"
        else:
            mid = (x0 + x1) / 2
            d += f" C {mid} {y0} {mid} {y1} {x1} {y1}"
    return f"<path d='{d}' fill='none' stroke='{colour}' stroke-width='5' stroke-linecap='round'/>"


def commit(x, y, colour, name=None, ring=False):
    if ring:
        out = f"<circle cx='{x}' cy='{y}' r='17' fill='#ffffff' stroke='{colour}' stroke-width='6'/>"
    else:
        out = f"<circle cx='{x}' cy='{y}' r='17' fill='{colour}' stroke='#ffffff' stroke-width='4'/>"
    if name:
        out += label(x, y + 6, name, 16, "#ffffff", weight="700")
    return out


def picture(name, comment, h, body):
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {h}' width='{W}' height='{h}'>",
           f"  <!-- {comment}; gemacht von bilder/diagramme.py, als Bild:\n"
           f"       ruby src/scripts/render-svg.rb src/content/class-quiz/{name}.svg 1520 -->",
           f"<rect width='{W}' height='{h}' fill='#ffffff'/>"]
    body(out)
    out.append("</svg>")
    (OUT / f"{name}.svg").write_text("\n".join(out) + "\n")
    print(OUT / f"{name}.svg")


A, B, NEXT = 230, 330, 460


def rejected(out):
    for y, place, colour, what in [(70, "Server", GREEN, "der andere Commit"), (200, "Dein Klon", AMBER, "dein Commit")]:
        out.append(label(40, y + 6, place, 18, TEXT, "start", "600"))
        out.append(line([(A, y), (B, y)], BLUE))
        out.append(line([(B, y), (NEXT, y)], colour))
        out.append(commit(A, y, BLUE, "A"))
        out.append(commit(B, y, BLUE, "B"))
        out.append(commit(NEXT, y, colour))
        out.append(label(NEXT + 32, y + 6, what, 16, colour, "start", "600"))
    out.append(label(A, 268, "Beide bauen auf B auf. Nähme der Server deinen Commit an,", 15, GREY, "start", italic=True))
    out.append(label(A, 290, "fehlte danach der andere.", 15, GREY, "start", italic=True))


def after_pull(out):
    y, up, down, merge = 140, 70, 210, 590
    out.append(label(40, y + 6, "Dein Klon", 18, TEXT, "start", "600"))
    out.append(label(40, y + 30, "nach dem Merge", 15, GREY, "start"))
    out.append(line([(A, y), (B, y)], BLUE))
    out.append(line([(B, y), (NEXT, up), (merge, y)], GREEN))
    out.append(line([(B, y), (NEXT, down), (merge, y)], AMBER))
    out.append(commit(A, y, BLUE, "A"))
    out.append(commit(B, y, BLUE, "B"))
    out.append(commit(NEXT, up, GREEN))
    out.append(label(NEXT, up - 30, "der andere Commit", 16, GREEN, weight="600"))
    out.append(commit(NEXT, down, AMBER))
    out.append(label(NEXT, down + 46, "dein Commit", 16, AMBER, weight="600"))
    out.append(commit(merge, y, BLUE, ring=True))
    out.append(label(merge + 30, y + 6, "Merge", 16, BLUE, "start", "600"))


picture("push-abgelehnt", "Server und Klon nach demselben B mit verschiedenen Commits", 310, rejected)
picture("nach-merge", "Nach dem Merge: beide Commits, zusammengeführt", 280, after_pull)
