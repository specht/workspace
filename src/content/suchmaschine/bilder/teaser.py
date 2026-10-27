#!/usr/bin/env python3
"""The card picture of »Eine Suchmaschine«: what the students' search engine
shows, a search field with »katze« and the hits ranked by incoming links
(rank_by_links on the crawled Tierlexikon: katze.html 3, hund.html 2, the ad
katzenfutter-billig.html 1), each with its title, address and the start of
its text; the ad last, its text nothing but the word. A page, not a graph,
so that it looks unlike the card of »Was eine Webseite über dich erfährt«.
Writes ../teaser.svg; render it with
src/scripts/render-svg.rb src/content/suchmaschine/teaser.svg 1280."""

from pathlib import Path

W, H = 1280, 720
SANS = "'IBM Plex Sans', system-ui, sans-serif"
MONO = "'IBM Plex Mono', ui-monospace, monospace"
TEXT, BLUE, BOX, GREY, LINE, AMBER = "#1f2937", "#2b5fb4", "#eef3fb", "#6b7280", "#d1d5db", "#d97706"

# title, address, the start of its text, incoming links
hits = [
    ("Hauskatze", "/katze.html", "Die Katze lebt seit fast zehntausend Jahren mit Menschen …", 3),
    ("Hund", "/hund.html", "Anders als die Katze lässt sich ein Hund gern erziehen …", 2),
    ("Katze Katze Katzenfutter", "/katzenfutter-billig.html", "Katze katze katze. Katzenfutter für jede Katze. Katze …", 1),
]


def text(x, y, content, size, colour=TEXT, anchor="start", weight="400", family=SANS):
    return f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{family}\" font-size='{size}' font-weight='{weight}' fill='{colour}'>{content}</text>"


def magnifier(cx, cy, colour):
    return (f"<circle cx='{cx}' cy='{cy}' r='15' fill='none' stroke='{colour}' stroke-width='5'/>"
            f"<line x1='{cx + 11}' y1='{cy + 11}' x2='{cx + 25}' y2='{cy + 25}' stroke='{colour}' stroke-width='6' stroke-linecap='round'/>")


def link_badge(x, y, count, colour):
    """The incoming links: a small arrow into a box, and the count."""
    return (f"<rect x='{x}' y='{y - 30}' width='172' height='44' rx='22' fill='#ffffff' stroke='{colour}' stroke-width='3'/>"
            f"<line x1='{x + 20}' y1='{y - 8}' x2='{x + 42}' y2='{y - 8}' stroke='{colour}' stroke-width='4' stroke-linecap='round'/>"
            f"<polygon points='{x + 52},{y - 8} {x + 40},{y - 16} {x + 40},{y}' fill='{colour}'/>"
            + text(x + 62, y + 2, f"{count} {'Link' if count == 1 else 'Links'}", 26, colour, weight="600"))


out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {W} {H}' width='{W}' height='{H}'>",
       f"<rect width='{W}' height='{H}' fill='#ffffff'/>"]
# the search field
out.append(f"<rect x='90' y='60' width='1100' height='92' rx='46' fill='{BOX}' stroke='{BLUE}' stroke-width='3'/>")
out.append(magnifier(150, 102, BLUE))
out.append(text(200, 122, "katze", 44, TEXT))
out.append(text(1150, 122, "3 Treffer", 28, GREY, anchor="end"))
# the hits, best first
for i, (title, address, start, links) in enumerate(hits):
    top = 220 + i * 165
    ad = links == 1
    colour = AMBER if ad else BLUE
    out.append(text(110, top + 40, title, 40, colour, weight="600"))
    out.append(text(110, top + 82, address, 26, GREY, family=MONO))
    out.append(text(110, top + 122, start, 28, TEXT))
    out.append(link_badge(998, top + 46, links, colour))
    if i < len(hits) - 1:
        out.append(f"<line x1='110' y1='{top + 148}' x2='1170' y2='{top + 148}' stroke='{LINE}' stroke-width='2'/>")
out.append("</svg>")
Path(__file__).resolve().parent.parent.joinpath("teaser.svg").write_text("\n".join(out) + "\n")
