#!/usr/bin/env python3
"""The pictures of »DNS« drawn in Python, in the style of dns-tree.svg
(IBM Plex Sans, the diagram palette of render-svg.rb):

- kaminsky: the race of »Wem kann man glauben?« as a sequence, time going
  down: the DNS server asks the responsible name server with a number, the
  attacker answers with guessed numbers, one fits and is believed, the real
  answer comes too late. The false address is from the range reserved for
  documentation (203.0.113.0/24).

    python3 src/content/dns/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/dns/kaminsky.svg 1640
"""

from pathlib import Path

SANS = "'IBM Plex Sans', 'DejaVu Sans', sans-serif"
TEXT, BLUE, GREY, LINE, RED, BOX = "#1f2937", "#2b5fb4", "#6b7280", "#d1d5db", "#dc2626", "#eef3fb"
OUT = Path(__file__).resolve().parent.parent


def label(x, y, text, size=16, colour=TEXT, anchor="middle", weight="400"):
    return (f"<text x='{x}' y='{y}' text-anchor='{anchor}' font-family=\"{SANS}\" font-size='{size}' "
            f"font-weight='{weight}' fill='{colour}'>{text}</text>")


def message(x0, x1, y, colour, dashed=False, width=2):
    """A horizontal arrow from x0 to x1 at y, its tip on the lifeline."""
    d = 1 if x1 > x0 else -1
    base = x1 - d * 12
    dash = " stroke-dasharray='8 6'" if dashed else ""
    return (f"<line x1='{x0}' y1='{y}' x2='{base}' y2='{y}' stroke='{colour}' stroke-width='{width}'{dash}/>"
            f"<polygon points='{x1},{y} {base},{y - 6} {base},{y + 6}' fill='{colour}'/>")


def kaminsky():
    w, h = 820, 380
    server, attacker, ns = 120, 390, 610
    out = [f"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 {w} {h}' width='{w}' height='{h}'>",
           "  <!-- Das Wettrennen der Antworten; gemacht von bilder/diagramme.py, als Bild:\n"
           "       ruby src/scripts/render-svg.rb src/content/dns/kaminsky.svg 1640 -->",
           f"<rect width='{w}' height='{h}' fill='#ffffff'/>"]
    # the three lifelines, each under its name
    for x, name, colour in [(server, "DNS-Server", BLUE), (attacker, "Angreifer", RED), (ns, "Nameserver von hackschule.de", BLUE)]:
        width = 8.4 * len(name) + 30
        out.append(f"<rect x='{x - width / 2}' y='16' width='{width}' height='38' rx='8' fill='{BOX}' stroke='{colour}' stroke-width='2'/>")
        out.append(label(x, 41, name, 16))
        out.append(f"<line x1='{x}' y1='54' x2='{x}' y2='{h - 16}' stroke='{LINE}' stroke-width='2'/>")
    out.append(label(w - 16, h - 22, "Zeit", 14, GREY, "end"))
    out.append(f"<line x1='{w - 30}' y1='80' x2='{w - 30}' y2='{h - 50}' stroke='{GREY}' stroke-width='1.5'/>")
    out.append(f"<polygon points='{w - 30},{h - 40} {w - 35},{h - 52} {w - 25},{h - 52}' fill='{GREY}'/>")
    # the question, with its number
    out.append(message(server, ns, 100, BLUE))
    out.append(label((server + ns) / 2 + 60, 92, "Wo ist hackschule.de? Nummer 4711", 15, BLUE))
    # the attacker's answers, guessed numbers
    for y, number, fits in [(160, 4709, False), (200, 4710, False), (240, 4711, True)]:
        colour = RED if fits else GREY
        out.append(message(attacker, server, y, colour, width=2.5 if fits else 1.6))
        out.append(label((server + attacker) / 2, y - 8, f"Nr. {number}: 203.0.113.66", 15, colour, weight="600" if fits else "400"))
        out.append(label(server - 12, y + 5, "geglaubt" if fits else "verworfen", 13, colour, "end"))
    # the real answer, too late
    out.append(message(ns, server, 310, GREY, dashed=True))
    out.append(label((server + ns) / 2 + 60, 302, "Nr. 4711: die echte Adresse", 15, GREY))
    out.append(label(server - 12, 315, "zu spät", 13, GREY, "end"))
    out.append("</svg>")
    (OUT / "kaminsky.svg").write_text("\n".join(out) + "\n")


kaminsky()
