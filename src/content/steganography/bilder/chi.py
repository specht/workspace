"""The chi-square test of »Steganographie« as a picture: the counts of the
values 200 to 211 in pairs (200 and 201, 202 and 203, …), on the left in a
photo, where the two values of a pair are rarely equal, on the right after
hiding random bits in the lowest bit, where they are nearly equal. Made-up
counts, of the size the page's example uses (300 times 208, 250 times 209).

    python3 src/content/steganography/bilder/chi.py
    ruby src/scripts/render-svg.rb src/content/steganography/chi.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import diagram_labels  # noqa: E402,F401  (runs the script in docker/drawings)

INK, GREY, LINE, BLUE, AMBER, WHITE = '#1f2937', '#6b7280', '#d1d5db', '#2b5fb4', '#d97706', '#ffffff'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
W, H = 680, 300
PHOTO = [310, 240, 290, 250, 260, 300, 330, 270, 300, 250, 280, 220]
HIDDEN = [278, 272, 268, 272, 283, 277, 296, 304, 272, 278, 253, 247]
TOP, BASE = 56, 236   # the bars from BASE up; 350 is the top of the scale
SCALE = (BASE - TOP) / 350

s = []


def text(x, y, t, size=15, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


def panel(x0, title, counts):
    width = 290
    text(x0 + width / 2, 28, title, 17, INK, weight=600)
    for v in (100, 200, 300):
        y = BASE - v * SCALE
        s.append(f'<line x1="{x0}" y1="{y:.1f}" x2="{x0 + width}" y2="{y:.1f}" stroke="{LINE}" stroke-width="1"/>')
        text(x0 - 6, y + 5, v, 12, GREY, 'end')
    s.append(f'<line x1="{x0}" y1="{BASE}" x2="{x0 + width}" y2="{BASE}" stroke="{GREY}" stroke-width="1.4"/>')
    pair_w = width / 6
    for k in range(6):
        for j in range(2):
            c = counts[2 * k + j]
            bx = x0 + k * pair_w + 7 + j * 19
            h = c * SCALE
            # a 2px gap of the surface between the two bars of a pair
            s.append(f'<rect x="{bx:.1f}" y="{BASE - h:.1f}" width="17" height="{h:.1f}" rx="2" fill="{BLUE if j == 0 else AMBER}"/>')
        text(x0 + k * pair_w + 7 + 18, BASE + 20, f'{200 + 2 * k}/{201 + 2 * k}', 11, GREY)


panel(60, 'ein Foto', PHOTO)
panel(380, 'mit Botschaft im untersten Bit', HIDDEN)
# the legend: which bar is which value of a pair
y = 284
s.append(f'<rect x="200" y="{y - 11}" width="13" height="13" rx="2" fill="{BLUE}"/>')
text(219, y, 'der gerade Wert eines Paares', 14, INK, 'start')
s.append(f'<rect x="420" y="{y - 11}" width="13" height="13" rx="2" fill="{AMBER}"/>')
text(439, y, 'der ungerade', 14, INK, 'start')

out = os.path.join(HERE, '..', 'chi.svg')
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Die Paare der Werte 200 bis 211, im Foto und mit Botschaft.\n'
            '       Gemacht von bilder/chi.py; die dunkle Fassung chi-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print(out)
