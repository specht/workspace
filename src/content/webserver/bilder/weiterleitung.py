"""A vote and its redirect (»Webserver«, »Was ein Formular schickt«), as a
sequence of messages between the browser and the server: POST /vote with
the body, 303 See Other with Location, GET /results, 200 OK with the page.
Why the detour is good is a task after it; the drawing only shows the way.

    python3 src/content/webserver/bilder/weiterleitung.py
    ruby src/scripts/render-svg.rb src/content/webserver/weiterleitung.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
from diagram_labels import code  # noqa: E402

INK, GREY, BLUE, AMBER, WHITE, PALE = '#1f2937', '#6b7280', '#2b5fb4', '#d97706', '#ffffff', '#eef3fb'
FONT = 'IBM Plex Sans, DejaVu Sans, sans-serif'
W, H = 640, 400
BROWSER, SERVER = 120, 520

s = []


def text(x, y, t, size=17, color=INK, anchor='middle', weight=400):
    s.append(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" fill="{color}" text-anchor="{anchor}" font-weight="{weight}">{t}</text>')


for x, name in ((BROWSER, 'Browser'), (SERVER, 'Server')):
    s.append(f'<rect x="{x - 64}" y="20" width="128" height="40" rx="8" fill="{PALE}" stroke="{INK}" stroke-width="1.8"/>')
    text(x, 46, name, 18, INK, weight=600)
    s.append(f'<line x1="{x}" y1="60" x2="{x}" y2="{H - 16}" stroke="{GREY}" stroke-width="1.4" stroke-dasharray="5 5"/>')

# the messages: from, to, first line, second line, colour
messages = [(BROWSER, SERVER, 'POST /vote', 'choice=curry', BLUE),
            (SERVER, BROWSER, '303 See Other', 'Location: /results', AMBER),
            (BROWSER, SERVER, 'GET /results', '', BLUE),
            (SERVER, BROWSER, '200 OK', 'die Ergebnisseite', AMBER)]
y = 108
for a, b, first, second, color in messages:
    d = 1 if b > a else -1
    s.append(f'<line x1="{a}" y1="{y}" x2="{b - d * 10}" y2="{y}" stroke="{color}" stroke-width="2.4"/>')
    s.append(f'<path d="M{b},{y} L{b - d * 13},{y - 6} L{b - d * 13},{y + 6} z" fill="{color}"/>')
    s.append(code(first, (a + b) / 2, y - 12, size=16, anchor='middle', background=False))
    if second:
        if second.startswith('die'):
            text((a + b) / 2, y + 26, second, 15, GREY)
        else:
            s.append(code(second, (a + b) / 2, y + 26, size=15, anchor='middle', background=False))
    y += 78

out = os.path.join(HERE, '..', 'weiterleitung.svg')
with open(out, 'w') as f:
    f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" font-family="{FONT}">\n'
            '  <!-- Eine Stimme und die Weiterleitung, als Folge von Nachrichten.\n'
            '       Gemacht von bilder/weiterleitung.py; die dunkle Fassung weiterleitung-dark.svg\n'
            '       und die Schriften: src/scripts/render-svg.rb -->\n'
            f'  <rect width="{W}" height="{H}" fill="{WHITE}"/>\n  ' + '\n  '.join(s) + '\n</svg>\n')
print(out)
