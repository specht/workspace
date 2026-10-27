"""The card picture of »Turingmaschinen«, as SVG beside the page.

    python3 src/content/turingmaschine/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/turingmaschine/teaser.svg 1280

- teaser: the tape of stufen/plus_one.py with 1011 on it, going on without
  end to both sides, the head in state q1 on the last 1, and the transition
  it is about to take above it.

The machines themselves are drawn on the page by the widget of »Endliche
Automaten« (automaton_layout.js); tapes and heads come from
src/scripts/automaton_pictures.py, in the same style.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')


def teaser():
    w, h = 1280, 720
    size, y0, at = 120, 420, 5
    symbols = ['_', '_', '1', '0', '1', '1', '_', '_', '_']
    x0 = (w - len(symbols) * size) / 2
    parts = [ap.tape(x0, y0, size, symbols, current=at, ends=True, width=3)]
    hx = x0 + at * size + size / 2
    parts.append(ap.tape_head(hx, y0 - 16, 130, 'q1'))
    parts.append(ap.text(hx, y0 - 140, '1 / 0, L', 44))
    render = 'src/scripts/render-svg.rb src/content/turingmaschine/teaser.svg 1280'
    with open(os.path.join(OUT, 'teaser.svg'), 'w') as f:
        f.write(ap.svg_head(w, h, 'Die Karte: das Band mit 1011, der Kopf in q1 auf der letzten 1; '
                            'gemacht von bilder/diagramme.py, als Bild:', render) + ''.join(parts) + '</svg>\n')


if __name__ == '__main__':
    teaser()
