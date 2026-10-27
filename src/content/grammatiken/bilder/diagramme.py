"""The pictures of »Grammatiken« drawn in Python, as SVG beside the page.

    python3 src/content/grammatiken/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/grammatiken/NAME.svg WIDTH

- aufrufe: the word ((())) read up to its first ), the index 3 marked, and
  beside it the calls of brackets that are open at that moment, the newest
  on top, drawn like the stack of »Kellerautomaten«.

The syntax diagrams and trees are drawn on the page by syntax_diagram.js, the
card picture by bilder/teaser.cjs with the same module; tapes and stacks come
from src/scripts/automaton_pictures.py.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/grammatiken/{name}.svg {scale * w}'
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render) + ''.join(parts) + '</svg>\n')


def aufrufe():
    w, h = 560, 260
    size = 44
    word = '((()))'
    x0, y = 30, 150
    parts = [ap.tape(x0, y, size, list(word), current=3, done=3)]
    # the index under the field being read
    parts.append(ap.text(x0 + 3.5 * size, y + size + 26, 'i = 3', 17, ap.ACCENT))
    for k in range(len(word)):
        parts.append(ap.text(x0 + (k + 0.5) * size, y - 10, k, 12, ap.GREY))
    # the open calls, the newest on top
    fx, fw, fh = 340, 190, 36
    base = 228
    labels = ['accepts(word)'] + [f'brackets(word, {k})' for k in range(4)]
    parts.append(ap.frames(fx, base, fw, fh, labels, size=15))
    top = base - len(labels) * fh
    parts.append(ap.curve((x0 + 3.5 * size, y - 30), (x0 + 3.5 * size + 20, top - 10),
                          (fx - 70, top + fh / 2), (fx - 12, top + fh / 2)))
    save('aufrufe', w, h, 'Die offenen Aufrufe von brackets bei ((())), am ersten )', parts)


aufrufe()
