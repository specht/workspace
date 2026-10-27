"""The pictures of »Kellerautomaten«, as SVG beside the page.

    python3 src/content/kellerautomat/bilder/diagramme.py
    ruby src/scripts/render-svg.rb src/content/kellerautomat/NAME.svg WIDTH

- teaser: the word ([()]) being read, the head in q0 on its ), and the stack
  beside it with ( on top, the bracket the ) closes; the arrow between them
  runs above the rest of the word.
- stack-schritte: the stack after each symbol of (()()), as the trace of
  stufen/pda.py prints it.

The automaton itself is drawn on the page by the widget of »Endliche
Automaten« (automaton_layout.js); tapes, stacks and heads come from
src/scripts/automaton_pictures.py, in the same style.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')


def save(name, w, h, comment, parts, scale=2):
    render = f'src/scripts/render-svg.rb src/content/kellerautomat/{name}.svg {scale * w}'
    with open(os.path.join(OUT, name + '.svg'), 'w') as f:
        f.write(ap.svg_head(w, h, comment + '; gemacht von bilder/diagramme.py, als Bild:', render) + ''.join(parts) + '</svg>\n')


def teaser():
    w, h = 1280, 720
    size, x0, y0, reading = 120, 70, 420, 3
    parts = [ap.tape(x0, y0, size, list('([()])'), current=reading, done=reading, width=3)]
    hx = x0 + reading * size + size / 2
    parts.append(ap.tape_head(hx, y0 - 16, 130, 'q0'))
    sx, base, ssize = 1000, 640, 112
    parts.append(ap.stack(sx, base, ssize, ['#', '(', '[', '('], width=3))
    top_y = base - 4 * ssize
    # from the head to the ( on top that the ) closes, above the rest of the word
    p0 = (hx + 56, y0 - 70)
    p3 = (sx - 18, top_y + ssize / 2)
    parts.append(ap.curve(p0, (p0[0] + 150, p0[1] - 30), (p3[0] - 220, p3[1]), p3, ap.GREY, 5, 24, 12))
    save('teaser', w, h, 'Die Karte: ein Wort aus Klammern wird gelesen, der Stack daneben', parts, 1)


def stack_steps():
    """A column per step, the symbol read above it, the bottom # at the same
    height everywhere."""
    word = '(()())'
    stacks = [['#']]
    for symbol in word:
        s = list(stacks[-1])
        if symbol == '(':
            s.append('(')
        else:
            s.pop()
        stacks.append(s)
    size, gap = 34, 22
    w = 40 + len(stacks) * (size + gap) - gap + 40
    h, base = 230, 200
    parts = []
    for k, s in enumerate(stacks):
        x = 40 + k * (size + gap)
        if k > 0:
            parts.append(ap.text(x + size / 2, 36, word[k - 1], 20))
        parts.append(ap.stack(x, base, size, s, walls=False))
        if k < len(stacks) - 1:
            parts.append(f"<line x1='{x + size + gap / 2}' y1='56' x2='{x + size + gap / 2}' y2='{base}' "
                         f"stroke='#d1d5db' stroke-width='1.5'/>")
    save('stack-schritte', w, h, 'Der Stack nach jedem Zeichen von (()()), wie die Ausgabe von pda.py', parts)


if __name__ == '__main__':
    teaser()
    stack_steps()
