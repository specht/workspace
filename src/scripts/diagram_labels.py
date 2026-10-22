"""Labels for hand-drawn SVG diagrams (render with src/scripts/render-svg.rb).

Two kinds, so that a diagram reads like the page around it:

- math(tex, x, y): a formula typeset by LaTeX, as on the page (KaTeX uses the
  same Computer Modern shapes): math(r'\\cos\\alpha', 120, 40). The glyphs
  become paths, so the picture needs no font; y is the baseline.
- code(text, x, y): code as on the page, `dot(a, b)`: 0xProto in the page's
  code colour on its pale pink background.

Both return an SVG fragment (a string) to put into the diagram. Colours are
the diagram palette's (render-svg.rb maps them for dark mode). LaTeX runs
once per formula; the results are cached in ~/.cache/diagram-labels.

    import sys; sys.path.insert(0, 'src/scripts')
    from diagram_labels import math, code
"""

import hashlib
import os
import re
import subprocess
import tempfile

TEXT = '#1f2937'
CODE_TEXT = '#e5185d'        # default.css, code
CODE_BACKGROUND = '#f384af'  # rgba(243, 132, 175, 0.2) in default.css
# quoted: a family name that starts with a digit is no CSS identifier, and
# unquoted the whole list is ignored (the text falls back to the sans font)
CODE_FONT = "'0xProto Nerd Font Mono', 'DejaVu Sans Mono', monospace"
CACHE = os.path.expanduser('~/.cache/diagram-labels')

_count = 0


def _typeset(tex):
    """The SVG of a formula from latex + dvisvgm, baseline at y = 0, in pt."""
    key = hashlib.sha256(tex.encode()).hexdigest()[:20]
    path = os.path.join(CACHE, key + '.svg')
    if not os.path.exists(path):
        os.makedirs(CACHE, exist_ok=True)
        source = ('\\documentclass{article}\n'
                  '\\usepackage[active,tightpage]{preview}\n'
                  '\\usepackage{amsmath,amssymb}\n'
                  '\\begin{document}\n'
                  '\\begin{preview}$' + tex + '$\\end{preview}\n'
                  '\\end{document}\n')
        with tempfile.TemporaryDirectory() as work:
            with open(os.path.join(work, 'label.tex'), 'w') as f:
                f.write(source)
            result = subprocess.run(['latex', '-interaction=nonstopmode', '-halt-on-error', 'label.tex'],
                                    cwd=work, capture_output=True, text=True)
            if result.returncode != 0:
                raise ValueError('LaTeX failed for %r:\n%s' % (tex, result.stdout[-800:]))
            subprocess.run(['dvisvgm', '--no-fonts', '--bbox=preview', 'label.dvi', '-o', path],
                           cwd=work, capture_output=True, check=True)
    with open(path) as f:
        return f.read()


def math(tex, x, y, size=16, color=TEXT, anchor='start'):
    """A formula with its baseline at (x, y); size like font-size in px.
    anchor: 'start', 'middle' or 'end', as text-anchor."""
    global _count
    _count += 1
    svg = _typeset(tex)
    view = [float(v) for v in re.search(r"viewBox='([^']*)'", svg).group(1).split()]
    left, _, width, _ = view
    # LaTeX sets 10 pt type: scale it to the size of the diagram's text
    scale = size / 10.0
    shift = {'start': 0, 'middle': width / 2, 'end': width}[anchor]
    defs = re.search(r'<defs>(.*?)</defs>', svg, re.S)
    body = svg.split('</defs>', 1)[1] if defs else re.split(r'<svg[^>]*>', svg, maxsplit=1)[1]
    body = re.sub(r'</svg>\s*$', '', body)
    # unique ids, so several formulas fit into one picture
    prefix = 'm%d-' % _count
    inner = ('<defs>%s</defs>' % defs.group(1) if defs else '') + body
    inner = re.sub(r"id='([^']*)'", lambda m: "id='%s%s'" % (prefix, m.group(1)), inner)
    inner = re.sub(r"xlink:href='#([^']*)'", lambda m: "xlink:href='#%s%s'" % (prefix, m.group(1)), inner)
    return ("<g transform='translate(%.2f %.2f) scale(%.4f) translate(%.4f 0)' fill='%s' stroke='none'>%s</g>"
            % (x, y, scale, -left - shift, color, inner))


def code(text, x, y, size=15, anchor='start', background=True):
    """Code as on the page: monospace in the code colour, on a pale pink
    rounded box. (x, y) is the baseline of the text."""
    escaped = text.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    # 0xProto is about 0.6 em wide per character
    width = 0.6 * size * len(text)
    start = {'start': x, 'middle': x - width / 2, 'end': x - width}[anchor]
    parts = []
    if background:
        pad = 0.3 * size
        parts.append("<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='%.1f' fill='%s' fill-opacity='0.2'/>"
                     % (start - pad, y - 0.82 * size, width + 2 * pad, 1.15 * size, 0.3 * size, CODE_BACKGROUND))
    parts.append("<text x='%.1f' y='%.1f' font-size='%d' fill='%s' font-family=\"%s\">%s</text>"
                 % (start, y, size, CODE_TEXT, CODE_FONT, escaped))
    return ''.join(parts)


def right_angle(cx, cy, ux, uy, vx, vy, radius=11, color=TEXT):
    """The German mark for a right angle at the corner (cx, cy) between the
    unit directions u and v: a quarter arc with a dot in it."""
    ax, ay = cx + ux * radius, cy + uy * radius
    bx, by = cx + vx * radius, cy + vy * radius
    # sweep towards v the short way round
    sweep = 1 if ux * vy - uy * vx > 0 else 0
    dot_x = cx + (ux + vx) * radius * 0.42
    dot_y = cy + (uy + vy) * radius * 0.42
    return ("<path d='M %.2f %.2f A %.2f %.2f 0 0 %d %.2f %.2f' fill='none' stroke='%s' stroke-width='1.2'/>"
            "<circle cx='%.2f' cy='%.2f' r='1.6' fill='%s'/>"
            % (ax, ay, radius, radius, sweep, bx, by, color, dot_x, dot_y, color))
