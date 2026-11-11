"""Adaptive streaming: above, the rate the player measures over 12 seconds
(a step for each segment of 2 s); below, the three rungs of the ladder of
»Eine Leiter fürs Streaming« as rows of segments, the dashed rate each rung
needs drawn into the chart in its colour. For each segment the player takes
the largest rung that fits under the measured rate: those segments are
filled with a purple dot, on a desktop joined by a purple line. Wide for a
desktop, narrow for a phone (the labels of the rungs above their rows
instead of beside them, and no line, which would cross them).

    src/scripts/drawings python3 src/content/video-compression/bilder/ladder.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/ladder.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/ladder-wide.svg
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
import automaton_pictures as ap  # noqa: E402

OUT = os.path.join(HERE, '..')
INK, GREY, LINE, WHITE, ACCENT = '#1f2937', '#6b7280', '#d1d5db', '#ffffff', '#7c3aed'
RUNGS = [('960 × 400', 1.65, '#2b5fb4'), ('640 × 266', 0.66, '#16a34a'), ('480 × 200', 0.275, '#d97706')]
MEASURED = [2.4, 2.0, 0.9, 0.5, 1.1, 2.2]       # Mbit/s during each segment


def chosen(rate):
    return next((i for i, (_, need, _) in enumerate(RUNGS) if need <= rate), len(RUNGS) - 1)


def mbit(v):
    return f'{v:g}'.replace('.', ',')


def picture(name, wide):
    size = 15 if wide else 13
    left = 236 if wide else 46
    seg = 120 if wide else 62
    width = left + seg * len(MEASURED) + 28
    chart_top, chart_h = 28, 130
    top_rate = 2.5
    y_of = lambda r: chart_top + chart_h * (1 - r / top_rate)
    parts = [ap.text(left, 16, 'gemessene Rate in Mbit/s', size, GREY, 'start')]
    # the axis: 0 to 2,5 Mbit/s
    for r in (0, 1, 2):
        y = y_of(r)
        parts.append(f"<line x1='{left}' y1='{y:.1f}' x2='{left + seg * len(MEASURED)}' y2='{y:.1f}' stroke='{LINE}' stroke-width='1'/>")
        parts.append(ap.text(left - 8, y + size * 0.35, mbit(r), size, GREY, 'end'))
    # what each rung needs, dashed in its colour
    for _, need, colour in RUNGS:
        y = y_of(need)
        parts.append(f"<line x1='{left}' y1='{y:.1f}' x2='{left + seg * len(MEASURED)}' y2='{y:.1f}' stroke='{colour}' stroke-width='1.5' stroke-dasharray='6 4'/>")
    # the measured rate, a step per segment
    path = []
    for k, r in enumerate(MEASURED):
        x0, x1, y = left + k * seg, left + (k + 1) * seg, y_of(r)
        path.append(f"{'M' if k == 0 else 'L'} {x0} {y:.1f} L {x1} {y:.1f}")
    parts.append(f"<path d='{' '.join(path)}' fill='none' stroke='{INK}' stroke-width='2.5'/>")
    # the rungs as rows of segments
    row_top = chart_top + chart_h + 26
    row_h = 34 if wide else 30
    gap = 14 if wide else 30
    centres = []
    for i, (label, need, colour) in enumerate(RUNGS):
        y = row_top + i * (row_h + gap) + (0 if wide else 18)
        text = f'{label}, {mbit(need)} Mbit/s'
        if wide:
            parts.append(ap.text(left - 10, y + row_h / 2 + size * 0.35, text, size, colour, 'end'))
        else:
            parts.append(ap.text(left, y - 6, text, size, colour, 'start'))
        for k, r in enumerate(MEASURED):
            x = left + k * seg
            picked = chosen(r) == i
            fill = f"fill='{colour}' fill-opacity='0.28'" if picked else f"fill='{WHITE}'"
            parts.append(f"<rect x='{x + 3}' y='{y:.1f}' width='{seg - 6}' height='{row_h}' rx='3' {fill} stroke='{colour if picked else LINE}' stroke-width='{2 if picked else 1.2}'/>")
            parts.append(ap.text(x + 12, y + row_h / 2 + size * 0.35, 'I', size - 2, GREY if not picked else colour, 'start'))
            if picked:
                centres.append((k, x + seg / 2, y + row_h / 2))
    # in the order of time, not of the rows
    centres = [(cx, cy) for _, cx, cy in sorted(centres)]
    # the line between them only where the labels are beside the rows; on
    # a phone they stand between the rows, and the line would cross them
    if wide:
        parts.append(f"<path d='{' '.join(('M' if n == 0 else 'L') + f' {cx:.1f} {cy:.1f}' for n, (cx, cy) in enumerate(centres))}' fill='none' stroke='{ACCENT}' stroke-width='2.5'/>")
    for cx, cy in centres:
        parts.append(f"<circle cx='{cx:.1f}' cy='{cy:.1f}' r='4.5' fill='{ACCENT}'/>")
    bottom = row_top + len(RUNGS) * (row_h + gap) + (0 if wide else 18)
    for k in range(len(MEASURED) + 1):
        parts.append(ap.text(left + k * seg, bottom + size + 2, f'{2 * k} s', size, GREY))
    height = bottom + size + 12
    head = ap.svg_head(round(width), round(height), 'Adaptives Streaming: die gemessene Rate und die Stufe für jedes Stück; gemacht von bilder/ladder.py, hell und dunkel:',
                       f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
    with open(os.path.join(OUT, f'{name}.svg'), 'w') as f:
        f.write(head + ''.join(parts) + '</svg>\n')


picture('ladder-wide', True)
picture('ladder', False)
