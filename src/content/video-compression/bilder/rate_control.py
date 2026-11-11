"""Fixed quality against fixed rate (»Feste Qualität, feste Rate«): the
glance and the camera move up the rock face, each encoded as quality.py
does, with -crf 28 and with -b:v 400k, and for each the bytes so far,
frame by frame over the four seconds (the packets, without the few bytes
of the MP4 around them). The slope of a line is its rate.
Left fixed quality: the camera move climbs far steeper than the glance.
Right fixed rate: both alike, under the dashed line of 400 kbit/s.

Two steps. The sizes, with ffmpeg and ffprobe in the Workspace image,
into rate-control.json beside this script:

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/bilder/rate_control.py compute

then the drawing:

    src/scripts/drawings python3 src/content/video-compression/bilder/rate_control.py
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/rate-control.svg
    src/scripts/drawings ruby src/scripts/render-svg.rb src/content/video-compression/rate-control-wide.svg
"""

import json
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, 'rate-control.json')
CLIPS = os.path.join(HERE, '..', '..', '..', 'static', 'video')
WAYS = {'crf28': ['-crf', '28'], '400k': ['-b:v', '400k']}


def compute():
    data = {}
    with tempfile.TemporaryDirectory() as tmp:
        for clip in ('glance', 'tilt'):
            for way, options in WAYS.items():
                target = os.path.join(tmp, f'{clip}-{way}.mp4')
                subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', os.path.join(CLIPS, f'{clip}.mp4'), '-c:v', 'libx264',
                                *options, target], check=True)
                out = subprocess.run(['ffprobe', '-v', 'error', '-select_streams', 'v', '-show_entries', 'frame=pkt_size',
                                      '-of', 'csv=p=0', target], check=True, capture_output=True, text=True).stdout
                data[f'{clip}-{way}'] = [int(v.strip(',')) for v in out.split() if v.strip(',')]
    with open(DATA, 'w') as f:
        json.dump(data, f)
    print({k: sum(v) for k, v in data.items()})


def draw():
    sys.path.insert(0, os.path.join(HERE, '..', '..', '..', 'scripts'))
    import automaton_pictures as ap
    INK, GREY, LINE, PURPLE = '#1f2937', '#6b7280', '#d1d5db', '#7c3aed'
    data = json.load(open(DATA))
    top_kb = 350

    def panel(parts, x0, w, y0, h, way, title, size, total_labels):
        x = lambda frame: x0 + w * frame / 96
        y = lambda kb: y0 + h - h * kb / top_kb
        parts.append(ap.text(x0, y0 - 12, title, size, INK, 'start'))
        for kb in (0, 100, 200, 300):
            parts.append(f"<line x1='{x0}' y1='{y(kb):.1f}' x2='{x0 + w}' y2='{y(kb):.1f}' stroke='{LINE}' stroke-width='1'/>")
            parts.append(ap.text(x0 - 6, y(kb) + size * 0.35, f'{kb} KB' if kb == 300 else f'{kb}', size, GREY, 'end'))
        for s in range(5):
            parts.append(ap.text(x(24 * s), y0 + h + size + 6, f'{s} s', size, GREY))
        if way == '400k':
            # 400 kbit/s: 50.000 bytes a second
            parts.append(f"<line x1='{x(0):.1f}' y1='{y(0):.1f}' x2='{x(96):.1f}' y2='{y(200):.1f}' stroke='{INK}' stroke-width='1.5' stroke-dasharray='6 5'/>")
            parts.append(ap.text(x(60), y(125) - 10, '400 kbit/s', size, INK, 'end'))
        for clip, colour, label in (('glance', GREY, 'Blick'), ('tilt', PURPLE, 'Fahrt')):
            sizes = data[f'{clip}-{way}']
            total, points = 0, [f'{x(0):.1f},{y(0):.1f}']
            for k, b in enumerate(sizes):
                total += b
                points.append(f'{x(k + 1):.1f},{y(total / 1000):.1f}')
            parts.append(f"<polyline points='{' '.join(points)}' fill='none' stroke='{colour}' stroke-width='2.5'/>")
            parts.append(ap.text(x(96) + 6, y(total / 1000) + size * 0.35, label, size, colour, 'start'))

    def picture(name, width, size, stacked):
        parts = []
        if stacked:
            w, h = width - 130, 150
            panel(parts, 70, w, 40, h, 'crf28', 'feste Qualität: -crf 28', size, True)
            panel(parts, 70, w, 40 + h + 80, h, '400k', 'feste Rate: -b:v 400k', size, True)
            height = 40 + 2 * h + 80 + size + 18
        else:
            w, h = (width - 300) / 2, 200
            panel(parts, 70, w, 40, h, 'crf28', 'feste Qualität: -crf 28', size, True)
            panel(parts, 70 + w + 150, w, 40, h, '400k', 'feste Rate: -b:v 400k', size, True)
            height = 40 + h + size + 18
        head = ap.svg_head(width, round(height), 'Die Bytes bis zu jedem Bild, mit fester Qualität und mit fester Rate; gemacht von bilder/rate_control.py, hell und dunkel:',
                           f'src/scripts/render-svg.rb src/content/video-compression/{name}.svg')
        with open(os.path.join(HERE, '..', f'{name}.svg'), 'w') as f:
            f.write(head + ''.join(parts) + '</svg>\n')

    picture('rate-control-wide', 960, 15, False)
    picture('rate-control', 440, 13, True)


if len(sys.argv) > 1 and sys.argv[1] == 'compute':
    compute()
else:
    draw()
