import json
import subprocess

from PIL import Image, ImageDraw, ImageFont

CLIPS = ['glance', 'pan', 'slope', 'tilt']
COLOURS = {'I': (13, 96, 174), 'P': (201, 133, 0), 'B': (18, 149, 159)}


def frame_sizes(path):
    command = ['ffprobe', '-v', 'error', '-select_streams', 'v', '-show_entries', 'frame=pict_type,pkt_size',
               '-of', 'json', path]
    data = json.loads(subprocess.run(command, capture_output=True, text=True, check=True).stdout)
    return [(frame['pict_type'], int(frame['pkt_size'])) for frame in data['frames']]


def decode_order(kinds):
    order = []
    waiting = []
    for k, kind in enumerate(kinds):
        if kind == 'B':
            waiting.append(k)
        else:
            order.append(k)
            order += waiting
            waiting = []
    return order + waiting


def stored_order(path, count):
    command = ['ffprobe', '-v', 'error', '-select_streams', 'v', '-show_entries', 'packet=pts',
               '-of', 'json', path]
    data = json.loads(subprocess.run(command, capture_output=True, text=True, check=True).stdout)
    times = [int(packet['pts']) for packet in data['packets']]
    step = sorted(times)[1] - sorted(times)[0]
    return [(t - min(times)) // step for t in times[:count]]


def draw_bars(frames, path, width=960, height=320):
    picture = Image.new('RGB', (width, height), 'white')
    draw = ImageDraw.Draw(picture)
    font = ImageFont.load_default(size=18)
    largest = max(size for _, size in frames)
    step = width / len(frames)
    for k, (kind, size) in enumerate(frames):
        top = height - 10 - (height - 50) * size / largest
        draw.rectangle([k * step + 1, top, (k + 1) * step - 2, height - 10], fill=COLOURS[kind])
    draw.text((8, 8), f'{largest // 1000} KB', fill='black', font=font)
    for n, kind in enumerate('IPB'):
        x = width - 150 + 50 * n
        draw.rectangle([x, 12, x + 14, 26], fill=COLOURS[kind])
        draw.text((x + 20, 8), kind, fill='black', font=font)
    picture.save(path)


if __name__ == '__main__':
    for name in CLIPS:
        frames = frame_sizes(f'{name}.mp4')
        kinds = ''.join(kind for kind, _ in frames)
        print(name, kinds[:25])
        for kind in 'IPB':
            sizes = [size for k, size in frames if k == kind]
            if sizes:
                print(f'   {kind}: {len(sizes):2} Bilder, im Mittel {sum(sizes) // len(sizes):6} Bytes')
        draw_bars(frames, f'{name}-bars.png')

    kinds = ''.join(kind for kind, _ in frame_sizes('glance.mp4'))[:13]
    print('Zeigen:     ', list(range(13)))
    print('Typen:      ', ' '.join(kinds))
    print('Gerechnet:  ', decode_order(kinds))
    print('In der Datei:', stored_order('glance.mp4', 13))
