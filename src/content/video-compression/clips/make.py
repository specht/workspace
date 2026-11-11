"""The clips of the video activity: four seconds each from »Spring«, the
2019 Blender open movie by Andy Goralczyk (© Blender Foundation |
cloud.blender.org/spring, CC BY 4.0), scaled to 960 × 400 (2,4 : 1 like
the film; both sides multiples of 16, the size of a macroblock), 24
frames per second, without sound, as H.264 of high quality (CRF 14) in
src/static/video/:

    glance.mp4      65,5 s: the girl looks to the side; only her eyes move
    pan.mp4    37 s:   a slow pan along a mountain ridge with clouds
    slope.mp4       395 s:  a slope in the evening sun, the girl walking; fine texture
    tilt.mp4  408 s:  the camera tilts fast up a rock face with plants

their first frames as stills for the page (glance.webp, …), and two
frames with all their colour, never through H.264 and its 4:2:0, as
PNG:

    slope.png       395 s:  the first frame of slope.mp4 (»Innerhalb eines Bildes«)
    portrait.png    68,5 s: frame 72 of glance.mp4, the girl close up, crisp (»Farbe sparen«)

The film comes from Wikimedia Commons, which has it from the Blender
Studio's YouTube channel, so it was compressed once before (VP9, 2048 ×
858); the clips are made from that file:

    curl -o spring.webm https://upload.wikimedia.org/wikipedia/commons/a/a5/Spring_-_Blender_Open_Movie.webm
    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo --entrypoint python3 \\
        hs_code_server:latest src/content/video-compression/clips/make.py spring.webm
"""

import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', '..', '..', 'static', 'video')
CLIPS = {'glance': 65.5, 'pan': 37.0, 'slope': 395.0, 'tilt': 408.0}
STILLS = {'slope': 395.0, 'portrait': 68.5}
SECONDS = 4
WIDTH, HEIGHT = 960, 400


def main(source):
    os.makedirs(OUT, exist_ok=True)
    for name, start in CLIPS.items():
        target = os.path.join(OUT, f'{name}.mp4')
        # scaled to 960 wide (402 high), then the middle 400 rows
        subprocess.run(['ffmpeg', '-v', 'error', '-y', '-ss', str(start), '-i', source, '-t', str(SECONDS), '-an',
                        '-vf', f'scale={WIDTH}:-2:flags=lanczos,crop={WIDTH}:{HEIGHT}', '-r', '24',
                        '-c:v', 'libx264', '-crf', '14', '-preset', 'slow', '-pix_fmt', 'yuv420p',
                        '-movflags', '+faststart', target], check=True)
        print(target, os.path.getsize(target), 'bytes')
    # the first frame of each clip as a still for the page, shown before it plays
    for name in CLIPS:
        subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', os.path.join(OUT, f'{name}.mp4'), '-frames:v', '1',
                        '-c:v', 'libwebp', '-quality', '80', os.path.join(OUT, f'{name}.webp')], check=True)
    for name, at in STILLS.items():
        target = os.path.join(OUT, f'{name}.png')
        subprocess.run(['ffmpeg', '-v', 'error', '-y', '-ss', str(at), '-i', source, '-frames:v', '1',
                        '-vf', f'scale={WIDTH}:-2:flags=lanczos+full_chroma_int+accurate_rnd,crop={WIDTH}:{HEIGHT},format=rgb24',
                        target], check=True)
        print(target, os.path.getsize(target), 'bytes')


main(sys.argv[1])
