"""The clips of the video activity: four seconds each from »Spring«, the
2019 Blender open movie by Andy Goralczyk (© Blender Foundation |
cloud.blender.org/spring, CC BY 4.0), scaled to 960 × 400 (2,4 : 1 like
the film; both sides multiples of 16, the size of a macroblock), 24
frames per second, without sound, as H.264 of high quality (CRF 14) in
src/static/video/:

    blick.mp4      65,5 s: the girl looks at the camera; only her eyes move
    schwenk.mp4    37 s:   a slow pan along a mountain ridge with clouds
    hang.mp4       395 s:  a slope in the evening sun, the girl walking; fine texture
    aufwaerts.mp4  408 s:  the camera tilts fast up a rock face with plants

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
CLIPS = {'blick': 65.5, 'schwenk': 37.0, 'hang': 395.0, 'aufwaerts': 408.0}
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


main(sys.argv[1])
