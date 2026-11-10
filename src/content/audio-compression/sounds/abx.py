"""The pieces for the listening test of »Audio komprimieren« (abx_widget.js):
musik.wav coded with MP3, AAC and Opus at three bit rates each, decoded
again by ffmpeg and cut to the same five seconds as the original, so that
the browser plays only lossless FLAC and every piece starts at the same
sample (a coded file played directly would start a little late, by the
codec's delay, and give the answer away). Checks that each piece lines up
with the original. Runs in the Workspace image, whose ffmpeg students use:

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo \\
        --entrypoint python3 hs_code_server:latest src/content/audio-compression/sounds/abx.py
"""

import os
import subprocess
import tempfile
import wave

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
SOURCE = os.path.join(HERE, '..', '..', '..', 'static', 'audio', 'musik.wav')
OUT = os.path.join(HERE, '..', '..', '..', 'static', 'audio', 'abx')
START, SECONDS, RATE = 1.0, 5.0, 44100
CODECS = {
    'mp3': (['-c:a', 'libmp3lame'], 'mp3', [32, 64, 128]),
    'aac': (['-c:a', 'aac'], 'm4a', [32, 64, 128]),
    'opus': (['-c:a', 'libopus'], 'opus', [16, 32, 64]),
}


def ffmpeg(*args):
    subprocess.run(['ffmpeg', '-v', 'error', '-y', *args], check=True)


def read(path):
    with wave.open(path, 'rb') as f:
        return np.frombuffer(f.readframes(f.getnframes()), dtype='<i2').astype(float)


def piece(source, target):
    """Five seconds of source (any format ffmpeg reads) as FLAC, mono, 44.100 Hz."""
    ffmpeg('-i', source, '-ss', str(START), '-t', str(SECONDS), '-ac', '1', '-ar', str(RATE),
           '-c:a', 'flac', target)


def lag(a, b):
    """How many samples b lies behind a, from the cross-correlation."""
    n = min(len(a), len(b))
    c = np.fft.irfft(np.fft.rfft(a[:n]) * np.conj(np.fft.rfft(b[:n])))
    shift = int(np.argmax(c))
    return shift if shift < n // 2 else shift - n


def main():
    os.makedirs(OUT, exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        piece(SOURCE, os.path.join(OUT, 'original.flac'))
        ffmpeg('-i', os.path.join(OUT, 'original.flac'), os.path.join(tmp, 'original.wav'))
        original = read(os.path.join(tmp, 'original.wav'))
        for name, (options, ext, rates) in CODECS.items():
            for kbit in rates:
                coded = os.path.join(tmp, f'{name}-{kbit}.{ext}')
                ffmpeg('-i', SOURCE, *options, '-b:a', f'{kbit}k', coded)
                target = os.path.join(OUT, f'{name}-{kbit}.flac')
                piece(coded, target)
                ffmpeg('-i', target, os.path.join(tmp, 'check.wav'))
                back = read(os.path.join(tmp, 'check.wav'))
                print(f'{name} {kbit:3} kbit/s: {os.path.getsize(coded):7} bytes coded, '
                      f'{len(back) - len(original):+d} samples longer, {lag(original, back):+d} samples late')


main()
