"""Speech and music for a call as float32 arrays at 16 kHz.

Speech comes from the speech service (src/phone/speech/server.py), which
keeps the cache and a fixed number of speech workers for all calls and
emulators. Music: a YouTube link through the speech service as well; a file
of the game is decoded here with ffmpeg, and only from the game's folder.
"""

import asyncio
import os
import re
import subprocess

import aiohttp
import numpy as np

from game import game_file
from mixer import RATE

SPEECH = os.environ.get('PHONE_SPEECH', 'http://phone-speech:8000')
MUSIC_SECONDS = 600
_files = {}                     # path, mtime → samples (a game's own music, decoded once)


async def _get(session, path, **params):
    async with session.get(SPEECH + path, params=params, timeout=aiohttp.ClientTimeout(total=600)) as response:
        if response.status != 200:
            raise RuntimeError(f'{path}: {response.status} {await response.text()}')
        return await response.read()


async def speak(session, sentence):
    data = await _get(session, '/speech', text=sentence, format='f32')
    return np.frombuffer(data, dtype=np.float32)


def is_youtube(source):
    return bool(re.match(r'https?://', source))


def _decode_file(path):
    pcm = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', path, '-t', str(MUSIC_SECONDS),
                          '-ac', '1', '-ar', str(RATE), '-f', 's16le', '-'],
                         check=True, capture_output=True, timeout=120).stdout
    samples = np.frombuffer(pcm, dtype=np.int16).astype(np.float32) / 32768.0
    peak = float(np.percentile(np.abs(samples), 99.5)) if len(samples) else 0.0
    if peak > 0:
        samples = np.clip(samples / peak * 0.7, -1, 1).astype(np.float32)
    return samples


async def music(session, folder, source):
    if is_youtube(source):
        data = await _get(session, '/music', source=source, format='f32')
        return np.frombuffer(data, dtype=np.float32)
    path = game_file(folder, source)
    if path is None:
        raise FileNotFoundError(source)
    key = (path, os.path.getmtime(path))
    if key not in _files:
        _files[key] = await asyncio.get_running_loop().run_in_executor(None, _decode_file, path)
    return _files[key]
