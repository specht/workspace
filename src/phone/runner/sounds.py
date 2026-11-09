"""Speech and music as float32 arrays at 16 kHz, cached on disk: speech
from Piper (one file per sentence and voice), music fetched once (yt-dlp
for YouTube, any URL or file through ffmpeg). Slow work runs in a thread,
so the mixer never waits for it."""

import asyncio
import hashlib
import os
import re
import subprocess

import numpy as np

from mixer import RATE, pcm16_to_float

CACHE = os.environ.get('PHONE_CACHE', '/cache')
VOICE = os.environ.get('PHONE_VOICE', '/voices/de_DE-thorsten-high.onnx')

_voice = None


def _piper():
    global _voice
    if _voice is None:
        from piper import PiperVoice
        _voice = PiperVoice.load(VOICE)
    return _voice


def _path(kind, key):
    digest = hashlib.sha1(key.encode('utf-8')).hexdigest()
    folder = os.path.join(CACHE, kind, digest[:2])
    os.makedirs(folder, exist_ok=True)
    return os.path.join(folder, digest + '.f32')


def _load(path):
    return np.fromfile(path, dtype=np.float32)


def _store(path, samples):
    tmp = path + '.tmp'
    samples.astype(np.float32).tofile(tmp)
    os.replace(tmp, path)


def sentences(text):
    """split into sentences, each spoken (and cached) on its own"""
    parts = re.split(r'(?<=[.!?…])\s+', ' '.join(text.split()))
    return [p for p in parts if p]


def speak_sentence(sentence):
    path = _path('speech', os.path.basename(VOICE) + '\n' + sentence)
    if os.path.exists(path):
        return _load(path)
    voice = _piper()
    data = b''.join(chunk.audio_int16_bytes for chunk in voice.synthesize(sentence))
    samples = pcm16_to_float(data, voice.config.sample_rate)
    # a short pause after each sentence, as a speaker breathes
    samples = np.concatenate([samples, np.zeros(int(RATE * 0.25), dtype=np.float32)])
    _store(path, samples)
    return samples


def fetch_music(source):
    """a YouTube link, another URL or a file; decoded once to 16 kHz mono"""
    path = _path('music', source)
    if os.path.exists(path):
        return _load(path)
    raw = path + '.download'
    if re.match(r'https?://(www\.)?(youtube\.com|youtu\.be|music\.youtube\.com)/', source):
        subprocess.run(['yt-dlp', '-q', '--no-playlist', '-f', 'bestaudio', '-o', raw, source],
                       check=True, timeout=300)
        source_file = raw
    else:
        source_file = source
    pcm = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', source_file, '-t', '600',
                          '-ac', '1', '-ar', str(RATE), '-f', 's16le', '-'],
                         check=True, capture_output=True, timeout=300).stdout
    if os.path.exists(raw):
        os.remove(raw)
    samples = np.frombuffer(pcm, dtype=np.int16).astype(np.float32) / 32768.0
    # as loud as the voice, roughly: normalise the music's peaks
    peak = float(np.percentile(np.abs(samples), 99.5)) if len(samples) else 0.0
    if peak > 0:
        samples = np.clip(samples / peak * 0.7, -1, 1)
    _store(path, samples)
    return samples


async def in_thread(function, *args):
    return await asyncio.get_running_loop().run_in_executor(None, function, *args)
