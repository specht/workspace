"""The speech service of the Telefonspiel (docs/plan-telefonspiel.md): one
place that makes speech and fetches music, for the runner's calls and for
the emulator in every Workspace, with one cache and a fixed number of
speech workers, so ten calls and a class trying their games at once share
the processor instead of fighting for it.

    GET /speech?text=…&format=wav|f32   a sentence or a few, 16 kHz mono
    GET /music?source=…&format=mp3|f32  a YouTube link, 16 kHz mono, looped by the client
    GET /health

Music comes only from YouTube: the service fetches nothing else, so no one
can make it fetch an address inside the network. The emulator and the
runner read the game's own files themselves.
"""

import asyncio
import hashlib
import json
import os
import re
import shutil
import struct
import subprocess
import sys
import threading
import time
import urllib.request
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor

import numpy as np
from aiohttp import web

RATE = 16000
CACHE = os.environ.get('PHONE_CACHE', '/cache')
VOICE = os.environ.get('PHONE_VOICE', '/voices/de_DE-thorsten-high.onnx')
WORKERS = int(os.environ.get('PHONE_SPEECH_WORKERS', '2'))
WAITING = int(os.environ.get('PHONE_SPEECH_WAITING', '100'))     # more requests waiting: 503
MAX_TEXT = 500                                                   # characters per request
MUSIC_SECONDS = 600                                              # the first ten minutes of a piece
KEEP_DAYS = 90                      # speech and music not played for this long are deleted (privacy.rb)
YOUTUBE = re.compile(r'^https://(www\.|m\.|music\.)?(youtube\.com/(watch\?v=|shorts/)|youtu\.be/)[\w-]{6,}([?&][\w=&%.-]*)?$')

_start = time.monotonic()


def log(*parts):
    print(f'{time.monotonic() - _start:8.2f}', *parts, file=sys.stderr, flush=True)


def cache_path(kind, key, ext):
    digest = hashlib.sha1(key.encode('utf-8')).hexdigest()
    folder = os.path.join(CACHE, kind, digest[:2])
    os.makedirs(folder, exist_ok=True)
    return os.path.join(folder, digest + ext)


def store(path, data):
    tmp = f'{path}.{os.getpid()}.tmp'
    with open(tmp, 'wb') as f:
        f.write(data)
    os.replace(tmp, path)


def sentences(text):
    parts = re.split(r'(?<=[.!?…:;])\s+', ' '.join(str(text).split()))
    return [p for p in parts if p]


# speech, in the worker processes --------------------------------------------

_voice = None


def _load_voice():
    global _voice
    from piper import PiperVoice
    _voice = PiperVoice.load(VOICE)


def ready():
    """nothing: started once per worker at the start, so each loads the voice
    then and not during the first call"""
    time.sleep(0.5)


def synthesize(sentence):
    """one sentence to float32 bytes at 16 kHz, with a short pause after it
    (as a speaker breathes); runs in a worker process"""
    import soxr
    data = b''.join(chunk.audio_int16_bytes for chunk in _voice.synthesize(sentence))
    samples = np.frombuffer(data, dtype=np.int16).astype(np.float32) / 32768.0
    if _voice.config.sample_rate != RATE:
        samples = soxr.resample(samples, _voice.config.sample_rate, RATE)
    samples = np.concatenate([samples.astype(np.float32), np.zeros(int(RATE * 0.25), dtype=np.float32)])
    return samples.tobytes()


def prune():
    """deletes speech and music not used for KEEP_DAYS"""
    limit = time.time() - KEEP_DAYS * 86400
    removed = 0
    for kind in ('speech', 'music'):
        for folder, _, files in os.walk(os.path.join(CACHE, kind)):
            for name in files:
                path = os.path.join(folder, name)
                try:
                    if os.path.getmtime(path) < limit:
                        os.remove(path)
                        removed += 1
                except OSError:
                    pass
    if removed:
        log(f'cache: {removed} files not played for {KEEP_DAYS} days deleted')


# yt-dlp, kept up to date ----------------------------------------------------
# YouTube changes often, and an old yt-dlp then gets 403. So the service
# keeps its own yt-dlp (with Deno, which it needs for YouTube) in
# CACHE/tools: at the start and once a day it asks PyPI for the newest
# version and installs it there when it is newer; after a failed download
# it does so at once (at most once an hour) and tries again. Until the first
# update, and when PyPI is out of reach, the version of the image runs.

TOOLS = os.path.join(CACHE, 'tools')
UPDATE_EVERY = 24 * 3600
_update_lock = threading.Lock()
_last_update = 0.0


def yt_dlp_env():
    env = dict(os.environ)
    if os.path.isdir(os.path.join(TOOLS, 'yt_dlp')):
        env['PYTHONPATH'] = TOOLS
        env['PATH'] = os.path.join(TOOLS, 'bin') + os.pathsep + env.get('PATH', '')
    return env


def yt_dlp_version():
    try:
        return subprocess.run([sys.executable, '-m', 'yt_dlp', '--version'], env=yt_dlp_env(), check=True,
                              capture_output=True, text=True, timeout=60).stdout.strip()
    except (subprocess.SubprocessError, OSError):
        return None


def update_yt_dlp(why):
    """the newest yt-dlp into TOOLS, unless it is there or was tried within the hour"""
    global _last_update
    with _update_lock:
        if time.time() - _last_update < 3600:
            return False
        _last_update = time.time()
        try:
            with urllib.request.urlopen('https://pypi.org/pypi/yt-dlp/json', timeout=30) as response:
                newest = json.load(response)['info']['version']
        except (OSError, ValueError, KeyError) as error:
            log(f'yt-dlp: PyPI not reachable ({error}); staying with {yt_dlp_version()}')
            return False
        current = yt_dlp_version()
        if current == newest:
            return False
        new, old = TOOLS + '.new', TOOLS + '.old'
        shutil.rmtree(new, ignore_errors=True)
        try:
            subprocess.run([sys.executable, '-m', 'pip', 'install', '-q', '--no-cache-dir', '--disable-pip-version-check',
                            '--target', new, f'yt-dlp[default,deno]=={newest}'],
                           check=True, capture_output=True, timeout=900, env={**os.environ, 'HOME': '/tmp'})
        except (subprocess.SubprocessError, OSError) as error:
            log(f'yt-dlp: installing {newest} failed: {error}')
            shutil.rmtree(new, ignore_errors=True)
            return False
        shutil.rmtree(old, ignore_errors=True)
        if os.path.exists(TOOLS):
            os.rename(TOOLS, old)
        os.rename(new, TOOLS)
        shutil.rmtree(old, ignore_errors=True)
        log(f'yt-dlp: {current} → {yt_dlp_version()} ({why})')
        return True


# music, in threads (they wait for yt-dlp and ffmpeg) -----------------------

def download(source, raw):
    subprocess.run([sys.executable, '-m', 'yt_dlp', '-q', '--no-playlist', '--no-warnings', '-f', 'bestaudio',
                    '--max-filesize', '80M', '-o', raw, source],
                   check=True, timeout=300, stdin=subprocess.DEVNULL, env=yt_dlp_env())


def fetch_youtube(source, path):
    raw = path + '.download'
    try:
        try:
            download(source, raw)
        except subprocess.CalledProcessError:
            if not update_yt_dlp(f'download of {source} failed'):
                raise
            download(source, raw)
        pcm = subprocess.run(['ffmpeg', '-loglevel', 'error', '-i', raw, '-t', str(MUSIC_SECONDS),
                              '-ac', '1', '-ar', str(RATE), '-f', 's16le', '-'],
                             check=True, capture_output=True, timeout=300).stdout
    finally:
        if os.path.exists(raw):
            os.remove(raw)
    samples = np.frombuffer(pcm, dtype=np.int16).astype(np.float32) / 32768.0
    # roughly as loud as the voice: the music's peaks normalised
    peak = float(np.percentile(np.abs(samples), 99.5)) if len(samples) else 0.0
    if peak > 0:
        samples = np.clip(samples / peak * 0.7, -1, 1).astype(np.float32)
    store(path, samples.astype(np.float32).tobytes())


def to_mp3(f32_path, mp3_path):
    data = subprocess.run(['ffmpeg', '-loglevel', 'error', '-f', 'f32le', '-ar', str(RATE), '-ac', '1',
                           '-i', f32_path, '-c:a', 'libmp3lame', '-b:a', '64k', '-f', 'mp3', '-'],
                          check=True, capture_output=True, timeout=300).stdout
    store(mp3_path, data)


def wav(f32_bytes):
    samples = np.frombuffer(f32_bytes, dtype=np.float32)
    pcm = (np.clip(samples, -1, 1) * 32767).astype('<i2').tobytes()
    header = b'RIFF' + struct.pack('<I', 36 + len(pcm)) + b'WAVE' + \
        b'fmt ' + struct.pack('<IHHIIHH', 16, 1, 1, RATE, RATE * 2, 2, 16) + \
        b'data' + struct.pack('<I', len(pcm))
    return header + pcm


class Service:
    def __init__(self):
        self.speech_pool = ProcessPoolExecutor(max_workers=WORKERS, initializer=_load_voice)
        self.music_pool = ThreadPoolExecutor(max_workers=2)
        self.pending = {}            # cache path → future: the same work only once at a time
        self.waiting = 0

    async def once(self, path, pool, function, *args):
        """the file at path, made by function(*args) unless it exists; its
        time is the last use (prune() deletes what is not used)"""
        if os.path.exists(path):
            try:
                os.utime(path)
            except OSError:
                pass
            return
        if path not in self.pending:
            if self.waiting >= WAITING:
                raise web.HTTPServiceUnavailable(text='busy')
            self.waiting += 1
            loop = asyncio.get_running_loop()

            async def make():
                try:
                    result = await loop.run_in_executor(pool, function, *args)
                    if isinstance(result, bytes):
                        store(path, result)
                finally:
                    self.waiting -= 1
                    self.pending.pop(path, None)
            self.pending[path] = asyncio.ensure_future(make())
        await asyncio.shield(self.pending[path])

    async def sentence(self, sentence):
        path = cache_path('speech', os.path.basename(VOICE) + '\n' + sentence, '.f32')
        await self.once(path, self.speech_pool, synthesize, sentence)
        with open(path, 'rb') as f:
            return f.read()

    async def speech(self, request):
        text = request.query.get('text', '')
        if not text.strip():
            raise web.HTTPBadRequest(text='text is missing')
        if len(text) > MAX_TEXT:
            raise web.HTTPRequestEntityTooLarge(MAX_TEXT, len(text), text=f'at most {MAX_TEXT} characters')
        started = time.monotonic()
        parts = [await self.sentence(s) for s in sentences(text)]
        audio = b''.join(parts)
        log(f'speech {len(text)} chars, {len(audio) / 4 / RATE:.1f} s audio in {time.monotonic() - started:.2f} s')
        if request.query.get('format') == 'f32':
            return web.Response(body=audio, content_type='application/octet-stream')
        return web.Response(body=wav(audio), content_type='audio/wav',
                            headers={'Cache-Control': 'max-age=86400'})

    async def music(self, request):
        source = request.query.get('source', '').strip()
        if not YOUTUBE.match(source):
            raise web.HTTPBadRequest(text='only YouTube links (https://www.youtube.com/watch?v=… or https://youtu.be/…)')
        f32 = cache_path('music', source, '.f32')
        started = time.monotonic()
        try:
            await self.once(f32, self.music_pool, fetch_youtube, source, f32)
        except subprocess.SubprocessError as error:
            log('music failed:', source, error)
            raise web.HTTPBadGateway(text='could not fetch this video')
        log(f'music {source} in {time.monotonic() - started:.2f} s')
        if request.query.get('format') == 'f32':
            return web.FileResponse(f32)
        mp3 = cache_path('music', source, '.mp3')
        await self.once(mp3, self.music_pool, to_mp3, f32, mp3)
        return web.FileResponse(mp3, headers={'Content-Type': 'audio/mpeg', 'Cache-Control': 'max-age=86400'})

    async def warm_up(self, app):
        loop = asyncio.get_running_loop()
        for _ in range(WORKERS):
            loop.run_in_executor(self.speech_pool, ready)
        app['updates'] = asyncio.ensure_future(self.keep_updated())

    async def keep_updated(self):
        """once a day: a newer yt-dlp, and away with what was not played for KEEP_DAYS"""
        loop = asyncio.get_running_loop()
        while True:
            await loop.run_in_executor(None, prune)
            await loop.run_in_executor(None, update_yt_dlp, 'daily check')
            await asyncio.sleep(UPDATE_EVERY)

    async def health(self, request):
        return web.json_response({'ok': True, 'workers': WORKERS, 'waiting': self.waiting})


def main():
    service = Service()
    app = web.Application()
    app.on_startup.append(service.warm_up)
    app.add_routes([web.get('/speech', service.speech), web.get('/music', service.music),
                    web.get('/health', service.health)])
    log(f'speech service: {WORKERS} workers, voice {os.path.basename(VOICE)}')
    web.run_app(app, port=8000, print=None, access_log=None)


if __name__ == '__main__':
    main()
