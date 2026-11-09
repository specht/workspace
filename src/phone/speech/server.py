"""The speech service of the Telefonspiel (docs/plan-telefonspiel.md): one
place that makes speech and fetches music, for the runner's calls and for
the emulator in every Workspace, with one cache and a fixed number of
speech workers, so ten calls and a class trying their games at once share
the processor instead of fighting for it.

    GET /speech?text=…&format=wav|f32[&mood=whisper]
                                        a sentence or a few, 16 kHz mono, Thorsten
                                        or Thorsten in a mood (MOODS)
    GET /music?source=…&format=mp3|f32[&start=75][&stop=112]
                                        a YouTube link, 16 kHz mono, looped by the client:
                                        from start (seconds) to stop, at most ten minutes
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
# Thorsten in moods: one model, a speaker per mood (say … mood: 'whisper')
MOOD_VOICE = os.environ.get('PHONE_MOOD_VOICE', '/voices/de_DE-thorsten_emotional-medium.onnx')
MOODS = ('neutral', 'amused', 'angry', 'disgusted', 'drunk', 'sleepy', 'surprised', 'whisper')
WORKERS = int(os.environ.get('PHONE_SPEECH_WORKERS', '2'))
THREADS = int(os.environ.get('PHONE_SPEECH_THREADS', '2'))      # per worker (_load)
WAITING = int(os.environ.get('PHONE_SPEECH_WAITING', '100'))     # more requests waiting: 503
MAX_TEXT = 500                                                   # characters per request
MUSIC_SECONDS = 600                                              # the longest piece: ten minutes from start
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


# Interjections without a vowel, which espeak spells letter by letter (»Psst«
# as P-S-S-T): their sound as phonemes, which Piper takes in [[ … ]], the
# punctuation after them inside so its pause stays. A list, not every word
# without a vowel, so ZDF and BVG are still spelled.
INTERJECTIONS = [
    (r'p+s+t+', 'pst'), (r'p+s?ch+t+', 'pʃt'), (r's?ch+h*|sh+', 'ʃː'), (r'h+m+', 'hmː'), (r'm+h+m+', 'mhm'),
    (r'b+r+', 'bʁː'), (r'g+r+', 'ɡʁː'), (r'p+f+', 'pfː'), (r't+s+', 'tsː'),
]
# English words espeak reads as German (Hotline as »Hot-lie-ne«) or the
# voice speaks badly: their English sound, chosen by ear.
LOANWORDS = [
    (r'hotlines?', None), (r'workspaces?', None),
]
LOANWORD_SOUNDS = {'hotline': 'hˈɒtlaɪn', 'hotlines': 'hˈɒtlaɪns', 'workspace': 'wˈœɐkspɛɪs', 'workspaces': 'wˈœɐkspɛɪsɪs'}
INTERJECTIONS += LOANWORDS
_INTERJECTION = re.compile(r'(?<![\w-])(' + '|'.join(f'(?:{p})' for p, _ in INTERJECTIONS) + r')(?![\w-])([,.!?;:…]*)', re.IGNORECASE)


def sounding(sentence):
    """the sentence as Piper should read it: interjections and some English
    words as their sound"""
    def sound(m):
        word = m.group(1).lower()
        for pattern, phonemes in INTERJECTIONS:
            if re.fullmatch(pattern, word):
                phonemes = phonemes or LOANWORD_SOUNDS[word]
                return f'[[ {phonemes}{m.group(2)} ]]'
        return m.group(0)
    return _INTERJECTION.sub(sound, sentence)


# speech, in the worker processes --------------------------------------------

_voice = None
_mood_voice = None


def _load(path):
    """a voice whose model computes on THREADS threads. Piper's own session
    takes one thread per core of the machine, and with the service's limit of
    two processors (config.rb) Docker throttles them: on six cores twice as
    slow as with two threads, on a server with more cores slower still."""
    import onnxruntime
    from piper import PiperVoice
    voice = PiperVoice.load(path)
    options = onnxruntime.SessionOptions()
    options.intra_op_num_threads = THREADS
    options.inter_op_num_threads = 1
    voice.session = onnxruntime.InferenceSession(str(path), sess_options=options, providers=['CPUExecutionProvider'])
    return voice


def _load_voice():
    global _voice, _mood_voice
    _voice = _load(VOICE)
    if os.path.exists(MOOD_VOICE):
        _mood_voice = _load(MOOD_VOICE)


def ready():
    """nothing: started once per worker at the start, so each loads the voice
    then and not during the first call"""
    time.sleep(0.5)


PHONEME_BLOCK = re.compile(r'\[\[(.*?)\]\]')


def phonemes_of(voice, sentence):
    """the sentence as one list of phonemes: espeak's for the words, ours
    for the interjections and loanwords, the game's own [[ … ]] as written.
    Piper's own [[ … ]] would start a new sentence after each block, voice
    every piece on its own at full volume, and an empty piece at the end as
    a burst of noise."""
    out = []

    def space():
        if out and out[-1] != ' ':
            out.append(' ')

    text = sounding(sentence)
    at = 0
    for m in list(PHONEME_BLOCK.finditer(text)) + [None]:
        part = text[at:m.start() if m else len(text)]
        if part.strip() and not re.search(r'\w', part):
            out.extend(part.strip())             # only punctuation: espeak would drop it
            if part[-1].isspace():
                space()
        elif part.strip():
            if part[0].isspace():
                space()
            for piece in voice.phonemize(part):
                if piece:
                    if out and out[-1] != ' ' and piece[0] != ' ':
                        space()
                    out.extend(piece)
            if part[-1].isspace():
                space()
        elif part:
            space()
        if m:
            out.extend(m.group(1).strip())
            at = m.end()
    while out and out[-1] == ' ':
        out.pop()
    return out


def synthesize(sentence, mood=None):
    """one sentence to float32 bytes at 16 kHz, with a short pause after it
    (as a speaker breathes), in a mood or not; runs in a worker process"""
    import soxr
    from piper import SynthesisConfig
    voice, config = _voice, None
    if mood:
        voice = _mood_voice
        config = SynthesisConfig(speaker_id=voice.config.speaker_id_map[mood])
    samples = np.asarray(voice.phoneme_ids_to_audio(voice.phonemes_to_ids(phonemes_of(voice, sentence)), config), dtype=np.float32)
    peak = float(np.max(np.abs(samples))) if samples.size else 0.0
    samples = samples / peak if peak > 1e-8 else np.zeros_like(samples)     # as Piper's synthesize does
    if voice.config.sample_rate != RATE:
        samples = soxr.resample(samples, voice.config.sample_rate, RATE)
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

# YouTube asks some servers to sign in (»confirm you're not a bot«): with
# PHONE_YOUTUBE_COOKIES in credentials.rb, config.rb mounts the cookies of a
# Google account kept for this (cookies.txt, read-only); yt-dlp writes renewed
# cookies back, so it gets a copy in /tmp, made anew at each start
YOUTUBE_COOKIES = os.environ.get('PHONE_YOUTUBE_COOKIES', '')
_cookies_lock = threading.Lock()


def cookies_args():
    if not YOUTUBE_COOKIES or not os.path.isfile(YOUTUBE_COOKIES):
        return []
    copy = '/tmp/youtube-cookies.txt'
    with _cookies_lock:
        if not os.path.exists(copy):
            shutil.copyfile(YOUTUBE_COOKIES, copy)
            os.chmod(copy, 0o600)
    return ['--cookies', copy]


def download(source, raw):
    subprocess.run([sys.executable, '-m', 'yt_dlp', '-q', '--no-playlist', '--no-warnings', '-f', 'bestaudio',
                    '--max-filesize', '80M', *cookies_args(), '-o', raw, source],
                   check=True, timeout=300, stdin=subprocess.DEVNULL, env=yt_dlp_env())


def fetch_youtube(source, raw):
    """the video's audio as YouTube sends it, kept beside the pieces cut
    from it, so another start or stop needs no second download"""
    tmp = raw + '.download'
    try:
        try:
            download(source, tmp)
        except subprocess.CalledProcessError:
            if not update_yt_dlp(f'download of {source} failed'):
                raise
            download(source, tmp)
        os.replace(tmp, raw)
    finally:
        if os.path.exists(tmp):
            os.remove(tmp)


def cut(raw, start, length, path):
    """the piece from start, length seconds long, 16 kHz mono float32"""
    pcm = subprocess.run(['ffmpeg', '-loglevel', 'error', '-ss', f'{start:g}', '-i', raw, '-t', f'{length:g}',
                          '-ac', '1', '-ar', str(RATE), '-f', 's16le', '-'],
                         check=True, capture_output=True, timeout=300).stdout
    samples = np.frombuffer(pcm, dtype=np.int16).astype(np.float32) / 32768.0
    if len(samples) < RATE // 10:
        # the one message students see (in the terminal, through the page)
        raise ValueError(f'Das Video ist kürzer als {start:g} Sekunden; start liegt dahinter.')
    # roughly as loud as the voice: the music's peaks normalised
    peak = float(np.percentile(np.abs(samples), 99.5))
    if peak > 0:
        samples = np.clip(samples / peak * 0.7, -1, 1).astype(np.float32)
    store(path, samples.astype(np.float32).tobytes())


def span(query):
    """start and stop of a /music request in seconds: (0.0, None) for the
    first ten minutes; ValueError for anything else than two times in order"""
    def number(name):
        value = query.get(name, '').strip()
        if not value:
            return None
        result = float(value)
        if not 0 <= result < 24 * 3600:
            raise ValueError(f'{name} out of range')
        return result
    start, stop = number('start') or 0.0, number('stop')
    if stop is not None and stop <= start:
        raise ValueError('stop must come after start')
    return start, stop


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
        # sentences made in advance (prepare=1) take at most all workers but
        # one, so a sentence a call waits for always finds one free
        self.preparing = asyncio.Semaphore(max(1, WORKERS - 1))
        self.queued = 0              # requests in advance waiting for their turn

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

    @staticmethod
    def speech_path(sentence, mood=None):
        # without a mood the key it always had
        voice = os.path.basename(VOICE) if not mood else f'{os.path.basename(MOOD_VOICE)}:{mood}'
        # the key from the text as read, so a sentence spelled before is made anew
        read = sounding(sentence)
        # a sentence with phonemes in it is made in one piece now, without the
        # noise at its end: a key of its own, so the old one is not used
        return cache_path('speech', voice + '\n' + read + ('\nwhole' if '[[' in read else ''), '.f32')

    async def sentence(self, sentence, mood=None, prepare=False):
        path = self.speech_path(sentence, mood)
        if prepare and not os.path.exists(path):
            if self.queued >= WAITING:
                raise web.HTTPServiceUnavailable(text='busy')
            self.queued += 1
            try:
                async with self.preparing:
                    await self.once(path, self.speech_pool, synthesize, sentence, mood)
            finally:
                self.queued -= 1
        else:
            await self.once(path, self.speech_pool, synthesize, sentence, mood)
        with open(path, 'rb') as f:
            return f.read()

    async def speech(self, request):
        text = request.query.get('text', '')
        if not text.strip():
            raise web.HTTPBadRequest(text='text is missing')
        if len(text) > MAX_TEXT:
            raise web.HTTPRequestEntityTooLarge(MAX_TEXT, len(text), text=f'at most {MAX_TEXT} characters')
        mood = request.query.get('mood', '').strip() or None
        if mood and mood not in MOODS:
            raise web.HTTPBadRequest(text=f'mood is one of {", ".join(MOODS)}')
        headers = {}
        if mood and not os.path.exists(MOOD_VOICE):
            # an image from before the moods: Thorsten without one, and the
            # answer says so (the emulator tells the terminal)
            mood = None
            headers['X-Phone-Mood'] = 'missing'
        started = time.monotonic()
        prepare = request.query.get('prepare') == '1'     # in advance, not for a call waiting now
        parts = [await self.sentence(s, mood, prepare) for s in sentences(text)]
        audio = b''.join(parts)
        log(f'speech {len(text)} chars, {len(audio) / 4 / RATE:.1f} s audio in {time.monotonic() - started:.2f} s')
        if request.query.get('format') == 'f32':
            return web.Response(body=audio, content_type='application/octet-stream', headers=headers)
        return web.Response(body=wav(audio), content_type='audio/wav',
                            headers={**headers, 'Cache-Control': 'no-cache' if headers else 'max-age=86400'})

    async def music(self, request):
        source = request.query.get('source', '').strip()
        if not YOUTUBE.match(source):
            raise web.HTTPBadRequest(text='only YouTube links (https://www.youtube.com/watch?v=… or https://youtu.be/…)')
        try:
            start, stop = span(request.query)
        except ValueError as error:
            raise web.HTTPBadRequest(text=str(error))
        length = min(stop - start, MUSIC_SECONDS) if stop is not None else MUSIC_SECONDS
        # the whole beginning keeps the key it always had
        key = source if not start and stop is None else f'{source}\n{start:g}-{"" if stop is None else f"{stop:g}"}'
        raw = cache_path('music', source, '.audio')
        f32 = cache_path('music', key, '.f32')
        started = time.monotonic()
        try:
            if not os.path.exists(f32):
                await self.once(raw, self.music_pool, fetch_youtube, source, raw)
            await self.once(f32, self.music_pool, cut, raw, start, length, f32)
        except subprocess.SubprocessError as error:
            log('music failed:', source, error)
            raise web.HTTPBadGateway(text='could not fetch this video')
        except ValueError as error:
            raise web.HTTPBadRequest(text=str(error))
        log(f'music {key!r} in {time.monotonic() - started:.2f} s')
        if request.query.get('format') == 'f32':
            return web.FileResponse(f32)
        mp3 = cache_path('music', key, '.mp3')
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

    async def prepared(self, request):
        """which of a game's fixed sentences and music are made already
        (POST {sentences: [{text, mood}], music: [link]}): the moderation
        publishes a game only when its sentences are, so the first caller
        does not wait (phone_games_api.rb). Makes nothing itself."""
        try:
            data = await request.json()
        except ValueError:
            raise web.HTTPBadRequest(text='JSON expected')
        total, ready, missing = 0, 0, []
        for item in (data.get('sentences') or [])[:2000]:
            text = str(item.get('text', ''))[:MAX_TEXT] if isinstance(item, dict) else ''
            mood = item.get('mood') if isinstance(item, dict) else None
            if mood not in MOODS or not os.path.exists(MOOD_VOICE):
                mood = None
            for part in sentences(text):
                total += 1
                if os.path.exists(self.speech_path(part, mood)):
                    ready += 1
                elif len(missing) < 20:
                    missing.append(part)
        music = [str(m) for m in (data.get('music') or [])[:100]]
        music_ready = sum(1 for m in music if os.path.exists(cache_path('music', m, '.audio')))
        return web.json_response({'total': total, 'ready': ready, 'missing': missing,
                                  'music': len(music), 'music_ready': music_ready})

    async def health(self, request):
        return web.json_response({'ok': True, 'workers': WORKERS, 'waiting': self.waiting})


def main():
    service = Service()
    app = web.Application()
    app.on_startup.append(service.warm_up)
    app.add_routes([web.get('/speech', service.speech), web.get('/music', service.music),
                    web.get('/health', service.health), web.post('/prepared', service.prepared)])
    log(f'speech service: {WORKERS} workers, voice {os.path.basename(VOICE)}')
    web.run_app(app, port=8000, print=None, access_log=None)


if __name__ == '__main__':
    main()
