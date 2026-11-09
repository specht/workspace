"""What the runner and the emulator have in common: running a game and
speaking its protocol (docs/plan-telefonspiel.md, »The protocol«), so a
game behaves the same on the real phone and in the Workspace.

A game writes a command as a line of JSON on stdout and reads the answer
on stdin. Lines that are not JSON are its ordinary output. A Phone is
where the commands go: the runner's call (RTP into Asterisk) or the
emulator's page in the browser. Only the standard library: the emulator
runs in every Workspace without installing anything.
"""

import asyncio
import json
import os
import re

LIB = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'lib')

# a file name to the command that runs it
LANGUAGES = {
    '.py': ['python3'],
    '.rb': ['ruby'],
    '.js': ['node'],
}


def command_for(args):
    """`spiel.rb` → ['ruby', 'spiel.rb']; anything else stays as it is"""
    if len(args) == 1:
        ext = os.path.splitext(args[0])[1]
        if ext in LANGUAGES:
            return LANGUAGES[ext] + args
    return list(args)


def sentences(text):
    """the text in sentences, so the first can play while the next is made"""
    parts = re.split(r'(?<=[.!?…:;])\s+', ' '.join(str(text).split()))
    return [p for p in parts if p]


def game_file(folder, source):
    """a file in the game's folder, or None for anything outside it"""
    path = os.path.realpath(os.path.join(folder, source))
    root = os.path.realpath(folder)
    if os.path.commonpath([path, root]) != root or not os.path.isfile(path):
        return None
    return path


# Thorsten's moods (say … mood:), the speakers of his emotional voice, and
# how the terminal names them
MOODS = {'neutral': 'neutral', 'amused': 'amüsiert', 'angry': 'wütend', 'disgusted': 'angewidert',
         'drunk': 'betrunken', 'sleepy': 'müde', 'surprised': 'überrascht', 'whisper': 'flüsternd'}


def mood_of(value):
    """None for none; ValueError with a message for the game for a mood that does not exist"""
    if value is None or value == '':
        return None
    mood = str(value).strip().lower()
    if mood not in MOODS:
        raise ValueError(f"say: die Stimmung »{value}« gibt es nicht; es gibt {', '.join(MOODS)}")
    return mood


# the longest piece of music: from start, at most ten minutes (the speech
# service and the runner cut no more)
MUSIC_SECONDS = 600


def clock(seconds):
    """75 → '1:15', 3723.5 → '1:02:03.5'"""
    whole, frac = divmod(round(seconds, 2), 1)
    h, rest = divmod(int(whole), 3600)
    m, s = divmod(rest, 60)
    text = f'{h}:{m:02d}:{s:02d}' if h else f'{m}:{s:02d}'
    return text + (f'{frac:.2f}'.rstrip('0')[1:] if frac else '')


def seconds(value, name):
    """a time as a game writes it: 75, 75.5, '75', '1:15' or '1:02:03'; None stays None"""
    if value is None or value == '':
        return None
    if isinstance(value, bool):
        raise ValueError(f'music: {name} ist keine Zeit')
    if isinstance(value, (int, float)):
        result = float(value)
    else:
        text = str(value).strip()
        m = re.fullmatch(r'(?:(\d+):)?(?:(\d+):)?(\d+(?:\.\d+)?)', text)
        if not m:
            raise ValueError(f"music: {name} »{value}« ist keine Zeit; schreib Sekunden (75) oder Minuten und Sekunden ('1:15')")
        parts = [float(p) for p in m.groups() if p is not None]
        if any(p >= 60 for p in parts[1:]):
            raise ValueError(f"music: {name} »{value}« ist keine Zeit; nach einem Doppelpunkt kommen höchstens 59 Sekunden oder Minuten")
        result = 0.0
        for p in parts:
            result = result * 60 + p
    if not result >= 0:
        raise ValueError(f'music: {name} darf nicht negativ sein')
    return result


def youtube_link(source):
    """one form per video (https://www.youtube.com/watch?v=ID), so youtu.be
    and a link with si= or list= are fetched and cached once; anything else
    as it is"""
    m = re.match(r'https://(?:www\.|m\.|music\.)?(?:youtube\.com/(?:watch\?(?:.*&)?v=|shorts/)|youtu\.be/)([\w-]{6,})', source)
    return f'https://www.youtube.com/watch?v={m.group(1)}' if m else source


def youtube_start(source):
    """the link without the start YouTube puts into a shared link (t=75,
    t=1m15s, start=75), and that start in seconds or None"""
    m = re.search(r'([?&])(t|start)=(\d+h)?(\d+m)?(\d+)?s?(?=&|#|$)', source)
    if not m or not any(m.group(i) for i in (3, 4, 5)):
        return source, None
    h, mi, se = (int((m.group(i) or '0').rstrip('hm')) for i in (3, 4, 5))
    before, after = source[:m.start()], source[m.end():]
    # »?t=5&x« → »?x«, »&t=5&x« → »&x«, »&t=5« → »«
    rest = before + m.group(1) + after[1:] if after.startswith('&') else before + after
    return rest, float(h * 3600 + mi * 60 + se)


def music_span(source, start=None, stop=None):
    """(source, start, stop) as the phone plays it: start in seconds (from a
    t= in a YouTube link if not given), stop in seconds or None for ten
    minutes after start. The piece loops from start to stop. ValueError with
    a message for the game when a time is wrong."""
    source = str(source)
    begin = seconds(start, 'start')
    end = seconds(stop, 'stop')
    if re.match(r'https?://', source):
        source, linked = youtube_start(source)
        source = youtube_link(source)
        if begin is None:
            begin = linked
    begin = begin or 0.0
    if end is not None:
        if end <= begin:
            raise ValueError(f'music: stop ({clock(end)}) muss nach start ({clock(begin)}) kommen')
        end = min(end, begin + MUSIC_SECONDS)
    return source, begin, end


def span_text(start, stop):
    """' von 1:15 bis 1:52' for the terminal; '' for the whole piece"""
    if stop is not None:
        return f' von {clock(start)} bis {clock(stop)}'
    return f' ab {clock(start)}' if start else ''


def environment(base=None):
    """the libraries of every language on their search paths"""
    env = dict(base if base is not None else os.environ)
    for var, folder in (('PYTHONPATH', 'python'), ('RUBYLIB', 'ruby'), ('NODE_PATH', 'javascript')):
        path = os.path.join(LIB, folder)
        env[var] = path + (os.pathsep + env[var] if env.get(var) else '')
    env['PYTHONUNBUFFERED'] = '1'
    return env


class Phone:
    """The other end of a game. Subclasses play speech and music; keys
    arrive with press(), and read_keys() is the same everywhere: keys
    pressed earlier (during speech) count, # ends early."""

    def __init__(self):
        self.keys = asyncio.Queue()
        self.ended = asyncio.Event()

    def log(self, *parts):
        pass

    def press(self, key):
        self.keys.put_nowait(key)

    def titled(self, title, subtitle):
        """the game named itself (title '…', subtitle '…'); nothing to do here"""

    async def say(self, text, interrupt=False, mood=None):
        """interrupt: a key stops the speech (and stays for the next keys);
        mood: one of MOODS or None"""
        raise NotImplementedError

    async def music(self, source, volume=1.0, start=0.0, stop=None):
        """source None or '' stops the music; start and stop come from music_span"""
        raise NotImplementedError

    async def play(self, source, interrupt=False):
        """a recording of the game's folder in place of the voice (an MP3 the
        student made); returns when it has been heard, like say"""
        raise NotImplementedError

    async def sleep(self, ms):
        await asyncio.sleep(max(0.0, float(ms)) / 1000)

    async def read_keys(self, count, timeout_ms):
        digits = ''
        while len(digits) < count:
            try:
                key = await asyncio.wait_for(self.keys.get(), max(0.0, timeout_ms) / 1000)
            except asyncio.TimeoutError:
                break
            if key == '#':
                break
            digits += key
        self.log('keys', repr(digits))
        return digits

    async def execute(self, command):
        """one command; the answer, or None to hang up"""
        if not isinstance(command, dict):
            return {'error': 'a command is a JSON object'}
        if 'title' in command or 'subtitle' in command:
            self.titled(str(command.get('title', '')), str(command.get('subtitle', '')))
            return {'ok': True}
        if 'say' in command:
            try:
                mood = mood_of(command.get('mood'))
            except ValueError as error:
                return {'error': str(error)}
            await self.say(str(command['say']), bool(command.get('interrupt')), mood)
            return {'ok': True}
        if 'sleep' in command:
            await self.sleep(command['sleep'])
            return {'ok': True}
        if 'play' in command:
            await self.play(str(command['play']), bool(command.get('interrupt')))
            return {'ok': True}
        if 'music' in command:
            source = command['music']
            if not source:
                await self.music(None, 1.0)
                return {'ok': True}
            try:
                source, start, stop = music_span(source, command.get('start'), command.get('stop'))
            except ValueError as error:
                return {'error': str(error)}
            await self.music(source, command.get('volume', 1.0), start, stop)
            return {'ok': True}
        if 'keys' in command:
            digits = await self.read_keys(max(1, int(command['keys'])), float(command.get('timeout', 10000)))
            return {'keys': digits}
        if command.get('hangup'):
            return None
        return {'error': 'unknown command'}


async def run_game(phone, command, cwd, env, output, preexec_fn=None, seconds=900):
    """run the game until it ends, hangs up, the caller hangs up or the time
    is over; output(kind, line) gets its ordinary lines ('out', 'err').
    Returns why it ended: 'ended', 'hangup', 'caller', 'time'."""
    process = await asyncio.create_subprocess_exec(
        *command, cwd=cwd, env=env, preexec_fn=preexec_fn, start_new_session=preexec_fn is None,
        stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE)

    async def errors():
        async for line in process.stderr:
            output('err', line.decode('utf-8', 'replace').rstrip('\n'))

    async def protocol():
        while not phone.ended.is_set():
            line = await process.stdout.readline()
            if not line:
                await process.wait()
                return 'ended'
            text = line.decode('utf-8', 'replace').strip()
            if not text.startswith('{'):
                output('out', line.decode('utf-8', 'replace').rstrip('\n'))
                continue
            try:
                command = json.loads(text)
            except json.JSONDecodeError:
                output('out', line.decode('utf-8', 'replace').rstrip('\n'))
                continue
            answer = await phone.execute(command)
            if phone.ended.is_set():
                return 'caller'
            if answer is None:
                return 'hangup'
            try:
                process.stdin.write((json.dumps(answer, ensure_ascii=False) + '\n').encode('utf-8'))
                await process.stdin.drain()
            except (ConnectionResetError, BrokenPipeError):
                await process.wait()
                return 'ended'
        return 'caller'

    error_task = asyncio.create_task(errors())
    protocol_task = asyncio.create_task(protocol())
    caller_task = asyncio.create_task(phone.ended.wait())
    try:
        done, _ = await asyncio.wait({protocol_task, caller_task}, timeout=seconds,
                                     return_when=asyncio.FIRST_COMPLETED)
        if protocol_task in done:
            reason = protocol_task.result()
        elif caller_task in done:
            reason = 'caller'
        else:
            reason = 'time'
    finally:
        for task in (protocol_task, caller_task):
            task.cancel()
        if process.returncode is None:
            try:
                os.killpg(process.pid, 9)
            except (ProcessLookupError, PermissionError):
                process.kill()
            await process.wait()
        try:
            await asyncio.wait_for(error_task, 1)
        except (asyncio.TimeoutError, asyncio.CancelledError):
            error_task.cancel()
    return reason, process.returncode
