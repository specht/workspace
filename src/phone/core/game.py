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


# For /statistik: one line per call (record_call), which the website
# collects every five minutes into numbers per hour and deletes. Set only on
# the server's line and runner (config.rb), never in a Workspace.
CALLS = os.environ.get('PHONE_CALLS', '')


def published_number(games_root, folder):
    """the number of the published game in this folder (published.json), or None"""
    try:
        with open(os.path.join(games_root, 'published.json')) as f:
            games = [g for g in json.load(f).get('games', []) if isinstance(g, dict)]
    except (OSError, ValueError, AttributeError):
        return None
    for g in games:
        if str(g.get('folder')) == str(folder) and re.fullmatch(r'\d{3,4}', str(g.get('number'))):
            return str(g.get('number'))
    return None


# What a caller hears when a published game stops with an error (a syntax
# error, an exception): instead of the call ending, the Telefonbuch again
CRASHED = 'Dieses Spiel hat gerade einen Fehler. Du kommst zurück ins Telefonbuch.'


def record_call(line, games, seconds, errors=()):
    """one call for the statistics: when it began (to the hour, as the
    website keeps it), on which line ('telefon' or 'browser'), how long, and
    the numbers of the games it reached, in order, and the errors of a game
    that crashed. Never who called: no number, no address."""
    if not CALLS:
        return
    import time
    began = time.time() - seconds
    entry = {'hour': int(began - began % 3600), 'line': line, 'seconds': int(round(seconds)), 'games': list(games)}
    # a game that stopped with an error: its number and the last lines its
    # language wrote (the error and its line), for the game's author
    if errors:
        entry['errors'] = [{'game': e['game'], 'text': e['text'][-1500:]} for e in errors if e.get('game')][:3]
    try:
        with open(os.path.join(CALLS, f'{line}.jsonl'), 'a') as f:
            f.write(json.dumps(entry) + '\n')
    except OSError:
        pass


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


# the sound files music and play take: the types the Telefonbuch copies
# with a game, directly beside it in its folder (src/ruby/include/
# phone_games.rb, KEEP and AUDIO); the phone decodes them with ffmpeg, the
# emulator's browser itself, and every browser plays MP3 and WAV
SOUND_TYPES = ('.mp3', '.wav', '.ogg', '.m4a')


def sound_file_problem(command, source):
    """None, or the message for the game when music or play gets a file it
    cannot take"""
    name = str(source)
    if '/' in name or '\\' in name:
        return (f'{command}: {name} liegt in einem Unterordner. Leg die Datei direkt neben dein Spiel; '
                'nur von dort kommt sie beim Veröffentlichen mit.')
    if os.path.splitext(name)[1].lower() not in SOUND_TYPES:
        return f'{command}: {name} spielt das Telefon nicht. Nimm eine Datei in MP3, WAV, Ogg oder M4A; MP3 spielt jeder Browser.'
    return None


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


def international(number):
    """a number as digits with the country code, the way SIP trunks report
    it: '+49 30 2290', '0049302290' and '030/22 90' all become '49302290'"""
    digits = re.sub(r'[^\d+]', '', str(number or ''))
    if digits.startswith('+'):
        return digits[1:]
    if digits.startswith('00'):
        return digits[2:]
    if digits.startswith('0'):
        return '49' + digits[1:]
    return digits


def game_number(dialed, line_number):
    """the game's number in what a call to the real line dialled (the
    runner, PHONE_DIRECT_DIAL): the line's number with a game's after it
    (»030/22 908 772-123«), a game's own three or four digits; '' when it
    is the line's number alone (the menu), None when it is neither. The
    browser's phone takes a game's number only, never a real one"""
    dialed = str(dialed or '')
    if not re.fullmatch(r'\+?[\d ]+', dialed):
        return None
    digits = re.sub(r'\D', '', dialed)
    # the line's own number first: with a short test number (100) it would
    # otherwise pass for a game's
    line = international(line_number)
    number = international(dialed)
    if line and number.startswith(line):
        rest = number[len(line):]
        return rest if rest == '' or re.fullmatch(r'\d{3,4}', rest) else None
    if re.fullmatch(r'\d{3,4}', digits) and not dialed.startswith(('0', '+')):
        return digits
    return None


class Phone:
    """The other end of a game. Subclasses play speech and music; keys
    arrive with press(), and read_keys() is the same everywhere: keys
    pressed earlier (during speech) count, # ends early."""

    def __init__(self):
        self.keys = asyncio.Queue()
        self.ended = asyncio.Event()
        # the number a game forwarded the call to (transfer)
        self.transfer_to = None

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

    def transfer_problem(self, number):
        """why the call cannot go on in the game of this number, or None"""
        return None

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
            problem = sound_file_problem('play', command['play'])
            if problem:
                return {'error': problem}
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
            problem = None if re.match(r'https?://', source) else sound_file_problem('music', source)
            if problem:
                return {'error': problem}
            await self.music(source, command.get('volume', 1.0), start, stop)
            return {'ok': True}
        if 'keys' in command:
            digits = await self.read_keys(max(1, int(command['keys'])), float(command.get('timeout', 10000)))
            return {'keys': digits}
        if command.get('hangup'):
            return None
        if 'transfer' in command:
            # the call goes on in the published game of this number, as if
            # the caller had dialled it; this game ends here
            number = str(command['transfer']).strip()
            if not re.fullmatch(r'\d{3,4}', number):
                return {'error': f'transfer: Eine Spielnummer hat drei oder vier Ziffern, nicht »{number}«.'}
            problem = self.transfer_problem(number)
            if problem:
                return {'error': problem}
            self.transfer_to = number
            return None
        return {'error': 'unknown command'}


async def run_game(phone, command, cwd, env, output, preexec_fn=None, seconds=900):
    """run the game until it ends, hangs up, the caller hangs up or the time
    is over; output(kind, line) gets its ordinary lines ('out', 'err').
    Returns why it ended: 'ended', 'hangup', 'transfer' (phone.transfer_to
    says where), 'caller', 'time'."""
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
                return 'transfer' if phone.transfer_to else 'hangup'
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
