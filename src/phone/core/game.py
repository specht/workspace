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
# the same in a game another one called (call): the line drops, and the
# calling game is back with no answer
CRASHED_CALLED = 'Dieses Spiel hat gerade einen Fehler.'

# The keys a connection keeps until the game asks for them (a real phone
# system's type-ahead): at most MAX_KEYS, more are lost. Each connection
# starts with only the digits dialled after its number (the extension).
MAX_KEYS = 20
# calls on hold at most (call inside call); one more and the line is
# overloaded: the caller hears it and the call ends. Each waits as a process
# of its own (a game in Ruby about 13 MB, in JavaScript about 50 MB)
MAX_DEPTH = 20
OVERLOADED = 'Alle Leitungen sind überlastet. Bitte ruf später noch einmal an.'
# a number with its extension: at most this many digits
MAX_DIALED = 20
# the letters of a phone's keys (DICE is 3423)
KEYPAD = {**{c: '2' for c in 'ABC'}, **{c: '3' for c in 'DEF'}, **{c: '4' for c in 'GHI'}, **{c: '5' for c in 'JKL'},
          **{c: '6' for c in 'MNO'}, **{c: '7' for c in 'PQRS'}, **{c: '8' for c in 'TUV'}, **{c: '9' for c in 'WXYZ'}}


def dial_digits(text):
    """what a number dials: its digits, letters as on the keys (FACT is
    3228); None for anything else"""
    out = ''
    for c in str(text).strip().upper():
        if c.isdigit():
            out += c
        elif c in KEYPAD:
            out += KEYPAD[c]
        elif c in '*#':
            out += c
        elif c in ' -/':
            continue
        else:
            return None
    return out


def match_number(dialed, numbers):
    """the game a dialled number reaches and what was dialled after it (the
    extension): (number, extension), or (None, '') for none. Numbers are
    prefix-free, as in a real network: none is the beginning of another, so
    the first that fits is the only one (the longest, should two fit)"""
    fitting = [n for n in numbers if n and str(dialed).startswith(n)]
    if not fitting:
        return None, ''
    number = max(fitting, key=len)
    return number, str(dialed)[len(number):]


def tones(digits, rate=16000):
    """the key tones (DTMF) of these digits as float samples: each key its two
    frequencies for 100 ms, 80 ms between the keys"""
    import math
    rows = {'1': 697, '2': 697, '3': 697, '4': 770, '5': 770, '6': 770, '7': 852, '8': 852, '9': 852, '*': 941, '0': 941, '#': 941}
    cols = {'1': 1209, '2': 1336, '3': 1477, '4': 1209, '5': 1336, '6': 1477, '7': 1209, '8': 1336, '9': 1477, '*': 1209, '0': 1336, '#': 1477}
    out = []
    on, off, fade = int(rate * 0.1), int(rate * 0.08), int(rate * 0.005)
    for key in digits:
        if key not in rows:
            continue
        for i in range(on):
            ramp = min(1.0, i / fade, (on - i) / fade)
            out.append(0.2 * ramp * (math.sin(2 * math.pi * rows[key] * i / rate) + math.sin(2 * math.pi * cols[key] * i / rate)))
        out.extend([0.0] * off)
    return out


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
        # the digits a called game answered with (back), None before
        self.returned = None
        # calls on hold below this one (call)
        self.depth = 0

    def log(self, *parts):
        pass

    def press(self, key):
        """a key reaches the connection: kept until the game asks for it, at
        most MAX_KEYS (more are lost, as on a full buffer)"""
        if self.keys.qsize() >= MAX_KEYS:
            self.log('key lost, the buffer is full:', key)
            return
        self.keys.put_nowait(key)

    def fresh_keys(self, first=''):
        """a new connection: nothing kept from before, only these digits (the
        extension dialled after the number)"""
        self.keys = asyncio.Queue()
        for key in str(first)[:MAX_KEYS]:
            self.keys.put_nowait(key)

    def first_keys(self, digits):
        """these digits before the ones kept (the extension, which was dialled
        before anything the caller pressed after it)"""
        kept = []
        while not self.keys.empty():
            kept.append(self.keys.get_nowait())
        self.fresh_keys((str(digits) + ''.join(kept))[:MAX_KEYS])

    async def dtmf(self, digits):
        """the key tones of these digits on the line, heard by the caller
        (dialling with call, answering with back)"""

    async def call_game(self, dialed):
        """call: the game of this number (with its extension) while this one
        waits on hold; returns the digits it answered with (back), '' when it
        ended without (hung up, crashed, no such number)"""
        return ''

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
        if command.get('clear_keys'):
            # what was pressed so far is forgotten (a phone system does this
            # before an important question); an extension not read yet too
            self.fresh_keys()
            return {'ok': True}
        if command.get('hangup'):
            return None
        if 'transfer' in command or 'call' in command:
            kind = 'transfer' if 'transfer' in command else 'call'
            number = dial_digits(command[kind])
            extension = dial_digits(command.get('extension', '') or '')
            if not number or not re.fullmatch(r'\d{3,4}\d*', number):
                return {'error': f'{kind}: Eine Spielnummer hat drei oder vier Ziffern oder Buchstaben, nicht »{command[kind]}«.'}
            if extension is None or not re.fullmatch(r'[\d*#]*', extension):
                return {'error': f'{kind}: Nach der Nummer kommen nur Ziffern, nicht »{command.get("extension")}«.'}
            dialed = number + extension
            if len(dialed) > MAX_DIALED:
                return {'error': f'{kind}: Höchstens {MAX_DIALED} Ziffern mit allem, was nach der Nummer kommt.'}
            if kind == 'transfer':
                problem = self.transfer_problem(dialed)
                if problem:
                    return {'error': problem}
                # the call goes on in the game of this number, as if the
                # caller had dialled it; this game ends here
                self.transfer_to = dialed
                return None
            # call: this game waits on hold, the caller hears the number
            # dialled and is with the other game; what it answers comes back.
            # A number without a game: »Kein Anschluss«, as on a real line,
            # and no answer
            if self.depth >= MAX_DEPTH:
                await self.say(OVERLOADED)
                self.log('overloaded at depth', self.depth)
                self.ended.set()
                return None
            await self.dtmf(dialed)
            answer = await self.call_game(dialed)
            return {'keys': answer}
        if 'back' in command:
            # answer the game that called: the digits as key tones, then #,
            # and this game ends; dialled by a person, the call ends
            digits = dial_digits(command['back'])
            if digits is None or not re.fullmatch(r'[\d*]*', digits) or len(digits) > MAX_DIALED:
                return {'error': f'back: Zurück gehen nur Ziffern, höchstens {MAX_DIALED}, nicht »{command["back"]}«.'}
            await self.dtmf(digits + '#')
            self.returned = digits
            return None
        return {'error': 'unknown command'}


async def run_game(phone, command, cwd, env, output, preexec_fn=None, seconds=900):
    """run the game until it ends, hangs up, the caller hangs up or the time
    is over; output(kind, line) gets its ordinary lines ('out', 'err').
    Returns why it ended: 'ended', 'hangup', 'transfer' (phone.transfer_to
    says where), 'back' (phone.returned: its answer), 'caller', 'time'."""
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
                return 'transfer' if phone.transfer_to else ('back' if phone.returned is not None else 'hangup')
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
