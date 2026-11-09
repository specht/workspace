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

    async def say(self, text):
        raise NotImplementedError

    async def music(self, source, volume=1.0):
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
        if 'say' in command:
            await self.say(str(command['say']))
            return {'ok': True}
        if 'sleep' in command:
            await self.sleep(command['sleep'])
            return {'ok': True}
        if 'music' in command:
            await self.music(command['music'], command.get('volume', 1.0))
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
